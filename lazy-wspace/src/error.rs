use std::borrow::Cow;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::io::{self, stderr, stdout, ErrorKind, Write};
use std::mem;
use std::process;
use std::rc::Rc;

use cfg_if::cfg_if;
use rug::Integer;
use thiserror::Error;
use wspace_syntax::hs::{self, Show};
use wspace_syntax::ws::ast::{self, Inst, LabelLit};

macro_rules! SrcLoc(
    ($file:literal : $line:literal : $col:literal in $package:tt : $module:expr) => {
        hs::SrcLoc {
            package: SrcLoc!(@stringify $package),
            module: SrcLoc!(@stringify $module),
            file: $file,
            line: $line,
            col: $col,
        }
    };
    (@stringify $s:literal) => { $s };
    (@stringify $($x:tt)+) => { concat!($(SrcLoc!(@to_string $x))+) };
    (@to_string $s:literal) => { $s };
    (@to_string $s:tt) => { stringify!($s) };
);
macro_rules! ListSrcLoc(($line:literal : $col:literal) => {
    SrcLoc!("libraries/base/GHC/List.hs":$line:$col in base:GHC.List)
});

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("incorrect usage")]
    Usage,
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Value(#[from] ValueError),
    #[error(transparent)]
    Eager(#[from] EagerError),
}

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum ParseError {
    #[error("incomplete instruction opcode")]
    IncompleteOpcode,
    #[error("unrecognized instruction opcode")]
    UnrecognizedOpcode,
    #[error("unterminated integer")]
    UnterminatedInteger,
    #[error("unterminated label")]
    UnterminatedLabel,
    // TODO: Reference label by span.
    #[error("undefined label {0}")]
    UndefinedLabel(LabelLit),
    #[error("implicit end")]
    ImplicitEnd,
    #[error("invalid UTF-8 sequence")]
    InvalidUtf8,
    #[error("unexpected river crab")]
    UnexpectedRiverCrab,
}

#[derive(Clone, Copy, Debug, Error, PartialEq, Eq, Hash)]
pub enum ValueError {
    #[error("empty integer literal")]
    EmptyLit,
    #[error("copy index out of bounds")]
    CopyLarge,
    #[error("copy at negative index")]
    CopyNegative,
    #[error("division by zero")]
    DivModZero,
    #[error("retrieve address out of bounds")]
    RetrieveLarge,
    #[error("retrieve at negative address")]
    RetrieveNegative,
    #[error("invalid integer from readi")]
    ReadiParse,
}

#[derive(Clone, Debug, Error)]
pub enum EagerError {
    #[error("stack underflow: {0}")]
    Underflow(Inst),
    #[error("store address overflow")]
    StoreOverflow,
    #[error("store at negative address")]
    StoreNegative,
    #[error("call stack underflow")]
    RetUnderflow,
    #[error("io error: {0}")]
    Io(Rc<io::Error>),
    #[error("printc invalid codepoint")]
    PrintcInvalid(Rc<Integer>),
    #[error("read at EOF")]
    ReadEof,
    #[error("read invalid UTF-8 sequence")]
    ReadInvalidUtf8,
}

#[derive(Clone, Copy, Debug, Error, PartialEq, Eq, Hash)]
pub enum UnderflowError {
    #[error("stack underflow")]
    Normal,
    #[error("slide with empty literal")]
    SlideEmpty,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HaskellError {
    pub out: OutKind,
    pub msg: Vec<u8>,
    pub code: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OutKind {
    Stdout,
    Stderr,
}

impl From<ast::ParseError> for Error {
    #[inline]
    fn from(err: ast::ParseError) -> Self {
        Error::Parse(err.into())
    }
}

impl From<ast::ParseError> for ParseError {
    #[inline]
    fn from(err: ast::ParseError) -> Self {
        match err {
            ast::ParseError::IncompleteOpcode => ParseError::IncompleteOpcode,
            ast::ParseError::UnrecognizedOpcode => ParseError::UnrecognizedOpcode,
            ast::ParseError::UnterminatedInteger => ParseError::UnterminatedInteger,
            ast::ParseError::UnterminatedLabel => ParseError::UnterminatedLabel,
            ast::ParseError::InvalidUtf8 => ParseError::InvalidUtf8,
            ast::ParseError::UnexpectedRiverCrab => ParseError::UnexpectedRiverCrab,
        }
    }
}

impl From<io::Error> for Error {
    #[inline]
    fn from(err: io::Error) -> Self {
        Error::Eager(err.into())
    }
}

impl From<io::Error> for EagerError {
    #[inline]
    fn from(err: io::Error) -> Self {
        match err.kind() {
            ErrorKind::InvalidData => EagerError::ReadInvalidUtf8,
            ErrorKind::UnexpectedEof => EagerError::ReadEof,
            _ => EagerError::Io(Rc::new(err)),
        }
    }
}

impl PartialEq for EagerError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (EagerError::Underflow(inst1), EagerError::Underflow(inst2)) => inst1 == inst2,
            (EagerError::Io(err1), EagerError::Io(err2)) => Rc::ptr_eq(err1, err2),
            (EagerError::PrintcInvalid(c1), EagerError::PrintcInvalid(c2)) => c1 == c2,
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}

impl Eq for EagerError {}

impl Hash for EagerError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            EagerError::Underflow(inst) => inst.hash(state),
            EagerError::StoreOverflow => {}
            EagerError::StoreNegative => {}
            EagerError::RetUnderflow => {}
            EagerError::Io(err) => Rc::as_ptr(err).hash(state),
            EagerError::PrintcInvalid(n) => n.hash(state),
            EagerError::ReadEof => {}
            EagerError::ReadInvalidUtf8 => {}
        }
    }
}

impl UnderflowError {
    #[inline]
    pub fn to_error(self, inst: &Inst) -> Error {
        match self {
            UnderflowError::Normal => {
                Error::from_inst(inst).unwrap_or_else(|| EagerError::Underflow(inst.clone()).into())
            }
            UnderflowError::SlideEmpty => ValueError::EmptyLit.into(),
        }
    }
}

impl Error {
    #[inline]
    pub fn from_inst(inst: &Inst) -> Option<Self> {
        match inst {
            Inst::Push(n) | Inst::Copy(n) | Inst::Slide(n) if n.is_empty() => {
                Some(ValueError::EmptyLit.into())
            }
            Inst::ParseError(err) => Some((*err).into()),
            _ => None,
        }
    }

    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            Error::Usage => {
                // Does not use binary name
                HaskellError::stdout("wspace 0.3 (c) 2003 Edwin Brady\n-------------------------------\nUsage: wspace [file]\n".to_owned())
            }
            Error::Parse(err) => err.to_haskell(wspace, filename),
            Error::Value(err) => err.to_haskell(wspace, filename),
            Error::Eager(err) => err.to_haskell(wspace, filename),
        }
    }
}

impl ParseError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            ParseError::IncompleteOpcode | ParseError::UnrecognizedOpcode => {
                HaskellError::stderr(wspace, "Unrecognised input\nCallStack (from HasCallStack):\n  error, called at Input.hs:103:11 in main:Input", 1)
            }
            ParseError::UnterminatedInteger => {
                HaskellError::stderr(wspace, "Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n", 1)
            }
            ParseError::UnterminatedLabel => {
                HaskellError::stderr(wspace, "Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n", 1)
            }
            ParseError::UndefinedLabel(l) => {
                HaskellError::stderr(wspace, &format!("user error (Undefined label ({}))", l.to_haskell_string()), 1)
            }
            ParseError::ImplicitEnd => {
                HaskellError::stderr(wspace, "Prelude.!!: index too large", 1)
            }
            ParseError::InvalidUtf8 => {
                HaskellError::stderr(wspace, &format!("{}: hGetContents: invalid argument (invalid byte sequence)", os_str_to_utf8_lossy_remove(filename)), 1)
            }
            ParseError::UnexpectedRiverCrab => panic!("not an error in wspace"),
        }
    }
}

impl ValueError {
    pub fn to_haskell(&self, wspace: &OsStr, _filename: &OsStr) -> HaskellError {
        HaskellError::stderr(wspace, &self.to_abstract_haskell().show(), 1)
    }

    pub fn to_abstract_haskell(&self) -> hs::Abort {
        match self {
            ValueError::EmptyLit => hs::Abort::error(
                "Prelude.last: empty list",
                hs::CallStack(vec![
                    ("last", SrcLoc!("Input.hs":119:7 in main:Input)),
                    ("lastError", ListSrcLoc!(191:29)),
                    ("errorEmptyList", ListSrcLoc!(196:13)),
                    ("error", ListSrcLoc!(1782:3)),
                ]),
            ),
            ValueError::CopyLarge | ValueError::RetrieveLarge => hs::Abort::error(
                "Prelude.!!: index too large",
                hs::CallStack(vec![
                    ("!!", SrcLoc!("VM.hs":63:33 in main:VM)),
                    ("tooLarge", ListSrcLoc!(1490:50)),
                    ("error", ListSrcLoc!(1480:14)),
                ]),
            ),
            ValueError::CopyNegative | ValueError::RetrieveNegative => hs::Abort::error(
                "Prelude.!!: negative index",
                hs::CallStack(vec![
                    ("!!", SrcLoc!("VM.hs":63:33 in main:VM)),
                    ("negIndex", ListSrcLoc!(1487:17)),
                    ("error", ListSrcLoc!(1483:12)),
                ]),
            ),
            ValueError::DivModZero => hs::Abort::DivZeroException,
            ValueError::ReadiParse => {
                hs::Abort::error_without_stack_trace("Prelude.read: no parse")
            }
        }
    }
}

impl EagerError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            EagerError::Underflow(inst) => {
                HaskellError::stderr(wspace, &format!("user error (Can't do {})", inst.show()), 1)
            }
            EagerError::StoreOverflow | EagerError::StoreNegative => {
                HaskellError::stderr_lines(wspace, &["Stack space overflow: current size 33624 bytes.", "Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it."], 2)
            }
            EagerError::RetUnderflow => {
                HaskellError::stderr(wspace, "user error (Can't do Return)", 1)
            }
            EagerError::Io(err) => panic!("{err}"),
            EagerError::PrintcInvalid(n) => {
                HaskellError::stderr(wspace, &format!("Prelude.chr: bad argument: {}", n.show()), 1)
            }
            EagerError::ReadEof => {
                HaskellError::stderr(wspace, "<stdin>: hGetChar: end of file", 1)
            }
            EagerError::ReadInvalidUtf8 => {
                HaskellError::stderr(wspace, &format!("{}: hGetContents: invalid argument (invalid byte sequence)", os_str_to_utf8_lossy_remove(filename)), 1)
            }
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        match self.out {
            OutKind::Stdout => {
                let mut w = stdout().lock();
                w.write_all(&self.msg).unwrap();
                w.flush().unwrap();
            }
            OutKind::Stderr => {
                let mut w = stderr().lock();
                w.write_all(&self.msg).unwrap();
                w.flush().unwrap();
            }
        }
        process::exit(self.code)
    }

    pub fn current_exe() -> OsString {
        // Haskell takes the last component after `/`, including when the last
        // component is `..`.
        // TODO: Determine how other path forms are handled.
        env::current_exe()
            .ok()
            .and_then(|p| p.components().next_back().map(|c| c.as_os_str().to_owned()))
            .unwrap_or_default()
    }

    #[inline]
    fn stdout(msg: String) -> Self {
        HaskellError {
            out: OutKind::Stdout,
            msg: msg.into_bytes(),
            code: 0,
        }
    }

    fn stderr(wspace: &OsStr, msg: &str, code: i32) -> Self {
        HaskellError {
            out: OutKind::Stderr,
            msg: Self::stderr_line(wspace, msg),
            code,
        }
    }

    fn stderr_lines(wspace: &OsStr, msgs: &[&str], code: i32) -> Self {
        let mut buf = Vec::new();
        for msg in msgs {
            buf.extend_from_slice(&Self::stderr_line(wspace, msg));
        }
        HaskellError {
            out: OutKind::Stderr,
            msg: buf,
            code,
        }
    }

    fn stderr_line(wspace: &OsStr, msg: &str) -> Vec<u8> {
        let wspace = os_str_to_bytes(wspace);
        let mut buf = Vec::with_capacity(wspace.len() + 2 + msg.len() + 1);
        buf.extend_from_slice(&wspace);
        buf.extend_from_slice(b": ");
        buf.extend_from_slice(msg.as_bytes());
        buf.push(b'\n');
        buf
    }
}

fn os_str_to_bytes(s: &OsStr) -> Cow<'_, [u8]> {
    cfg_if! {
        if #[cfg(unix)] {
            use std::os::unix::ffi::OsStrExt;
            s.as_bytes().into()
        } else if #[cfg(target_os = "wasi")] {
            use std::os::wasi::ffi::OsStrExt;
            s.as_bytes().into()
        } else {
            s.to_string_lossy()
        }
    }
}

#[allow(dead_code)]
fn os_string_into_bytes(s: OsString) -> Vec<u8> {
    // TODO: Replace with `OsStr::as_encoded_bytes`, once it's stabilized in
    // Rust 1.74.
    // https://github.com/rust-lang/rust/issues/111544
    // https://doc.rust-lang.org/beta/std/ffi/struct.OsStr.html#method.as_encoded_bytes

    cfg_if! {
        if #[cfg(unix)] {
            use std::os::unix::ffi::OsStringExt;
            s.into_vec()
        } else if #[cfg(target_os = "wasi")] {
            use std::os::wasi::ffi::OsStringExt;
            s.into_vec()
        } else if #[cfg(windows)] {
            s.into_string()
                .map(|s| s.into_bytes())
                .unwrap_or_else(|s| os_str_to_wtf8(&s))
        } else {
            s.into_string()
                .unwrap_or_else(|s| s.to_string_lossy().into_owned())
                .into_bytes()
        }
    }
}

#[cfg(windows)]
fn os_str_to_wtf8(s: &OsStr) -> Vec<u8> {
    use std::os::windows::ffi::OsStrExt;

    // A specialization of `String::from_utf16` allowing for invalid
    // sequences.
    let mut b = Vec::with_capacity(s.len());
    for ch in char::decode_utf16(s.encode_wide()) {
        let ch = ch.unwrap_or_else(|err| {
            // SAFETY: This deliberately produces an invalid char from
            // an unpaired surrogate. However, it is only used to encode
            // it to bytes and not kept as a char or in a str.
            // `transmute` bypasses the debug assertion in
            // `char::from_u32_unchecked`.
            unsafe { mem::transmute(err.unpaired_surrogate() as u32) }
        });
        b.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
    }
    b
}

fn os_str_to_utf8_lossy_remove(s: &OsStr) -> String {
    // TODO: This is incorrect when U+FFFD is in the input.
    s.to_string_lossy()
        .chars()
        .filter(|&ch| ch != '\u{fffd}')
        .collect()
}
