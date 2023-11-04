use std::borrow::Cow;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt::Debug;
use std::hash::Hash;
use std::io::{stderr, stdout, Write};
use std::process;
use std::rc::Rc;
use std::str;

use rug::Integer;
use thiserror::Error;
use wspace_syntax::hs::{self, Show};
use wspace_syntax::ws::ast::{self, Inst, LabelLit};

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum Error {
    #[error(transparent)]
    Usage(#[from] UsageError),
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Value(#[from] ValueError),
    #[error(transparent)]
    Eager(#[from] EagerError),
}

#[derive(Clone, Copy, Debug, Error, PartialEq, Eq, Hash)]
pub enum UsageError {
    #[error("incorrect usage")]
    ArgumentCount,
    #[error("no such file or directory")]
    NotFound,
    #[error("source file is a directory")]
    IsADirectory,
    #[error("permission denied")]
    PermissionDenied,
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

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum EagerError {
    #[error("stack underflow: {0}")]
    Underflow(Inst),
    #[error("store address overflow")]
    StoreOverflow,
    #[error("store at negative address")]
    StoreNegative,
    #[error("call stack underflow")]
    RetUnderflow,
    #[error("printc: invalid codepoint")]
    PrintcInvalidRange(Rc<Integer>),
    #[error("printc: surrogate half")]
    PrintcSurrogate(u32),
    #[error("print: operation not permitted")]
    PrintPermissionDenied,
    #[error("flush: operation not permitted")]
    FlushPermissionDenied,
    #[error("broken pipe")]
    BrokenPipe,
    #[error("readc: EOF")]
    ReadcEof,
    #[error("readi: EOF")]
    ReadiEof,
    #[error("readc: invalid UTF-8 sequence")]
    ReadcInvalidUtf8,
    #[error("readi: invalid UTF-8 sequence")]
    ReadiInvalidUtf8,
}

#[derive(Clone, Copy, Debug, Default, Error, PartialEq, Eq, Hash)]
pub enum UnderflowError {
    #[default]
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

    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            Error::Usage(err) => err.to_haskell(wspace, filename),
            Error::Parse(err) => err.to_haskell(wspace, filename),
            Error::Value(err) => err.to_haskell(wspace, filename),
            Error::Eager(err) => err.to_haskell(wspace, filename),
        }
    }
}

impl UsageError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            // https://github.com/wspace/whitespace-haskell/blob/master/main.hs#L35
            UsageError::ArgumentCount => {
                // Does not use binary name
                HaskellError::stdout("wspace 0.3 (c) 2003 Edwin Brady\n-------------------------------\nUsage: wspace [file]\n".to_owned())
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L51
            UsageError::NotFound => {
                let filename = os_str_to_utf8_lossy_remove(filename);
                HaskellError::stderr(wspace, &format!("{filename}: openFile: does not exist (No such file or directory)"), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L51
            UsageError::IsADirectory => {
                let filename = os_str_to_utf8_lossy_remove(filename);
                HaskellError::stderr(wspace, &format!("{filename}: openFile: inappropriate type (is a directory)"), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L51
            UsageError::PermissionDenied => {
                let filename = os_str_to_utf8_lossy_remove(filename);
                HaskellError::stderr(wspace, &format!("{filename}: openFile: permission denied (Permission denied)"), 1)
            }
        }
    }
}

impl ParseError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, filename: &OsStr) -> HaskellError {
        match self {
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L103
            ParseError::IncompleteOpcode | ParseError::UnrecognizedOpcode => {
                let err = hs::Abort::error("Unrecognised input", hs::call_stack![
                    error at "Input.hs":103:11 in main:Input,
                ]);
                HaskellError::stderr(wspace, &err.show(), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L108-L109
            ParseError::UnterminatedInteger => {
                HaskellError::stderr(wspace, "Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L114-L115
            ParseError::UnterminatedLabel => {
                HaskellError::stderr(wspace, "Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L127
            ParseError::UndefinedLabel(l) => {
                let err = hs::Abort::user_error(format!("Undefined label ({})", l.to_haskell_string()));
                HaskellError::stderr(wspace, &err.show(), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L51
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1490
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1479-1480
            ParseError::ImplicitEnd => {
                let err = hs::Abort::error("Prelude.!!: index too large", hs::call_stack![
                    "!!" at "VM.hs":51:19 in main:VM,
                    tooLarge at "libraries/base/GHC/List.hs":1490:50 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1480:14 in base:"GHC.List",
                ]);
                HaskellError::stderr(wspace, &err.show(), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L51
            ParseError::InvalidUtf8 => {
                let filename = os_str_to_utf8_lossy_remove(filename);
                // TODO: Print bad byte.
                HaskellError::stderr(wspace, &format!("{filename}: hGetContents: invalid argument (cannot decode byte sequence starting from ...)"), 1)
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
            // https://github.com/wspace/whitespace-haskell/blob/master/Input.hs#L119
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L191-196
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1780-1782
            ValueError::EmptyLit => hs::Abort::error(
                "Prelude.last: empty list",
                hs::call_stack![
                    last at "Input.hs":119:7 in main:Input,
                    lastError at "libraries/base/GHC/List.hs":191:29 in base:"GHC.List",
                    errorEmptyList at "libraries/base/GHC/List.hs":196:13 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1782:3 in base:"GHC.List",
                ],
            ),
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L63
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1490
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1479-1480
            ValueError::CopyLarge => hs::Abort::error(
                "Prelude.!!: index too large",
                hs::call_stack![
                    "!!" at "VM.hs":63:33 in main:VM,
                    tooLarge at "libraries/base/GHC/List.hs":1490:50 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1480:14 in base:"GHC.List",
                ],
            ),
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L63
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1487
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1482-1483
            ValueError::CopyNegative => hs::Abort::error(
                "Prelude.!!: negative index",
                hs::call_stack![
                    "!!" at "VM.hs":63:33 in main:VM,
                    negIndex at "libraries/base/GHC/List.hs":1487:17 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1483:12 in base:"GHC.List",
                ],
            ),
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L75-L76
            ValueError::DivModZero => hs::Abort::DivZeroException,
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L136
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1490
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1479-1480
            ValueError::RetrieveLarge => hs::Abort::error(
                "Prelude.!!: index too large",
                hs::call_stack![
                    "!!" at "VM.hs":136:31 in main:VM,
                    tooLarge at "libraries/base/GHC/List.hs":1490:50 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1480:14 in base:"GHC.List",
                ],
            ),
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L136
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1487
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/List.hs#L1482-1483
            ValueError::RetrieveNegative => hs::Abort::error(
                "Prelude.!!: negative index",
                hs::call_stack![
                    "!!" at "VM.hs":136:31 in main:VM,
                    negIndex at "libraries/base/GHC/List.hs":1487:17 in base:"GHC.List",
                    error at "libraries/base/GHC/List.hs":1483:12 in base:"GHC.List",
                ],
            ),
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L87
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Text/Read.hs#L113
            // https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Text/Read.hs#L79
            ValueError::ReadiParse => {
                hs::Abort::error_without_stack_trace("Prelude.read: no parse")
            }
        }
    }
}

impl EagerError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &OsStr, _filename: &OsStr) -> HaskellError {
        match self {
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L120
            EagerError::Underflow(inst) => {
                let err = hs::Abort::user_error(format!("Can't do {}", inst.show()));
                HaskellError::stderr(wspace, &err.show(), 1)
            }
            EagerError::StoreOverflow | EagerError::StoreNegative => {
                HaskellError::stderr_lines(wspace, &["Stack space overflow: current size 33616 bytes.", "Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it."], 2)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L120
            EagerError::RetUnderflow => {
                let err = hs::Abort::user_error("Can't do Return");
                HaskellError::stderr(wspace, &err.show(), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L78
            EagerError::PrintcInvalidRange(n) => {
                HaskellError::stderr(wspace, &format!("Prelude.chr: bad argument: {}", n.show()), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L78
            // TODO: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Encoding/Failure.hs#L217-222
            EagerError::PrintcSurrogate(n) => {
                HaskellError::stderr(wspace, &format!("<stdout>: hPutChar: invalid argument (cannot encode character '\\{n}')"), 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L91
            EagerError::PrintPermissionDenied => {
                HaskellError::stderr(wspace, "<stdout>: commitBuffer: permission denied (Operation not permitted)", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L79
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L92
            EagerError::FlushPermissionDenied => {
                // TODO: Test with read-only file.
                HaskellError::stderr(wspace, "<stdout>: hFlush: permission denied (Operation not permitted)", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L78-L79
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L91-L92
            EagerError::BrokenPipe => {
                // wspace appears to suppress SIGPIPE.
                HaskellError::stdout(String::new())
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L82
            EagerError::ReadcEof => {
                HaskellError::stderr(wspace, "<stdin>: hGetChar: end of file", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L86
            EagerError::ReadiEof => {
                HaskellError::stderr(wspace, "<stdin>: hGetLine: end of file", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L82
            // TODO: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Encoding/Failure.hs#L212-215
            EagerError::ReadcInvalidUtf8 => {
                // TODO: Print bad byte.
                HaskellError::stderr(wspace, "<stdin>: hGetChar: invalid argument (cannot decode byte sequence starting from ...)", 1)
            }
            // https://github.com/wspace/whitespace-haskell/blob/master/VM.hs#L86
            // TODO: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Encoding/Failure.hs#L212-215
            EagerError::ReadiInvalidUtf8 => {
                // TODO: Print bad byte.
                HaskellError::stderr(wspace, "<stdin>: hGetLine: invalid argument (cannot decode byte sequence starting from ...)", 1)
            }
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        if !self.msg.is_empty() {
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
        let wspace = wspace.as_encoded_bytes();
        let mut buf = Vec::with_capacity(wspace.len() + 2 + msg.len() + 1);
        buf.extend_from_slice(&wspace);
        buf.extend_from_slice(b": ");
        buf.extend_from_slice(msg.as_bytes());
        buf.push(b'\n');
        buf
    }
}

/// Removes sequences of invalid UTF-8 from the `OsStr` to produce valid UTF-8.
///
/// This is like `OsStr::to_string_lossy`, except it removes invalid sequences,
/// instead of replacing them with U+FFFD.
fn os_str_to_utf8_lossy_remove(s: &OsStr) -> Cow<'_, str> {
    let mut s = s.as_encoded_bytes();
    match simdutf8::compat::from_utf8(s) {
        Ok(s) => s.into(),
        Err(mut err) => {
            let mut cleaned = String::with_capacity(s.len());
            loop {
                cleaned += unsafe { str::from_utf8_unchecked(&s[..err.valid_up_to()]) };
                if let Some(error_len) = err.error_len() {
                    s = &s[err.valid_up_to() + error_len..];
                    match simdutf8::compat::from_utf8(s) {
                        Ok(s) => cleaned += s,
                        Err(err1) => {
                            err = err1;
                            continue;
                        }
                    }
                }
                break cleaned.into();
            }
        }
    }
}
