use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::io::{self, stderr, stdout, ErrorKind, Write};
use std::mem;
use std::process;
use std::rc::Rc;
use thiserror::Error;

use rug::Integer;
use wspace_syntax::hs::Show;

use crate::ast::{Inst, LabelLit};

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
    IncompleteInst,
    #[error("unrecognized instruction opcode")]
    UnrecognizedInst,
    #[error("unterminated integer")]
    UnterminatedInteger,
    #[error("unterminated label")]
    UnterminatedLabel,
    #[error("undefined label {0}")]
    UndefinedLabel(LabelLit),
    #[error("implicit end")]
    ImplicitEnd,
    #[error("invalid UTF-8 sequence")]
    InvalidUtf8,
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

#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
#[error("{msg}")]
pub struct HaskellError {
    out: OutKind,
    msg: String,
    code: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OutKind {
    Stdout,
    Stderr,
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
            UnderflowError::Normal => inst
                .err()
                .unwrap_or_else(|| EagerError::Underflow(inst.clone()).into()),
            UnderflowError::SlideEmpty => ValueError::EmptyLit.into(),
        }
    }
}

impl Error {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
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
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            ParseError::IncompleteInst | ParseError::UnrecognizedInst => {
                HaskellError::stderr(format!("{wspace}: Unrecognised input\nCallStack (from HasCallStack):\n  error, called at Input.hs:103:11 in main:Input\n"), 1)
            }
            ParseError::UnterminatedInteger => {
                HaskellError::stderr(format!("{wspace}: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n\n"), 1)
            }
            ParseError::UnterminatedLabel => {
                HaskellError::stderr(format!("{wspace}: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n\n"), 1)
            }
            ParseError::UndefinedLabel(l) => {
                HaskellError::stderr(format!("{wspace}: user error (Undefined label ({}))\n", l.to_haskell_string()), 1)
            }
            ParseError::ImplicitEnd => {
                HaskellError::stderr(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            ParseError::InvalidUtf8 => {
                HaskellError::stderr(format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n"), 1)
            }
        }
    }
}

impl ValueError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, _filename: &str) -> HaskellError {
        match self {
            ValueError::EmptyLit => {
                HaskellError::stderr(format!("{wspace}: Prelude.last: empty list\n"), 1)
            }
            ValueError::CopyLarge | ValueError::RetrieveLarge => {
                HaskellError::stderr(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            ValueError::CopyNegative | ValueError::RetrieveNegative => {
                HaskellError::stderr(format!("{wspace}: Prelude.!!: negative index\n"), 1)
            }
            ValueError::DivModZero => {
                HaskellError::stderr(format!("{wspace}: divide by zero\n"), 1)
            }
            ValueError::ReadiParse => {
                HaskellError::stderr(format!("{wspace}: Prelude.read: no parse\n"), 1)
            },

        }
    }
}

impl EagerError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            EagerError::Underflow(inst) => {
                HaskellError::stderr(format!("{wspace}: user error (Can't do {})\n", inst.show()), 1)
            }
            EagerError::StoreOverflow | EagerError::StoreNegative => {
                HaskellError::stderr(format!("{wspace}: Stack space overflow: current size 33624 bytes.\n{wspace}: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it.\n"), 2)
            }
            EagerError::RetUnderflow => {
                HaskellError::stderr(format!("{wspace}: user error (Can't do Return)\n"), 1)
            }
            EagerError::Io(err) => panic!("{err}"),
            EagerError::PrintcInvalid(n) => {
                HaskellError::stderr(format!("{wspace}: Prelude.chr: bad argument: {}", n.show()), 1)
            }
            EagerError::ReadEof => {
                HaskellError::stderr(format!("{wspace}: <stdin>: hGetChar: end of file\n"), 1)
            }
            EagerError::ReadInvalidUtf8 => {
                HaskellError::stderr(format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n"), 1)
            }
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        match self.out {
            OutKind::Stdout => {
                let mut w = stdout().lock();
                write!(w, "{}", self.msg).unwrap();
                w.flush().unwrap();
            }
            OutKind::Stderr => {
                let mut w = stderr().lock();
                write!(w, "{}", self.msg).unwrap();
                w.flush().unwrap();
            }
        }
        process::exit(self.code)
    }

    #[inline]
    pub fn stdout(msg: String) -> Self {
        HaskellError {
            out: OutKind::Stdout,
            msg,
            code: 0,
        }
    }

    #[inline]
    pub fn stderr(msg: String, code: i32) -> Self {
        HaskellError {
            out: OutKind::Stderr,
            msg,
            code,
        }
    }
}
