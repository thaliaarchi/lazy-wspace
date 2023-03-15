use std::fmt::Debug;
use std::io::{self, stderr, stdout, ErrorKind, Write};
use std::rc::Rc;
use std::thread;
use std::time::Duration;
use std::{mem, process};

use rug::Integer;

use crate::inst::{ArgKind, LabelLit, PrintableInst};
use crate::number::IntegerExt;

#[derive(Debug)]
pub enum Error {
    Usage,
    Parse(ParseError),
    Number(NumberError),
    Underflow(PrintableInst),
    StoreOverflow,
    RetUnderflow,
    Io(io::Error),
    PrintcInvalid(Rc<Integer>),
    ReadEof,
    ReadInvalidUtf8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    IncompleteInst,
    UnrecognizedInst,
    UnterminatedArg(ArgKind),
    UndefinedLabel(LabelLit),
    ImplicitEnd,
    InvalidUtf8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberError {
    EmptyLit,
    CopyLarge,
    CopyNegative,
    DivModZero,
    StoreNegative,
    RetrieveLarge,
    RetrieveNegative,
    ReadiParse,
    Internal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HaskellError {
    Stderr(String, i32),
    Stdout(String),
    InfiniteLoop,
}

impl From<ParseError> for Error {
    #[inline]
    fn from(err: ParseError) -> Self {
        Error::Parse(err)
    }
}

impl From<NumberError> for Error {
    #[inline]
    fn from(err: NumberError) -> Self {
        Error::Number(err)
    }
}

impl From<io::Error> for Error {
    #[inline]
    fn from(err: io::Error) -> Self {
        if err.kind() == ErrorKind::UnexpectedEof {
            Error::ReadEof
        } else {
            Error::Io(err)
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Error::Parse(err1), Error::Parse(err2)) => err1 == err2,
            (Error::Number(err1), Error::Number(err2)) => err1 == err2,
            (Error::Underflow(inst1), Error::Underflow(inst2)) => inst1 == inst2,
            (Error::Io(err1), Error::Io(err2)) => err1.kind() == err2.kind(),
            (Error::PrintcInvalid(err1), Error::PrintcInvalid(err2)) => err1 == err2,
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}

impl Eq for Error {}

impl Error {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            Error::Usage => {
                // Does not use binary name
                HaskellError::Stdout("wspace 0.3 (c) 2003 Edwin Brady\n-------------------------------\nUsage: wspace [file]\n".to_owned())
            }
            Error::Parse(err) => err.to_haskell(wspace, filename),
            Error::Number(err) => err.to_haskell(wspace, filename),
            Error::Underflow(inst) => {
                HaskellError::Stderr(format!("{wspace}: user error (Can't do {inst})\n"), 1)
            }
            Error::StoreOverflow => {
                HaskellError::Stderr(format!("{wspace}: Stack space overflow: current size 33624 bytes.\n{wspace}: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it.\n"), 2)
            }
            Error::RetUnderflow => {
                HaskellError::Stderr(format!("{wspace}: user error (Can't do Return)\n"), 1)
            }
            Error::Io(err) => panic!("{err}"),
            Error::PrintcInvalid(n) => {
                HaskellError::Stderr(format!("{wspace}: Prelude.chr: bad argument: {}", n.to_haskell_show()), 1)
            }
            Error::ReadEof => {
                HaskellError::Stderr(format!("{wspace}: <stdin>: hGetChar: end of file\n"), 1)
            }
            Error::ReadInvalidUtf8 => {
                HaskellError::Stderr(format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n"), 1)
            }
        }
    }
}

impl ParseError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            ParseError::IncompleteInst | ParseError::UnrecognizedInst => {
                HaskellError::Stderr(format!("{wspace}: Unrecognised input\nCallStack (from HasCallStack):\n  error, called at Input.hs:103:11 in main:Input\n"), 1)
            }
            ParseError::UnterminatedArg(ArgKind::Number) => {
                HaskellError::Stderr(format!("{wspace}: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n\n"), 1)
            }
            ParseError::UnterminatedArg(ArgKind::Label) => {
                HaskellError::Stderr(format!("{wspace}: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n\n"), 1)
            }
            ParseError::UndefinedLabel(l) => {
                HaskellError::Stderr(format!("{wspace}: user error (Undefined label ({}))\n", l.to_haskell_string()), 1)
            }
            ParseError::ImplicitEnd => {
                HaskellError::Stderr(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            ParseError::InvalidUtf8 => {
                HaskellError::Stderr(format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n"), 1)
            }
        }
    }
}

impl NumberError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, _filename: &str) -> HaskellError {
        match self {
            NumberError::EmptyLit => {
                HaskellError::Stderr(format!("{wspace}: Prelude.last: empty list\n"), 1)
            }
            NumberError::CopyLarge | NumberError::RetrieveLarge => {
                HaskellError::Stderr(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            NumberError::CopyNegative | NumberError::RetrieveNegative => {
                HaskellError::Stderr(format!("{wspace}: Prelude.!!: negative index\n"), 1)
            }
            NumberError::StoreNegative => HaskellError::InfiniteLoop,
            NumberError::DivModZero => {
                HaskellError::Stderr(format!("{wspace}: divide by zero\n"), 1)
            }
            NumberError::ReadiParse => {
                HaskellError::Stderr(format!("{wspace}: Prelude.read: no parse\n"), 1)
            },
            NumberError::Internal => panic!("BUG: internal error"),
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        match self {
            HaskellError::Stderr(err, code) => {
                let mut w = stderr().lock();
                write!(w, "{err}").unwrap();
                w.flush().unwrap();
                process::exit(*code);
            }
            HaskellError::Stdout(err) => {
                let mut w = stdout().lock();
                write!(w, "{err}").unwrap();
                w.flush().unwrap();
                process::exit(0);
            }
            HaskellError::InfiniteLoop => loop {
                thread::sleep(Duration::MAX);
            },
        }
    }
}
