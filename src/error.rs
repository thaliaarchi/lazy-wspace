use std::io::{stderr, Write};
use std::process;
use std::thread;
use std::time::Duration;

use crate::inst::{ArgKind, PrintableInst};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    Parse(ParseError),
    Number(NumberError),
    Underflow(PrintableInst),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    Unterminated(ArgKind),
    UnrecognizedInst,
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
    Internal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HaskellError {
    Error(String),
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

impl Error {
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            Error::Parse(err) => err.to_haskell(wspace, filename),
            Error::Number(err) => err.to_haskell(wspace, filename),
            Error::Underflow(inst) => {
                HaskellError::Error(format!("{wspace}: user error (Can't do {inst})\n"))
            }
        }
    }
}

impl ParseError {
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        HaskellError::Error(match self {
            ParseError::Unterminated(ArgKind::Number) => {
                format!("{wspace}: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n\n")
            }
            ParseError::Unterminated(ArgKind::Label) => {
                format!("{wspace}: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n\n")
            }
            ParseError::UnrecognizedInst => {
                format!("{wspace}: Unrecognised input\nCallStack (from HasCallStack):\n  error, called at Input.hs:103:11 in main:Input\n")
            }
            ParseError::ImplicitEnd => {
                format!("{wspace}: Prelude.!!: index too large\n")
            }
            ParseError::InvalidUtf8 => {
                format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n")
            }
        })
    }
}

impl NumberError {
    pub fn to_haskell(&self, wspace: &str, _filename: &str) -> HaskellError {
        match self {
            NumberError::EmptyLit => {
                HaskellError::Error(format!("{wspace}: Prelude.last: empty list\n"))
            }
            NumberError::CopyLarge | NumberError::RetrieveLarge => {
                HaskellError::Error(format!("{wspace}: Prelude.!!: index too large\n"))
            }
            NumberError::CopyNegative | NumberError::RetrieveNegative => {
                HaskellError::Error(format!("{wspace}: Prelude.!!: negative index\n"))
            }
            NumberError::StoreNegative => HaskellError::InfiniteLoop,
            NumberError::DivModZero => HaskellError::Error(format!("{wspace}: divide by zero\n")),
            NumberError::Internal => panic!("BUG: internal error"),
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        match self {
            HaskellError::Error(err) => {
                let mut w = stderr().lock();
                write!(w, "{err}").unwrap();
                w.flush().unwrap();
                process::exit(1);
            }
            HaskellError::InfiniteLoop => loop {
                thread::sleep(Duration::MAX);
            },
        }
    }
}
