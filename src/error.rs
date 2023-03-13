use std::io::{stderr, Write};
use std::process;
use std::rc::Rc;
use std::thread;
use std::time::Duration;

use rug::Integer;

use crate::inst::{ArgKind, LabelLit, PrintableInst};
use crate::number::IntegerExt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    Parse(ParseError),
    Number(NumberError),
    Underflow(PrintableInst),
    StoreOverflow,
    RetUnderflow,
    PrintcInvalid(Rc<Integer>),
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
    Internal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HaskellError {
    Error(String, i32),
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
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            Error::Parse(err) => err.to_haskell(wspace, filename),
            Error::Number(err) => err.to_haskell(wspace, filename),
            Error::Underflow(inst) => {
                HaskellError::Error(format!("{wspace}: user error (Can't do {inst})\n"), 1)
            }
            Error::StoreOverflow => {
                HaskellError::Error(format!("{wspace}: Stack space overflow: current size 33624 bytes.\n{wspace}: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it.\n"), 2)
            }
            Error::RetUnderflow => {
                HaskellError::Error(format!("{wspace}: user error (Can't do Return)\n"), 1)
            }
            Error::PrintcInvalid(n) => {
                HaskellError::Error(format!("{wspace}: Prelude.chr: bad argument: {}", n.to_haskell_show()), 1)
            }
        }
    }
}

impl ParseError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, filename: &str) -> HaskellError {
        match self {
            ParseError::IncompleteInst | ParseError::UnrecognizedInst => {
                HaskellError::Error(format!("{wspace}: Unrecognised input\nCallStack (from HasCallStack):\n  error, called at Input.hs:103:11 in main:Input\n"), 1)
            }
            ParseError::UnterminatedArg(ArgKind::Number) => {
                HaskellError::Error(format!("{wspace}: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n\n"), 1)
            }
            ParseError::UnterminatedArg(ArgKind::Label) => {
                HaskellError::Error(format!("{wspace}: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n\n"), 1)
            }
            ParseError::UndefinedLabel(l) => {
                HaskellError::Error(format!("{wspace}: user error (Undefined label ({}))\n", l.to_haskell_string()), 1)
            }
            ParseError::ImplicitEnd => {
                HaskellError::Error(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            ParseError::InvalidUtf8 => {
                HaskellError::Error(format!("{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)\n"), 1)
            }
        }
    }
}

impl NumberError {
    #[rustfmt::skip]
    pub fn to_haskell(&self, wspace: &str, _filename: &str) -> HaskellError {
        match self {
            NumberError::EmptyLit => {
                HaskellError::Error(format!("{wspace}: Prelude.last: empty list\n"), 1)
            }
            NumberError::CopyLarge | NumberError::RetrieveLarge => {
                HaskellError::Error(format!("{wspace}: Prelude.!!: index too large\n"), 1)
            }
            NumberError::CopyNegative | NumberError::RetrieveNegative => {
                HaskellError::Error(format!("{wspace}: Prelude.!!: negative index\n"), 1)
            }
            NumberError::StoreNegative => HaskellError::InfiniteLoop,
            NumberError::DivModZero => {
                HaskellError::Error(format!("{wspace}: divide by zero\n"), 1)
            }
            NumberError::Internal => panic!("BUG: internal error"),
        }
    }
}

impl HaskellError {
    pub fn handle(&self) -> ! {
        match self {
            HaskellError::Error(err, code) => {
                let mut w = stderr().lock();
                write!(w, "{err}").unwrap();
                w.flush().unwrap();
                process::exit(*code);
            }
            HaskellError::InfiniteLoop => loop {
                thread::sleep(Duration::MAX);
            },
        }
    }
}
