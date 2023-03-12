use std::fmt::Formatter;
use std::process;
use std::thread;
use std::time::Duration;

use crate::inst::{ArgKind, Inst};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    Parse(ParseError),
    Number(NumberError),
    Underflow(Inst),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    Unterminated(ArgKind),
    ImplicitEnd,
    InvalidUtf8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl From<NumberError> for Error {
    #[inline]
    fn from(err: NumberError) -> Self {
        Error::Number(err)
    }
}

impl Error {
    pub fn handle_wspace(&self, f: &mut Formatter<'_>, wspace: &str, filename: &str) -> ! {
        match self {
            Error::Parse(err) => err.handle_wspace(f, wspace, filename),
            Error::Number(err) => err.handle_wspace(f, wspace, filename),
            Error::Underflow(inst) => {
                writeln!(f, "{wspace}: user error (Can't do {inst})").unwrap();
                process::exit(1)
            }
        }
    }
}

impl ParseError {
    pub fn handle_wspace(&self, f: &mut Formatter<'_>, wspace: &str, filename: &str) -> ! {
        match self {
            ParseError::Unterminated(arg) => {
                match arg {
                    // Extra LFs are reproduced
                    ArgKind::Number => writeln!(
                        f,
                        "{wspace}: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n"
                    ),
                    ArgKind::Label => writeln!(
                        f,
                        "{wspace}: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n"
                    ),
                }
            }
            ParseError::ImplicitEnd => writeln!(f, "{wspace}: Prelude.!!: index too large"),
            ParseError::InvalidUtf8 => writeln!(
                f,
                "{wspace}: {filename}: hGetContents: invalid argument (invalid byte sequence)"
            ),
        }
        .unwrap();
        process::exit(1)
    }
}

impl NumberError {
    pub fn handle_wspace(&self, f: &mut Formatter<'_>, wspace: &str, _filename: &str) -> ! {
        match self {
            NumberError::EmptyLit => writeln!(f, "{wspace}: Prelude.last: empty list"),
            NumberError::CopyLarge | NumberError::RetrieveLarge => {
                writeln!(f, "{wspace}: Prelude.!!: index too large")
            }
            NumberError::CopyNegative | NumberError::RetrieveNegative => {
                writeln!(f, "{wspace}: Prelude.!!: negative index")
            }
            NumberError::StoreNegative => loop {
                thread::sleep(Duration::MAX);
            },
            NumberError::DivModZero => writeln!(f, "{wspace}: divide by zero"),
            NumberError::Internal => panic!("BUG: internal error"),
        }
        .unwrap();
        process::exit(1)
    }
}
