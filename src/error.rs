use std::fmt::{self, Display, Formatter};

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

// TODO: Substitute binary name

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(err) => write!(f, "{err}"),
            Error::Number(err) => write!(f, "{err}"),
            Error::Underflow(inst) => writeln!(f, "wspace: user error (Can't do {inst})"),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Unterminated(arg) => {
                match arg {
                    // Extra LFs are reproduced
                    ArgKind::Number => writeln!(
                        f,
                        "wspace: Input.hs:(108,5)-(109,51): Non-exhaustive patterns in function parseNum'\n"
                    ),
                    ArgKind::Label => writeln!(
                        f,
                        "wspace: Input.hs:(114,5)-(115,51): Non-exhaustive patterns in function parseStr'\n"
                    ),
                }
            }
            ParseError::ImplicitEnd => writeln!(f, "wspace: Prelude.!!: index too large"),
            // TODO: substitute program filename
            ParseError::InvalidUtf8 => writeln!(
                f,
                "wspace: <filename>: hGetContents: invalid argument (invalid byte sequence)"
            ),
        }
    }
}

impl Display for NumberError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            NumberError::EmptyLit => writeln!(f, "wspace: Prelude.last: empty list"),
            NumberError::CopyLarge | NumberError::RetrieveLarge => {
                writeln!(f, "wspace: Prelude.!!: index too large")
            }
            NumberError::CopyNegative | NumberError::RetrieveNegative => {
                writeln!(f, "wspace: Prelude.!!: negative index")
            }
            NumberError::DivModZero => writeln!(f, "wspace: divide by zero"),
            NumberError::Internal => panic!("BUG: cannot display internal error"),
        }
    }
}
