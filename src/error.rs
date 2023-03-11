use std::fmt::{self, Display, Formatter};

use crate::inst::Inst;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LazyError {
    EmptyLit,
    CopyLarge,
    CopyNegative,
    DivModZero,
    RetrieveLarge,
    RetrieveNegative,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    Lazy(LazyError),
    Underflow(Inst),
}

impl From<LazyError> for Error {
    #[inline]
    fn from(err: LazyError) -> Self {
        Error::Lazy(err)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lazy(err) => write!(f, "{err}"),
            Error::Underflow(inst) => writeln!(f, "wspace: user error (Can't do {inst})"),
        }
    }
}

impl Display for LazyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LazyError::EmptyLit => writeln!(f, "wspace: Prelude.last: empty list"),
            LazyError::CopyLarge | LazyError::RetrieveLarge => {
                writeln!(f, "wspace: Prelude.!!: index too large")
            }
            LazyError::CopyNegative | LazyError::RetrieveNegative => {
                writeln!(f, "wspace: Prelude.!!: negative index")
            }
            LazyError::DivModZero => writeln!(f, "wspace: divide by zero"),
        }
    }
}
