use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

use rug::Integer;

use crate::error::UnderflowError;

/// Representation of the Haskell `Int` type, which is used for stack indexing.
///
/// The reference interpreter uses `Int` for the operands of `copy` and `slide`.
/// Parsing of number literals is done directly with `Int`, so literals do not
/// have arbitrary precision and are wrapped to be in bounds.
///
/// Its bounds are defined as `minBound :: Int` and `maxBound :: Int`, which are
/// [guaranteed](https://hackage.haskell.org/package/base/docs/Data-Int.html) to
/// be at least -2<sup>29</sup> ..= 2<sup>29</sup>-1. These are observed on
/// 64-bit systems to be -2<sup>63</sup> ..= 2<sup>63</sup>-1 for Debian, macOS
/// (x86-64), and Windows with GHC 9.6.2, and to be -2<sup>31</sup> ..=
/// 2<sup>31</sup>-1 for the Whitespace 0.3 [32-bit Linux binary](https://web.archive.org/web/20150717214201id_/http://compsoc.dur.ac.uk:80/whitespace/downloads/wspace).
///
/// It is represented here as an `isize`.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int(pub isize);

impl Int {
    pub const MIN: Int = Int(isize::MIN);
    pub const MAX: Int = Int(isize::MAX);

    #[inline]
    pub fn checked_add(self, rhs: Int) -> Option<Int> {
        self.0.checked_add(rhs.0).map(Int)
    }

    #[inline]
    pub fn add_or_underflow(self, rhs: Int) -> Result<Int, UnderflowError> {
        self.checked_add(rhs).ok_or(UnderflowError::Normal)
    }

    #[inline]
    pub fn as_isize(self) -> isize {
        self.0
    }
}

impl From<isize> for Int {
    #[inline]
    fn from(n: isize) -> Self {
        Int(n)
    }
}

impl From<Int> for isize {
    #[inline]
    fn from(n: Int) -> Self {
        n.0
    }
}

impl From<&Integer> for Int {
    #[inline]
    fn from(n: &Integer) -> Self {
        Int(n.to_isize_wrapping())
    }
}

impl PartialEq<isize> for Int {
    #[inline]
    fn eq(&self, other: &isize) -> bool {
        &self.0 == other
    }
}

impl PartialEq<Int> for isize {
    #[inline]
    fn eq(&self, other: &Int) -> bool {
        self == &other.0
    }
}

impl PartialOrd<isize> for Int {
    fn partial_cmp(&self, other: &isize) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<Int> for isize {
    fn partial_cmp(&self, other: &Int) -> Option<Ordering> {
        self.partial_cmp(&other.0)
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}
