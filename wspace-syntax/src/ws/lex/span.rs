use std::fmt::{self, Debug, Formatter};
use std::ops::{Index, IndexMut, Range};

/// A range of byte offsets in a contiguous region of bytes.
///
/// `Span` is essentially [`Range<usize>`], but with `Copy`.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Span {
    /// The start byte offset of the span, inclusive.
    pub start: usize,
    /// The end byte offset of the span, exclusive.
    pub end: usize,
}

impl Span {
    /// Returns the length of this span.
    #[inline]
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Returns whether this span is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    /// Returns true when the given offset is contained within this span.
    #[inline]
    pub fn contains(&self, offset: usize) -> bool {
        self.start <= offset && offset < self.end
    }

    /// Returns this span as a range.
    #[inline]
    pub fn range(&self) -> Range<usize> {
        Range::from(*self)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Index<Span> for [u8] {
    type Output = [u8];

    #[inline]
    fn index(&self, index: Span) -> &[u8] {
        &self[index.range()]
    }
}

impl IndexMut<Span> for [u8] {
    #[inline]
    fn index_mut(&mut self, index: Span) -> &mut [u8] {
        &mut self[index.range()]
    }
}

impl Index<Span> for str {
    type Output = str;

    #[inline]
    fn index(&self, index: Span) -> &str {
        &self[index.range()]
    }
}

impl From<Range<usize>> for Span {
    #[inline]
    fn from(range: Range<usize>) -> Span {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for Range<usize> {
    #[inline]
    fn from(span: Span) -> Range<usize> {
        Range {
            start: span.start,
            end: span.end,
        }
    }
}

impl PartialEq<Range<usize>> for Span {
    #[inline]
    fn eq(&self, other: &Range<usize>) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl PartialEq<Span> for Range<usize> {
    #[inline]
    fn eq(&self, other: &Span) -> bool {
        self.start == other.start && self.end == other.end
    }
}
