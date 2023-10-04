use std::iter::FusedIterator;
use std::mem;

use bitvec::{
    order::{BitOrder, LocalBits, Lsb0, Msb0},
    slice::{BitSlice, Iter as BitIter},
    store::BitStore,
};

use crate::ws::lex::{Lexer, Span};
use crate::ws::Token;

#[derive(Clone, Debug)]
pub struct BitLexer<'a, O = Lsb0, T = u8>
where
    O: BitOrder,
    T: BitStore,
{
    iter: BitIter<'a, T, O>,
    len: usize,
}

#[derive(Clone, Debug)]
pub enum DynBitLexer<'a, T: BitStore = u8> {
    Lsb0(BitLexer<'a, Lsb0, T>),
    Msb0(BitLexer<'a, Msb0, T>),
}

#[derive(Clone, Copy, Debug)]
pub enum DynBitOrder {
    /// Corresponds to [`bitvec::order::Lsb0`].
    Lsb0,
    /// Corresponds to [`bitvec::order::Msb0`].
    Msb0,
    /// Corresponds to [`bitvec::order::LocalBits`].
    LocalBits,
}

impl DynBitOrder {
    #[inline]
    pub fn lex<'a, T: BitStore>(&self, src: &'a [T]) -> DynBitLexer<'a, T> {
        match self {
            DynBitOrder::Lsb0 => DynBitLexer::from(BitLexer::<Lsb0, T>::new(src)),
            DynBitOrder::Msb0 => DynBitLexer::from(BitLexer::<Msb0, T>::new(src)),
            DynBitOrder::LocalBits => DynBitLexer::from(BitLexer::<LocalBits, T>::new(src)),
        }
    }
}

impl<'a, O: BitOrder, T: BitStore> BitLexer<'a, O, T> {
    #[inline]
    pub fn new(src: &'a [T]) -> Self {
        let mut bits = BitSlice::from_slice(src);
        // Trim trailing zeros in the last element.
        let tz = bits.trailing_zeros();
        if 0 < tz && tz < mem::size_of::<T>() * 8 {
            bits = &bits[..bits.len() - tz];
        }
        BitLexer::from(bits)
    }
}

impl<'a, O: BitOrder, T: BitStore> From<&'a BitSlice<T, O>> for BitLexer<'a, O, T> {
    #[inline]
    fn from(bits: &'a BitSlice<T, O>) -> Self {
        BitLexer {
            iter: bits.iter(),
            len: bits.len(),
        }
    }
}

impl<O: BitOrder, T: BitStore> Lexer for BitLexer<'_, O, T> {}

impl<O: BitOrder, T: BitStore> Iterator for BitLexer<'_, O, T> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.len - self.iter.len();
        let tok = match self.iter.next().as_deref() {
            Some(true) => match self.iter.next().as_deref() {
                Some(true) => Token::L,
                Some(false) => Token::T,
                None => return None, // Marker bit
            },
            Some(false) => Token::S,
            None => return None, // EOF
        };
        let end = self.len - self.iter.len();
        Some((tok, Span::from(start..end)))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        // Lower bound is the case of all two-bit tokens, rounded up, and upper
        // bound is all one-bit tokens.
        ((self.iter.len() + 1) / 2, Some(self.iter.len()))
    }
}

impl<O: BitOrder, T: BitStore> FusedIterator for BitLexer<'_, O, T> {}

impl<T: BitStore> Lexer for DynBitLexer<'_, T> {}

impl<T: BitStore> Iterator for DynBitLexer<'_, T> {
    type Item = (Token, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            DynBitLexer::Lsb0(inner) => inner.next(),
            DynBitLexer::Msb0(inner) => inner.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            DynBitLexer::Lsb0(inner) => inner.size_hint(),
            DynBitLexer::Msb0(inner) => inner.size_hint(),
        }
    }
}

impl<T: BitStore> FusedIterator for DynBitLexer<'_, T> {}

impl<'a, T: BitStore> From<BitLexer<'a, Lsb0, T>> for DynBitLexer<'a, T> {
    #[inline]
    fn from(lex: BitLexer<'a, Lsb0, T>) -> Self {
        DynBitLexer::Lsb0(lex)
    }
}

impl<'a, T: BitStore> From<BitLexer<'a, Msb0, T>> for DynBitLexer<'a, T> {
    #[inline]
    fn from(lex: BitLexer<'a, Msb0, T>) -> Self {
        DynBitLexer::Msb0(lex)
    }
}
