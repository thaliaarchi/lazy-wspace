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
pub struct BitSyntax {
    order: DynBitOrder,
}

#[derive(Clone, Debug)]
pub struct BitLexer<'a, T = u8, O = Lsb0>
where
    T: BitStore,
    O: BitOrder,
{
    iter: BitIter<'a, T, O>,
    len: usize,
}

#[derive(Clone, Debug)]
pub enum DynBitLexer<'a, T: BitStore = u8> {
    Lsb0(BitLexer<'a, T, Lsb0>),
    Msb0(BitLexer<'a, T, Msb0>),
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

impl BitSyntax {
    #[inline]
    pub fn new(order: DynBitOrder) -> Self {
        BitSyntax { order }
    }

    #[inline]
    pub fn lex<'a, T: BitStore>(&self, src: &'a [T]) -> DynBitLexer<'a, T> {
        match self.order {
            DynBitOrder::Lsb0 => DynBitLexer::from(BitLexer::<T, Lsb0>::new(src)),
            DynBitOrder::Msb0 => DynBitLexer::from(BitLexer::<T, Msb0>::new(src)),
            DynBitOrder::LocalBits => DynBitLexer::from(BitLexer::<T, LocalBits>::new(src)),
        }
    }

    #[inline]
    pub fn order(&self) -> DynBitOrder {
        self.order
    }
}

impl<'a, T: BitStore, O: BitOrder> BitLexer<'a, T, O> {
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

impl<'a, T: BitStore, O: BitOrder> From<&'a BitSlice<T, O>> for BitLexer<'a, T, O> {
    #[inline]
    fn from(bits: &'a BitSlice<T, O>) -> Self {
        BitLexer {
            iter: bits.iter(),
            len: bits.len(),
        }
    }
}

impl<T: BitStore, O: BitOrder> Lexer for BitLexer<'_, T, O> {}

impl<T: BitStore, O: BitOrder> Iterator for BitLexer<'_, T, O> {
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

impl<T: BitStore, O: BitOrder> FusedIterator for BitLexer<'_, T, O> {}

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

impl<'a, T: BitStore> From<BitLexer<'a, T, Lsb0>> for DynBitLexer<'a, T> {
    #[inline]
    fn from(lex: BitLexer<'a, T, Lsb0>) -> Self {
        DynBitLexer::Lsb0(lex)
    }
}

impl<'a, T: BitStore> From<BitLexer<'a, T, Msb0>> for DynBitLexer<'a, T> {
    #[inline]
    fn from(lex: BitLexer<'a, T, Msb0>) -> Self {
        DynBitLexer::Msb0(lex)
    }
}
