use std::mem;

use bitvec::{
    order::{BitOrder, LocalBits, Lsb0, Msb0},
    slice::{BitSlice, Iter as BitSliceIter},
    store::BitStore,
};

use crate::ws::lex::{Lexer, Span};
use crate::ws::Token;

#[derive(Clone, Debug)]
pub struct BitLexer {
    order: DynBitOrder,
}

#[derive(Clone, Debug)]
pub struct BitIter<'s, T = usize, O = Lsb0>
where
    T: BitStore,
    O: BitOrder,
{
    iter: BitSliceIter<'s, T, O>,
    len: usize,
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

impl BitLexer {
    #[inline]
    pub fn new(order: DynBitOrder) -> Self {
        BitLexer { order }
    }

    #[inline]
    pub fn lex<'s>(&self, src: &'s [u8]) -> Box<dyn Lexer + 's> {
        match self.order {
            DynBitOrder::Lsb0 => Box::new(BitIter::<u8, Lsb0>::new(src)),
            DynBitOrder::Msb0 => Box::new(BitIter::<u8, Msb0>::new(src)),
            DynBitOrder::LocalBits => Box::new(BitIter::<u8, LocalBits>::new(src)),
        }
    }
}

impl<'s, T: BitStore, O: BitOrder> BitIter<'s, T, O> {
    #[inline]
    pub fn new(src: &'s [T]) -> Self {
        let mut bits = BitSlice::from_slice(src);
        // Trim trailing zeros in the last element.
        let tz = bits.trailing_zeros();
        if 0 < tz && tz < mem::size_of::<T>() * 8 {
            bits = &bits[..bits.len() - tz];
        }
        Self::from_bitslice(bits)
    }

    #[inline]
    pub fn from_bitslice(bits: &'s BitSlice<T, O>) -> Self {
        BitIter {
            iter: bits.into_iter(),
            len: bits.len(),
        }
    }
}

impl<'s, T: BitStore, O: BitOrder> Lexer for BitIter<'s, T, O> {}

impl<'s, T: BitStore, O: BitOrder> Iterator for BitIter<'s, T, O> {
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
