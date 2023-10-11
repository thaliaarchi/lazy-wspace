use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

use bitvec::prelude::*;
use rug::{integer::Order, ops::NegAssign, Integer};

use crate::hs;

/// Integer literal with significant leading zeros and sign in its bitwise
/// representation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntegerLit {
    value: Integer,
    sign: Sign,
    leading_zeros: usize,
}

/// Sign of an integer literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Sign {
    Pos,
    Neg,
    Empty,
}

/// Whitespace label literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LabelLit {
    bits: BitVec,
}

impl IntegerLit {
    /// Constructs an integer literal with a given value and no leading zeros in
    /// its binary representation.
    pub fn new<T: Into<Integer>>(value: T) -> Self {
        IntegerLit::new_leading(value, 0)
    }

    /// Constructs an integer literal with a given value and a number of leading
    /// zeros in its binary representation.
    pub fn new_leading<T: Into<Integer>>(value: T, leading_zeros: usize) -> Self {
        let value = value.into();
        let sign = if value.is_negative() {
            Sign::Neg
        } else {
            Sign::Pos
        };
        IntegerLit {
            value,
            sign,
            leading_zeros,
        }
    }

    /// Constructs an integer literal for 0, with a sign and leading zeros.
    pub const fn zero(is_negative: bool, leading_zeros: usize) -> Self {
        IntegerLit {
            value: Integer::ZERO,
            sign: if is_negative { Sign::Neg } else { Sign::Pos },
            leading_zeros,
        }
    }

    /// Constructs an integer literal, that has no sign or bits and evaluates to
    /// an error.
    pub const fn empty() -> Self {
        IntegerLit {
            value: Integer::ZERO,
            sign: Sign::Empty,
            leading_zeros: 0,
        }
    }

    #[inline]
    pub fn value(&self) -> Option<&Integer> {
        if !self.is_empty() {
            Some(&self.value)
        } else {
            None
        }
    }

    #[inline]
    pub fn sign(&self) -> Sign {
        self.sign
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sign == Sign::Empty
    }

    pub fn to_bits(&self) -> BitVec {
        signed_bits_from_integer(&self.value, self.sign, self.leading_zeros)
    }
}

impl Display for IntegerLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.value() {
            Some(n) => write!(f, "{n}"),
            None => write!(f, "<empty>"),
        }
    }
}

impl From<&BitSlice> for IntegerLit {
    fn from(bits: &BitSlice) -> Self {
        match bits.split_first() {
            None => IntegerLit::empty(),
            Some((sign, bits)) => {
                let mut value = integer_from_unsigned_bits(bits);
                if *sign {
                    value.neg_assign();
                }
                IntegerLit {
                    value,
                    sign: if *sign { Sign::Neg } else { Sign::Pos },
                    leading_zeros: bits.leading_zeros(),
                }
            }
        }
    }
}

impl LabelLit {
    #[inline]
    pub fn new(bits: BitVec) -> Self {
        LabelLit { bits }
    }

    /// Constructs a label from a non-negative integer.
    pub fn from_integer(value: &Integer) -> Self {
        assert!(!value.is_negative());
        LabelLit::new(unsigned_bits_from_integer(value))
    }

    #[inline]
    pub fn bits(&self) -> &BitSlice {
        &self.bits
    }

    /// Converts this label to an integer. Integer values should not be used
    /// when comparing labels for equality, because leading zeros are lost.
    pub fn to_integer(&self) -> Integer {
        integer_from_unsigned_bits(&self.bits)
    }

    /// Tries to decode this label as UTF-8.
    pub fn to_utf8(&self) -> Option<String> {
        if self.bits.len() % 8 == 0 {
            let mut bytes = Vec::with_capacity(self.bits.len() / 8);
            for byte in self.bits.chunks(8) {
                bytes.push(byte.load_le::<u8>().reverse_bits());
            }
            match simdutf8::basic::from_utf8(&bytes) {
                // SAFETY: Bytes have been validated as UTF-8.
                Ok(_) => unsafe { Some(String::from_utf8_unchecked(bytes)) },
                Err(_) => None,
            }
        } else {
            None
        }
    }

    /// Converts this label to a binary integer literal of its bits.
    pub fn to_binary_string(&self) -> String {
        let mut s = String::with_capacity(self.bits.len() + 2);
        s.push_str("0b");
        for bit in &self.bits {
            s.push(if *bit { '1' } else { '0' });
        }
        s
    }

    /// Converts to the string representation for labels used in the reference
    /// interpreter.
    pub fn to_haskell_string(&self) -> String {
        let mut s = String::with_capacity(self.bits.len());
        for bit in self.bits.iter().rev() {
            s.push(if *bit { '\t' } else { ' ' });
        }
        s
    }
}

impl Display for LabelLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@")?;
        if let Some(s) = self.to_utf8() {
            // Try to decode it as UTF-8
            if s.len() == 0
                || s.starts_with(|c| char::is_ascii_digit(&c))
                || s.contains(char::is_whitespace)
            {
                write!(f, "\"{s}\"")
            } else {
                write!(f, "{s}")
            }
        } else if self.bits.first().as_deref() != Some(&true) {
            // Print in binary, if it has leading zeros
            f.write_str(&self.to_binary_string())
        } else {
            // Otherwise, print it in decimal
            write!(f, "{}", self.to_integer())
        }
    }
}

/// A quoted version of [`LabelLit::to_haskell_string`].
impl hs::Show for LabelLit {
    fn show(&self) -> String {
        let mut s = String::with_capacity(self.bits.len() + self.bits.count_ones() + 2);
        s.push('"');
        for bit in self.bits.iter().rev() {
            s.push_str(if *bit { "\\t" } else { " " });
        }
        s.push('"');
        s
    }
}

impl From<BitVec> for LabelLit {
    #[inline]
    fn from(bits: BitVec) -> Self {
        LabelLit::new(bits)
    }
}

fn integer_from_unsigned_bits(bits: &BitSlice<usize, Lsb0>) -> Integer {
    let value = if bits.len() < usize::BITS as usize {
        let mut arr = BitArray::<_, Lsb0>::new([0usize; 1]);
        let slice = &mut arr[..bits.len()];
        slice.copy_from_bitslice(bits);
        slice.reverse();
        Integer::from_digits(arr.as_raw_slice(), Order::LsfLe)
    } else {
        let mut boxed = BitBox::<usize, Lsb0>::from(bits);
        boxed.force_align();
        boxed.fill_uninitialized(false);
        boxed.reverse();
        Integer::from_digits(boxed.as_raw_slice(), Order::LsfLe)
    };
    value
}

fn unsigned_bits_from_integer(value: &Integer) -> BitVec<usize, Lsb0> {
    let mut bits = BitVec::<usize, Lsb0>::from_vec(value.to_digits(Order::LsfLe));
    bits.truncate(bits.last_one().map_or(0, |i| i + 1));
    bits.reverse();
    bits
}

fn signed_bits_from_integer(
    value: &Integer,
    sign: Sign,
    leading_zeros: usize,
) -> BitVec<usize, Lsb0> {
    let mut bits;
    if value.cmp0() == Ordering::Equal {
        if leading_zeros == 0 && sign == Sign::Empty {
            bits = BitVec::new();
        } else {
            bits = BitVec::repeat(false, leading_zeros + 1);
            if sign == Sign::Neg {
                bits.set(0, true);
            }
        }
    } else {
        bits = unsigned_bits_from_integer(value);
        let len = bits.len();
        bits.reserve(leading_zeros + 1);
        // SAFETY: Newly-reserved bits are guaranteed to be allocated to zero.
        unsafe { bits.set_len(len + leading_zeros + 1) };
        // Panics when shifting by the length.
        if len != 0 {
            bits.shift_right(leading_zeros + 1);
        }
        if sign == Sign::Neg {
            bits.set(0, true);
        }
    }
    bits
}
