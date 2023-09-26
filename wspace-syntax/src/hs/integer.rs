//! Parsing for Haskell `Integer`.

use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

use rug::Integer;

/// An integer with the syntax of [`read :: String -> Integer`](https://hackage.haskell.org/package/base/docs/GHC-Read.html)
/// in Haskell.
///
/// # Syntax
///
/// Octal literals are prefixed with `0o` or `0O` and hexadecimal literals with
/// `0x` or `0X`. Binary literals with `0b` or `0B` are not supported. A leading
/// zero is interpreted as decimal, not octal. It may have a negative sign. It
/// may be surrounded by any number of parentheses. Unicode whitespace
/// characters may occur around the number, sign, or parentheses. Positive
/// signs, underscore digit separators, and exponents are not allowed.
///
/// Haskell's `String` must be UTF-8 and excludes surrogate halves, so it is
/// equivalent to Rust strings and validation happens outside of `read`.
///
/// ```bnf
/// read        ::= space* "(" read ")" space*
///               | space* integer space*
/// integer     ::= "-"? space* (dec_integer | oct_integer | hex_integer)
/// dec_integer ::= [0-9]+
/// oct_integer ::= "0" [oO] [0-7]+
/// hex_integer ::= "0" [xX] [0-9 a-f A-F]+
/// space       ::= \p{White_Space} NOT (U+0085 | U+2028 | U+2029)
/// ```
///
/// # Compliance
///
/// It has been tested to match the behavior of at least GHC versions 8.8.4 and
/// 9.4.4.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReadIntegerLit {
    pub value: Integer,
    pub is_negative: bool,
    pub base: ReadIntegerBase,
    pub leading_zeros: usize,
}

/// Integer base (radix).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ReadIntegerBase {
    Decimal = 10,
    Octal = 8,
    Hexadecimal = 16,
}

/// Error from parsing a [`ReadIntegerLit`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReadIntegerError {
    InvalidDigit,
    NoDigits,
    UnpairedParen,
    IllegalNeg,
}

/// Parses an integer with the syntax of [`read :: String -> Integer`](https://hackage.haskell.org/package/base/docs/GHC-Read.html)
/// in Haskell.
impl FromStr for ReadIntegerLit {
    type Err = ReadIntegerError;

    fn from_str(s: &str) -> Result<Self, ReadIntegerError> {
        ReadIntegerLit::parse_with_buffer(s, &mut Vec::new())
    }
}

impl ReadIntegerLit {
    /// Parses an integer as in [`ReadIntegerLit::from_str`], but with a buffer
    /// of digits to reuse allocations.
    pub fn parse_with_buffer(mut s: &str, digits: &mut Vec<u8>) -> Result<Self, ReadIntegerError> {
        // See:
        // - `Read Integer` instance: https://gitlab.haskell.org/ghc/ghc/-/blob/b8e4fe2318798185228fb5f8214ba2384ac95b4f/libraries/base/GHC/Read.hs#L616-619
        // - `Text.Read.Lex.numberToInteger`: https://gitlab.haskell.org/ghc/ghc/-/blob/b8e4fe2318798185228fb5f8214ba2384ac95b4f/libraries/base/Text/Read/Lex.hs#L87-90

        use ReadIntegerBase as Base;
        use ReadIntegerError as Error;

        #[inline]
        fn is_whitespace(ch: char) -> bool {
            ch.is_whitespace() && ch != '\u{0085}' && ch != '\u{2028}' && ch != '\u{2029}'
        }

        digits.clear();

        loop {
            s = s.trim_matches(is_whitespace);
            if !s.is_empty() {
                let (first, last) = (s.as_bytes()[0], s.as_bytes()[s.len() - 1]);
                if first == b'(' && last == b')' {
                    s = &s[1..s.len() - 1];
                    continue;
                } else if first == b'(' || last == b')' {
                    if first == b'-' {
                        return Err(Error::IllegalNeg);
                    }
                    return Err(Error::UnpairedParen);
                }
            }
            break;
        }

        let is_negative = if !s.is_empty() && s.as_bytes()[0] == b'-' {
            s = s[1..].trim_start_matches(is_whitespace);
            true
        } else {
            false
        };

        let b = s.as_bytes();
        let (base, b) = match b {
            [b'0', b'o' | b'O', b @ ..] => (Base::Octal, b),
            [b'0', b'x' | b'X', b @ ..] => (Base::Hexadecimal, b),
            _ => (Base::Decimal, b),
        };
        let leading_zeros = b.iter().take_while(|&&ch| ch == b'0').count();
        let b = &b[leading_zeros..];

        let mut value = Integer::new();
        if !b.is_empty() {
            digits.reserve(b.len());
            match base {
                Base::Decimal => {
                    for &ch in b {
                        let digit = ch.wrapping_sub(b'0');
                        if digit >= 10 {
                            return Err(Error::InvalidDigit);
                        }
                        digits.push(digit);
                    }
                }
                Base::Octal => {
                    for &ch in b {
                        let digit = ch.wrapping_sub(b'0');
                        if digit >= 8 {
                            return Err(Error::InvalidDigit);
                        }
                        digits.push(digit);
                    }
                }
                Base::Hexadecimal => {
                    for &ch in b {
                        let digit = match ch {
                            b'0'..=b'9' => ch - b'0',
                            b'a'..=b'f' => ch - b'a' + 10,
                            b'A'..=b'F' => ch - b'A' + 10,
                            _ => return Err(Error::InvalidDigit),
                        };
                        digits.push(digit);
                    }
                }
            }
            // SAFETY: Digits are constructed to be in range for the base.
            unsafe {
                value.assign_bytes_radix_unchecked(&digits, base as i32, is_negative);
            }
        } else if leading_zeros == 0 {
            return Err(Error::NoDigits);
        }

        Ok(ReadIntegerLit {
            value,
            is_negative,
            base,
            leading_zeros,
        })
    }
}

impl Display for ReadIntegerLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_negative {
            f.write_str("-")?;
        }
        match self.base {
            ReadIntegerBase::Decimal => {}
            ReadIntegerBase::Octal => f.write_str("0o")?,
            ReadIntegerBase::Hexadecimal => f.write_str("0x")?,
        }
        write!(f, "{:0<width$}", "", width = self.leading_zeros)?;
        if !self.value.is_zero() {
            match self.base {
                ReadIntegerBase::Decimal => write!(f, "{}", self.value.as_abs())?,
                ReadIntegerBase::Octal => write!(f, "{:o}", self.value.as_abs())?,
                ReadIntegerBase::Hexadecimal => write!(f, "{:x}", self.value.as_abs())?,
            }
        }
        Ok(())
    }
}
