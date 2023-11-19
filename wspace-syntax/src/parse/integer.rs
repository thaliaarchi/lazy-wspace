use rug::Integer;

use crate::ws::ast::IntegerLit;

/// Error from parsing an integer literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParseIntegerError {
    InvalidDigit(usize),
    NoDigits,
    DecimalLeadingZero,
    LeadingUnderscore,
    TrailingUnderscore,
}

impl IntegerLit {
    /// Parses a Whitespace assembly integer literal.
    ///
    /// The base of the integer is indicated by the prefix: `0b` or `0B` for
    /// binary, `0o` or `0O` for octal, `0x` or `0X` for hexadecimal, or decimal
    /// otherwise. Underscores may separate digits. Leading `0`s are significant
    /// and are preserved in the Whitespace encoding. Aside from `0` itself,
    /// leading `0`s are forbidden for decimal integers, because it could not be
    /// encoded unambiguous in Whitespace and to avoid confusion with C-style
    /// octal syntax. Binary numbers may omit digits to be encoded in Whitespace
    /// as the empty sequence. An integer can have a `-` sign.
    ///
    /// # Grammar
    ///
    /// ```bnf
    /// integer     ::= "-"? (dec_integer | bin_integer | oct_integer | hex_integer)
    /// dec_integer ::= ([1-9] ("_"* [0-9])* | "0")
    /// bin_integer ::= "0" [bB] ("_"* [01])*
    /// oct_integer ::= "0" [oO] ("_"* [0-7])+
    /// hex_integer ::= "0" [xX] ("_"* [0-9 a-f A-F])+
    /// ```
    pub fn parse_wsa<B: AsRef<[u8]>>(s: B) -> Result<Self, ParseIntegerError> {
        let s0 = s.as_ref();
        let len = s0.len();

        let (is_negative, s) = match s0.split_first() {
            Some((b'-', s)) => (true, s),
            _ => (false, s0),
        };

        let (base, digits, leading_binary_zeros) = match s.split_first() {
            Some((b'0', s)) => match s.split_first() {
                // Binary
                Some((b'b' | b'B', s)) => {
                    let (leading_zeros, s) = skip_leading_zeros(s);
                    let digits = parse_decimal_digits(s, len, 2)?;
                    (2, digits, leading_zeros)
                }

                // Octal
                Some((b'o' | b'O', s)) => {
                    let (leading_zeros, s) = skip_leading_zeros(s);
                    let digits = parse_decimal_digits(s, len, 8)?;
                    (8, digits, leading_zeros * 3)
                }

                // Hexadecimal
                Some((b'x' | b'X', s)) => {
                    let (leading_zeros, mut s) = skip_leading_zeros(s);
                    let mut digits = Vec::with_capacity(s.len());
                    while let Some((&b, s1)) = s.split_first() {
                        let digit = match b {
                            b'0'..=b'9' => b - b'0',
                            b'a'..=b'f' => b - b'a' + 10,
                            b'A'..=b'F' => b - b'A' + 10,
                            b'_' => continue,
                            _ => return Err(ParseIntegerError::InvalidDigit(len - s.len())),
                        };
                        digits.push(digit);
                        s = s1;
                    }
                    (16, digits, leading_zeros * 4)
                }

                // Zero with invalid trailing data
                Some(_) => {
                    for &b in s {
                        if b'0' <= b && b <= b'9' {
                            return Err(ParseIntegerError::DecimalLeadingZero);
                        } else if b != b'_' {
                            return Err(ParseIntegerError::InvalidDigit(len - s.len()));
                        }
                    }
                    return Err(ParseIntegerError::TrailingUnderscore);
                }

                // Zero
                None => return Ok(IntegerLit::zero(is_negative, 0)),
            },

            Some((b'_', _)) => return Err(ParseIntegerError::LeadingUnderscore),

            // Decimal
            Some(_) => {
                let digits = parse_decimal_digits(s, len, 10)?;
                (10, digits, 0)
            }

            None => return Err(ParseIntegerError::NoDigits),
        };

        if s0.last() == Some(&b'_') {
            return Err(ParseIntegerError::TrailingUnderscore);
        }

        if digits.is_empty() {
            if leading_binary_zeros == 0 && base != 2 {
                return Err(ParseIntegerError::NoDigits);
            } else {
                return Ok(IntegerLit::zero(is_negative, leading_binary_zeros));
            }
        }

        let mut value = Integer::new();
        // SAFETY: Digits have been verified to be in range for the base.
        unsafe {
            value.assign_bytes_radix_unchecked(&digits, base, is_negative);
        }
        Ok(IntegerLit::new_leading(value, leading_binary_zeros))
    }
}

#[inline]
fn skip_leading_zeros(mut s: &[u8]) -> (usize, &[u8]) {
    let mut leading_zeros = 0usize;
    while let Some((&b, s1)) = s.split_first() {
        match b {
            b'0' => leading_zeros += 1,
            b'_' => {}
            _ => break,
        }
        s = s1;
    }
    (leading_zeros, s)
}

#[inline]
fn parse_decimal_digits(mut s: &[u8], len: usize, base: u8) -> Result<Vec<u8>, ParseIntegerError> {
    let mut digits = Vec::with_capacity(s.len());
    while let Some((&b, s1)) = s.split_first() {
        let digit = b.wrapping_sub(b'0');
        if digit >= base {
            if b == b'_' {
                continue;
            }
            return Err(ParseIntegerError::InvalidDigit(len - s.len()));
        }
        digits.push(digit);
        s = s1;
    }
    Ok(digits)
}
