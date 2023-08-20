use rug::Integer;

/// Parsed Whitespace assembly integer literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntegerLit {
    pub value: Integer,
    pub sign: Sign,
    pub leading_binary_zeros: usize,
}

/// Sign of an integer literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Sign {
    Pos,
    Neg,
    Empty,
}

/// Error from parsing an integer literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    /// as the empty sequence. An integer can have a `-` or `+` sign.
    ///
    /// # Grammar
    ///
    /// ```bnf
    /// integer     ::= [-+]? (dec_integer | bin_integer | oct_integer | hex_integer)
    /// dec_integer ::= ([1-9] ("_"* [0-9])* | "0")
    /// bin_integer ::= "0" [bB] ("_"* [01])*
    /// oct_integer ::= "0" [oO] ("_"* [0-7])+
    /// hex_integer ::= "0" [xX] ("_"* [0-9 a-f A-F])+
    /// ```
    pub fn parse<B: AsRef<[u8]>>(s: B) -> Result<Self, ParseIntegerError> {
        let s0 = s.as_ref();
        let len = s0.len();

        let (sign, s) = match s0.split_first() {
            Some((b'+', s)) => (Sign::Pos, s),
            Some((b'-', s)) => (Sign::Neg, s),
            _ => (Sign::Empty, s0),
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
                None => {
                    return Ok(IntegerLit {
                        value: Integer::ZERO,
                        sign,
                        leading_binary_zeros: 0,
                    });
                }
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

        let mut value = Integer::new();
        if !digits.is_empty() {
            // SAFETY: Digits have been verified to be in range for the base.
            unsafe {
                value.assign_bytes_radix_unchecked(&digits, base, sign == Sign::Neg);
            }
        } else if leading_binary_zeros == 0 && base != 2 {
            return Err(ParseIntegerError::NoDigits);
        }

        Ok(IntegerLit {
            value,
            sign,
            leading_binary_zeros,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReadIntegerError {
    InvalidDigit,
    NoDigits,
    UnpairedParen,
    IllegalNeg,
}

/// Parse an integer like [`read :: String -> Integer`](https://hackage.haskell.org/package/base/docs/GHC-Read.html)
/// in Haskell.
///
/// Octal literals are prefixed with `0o` or `0O` and hexadecimal literals with
/// `0x` or `0X`. Binary literals with `0b` or `0B` are not supported. A leading
/// zero is interpreted as decimal, not octal. It may have a negative sign. It
/// may be surrounded by any number of parentheses. Unicode whitespace
/// characters may occur around the number, sign, or parentheses. Positive
/// signs, underscore digit separators, and exponents are not allowed.
///
/// Haskell's `String` must be UTF-8 and excludes surrogate halves, so
/// validation happens outside of `read` and we can use Rust strings.
///
/// It has been tested to match the behavior of at least GHC versions 8.8.4 and
/// 9.4.4.
///
/// # Grammar
///
/// ```bnf
/// read        ::= space* "(" read ")" space* | negate
/// negate      ::= space* "-"? space* integer space*
/// integer     ::= dec_integer | oct_integer | hex_integer
/// dec_integer ::= [0-9]+
/// oct_integer ::= "0" [oO] [0-7]+
/// hex_integer ::= "0" [xX] [0-9 a-f A-F]+
/// space       ::= \p{White_Space} NOT (U+0085 | U+2028 | U+2029)
/// ```
pub fn read_integer_haskell(s: &str) -> Result<Integer, ReadIntegerError> {
    // `Read Integer` instance source: https://gitlab.haskell.org/ghc/ghc/-/blob/887dc4fc5ad033b4dd2537e914d6d4a574b7fe23/libraries/base/GHC/Read.hs#L616-619

    #[inline]
    fn is_whitespace(ch: char) -> bool {
        ch.is_whitespace() && ch != '\u{0085}' && ch != '\u{2028}' && ch != '\u{2029}'
    }

    let mut s = s;
    loop {
        s = s.trim_matches(is_whitespace);
        if !s.is_empty() {
            let (first, last) = (s.as_bytes()[0], s.as_bytes()[s.len() - 1]);
            if first == b'(' && last == b')' {
                s = &s[1..s.len() - 1];
                continue;
            } else if first == b'(' || last == b')' {
                if first == b'-' {
                    return Err(ReadIntegerError::IllegalNeg);
                }
                return Err(ReadIntegerError::UnpairedParen);
            }
        }
        break;
    }
    let neg = if !s.is_empty() && s.as_bytes()[0] == b'-' {
        s = s[1..].trim_start_matches(is_whitespace);
        true
    } else {
        false
    };

    let b = s.as_bytes();
    let (digits, base) = match b {
        [b'0', b'o' | b'O', b @ ..] => {
            let mut digits = vec![0; b.len()];
            for (i, &ch) in b.iter().enumerate() {
                let digit = ch.wrapping_sub(b'0');
                if digit >= 8 {
                    return Err(ReadIntegerError::InvalidDigit);
                }
                digits[i] = digit;
            }
            (digits, 8)
        }
        [b'0', b'x' | b'X', b @ ..] => {
            let mut digits = vec![0; b.len()];
            for (i, &ch) in b.iter().enumerate() {
                let digit = match ch {
                    b'0'..=b'9' => ch - b'0',
                    b'a'..=b'f' => ch - b'a' + 10,
                    b'A'..=b'F' => ch - b'A' + 10,
                    _ => return Err(ReadIntegerError::InvalidDigit),
                };
                digits[i] = digit;
            }
            (digits, 16)
        }
        _ => {
            let mut digits = vec![0; b.len()];
            for (i, &ch) in b.iter().enumerate() {
                let digit = ch.wrapping_sub(b'0');
                if digit >= 10 {
                    return Err(ReadIntegerError::InvalidDigit);
                }
                digits[i] = digit;
            }
            (digits, 10)
        }
    };
    if digits.is_empty() {
        return Err(ReadIntegerError::NoDigits);
    }

    let mut value = Integer::new();
    // SAFETY: Digits have been verified to be in range for the base.
    unsafe {
        value.assign_bytes_radix_unchecked(&digits, base, neg);
    }
    Ok(value)
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
