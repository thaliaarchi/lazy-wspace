//! Parsing for string literals with regex-like syntax.
//!
//! # Grammar
//!
//! ```bnf
//! string_lit     ::= "\"" char_or_byte* "\""
//! char_or_byte   ::= unicode_char | unicode_escape | byte_escape | named_escape
//! unicode_char   ::= /* An arbitrary Unicode codepoint, except for LF, CR, or " */
//! unicode_escape ::= "\\u" hex_digit{4}
//!                  | "\\U" hex_digit{8}
//!                  | "\\u" "{" hex_digit+ "}"
//!                  | "\\U" "{" hex_digit+ "}"
//! byte_escape    ::= "\\x" hex_digit{2}
//! named_escape   ::= "\\" ("a" | "t" | "n" | "v" | "f" | "r" | "\"" | "'" | "\\")
//! hex_digit      ::= [0-9 a-f A-F]
//! ```
//!
//! # Differences from Rust
//!
//! - `_` separators in `\u{…}` removed, to match regex syntax
//! - `\0` escapes removed, to avoid octal syntax and backreferences
//! - Bare line feed forbidden
//! - Line continuations removed
//! - Added `\a`, `\f`, `\v` escapes
//! - Byte escapes allowed in strings and Unicode chars allowed in byte strings
//! - Braced unicode escapes not limited to 6 hex chars
//!
//! # Differences from `regex_syntax`
//!
//! - `\xhh` escapes must be ASCII in UTF-8 strings
//! - No `\x{…}` escapes
//! - No superfluous escapes are allowed (as in [`regex_syntax::is_escapeable_character`])

// Derived from [unescape.rs](https://github.com/rust-lang/rust/blob/master/compiler/rustc_lexer/src/unescape.rs)
// in rustc_lexer.

use std::ops::Range;
use std::str::Chars;

use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("{kind} at {span:?}")]
pub struct EscapeError {
    pub kind: EscapeErrorKind,
    pub span: Range<usize>,
}

/// Errors that can occur during string unescaping.
#[derive(Clone, Copy, Debug, Error, PartialEq, Eq)]
pub enum EscapeErrorKind {
    #[error("bare line feed (`\\n`)")]
    BareLineFeed,
    #[error("bare carriage return (`\\r`)")]
    BareCarriageReturn,
    #[error("bare double quote (`\"`)")]
    BareDoubleQuote,
    #[error("lone `\\` without escape sequence")]
    LoneSlash,
    #[error("invalid escape character")]
    InvalidEscape,

    #[error("numeric escape is too short")]
    TooShortHexEscape,
    #[error("invalid character in numeric escape")]
    InvalidCharInHexEscape,
    #[error("braced numeric escape without digits")]
    EmptyBraceEscape,
    #[error("missing closing brace in numeric escape")]
    UnclosedBraceEscape,
    #[error("invalid Unicode code point (surrogate half)")]
    LoneSurrogateUnicodeEscape,
    #[error("invalid Unicode code point (out of range)")]
    OutOfRangeUnicodeEscape,
    #[error("non-ASCII byte escape")]
    NonAsciiByte,
}

pub fn unescape_string(src: &str) -> Result<String, EscapeError> {
    let mut unescaped = String::new();
    scan_string(src, &mut |res, _span| {
        match res? {
            ByteOrChar::Byte(b) if b >= 0x80 => return Err(EscapeErrorKind::NonAsciiByte),
            ByteOrChar::Byte(b) => unescaped.push(b as char),
            ByteOrChar::Char(ch) => unescaped.push(ch),
        }
        Ok(())
    })?;
    Ok(unescaped)
}

pub fn unescape_byte_string(src: &str) -> Result<Vec<u8>, EscapeError> {
    let mut unescaped = Vec::new();
    scan_string(src, &mut |res, _span| {
        match res? {
            ByteOrChar::Byte(b) => unescaped.push(b),
            ByteOrChar::Char(ch) => match ch.len_utf8() {
                1 => unescaped.push(ch as u8),
                _ => unescaped.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes()),
            },
        }
        Ok(())
    })?;
    Ok(unescaped)
}

enum ByteOrChar {
    Byte(u8),
    Char(char),
}

impl From<u8> for ByteOrChar {
    #[inline]
    fn from(b: u8) -> Self {
        ByteOrChar::Byte(b)
    }
}

impl From<char> for ByteOrChar {
    #[inline]
    fn from(ch: char) -> Self {
        ByteOrChar::Char(ch)
    }
}

#[inline]
pub fn scan_string<F, T: From<u8> + From<char>>(
    src: &str,
    callback: &mut F,
) -> Result<(), EscapeError>
where
    F: FnMut(Result<T, EscapeErrorKind>, Range<usize>) -> Result<(), EscapeErrorKind>,
{
    let mut chars = src.chars();
    let mut start = 0;
    while let Some(c) = chars.next() {
        let res = match c {
            '\\' => scan_escape(&mut chars),
            '\n' => Err(EscapeErrorKind::BareLineFeed),
            '\r' => Err(EscapeErrorKind::BareCarriageReturn),
            '"' => Err(EscapeErrorKind::BareDoubleQuote),
            _ => Ok(c.into()),
        };
        let end = src.len() - chars.as_str().len();
        if let Err(err) = callback(res, start..end) {
            return Err(EscapeError {
                kind: err,
                span: start..end,
            });
        }
        start = end;
    }
    Ok(())
}

fn scan_escape<T: From<u8> + From<char>>(chars: &mut Chars<'_>) -> Result<T, EscapeErrorKind> {
    let res = match chars.next().ok_or(EscapeErrorKind::LoneSlash)? {
        'a' => b'\x07',
        't' => b'\t',
        'n' => b'\n',
        'v' => b'\x0B',
        'f' => b'\x0C',
        'r' => b'\r',
        '"' => b'"',
        '\'' => b'\'',
        '\\' => b'\\',
        'x' => scan_hex_digits(chars, 2)? as u8,
        'u' => return scan_unicode(chars, 4).map(Into::into),
        'U' => return scan_unicode(chars, 8).map(Into::into),
        _ => return Err(EscapeErrorKind::InvalidEscape),
    };
    Ok(res.into())
}

fn scan_hex_digits(chars: &mut Chars<'_>, len: usize) -> Result<u32, EscapeErrorKind> {
    let mut value = 0u32;
    for _ in 0..len {
        let digit = chars.next().ok_or(EscapeErrorKind::TooShortHexEscape)?;
        let digit = digit
            .to_digit(16)
            .ok_or(EscapeErrorKind::InvalidCharInHexEscape)?;
        value = value.saturating_mul(16).saturating_add(digit);
    }
    Ok(value)
}

fn scan_unicode(chars: &mut Chars<'_>, len: usize) -> Result<char, EscapeErrorKind> {
    let value = if chars.clone().next() == Some('{') {
        scan_unicode_brace(chars)
    } else {
        scan_hex_digits(chars, len)
    }?;
    char::from_u32(value).ok_or(if value > 0x10FFFF {
        EscapeErrorKind::OutOfRangeUnicodeEscape
    } else {
        EscapeErrorKind::LoneSurrogateUnicodeEscape
    })
}

fn scan_unicode_brace(chars: &mut Chars<'_>) -> Result<u32, EscapeErrorKind> {
    assert!(chars.next() == Some('{'));
    let mut value = 0u32;
    let mut empty = true;
    loop {
        match chars.next() {
            Some('}') => {
                if empty {
                    return Err(EscapeErrorKind::EmptyBraceEscape);
                }
                break;
            }
            Some(c) => {
                let digit: u32 = c
                    .to_digit(16)
                    .ok_or(EscapeErrorKind::InvalidCharInHexEscape)?;
                value = value.saturating_mul(16).saturating_add(digit);
                empty = false;
            }
            None => return Err(EscapeErrorKind::UnclosedBraceEscape),
        };
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use regex::{bytes::Regex as BytesRegex, Regex};

    use super::*;

    #[test]
    fn unescape_hex() {
        assert_eq!(
            unescape_string(r"\xff"),
            Err(EscapeError {
                kind: EscapeErrorKind::NonAsciiByte,
                span: 0..4,
            }),
        );
        assert_eq!(unescape_byte_string(r"\xff"), Ok(b"\xff".to_vec()));
    }

    #[test]
    fn regex_hex_semantics() {
        let re = Regex::new(r"\xff").unwrap();
        assert!(re.is_match("\u{ff}"));
        let re = BytesRegex::new(r"\xff").unwrap();
        assert!(re.is_match("\u{ff}".as_bytes()));
        assert!(!re.is_match(b"\xff"));

        assert!(Regex::new(r"(?-u)\xff").is_err());
        let re = BytesRegex::new(r"(?-u)\xff").unwrap();
        assert!(!re.is_match("\u{ff}".as_bytes()));
        assert!(re.is_match(b"\xff"));
    }
}
