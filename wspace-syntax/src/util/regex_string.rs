//! Parsing for string literals with regex-like syntax.
//!
//! # Grammar
//!
//! ```bnf
//! string_lit     ::= "\"" char_or_byte* "\""
//! char_or_byte   ::= unicode_char | unicode_escape | byte_escape | named_escape
//! unicode_char   ::= /* An arbitrary Unicode codepoint, except for LF or CR */
//! unicode_escape ::= ("\\u" | "\\U") "{" hex_digit{1,6} "}"
//!                  | "\\u" hex_digit{4}
//!                  | "\\U" hex_digit{8}
//! byte_escape    ::= "\\x" hex_digit{2}
//! named_escape   ::= "\\" ("a" | "f" | "t" | "n" | "r" | "v")
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
//! - No superfluous escapes are allowed (as in [`regex_syntax::is_escapeable_character`])

use std::ops::Range;
use std::str::Chars;

/// Errors and warnings that can occur during string unescaping.
#[derive(Debug, PartialEq, Eq)]
pub enum EscapeError {
    /// Escaped `\` character without continuation.
    LoneSlash,
    /// Invalid escape character (e.g., `\z`).
    InvalidEscape,
    /// Raw `\n` encountered.
    BareLineFeed,
    /// Raw `\r` encountered.
    BareCarriageReturn,
    /// Unescaped character that was expected to be escaped (e.g., `"`).
    EscapeOnlyChar,

    /// Numeric character escape is too short (e.g., `\x1`).
    TooShortHexEscape,
    /// Invalid character in numeric escape (e.g., `\xz`).
    InvalidCharInHexEscape,

    /// `\u` not followed by `{`.
    NoBraceInUnicodeEscape,
    /// Non-hexadecimal value in `\u{…}`.
    InvalidCharInUnicodeEscape,
    /// Unicode escape `\u{}` without digits.
    EmptyUnicodeEscape,
    /// No closing brace in `\u{…}` (e.g., `\u{12`).
    UnclosedUnicodeEscape,
    /// Invalid in-bound unicode character code (e.g., `\u{DFFF}`).
    LoneSurrogateUnicodeEscape,
    /// Out of bounds unicode character code (e.g., `\u{FFFFFF}`).
    OutOfRangeUnicodeEscape,
}

pub fn unescape_string<F, T: From<u8> + From<char>>(src: &str, callback: &mut F)
where
    F: FnMut(Range<usize>, Result<T, EscapeError>),
{
    let mut chars = src.chars();
    let mut start = 0;
    while let Some(c) = chars.next() {
        let res = match c {
            '\\' => scan_escape(&mut chars),
            '\n' => Err(EscapeError::BareLineFeed),
            '\r' => Err(EscapeError::BareCarriageReturn),
            '"' => Err(EscapeError::EscapeOnlyChar),
            _ => Ok(c.into()),
        };
        let end = src.len() - chars.as_str().len();
        callback(start..end, res);
        start = end;
    }
}

fn scan_escape<T: From<u8> + From<char>>(chars: &mut Chars<'_>) -> Result<T, EscapeError> {
    // Previous character was '\\', unescape what follows.
    let res = match chars.next().ok_or(EscapeError::LoneSlash)? {
        '"' => b'"',
        'a' => b'\x07',
        'f' => b'\x0C',
        't' => b'\t',
        'n' => b'\n',
        'r' => b'\r',
        'v' => b'\x0B',
        '\\' => b'\\',
        '\'' => b'\'',
        // TODO: range allowed for regex for x, u, and U?
        'x' => {
            // Parse hexadecimal character code.
            let hi = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let hi = hi.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;
            let lo = chars.next().ok_or(EscapeError::TooShortHexEscape)?;
            let lo = lo.to_digit(16).ok_or(EscapeError::InvalidCharInHexEscape)?;
            (hi * 16 + lo) as u8
        }
        'u' => return scan_unicode_brace(chars).map(Into::into),
        'U' => todo!(),
        _ => return Err(EscapeError::InvalidEscape),
    };
    Ok(res.into())
}

fn scan_unicode_brace(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    // We've parsed '\u', now we have to parse '{..}'.

    if chars.next() != Some('{') {
        return Err(EscapeError::NoBraceInUnicodeEscape);
    }
    let mut value = 0u32;
    let mut empty = true;
    loop {
        match chars.next() {
            None => return Err(EscapeError::UnclosedUnicodeEscape),
            Some('}') => {
                if empty {
                    return Err(EscapeError::EmptyUnicodeEscape);
                }
                return char::from_u32(value).ok_or_else(|| {
                    if value > 0x10FFFF {
                        EscapeError::OutOfRangeUnicodeEscape
                    } else {
                        EscapeError::LoneSurrogateUnicodeEscape
                    }
                });
            }
            Some(c) => {
                let digit: u32 = c
                    .to_digit(16)
                    .ok_or(EscapeError::InvalidCharInUnicodeEscape)?;
                value = value.saturating_mul(16).saturating_add(digit);
                empty = false;
            }
        };
    }
}
