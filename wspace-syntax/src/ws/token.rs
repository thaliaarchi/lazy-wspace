use crate::hs;
use crate::ws::MappingWriter;

/// Whitespace token.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    /// Space
    S,
    /// Tab
    T,
    /// Line feed
    L,
}

/// Whitespace token, extended with additional variants.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExtToken {
    Token(Token),
    /// Invalid UTF-8 sequence.
    InvalidUtf8,
    /// River crab alias for [`End`](crate::ws::Inst::End) from
    /// [GrassMudHorse](crate::ws::dialects::GrassMudHorseLexer).
    RiverCrab,
}

impl hs::Show for Token {
    fn show(&self) -> String {
        // Matches the manual instance of `Show` in `Tokens.hs`, which does not
        // produce valid Haskell syntax.
        match self {
            Token::S => " ".into(),
            Token::T => "\t".into(),
            Token::L => "\n".into(),
        }
    }
}

impl From<Token> for ExtToken {
    #[inline]
    fn from(tok: Token) -> Self {
        ExtToken::Token(tok)
    }
}

pub struct Mapping {
    pub s: Vec<u8>,
    pub t: Vec<u8>,
    pub l: Vec<u8>,
}

impl Mapping {
    #[inline]
    pub fn new(s: Vec<u8>, t: Vec<u8>, l: Vec<u8>) -> Self {
        Mapping { s, t, l }
    }

    #[inline]
    pub fn writer(&self) -> MappingWriter<'_> {
        MappingWriter::new(self)
    }
}

impl Default for Mapping {
    #[inline]
    fn default() -> Self {
        Mapping {
            s: b" ".to_vec(),
            t: b"\t".to_vec(),
            l: b"\n".to_vec(),
        }
    }
}
