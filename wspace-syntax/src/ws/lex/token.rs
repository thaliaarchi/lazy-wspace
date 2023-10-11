use crate::hs;

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
    /// River crab alias for [`End`](crate::ws::ast::Inst::End) from
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
