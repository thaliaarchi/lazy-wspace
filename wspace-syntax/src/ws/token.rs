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
