use std::iter::FusedIterator;
use std::str::Chars;

use crate::source::Span;
use crate::ws::lex::{ExtLexer, ExtToken, Token};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GmhToken {
    /// Grass (草 U+8349)
    G,
    /// Mud (泥 U+6CE5)
    M,
    /// Horse (马 U+9A6C)
    H,
    /// River (河 U+6CB3)
    R,
    /// Crab (蟹 U+87F9)
    C,
}

const G_CHAR: char = '草';
const M_CHAR: char = '泥';
const H_CHAR: char = '马';
const R_CHAR: char = '河';
const C_CHAR: char = '蟹';

#[derive(Clone, Debug)]
struct GmhScanner<'a> {
    iter: Chars<'a>,
    len: usize,
}

/// Lexer for GrassMudHorse.
#[derive(Clone, Debug)]
pub struct GmhLexer<'a> {
    iter: GmhScanner<'a>,
}

impl<'a> GmhScanner<'a> {
    #[inline]
    pub fn new(src: &'a str) -> Self {
        GmhScanner {
            iter: src.chars(),
            len: src.len(),
        }
    }
}

impl Iterator for GmhScanner<'_> {
    type Item = (GmhToken, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pre_len = self.iter.as_str().len();
            let tok = match self.iter.next()? {
                G_CHAR => GmhToken::G,
                M_CHAR => GmhToken::M,
                H_CHAR => GmhToken::H,
                R_CHAR => GmhToken::R,
                C_CHAR => GmhToken::C,
                _ => continue,
            };
            let start = self.len - pre_len;
            let end = self.len - self.iter.as_str().len();
            return Some((tok, Span::from(start..end)));
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        // Each of the characters is 3 bytes long.
        (0, Some((self.iter.as_str().len() + 2) / 3))
    }
}

impl FusedIterator for GmhScanner<'_> {}

impl<'a> GmhLexer<'a> {
    #[inline]
    pub fn new(src: &'a str) -> Self {
        GmhLexer {
            iter: GmhScanner::new(src),
        }
    }
}

impl ExtLexer for GmhLexer<'_> {}

impl Iterator for GmhLexer<'_> {
    type Item = (ExtToken, Span);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let mut r_span = None;
        loop {
            let (tok, span) = self.iter.next()?;
            match tok {
                GmhToken::G => return Some((Token::S.into(), span)),
                GmhToken::M => return Some((Token::T.into(), span)),
                GmhToken::H => return Some((Token::L.into(), span)),
                GmhToken::R => r_span = Some(span),
                GmhToken::C => {
                    if let Some(r_span) = r_span {
                        let rc_span = Span::from(r_span.start..span.end);
                        return Some((ExtToken::RiverCrab, rc_span));
                    }
                }
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl FusedIterator for GmhLexer<'_> {}

#[test]
fn unpaired_rc() {
    let src = "G草 R河 M泥 C蟹 H马 R河 C蟹 G草 C蟹 R河 M泥 C蟹 R河 R河 C蟹 C蟹 H马";
    let gmh_toks = vec![
        (GmhToken::G, Span::from(1..4)),
        (GmhToken::R, Span::from(6..9)),
        (GmhToken::M, Span::from(11..14)),
        (GmhToken::C, Span::from(16..19)),
        (GmhToken::H, Span::from(21..24)),
        (GmhToken::R, Span::from(26..29)),
        (GmhToken::C, Span::from(31..34)),
        (GmhToken::G, Span::from(36..39)),
        (GmhToken::C, Span::from(41..44)),
        (GmhToken::R, Span::from(46..49)),
        (GmhToken::M, Span::from(51..54)),
        (GmhToken::C, Span::from(56..59)),
        (GmhToken::R, Span::from(61..64)),
        (GmhToken::R, Span::from(66..69)),
        (GmhToken::C, Span::from(71..74)),
        (GmhToken::C, Span::from(76..79)),
        (GmhToken::H, Span::from(81..84)),
    ];
    assert_eq!(gmh_toks, GmhScanner::new(src).collect::<Vec<_>>());
    let toks = vec![
        (Token::S.into(), Span::from(1..4)),
        (Token::T.into(), Span::from(11..14)),
        (Token::L.into(), Span::from(21..24)),
        (ExtToken::RiverCrab, Span::from(26..34)),
        (Token::S.into(), Span::from(36..39)),
        (Token::T.into(), Span::from(51..54)),
        (ExtToken::RiverCrab, Span::from(66..74)),
        (Token::L.into(), Span::from(81..84)),
    ];
    assert_eq!(toks, GmhLexer::new(src).collect::<Vec<_>>());
}
