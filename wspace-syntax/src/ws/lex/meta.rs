use std::borrow::Cow;

use regex_automata::meta::BuildError as RegexBuildError;
use regex_syntax::hir::{Hir, HirKind};
use thiserror::Error;

use crate::ws::lex::{ByteLexer, BytesLexer, Lexer, RegexLexer};
use crate::ws::Token;

#[derive(Clone, Debug)]
pub struct MetaLexer {
    repr: LexerRepr,
}

#[derive(Clone, Debug)]
enum LexerRepr {
    Byte(ByteLexer),
    Bytes(BytesLexer),
    Regex(RegexLexer),
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LexerError {
    #[error("multiple patterns defined for token")]
    RepeatedToken,
    #[error("conflicting patterns")]
    ConflictingPatterns,
    #[error(transparent)]
    RegexBuild(#[from] RegexBuildError),
}

impl MetaLexer {
    pub fn new<'a>(
        token1: Token,
        pattern1: Pattern<'a>,
        token2: Token,
        pattern2: Pattern<'a>,
        token3: Token,
        pattern3: Pattern<'a>,
    ) -> Result<MetaLexer, LexerError> {
        if token1 == token2 || token1 == token3 || token2 == token3 {
            return Err(LexerError::RepeatedToken);
        }

        let repr = match (
            pattern1.as_literal(),
            pattern2.as_literal(),
            pattern3.as_literal(),
        ) {
            (Some(lit1), Some(lit2), _)
            | (Some(lit1), _, Some(lit2))
            | (_, Some(lit1), Some(lit2))
                if lit1 == lit2 =>
            {
                return Err(LexerError::ConflictingPatterns);
            }

            (Some(_), Some(_), Some(_)) => {
                let lit1 = pattern1.try_into_literal().unwrap();
                let lit2 = pattern2.try_into_literal().unwrap();
                let lit3 = pattern3.try_into_literal().unwrap();

                let mut lits = [(token1, lit1), (token2, lit2), (token3, lit3)];
                lits.sort_unstable_by_key(|&(tok, _)| tok as u8);
                let [(_, s), (_, t), (_, l)] = lits;

                if let (&[s], &[t], &[l]) = (&*s, &*t, &*l) {
                    let lex = ByteLexer::new(s, t, l)?;
                    LexerRepr::Byte(lex)
                } else {
                    let lex = BytesLexer::new(&*s, &*t, &*l)?;
                    LexerRepr::Bytes(lex)
                }
            }

            _ => {
                let hir1 = pattern1.into_hir();
                let hir2 = pattern2.into_hir();
                let hir3 = pattern3.into_hir();
                let lex = RegexLexer::from_hirs(token1, hir1, token2, hir2, token3, hir3)?;
                LexerRepr::Regex(lex)
            }
        };
        Ok(MetaLexer { repr })
    }

    pub fn lex<'l, 's>(&'l self, src: &'s [u8]) -> Box<dyn Lexer + 'l>
    where
        's: 'l,
    {
        match &self.repr {
            LexerRepr::Byte(lex) => Box::new(lex.lex(src)),
            LexerRepr::Bytes(lex) => Box::new(lex.lex(src)),
            LexerRepr::Regex(lex) => Box::new(lex.lex(src)),
        }
    }
}

// TODO: how to do UTF-8 vs bytes?
#[derive(Clone, Debug)]
pub enum Pattern<'a> {
    Literal(Cow<'a, [u8]>),
    Regex(Hir),
}

impl<'a> Pattern<'a> {
    fn as_literal(&self) -> Option<&[u8]> {
        match self {
            Pattern::Literal(lit) => Some(lit.as_ref()),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(lit) => Some(&lit.0),
                _ => None,
            },
        }
    }

    fn try_into_literal(self) -> Option<Cow<'a, [u8]>> {
        match self {
            Pattern::Literal(lit) => Some(lit),
            Pattern::Regex(hir) => match hir.kind() {
                HirKind::Literal(_) => {
                    if let HirKind::Literal(lit) = hir.into_kind() {
                        Some(lit.0.into_vec().into())
                    } else {
                        unreachable!()
                    }
                }
                _ => None,
            },
        }
    }

    fn into_hir(self) -> Hir {
        match self {
            Pattern::Literal(lit) => Hir::literal(lit.into_owned()),
            Pattern::Regex(hir) => hir,
        }
    }
}
