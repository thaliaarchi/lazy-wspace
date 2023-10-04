use std::fmt::{self, Debug, Formatter};
use std::iter::FusedIterator;

use bitvec::vec::BitVec;
use wspace_syntax::ws::lex::{ExtLexer, Span};
use wspace_syntax::ws::{ExtToken, Token};

use crate::ast::{Inst, IntegerLit, LabelLit};
use crate::error::ParseError;

pub struct Parser<L: ExtLexer> {
    lex: L,
    curr: Span,
}

impl<L: ExtLexer> Parser<L> {
    #[inline]
    pub fn new(lex: L) -> Self {
        Parser {
            lex,
            curr: Span::from(0..0),
        }
    }

    #[inline]
    fn next_inst(&mut self) -> Result<Option<Inst>, ParseError> {
        let first_tok = match self.next_token_inner() {
            Some((maybe_tok, span)) => {
                self.curr.start = span.start;
                maybe_tok?
            }
            None => return Ok(None),
        };
        let inst = match first_tok {
            Token::S => match self.next_token()? {
                Token::S => self.parse_integer(Inst::Push),
                Token::T => match self.next_token()? {
                    Token::S => self.parse_integer(Inst::Copy),
                    Token::T => ParseError::UnrecognizedOpcode.into(),
                    Token::L => self.parse_integer(Inst::Slide),
                },
                Token::L => match self.next_token()? {
                    Token::S => Inst::Dup,
                    Token::T => Inst::Swap,
                    Token::L => Inst::Drop,
                },
            },
            Token::T => match self.next_token()? {
                Token::S => match self.next_token()? {
                    Token::S => match self.next_token()? {
                        Token::S => Inst::Add,
                        Token::T => Inst::Sub,
                        Token::L => Inst::Mul,
                    },
                    Token::T => match self.next_token()? {
                        Token::S => Inst::Div,
                        Token::T => Inst::Mod,
                        Token::L => ParseError::UnrecognizedOpcode.into(),
                    },
                    Token::L => ParseError::UnrecognizedOpcode.into(),
                },
                Token::T => match self.next_token()? {
                    Token::S => Inst::Store,
                    Token::T => Inst::Retrieve,
                    Token::L => ParseError::UnrecognizedOpcode.into(),
                },
                Token::L => match self.next_token()? {
                    Token::S => match self.next_token()? {
                        Token::S => Inst::Printc,
                        Token::T => Inst::Printi,
                        Token::L => ParseError::UnrecognizedOpcode.into(),
                    },
                    Token::T => match self.next_token()? {
                        Token::S => Inst::Readc,
                        Token::T => Inst::Readi,
                        Token::L => ParseError::UnrecognizedOpcode.into(),
                    },
                    Token::L => ParseError::UnrecognizedOpcode.into(),
                },
            },
            Token::L => match self.next_token()? {
                Token::S => match self.next_token()? {
                    Token::S => self.parse_label(Inst::Label),
                    Token::T => self.parse_label(Inst::Call),
                    Token::L => self.parse_label(Inst::Jmp),
                },
                Token::T => match self.next_token()? {
                    Token::S => self.parse_label(Inst::Jz),
                    Token::T => self.parse_label(Inst::Jn),
                    Token::L => Inst::Ret,
                },
                Token::L => match self.next_token()? {
                    Token::S => ParseError::UnrecognizedOpcode.into(),
                    Token::T => ParseError::UnrecognizedOpcode.into(),
                    Token::L => Inst::End,
                },
            },
        };
        Ok(Some(inst))
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        match self.next_token_inner() {
            Some((res, _)) => res,
            None => Err(ParseError::IncompleteOpcode),
        }
    }

    fn next_token_inner(&mut self) -> Option<(Result<Token, ParseError>, Span)> {
        let (tok, span) = self.lex.next()?;
        let res = match tok {
            ExtToken::Token(tok) => Ok(tok),
            ExtToken::InvalidUtf8 => Err(ParseError::InvalidUtf8),
            ExtToken::RiverCrab => Err(ParseError::UnexpectedRiverCrab),
        };
        self.curr.end = span.end;
        Some((res, span))
    }

    fn parse_arg(&mut self, unterminated_err: ParseError) -> Result<BitVec, ParseError> {
        let mut arg = BitVec::new();
        while let Some((maybe_tok, _)) = self.next_token_inner() {
            match maybe_tok? {
                Token::S => arg.push(false),
                Token::T => arg.push(true),
                Token::L => return Ok(arg),
            }
        }
        Err(unterminated_err)
    }

    #[inline]
    fn parse_integer<F: FnOnce(IntegerLit) -> Inst>(&mut self, inst: F) -> Inst {
        match self.parse_arg(ParseError::UnterminatedInteger) {
            Ok(bits) => inst(IntegerLit::from(bits)),
            Err(err) => err.into(),
        }
    }

    #[inline]
    fn parse_label<F: FnOnce(LabelLit) -> Inst>(&mut self, inst: F) -> Inst {
        match self.parse_arg(ParseError::UnterminatedLabel) {
            Ok(bits) => inst(LabelLit::from(bits)),
            Err(err) => err.into(),
        }
    }
}

impl<L: ExtLexer> Iterator for Parser<L> {
    type Item = (Inst, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let inst = self.next_inst().transpose()?.into();
        Some((inst, self.curr))
    }
}

impl<L: ExtLexer + FusedIterator> FusedIterator for Parser<L> {}

impl<L: ExtLexer + Clone> Clone for Parser<L> {
    fn clone(&self) -> Self {
        Parser {
            lex: self.lex.clone(),
            curr: self.curr,
        }
    }
}

impl<L: ExtLexer + Debug> Debug for Parser<L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("lex", &self.lex)
            .field("curr", &self.curr)
            .finish()
    }
}
