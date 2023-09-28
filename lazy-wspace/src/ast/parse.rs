use std::iter::FusedIterator;

use bitvec::vec::BitVec;

use crate::ast::{Inst, IntegerLit, LabelLit, Lexer, Token};
use crate::error::ParseError;

pub struct Parser<'a> {
    lex: Lexer<'a>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(lex: Lexer<'a>) -> Self {
        Parser { lex }
    }

    fn parse_arg(&mut self, unterminated_err: ParseError) -> Result<BitVec, ParseError> {
        let mut arg = BitVec::new();
        loop {
            match self.lex.next() {
                Some(Token::S) => arg.push(false),
                Some(Token::T) => arg.push(true),
                Some(Token::L) => return Ok(arg),
                Some(Token::InvalidUtf8) => return Err(ParseError::InvalidUtf8),
                None => return Err(unterminated_err),
            };
        }
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

impl Iterator for Parser<'_> {
    type Item = Inst;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! default(
            ($val:expr, $default:expr) => { $val };
            (, $default:expr) => { $default };
        );
        macro_rules! match_token(
            ($(S => $on_s:expr,)? $(T => $on_t:expr,)? $(L => $on_l:expr,)? $(None => $on_none:expr,)?) => {
                match self.lex.next() {
                    Some(Token::S) => default!($($on_s)?, Some(ParseError::UnrecognizedInst.into())),
                    Some(Token::T) => default!($($on_t)?, Some(ParseError::UnrecognizedInst.into())),
                    Some(Token::L) => default!($($on_l)?, Some(ParseError::UnrecognizedInst.into())),
                    Some(Token::InvalidUtf8) => Some(ParseError::InvalidUtf8.into()),
                    None => default!($($on_none)?, Some(ParseError::IncompleteInst.into())),
                }
            }
        );

        match_token!(
            S => match_token!(
                S => Some(self.parse_integer(Inst::Push)),
                T => match_token!(
                    S => Some(self.parse_integer(Inst::Copy)),
                    L => Some(self.parse_integer(Inst::Slide)),
                ),
                L => match_token!(
                    S => Some(Inst::Dup),
                    T => Some(Inst::Swap),
                    L => Some(Inst::Drop),
                ),
            ),
            T => match_token!(
                S => match_token!(
                    S => match_token!(
                        S => Some(Inst::Add),
                        T => Some(Inst::Sub),
                        L => Some(Inst::Mul),
                    ),
                    T => match_token!(
                        S => Some(Inst::Div),
                        T => Some(Inst::Mod),
                    ),
                ),
                T => match_token!(
                    S => Some(Inst::Store),
                    T => Some(Inst::Retrieve),
                ),
                L => match_token!(
                    S => match_token!(
                        S => Some(Inst::Printc),
                        T => Some(Inst::Printi),
                    ),
                    T => match_token!(
                        S => Some(Inst::Readc),
                        T => Some(Inst::Readi),
                    ),
                ),
            ),
            L => match_token!(
                S => match_token!(
                    S => Some(self.parse_label(Inst::Label)),
                    T => Some(self.parse_label(Inst::Call)),
                    L => Some(self.parse_label(Inst::Jmp)),
                ),
                T => match_token!(
                    S => Some(self.parse_label(Inst::Jz)),
                    T => Some(self.parse_label(Inst::Jn)),
                    L => Some(Inst::Ret),
                ),
                L => match_token!(
                    L => Some(Inst::End),
                ),
            ),
            None => None,
        )
    }
}

impl FusedIterator for Parser<'_> {}
