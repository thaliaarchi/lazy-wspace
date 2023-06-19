// Lexer is modeled on the design of Cursor in rustc_lexer
// (https://github.com/rust-lang/rust/tree/e52794decda5e55d730436bb38c06958a117e2c8/compiler/rustc_lexer/src).

use std::iter::FusedIterator;
use std::str::Chars;

use unicode_ident::{is_xid_continue, is_xid_start};

/// Whitespace assembly token.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    fn new(kind: TokenKind, len: u32) -> Self {
        Token { kind, len }
    }
}

/// Whitespace assembly token kind.
///
/// A word represents an instruction mnemonic or a label, depending on the
/// context. Since words are usually separated by spaces (except for around some
/// punctuation), the character set can be permissive. Camel case, snake case,
/// and kebab case all work. Unicode identifiers are supported. Additionally,
/// all ASCII symbols are allowed in a word except for `:` `;` `,` `[` `]` and
/// `#`, which have their own meaning, and `(` `)` `{` `}` and `` ` ``, which
/// make less sense in an identifier and are reserved for future use.
///
/// # Grammar
///
/// ```bnf
/// word           ::= (XID_Start | word_symbol) (XID_Continue | word_symbol)*
/// word_symbol    ::= [!$%&*+-./<=>?@\\^_|~]
/// integer        ::= [-+]? (dec_integer | bin_integer | oct_integer | hex_integer)
/// dec_integer    ::= ([1-9] ("_"* [0-9])* | "0")
/// bin_integer    ::= "0" [bB] ("_"* [01])*
/// oct_integer    ::= "0" [oO] ("_"* [0-7])+
/// hex_integer    ::= "0" [xX] ("_"* [0-9 a-f A-F])+
/// char           ::= "'" … "'"
/// string         ::= "\"" … "\""
/// colon          ::= ":"
/// semi           ::= ";"
/// comma          ::= ","
/// lbracket       ::= "["
/// rbracket       ::= "]"
/// line_comment   ::= ("#" | "//" | "--") [^\n]*
/// block_comment  ::= "/*" .* "*/"
/// nested_comment ::= "{-" .*? (nested_comment .*?)* "-}"?
/// space          ::= " " | "\t"
/// line_break     ::= "\n" | "\r\n"
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// Instruction mnemonic or label
    Word,

    /// Integer literal
    Integer,
    /// `' '` character literal
    Char,
    /// `" "` string literal
    String,

    /// `:`
    Colon,
    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `[`
    LBracket,
    /// `]`
    RBracket,

    /// `#`, `//`, or `--` line comment
    LineComment,
    /// `/* */` non-nested or `{- -}` nested block comment
    BlockComment,
    /// Unicode whitespace, excluding LF
    Space,
    /// LF or CRLF
    LineBreak,
    /// End of file
    Eof,

    /// Token error
    Error,
}

const EOF_CHAR: char = '\0';

/// Lexer for Whitespace assembly.
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            chars: src.chars(),
            len_remaining: src.len(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        let ch = match self.bump() {
            Some(ch) => ch,
            None => return Token::new(TokenKind::Eof, 0),
        };
        let kind = match ch {
            '.' => match self.peek() {
                ch if is_word_start(ch) => {
                    self.bump();
                    self.word()
                }
                _ => TokenKind::Error,
            },

            '0'..='9' => self.integer(ch),
            '\'' => self.char(),
            '"' => self.string(),

            ':' => TokenKind::Colon,
            ';' => TokenKind::Semi,
            ',' => TokenKind::Comma,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,

            '+' => match self.peek() {
                ch @ '0'..='9' => {
                    self.bump();
                    self.integer(ch)
                }
                _ => self.word(),
            },
            '-' => match self.peek() {
                ch2 @ '0'..='9' => {
                    self.bump();
                    self.integer(ch2)
                }
                '-' => {
                    self.bump();
                    self.line_comment()
                }
                _ => self.word(),
            },
            '#' => self.line_comment(),
            '/' => match self.peek() {
                '/' => {
                    self.bump();
                    self.line_comment()
                }
                '*' => {
                    self.bump();
                    self.c_block_comment()
                }
                _ => self.word(),
            },
            '{' => match self.peek() {
                '-' => {
                    self.bump();
                    self.nested_block_comment()
                }
                _ => TokenKind::Error,
            },

            '\n' => TokenKind::LineBreak,
            '\r' => match self.peek() {
                '\n' => {
                    self.bump();
                    TokenKind::LineBreak
                }
                _ => TokenKind::Space,
            },

            _ if is_word_start(ch) => self.word(),
            _ if ch.is_whitespace() => self.space(),
            _ => TokenKind::Error,
        };
        let len = self.token_len();
        self.reset_token_len();
        Token::new(kind, len)
    }

    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn token_len(&self) -> u32 {
        (self.len_remaining - self.chars.as_str().len()) as u32
    }

    fn reset_token_len(&mut self) {
        self.len_remaining = self.chars.as_str().len();
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }

    fn word(&mut self) -> TokenKind {
        self.eat_while(is_word_continue);
        TokenKind::Word
    }

    fn integer(&mut self, first_digit: char) -> TokenKind {
        // Allow erroneous integers here, such as having no digits, digits
        // invalid for the base, or a leading zero in decimal.
        if first_digit == '0' {
            match self.peek() {
                'b' | 'B' => {
                    self.bump();
                }
                'o' | 'O' => {
                    self.bump();
                }
                'x' | 'X' => {
                    self.bump();
                    self.eat_while(|ch| matches!(ch, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'));
                    return TokenKind::Integer;
                }
                _ => {}
            }
        }
        self.eat_while(|ch| matches!(ch, '0'..='9' | '_'));
        TokenKind::Integer
    }

    fn char(&mut self) -> TokenKind {
        // Parse until either the quotes are terminated or the end of the line
        // or file is reached.
        loop {
            match self.peek() {
                '\'' => {
                    self.bump();
                    break;
                }
                '\\' => {
                    self.bump();
                    self.bump();
                }
                '\n' => break,
                EOF_CHAR if self.is_eof() => break,
                _ => {
                    self.bump();
                }
            }
        }
        TokenKind::Char
    }

    fn string(&mut self) -> TokenKind {
        while let Some(c) = self.bump() {
            match c {
                '"' => break,
                '\\' if matches!(self.peek(), '\\' | '"') => {
                    self.bump();
                }
                _ => {}
            }
        }
        TokenKind::String
    }

    fn line_comment(&mut self) -> TokenKind {
        self.eat_while(|ch| ch != '\n');
        TokenKind::LineComment
    }

    fn c_block_comment(&mut self) -> TokenKind {
        while let Some(c) = self.bump() {
            match c {
                '*' if self.peek() == '/' => {
                    self.bump();
                    break;
                }
                _ => {}
            }
        }
        TokenKind::BlockComment
    }

    fn nested_block_comment(&mut self) -> TokenKind {
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '{' if self.peek() == '-' => {
                    self.bump();
                    depth += 1;
                }
                '-' if self.peek() == '}' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        TokenKind::BlockComment
    }

    fn space(&mut self) -> TokenKind {
        self.eat_while(|ch| ch.is_whitespace() && ch != '\n');
        TokenKind::Space
    }
}

fn is_word_start(ch: char) -> bool {
    is_xid_start(ch) || is_word_symbol(ch)
}

fn is_word_continue(ch: char) -> bool {
    is_xid_continue(ch) || is_word_symbol(ch)
}

fn is_word_symbol(ch: char) -> bool {
    // All ASCII symbols except for : ; # " ' ` ( ) [ ] { }
    match ch {
        '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '_' | '|' | '~' => true,
        _ => false,
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.kind != TokenKind::Eof {
            Some(tok)
        } else {
            None
        }
    }
}

impl FusedIterator for Lexer<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(src: &str) -> Vec<Token> {
        Lexer::new(src).collect()
    }

    #[test]
    fn whitelips_macro() {
        // https://github.com/vii5ard/whitespace/blob/a42adf9407063fd4be09047e6d254364c5e5b9d2/example/lib/std.wsa#L17
        assert_eq!(
            vec![
                Token::new(TokenKind::Word, 5),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Word, 6),
                Token::new(TokenKind::Colon, 1),
                Token::new(TokenKind::LineBreak, 1),
                Token::new(TokenKind::Space, 2),
                Token::new(TokenKind::Word, 4),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Word, 7),
                Token::new(TokenKind::LineBreak, 1),
                Token::new(TokenKind::Space, 2),
                Token::new(TokenKind::Word, 4),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Word, 10),
                Token::new(TokenKind::LineBreak, 1),
                Token::new(TokenKind::Word, 2),
                Token::new(TokenKind::LineBreak, 1),
            ],
            lex("macro prints:\n  push $string\n  call printStack\n$$\n"),
        );
    }

    #[test]
    fn limews_macro() {
        // https://github.com/ManaRice/whitespace/blob/e8db8719e170c12875dac571c39ac811c7d0ec52/ws/wsa/hextest.wsa#L5
        assert_eq!(
            vec![
                Token::new(TokenKind::Space, 4),
                Token::new(TokenKind::Word, 5),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Word, 7),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::LBracket, 1),
                Token::new(TokenKind::Word, 4),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Char, 4),
                Token::new(TokenKind::Space, 1),
                Token::new(TokenKind::Word, 6),
                Token::new(TokenKind::RBracket, 1),
                Token::new(TokenKind::LineBreak, 1),
            ],
            lex("    macro PRINTNL [push '\\n' printc]\n"),
        );
    }
}
