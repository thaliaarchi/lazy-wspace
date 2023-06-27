use std::str::from_utf8_unchecked;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TokenKind {
    Word,
    String,
    UnterminatedString,
    Space,
    LF,
    LineComment,
    BlockComment,
    UnopenedBlockComment,
    UnclosedBlockComment,
}

const EOF_BYTE: u8 = 0;

#[derive(Clone, Debug)]
struct Lexer<'a> {
    src: &'a [u8],
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        // Source files are required to be valid UTF-8, but the grammar is all
        // ASCII, so we can scan over bytes instead of chars.
        Lexer {
            src: src.as_bytes(),
            offset: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<(TokenKind, &'a str)> {
        let start = self.offset;
        let tok = match self.bump()? {
            b' ' | b'\t' => {
                self.eat_while(|b| b == b' ' || b == b'\t');
                TokenKind::Space
            }
            b'\n' => TokenKind::LF,
            b'"' => self.string(),
            b';' => self.line_comment(),
            b'-' if self.peek() == b'-' => {
                self.bump();
                self.line_comment()
            }
            b'{' if self.peek() == b'-' => {
                self.bump();
                self.block_comment()
            }
            b'-' if self.peek() == b'}' => {
                self.bump();
                TokenKind::UnopenedBlockComment
            }
            _ => self.word(),
        };
        let s = unsafe { from_utf8_unchecked(&self.src[start..self.offset]) };
        Some((tok, s))
    }

    fn word(&mut self) -> TokenKind {
        loop {
            match self.peek() {
                EOF_BYTE | b' ' | b'\t' | b'"' | b';' => break,
                b'{' | b'-' if self.peek_n(1) == b'-' => break,
                _ => self.bump(),
            };
        }
        TokenKind::Word
    }

    fn string(&mut self) -> TokenKind {
        self.eat_while(|b| b != b'"');
        if self.peek() == b'"' {
            self.bump();
            TokenKind::String
        } else {
            TokenKind::UnterminatedString
        }
    }

    fn line_comment(&mut self) -> TokenKind {
        self.eat_while(|b| b != b'\n');
        TokenKind::LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                b'{' if self.peek() == b'-' => {
                    self.bump();
                    depth += 1;
                }
                b'-' if self.peek() == b'}' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        return TokenKind::BlockComment;
                    }
                }
                _ => {}
            }
        }
        TokenKind::UnclosedBlockComment
    }

    fn bump(&mut self) -> Option<u8> {
        match self.src.get(self.offset) {
            Some(b) => {
                self.offset += 1;
                Some(*b)
            }
            None => None,
        }
    }

    fn peek(&self) -> u8 {
        self.peek_n(0)
    }

    fn peek_n(&self, n: usize) -> u8 {
        self.src.get(self.offset + n).copied().unwrap_or(EOF_BYTE)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.src.len()
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(u8) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }
}
