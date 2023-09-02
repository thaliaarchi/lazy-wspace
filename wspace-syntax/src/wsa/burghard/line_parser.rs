use std::borrow::Cow;
use std::iter::FusedIterator;
use std::ops::Index;

use thiserror::Error;

use crate::source::{FileId, Span};
use crate::wsa::burghard::{Lexer, TokenKind};

/// Parser for lines of words in Burghard assembly.
#[derive(Clone, Debug)]
pub struct LineParser<'a> {
    lex: Lexer<'a>,
    has_error: bool,
    file: FileId,
}

/// Word or string in Burghard assembly.
///
/// When a block comment separates two words, without any surrounding spaces, it
/// concatenates the words to form one word. Strings are unwrapped and treated
/// identically to non-string words after lexing.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word<'a> {
    pub text: Cow<'a, str>,
    pub is_string: bool,
    pub span: Span,
}

/// Error from [`LineParser`].
#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum ParseError {
    #[error("unterminated string at {0:?}")]
    UnterminatedString(Span),
    #[error("unopened block comment at {0:?}")]
    UnopenedBlockComment(Span),
    #[error("unclosed block comment at {0:?}")]
    UnclosedBlockComment(Span),
    #[error("block comment between words concatenated {0:?} and {1:?}")]
    ConcatenatedWords(Span, Span),
    #[error("missing space between {0:?} and {1:?}")]
    MissingSpace(Span, Span),
}

impl<'a> LineParser<'a> {
    /// Constructs a `LineParser` for parsing the given source.
    #[inline]
    pub fn new(file: FileId, src: &'a str) -> Self {
        LineParser {
            lex: Lexer::new(src),
            has_error: false,
            file,
        }
    }

    /// Parses the next line into words, reusing allocations with the given
    /// buffer.
    pub fn next_line(&mut self, line: &mut Vec<Word<'a>>) {
        line.clear();
        let mut adjacent = None;

        while let Some(tok) = self.lex.next() {
            let span = Span::new(self.file, tok.start, tok.end - tok.start);

            let word = match tok.kind {
                TokenKind::Word => {
                    let text = self.text(tok.start..tok.end);
                    if let Some(adjacent_span) = adjacent {
                        if let Some(last) = line.last_mut() {
                            if !last.is_string {
                                // Concatenate words, that are separated by a
                                // block comment and no surrounding spaces.
                                self.warn(ParseError::ConcatenatedWords(adjacent_span, span));
                                adjacent = Some(span);
                                last.text += text;
                                last.span = Span::new(
                                    self.file,
                                    last.span.start(),
                                    tok.end - last.span.start(),
                                );
                                continue;
                            }
                        }
                    }
                    Word {
                        text: text.into(),
                        is_string: false,
                        span,
                    }
                }
                TokenKind::String => Word {
                    text: self.text(tok.start + 1..tok.end - 1).into(),
                    is_string: true,
                    span,
                },
                TokenKind::UnterminatedString => {
                    self.error(ParseError::UnterminatedString(span));
                    Word {
                        text: self.text(tok.start + 1..tok.end).into(),
                        is_string: true,
                        span,
                    }
                }
                TokenKind::Space => {
                    adjacent = None;
                    continue;
                }
                TokenKind::Lf => {
                    if line.len() != 0 {
                        break;
                    } else {
                        continue;
                    }
                }
                // The next token is LF or EOF.
                TokenKind::LineComment => continue,
                // Block comments are not treated as spaces and paste adjacent
                // words together.
                TokenKind::BlockComment => continue,
                TokenKind::UnopenedBlockComment => {
                    self.error(ParseError::UnopenedBlockComment(span));
                    continue;
                }
                TokenKind::UnclosedBlockComment => {
                    self.error(ParseError::UnclosedBlockComment(span));
                    continue;
                }
            };
            line.push(word);

            if let Some(adjacent_span) = adjacent.take() {
                self.warn(ParseError::MissingSpace(adjacent_span, span));
            }
            adjacent = Some(span);
        }
    }

    #[inline]
    fn text<T>(&self, range: T) -> &'a str
    where
        str: Index<T, Output = str>,
    {
        &self.lex.source_text()[range]
    }

    fn error(&mut self, err: ParseError) {
        // TODO: Make diagnostics system
        eprintln!("Error: {err}");
        self.has_error = true;
    }

    fn warn(&mut self, err: ParseError) {
        eprintln!("Warn: {err}");
    }
}

impl<'a> Iterator for LineParser<'a> {
    type Item = Vec<Word<'a>>;

    fn next(&mut self) -> Option<Vec<Word<'a>>> {
        let mut line = Vec::new();
        self.next_line(&mut line);
        if line.len() != 0 {
            Some(line)
        } else {
            None
        }
    }
}

impl FusedIterator for LineParser<'_> {}
