use std::iter::FusedIterator;
use std::str;

use thiserror::Error;

use crate::source::{FileId, FileSpan};
use crate::wsa::burghard::{Lexer, Token};
use crate::wsa::{Word, WordFormat};

/// Parser for lines of words in Burghard assembly.
#[derive(Clone, Debug)]
pub struct LineParser<'a> {
    lex: Lexer<'a>,
    src: &'a str,
    has_error: bool,
    file: FileId,
}

/// Error from [`LineParser`].
#[derive(Clone, Debug, Error, PartialEq, Eq, Hash)]
pub enum ParseError {
    #[error("unterminated string at {0:?}")]
    UnterminatedString(FileSpan),
    #[error("unopened block comment at {0:?}")]
    UnopenedBlockComment(FileSpan),
    #[error("unclosed block comment at {0:?}")]
    UnclosedBlockComment(FileSpan),
    #[error("block comment between words concatenated {0:?} and {1:?}")]
    ConcatenatedWords(FileSpan, FileSpan),
    #[error("missing space between {0:?} and {1:?}")]
    MissingSpace(FileSpan, FileSpan),
}

impl<'a> LineParser<'a> {
    /// Constructs a `LineParser` for parsing the given source.
    #[inline]
    pub fn new(file: FileId, src: &'a str) -> Self {
        LineParser {
            lex: Lexer::new(src),
            src,
            has_error: false,
            file,
        }
    }

    /// Parses the next line into words, reusing allocations with the given
    /// buffer.
    pub fn next_line(&mut self, line: &mut Vec<Word>) {
        line.clear();
        let mut adjacent = None;

        while let Some((tok, span)) = self.lex.next() {
            let text = &self.src[span.range()];
            let span = span.with_file(self.file);

            let word = match tok {
                Token::Word => {
                    if let Some(adjacent_span) = adjacent {
                        if let Some(last) = line.last_mut() {
                            if matches!(last.format, WordFormat::Bare | WordFormat::Spliced) {
                                // Concatenate words, that are separated by a
                                // block comment and no surrounding spaces.
                                self.warn(ParseError::ConcatenatedWords(adjacent_span, span));
                                adjacent = Some(span);
                                last.text += text;
                                last.format = WordFormat::Spliced;
                                last.span = FileSpan::new(
                                    self.file,
                                    last.span.start(),
                                    span.end() - last.span.start(),
                                );
                                continue;
                            }
                        }
                    }
                    Word {
                        text: text.into(),
                        format: WordFormat::Bare,
                        span,
                    }
                }
                Token::String => Word {
                    text: text[1..text.len() - 1].into(),
                    format: WordFormat::DoubleQuoted,
                    span,
                },
                Token::UnterminatedString => {
                    self.error(ParseError::UnterminatedString(span));
                    Word {
                        text: text[1..].into(),
                        format: WordFormat::DoubleQuoted,
                        span,
                    }
                }
                Token::Space => {
                    adjacent = None;
                    continue;
                }
                Token::Lf => {
                    if line.len() != 0 {
                        break;
                    } else {
                        continue;
                    }
                }
                // The next token is LF or EOF.
                Token::LineComment => continue,
                // Block comments are not treated as spaces and paste adjacent
                // words together.
                Token::BlockComment => continue,
                Token::UnopenedBlockComment => {
                    self.error(ParseError::UnopenedBlockComment(span));
                    continue;
                }
                Token::UnclosedBlockComment => {
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

    fn error(&mut self, err: ParseError) {
        // TODO: Make diagnostics system
        eprintln!("Error: {err}");
        self.has_error = true;
    }

    fn warn(&mut self, err: ParseError) {
        eprintln!("Warn: {err}");
    }
}

impl Iterator for LineParser<'_> {
    type Item = Vec<Word>;

    fn next(&mut self) -> Option<Self::Item> {
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
