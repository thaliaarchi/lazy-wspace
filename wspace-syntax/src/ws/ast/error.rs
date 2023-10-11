use thiserror::Error;

#[derive(Clone, Copy, Debug, Error, PartialEq, Eq, Hash)]
pub enum ParseError {
    #[error("incomplete instruction opcode")]
    IncompleteOpcode,
    #[error("unrecognized instruction opcode")]
    UnrecognizedOpcode,
    #[error("unterminated integer")]
    UnterminatedInteger,
    #[error("unterminated label")]
    UnterminatedLabel,
    #[error("invalid UTF-8 sequence")]
    InvalidUtf8,
    #[error("unexpected river crab")]
    UnexpectedRiverCrab,
}
