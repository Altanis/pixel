use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("CompilerError: Unexpected token {0} at line {1}")]
    UnidentifiedError(String, usize),
    #[error("CompilerError: Unexpected end of file while reading token. You may be missing a semicolon.")]
    UnexpectedEOF,
    #[error("CompilerError: Unexpected newline in the middle of string at line {0}")]
    UnexpectedNewline(usize),
    #[error("CompilerError: Unrecognised escape sequence {0} at line {1}")]
    InvalidEscapeSequence(String, usize),
    #[error("CompilerError: Degenerate number formation ({0}) at line {1}")]
    InvalidNumber(String, usize)
}