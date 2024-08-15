use std::num::ParseIntError;
use crate::error::ParseError::BadInt;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    /// Tried to parse a string, but it was not the expected length. The given `usize` is the
    /// actual length.
    BadStringLen(usize),
    /// Tried to parse a multi-line string but encountered a line that was not the expected length.
    /// The given `usize` is the actual length.
    BadLineLen(usize),
    /// Encountered an unexpected character in a string.
    BadChar(char),
    /// Tried to parse an empty string.
    EmptyString,
    /// Could not parse an integer from a string. This variant wraps the [`ParseIntError`] that was
    /// returned when trying to parse.
    BadInt(ParseIntError),
    /// Tried to parse a string which represents an invalid [`Move`].
    BadMove(MoveError),
    /// A generic error type where the given string could not be parsed for some reason.
    BadString(String)
    
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        BadInt(value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum MoveError {
    DisjointTiles
}
