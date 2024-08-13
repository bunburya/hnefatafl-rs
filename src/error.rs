use std::num::ParseIntError;
use crate::error::ParseError::BadInt;

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ParseError {
    BadStringLen(usize),
    BadLineLen(usize),
    BadChar(char),
    EmptyString,
    BadInt(ParseIntError),
    BadMove(MoveError),
    BadString(String)
    
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        BadInt(value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum MoveError {
    DisjointTiles
}
