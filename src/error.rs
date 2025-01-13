use std::num::ParseIntError;
use crate::error::ParseError::BadInt;

/// Errors that may be encountered when parsing a string.
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
    /// Tried to parse a string which represents an invalid [`Play`].
    BadPlay(PlayError),
    /// A generic error type where the given string could not be parsed for some reason.
    BadString(String)
    
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        BadInt(value)
    }
}

/// Errors that may be encountered when constructing a [`Play`].
#[derive(Debug, Eq, PartialEq)]
pub enum PlayError {
    DisjointTiles
}

/// Errors relating to the board.
#[derive(Debug, Eq, PartialEq)]
pub enum BoardError {
    /// Coordinates are out of bounds, ie, not on board.
    OutOfBounds,
    /// There is no piece at the given tile, where one is expected.
    NoPiece
}

/// Different ways a [`Play`] can be invalid.
#[derive(Debug, Eq, PartialEq)]
pub enum PlayInvalid {
    /// The piece being moved does not belong to the player whose turn it is.
    WrongPlayer,
    /// There is no piece to move at the given tile.
    NoPiece,
    /// The destination tile would be outside the board.
    OutOfBounds,
    /// The start and end tiles do not share an axis (ie, they are not on the same row or column).
    NoCommonAxis,
    /// Another piece is blocking the move.
    BlockedByPiece,
    /// The move is blocked by a special tile which, according to the game rules, is not passable
    /// by this piece.
    MoveThroughBlockedTile,
    /// This move would end on a special tile which, according to the game rules, this piece may not
    /// occupy.
    MoveOntoBlockedTile,
    /// The move is further than this piece is permitted to move in one go.
    TooFar,
    /// Game is already over.
    GameOver
}