use crate::tiles::Coords;
use crate::Axis::{Horizontal, Vertical};
use crate::ParseError::{BadPlay, BadString};
use crate::PlayError::DisjointTiles;
use crate::{Axis, ParseError, Piece, PlayError, PlayOutcome, Side, Tile};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

/// A single move of a piece from one tile to another. (Named "Play" rather than "Move" as the lower-cased version of
/// the latter would clash with the Rust keyword.)
///
/// This is implemented as a combination of source tile, axis of movement and displacement (with a
/// negative displacement representing a move "backwards" along the relevant axis, ie, to a
/// lower-numbered row or column). This way, moves are guaranteed to be along a row or column (but
/// are not guaranteed to be within the bounds of the board).
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Play {
    pub from: Tile,
    /// The axis along which the move occurs, ie, horizontal or vertical.
    pub axis: Axis,
    /// The signed distance in tiles covered by the move. A negative number means that the move is
    /// going "backwards", ie, to a lower-numbered row or column.
    pub displacement: i8
}

impl Play {

    pub fn new(from: Tile, axis: Axis, displacement: i8) -> Self {
        Self { from, axis, displacement }
    }

    /// Create a new [`Play`] from source and destination tiles.
    pub fn from_tiles(src: Tile, dst: Tile) -> Result<Self, PlayError> {
        let axis: Axis;
        let displacement: i8;
        if src.row == dst.row {
            axis = Horizontal;
            displacement = (dst.col as i8) - (src.col as i8);
        } else if src.col == dst.col {
            axis = Vertical;
            displacement = (dst.row as i8) - (src.row as i8);
        } else {
            return Err(DisjointTiles)
        };
        Ok(Self::new(src, axis, displacement))
    }

    /// The unsigned distance in tiles covered by the move. Basically the absolute value of
    /// [Play::displacement].
    pub fn distance(&self) -> u8 {
        self.displacement.unsigned_abs()
    }

    /// The move's destination tile.
    pub fn to(&self) -> Tile {
        let d = self.displacement;
        match self.axis {
            Vertical => Tile::new(((self.from.row as i8) + d) as u8, self.from.col),
            Horizontal => Tile::new(self.from.row, ((self.from.col as i8) + d) as u8)
        }
    }

    /// The row and column of the move's destination tile, as [`Coords`]. 
    pub fn to_coords(&self) -> Coords {
        let d = self.displacement;
        match self.axis {
            Vertical => Coords { row: (self.from.row as i8) + d, col: self.from.col as i8 },
            Horizontal => Coords { row: self.from.row as i8, col: (self.from.col as i8) + d }
        }
    }
}

impl FromStr for Play {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens: Vec<&str> = s.split('-').collect();
        if tokens.len() != 2 {
            return Err(BadString(String::from(s)))
        };
        let m_res = Play::from_tiles(
            Tile::from_str(tokens[0])?,
            Tile::from_str(tokens[1])?
        );
        match m_res {
            Ok(m) => Ok(m),
            Err(e) => Err(BadPlay(e))
        }
    }
}

impl Display for Play {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.from, self.to())
    }
}

/// A struct representing a capture of a single piece.
pub struct Capture {
    tile: Tile,
    piece: Piece
}

/// A record of a single play.
pub struct PlayRecord {
    /// The side that made the play.
    pub(crate) side: Side,
    /// Details of the play (piece movement) itself.
    pub(crate) play: Play,
    /// Details of the outcome of the play.
    pub(crate) outcome: PlayOutcome
}
