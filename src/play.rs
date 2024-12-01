use crate::tiles::{AxisOffset, Coords};
use crate::Axis::{Horizontal, Vertical};
use crate::ParseError::{BadPlay, BadString};
use crate::PlayError::DisjointTiles;
use crate::{Axis, ParseError, PlayError, PlayOutcome, Side, Tile};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

/// A single move of a piece from one tile to another. (Named "Play" rather than "Move" as the lower-cased version of
/// the latter would clash with the Rust keyword.)
///
/// This is implemented as a combination of source tile, axis of movement and displacement (with a
/// negative displacement representing a move "backwards" along the relevant axis, ie, to a
/// lower-numbered row or column). This way, moves are guaranteed to be along a row or column (but
/// are not guaranteed to be within the bounds of the board).
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Play {
    pub from: Tile,
    /// The axis along which the move occurs, ie, horizontal or vertical.
    pub movement: AxisOffset,
}

impl Play {

    pub fn new(from: Tile, movement: AxisOffset) -> Self {
        Self { from, movement }
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
        Ok(Self::new(src, AxisOffset::new(axis, displacement)))
    }

    /// The unsigned distance in tiles covered by the move. Basically the absolute value of
    /// the displacement.
    pub fn distance(&self) -> u8 {
        self.movement.displacement.unsigned_abs()
    }

    /// The move's destination tile.  **NOTE**: This creates a tile without checking whether it is
    /// in bounds. Use the output with caution.
    pub fn to(&self) -> Tile {
        let coords = self.to_coords();
        Tile::new(coords.row as u8, coords.col as u8)
    }

    /// The row and column of the move's destination tile, as [`Coords`]. 
    pub fn to_coords(&self) -> Coords {
        Coords::from(self.from) + self.movement
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


/// A record of a single play.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PlayRecord {
    /// The side that made the play.
    pub side: Side,
    /// Details of the play (piece movement) itself.
    pub play: Play,
    /// Details of the outcome of the play.
    pub outcome: PlayOutcome
}

impl PlayRecord {
    
    /// Whether these two records are equal, ignoring the outcomes of the moves.
    pub fn eq_ignore_outcome(&self, other: &Self) -> bool {
        self.side == other.side && self.play == other.play
    }
}

/// A record of all past plays in a game.
#[derive(Debug, Default)]
pub struct PlayHistory {
    attacker_moves: Vec<PlayRecord>,
    defender_moves: Vec<PlayRecord>
}