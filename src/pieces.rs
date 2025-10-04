use std::fmt::Display;
use std::ops::{BitOr, Shl};
use std::str::FromStr;
use crate::error::ParseError;
use crate::error::ParseError::BadChar;
use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};
use crate::pieces::Side::{Attacker, Defender};
use crate::tiles::Tile;

#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};
pub(crate) use crate::collections::pieceset::PieceSet;

/// A convenience reference to the king piece.
pub const KING: Piece = Piece { piece_type: King, side: Defender };

/// All the pieces commonly used in basic games (soldiers of each side and a defending king).
pub const BASIC_PIECES: PieceSet = PieceSet::none()
    .with_piece_type(Soldier)
    .with_piece(KING);

/// The two sides of the game (attacker and defender).
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Side {
    Attacker = 0,
    Defender = 8
}

impl Side {

    /// Return the other side.
    pub fn other(&self) -> Self {
        match self {
            Attacker => Defender,
            Defender => Attacker
        }
    }
}

impl FromStr for Side {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "attacker" => Ok(Attacker),
            "defender" => Ok(Defender),
            other => Err(ParseError::BadString(other.to_string()))
        }
    }
}

impl Display for Side {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attacker => write!(f, "attacker"),
            Defender => write!(f, "defender")
        }
    }   
}

/// The different types of pieces that can occupy a board.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum PieceType {
    King =      0b0000_0001,
    Soldier =   0b0000_0010,
    Knight =    0b0000_0100,
    Commander = 0b0000_1000,
    Guard =     0b0001_0000,
    Mercenary = 0b0010_0000
}

impl Shl<Side> for PieceType {
    
    type Output = u16;
    fn shl(self, rhs: Side) -> Self::Output {
        (self as u16) << (rhs as u16)
    }
}

impl BitOr<PieceType> for PieceType {
    type Output = u8;
    fn bitor(self, rhs: PieceType) -> Self::Output {
        (self as u8) | (rhs as u8)
    }
}

impl BitOr<PieceType> for u16 {
    type Output = u16;

    fn bitor(self, rhs: PieceType) -> Self::Output {
        self | (rhs as u16)
    }
}

/// A piece belonging to a particular side.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Piece {
    pub piece_type: PieceType,
    pub side: Side
}

impl Piece {
    /// Create a new piece of the given type and side.
    pub const fn new(piece_type: PieceType, side: Side) -> Self {
        Self { piece_type, side }
    }

    /// Create a new king piece.
    pub const fn king() -> Self {
        Self {
            piece_type: King,
            side: Defender
        }
    }

    /// Create a new attacking piece of the given type.
    pub const fn attacker(piece_type: PieceType) -> Self {
        Self {
            piece_type,
            side: Attacker
        }
    }

    /// Create a new defending piece of the given type.
    pub const fn defender(piece_type: PieceType) -> Self {
        Self {
            piece_type,
            side: Defender
        }
    }
}

impl From<Piece> for char {
    /// A single-character representation of a given piece.
    fn from(value: Piece) -> Self {
        let c = match value.piece_type {
            Soldier => 't',
            King => 'k',
            Knight => 'n',
            Commander => 'c',
            Guard => 'g',
            Mercenary => 'm'
        };
        match value.side {
            Attacker => c,
            Defender => c.to_ascii_uppercase()
        }
    }
}

impl TryFrom<char> for Piece {

    type Error = ParseError;
    fn try_from(mut value: char) -> Result<Self, Self::Error> {
        if !value.is_alphabetic() {
            return Err(BadChar(value))
        }
        let side = if value.is_ascii_uppercase() {
            value = value.to_ascii_lowercase();
            Defender
        } else {
            Attacker
        };
        match value {
            't' => Ok(Piece::new(Soldier, side)),
            'k' => Ok(Piece::new(King, side)),
            'n' => Ok(Piece::new(Knight, side)),
            'c' => Ok(Piece::new(Commander, side)),
            'g' => Ok(Piece::new(Guard, side)),
            'm' => Ok(Piece::new(Mercenary, side)),
            other => Err(BadChar(other))
        }
    }
}

/// A struct representing a combination of a tile and a piece.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlacedPiece {
    pub tile: Tile,
    pub piece: Piece
}

impl PlacedPiece {
    pub fn new(tile: Tile, piece: Piece) -> Self {
        Self { tile, piece }
    }
}



#[cfg(test)]
mod tests {
    use crate::pieces::{Piece, PieceSet, KING};
    use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};
    use crate::pieces::Side::{Attacker, Defender};

    #[test]
    fn test_piece_set() {
        let mut ps = PieceSet::from(vec![
            King,
            Soldier,
            Guard
        ]);
        for s in [Attacker, Defender] {
            assert!(ps.contains(Piece::new(King, s)));
            assert!(ps.contains(Piece::new(Soldier, s)));
            assert!(ps.contains(Piece::new(Guard, s)));
            assert!(!ps.contains(Piece::new(Commander, s)));
            assert!(!ps.contains(Piece::new(Knight, s)));
            assert!(!ps.contains(Piece::new(Mercenary, s)));
        }

        ps.unset_piece(Piece::new(King, Attacker));
        assert!(ps.contains(Piece::new(King, Defender)));
        assert!(!ps.contains(Piece::new(King, Attacker)));
        for s in [Attacker, Defender] {
            assert!(ps.contains(Piece::new(Soldier, s)));
            assert!(ps.contains(Piece::new(Guard, s)));
            assert!(!ps.contains(Piece::new(Commander, s)));
            assert!(!ps.contains(Piece::new(Knight, s)));
            assert!(!ps.contains(Piece::new(Mercenary, s)));
        }

        ps.set_piece(Piece::new(Commander, Defender));
        assert!(ps.contains(Piece::new(Commander, Defender)));
        assert!(!ps.contains(Piece::new(Commander, Attacker)));
        assert!(ps.contains(Piece::new(King, Defender)));
        assert!(!ps.contains(Piece::new(King, Attacker)));
        for s in [Attacker, Defender] {
            assert!(ps.contains(Piece::new(Soldier, s)));
            assert!(ps.contains(Piece::new(Guard, s)));
            assert!(!ps.contains(Piece::new(Knight, s)));
            assert!(!ps.contains(Piece::new(Mercenary, s)));
        }
    }

    #[test]
    fn test_empty() {
        assert!(PieceSet::none().is_empty());
        assert!(!PieceSet::from(vec![King]).is_empty());
        assert!(!PieceSet::from(vec![Soldier, Guard]).is_empty());
        assert!(!PieceSet::from_piece(Piece::new(Soldier, Attacker)).is_empty());
        assert!(!PieceSet::all().is_empty());

    }

    #[test]
    fn test_king_only() {
        assert!(PieceSet::from(vec![King]).is_king_only());
        assert!(PieceSet::from_piece(KING).is_king_only());
        assert!(!PieceSet::from(vec![King, Soldier]).is_king_only());
        assert!(!PieceSet::from(vec![King, Soldier, Guard]).is_king_only());
        assert!(!PieceSet::from(vec![Soldier, Guard, Commander]).is_king_only());
        assert!(!PieceSet::none().is_king_only());
    }
}
