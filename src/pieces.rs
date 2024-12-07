use std::ops::{BitOr, Shl};
use crate::error::ParseError;
use crate::error::ParseError::BadChar;
use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};
use crate::pieces::Side::{Attacker, Defender};
use crate::tiles::Tile;

/// A convenience reference to the king piece.
pub const KING: Piece = Piece { piece_type: King, side: Defender };

/// The two sides of the game (attacker and defender).
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

/// The different types of pieces that can occupy a board.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
/// A piece belonging to a particular side.
pub struct Piece {
    pub piece_type: PieceType,
    pub side: Side
}

impl Piece {
    /// Create a new piece of the given type and side.
    pub fn new(piece_type: PieceType, side: Side) -> Self {
        Self { piece_type, side }
    }

    /// Create a new king piece.
    pub fn king() -> Self {
        Self {
            piece_type: King,
            side: Defender
        }
    }

    /// Create a new attacking piece of the given type.
    pub fn attacker(piece_type: PieceType) -> Self {
        Self {
            piece_type,
            side: Attacker
        }
    }

    /// Create a new defending piece of the given type.
    pub fn defender(piece_type: PieceType) -> Self {
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
pub struct PlacedPiece {
    pub tile: Tile,
    pub piece: Piece
}

impl PlacedPiece {
    pub fn new(tile: Tile, piece: Piece) -> Self {
        Self { tile, piece }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct PieceSet(u16);

impl From<u16> for PieceSet {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<PieceType> for PieceSet {

    /// Create a new [`PieceSet`] which includes only the given piece type (on each side).
    fn from(value: PieceType) -> Self {
        Self::from_piece_type(value)
    }
}

impl From<Vec<PieceType>> for PieceSet {
    /// Create a new [`PieceSet`] containing the given piece types (on both sides).
    fn from(value: Vec<PieceType>) -> Self {
        Self(value.into_iter().fold(0u16, |acc, piece_type| {
            acc | (piece_type as u16) | ((piece_type as u16) << 8)
        }))
    }
}

impl From<Piece> for PieceSet {
    fn from(value: Piece) -> Self {
        Self::from_piece(value)
    }
}

impl From<Vec<Piece>> for PieceSet {
    fn from(value: Vec<Piece>) -> Self {
        Self(value.into_iter().fold(0u16, |acc, piece| {
            acc | (piece.piece_type << piece.side)
        }))
    }
}

impl Shl<Side> for u16 {
    type Output = u16;

    fn shl(self, rhs: Side) -> Self::Output {
        self << (rhs as u16)
    }
}

impl From<Side> for PieceSet {
    fn from(value: Side) -> Self {
        Self(0b1111_1111u16 << value)
    }
}

impl PieceSet {

    /// Create a new empty [`PieceSet`].
    pub const fn none() -> Self {
        Self(0)
    }

    /// Create a new [`PieceSet`] which includes all pieces on both sides.
    pub const fn all() -> Self {
        Self(0b1111_1111_1111_1111)
    }
    
    /// Create a new [`PieceSet`] which includes the given piece type (on both sides).
    /// 
    /// **NOTE**: You can also use `PieceSet::from(piece_type)` for the same effect, but this
    /// function is `const`.
    pub const fn from_piece_type(value: PieceType) -> Self {
        Self((value as u16) | ((value as u16) << 8))
    }

    /// Create a new [`PieceSet`] which includes the given pieces.
    ///
    /// **NOTE**: You can also use `PieceSet::from(piece)` for the same effect, but this function
    /// is `const`.
    pub const fn from_piece(value: Piece) -> Self {
        Self((value.piece_type as u16) << (value.side as u16))
    }

    /// Get the bitmask corresponding to the given piece type and side. If `side` is `None`, the
    /// mask will represent the piece type of each side.
    fn get_mask(&self, piece_type: PieceType, side: Option<Side>) -> u16 {
        if let Some(s) = side {
            piece_type << s
        } else {
            (piece_type as u16) | ((piece_type as u16) << 8) 
        }
    }
    
    /// Add the given piece to the set.
    pub fn set_piece(&mut self, piece: Piece) {
        self.0 |= self.get_mask(piece.piece_type, Some(piece.side));
    }
    
    /// Add the given piece type (both sides) to the set.
    pub fn set_piece_type(&mut self, piece_type: PieceType) {
        self.0 |= self.get_mask(piece_type, None)
    }

    /// Remove the given piece from the set.
    pub fn unset_piece(&mut self, piece: Piece) {
        self.0 &= !self.get_mask(piece.piece_type, Some(piece.side));
    }
    
    /// Remove the given piece type (both sides) from the set.
    pub fn unset_piece_type(&mut self, piece_type: PieceType) {
        self.0 &= !self.get_mask(piece_type, None)
    }

    /// Check whether the set contains the given piece.
    pub fn contains(&self, piece: Piece) -> bool {
        self.0 & self.get_mask(piece.piece_type, Some(piece.side)) > 0
    }
    
}

#[cfg(test)]
mod tests {
    use crate::pieces::{Piece, PieceSet};
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
}
