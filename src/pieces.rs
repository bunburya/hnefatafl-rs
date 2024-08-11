use std::ops::BitOr;
use crate::error::ParseError;
use crate::error::ParseError::BadChar;
use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};
use crate::pieces::Side::{Attacker, Defender};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum Side {
    Attacker = 0x40,
    Defender = 0x80
}

impl Side {

    /// Return the other side.
    pub(crate) fn other(&self) -> Self {
        match self {
            Attacker => Defender,
            Defender => Attacker
        }
    }
}

/// The different types of pieces that can occupy a board.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum PieceType {
    King = 0x01,
    Soldier = 0x02,
    Knight = 0x04,
    Commander = 0x08,
    Guard = 0x10,
    Mercenary = 0x20
}

impl BitOr<PieceType> for PieceType {
    type Output = u8;
    fn bitor(self, rhs: PieceType) -> Self::Output {
        (self as u8) | (rhs as u8)
    }
}

impl BitOr<PieceType> for u8 {
    type Output = u8;

    fn bitor(self, rhs: PieceType) -> Self::Output {
        self | (rhs as u8)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
/// A piece belonging to a particular side.
pub(crate) struct Piece {
    pub(crate) piece_type: PieceType,
    pub(crate) side: Side
}

impl Piece {
    /// Create a new piece of the given type and side.
    pub(crate) fn new(piece_type: PieceType, side: Side) -> Self {
        Self { piece_type, side }
    }

    /// Create a new king piece.
    pub(crate) fn king() -> Self {
        Self {
            piece_type: King,
            side: Defender
        }
    }

    /// Create a new attacking piece of the given type.
    pub(crate) fn attacker(piece_type: PieceType) -> Self {
        Self {
            piece_type,
            side: Attacker
        }
    }

    /// Create a new defending piece of the given type.
    pub(crate) fn defender(piece_type: PieceType) -> Self {
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

#[derive(Copy, Clone, Debug)]
pub(crate) struct PieceSet(u8);

impl PieceSet {

    pub(crate) const fn from_byte(byte: u8) -> Self {
        Self { 0: byte }
    }

    pub(crate) const fn from_piece_type(piece_type: PieceType) -> Self {
        Self { 0: piece_type as u8 }
    }

    pub(crate) const fn none() -> Self {
        Self { 0: 0 }
    }

    pub(crate) const fn all() -> Self {
        Self { 0: 0xFF }
    }

    pub(crate) fn from_piece_types<T: IntoIterator<Item = PieceType>>(piece_types: T) -> Self {
        PieceSet{ 0: piece_types.into_iter().fold(0, u8::bitor) }
    }

    pub(crate) fn set(&mut self, piece: PieceType) {
        self.0 |= piece as u8
    }

    pub(crate) fn unset(&mut self, piece: PieceType) {
        self.0 &= !(piece as u8)
    }

    pub(crate) fn contains(&self, piece: PieceType) -> bool {
        self.0 & (piece as u8) > 0
    }
    
}

#[cfg(test)]
mod tests {
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};

    #[test]
    fn test_piece_set() {
        let mut ps = PieceSet::from_piece_types(vec![
            King,
            Soldier,
            Guard
        ]);
        assert!(ps.contains(King));
        assert!(ps.contains(Soldier));
        assert!(ps.contains(Guard));
        assert!(!ps.contains(Commander));
        assert!(!ps.contains(Knight));
        assert!(!ps.contains(Mercenary));

        ps.unset(King);
        assert!(!ps.contains(King));
        assert!(ps.contains(Soldier));
        assert!(ps.contains(Guard));
        assert!(!ps.contains(Commander));
        assert!(!ps.contains(Knight));
        assert!(!ps.contains(Mercenary));

        ps.set(Commander);
        assert!(ps.contains(Commander));
        assert!(!ps.contains(King));
        assert!(ps.contains(Soldier));
        assert!(ps.contains(Guard));
        assert!(!ps.contains(Knight));
        assert!(!ps.contains(Mercenary));
    }

}