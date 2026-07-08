use serde::{Deserialize, Serialize};
use crate::collections::PieceTypeDict;
use crate::pieces::{Piece, PieceType, Side};


/// A generic struct that allows each [`Piece`] to be associated with a value.
#[derive(PartialEq, Eq, Copy, Clone, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PieceDict<T> {
    pub(crate) attacker: PieceTypeDict<T>,
    pub(crate) defender: PieceTypeDict<T>,
}

impl<T> PieceDict<T> {

    /// Get a reference to the value associated with the given `Piece`.
    pub fn get(&self, piece: Piece) -> &T {
        match piece.side {
            Side::Attacker => &self.attacker,
            Side::Defender => &self.defender,
        }.get(piece.piece_type)
    }

    /// Get a mutable reference to the value associated with the given `Piece`.
    pub fn get_mut(&mut self, piece: Piece) -> &mut T {
        match piece.side {
            Side::Attacker => &mut self.attacker,
            Side::Defender => &mut self.defender,
        }.get_mut(piece.piece_type)
    }

    /// Set the value associated with the given `Piece`.
    pub fn set(&mut self, piece: Piece, value: T) {
        match piece.side {
            Side::Attacker => &mut self.attacker,
            Side::Defender => &mut self.defender
        }.set(piece.piece_type, value)
    }
}

impl<T: Copy> PieceDict<T> {
    /// Create a new `PieceDict` with all `Piece`s set to the given value.
    pub const fn new(value: T) -> Self {
        Self {
            attacker: PieceTypeDict::new(value),
            defender: PieceTypeDict::new(value),
        }
    }

    pub const fn with(self, piece: Piece, value: T) -> Self {
        let Self { attacker, defender } = self;
        match piece.side {
            Side::Attacker => Self { attacker: attacker.with(piece.piece_type, value), defender },
            Side::Defender => Self { attacker, defender: defender.with(piece.piece_type, value) }
        }
    }

}