use std::ops::Shl;
use crate::pieces::{Piece, PieceType, Side};
use crate::pieces::PieceType::King;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A set of (unplaced) pieces. Can contain pieces of each side.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

    /// Return a copy of this [`PieceSet`] but with the given piece included.
    pub const fn with_piece(&self, piece: Piece) -> Self {
        Self(self.0 | self.get_mask(piece.piece_type, Some(piece.side)))
    }

    /// Return a copy of this [`PieceSet`] but with the given piece type (both sides) included.
    pub const fn with_piece_type(&self, piece_type: PieceType) -> Self {
        Self(self.0 | self.get_mask(piece_type, None))
    }

    /// Get the bitmask corresponding to the given piece type and side. If `side` is `None`, the
    /// mask will represent the piece type of each side.
    const fn get_mask(&self, piece_type: PieceType, side: Option<Side>) -> u16 {
        if let Some(s) = side {
            (piece_type as u16) << (s as u16)
        } else {
            (piece_type as u16) | ((piece_type as u16) << 8)
        }
    }

    /// Add the given piece to the set.
    pub const fn set_piece(&mut self, piece: Piece) {
        self.0 |= self.get_mask(piece.piece_type, Some(piece.side));
    }

    /// Add the given piece type (both sides) to the set.
    pub const fn set_piece_type(&mut self, piece_type: PieceType) {
        self.0 |= self.get_mask(piece_type, None)
    }

    /// Remove the given piece from the set.
    pub const fn unset_piece(&mut self, piece: Piece) {
        self.0 &= !self.get_mask(piece.piece_type, Some(piece.side));
    }

    /// Remove the given piece type (both sides) from the set.
    pub const fn unset_piece_type(&mut self, piece_type: PieceType) {
        self.0 &= !self.get_mask(piece_type, None)
    }

    /// Check whether the set contains the given piece.
    pub const fn contains(&self, piece: Piece) -> bool {
        self.0 & self.get_mask(piece.piece_type, Some(piece.side)) > 0
    }

    /// Check whether the set contains all the pieces in the other set.
    pub const fn contains_set(&self, other: &Self) -> bool {
        (other.0 & self.0) == other.0
    }

    /// Check whether the set is empty.
    pub const fn is_empty(&self) -> bool {
        // Filter irrelevant bits
        self.0 & 0b0011_1111_0011_1111 == 0
    }

    /// Check whether the set contains only the king. Returns `true` if the set contains only the
    /// defending king OR if the set contains only both kings, but returns `false` if the set
    /// contains only the attacking king.
    pub const fn is_king_only(&self) -> bool {
        let mut c = *self;
        c.unset_piece_type(King);
        (!self.is_empty()) && c.is_empty()
    }
}