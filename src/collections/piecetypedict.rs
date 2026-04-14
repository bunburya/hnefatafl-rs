use std::fmt::Debug;
use serde::{Deserialize, Serialize};
use crate::pieces::PieceType;

/// A generic struct that allows each [`PieceType`] to be associated with a value.
///
/// Unlike a normal "dict", all `PieceType`s *must* have an associated value of type `T`.
#[derive(PartialEq, Eq, Copy, Clone, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PieceTypeDict<T> {
    pub(crate) values: [T; 6]
}

impl<T> PieceTypeDict<T> {

    /// Get a reference to the value associated with the given `PieceType`.
    pub fn get(&self, piece_type: PieceType) -> &T {
        &self.values[(piece_type as u8).trailing_zeros() as usize]
    }

    /// Get a mutable reference to the value associated with the given `PieceType`.
    pub fn get_mut(&mut self, piece_type: PieceType) -> &mut T {
        &mut self.values[(piece_type as u8).trailing_zeros() as usize]
    }

    /// Set the value associated with the given `PieceType`.
    pub fn set(&mut self, piece_type: PieceType, value: T) {
        self.values[(piece_type as u8).trailing_zeros() as usize] = value;
    }
}

impl<T: Copy> PieceTypeDict<T> {
    /// Create a new `PieceTypeDict` with all `PieceType`s set to the given value.
    pub const fn new(value: T) -> Self {
        Self { values: [value; 6] }
    }

    /// Return a new `PieceTypeDict` with the given `PieceType` set to the given value, destroying
    /// the original `PieceTypeDict` in the process. This allows chained calls.
    pub const fn with(mut self, piece_type: PieceType, value: T) -> Self {
        self.values[(piece_type as u8).trailing_zeros() as usize] = value;
        self
    }

}