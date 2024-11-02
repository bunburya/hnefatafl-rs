use crate::{BitField, Piece, Side, Tile};
use crate::PieceType::{King, Soldier};
use crate::Side::{Attacker, Defender};

/// Store information on the current board state (ie, pieces). This struct currently handles only a
/// simple board, ie, a king and soldiers (no knights, commanders, etc).
pub trait BoardState: Default {

    /// Get the tile on which the king is currently placed.
    fn get_king(&self) -> Tile;

    /// Store the given location as the position of the king. **NB**: Does not set the relevant bit
    /// (or unset the bit corresponding to the king's previous location), which must be handled
    /// separately.
    fn set_king(&mut self, t: Tile);

    /// Check whether the given tile contains the king.
    fn is_king(&self, t: Tile) -> bool {
        self.get_king() == t
    }

    /// Place a piece representing the given side at the given position by setting the relevant bit.
    fn place_piece(&mut self, t: Tile, piece: Piece);

    /// Clear a tile by unsetting the relevant bit.
    fn clear_tile(&mut self, t: Tile);

    fn get_piece(&self, t: Tile) -> Option<Piece>;

    /// Check if there is any piece occupying a tile.
    fn tile_occupied(&self, t: Tile) -> bool;

    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    fn count_pieces(&self, side: Side) -> u8;
}

/// Store information on the current board state (ie, pieces). This struct currently handles only a
/// simple board, ie, a king and soldiers (no knights, commanders, etc).
///
/// Bitfields are used to minimise memory usage. The parameter `T` is a type that implements the
/// [`BitField`] trait (most of the integer types do this), ensuring that it supports the relevant
/// bitwise operations.  A single integer of type `T` is used to record the positions of all
/// attacker pieces, and another integer is used to record the positions of the defender bits. There
/// should generally be some bits left over, which are used to encode the current position of the
/// king.
///
/// Currently only basic getting and setting is implemented at the bitfield level. More complex game
/// logic (like checking move validity, etc) is implemented elsewhere and uses [Tile] structs. If
/// performance was an issue we could look at moving some of that logic to the bitfield level.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default)]
pub struct BitfieldBoardState<T: BitField> {
    attackers: T,
    defenders: T
}

impl<T: BitField> BoardState for BitfieldBoardState<T> {

    /// Get the tile on which the king is currently placed.
    fn get_king(&self) -> Tile {
        let row = (self.defenders.to_be_bytes().as_ref()[0] & 0b1111_0000) >> 4;
        let col = (self.attackers.to_be_bytes().as_ref()[0] & 0b1111_0000) >> 4;
        Tile::new(row, col)
    }

    /// Store the given location as the position of the king. **NB**: Does not set the relevant bit
    /// (or unset the bit corresponding to the king's previous location), which must be handled
    /// separately.
    fn set_king(&mut self, t: Tile) {
        let mut def_bytes = self.defenders.to_be_bytes();
        let def_bytes_slice = def_bytes.as_mut();
        def_bytes_slice[0] &= 0b0000_1111;  // Unset 4 most significant bits
        def_bytes_slice[0] |= t.row << 4;  // Set 4 most significant bits to row
        self.defenders = T::from_be_bytes_slice(def_bytes_slice);
        let mut att_bytes = self.attackers.to_be_bytes();
        let att_bytes_slice = att_bytes.as_mut();
        att_bytes_slice[0] &= 0b0000_1111;
        att_bytes_slice[0] |= t.col << 4;
        self.attackers = T::from_be_bytes_slice(att_bytes_slice);
    }

    /// Place a piece representing the given side at the given position by setting the relevant bit.
    fn place_piece(&mut self, t: Tile, piece: Piece) {
        let mask = T::tile_mask(t);
        match piece.side {
            Attacker => self.attackers |= mask,
            Defender => self.defenders |= mask
        }
        if piece.piece_type == King {
            self.set_king(t)
        }
    }

    /// Clear a tile by unsetting the relevant bit.
    fn clear_tile(&mut self, t: Tile) {
        let mask = !T::tile_mask(t);
        self.attackers &= mask;
        self.defenders &= mask;
    }

    fn get_piece(&self, t: Tile) -> Option<Piece> {
        let mask = T::tile_mask(t);
        if (self.defenders & mask) > 0.into() {
            if self.is_king(t) {
                Some(Piece::king())
            } else {
                Some(Piece::defender(Soldier))
            }
        } else if (self.attackers & mask) > 0.into() {
            Some(Piece::attacker(Soldier))
        } else {
            None
        }

    }

    /// Check if there is any piece occupying a tile.
    fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers;
        let mask = T::tile_mask(t);
        (all_pieces & mask) > 0.into()
    }

    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    fn count_pieces(&self, side: Side) -> u8 {
        match side {
            Attacker => self.attackers,
            Defender => self.defenders
        }.count_ones() as u8
    }
}

pub type SmallBoardState = BitfieldBoardState<u64>;
pub type MediumBoardState = BitfieldBoardState<u128>;