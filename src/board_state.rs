use crate::ParseError::BadLineLen;
use crate::PieceType::{King, Soldier};
use crate::Side::{Attacker, Defender};
use crate::{BitField, ParseError, Piece, Side, Tile};
use primitive_types::{U256, U512};


/// Store information on the current board state (ie, pieces). 
pub trait BoardState: Default + Clone {
    
    type Iter: Iterator<Item=Tile>;

    /// Get the tile on which the king is currently placed.
    fn get_king(&self) -> Tile;

    /// Store the given location as the position of the king. 
    fn set_king(&mut self, t: Tile);

    /// Check whether the given tile contains the king.
    fn is_king(&self, t: Tile) -> bool {
        self.get_king() == t
    }

    /// Place a piece representing the given side at the given position.
    fn place_piece(&mut self, t: Tile, piece: Piece);

    /// Clear a tile.
    fn clear_tile(&mut self, t: Tile);

    /// Get the piece that occupies the given tile, if any.
    fn get_piece(&self, t: Tile) -> Option<Piece>;

    /// Check if there is any piece occupying a tile.
    fn tile_occupied(&self, t: Tile) -> bool;

    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    fn count_pieces(&self, side: Side) -> u8;

    /// Iterate over all occupied tiles on the board. Order of iteration is not guaranteed.
    fn iter_occupied(&self, side: Side) -> Self::Iter;
    
    /// Parse board state from a string, returning the state and the length of the board's side.
    fn from_str_with_side_len(value: &str) -> Result<(Self, u8), ParseError> {
        let s = value.trim();
        let mut side_len = 0u8;
        let mut state = Self::default();
        for (r, line) in s.lines().enumerate() {
            let line_len = line.len() as u8;
            if side_len == 0 {
                side_len = line_len
            } else if line_len != side_len {
                return Err(BadLineLen(line.len()))
            }
            for (c, chr) in line.chars().enumerate() {
                if chr != '.' {
                    state.place_piece(Tile::new(r as u8, c as u8), Piece::try_from(chr)?)
                }
            }
        }
        Ok((state, side_len))
    }

    fn from_fen_with_side_len(value: &str) -> Result<(Self, u8), ParseError> {
        let mut side_len = 0u8;
        let mut state = Self::default();
        for (r, line) in value.split('/').enumerate() {
            let mut n_empty = 0;
            let mut c = 0u8;
            for chr in line.chars() {
                if chr.is_digit(10) {
                    n_empty = (n_empty * 10) + (chr as u8 - '0' as u8);
                } else {
                    c += n_empty;
                    n_empty = 0;
                    state.place_piece(Tile::new(r as u8, c), Piece::try_from(chr)?);
                    c += 1;
                }
            }
            if n_empty > 0 {
                c += n_empty;
            }
            println!("{line}, size {c}");
            if side_len == 0 {
                side_len = c;
            } else if side_len != c {
                return Err(BadLineLen(c as usize))
            }
        }
        Ok((state, side_len))
    }
}

pub struct BitfieldIter<T: BitField> {
    /// Bitfield representing board state.
    state: T,
    /// Keeps track of current position in the bitfield.
    i: u32,
}

impl<T: BitField> Iterator for BitfieldIter<T> {
    type Item = Tile;

    fn next(&mut self) -> Option<Self::Item> {
        let skipped = self.state >> self.i;
        if skipped.is_empty()  {
            return None
        }
        self.i += skipped.trailing_zeros() + 1;
        Some(T::bit_to_tile(self.i - 1))
    }
}

/// Store information on the current board state (ie, pieces) using bitfields. This struct currently
/// handles only a simple board, ie, a king and soldiers (no knights, commanders, etc).
///
/// The parameter `T` is a type that implements the [`BitField`] trait, ensuring that it supports
/// the relevant bitwise operations.  A single integer of type `T` is used to record the positions
/// of all attacker pieces, and another integer is used to record the positions of the defender
/// bits. There should generally be some bits left over, which are used to encode the current
/// position of the king.
///
/// Currently only basic getting and setting is implemented at the bitfield level. More complex game
/// logic (like checking move validity, etc) is implemented elsewhere and uses [Tile] structs. If
/// performance was an issue we could look at moving some of that logic to the bitfield level.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default, Debug)]
pub struct BitfieldBoardState<T: BitField> {
    attackers: T,
    defenders: T
}

impl<T: BitField> BoardState for BitfieldBoardState<T> {
    
    type Iter = BitfieldIter<T>;

    /// Get the tile occupied by the king.
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

    /// Get the piece, if any, that occupies the given tile.
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

    /// Check whether the given tile is occupied by any piece.
    fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers;
        let mask = T::tile_mask(t);
        (all_pieces & mask) > 0.into()
    }

    /// Return the number of pieces of the given side on the board.
    fn count_pieces(&self, side: Side) -> u8 {
        match side {
            Attacker => self.attackers,
            Defender => self.defenders
        }.count_ones() as u8
    }

    /// Return an iterator over the tiles that are occupied by pieces of the given side.
    fn iter_occupied(&self, side: Side) -> Self::Iter {
        let state_with_king = match side {
            Attacker => self.attackers,
            Defender => self.defenders
        };
        // unset bits which encode the position of the king
        let mut state_bytes = state_with_king.to_be_bytes();
        let state_bytes_slice = state_bytes.as_mut();
        state_bytes_slice[0] &= 0b0000_1111;  // Unset 4 most significant bits
        let state = T::from_be_bytes_slice(state_bytes_slice);
        Self::Iter {
            state,
            i: 0
        }
    }
}

/// Board state suitable for boards up to 7x7.
pub type SmallBoardState = BitfieldBoardState<u64>;
/// Board state suitable for boards up to 11x11.
pub type MediumBoardState = BitfieldBoardState<u128>;

/// Board state suitable for boards up to 15x15.
pub type LargeBoardState = BitfieldBoardState<U256>;

/// Board state suitable for boards up to 21x21.
pub type HugeBoardState = BitfieldBoardState<U512>;

#[cfg(test)]
mod tests {
    use crate::board_state::{BoardState, LargeBoardState};
    use crate::Side::{Attacker, Defender};
    use crate::{hashset, BitfieldBoardState, MediumBoardState, Tile};
    use std::collections::HashSet;

    #[test]
    fn test_from_fen() {
        let res = LargeBoardState::from_fen_with_side_len(
            "3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3"
        );
        assert!(res.is_ok());
        let (state, len) = res.unwrap();

        let (bench_state, bench_len) = LargeBoardState::from_str_with_side_len(
            &[
                "...t...",
                "...t...",
                "...T...",
                "ttTKTtt",
                "...T...",
                "...t...",
                "...t..."
            ].join("\n")
        ).unwrap();
        assert_eq!(state, bench_state);
        assert_eq!(len, bench_len);
    }

    #[test]
    fn test_iter_occupied() {
        let state_str = [
            "...t...",
            "...t...",
            "...T...",
            "ttTKTtt",
            "...T...",
            "...t...",
            "...t..."
        ].join("\n");
        let (board_state, _): (MediumBoardState, u8) 
            = BitfieldBoardState::from_str_with_side_len(&state_str).unwrap();
        let attackers: HashSet<Tile> = board_state.iter_occupied(Attacker).collect();
        let expected = hashset!(
            Tile::new(0, 3),
            Tile::new(1, 3),
            Tile::new(5, 3),
            Tile::new(6, 3),
            Tile::new(3, 0),
            Tile::new(3, 1),
            Tile::new(3, 5),
            Tile::new(3, 6)
        );
        assert_eq!(attackers, expected);
        let defenders: HashSet<Tile> = board_state.iter_occupied(Defender).collect();
        let expected = hashset!(
            Tile::new(2, 3),
            Tile::new(3, 3),
            Tile::new(4, 3),
            Tile::new(3, 2),
            Tile::new(3, 4)
        );
        assert_eq!(defenders, expected);
    }
}