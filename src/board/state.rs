use primitive_types::{U256, U512};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;
use crate::bitfield::BitField;
use crate::error::ParseError;
use crate::error::ParseError::BadLineLen;
use crate::pieces::{Piece, Side};
use crate::pieces::PieceType::{King, Soldier};
use crate::tiles::Tile;

/// Store information on the current board state (ie, pieces).
pub trait BoardState: Default + Clone + Copy + Display + FromStr + Debug + PartialEq {
    
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
    fn set_piece(&mut self, t: Tile, piece: Piece);

    /// Clear a tile.
    fn clear_tile(&mut self, t: Tile);

    /// Get the piece that occupies the given tile, if any.
    fn get_piece(&self, t: Tile) -> Option<Piece>;

    /// Check if there is any piece occupying a tile.
    fn tile_occupied(&self, t: Tile) -> bool;

    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    fn count_pieces(&self, side: Side) -> u8;

    /// Return an iterator over the tiles that are occupied by pieces of the given side. Order of
    /// iteration is not guaranteed.
    fn iter_occupied(&self, side: Side) -> Self::Iter;

    /// Move a piece from one position to another. This does not check whether a move is valid; it
    /// just unsets the bit at `from` and sets the bit at `to`. Returns the piece that was moved.
    /// Panics if there is no piece at `from`.
    fn move_piece(&mut self, from: Tile, to: Tile) -> Piece;

    /// Parse board state from (the relevant part of) a string in FEN format.
    fn from_fen(s: &str) -> Result<Self, ParseError>;

    /// Parse board state from a string in the format output by [`Self::to_display_str`].
    fn from_display_str(s: &str) -> Result<Self, ParseError>;

    /// Return a string in FEN format representing the board state.
    fn to_fen(&self) -> String;

    /// Return a string representing the board state, in a format suitable for printing.
    fn to_display_str(&self) -> String;
    
    /// Return the length of the board's side.
    fn side_len(&self) -> u8;

    /// Swap the pieces at two positions.
    fn swap_pieces(&mut self, t1: Tile, t2: Tile) {
        let p1 = self.get_piece(t1);
        let p2 = self.get_piece(t2);
        for (occupant, tile) in [(p2, t1), (p1, t2)] {
            if let Some(p) = occupant {
                self.set_piece(tile, p);
            } else {
                self.clear_tile(tile);
            }
        }
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
    defenders: T,
    side_len: u8
}

impl<T: BitField> BoardState for BitfieldBoardState<T> {
    
    type Iter = BitfieldIter<T>;

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

    fn set_piece(&mut self, t: Tile, piece: Piece) {
        let mask = T::tile_mask(t);
        let imask = !mask;
        match piece.side {
            Side::Attacker => {
                self.attackers |= mask;
                self.defenders &= imask;
            },
            Side::Defender => {
                self.defenders |= mask;
                self.attackers &= !mask;
            }
        }
        if piece.piece_type == King {
            self.set_king(t)
        }
    }

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

    fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers;
        let mask = T::tile_mask(t);
        (all_pieces & mask) > 0.into()
    }

    fn count_pieces(&self, side: Side) -> u8 {
        (match side {
            Side::Attacker => self.attackers,
            Side::Defender => self.defenders
        } << 4).count_ones() as u8
    }

    fn iter_occupied(&self, side: Side) -> Self::Iter {
        let state_with_king = match side {
            Side::Attacker => self.attackers,
            Side::Defender => self.defenders
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

    fn move_piece(&mut self, from: Tile, to: Tile) -> Piece {
        let piece = self.get_piece(from).expect("No piece to move.");
        self.set_piece(to, piece);
        self.clear_tile(from);
        piece
    }

    fn from_fen(fen: &str) -> Result<Self, ParseError> {
        let mut state = Self::default();
        for (r, line) in fen.split('/').enumerate() {
            let mut n_empty = 0;
            let mut c = 0u8;
            for chr in line.chars() {
                if chr.is_digit(10) {
                    n_empty = (n_empty * 10) + (chr as u8 - '0' as u8);
                } else {
                    c += n_empty;
                    n_empty = 0;
                    state.set_piece(Tile::new(r as u8, c), Piece::try_from(chr)?);
                    c += 1;
                }
            }
            if n_empty > 0 {
                c += n_empty;
            }
            if state.side_len == 0 {
                state.side_len = c;
            } else if state.side_len != c {
                return Err(BadLineLen(c as usize))
            }
        }
        Ok(state)
    }

    fn from_display_str(display_str: &str) -> Result<Self, ParseError> {
        let s = display_str.trim();
        let mut state = Self::default();
        for (r, line) in s.lines().enumerate() {
            let line_len = line.len() as u8;
            if state.side_len == 0 {
                state.side_len = line_len
            } else if line_len != state.side_len {
                return Err(BadLineLen(line.len()))
            }
            for (c, chr) in line.chars().enumerate() {
                if chr != '.' {
                    state.set_piece(Tile::new(r as u8, c as u8), Piece::try_from(chr)?)
                }
            }
        }
        Ok(state)
    }

    fn to_fen(&self) -> String {
        let mut s = String::new();
        for row in 0..self.side_len {
            let mut n_empty = 0;
            for col in 0..self.side_len {
                let t = Tile::new(row, col);
                if let Some(piece) = self.get_piece(t) {
                    if n_empty > 0 {
                        s.push_str(n_empty.to_string().as_str());
                        n_empty = 0;
                    }
                    s.push(piece.into());
                } else {
                    n_empty += 1;
                }
            }
            if n_empty > 0 {
                s.push_str(n_empty.to_string().as_str());
            }
            if row < self.side_len - 1 {
                s.push('/');
            }
        }
        s
    }

    fn to_display_str(&self) -> String {
        let mut s = String::new();
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                let t = Tile::new(r, c);
                let p = self.get_piece(t);
                match p {
                    Some(piece) => s.push(piece.into()),
                    None => s.push('.'),
                }
            }
            s.push('\n');
        }
        s
    }

    fn side_len(&self) -> u8 {
        self.side_len
    }
}

impl<T: BitField> FromStr for BitfieldBoardState<T> {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl <T: BitField> Display for BitfieldBoardState<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_display_str())
    }
}

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 7x7.
pub type SmallBasicBoardState = BitfieldBoardState<u64>;
/// Board state supporting basic pieces (soldier and king), suitable for boards up to 11x11.
pub type MediumBasicBoardState = BitfieldBoardState<u128>;

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 15x15.
pub type LargeBasicBoardState = BitfieldBoardState<U256>;

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 21x21.
pub type HugeBasicBoardState = BitfieldBoardState<U512>;

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::str::FromStr;
    use crate::board::state::{BoardState, MediumBasicBoardState, SmallBasicBoardState};
    use crate::pieces::Piece;
    use crate::pieces::PieceType::{King, Soldier};
    use crate::pieces::Side::{Attacker, Defender};
    use crate::preset::boards;
    use crate::tiles::Tile;

    #[test]
    fn test_from_str() {
        let from_fen = SmallBasicBoardState::from_fen(
            "3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3"
        );
        let from_display_str = SmallBasicBoardState::from_display_str(
            &[
                "...t...",
                "...t...",
                "...T...",
                "ttTKTtt",
                "...T...",
                "...t...",
                "...t..."
            ].join("\n")
        );
        assert_eq!(from_fen, from_display_str);
    }

    #[test]
    fn test_piece_movement() {
        let start_str = "3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3";
        let expected_str = "3tK2/3t1t1/3T3/ttT1Ttt/1T1T3/3t3/3t3";
        let res = SmallBasicBoardState::from_str(start_str);
        assert!(res.is_ok());
        let mut state = res.unwrap();
        assert_eq!(state.get_king(), Tile::new(3, 3));
        state.set_piece(Tile::new(1, 5), Piece::attacker(Soldier));
        state.set_piece(Tile::new(4, 1), Piece::defender(Soldier));
        state.move_piece(Tile::new(3, 3), Tile::new(0, 4));
        assert_eq!(state.get_king(), Tile::new(0, 4));
        assert_eq!(state.to_fen(), expected_str);
    
        let occupied = [
            Tile::new(0, 3),
            Tile::new(2, 3),
            Tile::new(0, 4)
        ];
        for t in occupied {
            assert!(state.tile_occupied(t));
        }
        let empty = [
            Tile::new(3, 3),
            Tile::new(5, 4),
            Tile::new(1, 1)
        ];
        for t in empty {
            assert!(!state.tile_occupied(t));
        }
    }


    #[test]
    fn test_iter_occupied() {
        let state = SmallBasicBoardState::from_str("3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3").unwrap();
        let attackers: HashSet<Tile> = state.iter_occupied(Attacker).collect();
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
        let defenders: HashSet<Tile> = state.iter_occupied(Defender).collect();
        let expected = hashset!(
            Tile::new(2, 3),
            Tile::new(3, 3),
            Tile::new(4, 3),
            Tile::new(3, 2),
            Tile::new(3, 4)
        );
        assert_eq!(defenders, expected);
    }

    #[test]
    fn test_swap_pieces() {
        let mut board = SmallBasicBoardState::from_str("5/1K3/5/5/3t1").unwrap();
        assert_eq!(board.get_piece(Tile::new(1, 1)), Some(Piece::new(King, Defender)));
        assert_eq!(board.get_piece(Tile::new(4, 3)), Some(Piece::new(Soldier, Attacker)));
        assert_eq!(board.get_king(), Tile::new(1, 1));
        board.swap_pieces(Tile::new(1, 1), Tile::new(4, 3));
        assert_eq!(board.get_piece(Tile::new(4, 3)), Some(Piece::new(King, Defender)));
        assert_eq!(board.get_piece(Tile::new(1, 1)), Some(Piece::new(Soldier, Attacker)));
        assert_eq!(board.get_king(), Tile::new(4, 3));

    }
    
    #[test]
    fn test_count_pieces() {
        let board = MediumBasicBoardState::from_str(boards::COPENHAGEN).unwrap();
        assert_eq!(board.count_pieces(Attacker), 24);
        assert_eq!(board.count_pieces(Defender), 13);
    }
}