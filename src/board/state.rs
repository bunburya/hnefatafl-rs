use crate::bitfield::BitField;
use crate::error::ParseError;
use crate::error::ParseError::BadLineLen;
use crate::pieces::PieceType::{King, Soldier};
use crate::pieces::{Piece, PieceSet, Side};
use crate::tiles::Tile;
use primitive_types::{U256, U512};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;

use crate::tileset::TileSet;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Store information on the current board state (ie, pieces).
pub trait BoardState: Default + Clone + Copy + Display + FromStr + Debug + PartialEq {

    type BitField: BitField;

    /// Get the tile on which the king is currently placed.
    fn get_king(&self) -> Option<Tile>;

    /// Store the given location as the position of the king.
    fn set_king(&mut self, t: Tile);

    /// Check whether the given tile contains the king.
    fn is_king(&self, t: Tile) -> bool {
        self.get_king() == Some(t)
    }

    /// Place a piece representing the given side at the given position.
    fn set_piece(&mut self, t: Tile, piece: Piece);

    /// Clear a tile.
    fn clear_tile(&mut self, t: Tile);

    /// Clear all tiles in the given set.
    fn clear_tiles(&mut self, tiles: TileSet<Self::BitField>);

    /// Get the piece that occupies the given tile, if any.
    fn get_piece(&self, t: Tile) -> Option<Piece>;

    /// Check if there is any piece occupying a tile.
    fn tile_occupied(&self, t: Tile) -> bool;

    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    fn count_pieces_of_side(&self, side: Side) -> u8;

    /// Return an iterator over the tiles that are occupied by pieces of the given side. Order of
    /// iteration is not guaranteed.
    fn occupied_by_side(&self, side: Side) -> TileSet<Self::BitField>;

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

    /// Return a set of all tiles occupied by any piece in the given set.
    fn occupied_by(&self, piece_set: PieceSet) -> TileSet<Self::BitField>;

    /// Return the set of all tiles occupied by any piece.
    fn occupied_by_any(&self) -> TileSet<Self::BitField>;

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
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound = "B: serde::Serialize + serde::de::DeserializeOwned"))]
pub struct BitfieldBasicBoardState<B: BitField> {
    attackers: TileSet<B>,
    defenders: TileSet<B>,
    king: TileSet<B>,
    side_len: u8
}

impl<B: BitField> BoardState for BitfieldBasicBoardState<B> {

    type BitField = B;

    fn get_king(&self) -> Option<Tile> {
        if self.king.is_empty() {
            None
        } else {
            Some(self.king.first())
        }
    }

    fn set_king(&mut self, t: Tile) {
        self.king.insert(t);
    }

    fn set_piece(&mut self, t: Tile, piece: Piece) {
        match piece {
            Piece { piece_type: Soldier, side: Side::Attacker } => {
                self.attackers.insert(t);
                self.defenders.remove(t);
                self.king.remove(t);
            },
            Piece { piece_type: Soldier, side: Side::Defender } => {
                self.defenders.insert(t);
                self.attackers.remove(t);
                self.king.remove(t);
            },
            Piece { piece_type: King, side: Side::Defender } => {
                self.king.insert(t);
                self.attackers.remove(t);
                self.defenders.remove(t);
            },
            // Attacking king. Should never happen. Just do nothing.
            _ => ()
        }
    }

    fn clear_tile(&mut self, t: Tile) {
        self.attackers.remove(t);
        self.defenders.remove(t);
        self.king.remove(t);
    }

    fn clear_tiles(&mut self, tiles: TileSet<B>) {
        let inv = !tiles;
        self.attackers &= inv;
        self.defenders &= inv;
        self.king &= inv;
    }

    fn get_piece(&self, t: Tile) -> Option<Piece> {
        if self.attackers.contains(t) {
            Some(Piece::attacker(Soldier))
        } else if self.defenders.contains(t) {
            Some(Piece::defender(Soldier))
        } else if self.king.contains(t) {
            Some(Piece::king())
        } else {
            None
        }
    }

    fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers | self.king;
        all_pieces.contains(t)
    }

    fn count_pieces_of_side(&self, side: Side) -> u8 {
        (match side {
            Side::Attacker => self.attackers.count(),
            Side::Defender => (self.defenders | self.king).count()
        }) as u8
    }

    fn occupied_by_side(&self, side: Side) -> TileSet<B> {
        match side {
            Side::Attacker => self.attackers,
            Side::Defender => self.defenders | self.king
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

    fn occupied_by(&self, piece_set: PieceSet) -> TileSet<B> {
        let mut s = TileSet::empty();
        if piece_set.contains(Piece::attacker(Soldier)) {
            s.extend(&self.attackers);
        }
        if piece_set.contains(Piece::defender(Soldier)) {
            s.extend(&self.defenders);
        }
        if piece_set.contains(Piece::king()) {
            s.extend(&self.king);
        }
        s
    }

    fn occupied_by_any(&self) -> TileSet<B> {
        self.attackers | self.defenders | self.king
    }
}

impl<T: BitField> FromStr for BitfieldBasicBoardState<T> {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl <T: BitField> Display for BitfieldBasicBoardState<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_display_str())
    }
}

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 7x7.
pub type SmallBasicBoardState = BitfieldBasicBoardState<u64>;
/// Board state supporting basic pieces (soldier and king), suitable for boards up to 11x11.
pub type MediumBasicBoardState = BitfieldBasicBoardState<u128>;

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 15x15.
pub type LargeBasicBoardState = BitfieldBasicBoardState<U256>;

/// Board state supporting basic pieces (soldier and king), suitable for boards up to 21x21.
pub type HugeBasicBoardState = BitfieldBasicBoardState<U512>;

#[cfg(test)]
mod tests {
    use crate::board::state::{BoardState, MediumBasicBoardState, SmallBasicBoardState};
    use crate::pieces::Piece;
    use crate::pieces::PieceType::{King, Soldier};
    use crate::pieces::Side::{Attacker, Defender};
    use crate::preset::boards;
    use crate::tiles::Tile;
    use crate::tileset::TileSet;
    use std::str::FromStr;

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
        assert_eq!(state.get_king(), Some(Tile::new(3, 3)));
        state.set_piece(Tile::new(1, 5), Piece::attacker(Soldier));
        state.set_piece(Tile::new(4, 1), Piece::defender(Soldier));
        state.move_piece(Tile::new(3, 3), Tile::new(0, 4));
        assert_eq!(state.get_king(), Some(Tile::new(0, 4)));
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
        let attackers = state.occupied_by_side(Attacker);
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
        assert_eq!(attackers, TileSet::from(expected));
        let defenders = state.occupied_by_side(Defender);
        let expected = hashset!(
            Tile::new(2, 3),
            Tile::new(3, 3),
            Tile::new(4, 3),
            Tile::new(3, 2),
            Tile::new(3, 4)
        );
        assert_eq!(defenders, TileSet::from(expected));
    }

    #[test]
    fn test_swap_pieces() {
        let mut board = SmallBasicBoardState::from_str("5/1K3/5/5/3t1").unwrap();
        assert_eq!(board.get_piece(Tile::new(1, 1)), Some(Piece::new(King, Defender)));
        assert_eq!(board.get_piece(Tile::new(4, 3)), Some(Piece::new(Soldier, Attacker)));
        assert_eq!(board.get_king(), Some(Tile::new(1, 1)));
        board.swap_pieces(Tile::new(1, 1), Tile::new(4, 3));
        assert_eq!(board.get_piece(Tile::new(4, 3)), Some(Piece::new(King, Defender)));
        assert_eq!(board.get_piece(Tile::new(1, 1)), Some(Piece::new(Soldier, Attacker)));
        assert_eq!(board.get_king(), Some(Tile::new(4, 3)));

    }
    
    #[test]
    fn test_count_pieces() {
        let board = MediumBasicBoardState::from_str(boards::COPENHAGEN).unwrap();
        assert_eq!(board.count_pieces_of_side(Attacker), 24);
        assert_eq!(board.count_pieces_of_side(Defender), 13);
    }
}