use crate::bitfield::BitField;
use crate::error::ParseError;
use crate::error::ParseError::BadLineLen;
use crate::pieces::{Piece, Side};
use crate::tiles::Tile;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;

use crate::collections::piecemap::{BasicPieceMap, PieceMap};
use crate::collections::pieceset::PieceSet;
use crate::collections::tileset::TileSet;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};


/// This struct stores information about piece placement, by piece type. It is mainly a wrapper
/// around a [`PieceMap`] though it also stores board length. This struct represents a simple board,
/// i.e. a king and soldiers (no knights, commanders, etc.).
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BoardState<P: PieceMap> {
    pub pieces: P,
    side_len: u8,
}

impl<P: PieceMap> BoardState<P> {

    /// Check whether the given tile contains the king.
    pub(crate) fn is_king(&self, t: Tile) -> bool {
        self.pieces.find_king() == Some(t)
    }

    /// Place a piece representing the given side at the given position.
    fn set_piece(&mut self, t: Tile, piece: Piece) {
        self.pieces.set(t, piece);
    }

    /// Clear a tile.
    fn clear_tile(&mut self, t: Tile) {
        self.pieces.remove(t);
    }

    /// Clear all tiles in the given set.
    fn clear_tiles(&mut self, tiles: &TileSet<P::BitField>) {
        self.pieces.clear_tiles(*tiles);
    }

    /// Get the piece that occupies the given tile, if any.
    pub(crate) fn get_piece(&self, t: Tile) -> Option<Piece> {
        self.pieces.get(t)
    }

    /// Check if there is any piece occupying a tile.
    pub(crate) fn tile_occupied(&self, t: Tile) -> bool {
        self.pieces.occupied().contains(t)
    }

    /// Move a piece from one position to another. This does not check whether a move is valid; it
    /// just unsets the bit at `from` and sets the bit at `to`. Returns the piece that was moved.
    /// Panics if there is no piece at `from`.
    pub fn move_piece(&mut self, from: Tile, to: Tile) -> Piece {
        let piece = self.get_piece(from).expect("No piece to move.");
        self.set_piece(to, piece);
        self.clear_tile(from);
        piece
    }

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


    /// Parse board state from (the relevant part of) a string in FEN format.
    pub(crate) fn from_fen(fen: &str) -> Result<Self, ParseError> {
        let (pieces, side_len) = P::from_fen(fen)?;
        Ok(Self { pieces, side_len })
    }

    /// Parse board state from a string in the format output by [`Self::to_display_str`].
    fn from_display_str(display_str: &str) -> Result<Self, ParseError> {
        let s = display_str.trim();
        let mut state = Self::default();
        for (r, line) in s.lines().enumerate() {
            let line_len = line.len() as u8;
            if state.side_len == 0 {
                state.side_len = line_len
            } else if line_len != state.side_len {
                return Err(BadLineLen(line.len()));
            }
            for (c, chr) in line.chars().enumerate() {
                if chr != '.' {
                    state.set_piece(Tile::new(r as u8, c as u8), Piece::try_from(chr)?)
                }
            }
        }
        Ok(state)
    }

    /// Return a string in FEN format representing the board state.
    pub(crate) fn to_fen(&self) -> String {
        self.pieces.to_fen(self.side_len)
    }

    /// Return a string representing the board state, in a format suitable for printing.
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

    /// Return the length of the board's side.
    pub(crate) fn side_len(&self) -> u8 {
        self.side_len
    }

    /// Return a set of all tiles occupied by any piece in the given set.
    fn occupied_by(&self, piece_set: &PieceSet) -> TileSet<P::BitField> {
        self.pieces.occupied_by(*piece_set)
    }

    /// Return the set of all tiles occupied by any piece.
    fn occupied_by_any(&self) -> TileSet<P::BitField> {
        self.pieces.occupied()
    }

}

impl<P: PieceMap> FromStr for BoardState<P> {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl<P: PieceMap> Display for BoardState<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_display_str())
    }
}

#[cfg(test)]
mod tests {
    use crate::aliases::{MediumBasicBoardState, SmallBasicBoardState};
    use crate::collections::tileset::TileSet;
    use crate::collections::PieceMap;
    use crate::pieces::Piece;
    use crate::pieces::PieceType::{King, Soldier};
    use crate::pieces::Side::{Attacker, Defender};
    use crate::preset::boards;
    use crate::tiles::Tile;
    use std::str::FromStr;

    #[test]
    fn test_from_str() {
        let from_fen = SmallBasicBoardState::from_fen("3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3");
        let from_display_str = SmallBasicBoardState::from_display_str(
            &[
                "...t...", "...t...", "...T...", "ttTKTtt", "...T...", "...t...", "...t...",
            ]
            .join("\n"),
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
        assert_eq!(state.pieces.find_king(), Some(Tile::new(3, 3)));
        state.set_piece(Tile::new(1, 5), Piece::attacker(Soldier));
        state.set_piece(Tile::new(4, 1), Piece::defender(Soldier));
        state.move_piece(Tile::new(3, 3), Tile::new(0, 4));
        assert_eq!(state.pieces.find_king(), Some(Tile::new(0, 4)));
        assert_eq!(state.to_fen(), expected_str);

        let occupied = [Tile::new(0, 3), Tile::new(2, 3), Tile::new(0, 4)];
        for t in occupied {
            assert!(state.tile_occupied(t));
        }
        let empty = [Tile::new(3, 3), Tile::new(5, 4), Tile::new(1, 1)];
        for t in empty {
            assert!(!state.tile_occupied(t));
        }
    }

    #[test]
    fn test_iter_occupied() {
        let state = SmallBasicBoardState::from_str("3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3").unwrap();
        let attackers = state.pieces.occupied_by_side(Attacker);
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
        assert_eq!(attackers, TileSet::from(expected.iter()));
        let defenders = state.pieces.occupied_by_side(Defender);
        let expected = hashset!(
            Tile::new(2, 3),
            Tile::new(3, 3),
            Tile::new(4, 3),
            Tile::new(3, 2),
            Tile::new(3, 4)
        );
        assert_eq!(defenders, TileSet::from(expected.iter()));
    }

    #[test]
    fn test_swap_pieces() {
        let mut board = SmallBasicBoardState::from_str("5/1K3/5/5/3t1").unwrap();
        assert_eq!(
            board.get_piece(Tile::new(1, 1)),
            Some(Piece::new(King, Defender))
        );
        assert_eq!(
            board.get_piece(Tile::new(4, 3)),
            Some(Piece::new(Soldier, Attacker))
        );
        assert_eq!(board.pieces.find_king(), Some(Tile::new(1, 1)));
        board.swap_pieces(Tile::new(1, 1), Tile::new(4, 3));
        assert_eq!(
            board.get_piece(Tile::new(4, 3)),
            Some(Piece::new(King, Defender))
        );
        assert_eq!(
            board.get_piece(Tile::new(1, 1)),
            Some(Piece::new(Soldier, Attacker))
        );
        assert_eq!(board.pieces.find_king(), Some(Tile::new(4, 3)));
    }

    #[test]
    fn test_count_pieces() {
        let board = MediumBasicBoardState::from_str(boards::COPENHAGEN).unwrap();
        assert_eq!(board.pieces.count_pieces_of_side(Attacker), 24);
        assert_eq!(board.pieces.count_pieces_of_side(Defender), 13);
    }
}
