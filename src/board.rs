use std::fmt::{Display, Formatter, Write};
use std::ops::RangeBounds;
use std::str::FromStr;
use Side::{Attacker, Defender};

use crate::error::ParseError;
use crate::error::ParseError::BadLineLen;
use crate::pieces::{Piece, Side};
use crate::pieces::PieceType::{King, Soldier};
use crate::tiles::{Move, Tile};
use crate::traits::BitField;

/// Store information on the current board state (ie, pieces). This struct currently handles only a
/// simple board, ie, a king and soldiers on a 7x7 board.
///
/// Bitfields are used to minimise memory usage. A single u64 is used to record the positions of
/// all attacker pieces, and another u64 is used to record the positions of the defender bits.
/// On a 7x7 board, there are some unused bits: we use the first 7 bits of the least significant 7
/// bytes to record the pieces. The most significant byte of the `defenders` bitfield is used to
/// encode the position of the king.
///
/// Currently only basic getting and setting is implemented at the bitfield level. More complex game
/// logic (like checking move validity, etc) is implemented elsewhere and uses [Tile] structs. If
/// performance was an issue we could look at moving some of that logic to the bitfield level.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default)]
pub(crate) struct SimpleBoardState<T: BitField> {
    attackers: T,
    defenders: T
}

impl<T: BitField> SimpleBoardState<T> {

    /// Get the tile on which the king is currently placed.
    pub(crate) fn get_king(&self) -> Tile {
        let row_bits = self.defenders.to_be_bytes().as_ref()[0] & 0b1111_0000;
        let col_bits = self.attackers.to_be_bytes().as_ref()[0] >> 4;
        Tile::from_byte(row_bits | col_bits)
    }

    /// Store the given location as the position of the king. **NB**: Does not set the relevant bit
    /// (or unset the bit corresponding to the king's previous location), which must be handled
    /// separately.
    pub(crate) fn set_king(&mut self, t: Tile) {
        let mut def_bytes = self.defenders.to_be_bytes();
        let def_bytes_slice = def_bytes.as_mut();
        def_bytes_slice[0] &= 0b0000_1111;  // Unset 4 most significant bits
        def_bytes_slice[0] |= t.row_bits();  // Set 4 most significant bits to row
        self.defenders = T::from_be_bytes_slice(def_bytes_slice);
        let mut att_bytes = self.attackers.to_be_bytes();
        let att_bytes_slice = att_bytes.as_mut();
        att_bytes_slice[0] &= 0b0000_1111;
        att_bytes_slice[0] |= t.col_bits() << 4;
        self.attackers = T::from_be_bytes_slice(att_bytes_slice);
    }

    /// Check whether the given tile contains the king.
    pub(crate) fn is_king(&self, t: Tile) -> bool {
        self.get_king() == t
    }

    /// Place a piece representing the given side at the given position by setting the relevant bit.
    pub(crate) fn place_piece(&mut self, t: Tile, piece: Piece) {
        let mask = T::tile_mask(t);
        //println!("Setting {t:?} to {side:?}. Mask {mask}.");
        match piece.side {
            Attacker => self.attackers |= mask,
            Defender => self.defenders |= mask
        }
        if piece.piece_type == King {
            self.set_king(t)
        }
    }

    /// Clear a tile by unsetting the relevant bit.
    pub(crate) fn clear_tile(&mut self, t: Tile) {
        let mask = !T::tile_mask(t);
        self.attackers &= mask;
        self.defenders &= mask;
    }

    pub(crate) fn get_piece(&self, t: Tile) -> Option<Piece> {
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
    pub(crate) fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers;
        let mask = T::tile_mask(t);
        (all_pieces & mask) > 0.into()
    }

    /// Execute the given move.
    pub(crate) fn do_move(&mut self, m: Move) {
        self.move_piece(m.from, m.to())
    }

    /// Move a piece from one position to another. This does not check whether a move is valid; it
    /// just unsets the bit at `from` and sets the bit at `to`.
    pub(crate) fn move_piece(&mut self, from: Tile, to: Tile) {
        let maybe_piece = self.get_piece(from);
        if let Some(piece) = maybe_piece {
            if piece.piece_type == King {
                self.set_king(to)
            }
            self.place_piece(to, piece);
            self.clear_tile(from)
        }
    }
    
    pub(crate) fn count_pieces(&self, side: Side) -> u8 {
        match side {
            Attacker => self.attackers,
            Defender => self.defenders
        }.count_ones() as u8
    }
}

pub(crate) struct Board<T: BitField> {
    pub(crate) state: SimpleBoardState<T>,
    side_len: u8,
    pub(crate) throne: Tile,
    pub(crate) corners: [Tile; 4]
}

impl<T: BitField> Board<T> {
    
    fn new(side_len: u8) -> Self {
        let corners = [
            Tile::new(0, 0),
            Tile::new(0, side_len - 1),
            Tile::new(side_len - 1, side_len - 1),
            Tile::new(side_len - 1, 0)
        ];
        Self {
            state: SimpleBoardState::default(),
            side_len,
            throne: Tile::new(side_len / 2, side_len / 2),
            corners
        }
    }

    fn with_state(state: SimpleBoardState<T>, side_len: u8) -> Self {
        let mut board = Board::new(side_len);
        board.state = state;
        board
    }


    pub(crate) fn tile_in_bounds(&self, tile: Tile) -> bool {
        let r = 0..self.side_len;
        r.contains(&tile.row()) && r.contains(&tile.col())
    }

    pub(crate) fn neighbors(&self, tile: Tile) -> Vec<Tile> {
        let row = tile.row();
        let col = tile.col();
        let mut neighbors: Vec<Tile> = vec![
            Tile::new(row+1, col),
            Tile::new(row, col+1)
        ];
        if row > 0 {
            neighbors.push(Tile::new(row-1, col))
        }
        if col > 0 {
            neighbors.push(Tile::new(row, col-1))
        }
        neighbors
    }

    pub(crate) fn tiles_between(&self, t1: Tile, t2: Tile) -> Vec<Tile> {
        let mut tiles: Vec<Tile> = vec![];
        let (r1, c1, r2, c2) = (t1.row(), t1.col(), t2.row(), t2.col());
        if r1 == r2 {
            let col_range = if c1 > c2 {
                (c2+1)..c1
            } else {
                (c1+1)..c2
            };
            for col in col_range {
                tiles.push(Tile::new(r1, col))
            }
        } else if c1 == c2 {
            let row_range = if r1 > r2 {
                (r2 + 1)..r1
            } else {
                (r1 + 1)..r2
            };
            for row in row_range {
                tiles.push(Tile::new(row, c1))
            }
        }
        tiles
    }

    /// Check whether the given tile is occupied by any piece.
    pub(crate) fn tile_occupied(&self, tile: Tile) -> bool {
        self.state.tile_occupied(tile)
    }
    
    /// Check whether the given tile is at the edge of the board (including at a corner).
    pub(crate) fn tile_at_edge(&self, tile: Tile) -> bool {
        tile.row() == 0 
            || tile.row() == self.side_len - 1 
            || tile.col() == 0 
            || tile.col() == self.side_len - 1
    }

    pub(crate) fn place_piece(&mut self, tile: Tile, piece: Piece) {
        self.state.place_piece(tile, piece);
    }

    pub(crate) fn do_move(&mut self, m: Move) {
        self.state.do_move(m)
    }

    pub(crate) fn get_piece(&self, tile: Tile) -> Option<Piece> {
        self.state.get_piece(tile)
    }

    pub(crate) fn get_king(&self) -> Tile {
        self.state.get_king()
    }

    pub(crate) fn is_king(&self, tile: Tile) -> bool {
        self.state.is_king(tile)
    }

}

impl<T: BitField> Display for Board<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                let t = Tile::new(r, c);
                let p = self.state.get_piece(t);
                //println!("Checking {t:?}, piece is {p:?}");
                match p {
                    Some(piece) => f.write_char(piece.into())?,
                    None => f.write_char('.')?,
                }
            }
            f.write_char('\n')?
        }
        Ok(())
    }
}

impl<T: BitField> FromStr for Board<T> {
    type Err = ParseError;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let s = value.trim();
        let mut side_len = 0u8;
        let mut state = SimpleBoardState::default();
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
        Ok(Self::with_state(state, side_len))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::str::FromStr;
    use crate::board::{Board, Tile};
    use crate::pieces::Piece;
    use crate::pieces::PieceType::Soldier;

    /// Assert that the given vector does not contain duplicates, and contains the same items as
    /// a comparison vector (ignoring order).
    fn check_tile_vec(actual: Vec<Tile>, expected: Vec<Tile>) {
        let actual_set: HashSet<Tile> = actual.iter().copied().collect();
        assert_eq!(actual_set.len(), actual.len(), "Vec contains duplicates");
        let expected_set: HashSet<Tile> = expected.iter().copied().collect();
        assert_eq!(actual_set, expected_set);
    }

    #[test]
    fn test_board() {
        let start_str = "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t...";
        let expected_str = "...tK..\n...t.t.\n...T...\nttT.Ttt\n.T.T...\n...t...\n...t...\n";
        let board_result = Board::from_str(start_str);
        assert!(board_result.is_ok());
        let mut board: Board<u64> = board_result.unwrap();
        assert_eq!(board.get_king(), Tile::new(3, 3));
        board.place_piece(Tile::new(1, 5), Piece::attacker(Soldier));
        board.place_piece(Tile::new(4, 1), Piece::defender(Soldier));
        board.state.move_piece(Tile::new(3, 3), Tile::new(0, 4));
        assert_eq!(board.get_king(), Tile::new(0, 4));
        assert_eq!(format!("{board}"), expected_str);

        let n = board.neighbors(Tile::new(0, 0));
        check_tile_vec(n, vec![
            Tile::new(0, 1),
            Tile::new(1, 0)
        ]);

        let n = board.neighbors(Tile::new(3, 2));
        check_tile_vec(n, vec![
            Tile::new(2, 2),
            Tile::new(3, 1),
            Tile::new(3, 3),
            Tile::new(4, 2),
        ]);

        let b = board.tiles_between(Tile::new(2, 2), Tile::new(2, 5));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(2, 4)
        ]);

        let b = board.tiles_between(Tile::new(1, 3), Tile::new(4, 3));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(3, 3)
        ]);

        let b = board.tiles_between(Tile::new(1, 1), Tile::new(3, 3));
        assert!(b.is_empty());

        let b = board.tiles_between(Tile::new(1, 1), Tile::new(1, 1));
        assert!(b.is_empty());

        let b = board.tiles_between(Tile::new(1, 1), Tile::new(1, 2));
        assert!(b.is_empty());

        let occupied = [
            Tile::new(0, 3),
            Tile::new(2, 3),
            Tile::new(0, 4)
        ];
        for t in occupied {
            println!("{t:?}");
            assert!(board.state.tile_occupied(t));
        }
        let empty = [
            Tile::new(3, 3),
            Tile::new(5, 4),
            Tile::new(1, 1)
        ];
        for t in empty {
            println!("{t:?}");
            assert!(!board.state.tile_occupied(t));
        }
    }

}