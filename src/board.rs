use std::fmt::{Display, Formatter, Write};

use Side::{Attacker, Defender};

use crate::board::BoardError::{BadChar, BadLineLen};

#[derive(Debug)]
pub(crate) enum BoardError {
    BadStringLen(usize),
    BadLineLen(usize),
    BadChar(char)
}

#[derive(Debug)]
pub(crate) enum Side {
    Attacker,
    Defender(bool)
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub(crate) struct Tile {
    row: u8,
    col: u8
}

impl Tile {
    pub(crate) fn new(row: u8, col: u8) -> Self {
        Self { row, col }
    }

    fn to_mask(&self) -> u64 {
        return 1 << ((self.row * 8) + self.col)
    }

    fn to_byte(&self, side: u8) -> u8 {
        (self.row * side) + self.col
    }

    fn from_byte(byte: u8, side: u8) -> Self {
        Self {
            row: byte / side,
            col: byte % side
        }
    }

}

/// Store information on the current board state (ie, pieces).
///
/// Bitfields are used to minimise memory usage. A single u64 is used to record the positions of
/// all attacker pieces, and another u64 is used to record the positions of the defender bits.
/// On a 7x7 board, there are some unused bits: we use the first 7 bits of the least significant 7
/// bytes to record the pieces. The most significant byte of the `defenders` bitfield is used to
/// encode the position of the king (as a number from 0-48 inclusive).
#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub(crate) struct BoardState {
    attackers: u64,
    defenders: u64
}

impl Default for BoardState {
    fn default() -> Self {
        Self {
            attackers: 0,
            defenders: 0
        }
    }
}

impl BoardState {

    /// Get the tile on which the king is currently placed.
    pub(crate) fn get_king(&self, board_side: u8) -> Tile {
        let posn = self.defenders.to_be_bytes()[0];
        Tile::from_byte(posn, board_side)
    }

    /// Store the given location as the position of the king. **NB**: Does not set the relevant bit
    /// (or unset the bit corresponding to the king's previous location), which must be handled
    /// separately.
    pub(crate) fn set_king(&mut self, t: &Tile, board_side: u8) {
        let mut bytes = self.defenders.to_be_bytes();
        bytes[0] = t.to_byte(board_side);
        self.defenders = u64::from_be_bytes(bytes)
    }

    /// Check whether the given tile contains the king.
    pub(crate) fn is_king(&self, t: &Tile, board_side: u8) -> bool {
        &self.get_king(board_side) == t
    }

    /// Place a piece representing the given side at the given position by setting the relevant bit.
    pub(crate) fn place_piece(&mut self, t: &Tile, side: Side, board_side: u8) {
        let mask = t.to_mask();
        //println!("Setting {t:?} to {side:?}. Mask {mask}.");
        match side {
            Attacker => self.attackers |= mask,
            Defender(is_king) => {
                self.defenders |= mask;
                if is_king {
                    self.set_king(t, board_side)
                }
            }
        }
    }

    /// Clear a tile by unsetting the relevant bit.
    pub(crate) fn clear_tile(&mut self, t: &Tile) {
        let mask = !t.to_mask();
        self.attackers &= mask;
        self.defenders &= mask;
    }

    /// Return whether there is a piece (and which side the piece represents) at the given position.
    pub(crate) fn get_tile(&self, t: &Tile, board_side: u8) -> Option<Side> {
        let mask = t.to_mask();
        if (self.defenders & mask) > 0 {
            Some(Defender(self.is_king(t, board_side)))
        } else if (self.attackers & mask) > 0 {
            Some(Attacker)
        } else {
            None
        }
    }

    /// Move a piece from one position to another. This does not check whether a move is valid; it
    /// just unsets the bit at `from` and sets the bit at `to`.
    pub(crate) fn move_piece(&mut self, from: &Tile, to: &Tile, board_side: u8) {
        let maybe_side = self.get_tile(from, board_side);
        match maybe_side {
            Some(side) => {
                if let Defender(true) = side {
                    self.set_king(to, board_side)
                }
                self.place_piece(to, side, board_side);
            },
            None => {}
        }
        self.clear_tile(from)
    }
}

impl TryFrom<&str> for BoardState {
    type Error = BoardError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut b = BoardState::default();
        for (r, line) in value.lines().enumerate() {
            for (c, chr) in line.chars().enumerate() {
                match chr {
                    'A' => b.place_piece(&Tile::new(r as u8, c as u8), Attacker, 7),
                    'D' => b.place_piece(&Tile::new(r as u8, c as u8), Defender(false), 7),
                    'K' => b.place_piece(&Tile::new(r as u8, c as u8), Defender(true), 7),
                    '.' => {},
                    other => return Err(BadChar(other))
                }
            }
        }
        Ok(b)
    }
}

pub(crate) struct Board {
    state: BoardState,
    side_len: u8
}

impl Board {
    fn new(side: u8) -> Self {
        Self {
            state: BoardState::default(),
            side_len: side
        }
    }

    fn neighbors(&self, tile: Tile) -> Vec<Tile> {
        let mut neighbors: Vec<Tile> = vec![];
        let board_range = 0..self.side_len as i8;
        for r in -1..2i8 {
            for c in -1..2i8 {
                if r == 0 && c == 0 {
                    continue
                }
                let new_row = (tile.row as i8) + r;
                if !board_range.contains(&new_row) {
                    continue;
                }
                let new_col = (tile.col as i8) + c;
                if !board_range.contains(&new_col) {
                    continue;
                }
                neighbors.push(Tile::new(new_row as u8, new_col as u8));
            }
        }
        neighbors
    }

    pub(crate) fn tiles_between(&self, t1: &Tile, t2: &Tile) -> Vec<Tile> {
        let mut tiles: Vec<Tile> = vec![];
        if t1.row == t2.row {
            let col_range = if t1.col > t2.col {
                (t2.col+1)..t1.col
            } else {
                (t1.col+1)..t2.col
            };
            for col in col_range {
                tiles.push(Tile::new(t1.row, col))
            }
        } else if t1.col == t2.col {
            let row_range = if t1.row > t2.row {
                (t2.row + 1)..t1.row
            } else {
                (t1.row + 1)..t2.row
            };
            for row in row_range {
                tiles.push(Tile::new(row, t1.col))
            }
        }
        tiles
    }

    pub(crate) fn place_piece(&mut self, tile: &Tile, side: Side) {
        self.state.place_piece(tile, side, self.side_len);
    }

    pub(crate) fn move_piece(&mut self, from: &Tile, to: &Tile) {
        self.state.move_piece(from, to, self.side_len)
    }

    pub(crate) fn get_king(&self) -> Tile {
        self.state.get_king(self.side_len)
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                let t = Tile::new(r, c);
                let p = self.state.get_tile(&t, self.side_len);
                //println!("Checking {t:?}, piece is {p:?}");
                match p {
                    Some(Attacker) => f.write_char('A')?,
                    Some(Defender(false)) => f.write_char('D')?,
                    Some(Defender(true)) => f.write_char('K')?,
                    None => f.write_char('.')?,
                }
            }
            f.write_char('\n')?
        }
        Ok(())
    }
}

impl TryFrom<&str> for Board {
    type Error = BoardError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let s = value.trim();
        let mut side_len = 0u8;
        let mut state = BoardState::default();
        for (r, line) in s.lines().enumerate() {
            let line_len = line.len() as u8;
            if side_len == 0 {
                side_len = line_len
            } else if line_len != side_len {
                return Err(BadLineLen(line.len()))
            }
            for (c, chr) in line.chars().enumerate() {
                match chr {
                    'A' => state.place_piece(
                        &Tile::new(r as u8, c as u8),
                        Attacker,
                        side_len
                    ),
                    'D' => state.place_piece(
                        &Tile::new(r as u8, c as u8),
                        Defender(false),
                        side_len
                    ),
                    'K' => state.place_piece(
                        &Tile::new(r as u8, c as u8),
                        Defender(true),
                        side_len
                    ),
                    '.' => {},
                    other => return Err(BadChar(other))
                }
            }
        }
        Ok(Self {
            state,
            side_len
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use crate::board::{Board, Tile};
    use crate::board::Side::{Attacker, Defender};

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
        let start_str = "...A...\n...A...\n...D...\nAADKDAA\n...D...\n...A...\n...A...";
        let expected_str = "...AK..\n...A.A.\n...D...\nAAD.DAA\n.D.D...\n...A...\n...A...\n";
        let board_result = Board::try_from(start_str);
        assert!(board_result.is_ok());
        let mut board = board_result.unwrap();
        board.place_piece(&Tile::new(1, 5), Attacker);
        board.place_piece(&Tile::new(4, 1), Defender(false));
        board.move_piece(&Tile::new(3, 3), &Tile::new(0, 4));
        assert_eq!(format!("{board}"), expected_str);

        let n = board.neighbors(Tile::new(0, 0));
        check_tile_vec(n, vec![
            Tile::new(0, 1),
            Tile::new(1, 1),
            Tile::new(1, 0)
        ]);

        let n = board.neighbors(Tile::new(3, 2));
        check_tile_vec(n, vec![
            Tile::new(2, 1),
            Tile::new(2, 2),
            Tile::new(2, 3),
            Tile::new(3, 1),
            Tile::new(3, 3),
            Tile::new(4, 1),
            Tile::new(4, 2),
            Tile::new(4, 3)
        ]);

        let b = board.tiles_between(&Tile::new(2, 2), &Tile::new(2, 5));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(2, 4)
        ]);

        let b = board.tiles_between(&Tile::new(1, 3), &Tile::new(4, 3));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(3, 3)
        ]);

        let b = board.tiles_between(&Tile::new(1, 1), &Tile::new(3, 3));
        assert!(b.is_empty());

        let b = board.tiles_between(&Tile::new(1, 1), &Tile::new(1, 1));
        assert!(b.is_empty());

        let b = board.tiles_between(&Tile::new(1, 1), &Tile::new(1, 2));
        assert!(b.is_empty());
    }

}