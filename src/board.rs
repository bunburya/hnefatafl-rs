use crate::error::ParseError;
use crate::error::ParseError::BadLineLen;
use crate::pieces::PieceType::{King, Soldier};
use crate::pieces::{Piece, Side};
use crate::tiles::Tile;
use crate::traits::BitField;
use crate::PieceSet;
use std::collections::HashSet;
use std::fmt::{Display, Formatter, Write};
use std::ops::RangeBounds;
use std::str::FromStr;
use Side::{Attacker, Defender};

const NEIGHBOR_OFFSETS: [[i8; 2]; 4] = [[-1, 0], [1, 0], [0, -1], [0, 1]];

/// A space on the board that is enclosed by pieces.
pub struct Enclosure {
    /// A set of all occupied enclosed tiles.
    occupied: HashSet<Tile>,
    /// A set of all unoccupied enclosed tiles.
    unoccupied: HashSet<Tile>,
    /// A set of tiles representing the boundary of the enclosure, ie, the enclosing pieces.
    boundary: HashSet<Tile>
}

impl Default for Enclosure {
    fn default() -> Self {
        Enclosure {
            occupied: HashSet::new(),
            unoccupied: HashSet::new(),
            boundary: HashSet::new()
        }
    }
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
pub struct SimpleBoardState<T: BitField> {
    attackers: T,
    defenders: T
}

impl<T: BitField> SimpleBoardState<T> {

    /// Get the tile on which the king is currently placed.
    pub fn get_king(&self) -> Tile {
        let row = (self.defenders.to_be_bytes().as_ref()[0] & 0b1111_0000) >> 4;
        let col = (self.attackers.to_be_bytes().as_ref()[0] & 0b1111_0000) >> 4;
        Tile::new(row, col)
    }

    /// Store the given location as the position of the king. **NB**: Does not set the relevant bit
    /// (or unset the bit corresponding to the king's previous location), which must be handled
    /// separately.
    pub fn set_king(&mut self, t: Tile) {
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

    /// Check whether the given tile contains the king.
    pub fn is_king(&self, t: Tile) -> bool {
        self.get_king() == t
    }

    /// Place a piece representing the given side at the given position by setting the relevant bit.
    pub fn place_piece(&mut self, t: Tile, piece: Piece) {
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
    pub fn clear_tile(&mut self, t: Tile) {
        let mask = !T::tile_mask(t);
        self.attackers &= mask;
        self.defenders &= mask;
    }

    pub fn get_piece(&self, t: Tile) -> Option<Piece> {
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
    pub fn tile_occupied(&self, t: Tile) -> bool {
        let all_pieces = self.defenders | self.attackers;
        let mask = T::tile_mask(t);
        (all_pieces & mask) > 0.into()
    }
    
    /// Count the number of pieces of the given side left on the board. Includes the king for
    /// defenders.
    pub fn count_pieces(&self, side: Side) -> u8 {
        match side {
            Attacker => self.attackers,
            Defender => self.defenders
        }.count_ones() as u8
    }
}

pub struct Board<T: BitField> {
    pub state: SimpleBoardState<T>,
    pub side_len: u8,
    pub throne: Tile,
    pub corners: [Tile; 4]
}

impl<T: BitField> Board<T> {
    
    pub fn new(side_len: u8) -> Self {
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

    pub fn with_state(state: SimpleBoardState<T>, side_len: u8) -> Self {
        let mut board = Board::new(side_len);
        board.state = state;
        board
    }


    pub fn tile_in_bounds(&self, tile: Tile) -> bool {
        let r = 0..self.side_len;
        r.contains(&tile.row) && r.contains(&tile.col)
    }
    
    /// Check whether the given row and column refer to a tile on the board. `row` and `col` are
    /// signed integers to allow for negative values (which will always be out of bounds).
    pub fn row_col_in_bounds(&self, row: i8, col: i8) -> bool {
        row >= 0 && col >= 0 && (row as u8) < self.side_len && (col as u8) < self.side_len
    }

    /// Find a tile's neighbours (ie, the directly above, below and to either side of it).
    pub fn neighbors(&self, tile: Tile) -> Vec<Tile> {
        let row = tile.row;
        let col = tile.col;
        let signed_row = row as i8;
        let signed_col = col as i8;
        let mut neighbors: Vec<Tile> = vec![];
        for [r_off, c_off] in NEIGHBOR_OFFSETS.iter() {
            let r = signed_row + r_off;
            let c = signed_col + c_off;
            if self.row_col_in_bounds(r, c) {
                neighbors.push(Tile::new(r as u8, c as u8));
            }
        } 
        neighbors
    }

    /// Get all the tiles between the given two tiles. If given tiles do not share a row or column,
    /// an empty vector is returned.
    pub fn tiles_between(&self, t1: Tile, t2: Tile) -> Vec<Tile> {
        let mut tiles: Vec<Tile> = vec![];
        let (r1, c1, r2, c2) = (t1.row, t1.col, t2.row, t2.col);
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
    pub fn tile_occupied(&self, tile: Tile) -> bool {
        self.state.tile_occupied(tile)
    }
    
    /// Check whether the tile at the given row and column is occupied by any piece.
    pub fn row_col_occupied(&self, row: u8, col: u8) -> bool {
        self.tile_occupied(Tile::new(row, col))
    }
    
    /// Check whether the given tile is at the edge of the board (including at a corner).
    pub fn tile_at_edge(&self, tile: Tile) -> bool {
        tile.row == 0
            || tile.row == self.side_len - 1
            || tile.col == 0
            || tile.col == self.side_len - 1
    }
    
    /// Check whether the given tile is surrounded on all sides by pieces (friend or foe).
    pub fn tile_surrounded(&self, tile: Tile) -> bool {
        self.neighbors(tile).iter().all(|t| self.tile_occupied(*t))
    }

    /// Place the given piece on the given tile.
    pub fn place_piece(&mut self, tile: Tile, piece: Piece) {
        self.state.place_piece(tile, piece);
    }
    
    /// Remove the piece at the given tile.
    pub fn remove_piece(&mut self, tile: Tile) {
        self.state.clear_tile(tile)
    }

    /// Move a piece from one position to another. This does not check whether a move is valid; it
    /// just unsets the bit at `from` and sets the bit at `to`.
    pub fn move_piece(&mut self, from: Tile, to: Tile) {
        let maybe_piece = self.get_piece(from);
        if let Some(piece) = maybe_piece {
            if piece.piece_type == King {
                self.state.set_king(to)
            }
            self.place_piece(to, piece);
            self.state.clear_tile(from)
        }
    }

    /// Get the piece (if any) that occupies the given tile.
    pub fn get_piece(&self, tile: Tile) -> Option<Piece> {
        self.state.get_piece(tile)
    }

    /// Get the current position of the king.
    pub fn get_king(&self) -> Tile {
        self.state.get_king()
    }

    /// Check whether the given tile is occupied by the king.
    pub fn is_king(&self, tile: Tile) -> bool {
        self.state.is_king(tile)
    }
    
    fn row_col_enclosed(
        &self,
        row: u8,
        col: u8,
        enclosed_piece_types: PieceSet,
        enclosing_piece_types: PieceSet,
        enclosure: &mut Enclosure,
    ) -> Option<bool> {
        println!("checking ({row}, {col})");
        let tile = Tile::new(row, col);
        if !self.tile_in_bounds(tile) {
            println!("found OOB tile");
            Some(false)
        } else if let Some(p) = self.get_piece(tile) {
            return if enclosed_piece_types.contains(p) {
                enclosure.occupied.insert(tile);
                Some(true)
            } else if enclosing_piece_types.contains(p) {
                enclosure.boundary.insert(tile);
                Some(false)
            } else {
                // Tile occupied by piece that can neither enclose nor be enclosed, so no enclosure
                // is present.
                None
            }
        } else {
            // Empty tiles can be enclosed.
            enclosure.unoccupied.insert(tile);
            Some(true)
        }
    }
    
    /// Find an area surrounding the tile at `start`, containing only the pieces in
    /// `enclosed_pieces` and which is fully surrounded by pieces other than those in
    /// `enclosed_pieces`. If `abort_on_edge` or `abort_on_corner` is `true` and an edge or corner,
    /// respectively, is encountered, no enclosure is found.
    /// 
    /// Uses a flood fill algorithm based on https://en.wikipedia.org/wiki/Flood_fill#Span_filling
    pub fn find_enclosure(
        &self,
        row: u8,
        col: u8,
        enclosed_pieces: PieceSet,
        enclosing_pieces: PieceSet,
        abort_on_edge: bool,
        abort_on_corner: bool
    ) -> Option<Enclosure> {
        let mut enclosure = Enclosure::default();
        if !self.row_col_enclosed(row, col, enclosed_pieces, enclosing_pieces, &mut enclosure)? {
            return None
        }
        let mut stack: Vec<(u8, u8, u8, i8)> = vec![];
        stack.push((col, col, row, 1));
        if row > 0 {
            stack.push((col, col, row - 1, -1));
        }
        while let Some((mut c1, c2, r, dr)) = stack.pop() {
            let mut c = c1;
            if self.row_col_enclosed(r, c, enclosed_pieces, enclosing_pieces, &mut enclosure)? {
                if c == 0 {
                    break
                }
                while (c > 0) && self.row_col_enclosed(
                    r, 
                    c - 1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure
                )? {
                    let t= Tile::new(r, c - 1);
                    if (abort_on_edge && self.tile_at_edge(t)) 
                        || (abort_on_corner && self.corners.contains(&t)) {
                        return None
                    }
                    c -= 1
                }
                
                if (c < c1) && (c1 > 0) && ((r as i8) >= dr) 
                    && ((r as i8) - dr < self.side_len as i8) {
                    stack.push((c, c1 - 1, ((r as i8) - dr) as u8, -dr))
                }
            } 
            while c1 <= c2 {
                if c1 >= self.side_len {
                    break
                }
                while self.row_col_enclosed(
                    r,
                    c1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure
                )? {
                    let t= Tile::new(r, c1);
                    if abort_on_edge && self.tile_at_edge(t) {
                        return None
                    }
                    if abort_on_corner && self.corners.contains(&t) {
                        return None
                    }
                    c1 += 1
                }
                
                if (c1 > c) && (c1 > 0) && ((r as i8) > -dr) {
                    stack.push((c, c1 - 1, ((r as i8) + dr) as u8, dr));
                }
                if (c1 > 0) && (c2 + 1 < self.side_len) && (c1 - 1 > c2) {
                    stack.push((c2 + 1, c1 - 1, ((r as i8) - dr) as u8, -dr))
                }
                
                c1 += 1;
                
                while (c1 < c2) && !self.row_col_enclosed(
                    r,
                    c1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure
                )? {
                    c1 += 1
                }
                c = c1
                
            }
            
        }
        Some(enclosure)
    }
    
}

impl<T: BitField> Display for Board<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                let t = Tile::new(r, c);
                let p = self.state.get_piece(t);
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

/// A [`Board`] suitable for board sizes up 7x7. 
pub type SmallBoard = Board<u64>;

/// A [`Board`] suitable for board sizes up to 11x11.
pub type MediumBoard = Board<u128>;

#[cfg(test)]
mod tests {
    use crate::board::{Board, SmallBoard, Tile};
    use crate::pieces::Piece;
    use crate::pieces::PieceType::Soldier;
    use crate::PieceType::King;
    use crate::Side::{Attacker, Defender};
    use crate::PieceSet;
    use std::collections::HashSet;
    use std::str::FromStr;

    /// Assert that the given vector does not contain duplicates, and contains the same items as
    /// a comparison vector (ignoring order).
    fn check_tile_vec(actual: Vec<Tile>, expected: Vec<Tile>) {
        let actual_set: HashSet<Tile> = actual.iter().copied().collect();
        assert_eq!(actual_set.len(), actual.len(), "Vec contains duplicates");
        let mut actual_sorted = actual.clone();
        actual_sorted.sort();
        let mut expected_sorted = expected.clone();
        expected_sorted.sort();
        assert_eq!(actual_sorted, expected_sorted);
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
        board.move_piece(Tile::new(3, 3), Tile::new(0, 4));
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
            assert!(board.state.tile_occupied(t));
        }
        let empty = [
            Tile::new(3, 3),
            Tile::new(5, 4),
            Tile::new(1, 1)
        ];
        for t in empty {
            assert!(!board.state.tile_occupied(t));
        }
    }
    
    #[test]
    fn test_enclosures() {
        let full_enclosure = [
            "..ttt..",
            ".t.K.t.",
            "..tttt.",
            ".......",
            ".......",
            ".......",
            ".......",
        ].join("\n");
        let encl_with_edge = [
            "..t.t..",
            ".t.K.t.",
            "..tttt.",
            ".......",
            ".......",
            ".......",
            ".......",
        ].join("\n");
        let encl_with_corner = [
            ".....t.",
            "....tK.",
            "....ttt",
            ".......",
            ".......",
            ".......",
            ".......",
        ].join("\n");
        let encl_with_soldier = [
            "..ttt..",
            ".t.KTt.",
            "..tttt.",
            ".......",
            ".......",
            ".......",
            ".......",
        ].join("\n");
        let board: SmallBoard = Board::from_str(full_enclosure.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            1, 
            3,
            PieceSet::from(King),
            PieceSet::from(Soldier),
            true, 
            true
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 3)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(1, 2), Tile::new(1, 4)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 3), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );

        let board: SmallBoard = Board::from_str(encl_with_edge.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            1,
            3,
            PieceSet::from(King),
            PieceSet::from(Soldier),
            true,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            1,
            3,
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            true
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 3)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(0, 3), Tile::new(1, 2), Tile::new(1, 4)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );
        
        let board: SmallBoard = Board::from_str(encl_with_corner.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            1,
            3,
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            1,
            5,
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            false
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 5)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(0, 6), Tile::new(1, 6)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 5), Tile::new(1, 4),
                Tile::new(2, 5), Tile::new(2, 6)
            ]
        );

        let board: SmallBoard = Board::from_str(encl_with_soldier.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            1,
            3,
            PieceSet::from(King),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            true,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            1,
            3,
            PieceSet::from(vec![Piece::new(King, Defender), Piece::new(Soldier, Defender)]),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            true,
            true
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(
            encl.occupied.into_iter().collect(),
            vec![Tile::new(1, 3), Tile::new(1, 4)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(1, 2)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 3), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );
    }

}
