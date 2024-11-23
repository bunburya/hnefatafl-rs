use crate::board_state::{BoardState, MediumBoardState, SmallBoardState};
use crate::error::{BoardError, ParseError};
use crate::pieces::Piece;
use crate::pieces::PieceType::King;
use crate::tiles::{Coords, Tile};
use crate::utils::UniqueStack;
use crate::PieceSet;
use std::collections::HashSet;
use std::fmt::{Display, Formatter, Write};
use std::str::FromStr;

const NEIGHBOR_OFFSETS: [[i8; 2]; 4] = [[-1, 0], [1, 0], [0, -1], [0, 1]];

/// A space on the board that is enclosed by pieces.
#[derive(Debug, Default)]
pub struct Enclosure {
    /// A set of all occupied enclosed tiles.
    pub occupied: HashSet<Tile>,
    /// A set of all unoccupied enclosed tiles.
    pub unoccupied: HashSet<Tile>,
    /// A set of tiles representing the boundary of the enclosure, ie, the enclosing pieces.
    pub boundary: HashSet<Tile>
}

impl Enclosure {
    pub fn contains(&self, tile: &Tile) -> bool {
        self.occupied.contains(tile) || self.unoccupied.contains(tile)
    }
}

/// Information about the game board, including its current state (as a struct implementing the
/// [`BoardState`] trait), its side length and the positions of its throne and corners.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Board<T: BoardState> {
    pub state: T,
    pub side_len: u8,
    pub throne: Tile,
    pub corners: [Tile; 4]
}

impl<T: BoardState> Board<T> {
    
    /// Create an empty board with the given side length.
    pub fn new(side_len: u8) -> Self {
        let corners = [
            Tile::new(0, 0),
            Tile::new(0, side_len - 1),
            Tile::new(side_len - 1, side_len - 1),
            Tile::new(side_len - 1, 0)
        ];
        Self {
            state: T::default(),
            side_len,
            throne: Tile::new(side_len / 2, side_len / 2),
            corners
        }
    }

    /// Create a new board with the given state.
    pub fn with_state(state: T, side_len: u8) -> Self {
        let mut board = Board::new(side_len);
        board.state = state;
        board
    }
    
    /// Check whether the given tile is on the board. Ideally should not be necessary as [`Tile`]s
    /// should always represent a position on the board and out-of-bounds [`Tile`]s should not be
    /// created.
    pub fn tile_in_bounds(&self, tile: Tile) -> bool {
        let r = 0..self.side_len;
        r.contains(&tile.row) && r.contains(&tile.col)
    }
    
    /// Convert an unbounded [`Coords`] to a [`Tile`] representing a position on the board, if
    /// possible. If the coords represents a position not on the board, return a
    /// [`BoardError::OutOfBounds`] error.
    pub fn coords_to_tile(&self, coords: Coords) -> Result<Tile, BoardError> {
        if self.coords_in_bounds(coords) {
            Ok(Tile::new(coords.row as u8, coords.col as u8))
        } else {
            Err(BoardError::OutOfBounds)
        }
    }
    
    /// Check whether the coords refer to a position on the board.
    pub fn coords_in_bounds(&self, coords: Coords) -> bool {
        let range = 0..(self.side_len as i8);
        range.contains(&coords.row) && range.contains(&coords.col)
    }

    /// Find a tile's neighbours (ie, the directly above, below and to either side of it).
    pub fn neighbors(&self, tile: Tile) -> Vec<Tile> {
        let row = tile.row;
        let col = tile.col;
        let signed_row = row as i8;
        let signed_col = col as i8;
        let mut neighbors: Vec<Tile> = vec![];
        for [r_off, c_off] in NEIGHBOR_OFFSETS.iter() {
            let coords = Coords { row: signed_row + r_off, col: signed_col + c_off };
            if let Ok(t) = self.coords_to_tile(coords) {
                neighbors.push(t);
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
    /// just unsets the bit at `from` and sets the bit at `to`. Returns the piece that was moved.
    /// Panics if there is no piece at `from`.
    pub fn move_piece(&mut self, from: Tile, to: Tile) -> Piece {
        let piece = self.get_piece(from).expect("No piece to move.");
        if piece.piece_type == King {
            self.state.set_king(to)
        }
        self.place_piece(to, piece);
        self.state.clear_tile(from);
        piece
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
    
    /// Check whether the tile at the given row and column is part of an enclosure.
    /// Used by [`Self::find_enclosure`]. 
    fn row_col_enclosed(
        &self,
        row: i8,
        col: i8,
        enclosed_piece_types: PieceSet,
        enclosing_piece_types: PieceSet,
        enclosure: &mut Enclosure,
    ) -> Option<bool> {
        let coords = Coords { row, col };
        if let Ok(tile) = self.coords_to_tile(coords) {
            if let Some(p) = self.get_piece(tile) {
                if enclosed_piece_types.contains(p) {
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
        } else {
            Some(false)
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
        tile: Tile,
        enclosed_pieces: PieceSet,
        enclosing_pieces: PieceSet,
        abort_on_edge: bool,
        abort_on_corner: bool
    ) -> Option<Enclosure> {
        let Coords { row, col } = Coords::from(tile);
        let mut enclosure = Enclosure::default();
        if !self.row_col_enclosed(row, col, enclosed_pieces, enclosing_pieces, &mut enclosure)? {
            return None
        }
        // In theory, I don't think we would need a UniqueStack if our flood fill logic worked
        // perfectly, but we were getting infinite loops on certain grids as it seemed certain
        // values were being pushed to the stack repeatedly. I couldn't debug it so this is an easy
        // option to address it.
        let mut stack: UniqueStack<(i8, i8, i8, i8)> = UniqueStack::default();
        stack.push((col, col, row, 1));
        stack.push((col, col, row - 1, -1));
        while let Some((mut c1, c2, r, dr)) = stack.pop() {
            let mut c = c1;
            if self.row_col_enclosed(r, c, enclosed_pieces, enclosing_pieces, &mut enclosure)? {
                while self.row_col_enclosed(
                    r, 
                    c - 1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure
                )? {
                    let t= Tile::new(r as u8, (c - 1) as u8);
                    if (abort_on_edge && self.tile_at_edge(t))
                        || (abort_on_corner && self.corners.contains(&t)) {
                        return None
                    }
                    c -= 1
                }
                
                if c < c1 {
                    stack.push((c, c1 - 1, r - dr, -dr))
                }
            } 
            while c1 <= c2 {
                while self.row_col_enclosed(
                    r,
                    c1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure
                )? {
                    let t= Tile::new(r as u8, c1 as u8);
                    if abort_on_edge && self.tile_at_edge(t) {
                        return None
                    }
                    if abort_on_corner && self.corners.contains(&t) {
                        return None
                    }
                    c1 += 1
                }
                
                if c1 > c {
                    stack.push((c, c1 - 1, r + dr, dr));
                }
                if c1 - 1 > c2 {
                    stack.push((c2 + 1, c1 - 1, r - dr, -dr))
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

    /// Return an iterator over all tiles on the board.
    pub fn iter_tiles(&self) -> TileIterator {
        TileIterator::new(self.side_len)
    }
    
}

impl<T: BoardState> Display for Board<T> {
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

impl<T: BoardState> FromStr for Board<T> {
    type Err = ParseError;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let (state, side_len) = T::from_str_with_side_len(value)?;
        Ok(Self::with_state(state, side_len))
    }
}

pub struct TileIterator {
    side_len: u8,
    current_row: u8,
    current_col: u8
}

impl TileIterator {
    fn new(side_len: u8) -> Self {
        Self {
            side_len,
            current_row: 0,
            current_col: 0
        }
    }
}

impl Iterator for TileIterator {
    type Item = Tile;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_row >= self.side_len {
            return None
        }
        let tile = Tile::new(self.current_row, self.current_col);
        if self.current_col >= self.side_len - 1 {
            self.current_row += 1;
            self.current_col = 0;
        } else {
            self.current_col += 1;
        }
        Some(tile)
    }
}

/// A [`Board`] suitable for board sizes up 7x7. 
pub type SmallBoard = Board<SmallBoardState>;

/// A [`Board`] suitable for board sizes up to 11x11.
pub type MediumBoard = Board<MediumBoardState>;



#[cfg(test)]
mod tests {
    use crate::board::{Board, BoardState, SmallBoard, Tile};
    use crate::board_state::SmallBoardState;
    use crate::pieces::Piece;
    use crate::pieces::PieceType::Soldier;
    use crate::PieceSet;
    use crate::PieceType::King;
    use crate::Side::{Attacker, Defender};
    use std::collections::HashSet;
    use std::str::FromStr;
    use crate::preset::boards;

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
        let mut board: Board<SmallBoardState> = board_result.unwrap();
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
        let encl_edge_2 = [
            ".t..t..",
            ".t.K.t.",
            "..tttt.",
            ".......",
            ".......",
            ".......",
            ".......",
        ].join("\n");
        let board: SmallBoard = Board::from_str(full_enclosure.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            Tile::new(1, 3),
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
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            true,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            Tile::new(1, 3),
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
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            Tile::new(1, 5),
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
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            true,
            true
        );
        assert!(encl_res.is_none());
        let encl_res = board.find_enclosure(
            Tile::new(1, 3),
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

        let board: SmallBoard = Board::from_str(encl_edge_2.as_str()).unwrap();
        let encl_res = board.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            false,
            false
        );
        assert!(encl_res.is_some());
    }

    #[test]
    fn test_iter_tiles() {
        let board: SmallBoard = Board::from_str(boards::BRANDUBH).unwrap();
        let mut iter = board.iter_tiles();
        for r in 0..7 {
            for c in 0..7 {
                assert_eq!(iter.next(), Some(Tile::new(r, c)));
            }
        }
        assert_eq!(iter.next(), None);
    }

}
