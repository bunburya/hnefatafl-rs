use crate::board::state::BoardState;
use crate::error::BoardError;
use crate::tiles::{Coords, Tile, TileIterator};

const NEIGHBOR_OFFSETS: [[i8; 2]; 4] = [[-1, 0], [1, 0], [0, -1], [0, 1]];

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct SpecialTiles {
    pub throne: Tile,
    pub corners: [Tile; 4]
}

impl From<u8> for SpecialTiles {
    fn from(board_len: u8) -> Self {
        let corners = [
            Tile::new(0, 0),
            Tile::new(0, board_len - 1),
            Tile::new(board_len - 1, board_len - 1),
            Tile::new(board_len - 1, 0)
        ];
        let throne = Tile::new(board_len / 2, board_len / 2);
        Self { corners, throne }
    }
}

/// This struct contains information about the geometry of the board, such as its size and the
/// positions of various special tiles. It does not contain information about piece placement or any
/// other state that would be expected to change over the course of a game.
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct BoardGeometry {
    pub side_len: u8,
    pub special_tiles: SpecialTiles
}

impl BoardGeometry {

    /// Create an empty board with the given side length.
    pub fn new(side_len: u8) -> Self {
        Self { side_len, special_tiles: SpecialTiles::from(side_len) }
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

    /// Check whether the given tile is at the edge of the board (including at a corner).
    pub fn tile_at_edge(&self, tile: Tile) -> bool {
        tile.row == 0
            || tile.row == self.side_len - 1
            || tile.col == 0
            || tile.col == self.side_len - 1
    }

    /// Check whether the given tile is surrounded on all sides by pieces (friend or foe).
    pub fn tile_surrounded(&self, tile: Tile, state: impl BoardState) -> bool {
        self.neighbors(tile).iter().all(|t| state.tile_occupied(*t))
    }
    
    /// Return an iterator over all tiles on the board.
    pub fn iter_tiles(&self) -> TileIterator {
        TileIterator::new(self.side_len)
    }

    /// Generate the FEN string describing the current board state
    pub fn to_fen(&self, state: &impl BoardState) -> String {
        let mut s = String::new();
        for row in 0..self.side_len {
            let mut n_empty = 0;
            for col in 0..self.side_len {
                let t = Tile::new(row, col);
                if let Some(piece) = state.get_piece(t) {
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
}

#[cfg(test)]
mod tests {
    use crate::board::geometry::BoardGeometry;
    use crate::tiles::Tile;
    use crate::utils::check_tile_vec;

    #[test]
    fn test_neighbors() {
        let geo = BoardGeometry::new(7);
        let n = geo.neighbors(Tile::new(0, 0));
        check_tile_vec(n, vec![
            Tile::new(0, 1),
            Tile::new(1, 0)
        ]);

        let n = geo.neighbors(Tile::new(3, 2));
        check_tile_vec(n, vec![
            Tile::new(2, 2),
            Tile::new(3, 1),
            Tile::new(3, 3),
            Tile::new(4, 2),
        ]);

        let b = geo.tiles_between(Tile::new(2, 2), Tile::new(2, 5));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(2, 4)
        ]);

        let b = geo.tiles_between(Tile::new(1, 3), Tile::new(4, 3));
        check_tile_vec(b, vec![
            Tile::new(2, 3),
            Tile::new(3, 3)
        ]);

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(3, 3));
        assert!(b.is_empty());

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(1, 1));
        assert!(b.is_empty());

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(1, 2));
        assert!(b.is_empty());
    }
}