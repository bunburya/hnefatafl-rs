use crate::tiles::Tile;

/// Iterator over all tiles on a board.
pub struct TileIterator {
    side_len: u8,
    current_row: u8,
    current_col: u8
}

impl TileIterator {
    pub(crate) fn new(side_len: u8) -> Self {
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
