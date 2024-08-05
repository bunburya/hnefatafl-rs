/// The location of a single tile on the board, ie, row and column. This struct is only a reference
/// to a location on the board, and does not contain any other information such as piece placement,
/// etc.
///
/// It is implemented as a single byte, where the most significant four bits describe the row
/// and the least significant four bits describe the column. It is therefore appropriate for use
/// with square boards up to 16x16.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub(crate) struct Tile {
    pub(crate) byte: u8
}

impl Tile {
    pub(crate) fn new(row: u8, col: u8) -> Self {
        Self { byte: (row << 4) | (col & 0x0F) }
    }

    pub(crate) fn from_byte(byte: u8) -> Self {
        Self { byte }
    }

    pub(crate) fn row(&self) -> u8 {
        self.byte >> 4
    }

    pub(crate) fn col(&self) -> u8 {
        self.byte & 0x0F
    }

    pub(crate) fn to_mask(&self) -> u64 {
        return 1 << ((self.row() * 8) + self.col())
    }

    pub(crate) fn distance_to(&self, other: Tile) -> u8 {
        if self.row() == other.row() {
            self.col().abs_diff(other.col())
        } else {
            self.row().abs_diff(other.row())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tiles::Tile;

    #[test]
    fn test_tile_creation() {
        for r in 0..16 {
            for c in 0..16 {
                let t = Tile::new(r, c);
                assert_eq!(t.row(), r);
                assert_eq!(t.col(), c);
            }
        }
    }

    #[test]
    fn test_distance() {
        assert_eq!(Tile::new(2, 1).distance_to(Tile::new(2, 4)), 3);
        assert_eq!(Tile::new(2, 5).distance_to(Tile::new(2, 0)), 5);
        assert_eq!(Tile::new(6, 1).distance_to(Tile::new(2, 1)), 4);
        assert_eq!(Tile::new(2, 3).distance_to(Tile::new(3, 3)), 1);
    }
}