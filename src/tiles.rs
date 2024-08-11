use crate::error::MoveError;
use crate::error::MoveError::DisjointTiles;
use crate::tiles::Plane::{Horizontal, Vertical};
use std::fmt::{Debug, Formatter};

/// The location of a single tile on the board, ie, row and column. This struct is only a reference
/// to a location on the board, and does not contain any other information such as piece placement,
/// etc.
///
/// It is implemented as a single byte, where the most significant four bits describe the row
/// and the least significant four bits describe the column. It is therefore appropriate for use
/// with square boards up to 16x16.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Tile(pub(crate) u8);

impl Tile {
    pub(crate) fn new(row: u8, col: u8) -> Self {
        Self { 0: (row << 4) | (col & 0x0F) }
    }

    pub(crate) fn from_byte(byte: u8) -> Self {
        Self { 0: byte }
    }

    pub(crate) fn row(&self) -> u8 {
        self.0 >> 4
    }

    pub(crate) fn col(&self) -> u8 {
        self.0 & 0x0F
    }

    pub(crate) fn to_mask(&self) -> u64 {
        1 << ((self.row() * 8) + self.col())
    }

    pub(crate) fn distance_to(&self, other: Tile) -> u8 {
        if self.row() == other.row() {
            self.col().abs_diff(other.col())
        } else {
            self.row().abs_diff(other.row())
        }
    }
}

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tile(row={}, col={})", self.row(), self.col())
    }
}

impl From<Tile> for (u8, u8) {
    fn from(value: Tile) -> Self {
        (value.row(), value.col())
    }
}

#[derive(Eq, PartialEq, Debug)]
enum Plane {
    Vertical = 0,
    Horizontal = 1
}

/// A single move from one tile to another.
///
/// This is implemented a combination of source tile and another byte which encodes the direction
/// and distance of movement. The most significant bit represents whether the move is vertical or
/// horizontal and the remaining bits represent the distance to be moved (with a negative value
/// representing a move "backwards" along the relevant plane, ie, to a lower-numbered row or
/// column). This way, moves are guaranteed to be along a row or column.
#[derive(Debug)]
pub(crate) struct Move {
    pub(crate) from: Tile,
    plane_disp: u8
}

impl Move {

    pub(crate) fn new(src: Tile, dst: Tile) -> Result<Self, MoveError> {
        let plane_bit: u8;
        let len: i8;
        if src.row() == dst.row() {
            plane_bit = 0x80;
            len = (dst.col() as i8) - (src.col() as i8);
        } else if src.col() == dst.col() {
            plane_bit = 0;
            len = (dst.row() as i8) - (src.row() as i8);
        } else {
            return Err(DisjointTiles)
        };
        Ok(Self {
            from: src,
            plane_disp: plane_bit | ((len & 0x7F) as u8)
        })
    }
    pub(crate) fn plane(&self) -> Plane {
        if (self.plane_disp & 0x80) == 0 {
            Vertical
        } else {
            Horizontal
        }
    }

    pub(crate) fn displacement(&self) -> i8 {
        let bits = (self.plane_disp & 0x7F) as i8;
        if (bits & 0x40) != 0 {
            bits | !0b0111_1111
        } else {
            bits
        }
    }

    /// The move's destination tile.
    pub(crate) fn to(&self) -> Tile {
        let d = self.displacement();
        match self.plane() {
            Vertical => Tile::new(((self.from.row() as i8) + d) as u8, self.from.col()),
            Horizontal => Tile::new(self.from.row(), ((self.from.col() as i8) + d) as u8)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tiles::Plane::{Horizontal, Vertical};
    use crate::tiles::{Move, Tile};

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

    #[test]
    fn test_moves() {
        let m_res = Move::new(Tile::new(2, 4), Tile::new(2, 6));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 4));
        assert_eq!(m.plane(), Horizontal);
        assert_eq!(m.displacement(), 2);
        assert_eq!(m.to(), Tile::new(2, 6));

        let m_res = Move::new(Tile::new(2, 3), Tile::new(5, 3));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 3));
        assert_eq!(m.plane(), Vertical);
        assert_eq!(m.displacement(), 3);
        assert_eq!(m.to(), Tile::new(5, 3));

        let m_res = Move::new(Tile::new(1, 4), Tile::new(1, 1));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(1, 4));
        assert_eq!(m.plane(), Horizontal);
        assert_eq!(m.displacement(), -3);
        assert_eq!(m.to(), Tile::new(1, 1));

        let m_res = Move::new(Tile::new(7, 5), Tile::new(0, 5));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(7, 5));
        assert_eq!(m.plane(), Vertical);
        assert_eq!(m.displacement(), -7);
        assert_eq!(m.to(), Tile::new(0, 5));

        let m_res = Move::new(Tile::new(2, 3), Tile::new(3, 6));
        println!("{m_res:?}");
        assert!(m_res.is_err());
    }
}