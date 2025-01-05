use crate::error::ParseError::{BadChar, EmptyString};
use crate::error::ParseError;
use crate::tiles::Axis::{Horizontal, Vertical};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Add;
use std::str::FromStr;

/// An offset which can be applied to [`Coords`] and which is composed of the axis of movement and
/// an offset along that axis.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AxisOffset {
    /// The axis along which the tile is offset.
    pub axis: Axis,
    /// The signed length in tiles of the offset. A negative number means that the offset is
    /// going "backwards", ie, to a lower-numbered row or column.
    pub displacement: i8,
}

impl AxisOffset {
    pub fn new(axis: Axis, displacement: i8) -> Self {
        Self { axis, displacement }
    }

    /// Return the Manhattan distance represented by this offset.
    pub fn manhattan_dist(&self) -> u8 {
        self.displacement.unsigned_abs()
    }
}

/// An offset which can be applied to [`Coords`] and which is composed of the row and column offset
/// to be applied.
#[derive(Debug, Copy, Clone)]
pub struct RowColOffset{
    row: i8,
    col: i8
}

impl RowColOffset {
    pub fn new(row: i8, col: i8) -> Self {
        Self { row, col }
    }

    /// Return the Manhattan distance represented by this offset.
    pub fn manhattan_dist(&self) -> u8 {
        self.row.unsigned_abs() + self.col.unsigned_abs()
    }
}

/// An unbounded row-column pair representing a hypothetical location, which may or may not be on
/// the board. Can be used to represent out-of-bounds locations, including those with negative row
/// or column values.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Coords {
    pub row: i8,
    pub col: i8
}

impl Coords {
    pub fn new(row: i8, col: i8) -> Self {
        Self { row, col }
    }

    pub fn row_col_offset_from(&self, other: Coords) -> RowColOffset {
        RowColOffset {
            row: self.row - other.row,
            col: self.col - other.col
        }
    }
}

impl From<Tile> for Coords {
    fn from(t: Tile) -> Self {
        Self {
            row: t.row as i8,
            col: t.col as i8
        }
    }
}

impl Add<RowColOffset> for Coords {
    type Output = Self;

    fn add(self, rhs: RowColOffset) -> Self {
        Self {
            row: self.row + rhs.row,
            col: self.col + rhs.col
        }
    }
}

impl Add<AxisOffset> for Coords {
    type Output = Self;
    fn add(self, rhs: AxisOffset) -> Self {
        match rhs.axis {
            Vertical => Coords::new(self.row + rhs.displacement, self.col),
            Horizontal => Coords::new(self.row, self.col + rhs.displacement),
        }
    }
}

/// The location of a single tile on the board, ie, row and column. This struct is only a reference
/// to a location on the board, and does not contain any other information such as piece placement,
/// etc.
///
/// Avoid constructing `Tile`s which may refer to positions not on the game board (use [`Coords`]
/// for that instead).
#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Tile {
    pub row: u8,
    pub col: u8
}

impl Tile {
    
    /// Create a new [`Tile`] with the given row and column.
    pub fn new(row: u8, col: u8) -> Self {
        Self { row, col }
    }
    
    /// The tile's position on the given axis, ie, the tile's row if `axis` is [`Vertical`] and its
    /// column if `axis` is [`Horizontal`]. 
    pub fn posn_on_axis(&self, axis: Axis) -> u8 {
        match axis {
            Vertical => self.row,
            Horizontal => self.col
        }
    }
    
}

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tile(row={}, col={})", self.row, self.col)
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", (self.col + 97) as char, self.row + 1)
    }
}

impl FromStr for Tile {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let col = if let Some(&byte) = s.as_bytes().first() {
            if !(97..=122).contains(&byte) {
                return Err(BadChar(byte as char))
            }
            byte - 97
        } else {
            return Err(EmptyString)
        };
        Ok(Tile::new(s[1..].parse::<u8>()? - 1, col))

    }
}

impl From<Tile> for (u8, u8) {
    fn from(value: Tile) -> Self {
        (value.row, value.col)
    }
}

/// A single axis of movement (vertical or horizontal).
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum Axis {
    Vertical = 0,
    Horizontal = 0x80
}

impl Axis {
    pub fn other(&self) -> Axis {
        match self {
            Vertical => Horizontal,
            Horizontal => Vertical
        }
    }
}

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


#[cfg(test)]
mod tests {
    use crate::error::ParseError::{BadChar, BadInt, BadPlay, BadString, EmptyString};
    use crate::error::PlayError;
    use crate::play::Play;
    use crate::tiles::Axis::{Horizontal, Vertical};
    use crate::tiles::Tile;
    use std::str::FromStr;

    #[test]
    fn test_tile_creation() {
        for r in 0..16 {
            for c in 0..16 {
                let t = Tile::new(r, c);
                assert_eq!(t.row, r);
                assert_eq!(t.col, c);
            }
        }
    }
    
    #[test]
    fn test_moves() {
        let p_res = Play::from_tiles(Tile::new(2, 4), Tile::new(2, 6));
        assert!(p_res.is_ok());
        let p = p_res.unwrap();
        assert_eq!(p.from, Tile::new(2, 4));
        assert_eq!(p.movement.axis, Horizontal);
        assert_eq!(p.movement.displacement, 2);
        assert_eq!(p.to(), Tile::new(2, 6));

        let p_res = Play::from_tiles(Tile::new(2, 3), Tile::new(5, 3));
        assert!(p_res.is_ok());
        let p = p_res.unwrap();
        assert_eq!(p.from, Tile::new(2, 3));
        assert_eq!(p.movement.axis, Vertical);
        assert_eq!(p.movement.displacement, 3);
        assert_eq!(p.to(), Tile::new(5, 3));

        let p_res = Play::from_tiles(Tile::new(1, 4), Tile::new(1, 1));
        assert!(p_res.is_ok());
        let p = p_res.unwrap();
        assert_eq!(p.from, Tile::new(1, 4));
        assert_eq!(p.movement.axis, Horizontal);
        assert_eq!(p.movement.displacement, -3);
        assert_eq!(p.distance(), 3);
        assert_eq!(p.to(), Tile::new(1, 1));

        let p_res = Play::from_tiles(Tile::new(7, 5), Tile::new(0, 5));
        assert!(p_res.is_ok());
        let p = p_res.unwrap();
        assert_eq!(p.from, Tile::new(7, 5));
        assert_eq!(p.movement.axis, Vertical);
        assert_eq!(p.movement.displacement, -7);
        assert_eq!(p.to(), Tile::new(0, 5));

        let m_res = Play::from_tiles(Tile::new(2, 3), Tile::new(3, 6));
        assert!(m_res.is_err());
    }
    
    #[test]
    fn test_parsing_tiles() {
        let parsed_t = Tile::from_str("a8");
        let t = Tile::new(7, 0);
        assert!(parsed_t.is_ok());
        assert_eq!(parsed_t.unwrap(), t);
        assert_eq!(t.to_string(), "a8");

        let parsed_t = Tile::from_str("f14");
        let t = Tile::new(13, 5);
        assert!(parsed_t.is_ok());
        assert_eq!(parsed_t.unwrap(), t);
        assert_eq!(t.to_string(), "f14");
        
        assert_eq!(Tile::from_str(""), Err(EmptyString));
        assert_eq!(Tile::from_str("[53"), Err(BadChar('[')));
        assert!(matches!(Tile::from_str("a!!"), Err(BadInt(_))));
    }
    
    #[test]
    fn test_parsing_moves() {
        let parsed_m = Play::from_str("a8-a11");
        let m = Play::from_tiles(
            Tile::new(7, 0), 
            Tile::new(10, 0)
        ).unwrap();
        assert!(parsed_m.is_ok());
        assert_eq!(parsed_m.unwrap(), m);
        assert_eq!(m.to_string(), "a8-a11");

        let parsed_m = Play::from_str("f5-d5");
        let m = Play::from_tiles(
            Tile::new(4, 5),
            Tile::new(4, 3)
        ).unwrap();
        assert!(parsed_m.is_ok());
        assert_eq!(parsed_m.unwrap(), m);
        assert_eq!(m.to_string(), "f5-d5");
        
        let parsed_m = Play::from_str("f5-d6");
        assert_eq!(parsed_m, Err(BadPlay(PlayError::DisjointTiles)));
        
        let parsed_m = Play::from_str("f5-d7-d6");
        assert_eq!(parsed_m, Err(BadString(String::from("f5-d7-d6"))));
        
        let parsed_m = Play::from_str("f5-d]");
        assert!(matches!(parsed_m, Err(BadInt(_))));
        
        let parsed_m = Play::from_str("!5-d5");
        assert_eq!(parsed_m, Err(BadChar('!')));
    }
}