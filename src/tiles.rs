use std::collections::HashSet;
use crate::error::MoveError::DisjointTiles;
use crate::error::ParseError::{BadChar, BadMove, BadString, EmptyString};
use crate::error::{MoveError, ParseError};
use crate::tiles::Axis::{Horizontal, Vertical};
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

/// The location of a single tile on the board, ie, row and column. This struct is only a reference
/// to a location on the board, and does not contain any other information such as piece placement,
/// etc.
///
/// It is implemented as a single byte, where the most significant four bits describe the row
/// and the least significant four bits describe the column. It is therefore appropriate for use
/// with square boards up to 16x16.
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
    
    /// The tile's position on the given axis, ie, the tile's row is `axis` is [`Vertical`] and its
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

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
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

/// A single move from one tile to another.
///
/// This is implemented as a combination of source tile, axis of movement and displacement (with a
/// negative displacement representing a move "backwards" along the relevant axis, ie, to a
/// lower-numbered row or column). This way, moves are guaranteed to be along a row or column (but
/// are not guaranteed to be within the bounds of the board).
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Move {
    pub from: Tile,
    /// The axis along which the move occurs, ie, horizontal or vertical.
    axis: Axis,
    /// The signed distance in tiles covered by the move. A negative number means that the move is
    /// going "backwards", ie, to a lower-numbered row or column.
    displacement: i8
}

impl FromStr for Move {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens: Vec<&str> = s.split('-').collect();
        if tokens.len() != 2 {
            return Err(BadString(String::from(s)))
        };
        let m_res = Move::from_tiles(
            Tile::from_str(tokens[0])?,
            Tile::from_str(tokens[1])?
        );
        match m_res {
            Ok(m) => Ok(m),
            Err(e) => Err(BadMove(e))
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.from, self.to())
    }
}

impl Move {
    
    pub fn new(from: Tile, axis: Axis, displacement: i8) -> Self {
        Self { from, axis, displacement }
    }
    
    /// Create a new [`Move`] from source and destination tiles.
    pub fn from_tiles(src: Tile, dst: Tile) -> Result<Self, MoveError> {
        let axis: Axis;
        let displacement: i8;
        if src.row == dst.row {
            axis = Horizontal;
            displacement = (dst.col as i8) - (src.col as i8);
        } else if src.col == dst.col {
            axis = Vertical;
            displacement = (dst.row as i8) - (src.row as i8);
        } else {
            return Err(DisjointTiles)
        };
        Ok(Self::new(src, axis, displacement))
    }

    pub fn from_str_with_captures(s: &str) -> Result<(Self, HashSet<Tile>), ParseError> {
        if s.is_empty() {
            return Err(EmptyString);
        }
        let tokens = s.split('x').collect::<Vec<&str>>();
        let m = Self::from_str(tokens[0])?;
        let mut captures: HashSet<Tile> = HashSet::new();
        for c in tokens[1..].iter() {
            captures.insert(Tile::from_str(c)?);
        }
        Ok((m, captures))
    }
    
    /// The unsigned distance in tiles covered by the move. Basically the absolute value of
    /// [Move::displacement].
    pub fn distance(&self) -> u8 {
        self.displacement.unsigned_abs()
    }

    /// The move's destination tile.
    pub fn to(&self) -> Tile {
        let d = self.displacement;
        match self.axis {
            Vertical => Tile::new(((self.from.row as i8) + d) as u8, self.from.col),
            Horizontal => Tile::new(self.from.row, ((self.from.col as i8) + d) as u8)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ParseError::{BadChar, BadInt, BadMove, BadString, EmptyString};
    use crate::tiles::Axis::{Horizontal, Vertical};
    use crate::tiles::{Move, Tile};
    use std::str::FromStr;
    use crate::error::MoveError;

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
        let m_res = Move::from_tiles(Tile::new(2, 4), Tile::new(2, 6));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 4));
        assert_eq!(m.axis, Horizontal);
        assert_eq!(m.displacement, 2);
        assert_eq!(m.to(), Tile::new(2, 6));

        let m_res = Move::from_tiles(Tile::new(2, 3), Tile::new(5, 3));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 3));
        assert_eq!(m.axis, Vertical);
        assert_eq!(m.displacement, 3);
        assert_eq!(m.to(), Tile::new(5, 3));

        let m_res = Move::from_tiles(Tile::new(1, 4), Tile::new(1, 1));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(1, 4));
        assert_eq!(m.axis, Horizontal);
        assert_eq!(m.displacement, -3);
        assert_eq!(m.distance(), 3);
        assert_eq!(m.to(), Tile::new(1, 1));

        let m_res = Move::from_tiles(Tile::new(7, 5), Tile::new(0, 5));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(7, 5));
        assert_eq!(m.axis, Vertical);
        assert_eq!(m.displacement, -7);
        assert_eq!(m.to(), Tile::new(0, 5));

        let m_res = Move::from_tiles(Tile::new(2, 3), Tile::new(3, 6));
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
        let parsed_m = Move::from_str("a8-a11");
        let m = Move::from_tiles(
            Tile::new(7, 0), 
            Tile::new(10, 0)
        ).unwrap();
        assert!(parsed_m.is_ok());
        assert_eq!(parsed_m.unwrap(), m);
        assert_eq!(m.to_string(), "a8-a11");

        let parsed_m = Move::from_str("f5-d5");
        let m = Move::from_tiles(
            Tile::new(4, 5),
            Tile::new(4, 3)
        ).unwrap();
        assert!(parsed_m.is_ok());
        assert_eq!(parsed_m.unwrap(), m);
        assert_eq!(m.to_string(), "f5-d5");
        
        let parsed_m = Move::from_str("f5-d6");
        assert_eq!(parsed_m, Err(BadMove(MoveError::DisjointTiles)));
        
        let parsed_m = Move::from_str("f5-d7-d6");
        assert_eq!(parsed_m, Err(BadString(String::from("f5-d7-d6"))));
        
        let parsed_m = Move::from_str("f5-d]");
        assert!(matches!(parsed_m, Err(BadInt(_))));
        
        let parsed_m = Move::from_str("!5-d5");
        assert_eq!(parsed_m, Err(BadChar('!')));
    }
}