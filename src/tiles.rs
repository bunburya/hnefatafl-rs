use crate::error::MoveError::DisjointTiles;
use crate::error::ParseError::{BadChar, BadMove, BadString, EmptyString};
use crate::error::{MoveError, ParseError};
use crate::tiles::Plane::{Horizontal, Vertical};
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

/// The location of a single tile on the board, ie, row and column. This struct is only a reference
/// to a location on the board, and does not contain any other information such as piece placement,
/// etc.
///
/// It is implemented as a single byte, where the most significant four bits describe the row
/// and the least significant four bits describe the column. It is therefore appropriate for use
/// with square boards up to 16x16.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Tile(pub(crate) u8);

impl Tile {
    
    /// Create a new [`Tile`] with the given row and column.
    pub fn new(row: u8, col: u8) -> Self {
        Self((row << 4) | (col & 0x0F))
    }
    
    /// The row on which this tile is situated.
    pub fn row(&self) -> u8 {
        self.0 >> 4
    }
    
    /// The column on which this tile is situated.
    pub fn col(&self) -> u8 {
        self.0 & 0b0000_1111
    }

    pub(crate) fn from_byte(byte: u8) -> Self {
        Self(byte)
    }

    /// Get only the bits representing the row (with the other bits set to zero).
    pub(crate) fn row_bits(&self) -> u8 {
        self.0 & 0b1111_0000
    }
    
    /// Get only the bits representing the column (with the other bits set to zero).
    pub(crate) fn col_bits(&self) -> u8 {
        self.0 & 0b0000_1111
    }
}

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tile(row={}, col={})", self.row(), self.col())
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", (self.col() + 97) as char, self.row() + 1)
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
        (value.row(), value.col())
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Plane {
    Vertical = 0,
    Horizontal = 1
}

/// A single move from one tile to another.
///
/// This is implemented as a combination of source tile and another byte which encodes the direction
/// and distance of movement. The most significant bit represents whether the move is vertical or
/// horizontal and the remaining bits represent the distance to be moved (with a negative value
/// representing a move "backwards" along the relevant plane, ie, to a lower-numbered row or
/// column). This way, moves are guaranteed to be along a row or column.
#[derive(Debug, Eq, PartialEq)]
pub struct Move {
    pub from: Tile,
    plane_disp: u8
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
    
    /// Create a new [`Move`] from source and destination tiles.
    pub fn from_tiles(src: Tile, dst: Tile) -> Result<Self, MoveError> {
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
    
    /// The plane along which the move occurs, ie, horizontal or vertical.
    pub fn plane(&self) -> Plane {
        if (self.plane_disp & 0x80) == 0 {
            Vertical
        } else {
            Horizontal
        }
    }

    /// The signed distance in tiles covered by the move. A negative number means that the move is
    /// going "backwards", ie, to a lower-numbered row or column.
    pub fn displacement(&self) -> i8 {
        let bits = (self.plane_disp & 0x7F) as i8;
        if (bits & 0x40) != 0 {
            bits | !0b0111_1111
        } else {
            bits
        }
    }
    
    /// The unsigned distance in tiles covered by the move. Basically the absolute value of
    /// [Move::displacement].
    pub fn distance(&self) -> u8 {
        self.displacement().unsigned_abs()
    }

    /// The move's destination tile.
    pub fn to(&self) -> Tile {
        let d = self.displacement();
        match self.plane() {
            Vertical => Tile::new(((self.from.row() as i8) + d) as u8, self.from.col()),
            Horizontal => Tile::new(self.from.row(), ((self.from.col() as i8) + d) as u8)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::ParseError::{BadChar, BadInt, BadMove, BadString, EmptyString};
    use crate::tiles::Plane::{Horizontal, Vertical};
    use crate::tiles::{Move, Tile};
    use std::str::FromStr;
    use crate::error::MoveError;

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
    fn test_moves() {
        let m_res = Move::from_tiles(Tile::new(2, 4), Tile::new(2, 6));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 4));
        assert_eq!(m.plane(), Horizontal);
        assert_eq!(m.displacement(), 2);
        assert_eq!(m.to(), Tile::new(2, 6));

        let m_res = Move::from_tiles(Tile::new(2, 3), Tile::new(5, 3));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(2, 3));
        assert_eq!(m.plane(), Vertical);
        assert_eq!(m.displacement(), 3);
        assert_eq!(m.to(), Tile::new(5, 3));

        let m_res = Move::from_tiles(Tile::new(1, 4), Tile::new(1, 1));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(1, 4));
        assert_eq!(m.plane(), Horizontal);
        assert_eq!(m.displacement(), -3);
        assert_eq!(m.distance(), 3);
        assert_eq!(m.to(), Tile::new(1, 1));

        let m_res = Move::from_tiles(Tile::new(7, 5), Tile::new(0, 5));
        assert!(m_res.is_ok());
        let m = m_res.unwrap();
        assert_eq!(m.from, Tile::new(7, 5));
        assert_eq!(m.plane(), Vertical);
        assert_eq!(m.displacement(), -7);
        assert_eq!(m.to(), Tile::new(0, 5));

        let m_res = Move::from_tiles(Tile::new(2, 3), Tile::new(3, 6));
        println!("{m_res:?}");
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