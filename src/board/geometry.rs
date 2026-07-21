use crate::board::state::BoardState;
use crate::collections::tileset::TileSet;
use crate::error::BoardError;
use crate::tiles::{Coords, RowColOffset, Tile, BoardIterator};

use crate::collections::PieceMap;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use crate::bitfield::BitField;

const NEIGHBOR_OFFSETS: [[i8; 2]; 4] = [[-1, 0], [1, 0], [0, -1], [0, 1]];

/// Represents the different ways that the position of special tiles can be determined.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound = ""))]
pub enum SpecialTilePosition<B: BitField> {
    /// The tiles in the given [`TileSet`] are the special tiles.
    Custom(TileSet<B>),
    /// The tiles are in the phyical corners of the board.
    Corners,
    /// There is one tile, in the centre of the board. If the board has an even side length, this
    /// will result in a throne slightly off-centre, in the upper left quarter of the board.
    Center,
    /// There are no special tiles.
    None
}

/// Calculate the physical corners of a board.
fn calculate_corners<B: BitField>(side_len: u8) -> TileSet<B> {
    let mut corners = TileSet::empty();
    corners.insert(Tile::new(0, 0));
    corners.insert(Tile::new(0, side_len - 1));
    corners.insert(Tile::new(side_len - 1, side_len - 1));
    corners.insert(Tile::new(side_len - 1, 0));
    corners

}

/// Calculate the physical centre of a board. If the board has an even side length, the "centre"
/// will in fact be slightly off-centre, in the upper left quarter of the board.
fn calculate_center<B: BitField>(side_len: u8) -> TileSet<B> {
    let mut center = TileSet::empty();
    center.insert(Tile::new(side_len / 2, side_len / 2));
    center
}

impl<B: BitField> SpecialTilePosition<B> {
    /// Convert this rule to a concrete tileset.
    pub fn as_tileset(&self, board_len: u8) -> TileSet<B> {
        match self {
            Self::Custom(ts) => *ts,
            Self::Corners => calculate_corners(board_len),
            Self::Center => calculate_center(board_len),
            Self::None => TileSet::empty(),
        }
    }
}

/// The rules for determining where all special tiles are placed on a board.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound = ""))]
pub struct SpecialTilePlacementRules<B: BitField> {
    pub throne: SpecialTilePosition<B>,
    pub corners: SpecialTilePosition<B>,
    pub attacker_fortresses: SpecialTilePosition<B>,
    pub defender_fortresses: SpecialTilePosition<B>,
}

impl<B: BitField> SpecialTilePlacementRules<B> {

    /// Create rules that specify no special tiles of any kind should be placed on the board.
    pub const fn none() -> SpecialTilePlacementRules<B> {
        Self {
            throne: SpecialTilePosition::None,
            corners: SpecialTilePosition::None,
            attacker_fortresses: SpecialTilePosition::None,
            defender_fortresses: SpecialTilePosition::None,
        }
    }

    /// Create rules that specify a single throne in the center of the board and no other special
    /// tiles.
    pub const fn center_throne() -> SpecialTilePlacementRules<B> {
        Self {
            throne: SpecialTilePosition::Center,
            corners: SpecialTilePosition::None,
            attacker_fortresses: SpecialTilePosition::None,
            defender_fortresses: SpecialTilePosition::None,
        }
    }

    /// Create rules that specify special tiles in the physical corners of the board and no other
    /// special tiles.
    pub const fn physical_corners() -> SpecialTilePlacementRules<B> {
        Self {
            throne: SpecialTilePosition::None,
            corners: SpecialTilePosition::Corners,
            attacker_fortresses: SpecialTilePosition::None,
            defender_fortresses: SpecialTilePosition::None,
        }
    }

    /// Create rules that specify a centre throne and corner tiles, and no other special tiles.
    pub const fn throne_and_corners() -> SpecialTilePlacementRules<B> {
        Self {
            throne: SpecialTilePosition::Center,
            corners: SpecialTilePosition::Corners,
            attacker_fortresses: SpecialTilePosition::None,
            defender_fortresses: SpecialTilePosition::None,
        }
    }

    /// Generate a set of special tile placements based on these rules and a given board length.
    pub fn as_special_tiles(&self, board_len: u8) -> SpecialTiles<B> {
        SpecialTiles {
            throne: self.throne.as_tileset(board_len),
            corners: self.corners.as_tileset(board_len),
            attacker_fortresses: self.attacker_fortresses.as_tileset(board_len),
            defender_fortresses: self.defender_fortresses.as_tileset(board_len),
        }
    }
}

/// The placement of special tiles for a particular board.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound = ""))]
pub struct SpecialTiles<B: BitField> {
    pub throne: TileSet<B>,
    pub corners: TileSet<B>,
    pub attacker_fortresses: TileSet<B>,
    pub defender_fortresses: TileSet<B>,
}

impl<B: BitField> SpecialTiles<B> {
    pub fn none() -> SpecialTiles<B> {
        Self {
            throne: TileSet::empty(),
            corners: TileSet::empty(),
            attacker_fortresses: TileSet::empty(),
            defender_fortresses: TileSet::empty(),
        }
    }
}

/// This struct contains information about the geometry of the board, such as its size and the
/// positions of various special tiles. It does not contain information about piece placement or any
/// other state that would be expected to change over the course of a game.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BoardGeometry<P: PieceMap> {
    pub side_len: u8,
    pub special_tiles: SpecialTiles<P::BitField>,
}

impl<P: PieceMap> BoardGeometry<P> {
    /// Create an empty board with the given side length.
    pub fn new(
        side_len: u8,
        special_tile_rules: SpecialTilePlacementRules<P::BitField>
    ) -> Result<Self, BoardError> {
        if side_len < 3 {
            Err(BoardError::TooSmall)
        } else {
            Ok(Self {
                side_len,
                special_tiles: special_tile_rules.as_special_tiles(side_len),
            })
        }
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
    // TODO: Remove `Vec` allocation here. Neighbours are variable as tile may be at edge of board,
    // but we could implement custom iterator for this.
    pub fn neighbors(&self, tile: Tile) -> Vec<Tile> {
        let row = tile.row;
        let col = tile.col;
        let signed_row = row as i8;
        let signed_col = col as i8;
        let mut neighbors: Vec<Tile> = vec![];
        for [r_off, c_off] in NEIGHBOR_OFFSETS.iter() {
            let coords = Coords {
                row: signed_row + r_off,
                col: signed_col + c_off,
            };
            if let Ok(t) = self.coords_to_tile(coords) {
                neighbors.push(t);
            }
        }
        neighbors
    }

    /// Get all the tiles between the given two tiles. If given tiles do not share a row or column,
    /// an empty vector is returned.
    // TODO: Figure out if we could remove the `Vec` allocation here. Either have an iterator or
    // even a TileSet?
    pub fn tiles_between(&self, t1: Tile, t2: Tile) -> Vec<Tile> {
        let mut tiles: Vec<Tile> = vec![];
        let (r1, c1, r2, c2) = (t1.row, t1.col, t2.row, t2.col);
        if r1 == r2 {
            let col_range = if c1 > c2 { (c2 + 1)..c1 } else { (c1 + 1)..c2 };
            for col in col_range {
                tiles.push(Tile::new(r1, col))
            }
        } else if c1 == c2 {
            let row_range = if r1 > r2 { (r2 + 1)..r1 } else { (r1 + 1)..r2 };
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

    /// Return the tile (if any) which is the same distance from `t2` as `t2` is from `t1`.
    pub fn far_tile(&self, t1: Tile, t2: Tile) -> Option<Tile> {
        let c1: Coords = t1.into();
        let c2: Coords = t2.into();
        let offset: RowColOffset = c2 - c1;
        self.coords_to_tile(c2 + offset).ok()
    }

    /// Check whether the given tile is surrounded on all sides by pieces (friend or foe).
    pub fn tile_surrounded(&self, tile: Tile, state: &BoardState<P>) -> bool {
        self.neighbors(tile).iter().all(|t| state.tile_occupied(*t))
    }

    /// Check whether the given `Coords` refer to a corner.
    pub fn coords_are_corner(&self, coords: Coords) -> bool {
        if let Ok(t) = self.coords_to_tile(coords) {
            self.special_tiles.corners.contains(t)
        } else {
            false
        }
    }

    /// Return an iterator over all tiles on the board.
    pub fn iter_tiles(&self) -> BoardIterator {
        BoardIterator::new(self.side_len)
    }

    /// Generate the FEN string describing the current board state
    pub fn to_fen(&self, state: &BoardState<P>) -> String {
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
    use crate::aliases::SmallBasicPieceMap;
    use crate::board::geometry::{BoardGeometry, SpecialTilePlacementRules};
    use crate::tiles::Tile;
    use crate::utils::check_tile_vec;

    #[test]
    fn test_neighbors() {
        let geo: BoardGeometry<SmallBasicPieceMap> = BoardGeometry::new(
            7,
            SpecialTilePlacementRules::none()
        ).unwrap();
        let n = geo.neighbors(Tile::new(0, 0));
        check_tile_vec(n, vec![Tile::new(0, 1), Tile::new(1, 0)]);

        let n = geo.neighbors(Tile::new(3, 2));
        check_tile_vec(
            n,
            vec![
                Tile::new(2, 2),
                Tile::new(3, 1),
                Tile::new(3, 3),
                Tile::new(4, 2),
            ],
        );

        let b = geo.tiles_between(Tile::new(2, 2), Tile::new(2, 5));
        check_tile_vec(b, vec![Tile::new(2, 3), Tile::new(2, 4)]);

        let b = geo.tiles_between(Tile::new(1, 3), Tile::new(4, 3));
        check_tile_vec(b, vec![Tile::new(2, 3), Tile::new(3, 3)]);

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(3, 3));
        assert!(b.is_empty());

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(1, 1));
        assert!(b.is_empty());

        let b = geo.tiles_between(Tile::new(1, 1), Tile::new(1, 2));
        assert!(b.is_empty());
    }

    #[test]
    fn test_far_tile() {
        let geo: BoardGeometry<SmallBasicPieceMap> = BoardGeometry::new(
            7,
            SpecialTilePlacementRules::none()
        ).unwrap();
        assert_eq!(geo.far_tile(Tile::new(0, 0), Tile::new(0, 1)), Some(Tile::new(0, 2)));
        assert_eq!(geo.far_tile(Tile::new(1, 1), Tile::new(2, 3)), Some(Tile::new(3, 5)));
        assert_eq!(geo.far_tile(Tile::new(1, 1), Tile::new(1, 1)), Some(Tile::new(1, 1)));
        assert_eq!(geo.far_tile(Tile::new(2, 1), Tile::new(1, 1)), Some(Tile::new(0, 1)));
        assert_eq!(geo.far_tile(Tile::new(1, 1), Tile::new(0, 0)), None);
        assert_eq!(geo.far_tile(Tile::new(0, 5), Tile::new(0, 6)), None);
    }
}
