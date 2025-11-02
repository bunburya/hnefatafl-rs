use crate::bitfield::BitField;
use crate::tiles::Tile;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound = "B: serde::Serialize + serde::de::DeserializeOwned")
)]
pub struct TileSet<B: BitField> {
    bitfield: B,
}

impl<B: BitField> TileSet<B> {
    /// Return an empty set.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Return the number of tiles in the set.
    pub fn count(&self) -> u32 {
        self.bitfield.count_ones()
    }

    /// Check whether the given tile is in the set.
    pub fn contains(&self, t: Tile) -> bool {
        !(B::tile_mask(t) & self.bitfield).is_empty()
    }

    /// Check whether the tile represented by the given mask is in the set
    /// (see [`BitField::tile_mask`]).
    ///
    /// **NOTE**: [`TileSet::contains`] works on tiles directly and is generally preferable; this
    /// function is included to allow certain performance optimisations (e.g. where we need to check
    /// if the same tile is in multiple sets).
    pub fn contains_mask(&self, mask: B) -> bool {
        !(mask & self.bitfield).is_empty()
    }

    /// Check whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.bitfield.is_empty()
    }

    /// Add the given tile to the set, if not present.
    pub fn insert(&mut self, t: Tile) {
        self.bitfield |= B::tile_mask(t)
    }

    /// Remove the given tile from the set, if present.
    pub fn remove(&mut self, t: Tile) {
        self.bitfield &= !B::tile_mask(t)
    }

    /// Remove all tiles from the set.
    pub fn clear(&mut self) {
        self.bitfield.clear()
    }

    /// Extend this set by adding all the elements of the other set.
    pub fn extend(&mut self, other: &Self) {
        self.bitfield |= other.bitfield
    }

    /// Return a set containing each tile in `self` or `other`.
    pub fn union(&self, other: &Self) -> Self {
        Self {
            bitfield: self.bitfield | other.bitfield,
        }
    }

    /// Return a set containing each tile that is in `self` but not in `other`.
    pub fn difference(&self, other: &Self) -> Self {
        Self {
            bitfield: self.bitfield & !other.bitfield,
        }
    }

    /// Return a set containing each tile that is in both `self` and `other`.
    pub fn intersection(&self, other: &Self) -> Self {
        Self {
            bitfield: self.bitfield & other.bitfield,
        }
    }

    /// Return the first tile in the set. (Order is not generally guaranteed so in practice this
    /// is mainly useful to find the *only* tile in the set, where we know there is only one).
    pub fn first(&self) -> Tile {
        B::bit_to_tile(self.bitfield.trailing_zeros())
    }
}

impl<B: BitField> IntoIterator for TileSet<B> {
    type Item = Tile;
    type IntoIter = BitfieldTileIter<B>;

    fn into_iter(self) -> Self::IntoIter {
        BitfieldTileIter::new(self.bitfield)
    }
}

impl<B: BitField> IntoIterator for &TileSet<B> {
    type Item = Tile;
    type IntoIter = BitfieldTileIter<B>;
    fn into_iter(self) -> Self::IntoIter {
        BitfieldTileIter::new(self.bitfield)
    }
}

impl<B: BitField> BitOr for TileSet<B> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(&rhs)
    }
}

impl<B: BitField> BitAnd for TileSet<B> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.intersection(&rhs)
    }
}

impl<B: BitField> BitOrAssign for TileSet<B> {
    fn bitor_assign(&mut self, rhs: Self) {
        self.extend(&rhs)
    }
}

impl<B: BitField> BitAndAssign for TileSet<B> {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = self.intersection(&rhs)
    }
}

impl<B: BitField> Not for TileSet<B> {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            bitfield: !self.bitfield,
        }
    }
}

/// An iterator over set bits in a [`BitField`]. The index of each set bit is converted to a
/// [`Tile`] before being yielded.
pub struct BitfieldTileIter<B: BitField> {
    /// Bitfield representing board state.
    state: B,
    /// Keeps track of current position in the bitfield.
    i: u32,
}

impl<B: BitField> BitfieldTileIter<B> {
    pub fn new(state: B) -> Self {
        Self { state, i: 0 }
    }
}

impl<B: BitField> Iterator for BitfieldTileIter<B> {
    type Item = Tile;

    fn next(&mut self) -> Option<Self::Item> {
        let skipped = self.state >> self.i;
        if skipped.is_empty() {
            return None;
        }
        self.i += skipped.trailing_zeros() + 1;
        Some(B::bit_to_tile(self.i - 1))
    }
}

impl<B: BitField> From<TileSet<B>> for HashSet<Tile> {
    fn from(tile_set: TileSet<B>) -> Self {
        let mut set = HashSet::new();
        for t in tile_set {
            set.insert(t);
        }
        set
    }
}

impl<'a, B: BitField, T: Iterator<Item = &'a Tile>> From<T> for TileSet<B> {
    fn from(tiles: T) -> Self {
        let mut tile_set = Self::empty();
        for t in tiles {
            tile_set.insert(*t);
        }
        tile_set
    }
}

/// Creates a [`TileSet`] containing the arguments, similar to [`vec!`].
#[macro_export]
macro_rules! tileset {
    ($( $x: expr ),* ) => {
        {
            use $crate::collections::tileset::TileSet;
            let mut tmp = TileSet::empty();
            $(
                tmp.insert($x);
            )*
            tmp
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile;

    #[test]
    fn basic_test() {
        let mut tile_set = TileSet::<u64>::empty();
        assert!(tile_set.is_empty());
        assert_eq!(tile_set.count(), 0);
        tile_set.insert(Tile::new(1, 3));
        assert!(!tile_set.is_empty());
        assert_eq!(tile_set.count(), 1);
        assert!(tile_set.contains(Tile::new(1, 3)));
        assert!(!tile_set.contains(Tile::new(1, 4)));

        let other_tile_set = TileSet::<u64>::from(
            vec![&Tile::new(4, 2), &Tile::new(1, 3), &Tile::new(6, 1)].into_iter(),
        );
        assert_eq!(other_tile_set.count(), 3);
        let intersection = tile_set.intersection(&other_tile_set);
        assert!(intersection.contains(Tile::new(1, 3)));
        assert!(!intersection.contains(Tile::new(4, 2)));
        assert!(!intersection.contains(Tile::new(6, 1)));
    }
}
