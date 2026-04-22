use crate::bitfield::BitField;
use crate::collections::tileset::{BitfieldTileIter, TileSet};
use crate::pieces::PieceType::{King, Soldier, Knight, Commander, Guard, Mercenary};
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, PlacedPiece, Side};
use crate::tiles::Tile;
use std::fmt::Debug;
use std::hash::Hash;

use crate::collections::pieceset::PieceSet;
use crate::error::{ParseError, PieceMapError};
use crate::error::ParseError::BadLineLen;
#[cfg(feature = "serde")]
use serde::{de::DeserializeOwned, Deserialize, Serialize};

pub trait CommonPieceMapSuperTraits:
    Default
    + Debug
    + PartialEq
    + Eq
    + Clone
    + Copy
    + IntoIterator<Item = PlacedPiece>
    + FromIterator<PlacedPiece>
{
}

#[cfg(feature = "serde")]
pub trait PieceMapSuperTraits: CommonPieceMapSuperTraits + Serialize {}

#[cfg(not(feature = "serde"))]
pub trait PieceMapSuperTraits: CommonPieceMapSuperTraits {}

/// A mapping of tiles to the pieces (if any) that occupy them. Behaves in some way like a
/// `HashMap<Tile, Option<Piece>>` might, though it's not necessarily implemented that way.
pub trait PieceMap: PieceMapSuperTraits {
    type BitField: BitField;

    /// Return a set of all tiles that are occupied by any piece.
    fn occupied(&self) -> TileSet<Self::BitField>;

    /// Return a set of all tiles that are occupied by any piece in the given set.
    fn occupied_by(&self, piece_set: PieceSet) -> TileSet<Self::BitField>;

    /// Get the piece, if any, at the given tile.
    fn get(&self, t: Tile) -> Option<Piece>;

    /// Check if the given tile contains the given piece.
    fn contains_piece(&self, t: Tile, piece: Piece) -> bool;

    /// Check if the given tile contains any piece.
    fn contains_any(&self, t: Tile) -> bool {
        self.occupied().contains(t)
    }

    /// Insert the given piece at the given tile.
    fn set(&mut self, t: Tile, piece: Piece) -> Result<(), PieceMapError>;

    /// Insert the given piece at the relevant tile.
    fn set_placed_piece(&mut self, p: PlacedPiece) {
        self.set(p.tile, p.piece);
    }

    /// Clear the given tile.
    fn remove(&mut self, t: Tile);

    /// Clear all tiles in the given set.
    fn clear_tiles(&mut self, tiles: TileSet<Self::BitField>);

    /// Return a copy of this map with all pieces in the given set removed.
    fn without_pieces(&self, piece_set: PieceSet) -> Self;

    /// Extend this map with the pieces in the given map.
    fn extend(&mut self, other: &Self);

    /// Check whether this map is empty.
    fn is_empty(&self) -> bool;

    /// Parse board state from (the relevant part of) a string in FEN format.
    fn from_fen(fen: &str) -> Result<(Self, u8), ParseError> {
        let mut pm = Self::default();
        let mut side_len = 0;
        for (r, line) in fen.split('/').enumerate() {
            let mut n_empty = 0;
            let mut c = 0u8;
            for chr in line.chars() {
                if chr.is_ascii_digit() {
                    n_empty = (n_empty * 10) + (chr as u8 - b'0');
                } else {
                    c += n_empty;
                    n_empty = 0;
                    pm.set(Tile::new(r as u8, c), Piece::try_from(chr)?);
                    c += 1;
                }
            }
            if n_empty > 0 {
                c += n_empty;
            }
            if side_len == 0 {
                side_len = c;
            } else if side_len != c {
                return Err(BadLineLen(c as usize));
            }
        }
        Ok((pm, side_len))
    }

    /// Return a string in FEN format representing the board state.
    fn to_fen(&self, side_len: u8) -> String {
        let mut s = String::new();
        for row in 0..side_len {
            let mut n_empty = 0;
            for col in 0..side_len {
                let t = Tile::new(row, col);
                if let Some(piece) = self.get(t) {
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
            if row < side_len - 1 {
                s.push('/');
            }
        }
        s
    }

    /// Return an iterator over all tiles containing the given piece.
    fn iter_tiles_for_piece(&self, piece: Piece) -> BitfieldTileIter<Self::BitField>;

    /// Given one supported piece, return the next supported piece. If `None` is passed, return the
    /// first supported piece.
    fn next_piece(piece: Option<Piece>) -> Option<Piece>;

    /// Return the tile containing the king, if any.
    fn find_king(&self) -> Option<Tile>;

    /// Count the number of pieces of the given side.
    fn count_pieces_of_side(&self, side: Side) -> u32;

    /// Return the set of all tiles occupied by the given side.
    fn occupied_by_side(&self, side: Side) -> TileSet<Self::BitField>;

}

/// A [`PieceMap`] implemented using bitfields which is capable of representing the basic pieces
/// (attacking and defending soldiers, and defending king).
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound = "B: serde::Serialize + serde::de::DeserializeOwned")
)]
pub struct BasicPieceMap<B: BitField> {
    pub attacking_soldier: TileSet<B>,
    pub defending_soldier: TileSet<B>,
    pub king: TileSet<B>,
}

impl<B: BitField> CommonPieceMapSuperTraits for BasicPieceMap<B> {}

#[cfg(feature = "serde")]
impl<B: BitField> PieceMapSuperTraits for BasicPieceMap<B> where B: Serialize + DeserializeOwned {}

#[cfg(not(feature = "serde"))]
impl<B: BitField> PieceMapSuperTraits for BasicPieceMap<B> {}

impl<B: BitField> PieceMap for BasicPieceMap<B> {
    type BitField = B;

    fn occupied(&self) -> TileSet<B> {
        self.attacking_soldier | self.defending_soldier | self.king
    }

    fn occupied_by(&self, piece_set: PieceSet) -> TileSet<B> {
        let mut ts = TileSet::empty();
        if piece_set.contains(Piece::attacker(Soldier)) {
            ts.extend(&self.attacking_soldier);
        }
        if piece_set.contains(Piece::defender(Soldier)) {
            ts.extend(&self.defending_soldier);
        }
        if piece_set.contains(Piece::king()) {
            ts.extend(&self.king);
        }
        ts
    }

    fn get(&self, t: Tile) -> Option<Piece> {
        let mask = B::tile_mask(t);
        if self.attacking_soldier.contains_mask(mask) {
            Some(Piece::attacker(Soldier))
        } else if self.defending_soldier.contains_mask(mask) {
            Some(Piece::defender(Soldier))
        } else if self.king.contains_mask(mask) {
            Some(Piece::king())
        } else {
            None
        }
    }

    fn contains_piece(&self, t: Tile, piece: Piece) -> bool {
        let ts = match piece {
            Piece {
                side: Attacker,
                piece_type: Soldier,
            } => self.attacking_soldier,
            Piece {
                side: Defender,
                piece_type: Soldier,
            } => self.defending_soldier,
            Piece {
                side: Defender,
                piece_type: King,
            } => self.king,
            _ => return false,
        };
        ts.contains(t)
    }

    fn set(&mut self, t: Tile, piece: Piece) -> Result<(), PieceMapError> {
        match piece {
            Piece {
                piece_type: Soldier,
                side: Attacker,
            } => {
                self.attacking_soldier.insert(t);
                self.defending_soldier.remove(t);
                self.king.remove(t);
            }
            Piece {
                piece_type: Soldier,
                side: Defender,
            } => {
                self.defending_soldier.insert(t);
                self.attacking_soldier.remove(t);
                self.king.remove(t);
            }
            Piece {
                piece_type: King,
                side: Defender,
            } => {
                self.king.insert(t);
                self.attacking_soldier.remove(t);
                self.defending_soldier.remove(t);
            }
            other => return Err(PieceMapError::UnsupportedPiece(other)),
        }
        Ok(())
    }

    fn remove(&mut self, t: Tile) {
        self.attacking_soldier.remove(t);
        self.defending_soldier.remove(t);
        self.king.remove(t);
    }

    fn clear_tiles(&mut self, tiles: TileSet<B>) {
        let inv = !tiles;
        self.attacking_soldier &= inv;
        self.defending_soldier &= inv;
        self.king &= inv;
    }

    fn without_pieces(&self, piece_set: PieceSet) -> Self {
        let mut w = Self::default();
        if piece_set.contains(Piece::attacker(Soldier)) {
            w.attacking_soldier = self.attacking_soldier;
        }
        if piece_set.contains(Piece::defender(Soldier)) {
            w.defending_soldier = self.defending_soldier;
        }
        if piece_set.contains(Piece::king()) {
            w.king = self.king;
        }
        w
    }

    fn extend(&mut self, other: &Self) {
        self.attacking_soldier.extend(&other.attacking_soldier);
        self.defending_soldier.extend(&other.defending_soldier);
        self.king.extend(&other.king);
    }

    fn is_empty(&self) -> bool {
        self.attacking_soldier.is_empty()
            && self.defending_soldier.is_empty()
            && self.king.is_empty()
    }

    fn iter_tiles_for_piece(&self, piece: Piece) -> BitfieldTileIter<Self::BitField> {
        match piece {
            Piece { side: Attacker, piece_type: Soldier } => self.attacking_soldier.into_iter(),
            Piece { side: Defender, piece_type: Soldier } => self.defending_soldier.into_iter(),
            Piece { side: Defender, piece_type: King } => self.king.into_iter(),
            _ => panic!("Invalid piece type: {:?}", piece),
        }
    }

    fn next_piece(piece: Option<Piece>) -> Option<Piece> {
        match piece {
            None => Some(Piece::attacker(Soldier)),
            Some(Piece { side: Attacker, piece_type: Soldier }) => Some(Piece::defender(Soldier)),
            Some(Piece { side: Defender, piece_type: Soldier }) => Some(Piece::king()),
            Some(Piece { side: Defender, piece_type: King }) => None,
            other => panic!("Invalid piece type: {:?}", other),
        }
    }

    fn find_king(&self) -> Option<Tile> {
        self.king.first()
    }

    fn count_pieces_of_side(&self, side: Side) -> u32 {
        match side {
            Attacker => self.attacking_soldier.count(),
            Defender => self.defending_soldier.count() + self.king.count(),
        }
    }

    fn occupied_by_side(&self, side: Side) -> TileSet<Self::BitField> {
        match side {
            Attacker => self.attacking_soldier,
            Defender => self.defending_soldier | self.king,
        }
    }
}

impl<B: BitField> IntoIterator for BasicPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = PieceMapIterator<BasicPieceMap<B>>;
    fn into_iter(self) -> Self::IntoIter {
        PieceMapIterator::new(self)
    }
}

impl<B: BitField> IntoIterator for &BasicPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = PieceMapIterator<BasicPieceMap<B>>;
    fn into_iter(self) -> Self::IntoIter {
        PieceMapIterator::new(*self)
    }
}

impl<B: BitField> FromIterator<PlacedPiece> for BasicPieceMap<B> {
    fn from_iter<T: IntoIterator<Item = PlacedPiece>>(iter: T) -> Self {
        let mut tmp = Self::default();
        for p in iter {
            tmp.set_placed_piece(p);
        }
        tmp
    }
}

/// Create a new [`BasicPieceMap`] with the given placed pieces. The first argument is the
/// [`BitField`] implementation to use.
#[macro_export]
macro_rules! basic_piecemap {
    ($t:ty, $($piece:expr),*) => {
        {
            use $crate::collections::piecemap::PieceMap;
            let mut tmp = $crate::collections::piecemap::BasicPieceMap::<$t>::default();
            $(
                tmp.set_placed_piece($piece);
            )*
            tmp
        }
    };
}


/// A [`PieceMap`] implemented using bitfields which is capable of representing all supported pieces
/// (attacking and defending soldiers, knights, commanders, guards and mercenaries, and defending
/// king).
#[derive(Copy, Clone, Hash, Eq, PartialEq, Default, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(bound = "B: serde::Serialize + serde::de::DeserializeOwned"),
)]
pub struct BerserkPieceMap<B: BitField> {
    pub attacking_soldier: TileSet<B>,
    pub defending_soldier: TileSet<B>,
    pub commander: TileSet<B>,
    pub knight: TileSet<B>,
    pub king: TileSet<B>,

}

impl<B: BitField> CommonPieceMapSuperTraits for BerserkPieceMap<B> {}

#[cfg(feature = "serde")]
impl<B: BitField> PieceMapSuperTraits for BerserkPieceMap<B> where B: Serialize + DeserializeOwned {}

#[cfg(not(feature = "serde"))]
impl<B: BitField> PieceMapSuperTraits for BerserkPieceMap<B> {}

impl<B: BitField> PieceMap for BerserkPieceMap<B> {
    type BitField = B;

    fn occupied(&self) -> TileSet<B> {
        self.attacking_soldier | self.defending_soldier | self.commander | self.knight | self.king
    }

    fn occupied_by(&self, piece_set: PieceSet) -> TileSet<B> {
        let mut ts = TileSet::empty();
        if piece_set.contains(Piece::attacker(Soldier)) {
            ts.extend(&self.attacking_soldier);
        }
        if piece_set.contains(Piece::defender(Soldier)) {
            ts.extend(&self.defending_soldier);
        }
        if piece_set.contains(Piece::attacker(Commander)) {
            ts.extend(&self.commander);
        }
        if piece_set.contains(Piece::defender(Knight)) {
            ts.extend(&self.knight);
        }
        if piece_set.contains(Piece::king()) {
            ts.extend(&self.king);
        }
        ts
    }

    fn get(&self, t: Tile) -> Option<Piece> {
        let mask = B::tile_mask(t);
        if self.attacking_soldier.contains_mask(mask) {
            Some(Piece::attacker(Soldier))
        } else if self.defending_soldier.contains_mask(mask) {
            Some(Piece::defender(Soldier))
        } else if self.commander.contains_mask(mask) {
            Some(Piece::attacker(Commander))
        } else if self.knight.contains_mask(mask) {
            Some(Piece::defender(Knight))
        } else if self.king.contains_mask(mask) {
            Some(Piece::king())
        } else {
            None
        }
    }

    fn contains_piece(&self, t: Tile, piece: Piece) -> bool {
        let ts = match piece {
            Piece {
                side: Attacker,
                piece_type: Soldier,
            } => self.attacking_soldier,
            Piece {
                side: Defender,
                piece_type: Soldier,
            } => self.defending_soldier,
            Piece {
                side: Attacker,
                piece_type: Commander,
            } => self.commander,
            Piece {
                side: Defender,
                piece_type: Knight,
            } => self.knight,
            Piece {
                side: Defender,
                piece_type: King,
            } => self.king,
            _ => return false,
        };
        ts.contains(t)
    }

    fn set(&mut self, t: Tile, piece: Piece) -> Result<(), PieceMapError> {
        match piece {
            Piece {
                piece_type: Soldier,
                side: Attacker,
            } => {
                self.attacking_soldier.insert(t);
                self.defending_soldier.remove(t);
                self.commander.remove(t);
                self.knight.remove(t);
                self.king.remove(t);
            }
            Piece {
                piece_type: Soldier,
                side: Defender,
            } => {
                self.defending_soldier.insert(t);
                self.attacking_soldier.remove(t);
                self.commander.remove(t);
                self.knight.remove(t);
                self.king.remove(t);
            },
            Piece {
                piece_type: Commander,
                side: Attacker,
            } => {
                self.defending_soldier.remove(t);
                self.attacking_soldier.remove(t);
                self.commander.insert(t);
                self.knight.remove(t);
                self.king.remove(t);
            },
            Piece {
                piece_type: Knight,
                side: Defender,
            } => {
                self.defending_soldier.remove(t);
                self.attacking_soldier.remove(t);
                self.commander.remove(t);
                self.knight.insert(t);
                self.king.remove(t);
            },
            Piece {
                piece_type: King,
                side: Defender,
            } => {
                self.attacking_soldier.remove(t);
                self.defending_soldier.remove(t);
                self.commander.remove(t);
                self.knight.remove(t);
                self.king.insert(t);
            }
            other => return Err(PieceMapError::UnsupportedPiece(other)),
        }
        Ok(())
    }

    fn remove(&mut self, t: Tile) {
        self.attacking_soldier.remove(t);
        self.defending_soldier.remove(t);
        self.commander.remove(t);
        self.knight.remove(t);
        self.king.remove(t);
    }

    fn clear_tiles(&mut self, tiles: TileSet<B>) {
        let inv = !tiles;
        self.attacking_soldier &= inv;
        self.defending_soldier &= inv;
        self.commander &= inv;
        self.knight &= inv;
        self.king &= inv;
    }

    fn without_pieces(&self, piece_set: PieceSet) -> Self {
        let mut w = Self::default();
        if piece_set.contains(Piece::attacker(Soldier)) {
            w.attacking_soldier = self.attacking_soldier;
        }
        if piece_set.contains(Piece::defender(Soldier)) {
            w.defending_soldier = self.defending_soldier;
        }
        if piece_set.contains(Piece::attacker(Commander)) {
            w.commander = self.commander;
        }
        if piece_set.contains(Piece::defender(Knight)) {
            w.knight = self.knight;
        }
        if piece_set.contains(Piece::king()) {
            w.king = self.king;
        }
        w
    }

    fn extend(&mut self, other: &Self) {
        self.attacking_soldier.extend(&other.attacking_soldier);
        self.defending_soldier.extend(&other.defending_soldier);
        self.commander.extend(&other.commander);
        self.knight.extend(&other.knight);
        self.king.extend(&other.king);
    }

    fn is_empty(&self) -> bool {
        self.attacking_soldier.is_empty()
            && self.defending_soldier.is_empty()
            && self.commander.is_empty()
            && self.knight.is_empty()
            && self.king.is_empty()
    }

    fn iter_tiles_for_piece(&self, piece: Piece) -> BitfieldTileIter<Self::BitField> {
        match piece {
            Piece { side: Attacker, piece_type: Soldier } => self.attacking_soldier.into_iter(),
            Piece { side: Defender, piece_type: Soldier } => self.defending_soldier.into_iter(),
            Piece { side: Attacker, piece_type: Commander } => self.commander.into_iter(),
            Piece { side: Defender, piece_type: Knight } => self.knight.into_iter(),
            Piece { side: Defender, piece_type: King } => self.king.into_iter(),
            _ => panic!("Invalid piece type: {:?}", piece),
        }
    }

    fn next_piece(piece: Option<Piece>) -> Option<Piece> {
        match piece {
            None => Some(Piece::attacker(Soldier)),
            Some(Piece { side: Attacker, piece_type: Soldier }) => Some(Piece::defender(Soldier)),
            Some(Piece { side: Defender, piece_type: Soldier }) => Some(Piece::king()),
            Some(Piece { side: Defender, piece_type: King }) => Some(Piece::attacker(Commander)),
            Some(Piece { side: Attacker, piece_type: Commander }) => Some(Piece::defender(Knight)),
            Some(Piece { side: Defender, piece_type: Knight }) => None,
            other => panic!("Invalid piece type: {:?}", other),
        }
    }

    fn find_king(&self) -> Option<Tile> {
        self.king.first()
    }

    fn count_pieces_of_side(&self, side: Side) -> u32 {
        match side {
            Attacker => self.attacking_soldier.count() + self.commander.count(),
            Defender => self.defending_soldier.count() + self.knight.count() + self.king.count(),
        }
    }

    fn occupied_by_side(&self, side: Side) -> TileSet<Self::BitField> {
        match side {
            Attacker => self.attacking_soldier | self.commander,
            Defender => self.defending_soldier | self.knight | self.king,
        }
    }

}

impl<B: BitField> IntoIterator for BerserkPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = PieceMapIterator<BerserkPieceMap<B>>;
    fn into_iter(self) -> Self::IntoIter {
        PieceMapIterator::new(self)
    }
}

impl<B: BitField> IntoIterator for &BerserkPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = PieceMapIterator<BerserkPieceMap<B>>;
    fn into_iter(self) -> Self::IntoIter {
        PieceMapIterator::new(*self)
    }
}

impl<B: BitField> FromIterator<PlacedPiece> for BerserkPieceMap<B> {
    fn from_iter<T: IntoIterator<Item = PlacedPiece>>(iter: T) -> Self {
        let mut tmp = Self::default();
        for p in iter {
            tmp.set_placed_piece(p);
        }
        tmp
    }
}

pub struct PieceMapIterator<P: PieceMap> {
    piece_map: P,
    current_iter: BitfieldTileIter<P::BitField>,
    current_piece: Piece,
}

impl<P: PieceMap> PieceMapIterator<P> {
    pub fn new(piece_map: P) -> Self {
        let p = P::next_piece(None).expect("`PieceMap` must support at least one piece.");
        Self {
            piece_map,
            current_iter: piece_map.iter_tiles_for_piece(p),
            current_piece: Piece::attacker(Soldier),
        }
    }
}

impl<P: PieceMap> Iterator for PieceMapIterator<P> {
    type Item = PlacedPiece;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(tile) = self.current_iter.next() {
                return Some(PlacedPiece {
                    tile,
                    piece: self.current_piece,
                });
            } else {
                self.current_piece = P::next_piece(Some(self.current_piece))?;
                self.current_iter = self.piece_map.iter_tiles_for_piece(self.current_piece)
            }
        }
    }
}