use crate::bitfield::BitField;
use crate::collections::tileset::{BitfieldTileIter, TileSet};
use crate::pieces::PieceType::{King, Soldier};
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, PlacedPiece, Side};
use crate::tiles::Tile;
use std::fmt::Debug;
use std::hash::Hash;

use crate::collections::pieceset::PieceSet;
use crate::error::ParseError;
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
pub trait PieceMapSuperTraits: CommonPieceMapSuperTraits + Serialize + DeserializeOwned {}

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
    fn contains_any(&self, t: Tile) -> bool;

    /// Insert the given piece at the given tile.
    fn set(&mut self, t: Tile, piece: Piece);

    /// Insert the given piece at the relevant tile.
    fn set_placed_piece(&mut self, p: PlacedPiece);

    /// Clear the given tile.
    fn remove(&mut self, t: Tile);

    /// Clear all tiles in the given set.
    fn remove_tiles(&mut self, tiles: TileSet<Self::BitField>);

    /// Clear tiles occupied by any pieces in the given set.
    fn remove_pieces(&mut self, piece_set: PieceSet);

    /// Return a copy of this map with all pieces in the given set removed.
    fn without_pieces(&self, piece_set: PieceSet) -> Self;

    /// Extend this map with the pieces in the given map.
    fn extend(&mut self, other: &Self);

    /// Check whether this map is empty.
    fn is_empty(&self) -> bool;

    /// Parse board state from (the relevant part of) a string in FEN format.
    fn from_fen(s: &str) -> Result<(Self, u8), ParseError>;

    /// Return a string in FEN format representing the board state.
    fn to_fen(&self, side_len: u8) -> String;
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

    fn contains_any(&self, t: Tile) -> bool {
        self.occupied().contains(t)
    }

    fn set(&mut self, t: Tile, piece: Piece) {
        match piece {
            Piece {
                piece_type: Soldier,
                side: Side::Attacker,
            } => {
                self.attacking_soldier.insert(t);
                self.defending_soldier.remove(t);
                self.king.remove(t);
            }
            Piece {
                piece_type: Soldier,
                side: Side::Defender,
            } => {
                self.defending_soldier.insert(t);
                self.attacking_soldier.remove(t);
                self.king.remove(t);
            }
            Piece {
                piece_type: King,
                side: Side::Defender,
            } => {
                self.king.insert(t);
                self.attacking_soldier.remove(t);
                self.defending_soldier.remove(t);
            }
            other => panic!("Invalid piece type: {:?}.", other),
        }
    }

    fn set_placed_piece(&mut self, p: PlacedPiece) {
        self.set(p.tile, p.piece);
    }

    fn remove(&mut self, t: Tile) {
        self.attacking_soldier.remove(t);
        self.defending_soldier.remove(t);
        self.king.remove(t);
    }

    fn remove_tiles(&mut self, tiles: TileSet<B>) {
        let inv = !tiles;
        self.attacking_soldier &= inv;
        self.defending_soldier &= inv;
        self.king &= inv;
    }

    fn remove_pieces(&mut self, piece_set: PieceSet) {
        if !piece_set.contains(Piece::attacker(Soldier)) {
            self.attacking_soldier.clear();
        }
        if !piece_set.contains(Piece::defender(Soldier)) {
            self.defending_soldier.clear();
        }
        if !piece_set.contains(Piece::king()) {
            self.king.clear();
        }
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
}

pub struct BasicPieceMapIterator<B: BitField> {
    piece_map: BasicPieceMap<B>,
    current_iter: BitfieldTileIter<B>,
    current_piece: Piece,
}

impl<B: BitField> BasicPieceMapIterator<B> {
    pub fn new(piece_map: BasicPieceMap<B>) -> Self {
        Self {
            piece_map,
            current_iter: piece_map.attacking_soldier.into_iter(),
            current_piece: Piece::attacker(Soldier),
        }
    }

    fn next_piece(&self) -> Option<Piece> {
        match self.current_piece {
            Piece {
                side: Attacker,
                piece_type: Soldier,
            } => Some(Piece::defender(Soldier)),
            Piece {
                side: Defender,
                piece_type: Soldier,
            } => Some(Piece::king()),
            Piece {
                side: Defender,
                piece_type: King,
            } => None,
            other => panic!("Invalid piece type: {:?}", other),
        }
    }

    fn get_iter(&self) -> BitfieldTileIter<B> {
        match self.current_piece {
            Piece {
                side: Attacker,
                piece_type: Soldier,
            } => self.piece_map.attacking_soldier.into_iter(),
            Piece {
                side: Defender,
                piece_type: Soldier,
            } => self.piece_map.defending_soldier.into_iter(),
            Piece {
                side: Defender,
                piece_type: King,
            } => self.piece_map.king.into_iter(),
            _ => panic!("Invalid piece type: {:?}", self.current_piece),
        }
    }
}

impl<B: BitField> Iterator for BasicPieceMapIterator<B> {
    type Item = PlacedPiece;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(tile) = self.current_iter.next() {
                return Some(PlacedPiece {
                    tile,
                    piece: self.current_piece,
                });
            } else {
                self.current_piece = self.next_piece()?;
                self.current_iter = self.get_iter()
            }
        }
    }
}

impl<B: BitField> IntoIterator for BasicPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = BasicPieceMapIterator<B>;
    fn into_iter(self) -> Self::IntoIter {
        BasicPieceMapIterator::new(self)
    }
}

impl<B: BitField> IntoIterator for &BasicPieceMap<B> {
    type Item = PlacedPiece;
    type IntoIter = BasicPieceMapIterator<B>;
    fn into_iter(self) -> Self::IntoIter {
        BasicPieceMapIterator::new(*self)
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
