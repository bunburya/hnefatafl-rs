use crate::bitfield::BitField;
use crate::collections::tileset::{BitfieldTileIter, TileSet};
use crate::pieces::PieceType::{King, Soldier, Knight, Commander};
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

// --- PieceMap generation macro -------------------------------------------------------------------
//
// The concrete `PieceMap` implementations are structurally identical: one `TileSet` "plane" per
// supported (side, piece type) combination, with every trait method being a fixed fold over those
// planes. Rather than hand-write (and hand-synchronise) that boilerplate for each variant, the
// `define_piecemap!` macro generates a specialised struct and its full `PieceMap` impl from a list
// of `field => (Side, PieceType)` entries. Each generated map still allocates only the planes it
// declares (so the memory/copy profile is identical to a hand-written struct), but the per-plane
// bookkeeping is defined exactly once here.

/// Internal helper: yields `self.$field.first()` for the king plane, `None` otherwise. Used to give
/// `find_king` O(1) direct access to the king plane without the caller having to know which field
/// it is.
macro_rules! __pm_king_tile {
    ($self:ident, $field:ident, King) => { $self.$field.first() };
    ($self:ident, $field:ident, $other:ident) => { None::<Tile> };
}

/// Internal helper: yields `self.$field.count()` when the plane's side matches the queried side,
/// else `0`.
macro_rules! __pm_side_count {
    ($self:ident, $field:ident, Attacker, Attacker) => { $self.$field.count() };
    ($self:ident, $field:ident, Defender, Defender) => { $self.$field.count() };
    ($self:ident, $field:ident, $plane:ident, $queried:ident) => { 0u32 };
}

/// Internal helper: yields `self.$field` (a `TileSet`) when the plane's side matches the queried
/// side, else an empty `TileSet`.
macro_rules! __pm_side_tiles {
    ($self:ident, $field:ident, Attacker, Attacker) => { $self.$field };
    ($self:ident, $field:ident, Defender, Defender) => { $self.$field };
    ($self:ident, $field:ident, $plane:ident, $queried:ident) => { TileSet::empty() };
}

/// Generate a specialised [`PieceMap`] struct and its full trait implementation from a list of
/// `field => (Side, PieceType)` planes. See the module-level note above for the rationale.
macro_rules! define_piecemap {
    (
        $(#[$meta:meta])*
        $name:ident { $( $field:ident => ($side:ident, $ptype:ident) ),* $(,)? }
    ) => {
        $(#[$meta])*
        #[derive(Copy, Clone, Hash, Eq, PartialEq, Default, Debug)]
        #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
        #[cfg_attr(
            feature = "serde",
            serde(bound = "B: serde::Serialize + serde::de::DeserializeOwned")
        )]
        pub struct $name<B: BitField> {
            $( pub $field: TileSet<B>, )*
        }

        impl<B: BitField> CommonPieceMapSuperTraits for $name<B> {}

        #[cfg(feature = "serde")]
        impl<B: BitField> PieceMapSuperTraits for $name<B> where B: Serialize + DeserializeOwned {}

        #[cfg(not(feature = "serde"))]
        impl<B: BitField> PieceMapSuperTraits for $name<B> {}

        impl<B: BitField> PieceMap for $name<B> {
            type BitField = B;

            fn occupied(&self) -> TileSet<B> {
                TileSet::empty() $( | self.$field )*
            }

            fn occupied_by(&self, piece_set: PieceSet) -> TileSet<B> {
                let mut ts = TileSet::empty();
                $(
                    if piece_set.contains(Piece::new($ptype, $side)) {
                        ts.extend(&self.$field);
                    }
                )*
                ts
            }

            fn get(&self, t: Tile) -> Option<Piece> {
                let mask = B::tile_mask(t);
                $(
                    if self.$field.contains_mask(mask) {
                        return Some(Piece::new($ptype, $side));
                    }
                )*
                None
            }

            fn contains_piece(&self, t: Tile, piece: Piece) -> bool {
                let ts = match piece {
                    $( Piece { side: $side, piece_type: $ptype } => self.$field, )*
                    _ => return false,
                };
                ts.contains(t)
            }

            fn set(&mut self, t: Tile, piece: Piece) -> Result<(), PieceMapError> {
                match piece {
                    $(
                        Piece { side: $side, piece_type: $ptype } => {
                            // Clear every plane at `t` first, then set the matching one, so a tile
                            // is never simultaneously occupied by two pieces.
                            self.remove(t);
                            self.$field.insert(t);
                        }
                    )*
                    other => return Err(PieceMapError::UnsupportedPiece(other)),
                }
                Ok(())
            }

            fn remove(&mut self, t: Tile) {
                $( self.$field.remove(t); )*
            }

            fn clear_tiles(&mut self, tiles: TileSet<B>) {
                let inv = !tiles;
                $( self.$field &= inv; )*
            }

            fn without_pieces(&self, piece_set: PieceSet) -> Self {
                let mut w = Self::default();
                $(
                    if piece_set.contains(Piece::new($ptype, $side)) {
                        w.$field = self.$field;
                    }
                )*
                w
            }

            fn extend(&mut self, other: &Self) {
                $( self.$field.extend(&other.$field); )*
            }

            fn is_empty(&self) -> bool {
                true $( && self.$field.is_empty() )*
            }

            fn iter_tiles_for_piece(&self, piece: Piece) -> BitfieldTileIter<Self::BitField> {
                match piece {
                    $( Piece { side: $side, piece_type: $ptype } => self.$field.into_iter(), )*
                    _ => panic!("Invalid piece type: {:?}", piece),
                }
            }

            fn next_piece(piece: Option<Piece>) -> Option<Piece> {
                // Planes in declaration order; iteration walks this sequence.
                const ORDER: &[Piece] = &[ $( Piece::new($ptype, $side) ),* ];
                match piece {
                    None => ORDER.first().copied(),
                    Some(p) => match ORDER.iter().position(|x| *x == p) {
                        Some(i) => ORDER.get(i + 1).copied(),
                        None => panic!("Invalid piece type: {:?}", p),
                    },
                }
            }

            fn find_king(&self) -> Option<Tile> {
                None::<Tile> $( .or_else(|| __pm_king_tile!(self, $field, $ptype)) )*
            }

            fn count_pieces_of_side(&self, side: Side) -> u32 {
                match side {
                    Attacker => 0u32 $( + __pm_side_count!(self, $field, $side, Attacker) )*,
                    Defender => 0u32 $( + __pm_side_count!(self, $field, $side, Defender) )*,
                }
            }

            fn occupied_by_side(&self, side: Side) -> TileSet<Self::BitField> {
                match side {
                    Attacker => TileSet::empty() $( | __pm_side_tiles!(self, $field, $side, Attacker) )*,
                    Defender => TileSet::empty() $( | __pm_side_tiles!(self, $field, $side, Defender) )*,
                }
            }
        }

        impl<B: BitField> IntoIterator for $name<B> {
            type Item = PlacedPiece;
            type IntoIter = PieceMapIterator<$name<B>>;
            fn into_iter(self) -> Self::IntoIter {
                PieceMapIterator::new(self)
            }
        }

        impl<B: BitField> IntoIterator for &$name<B> {
            type Item = PlacedPiece;
            type IntoIter = PieceMapIterator<$name<B>>;
            fn into_iter(self) -> Self::IntoIter {
                PieceMapIterator::new(*self)
            }
        }

        impl<B: BitField> FromIterator<PlacedPiece> for $name<B> {
            fn from_iter<T: IntoIterator<Item = PlacedPiece>>(iter: T) -> Self {
                let mut tmp = Self::default();
                for p in iter {
                    tmp.set_placed_piece(p);
                }
                tmp
            }
        }
    };
}

define_piecemap! {
    /// A [`PieceMap`] implemented using bitfields which is capable of representing the basic pieces
    /// (attacking and defending soldiers, and defending king).
    BasicPieceMap {
        attacking_soldier => (Attacker, Soldier),
        defending_soldier => (Defender, Soldier),
        king              => (Defender, King),
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

define_piecemap! {
    /// A [`PieceMap`] implemented using bitfields which is capable of representing the pieces used in
    /// berserk variants: attacking and defending soldiers, an attacking commander, a defending
    /// knight, and the defending king.
    BerserkPieceMap {
        attacking_soldier => (Attacker, Soldier),
        defending_soldier => (Defender, Soldier),
        commander         => (Attacker, Commander),
        knight            => (Defender, Knight),
        king              => (Defender, King),
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
            current_piece: p,
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
