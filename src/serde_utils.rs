#![cfg(feature = "serde")]

use std::str::FromStr;
use crate::bitfield::BitField;
use crate::board::state::{BitfieldBasicBoardState, BoardState};
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use crate::play::Play;

// Custom `Serialize`/`Deserialize` implementation for board state (using FEN notation strings)

#[derive(Default)]
pub(crate) struct FenVisitor<B: BoardState>(std::marker::PhantomData<B>);

impl<'de, B: BoardState> Visitor<'de> for FenVisitor<B> {
    type Value = B;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string representing the board state in FEN format")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where E: serde::de::Error {
        if let Ok(b) = Self::Value::from_fen(value) {
            Ok(b)
        } else {
            Err(E::invalid_value(
                serde::de::Unexpected::Str(value),
                &self
            ))
        }
    }
}

impl <T: BitField> Serialize for BitfieldBasicBoardState<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.to_fen().as_str())
    }
}

impl <'de, T: BitField> Deserialize<'de> for BitfieldBasicBoardState<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(FenVisitor::default())
    }
}

// Custom `Serialize`/`Deserialize` implementation for single play

impl Serialize for Play {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.to_string().as_str())
    }
}

struct PlayVisitor;

impl<'de> Visitor<'de> for PlayVisitor {
    type Value = Play;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a string representing the play, eg, 'a3-c3'")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where E: serde::de::Error {
        if let Ok(b) = Self::Value::from_str(value) {
            Ok(b)
        } else {
            Err(E::invalid_value(
                serde::de::Unexpected::Str(value),
                &self
            ))
        }
    }
}

impl<'de> Deserialize<'de> for Play {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(PlayVisitor)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use crate::game::{Game, MediumBasicGame};
    use crate::preset::{rules, boards};
    use bincode;
    use bincode::serde::{encode_to_vec, decode_from_slice};
    use crate::play::Play;

    #[test]
    fn test_round_trip() {
        let mut g: MediumBasicGame = Game::new(rules::COPENHAGEN, boards::COPENHAGEN)
            .expect("failed to create game");
        let cfg = bincode::config::standard();
        let bytes = encode_to_vec(&g, cfg).unwrap();
        let (back, _len) = decode_from_slice(&bytes, cfg).unwrap();
        assert_eq!(g, back);
        g.do_play(Play::from_str("h11-h7").expect("bad play")).expect("failed to do play");
        g.do_play(Play::from_str("f8-h8").expect("bad play")).expect("failed to do play");
        let bytes = encode_to_vec(&g, cfg).unwrap();
        let (back, _len) = decode_from_slice(&bytes, cfg).unwrap();
        assert_eq!(g, back);
    }
}