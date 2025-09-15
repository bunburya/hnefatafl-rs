//! This module contains some concrete implementations of various generic structs for ease of use.
//! The general naming convention used is:
//!
//! * `Small*`: Suitable for use on boards up to 7x7.
//! * `Medium*`: Suitable for use on boards up to 11x11.
//! * `Large*`: Suitable for use on boards up to 15x15.
//! * `Huge*`: Suitable for use on boards up to 21x21.
//!
//! `Basic` in the name means that it supports the basic tafl pieces, namely, attacking soldiers,
//! defending soldiers and defending king (no knights, commanders, etc.).

use crate::board::state::BasicBoardState;
use crate::game::Game;
use crate::play::PlayRecord;
use primitive_types::{U256, U512};
use crate::game::state::GameState;

pub type SmallBitfield = u64;
pub type MediumBitfield = u128;
pub type LargeBitfield = U256;
pub type HugeBitfield = U512;

pub type SmallPlayRecord = PlayRecord<SmallBitfield>;
pub type MediumPlayRecord = PlayRecord<MediumBitfield>;
pub type LargePlayRecord = PlayRecord<LargeBitfield>;
pub type HugePlayRecord = PlayRecord<HugeBitfield>;

pub type SmallBasicBoardState = BasicBoardState<SmallBitfield>;
pub type MediumBasicBoardState = BasicBoardState<MediumBitfield>;
pub type LargeBasicBoardState = BasicBoardState<LargeBitfield>;
pub type HugeBasicBoardState = BasicBoardState<HugeBitfield>;

pub type SmallBasicGameState = GameState<SmallBasicBoardState>;
pub type MediumBasicGameState = GameState<MediumBasicBoardState>;
pub type LargeBasicGameState = GameState<LargeBasicBoardState>;
pub type HugeBasicGameState = GameState<HugeBasicBoardState>;


pub type SmallBasicGame = Game<SmallBasicBoardState>;
pub type MediumBasicGame = Game<MediumBasicBoardState>;
pub type LargeBasicGame = Game<LargeBasicBoardState>;
pub type HugeBasicGame = Game<HugeBasicBoardState>;