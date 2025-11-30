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

use crate::board::state::BoardState;
use crate::game::logic::GameLogic;
use crate::game::state::GameState;
use crate::game::Game;
use crate::play::PlayRecord;
use primitive_types::{U256, U512};
use crate::collections::BasicPieceMap;
use crate::collections::piecemap::BerserkPieceMap;

pub type SmallBitfield = u64;
pub type MediumBitfield = u128;
pub type LargeBitfield = U256;
pub type HugeBitfield = U512;

pub type SmallBasicPieceMap = BasicPieceMap<SmallBitfield>;
pub type MediumBasicPieceMap = BasicPieceMap<MediumBitfield>;
pub type LargeBasicPieceMap = BasicPieceMap<LargeBitfield>;
pub type HugeBasicPieceMap = BasicPieceMap<HugeBitfield>;

pub type SmallBasicBoardState = BoardState<SmallBasicPieceMap>;
pub type MediumBasicBoardState = BoardState<MediumBasicPieceMap>;
pub type LargeBasicBoardState = BoardState<LargeBasicPieceMap>;
pub type HugeBasicBoardState = BoardState<HugeBasicPieceMap>;

pub type SmallBasicPlayRecord = PlayRecord<SmallBasicPieceMap>;
pub type MediumBasicPlayRecord = PlayRecord<MediumBasicPieceMap>;
pub type LargeBasicPlayRecord = PlayRecord<LargeBasicPieceMap>;
pub type HugeBasicPlayRecord = PlayRecord<HugeBasicPieceMap>;

pub type SmallBasicGameState = GameState<SmallBasicPieceMap>;
pub type MediumBasicGameState = GameState<MediumBasicPieceMap>;
pub type LargeBasicGameState = GameState<LargeBasicPieceMap>;
pub type HugeBasicGameState = GameState<HugeBasicPieceMap>;

pub type SmallBasicGameLogic = GameLogic<SmallBasicPieceMap>;
pub type MediumBasicGameLogic = GameLogic<MediumBasicPieceMap>;
pub type LargeBasicGameLogic = GameLogic<LargeBasicPieceMap>;
pub type HugeBasicGameLogic = GameLogic<HugeBasicPieceMap>;

pub type SmallBasicGame = Game<SmallBasicPieceMap>;
pub type MediumBasicGame = Game<MediumBasicPieceMap>;
pub type LargeBasicGame = Game<LargeBasicPieceMap>;
pub type HugeBasicGame = Game<HugeBasicPieceMap>;

pub type SmallBerserkPieceMap = BerserkPieceMap<SmallBitfield>;
pub type MediumBerserkPieceMap = BerserkPieceMap<MediumBitfield>;
pub type LargeBerserkPieceMap = BerserkPieceMap<LargeBitfield>;
pub type HugeBerserkPieceMap = BerserkPieceMap<HugeBitfield>;

pub type SmallBerserkBoardState = BoardState<SmallBerserkPieceMap>;
pub type MediumBerserkBoardState = BoardState<MediumBerserkPieceMap>;
pub type LargeBerserkBoardState = BoardState<LargeBerserkPieceMap>;
pub type HugeBerserkBoardState = BoardState<HugeBerserkPieceMap>;

pub type SmallBerserkPlayRecord = PlayRecord<SmallBerserkPieceMap>;
pub type MediumBerserkPlayRecord = PlayRecord<MediumBerserkPieceMap>;
pub type LargeBerserkPlayRecord = PlayRecord<LargeBerserkPieceMap>;
pub type HugeBerserkPlayRecord = PlayRecord<HugeBerserkPieceMap>;

pub type SmallBerserkGameState = GameState<SmallBerserkPieceMap>;
pub type MediumBerserkGameState = GameState<MediumBerserkPieceMap>;
pub type LargeBerserkGameState = GameState<LargeBerserkPieceMap>;
pub type HugeBerserkGameState = GameState<HugeBerserkPieceMap>;

pub type SmallBerserkGameLogic = GameLogic<SmallBerserkPieceMap>;
pub type MediumBerserkGameLogic = GameLogic<MediumBerserkPieceMap>;
pub type LargeBerserkGameLogic = GameLogic<LargeBerserkPieceMap>;
pub type HugeBerserkGameLogic = GameLogic<HugeBerserkPieceMap>;

pub type SmallBerserkGame = Game<SmallBerserkPieceMap>;
pub type MediumBerserkGame = Game<MediumBerserkPieceMap>;
pub type LargeBerserkGame = Game<LargeBerserkPieceMap>;
pub type HugeBerserkGame = Game<HugeBerserkPieceMap>;
