mod rules;
mod pieces;
mod board;
mod error;
pub mod game;
mod tiles;
mod bitfield;
mod utils;
mod board_state;
pub mod play;
pub mod preset;

pub use crate::{
    game::{
        Game,
        GameStatus,
        GameOutcome,
        PlayOutcome,
    },
    board::{
        Board,
        SmallBoard,
        MediumBoard,
    },
    board_state::{
        BitfieldBoardState,
        SmallBoardState,
        MediumBoardState,
    },
    error::{
        ParseError,
        PlayError,
        InvalidPlay
    },
    pieces::{
        Piece,
        PieceSet,
        PieceType,
        Side
    },
    rules::{
        Ruleset,
        KingStrength,
        ThroneRule,
        HostilityRules
    },
    tiles::{
        Coords,
        Tile,
        Axis
    },
    play::Play,
    bitfield::BitField,
};