mod rules;
mod pieces;
//mod board;
pub mod error;
pub mod game;
mod tiles;
mod bitfield;
mod utils;
pub mod board_state;
pub mod play;
pub mod preset;
pub mod game_state;
pub mod game_logic;
pub mod board_geo;
mod play_iter;
mod tile_iter;

pub use crate::{
    game::{
        Game,
        GameStatus,
        GameOutcome,
        PlayOutcome,
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