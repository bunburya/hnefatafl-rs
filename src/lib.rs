extern crate core;

pub mod rules;
pub mod pieces;
//mod board;
pub mod error;
pub mod game;
pub mod tiles;
pub mod bitfield;
pub mod utils;
pub mod board_state;
pub mod play;
pub mod preset;
pub mod game_state;
pub mod game_logic;
pub mod board_geo;
pub mod play_iter;
pub mod tile_iter;

pub use crate::{
    game::Game,
    game_state::GameState,
    game_logic::GameLogic,
    rules::Ruleset
};