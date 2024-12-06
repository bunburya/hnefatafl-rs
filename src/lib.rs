extern crate core;

/// Miscellaneous utilities used elsewhere in the crate.
#[macro_use]
mod utils;

/// Code for defining game rules.
pub mod rules;

/// Code relating to game pieces.
pub mod pieces;

/// Errors used elsewhere in the crate.
pub mod error;

/// Code for implementing a game, including game logic and state.
pub mod game;

/// Code relating to board tiles and coordinates.
pub mod tiles;

/// An implementation of a bitfield, used to hold board state.
pub mod bitfield;

/// Code relating to "plays" (ie, game moves).
pub mod play;

/// Pre-defined rulesets and board positions.
pub mod preset;

/// Code relating to the board, including board state and geometry.
pub mod board;