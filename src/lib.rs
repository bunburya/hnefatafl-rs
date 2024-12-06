//! This crate provides functionality for creating software related to the
//! [tafl](https://en.wikipedia.org/wiki/Tafl_games) family of board games. It includes structs,
//! enums and traits that encapsulate game data and logic, helpful to build games, AIs, etc. It is
//! not a goal of this crate to provide any concrete implementations of game clients or AIs.
//! 
//! # Getting started
//! 
//! As a starting point, you will likely want to use the following structs if you are implementing a
//! game engine or similar:
//! 
//! - [`rules::Ruleset`]: Specifies the rules for the game.
//! - [`game::logic::GameLogic`]: Keeps a copy of the game rules, and implements the logic required
//!   to assess the validity and outcome of a given move. The data stored in this struct is expected
//!   to be static over the course of a single game. It does not keep information about the current
//!   game state, but rather, its methods take a reference to that state as necessary.
//! - [`game::state::GameState`]: Keeps track of the current state of the game - that is, all the
//!   data that changes frequently, such as positions of pieces on the board, which side is to play,
//!   etc. The aim is to minimise the memory footprint of this data and avoid heap allocations so
//!   that game state can be copied and passed around efficiently.
//! - [`game::Game`]: This struct contains a `GameLogic` and a `GameState`, and also contains a
//!   couple of `Vec`s which keep track of previous moves (to allow move history to be shown to the
//!   user) and previous game states (to allow move undoing). Therefore, it is a helpful struct when
//!   building a game client, for example.
//! 
//! You can roll your own ruleset and starting board setup, or you can choose from one of the common
//! variants which are included in the [`preset::rules`] and [`preset::boards`] modules.
//! 
//! # Board state
//! 
//! There are various ways to represent the placement of pieces on the game board. For example, one
//! could use bitfields, or an array, etc. These different representations have different trade-offs
//! between performance, memory efficiency and code readability. To allow developers to roll their
//! own implementations if they wish, the `GameState` struct (and therefore the `Game` struct as
//! well) is generic over the parameter `T`, where `T` implements the [`board::state::BoardState`]
//! trait.
//! 
//! `hnefatafl`'s default representation of the board state uses bitfields to represent piece
//! placement (see [`board::state::BitfieldBoardState`]). This means that larger boards will require
//! the use of larger integer types to represent them - for example, a 7x7 board can be represented
//! by a couple of `u64`s, but an 11x11 board can't (at least, using our existing implementation).
//! This crate therefore provides a number of different `BoardState` implementations which use
//! differently-sized integers, so that you can represent larger boards if necessary, but if you
//! only want to represent a smaller board, you don't need to use a very large integer type. 
//!
//! If none of this matters to you, and you just want to use a reasonable default representation,
//! dealing with type parameters all the time can be annoying, so a few type aliases are provided to
//! avoid having to specify them all the time.
//! 
//! Default `GameState` implementations:
//! - [`game::state::SmallBasicGameState`]
//! - [`game::state::MediumBasicGameState`]
//! - [`game::state::LargeBasicGameState`]
//! - [`game::state::HugeBasicGameState`]
//! 
//! And `Game` implementations which are based on them:
//! - [`game::SmallBasicGame`]
//! - [`game::MediumBasicGame`]
//! - [`game::LargeBasicGame`]
//! - [`game::HugeBasicGame`]
//! 
//! So if you just want to play a game on a 7x7 board, you can use a `SmallBasicGame` instead of a
//! `Game<BitfieldBoardState<u64>>`.

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