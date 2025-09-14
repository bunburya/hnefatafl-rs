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
//! Board state, and other collections of pieces and tiles, are generally implemented using
//! bitfields (integers). Larger boards require larger integer types to be able to display
//! the board state. For example, a 7x7 board can be represented using `u64`s, but an 11x11 board
//! would require `u128`s.
//!
//! In addition, board state generally uses a separate bitfield for each side and
//! piece type that can be placed on the board. Therefore, for variants which support a wider range
//! of pieces, more bitfields are required. To allow different board sizes and piece sets
//! to be represented, while also allowing state for smaller, simpler games to be represented
//! efficiently, `hnefatafl` uses a number of different traits and structs to represent board state.
//!
//! Some of the key traits and structs used to store game state are described below:
//!
//! * [`bitfield::BitField`] is a trait allowing access to a bitfield and basic conversions between [`Tile`]s
//!   and bit indices. It is implemented on a number of different integer types. It is kind of the
//!   building block of the other structs and traits used to represent board state.
//! * [`collections::TileSet`] is a struct that represents a set of [`Tiles`] on a board. It is generic over
//!   `BitField`.
//! * [`collections::PieceMap`] is a trait allowing mapping of tiles to the pieces (if any) that occupy them.
//!   `PieceMap` has an associated type `BitField` which is the bitfield type used to represent the
//!   board state. Several of the collections returned by functions on `PieceMap` are generic over
//!   that type.
//! * [`collections::BasicPieceMap`] is a struct that implements `PieceMap` using `TileSet`s to represent
//!   the "basic" pieces: attacking soldiers, defending soldiers and defending king.
//! * [`board::state::BoardState`] is a trait allowing access to the board state. It has two associated types:
//!   `BitField` and `PieceMap` (each of which is constrained by the trait of the same name) which
//!   are used to store and represent board state internally.
//! * [`board::state::BasicBoardState`] is a struct that implements `BoardState` for basic pieces. It is generic
//!   over `BitField` and uses a `BasicPieceMap` to store state internally.
//! * [`game::state::GameState`] is a struct that represents the current state of a game. It is generic over
//!   `BoardState`, and includes both board state and other game state, such as the player whose
//!   turn it is.
//! * [`game::Game`] is a struct that represents a game. It groups together state, rules and history. It
//!   is generic over `BoardState`.
//!
//! To reduce the complexity of having to deal with generics and associated types for simple use
//! cases, some concrete implementations of these traits are provided. These are described below.
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
//! `Game<BasicBoardState<u64>>`.
//!
//! # Serialization
//!
//! If compiled with the `serde` feature, the structs in this crate may be serialised and
//! deserialized using the [`serde`](https://serde.rs/) crate.

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

/// Code relating to collections of pieces and tiles.
pub mod collections;
