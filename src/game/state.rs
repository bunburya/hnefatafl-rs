use crate::board::state::BoardState;
use crate::error::ParseError;
use crate::game::GameStatus;
use crate::game::GameStatus::Ongoing;
use crate::pieces::Side;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::cmp::PartialEq;
use std::fmt::Display;
use crate::collections::PieceMap;

/// A struct containing the minimum information needed to uniquely identify a position in the game.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Position<P: PieceMap> {
    pub(crate) board_state: BoardState<P>,
    pub(crate) side_to_play: Side,
    pub(crate) status: GameStatus,
}

impl<P: PieceMap> From<&GameState<P>> for Position<P> {
    fn from(game_state: &GameState<P>) -> Self {
        Self {
            board_state: game_state.board,
            side_to_play: game_state.side_to_play,
            status: game_state.status,
        }
    }
}

impl<P: PieceMap> Display for Position<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {:?}",
            self.board_state.to_fen(),
            self.side_to_play,
            self.status
        )
    }
}

/// This struct contains all state that can be used to evaluate play outcomes and board positions
/// and that changes regularly. The idea is to keep this struct as small as possible to facilitate
/// efficient play evaluation.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct GameState<P: PieceMap> {
    /// Board state, ie, the current pieces on the board.
    pub board: BoardState<P>,
    /// The side whose turn it is.
    pub side_to_play: Side,
    /// Number of plays since a piece was last captured.
    pub plays_since_capture: usize,
    /// Current status of the game.
    pub status: GameStatus,
    /// Number of plays that have been taken by either side.
    pub turn: usize,
}

impl<P: PieceMap> GameState<P> {
    pub fn new(fen_str: &str, side_to_play: Side) -> Result<Self, ParseError> {
        Ok(Self {
            board: BoardState::<P>::from_fen(fen_str)?,
            side_to_play,
            plays_since_capture: 0,
            status: Ongoing,
            turn: 0,
        })
    }
}
