use crate::board::state::BoardState;
use crate::error::ParseError;
use crate::game::GameStatus;
use crate::game::GameStatus::Ongoing;
use crate::pieces::Side;
use std::cmp::PartialEq;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A struct containing the minimum information needed to uniquely identify a position in the game.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Position<B: BoardState> {
    board_state: B,
    side_to_play: Side,
    status: GameStatus,
}

impl<B: BoardState> From<&GameState<B>> for Position<B> {
    fn from(game_state: &GameState<B>) -> Self {
        Self {
            board_state: game_state.board,
            side_to_play: game_state.side_to_play,
            status: game_state.status,
        }
    }
}

/// A fixed size queue of game positions. Pushing a new value to the end of the queue drops the
/// first item in the queue.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PositionQueue<B: BoardState> {
    // 32 chosen because that's the longest array for which `serde` can derive `Serialize` and
    // `Deserialize`
    queue: [Option<Position<B>>; 32],
    first_i: usize,
}

impl<B: BoardState> PositionQueue<B> {
    /// Push a new position to the queue.
    pub(crate) fn push(&mut self, value: GameState<B>) {
        self.queue[self.first_i] = Some((&value).into());
        self.first_i = if self.first_i == 31 {
            0
        } else {
            self.first_i + 1
        }
    }

    /// Returns true if the given position appears at least `n` times in the queue.
    pub(crate) fn appears_n_times(&self, posn: Position<B>, n: usize) -> bool {
        let mut count = 0;
        for p in self.queue {
            if p == Some(posn) {
                count += 1;
                if count == n {
                    return true;
                }
            }
        }
        false
    }
}

/// This struct contains all state that can be used to evaluate play outcomes and board positions
/// and that changes regularly. The idea is to keep this struct as small as possible to facilitate
/// efficient play evaluation.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct GameState<B: BoardState> {
    /// Board state, ie, the current pieces on the board.
    pub board: B,
    /// The side whose turn it is.
    pub side_to_play: Side,
    /// Tracker for repetitions.
    pub recent_positions: PositionQueue<B>,
    /// Number of plays since a piece was last captured.
    pub plays_since_capture: usize,
    /// Current status of the game.
    pub status: GameStatus,
    /// Number of plays that have been taken by either side.
    pub turn: usize,
}

impl<B: BoardState> GameState<B> {
    pub fn new(fen_str: &str, side_to_play: Side) -> Result<Self, ParseError> {
        Ok(Self {
            board: B::from_fen(fen_str)?,
            side_to_play,
            recent_positions: PositionQueue::default(),
            plays_since_capture: 0,
            status: Ongoing,
            turn: 0,
        })
    }
}
