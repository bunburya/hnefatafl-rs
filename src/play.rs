use crate::error::ParseError::{BadPlay, BadString};
use crate::error::PlayError::DisjointTiles;
use crate::error::{BoardError, ParseError, PlayError};
use crate::game::logic::GameLogic;
use crate::game::state::GameState;
use crate::game::GameOutcome;
use crate::pieces::{Piece, PlacedPiece, Side};
use crate::tiles::Axis::{Horizontal, Vertical};
use crate::tiles::{Axis, AxisOffset, Coords, Tile};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use crate::board::state::BoardState;
use crate::collections::piecemap::PieceMap;
use crate::game::GameOutcome::{Draw, Win};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A single move of a piece from one tile to another. (Named "Play" rather than "Move" as the lower-cased version of
/// the latter would clash with the Rust keyword.)
///
/// This is implemented as a combination of source tile, axis of movement and displacement (with a
/// negative displacement representing a move "backwards" along the relevant axis, ie, to a
/// lower-numbered row or column). This way, moves are guaranteed to be along a row or column (but
/// are not guaranteed to be within the bounds of the board, nor are they guaranteed to be valid
/// generally).
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Play {
    pub from: Tile,
    /// The axis along which the move occurs, ie, horizontal or vertical.
    pub movement: AxisOffset,
}

impl Play {
    pub fn new(from: Tile, movement: AxisOffset) -> Self {
        Self { from, movement }
    }

    /// Create a new [`Play`] from source and destination tiles.
    pub fn from_tiles(src: Tile, dst: Tile) -> Result<Self, PlayError> {
        let axis: Axis;
        let displacement: i8;
        if src.row == dst.row {
            axis = Horizontal;
            displacement = (dst.col as i8) - (src.col as i8);
        } else if src.col == dst.col {
            axis = Vertical;
            displacement = (dst.row as i8) - (src.row as i8);
        } else {
            return Err(DisjointTiles);
        };
        Ok(Self::new(src, AxisOffset::new(axis, displacement)))
    }

    /// The unsigned distance in tiles covered by the move. Basically the absolute value of
    /// the displacement.
    pub fn distance(&self) -> u8 {
        self.movement.displacement.unsigned_abs()
    }

    /// The move's destination tile.  **NOTE**: This creates a tile without checking whether it is
    /// in bounds. Use the output with caution.
    pub fn to(&self) -> Tile {
        let coords = self.to_coords();
        Tile::new(coords.row as u8, coords.col as u8)
    }

    /// The row and column of the move's destination tile, as [`Coords`].
    pub fn to_coords(&self) -> Coords {
        Coords::from(self.from) + self.movement
    }
}

impl FromStr for Play {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens: Vec<&str> = s.split('-').collect();
        if tokens.len() != 2 {
            return Err(BadString(String::from(s)));
        };
        let m_res = Play::from_tiles(Tile::from_str(tokens[0])?, Tile::from_str(tokens[1])?);
        match m_res {
            Ok(m) => Ok(m),
            Err(e) => Err(BadPlay(e)),
        }
    }
}

impl Display for Play {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.from, self.to())
    }
}

/// A thin wrapper around a [`Play`], intended to indicate that the `Play` is known to be a valid
/// play in the current game.
///
/// **NOTE:** A `ValidPlay` should only be constructed with a `Play` that is known to be valid.
/// Passing a `ValidPlay` around an invalid `Play` to a function can cause panics or bad program
/// state. It is generally preferable to create a `ValidPlay` by passing a `Play` to the
/// [`GameLogic::validate_play`] method.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ValidPlay {
    pub play: Play,
}

impl Display for ValidPlay {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.play.fmt(f)
    }
}

/// The effects of a single play, including captures and the game outcome caused by the play, if
/// any.
#[derive(Eq, PartialEq, Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlayEffects<B: BoardState> {
    /// Tiles containing pieces that have been captured by the move.
    pub captures: B::PieceMap,
    /// The outcome of the game, if the move has brought the game to an end.
    pub game_outcome: Option<GameOutcome>,
}

/// A record of a single play.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PlayRecord<B: BoardState> {
    /// The side that made the play.
    pub side: Side,
    /// Details of the play (piece movement) itself.
    pub play: Play,
    /// Details of the effects of the play.
    pub effects: PlayEffects<B>,
}

impl<B: BoardState> PlayRecord<B> {
    /// Whether these two records are equal, ignoring the outcomes of the moves.
    pub fn eq_ignore_outcome(&self, other: &Self) -> bool {
        self.side == other.side && self.play == other.play
    }
}

impl<B: BoardState> Display for PlayRecord<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.play)?;
        if !self.effects.captures.is_empty() {
            write!(
                f,
                "x{}",
                self.effects
                    .captures
                    .clone()
                    .into_iter()
                    .map(|pp: PlacedPiece| pp.tile.to_string())
                    .collect::<Vec<_>>()
                    .join("/")
            )?;
        }
        match self.effects.game_outcome {
            Some(Win(_, side)) => {
                if side == Side::Attacker {
                    write!(f, "++")?;
                } else {
                    write!(f, "--")?;
                }
            }
            Some(Draw(_)) => write!(f, "==")?,
            None => {}
        }
        Ok(())
    }
}

/// An iterator over the possible plays that can be made by the piece at the given tile. Note that
/// because this struct holds a reference to the [`GameLogic`] and [`GameState`], neither may be
/// mutated while the iterator exists. Order of iteration is not guaranteed.
pub struct ValidPlayIterator<'a, 'b, B: BoardState> {
    game_logic: &'a GameLogic<B>,
    game_state: &'b GameState<B>,
    start_tile: Tile,
    piece: Piece,
    movement: AxisOffset,
}

impl<'logic, 'state, B: BoardState> ValidPlayIterator<'logic, 'state, B> {
    pub fn new(
        game_logic: &'logic GameLogic<B>,
        game_state: &'state GameState<B>,
        tile: Tile,
    ) -> Result<Self, BoardError> {
        if let Some(piece) = game_state.board.get_piece(tile) {
            Ok(Self {
                game_logic,
                game_state,
                start_tile: tile,
                piece,
                movement: AxisOffset {
                    axis: Vertical,
                    displacement: 1,
                },
            })
        } else {
            Err(BoardError::NoPiece)
        }
    }

    /// Get the next direction by rotating the current direction 90 degrees. If the rotation would
    /// bring us back to the start, return `None` instead as we have been through all rotations.
    fn next_direction(&self) -> Option<AxisOffset> {
        match self.movement.axis {
            Vertical => {
                if self.movement.displacement > 0 {
                    Some(AxisOffset {
                        axis: Vertical,
                        displacement: -1,
                    })
                } else {
                    Some(AxisOffset {
                        axis: Horizontal,
                        displacement: 1,
                    })
                }
            }
            Horizontal => {
                if self.movement.displacement > 0 {
                    Some(AxisOffset {
                        axis: Horizontal,
                        displacement: -1,
                    })
                } else {
                    None
                }
            }
        }
    }
}

impl<'logic, 'state, B: BoardState> Iterator for ValidPlayIterator<'logic, 'state, B> {
    type Item = ValidPlay;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            //let dest_coords = Coords::from(self.current_tile) + self.direction;
            let play = Play::new(self.start_tile, self.movement);
            if let Ok(dest_tile) = self.game_logic.board_geo.coords_to_tile(play.to_coords()) {
                // New tile is in bounds

                // Increase the step for the next iteration.
                self.movement.displacement += if self.movement.displacement.is_positive() {
                    1
                } else {
                    -1
                };
                let (can_occupy, can_pass) =
                    self.game_logic
                        .can_occupy_or_pass(play, self.piece, self.game_state);
                if can_occupy {
                    // We found a tile we can occupy, so return that
                    return Some(ValidPlay {
                        play: Play::from_tiles(self.start_tile, dest_tile)
                            .expect("Tiles should be on same axis."),
                    });
                } else if can_pass {
                    // We can't occupy this tile, but we can pass it, so go back to the start of the
                    // loop to continue in the same direction
                    continue;
                } else {
                    // We can neither occupy nor pass this tile so move on to trying the next
                    // direction. If we have already tried all the directions, there are no more
                    // plays available so return `None`.
                    self.movement = self.next_direction()?;
                    continue;
                }
            } else {
                // New tile would be out of bounds so move on to trying the next direction.
                self.movement = self.next_direction()?;
                continue;
            }
        }
    }
}
