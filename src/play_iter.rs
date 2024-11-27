use crate::board_state::BoardState;
use crate::error::BoardError;
use crate::game_logic::GameLogic;
use crate::game_state::GameState;
use crate::tiles::AxisOffset;
use crate::Axis::{Horizontal, Vertical};
use crate::{Piece, Play, Tile};

/// An iterator over the possible plays that can be made by the piece at the given tile. Note that
/// because this struct holds a reference to the [`GameLogic`] and [`GameState`], neither may be
/// mutated while the iterator exists. Order of iteration is not guaranteed.

pub struct PlayIterator<'a, 'b, T: BoardState> {
    game_logic: &'a GameLogic,
    game_state: &'b GameState<T>,
    start_tile: Tile,
    piece: Piece,
    movement: AxisOffset,
}

impl<'logic, 'state, T: BoardState> PlayIterator<'logic, 'state, T> {

    pub fn new(game_logic: &'logic GameLogic, game_state: &'state GameState<T>, tile: Tile) -> Result<Self, BoardError> {
        if let Some(piece) = game_state.board.get_piece(tile) {
            Ok(Self {
                game_logic,
                game_state,
                start_tile: tile,
                piece,
                movement: AxisOffset { axis: Vertical, displacement: 1 }
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
                    Some(AxisOffset { axis: Vertical, displacement: -1 })
                } else {
                    Some(AxisOffset { axis: Horizontal, displacement: 1 })
                }
            },
            Horizontal => {
                if self.movement.displacement > 0 {
                    Some(AxisOffset { axis: Horizontal, displacement: -1 })
                } else {
                    None
                }
            }
        }
    }
}

impl<'logic, 'state, T: BoardState> Iterator for PlayIterator<'logic, 'state, T> {
    type Item = Play;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            //let dest_coords = Coords::from(self.current_tile) + self.direction;
            let play = Play::new(self.start_tile, self.movement);
            if let Ok(dest_tile) = self.game_logic.board_geo.coords_to_tile(play.to_coords()) {
                // New tile is in bounds

                // Increase the step for the next iteration.
                self.movement.displacement +=
                    if self.movement.displacement.is_positive() { 1 } else { -1 };
                let (can_occupy, can_pass) = self.game_logic.can_occupy_or_pass(play, self.piece, self.game_state);
                if can_occupy {
                    // We found a tile we can occupy, so return that
                    return Some(Play::from_tiles(self.start_tile, dest_tile)
                        .expect("Tiles should be on same axis."))
                } else if can_pass {
                    // We can't occupy this tile, but we can pass it, so go back to the start of the
                    // loop to continue in the same direction
                    continue
                } else {
                    // We can neither occupy nor pass this tile so move on to trying the next
                    // direction. If we have already tried all the directions, there are no more
                    // plays available so return `None`.
                    self.movement = self.next_direction()?;
                    continue
                }
            } else {
                // New tile would be out of bounds so move on to trying the next direction.
                self.movement = self.next_direction()?;
                continue
            }
        }
    }
}