use crate::board::Board;
use crate::error::ParseError;
use crate::pieces::PieceType::King;
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, Side};
use crate::rules::GameOutcome::Winner;
use crate::rules::KingStrength::{Strong, StrongByThrone, Weak};
use crate::rules::{GameOutcome, MoveOutcome, Ruleset, ThroneRule};
use crate::tiles::{Move, Tile};

pub(crate) struct Game {
    pub(crate) board: Board,
    rules: Ruleset,
    turn: u32,
    side_to_play: Side
}

impl Game {

    pub(crate) fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        Ok(Self {
            board: Board::try_from(starting_board)?,
            rules,
            turn: 0,
            side_to_play: if rules.attacker_starts { Attacker } else { Defender }
        })
    }

    /// Determine whether a given tile is hostile to the given piece.
    pub(crate) fn tile_is_hostile(&self, tile: Tile, piece: Piece) -> bool {
        if let Some(other_piece) = self.board.get_piece(tile) {
            // Tile contains a piece. If the piece is of a different side, tile is hostile, unless
            // that piece is an unarmed king.
            (other_piece.side != piece.side)
                && (self.rules.armed_king || !(other_piece.piece_type == King))
        } else {
            // Tile is empty. So it is only hostile if it is a special tile/edge and the rules state
            // that it is hostile to the given piece.
            (self.rules.hostility.throne.contains(piece.piece_type) && tile == self.board.throne)
                || (self.rules.hostility.corners.contains(piece.piece_type)
                    && self.board.corners.contains(&tile))
                || (self.rules.hostility.edge.contains(piece.piece_type)
                    && !self.board.tile_in_bounds(tile))
        }
    }

    pub(crate) fn is_valid_move(&self, piece_move: Move) -> bool {
        let from = piece_move.from;
        let to = piece_move.to();
        let maybe_piece = self.board.get_piece(from);
        match maybe_piece {
            None => false,
            Some(piece) => {
                if !(self.board.tile_in_bounds(from) && self.board.tile_in_bounds(to)) {
                    // Tile out of bounds
                    return false
                }
                if (from.row() != to.row()) && (from.col() != to.col()) {
                    // Tiles not on same row or column
                    return false
                }
                let between = self.board.tiles_between(from, to);
                if between.iter().any(|t| self.board.tile_occupied(*t)) {
                    // Move is blocked by a piece
                    return false
                }
                if (
                    (self.rules.throne_movement == ThroneRule::NoPass)
                        || ((self.rules.throne_movement == ThroneRule::KingPass)
                            && piece.piece_type != King)
                ) && between.contains(&self.board.throne) {
                    // Move is blocked by the throne
                    return false
                }
                if ((self.rules.throne_movement == ThroneRule::NoEntry)
                    || ((self.rules.throne_movement == ThroneRule::KingEntry)
                        && piece.piece_type != King)
                ) && (to == self.board.throne) {
                    // Illegal move on to the throne
                    return false
                }
                if self.rules.slow_pieces.contains(piece.piece_type) && from.distance_to(to) > 1 {
                    // Slow piece can't move more than one space at a time
                    return false
                }
                true
            }
        }
    }

    pub(crate) fn king_is_strong(&self) -> bool {
        match self.rules.king_strength {
            Strong => true,
            Weak => false,
            StrongByThrone => {
                let k = self.board.get_king();
                self.board.corners.contains(&k) || self.board.throne == k
            }
        }
    }
    
    pub(crate) fn get_game_outcome(&self, m: Move, captures: &[Tile]) -> Option<GameOutcome> {
        if captures.len() as u8 >= self.board.state.count_pieces(self.side_to_play.other()) {
            return Some(Winner(self.side_to_play))
        } 
        if self.side_to_play == Attacker && captures.contains(&self.board.state.get_king()) {
            return Some(Winner(Attacker))
        }
        if (self.side_to_play == Defender) 
            && self.board.is_king(m.from) 
            && (
                (self.rules.edge_escape && self.board.tile_at_edge(m.to()))
                || (!self.rules.edge_escape && self.board.corners.contains(&m.to()))
        ) {
            return Some(Winner(Defender))
        }
        None
    }

    pub(crate) fn move_outcome(&self, m: Move) -> Option<MoveOutcome> {
        let mover = self.board.get_piece(m.from)?;
        let to = m.to();
        let mut captures: Vec<Tile> = vec![];
        for n in self.board.neighbors(to) {
            if let Some(other_piece) = self.board.get_piece(n) {
                if other_piece.side == mover.side {
                    // Friendly neighbour so no possibility for capture
                    continue
                }
                let far_tile = Tile::new(
                    to.row() + ((n.row() - to.row()) * 2),
                    to.col() + ((n.col() - to.col()) * 2)
                );
                if self.tile_is_hostile(far_tile, other_piece) {
                    // We know that the neighbouring opposing piece is surrounded by the moving
                    // piece and another hostile tile. So it is captured, *unless* it is a 
                    // strong king.
                    if (other_piece.piece_type == King) && self.king_is_strong() {
                        // Get the tiles surrounding `n` on the perpendicular axis.
                        let adj_tiles: [Tile; 2] = if to.row() == far_tile.row() {
                            [
                                Tile::new(n.row() + 1, n.col()),
                                Tile::new(n.row() - 1, n.col())
                            ]
                        } else {
                            [
                                Tile::new(n.row(), n.col() + 1),
                                Tile::new(n.row(), n.col() - 1)
                            ]
                        };
                        // Check if these tiles are also hostile
                        if !self.tile_is_hostile(adj_tiles[0], other_piece)
                            || !self.tile_is_hostile(adj_tiles[1], other_piece) {
                            continue
                        }
                    }
                    captures.push(n);
                }
            }
        }
        let game_outcome = self.get_game_outcome(m, &captures);
        Some(MoveOutcome { captures, game_outcome })
    }
}

#[cfg(test)]
mod tests {
    use crate::game::Game;
    use crate::rules::FED_BRAN;

    #[test]
    fn test_fed_bran_move_logic() {
        let g = Game::new(
            FED_BRAN, 
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();
        
    }
}