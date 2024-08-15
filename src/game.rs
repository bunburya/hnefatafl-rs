use crate::board::Board;
use crate::error::ParseError;
use crate::game::MoveValidity::{BlockedByPiece, BlockedByThrone, IllegalThroneEntry, NoCommonAxis, NoPiece, OutOfBounds, TooFar, ValidMove};
use crate::pieces::PieceType::King;
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, Side};
use crate::rules::KingStrength::{Strong, StrongByThrone, Weak};
use crate::rules::{Ruleset, ThroneRule};
use crate::tiles::{Move, Tile};
use std::collections::HashSet;
use std::str::FromStr;
use crate::game::GameOutcome::Winner;
use crate::traits::BitField;

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum MoveValidity {
    /// Move is valid.
    ValidMove,
    /// There is no piece to move at the given tile.
    NoPiece,
    /// The destination tile would be outside the board.
    OutOfBounds,
    /// The start and end tiles do not share an axis (ie, they are not on the same row or column).
    NoCommonAxis,
    /// Another piece is blocking the move.
    BlockedByPiece,
    /// The throne is blocking the move (and the rules do not permit this piece to pass through the
    /// throne).
    BlockedByThrone,
    /// This move would end on the throne (and the rules do not permit this piece to enter the
    /// throne).
    IllegalThroneEntry,
    /// The move is further than this piece is permitted to move in one go.
    TooFar
}

#[derive(Eq, PartialEq, Debug)]
pub(crate) enum GameOutcome {
    Winner(Side),
    Draw
}


#[derive(Eq, PartialEq, Debug, Default)]
pub(crate) struct MoveOutcome {
    pub(crate) captures: HashSet<Tile>,
    pub(crate) game_outcome: Option<GameOutcome>
}

pub(crate) struct Game<T: BitField> {
    pub(crate) board: Board<T>,
    rules: Ruleset,
    turn: u32,
    side_to_play: Side
}

impl<T: BitField> Game<T> {

    pub(crate) fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        Ok(Self {
            board: Board::from_str(starting_board)?,
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

    pub(crate) fn check_move_validity(&self, m: Move) -> MoveValidity {
        let from = m.from;
        let to = m.to();
        let maybe_piece = self.board.get_piece(from);
        match maybe_piece {
            None => NoPiece,
            Some(piece) => {
                if !(self.board.tile_in_bounds(from) && self.board.tile_in_bounds(to)) {
                    // Tile out of bounds
                    return OutOfBounds
                }
                if (from.row() != to.row()) && (from.col() != to.col()) {
                    // Tiles not on same row or column
                    return NoCommonAxis
                }
                let between = self.board.tiles_between(from, to);
                if between.iter().any(|t| self.board.tile_occupied(*t)) {
                    // Move is blocked by a piece
                    return BlockedByPiece
                }
                if (
                    (self.rules.throne_movement == ThroneRule::NoPass)
                        || ((self.rules.throne_movement == ThroneRule::KingPass)
                            && piece.piece_type != King)
                ) && between.contains(&self.board.throne) {
                    // Move is blocked by the throne
                    return BlockedByThrone
                }
                if ((self.rules.throne_movement == ThroneRule::NoEntry)
                    || ((self.rules.throne_movement == ThroneRule::KingEntry)
                        && piece.piece_type != King)
                ) && (to == self.board.throne) {
                    // Illegal move on to the throne
                    return IllegalThroneEntry
                }
                if self.rules.slow_pieces.contains(piece.piece_type) && m.distance() > 1 {
                    // Slow piece can't move more than one space at a time
                    return TooFar
                }
                ValidMove
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
    
    pub(crate) fn get_game_outcome(&self, m: Move, caps: &HashSet<Tile>) -> Option<GameOutcome> {
        if caps.len() as u8 >= self.board.state.count_pieces(self.side_to_play.other()) {
            // All opposing pieces have been captured.
            return Some(Winner(self.side_to_play))
        } 
        if self.side_to_play == Attacker && caps.contains(&self.board.state.get_king()) {
            // Attacker has captured the king.
            return Some(Winner(Attacker))
        }
        if (self.side_to_play == Defender) 
            && self.board.is_king(m.from) 
            && (
                (self.rules.edge_escape && self.board.tile_at_edge(m.to()))
                || (!self.rules.edge_escape && self.board.corners.contains(&m.to()))
        ) {
            // King has escaped.
            return Some(Winner(Defender))
        }
        None
    }

    pub(crate) fn move_outcome(&self, m: Move) -> MoveOutcome {
        let mut captures: HashSet<Tile> = HashSet::new();
        let occupant = self.board.get_piece(m.from);
        match occupant {
            None => MoveOutcome { captures, game_outcome: None },
            Some(mover) => {
                let to = m.to();
                for n in self.board.neighbors(to) {
                    if let Some(other_piece) = self.board.get_piece(n) {
                        if other_piece.side == mover.side {
                            // Friendly neighbour so no possibility for capture
                            continue
                        }
                        let signed_to_row = to.row() as i8;
                        let signed_to_col = to.col() as i8;
                        let signed_n_row = n.row() as i8;
                        let signed_n_col = n.col() as i8;
                        let far_tile = Tile::new(
                            (signed_to_row + (signed_n_row - signed_to_row) * 2) as u8,
                            (signed_to_col + (signed_n_col - signed_to_col) * 2) as u8
                        );
                        if self.tile_is_hostile(far_tile, other_piece) {
                            // We know that the neighbouring opposing piece is surrounded by the
                            // moving piece and another hostile tile. So it is captured, *unless* it
                            // is a strong king.
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
                            captures.insert(n);
                        }
                    }
                }
                let game_outcome = self.get_game_outcome(m, &captures);
                MoveOutcome { captures, game_outcome }
            } 
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::game::{Game, MoveOutcome};
    use crate::game::GameOutcome::Winner;
    use crate::game::MoveValidity::{
        BlockedByPiece,
        BlockedByThrone,
        IllegalThroneEntry,
        NoPiece,
        OutOfBounds,
        TooFar,
        ValidMove
    };
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::King;
    use crate::pieces::Side::{Attacker, Defender};
    use crate::rules::ThroneRule::NoPass;
    use crate::rules::{Ruleset, FED_BRAN};
    use crate::tiles::{Move, Tile};

    const TEST_RULES: Ruleset = Ruleset {
        slow_pieces: PieceSet::from_piece_type(King),
        throne_movement: NoPass,
        ..FED_BRAN
    };
    
    #[test]
    fn test_check_move_validity() {
        let mut fb_game: Game<u64> = Game::new(
            FED_BRAN, 
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(4, 2)
            ).unwrap()),
            ValidMove
        );
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 3),
                Tile::new(3, 2)
            ).unwrap()),
            ValidMove
        );
        
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(1, 1),
                Tile::new(2, 1)
            ).unwrap()),
            NoPiece
        );
        
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 7)
            ).unwrap()),
            OutOfBounds
        );

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(0, 3),
                Tile::new(2, 3)
            ).unwrap()),
            BlockedByPiece
        );

        fb_game.board.do_move(Move::from_tiles(Tile::new(3, 2), Tile::new(4, 2)).unwrap());
        fb_game.board.do_move(Move::from_tiles(Tile::new(3, 3), Tile::new(3, 2)).unwrap());

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(2, 3),
                Tile::new(3, 3)
            ).unwrap()),
            IllegalThroneEntry
        );

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 3)
            ).unwrap()),
            ValidMove
        );
        
        let test_game: Game<u64> = Game::new(
            TEST_RULES,
            ".......\n.....Tt\n..T....\n..t..t.\nTt....T\n..t....\n..T..K."
        ).unwrap();
        
        assert_eq!(
            test_game.check_move_validity(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 3)
            ).unwrap()),
            TooFar
        );
        
        assert_eq!(
            test_game.check_move_validity(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 4)
            ).unwrap()),
            ValidMove
        );
        
        assert_eq!(
            test_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 4)
            ).unwrap()),
            BlockedByThrone
        );
    }
    
    #[test]
    fn test_check_move_outcome() {
        let mut test_game: Game<u64> = Game::new(
            TEST_RULES,
            "....t..\n.....Tt\n..T....\n..t..t.\nTt....T\n..t....\n..T..K."
        ).unwrap();
        
        assert_eq!(
            test_game.move_outcome(Move::from_tiles(
                Tile::new(0, 4),
                Tile::new(6, 4)
            ).unwrap()),
            MoveOutcome {
                captures: [Tile::new(6, 5)].into(),
                game_outcome: Some(Winner(Attacker))
            }
        );
        
        assert_eq!(
            test_game.move_outcome(Move::from_tiles(
                Tile::new(4, 6),
                Tile::new(4, 2)
            ).unwrap()),
            MoveOutcome {
                captures: [
                    Tile::new(4, 1),
                    Tile::new(3, 2),
                    Tile::new(5, 2),
                ].into(),
                game_outcome: None
            }
        );
        
        test_game.side_to_play = test_game.side_to_play.other();
        
        assert_eq!(
            test_game.move_outcome(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 6)
            ).unwrap()),
            MoveOutcome {
                captures: [].into(),
                game_outcome: Some(Winner(Defender))
            }
        );
        
        assert_eq!(
            test_game.move_outcome(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(5, 5)
            ).unwrap()),
            MoveOutcome::default()
        )
    }

}