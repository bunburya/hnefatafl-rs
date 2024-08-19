use crate::board::Board;
use crate::error::ParseError;
use crate::game::GameOutcome::Winner;
use crate::game::GameStatus::{Ongoing, Over};
use crate::game::InvalidMove::{BlockedByPiece, MoveOntoBlockedTile, MoveThroughBlockedTile, NoCommonAxis, NoPiece, OutOfBounds, TooFar};
use crate::game::MoveValidity::{Invalid, Valid};
use crate::pieces::PieceType::King;
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, Side};
use crate::rules::KingStrength::{Strong, StrongByThrone, Weak};
use crate::rules::{Ruleset, ThroneRule};
use crate::tiles::{Move, Tile};
use crate::traits::BitField;
use crate::InvalidMove::WrongPlayer;
use std::collections::HashSet;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum InvalidMove {
    /// The piece being moved does not belong to the player whose turn it is.
    WrongPlayer,
    /// There is no piece to move at the given tile.
    NoPiece,
    /// The destination tile would be outside the board.
    OutOfBounds,
    /// The start and end tiles do not share an axis (ie, they are not on the same row or column).
    NoCommonAxis,
    /// Another piece is blocking the move.
    BlockedByPiece,
    /// The move is blocked by a special tile which, according to the game rules, is not passable
    /// by this piece.
    MoveThroughBlockedTile,
    /// This move would end on a special tile which, according to the game rules, this piece may not
    /// occupy.
    MoveOntoBlockedTile,
    /// The move is further than this piece is permitted to move in one go.
    TooFar
}

/// The outcome of a single game.
#[derive(Eq, PartialEq, Debug)]
pub enum GameOutcome {
    /// Game has been won by the specified side.
    Winner(Side),
    /// Game has ended in a draw.
    Draw
}

/// A struct describing the outcome of a single move.
#[derive(Eq, PartialEq, Debug, Default)]
pub struct MoveOutcome {
    /// Tiles containing pieces that have been captured by the move.
    pub captures: HashSet<Tile>,
    /// The outcome of the game, if the move has brought the game to an end.
    pub game_outcome: Option<GameOutcome>
}

/// The current status of the game.
#[derive(Eq, PartialEq, Debug)]
pub enum GameStatus {
    /// Game is still ongoing.
    Ongoing,
    /// Game is over, with the given outcome.
    Over(GameOutcome)
}

/// Whether a move is valid.
#[derive(Eq, PartialEq, Debug)]
pub enum MoveValidity {
    /// Move is valid.
    Valid,
    /// Move is invalid, for the given reason.
    Invalid(InvalidMove)
}

/// A struct representing a single game, including all state and associated information (such as
/// rules) needed to play.
pub struct Game<T: BitField> {
    pub board: Board<T>,
    pub rules: Ruleset,
    pub turn: u32,
    pub side_to_play: Side
}

impl<T: BitField> Game<T> {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        Ok(Self {
            board: Board::from_str(starting_board)?,
            rules,
            turn: 0,
            side_to_play: if rules.attacker_starts { Attacker } else { Defender }
        })
    }

    /// Determine whether the given tile is hostile to the given piece.
    pub fn tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        if let Some(other_piece) = self.board.get_piece(tile) {
            // Tile contains a piece. If the piece is of a different side, tile is hostile, unless
            // that piece is an unarmed king.
            (other_piece.side != piece.side)
                && (self.rules.armed_king || !(other_piece.piece_type == King))
        } else {
            // Tile is empty. So it is only hostile if it is a special tile/edge and the rules state
            // that it is hostile to the given piece.
            (self.rules.hostility.throne.contains(piece) && tile == self.board.throne)
                || (self.rules.hostility.corners.contains(piece)
                    && self.board.corners.contains(&tile))
                || (self.rules.hostility.edge.contains(piece)
                    && !self.board.tile_in_bounds(tile))
        }
    }
    
    /// Determine whether the position at the given row and tile is hostile to the given piece,
    /// including whether the position is a hostile edge. `row` and `col` are `i8`s to allow for
    /// negative values (which are out of bounds).
    pub fn row_col_hostile(&self, row: i8, col: i8, piece: Piece) -> bool {
        if self.board.row_col_in_bounds(row, col) {
            self.tile_hostile(Tile::new(row as u8, col as u8), piece)
        } else {
            self.rules.hostility.edge.contains(piece)
        }
    }

    /// Check whether a move is valid. If the move is valid, returns `None`; otherwise, the wrapped
    /// [`InvalidMove`] variant indicates why the move is invalid..
    pub fn check_move_validity(&self, m: Move) -> MoveValidity {
        let from = m.from;
        let to = m.to();
        let maybe_piece = self.board.get_piece(from);
        match maybe_piece {
            None => Invalid(NoPiece),
            Some(piece) => {
                if piece.side != self.side_to_play {
                    return Invalid(WrongPlayer);
                }
                if !(self.board.tile_in_bounds(from) && self.board.tile_in_bounds(to)) {
                    return Invalid(OutOfBounds)
                }
                if (from.row() != to.row()) && (from.col() != to.col()) {
                    return Invalid(NoCommonAxis)
                }
                let between = self.board.tiles_between(from, to);
                if between.iter().any(|t| self.board.tile_occupied(*t)) {
                    return Invalid(BlockedByPiece)
                }
                if !self.rules.may_enter_corners.contains(piece) &&
                    self.board.corners.contains(&to) {
                    return Invalid(MoveOntoBlockedTile)
                }
                if (
                    (self.rules.throne_movement == ThroneRule::NoPass)
                        || ((self.rules.throne_movement == ThroneRule::KingPass)
                            && piece.piece_type != King)
                ) && between.contains(&self.board.throne) {
                    return Invalid(MoveThroughBlockedTile)
                }
                if ((self.rules.throne_movement == ThroneRule::NoEntry)
                    || ((self.rules.throne_movement == ThroneRule::KingEntry)
                        && piece.piece_type != King)
                ) && (to == self.board.throne) {
                    return Invalid(MoveOntoBlockedTile)
                }
                if self.rules.slow_pieces.contains(piece) && m.distance() > 1 {
                    // Slow piece can't move more than one space at a time
                    return Invalid(TooFar)
                }
                Valid
            }
        }
    }

    /// Check whether the king is strong (must be surrounded on all four sides to be captured),
    /// considering the game rules and the king's current position. 
    pub fn king_is_strong(&self) -> bool {
        match self.rules.king_strength {
            Strong => true,
            Weak => false,
            StrongByThrone => {
                let k = self.board.get_king();
                self.board.corners.contains(&k) || self.board.throne == k
            }
        }
    }
    
    /// Get the outcome of the game, if any. If None, the game is still ongoing.
    pub fn get_game_outcome(&self, m: Move, caps: &HashSet<Tile>) -> Option<GameOutcome> {
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

    /// Get the outcome of a move (number of captures, whether it ends the game, etc).
    pub fn get_move_outcome(&self, m: Move) -> MoveOutcome {
        let mut captures: HashSet<Tile> = HashSet::new();
        let occupant = self.board.get_piece(m.from);
        if occupant.is_none() {
            return MoveOutcome { captures, game_outcome: None }
        }
        let mover = occupant.expect("Occupant must not be None");
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
                let signed_far_row = signed_to_row + ((signed_n_row - signed_to_row) * 2);
                let signed_far_col = signed_to_col + ((signed_n_col - signed_to_col) * 2);
                // Check if the tile on the other side of the neighbour is a hostile tile, or if the
                // neighbour is on the edge and the edge is treated as hostile to that piece
                
                if self.row_col_hostile(signed_far_row, signed_far_col, other_piece) {
                    // We know that the neighbouring opposing piece is surrounded by the
                    // moving piece and another hostile tile. So it is captured, *unless* it
                    // is a strong king.
                    if (other_piece.piece_type == King) && self.king_is_strong() {
                        // Get the tiles surrounding `n` on the perpendicular axis.
                        let perp_hostile= if to.row() == n.row() {
                            self.row_col_hostile(signed_n_row + 1, signed_n_col, other_piece)
                                && self.row_col_hostile(signed_n_row - 1, signed_n_col, other_piece)
                        } else {
                            self.row_col_hostile(signed_n_row, signed_n_col + 1, other_piece)
                                && self.row_col_hostile(signed_n_row, signed_n_col - 1, other_piece)
                        };
                        if !perp_hostile {
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
    
    /// Actually "do" a move, checking validity, 
    pub fn do_move(&mut self, m: Move) -> Result<GameStatus, InvalidMove> {
        if let Invalid(v) = self.check_move_validity(m) {
            return Err(v)
        };
        let move_outcome = self.get_move_outcome(m);
        self.board.move_piece(m.from, m.to());
        for c in move_outcome.captures {
            self.board.remove_piece(c)
        }

        self.turn += 1;
        self.side_to_play = self.side_to_play.other();
        Ok(match move_outcome.game_outcome {
            Some(outcome) => Over(outcome),
            None => Ongoing
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::game::GameOutcome::Winner;
    use crate::game::InvalidMove::{
        BlockedByPiece,
        MoveOntoBlockedTile,
        MoveThroughBlockedTile,
        NoPiece,
        OutOfBounds,
        TooFar
    };
    use crate::game::MoveValidity::{Invalid, Valid};
    use crate::game::{Game, MoveOutcome};
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::King;
    use crate::pieces::Side::{Attacker, Defender};
    use crate::rules::ThroneRule::NoPass;
    use crate::rules::{Ruleset, COPENHAGEN_HNEFATAFL, FEDERATION_BRANDUBH};
    use crate::tiles::{Move, Tile};
    use std::fs;
    use std::path::PathBuf;

    const TEST_RULES: Ruleset = Ruleset {
        slow_pieces: PieceSet::from_piece_type(King),
        throne_movement: NoPass,
        ..FEDERATION_BRANDUBH
    };
    
    #[test]
    fn test_check_move_validity() {
        let mut fb_game: Game<u64> = Game::new(
            FEDERATION_BRANDUBH,
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(4, 2)
            ).unwrap()),
            Valid
        );
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 0)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 3),
                Tile::new(3, 2)
            ).unwrap()),
            Valid
        );
        
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(1, 1),
                Tile::new(2, 1)
            ).unwrap()),
            Invalid(NoPiece)
        );
        
        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 7)
            ).unwrap()),
            Invalid(OutOfBounds)
        );

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(0, 3),
                Tile::new(2, 3)
            ).unwrap()),
            Invalid(BlockedByPiece)
        );

        fb_game.board.move_piece(Tile::new(3, 2), Tile::new(4, 2));
        fb_game.board.move_piece(Tile::new(3, 3), Tile::new(3, 2));

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(2, 3),
                Tile::new(3, 3)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );

        assert_eq!(
            fb_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 3)
            ).unwrap()),
            Valid
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
            Invalid(TooFar)
        );
        
        assert_eq!(
            test_game.check_move_validity(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 4)
            ).unwrap()),
            Valid
        );
        
        assert_eq!(
            test_game.check_move_validity(Move::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 4)
            ).unwrap()),
            Invalid(MoveThroughBlockedTile)
        );
    }
    
    #[test]
    fn test_check_move_outcome() {
        let mut test_game: Game<u64> = Game::new(
            TEST_RULES,
            "....t..\n.....Tt\n..T....\n..t..t.\nTt....T\n..t....\n..T..K."
        ).unwrap();
        
        assert_eq!(
            test_game.get_move_outcome(Move::from_tiles(
                Tile::new(0, 4),
                Tile::new(6, 4)
            ).unwrap()),
            MoveOutcome {
                captures: [Tile::new(6, 5)].into(),
                game_outcome: Some(Winner(Attacker))
            }
        );
        
        assert_eq!(
            test_game.get_move_outcome(Move::from_tiles(
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
            test_game.get_move_outcome(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 6)
            ).unwrap()),
            MoveOutcome {
                captures: [].into(),
                game_outcome: Some(Winner(Defender))
            }
        );
        
        assert_eq!(
            test_game.get_move_outcome(Move::from_tiles(
                Tile::new(6, 5),
                Tile::new(5, 5)
            ).unwrap()),
            MoveOutcome::default()
        )
    }
    
    #[test]
    fn test_real_games() {
        let f: PathBuf = [
            env!("CARGO_MANIFEST_DIR"),
            "resources",
            "test",
            "games",
            "copenhagen_hnefatafl.csv"
        ].iter().collect();
        let s = fs::read_to_string(f).unwrap();
        let lines = s.split('\n');
        for line in lines {
            if line.starts_with('#') {
                continue
            }
            println!("{line}");
            let mut g: Game<u128> = Game::new(
                COPENHAGEN_HNEFATAFL,
                "...ttttt...\n.....t.....\n...........\nt....T....t\nt...TTT...t\ntt.TTKTT.tt\nt...TTT...t\nt....T....t\n...........\n.....t.....\n...ttttt..."
            ).unwrap();
            let cols = line.split(',').collect::<Vec<&str>>();
            let moves = cols[0].split(' ').collect::<Vec<&str>>();
            for m_str in moves {
                let mc_opt = Move::from_str_with_captures(m_str);
                if let Ok((m, c)) = mc_opt {
                    assert_eq!(g.check_move_validity(m), Valid);
                    let m_outcome = g.get_move_outcome(m);
                    if !c.is_empty() {
                        assert_eq!(m_outcome.captures, c);
                    }
                    
                    let game_status = g.do_move(m);
                    assert!(game_status.is_ok());
                    

                } else {
                    assert_eq!(m_str, "timeout")
                }
            }
            
        }
    }

}