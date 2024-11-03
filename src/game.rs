use crate::board::{Board, Enclosure};
use crate::error::ParseError;
use crate::game::GameOutcome::Winner;
use crate::game::GameStatus::{Ongoing, Over};
use crate::game::InvalidMove::{BlockedByPiece, MoveOntoBlockedTile, MoveThroughBlockedTile, NoCommonAxis, NoPiece, OutOfBounds, TooFar};
use crate::game::MoveValidity::{Invalid, Valid};
use crate::pieces::PieceType::King;
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, Side};
use crate::rules::KingAttack::{Anvil, Armed, Hammer};
use crate::rules::KingStrength::{Strong, StrongByThrone, Weak};
use crate::rules::{Ruleset, ShieldwallRules, ThroneRule};
use crate::tiles::{Coords, Tile};
use crate::bitfield::BitField;
use crate::Axis::{Horizontal, Vertical};
use crate::InvalidMove::WrongPlayer;
use crate::PieceType::Soldier;
use crate::{Axis, PieceSet};
use std::cmp::PartialEq;
use std::collections::HashSet;
use std::str::FromStr;
use crate::board_state::BoardState;
use crate::play::{Play, PlayWithCaptures};

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
pub struct PlayOutcome {
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
pub struct Game<T: BoardState> {
    pub board: Board<T>,
    pub rules: Ruleset,
    pub turn: u32,
    pub side_to_play: Side,
    pub play_record: Vec<PlayWithCaptures>
}

impl<T: BoardState> Game<T> {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        Ok(Self {
            board: Board::from_str(starting_board)?,
            rules,
            turn: 0,
            side_to_play: if rules.attacker_starts { Attacker } else { Defender },
            play_record: Vec::new()
        })
    }
    
    /// Determine whether the given tile is hostile specifically by reference to the rules regarding
    /// hostility of special tiles.
    pub fn special_tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        (self.rules.hostility.throne.contains(piece) && tile == self.board.throne)
            || (self.rules.hostility.corners.contains(piece)
            && self.board.corners.contains(&tile))
            || (self.rules.hostility.edge.contains(piece)
            && !self.board.tile_in_bounds(tile))
    }

    /// Determine whether the given tile is hostile to the given piece.
    pub fn tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        if let Some(other_piece) = self.board.get_piece(tile) {
            // Tile contains a piece. If the piece is of a different side, tile is hostile, unless
            // that piece is an unarmed king.
            (other_piece.side != piece.side) && (
                other_piece.piece_type != King 
                    || self.rules.king_attack == Armed
                    || self.rules.king_attack == Anvil
            )
        } else {
            // Tile is empty. So it is only hostile if it is a special tile/edge and the rules state
            // that it is hostile to the given piece.
            self.special_tile_hostile(tile, piece)
        }
    }
    
    /// Determine whether the position at the given coordinates is hostile to the given piece,
    /// including whether the position is a hostile edge. 
    pub fn coords_hostile(&self, coords: Coords, piece: Piece) -> bool {
        if self.board.coords_in_bounds(coords) {
            self.tile_hostile(Tile::new(coords.row as u8, coords.col as u8), piece)
        } else {
            self.rules.hostility.edge.contains(piece)
        }
    }

    /// Check whether a move is valid. If the move is valid, returns `None`; otherwise, the wrapped
    /// [`InvalidMove`] variant indicates why the move is invalid.
    pub fn check_move_validity(&self, play: Play) -> MoveValidity {
        let from = play.from;
        let to = play.to();
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
                if (from.row != to.row) && (from.col != to.col) {
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
                if self.rules.slow_pieces.contains(piece) && play.distance() > 1 {
                    // Slow piece can't move more than one space at a time
                    return Invalid(TooFar)
                }
                Valid
            }
        }
    }

    /// Check whether the king is *currently* strong (must be surrounded on all four sides to be
    /// captured), considering the game rules and the king's current position (for example, the
    /// rules may provide that the king is only strong on or beside the throne). 
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
    pub fn get_game_outcome(&self, play: Play, caps: &HashSet<Tile>) -> Option<GameOutcome> {
        if caps.len() as u8 >= self.board.state.count_pieces(self.side_to_play.other()) {
            // All opposing pieces have been captured.
            return Some(Winner(self.side_to_play))
        } 
        if self.side_to_play == Attacker && caps.contains(&self.board.state.get_king()) {
            // Attacker has captured the king.
            return Some(Winner(Attacker))
        }
        if self.side_to_play == Defender {
            if self.board.is_king(play.from) && (
                (self.rules.edge_escape && self.board.tile_at_edge(play.to())) 
                    || (!self.rules.edge_escape && self.board.corners.contains(&play.to()))
            ) {
                // King has escaped.
                return Some(Winner(Defender))
            }
            if self.rules.exit_fort && self.detect_exit_fort() {
                return Some(Winner(Defender))
            }
        }
        None
    }
    
    /// Whether the tile (if any) at the given [`Coords`] can theoretically be occupied by the given
    /// piece according to the rules of the game. Does not take account of whether the tile is
    /// already occupied or actually accessible.
    pub fn coords_occupiable(&self, coords: Coords, piece: Piece) -> bool {
        if !self.board.coords_in_bounds(coords) {
            return false
        }
        let t = Tile::new(coords.row as u8, coords.col as u8);
        if self.board.throne == t && (
            (self.rules.throne_movement == ThroneRule::NoEntry) 
                || (self.rules.throne_movement == ThroneRule::KingEntry && piece.piece_type != King)
        ) {
            return false
        }
        if !self.rules.may_enter_corners.contains(piece) && self.board.corners.contains(&t) {
            return false
        }
        true
    }

    /// Check whether the given [`Enclosure`] is "secure", ie, no piece on its boundary is
    /// vulnerable to capture. If `inside_safe` is `true`, then any square inside the enclosure is
    /// considered not to be threatening to a boundary piece. Similarly, if `outside_safe` is
    /// `true`, then any square not inside the enclosure is considered not to be threatening to a
    /// boundary piece.
    pub fn enclosure_secure(&self, encl: &Enclosure, inside_safe: bool, outside_safe: bool) -> bool {
        if inside_safe && outside_safe {
            // If both inside and outside the enclosure are safe, then the enclosure must be secure.
            return true
        }
        for t in &encl.boundary {
            let piece = self.board.get_piece(*t)
                .expect("Boundary should not include empty tiles.");
            // It would be more efficient to just find the hostile piece once, and would usually
            // work the same. But technically a boundary could consist of pieces of different sides
            // (though I'm not sure why that would ever be necessary).
            let hostile_soldier = Piece::new(Soldier, piece.side.other());
            
            // Tile is unprotected along an axis if each neighbouring tile along that axis is not
            // deemed safe and either (a) is hostile; or (b) can be occupied by a hostile soldier
            // and is currently unoccupied.
            'axisloop: for axis in [Vertical, Horizontal] {
                for d in [-1, 1] {
                    let n_coords = Play::new(*t, axis, d).to_coords();
                    if let Ok(n_tile) = self.board.coords_to_tile(n_coords) {
                        let is_inside = encl.contains(&n_tile);
                        if (inside_safe && is_inside) || (outside_safe && !is_inside) {
                            // Tile is on a side of the boundary that is known to be safe (ie, no
                            // enemies). Therefore, it is safe unless it is a hostile tile.
                            if !self.special_tile_hostile(n_tile, piece) {
                                continue 'axisloop;
                            }
                        }
                        if (!self.tile_hostile(n_tile, piece)) && (
                            self.board.tile_occupied(n_tile)
                                || !self.coords_occupiable(n_coords, hostile_soldier)
                        ) {
                            // Tile is not hostile, AND is either occupied (by a friendly piece) or
                            // is not occupiable by a hostile piece according to the game rules
                            continue 'axisloop;
                        }
                    } else {
                        // Coords are out of bounds
                        if !self.rules.hostility.edge.contains(piece) {
                            // Piece is at edge and edge is not hostile
                            continue 'axisloop;
                        }
                    }
                }
                return false
            }
        }
        true
    }
    
    
    /// A method used internally by [`Game::detect_shieldwall`]. This method searches in one
    /// direction (along the relevant edge) to find a valid shieldwall. Returns `None` if no
    /// shieldwall found or, otherwise, a set of all tiles caught in the shieldwall (**not** 
    /// necessarily all tiles *captured* by the shieldwall, as some tiles may be occupied by pieces
    /// that cannot be captured in a shieldwall).
    fn dir_sw_search(
        &self,
        play: Play,
        sw_rule: ShieldwallRules,
        axis: Axis,
        away_from_edge: i8,
        dir: i8
    ) -> Option<HashSet<Tile>> {
        let mut t = play.to();
        // Key is an occupied tile at edge of the board (which is threatened with capture);
        // value is the tile, on the opposite side to the edge, occupied by the opposing piece.
        let mut wall: HashSet<Tile> = HashSet::new();
        loop {
            let step = Play::new(t, axis, dir);
            // Move one tile along the edge
            t = step.to();
            if !self.board.tile_in_bounds(t) {
                // We have reached the edge of the board without finding a closing piece.
                // No shieldwall.
                return None
            }
            if !(
                self.board.tile_occupied(t) 
                    || sw_rule.corners_may_close 
                    && self.board.corners.contains(&t)
            ) {
                // We have encountered a tile that is not occupied and is not a corner that may
                // close. No shieldwall.
                return None
            }
            let piece_opt = self.board.get_piece(t);
            if piece_opt.is_none() {
                // We have already broken out of this loop if the tile is unoccupied, unless it
                // is a closing corner. Therefore, if `piece` is `None`, we must be at a
                // closing corner.
                return if wall.len() < 2 { None } else { Some(wall) };
            }
            let piece = piece_opt.expect("Tile should be occupied.");
            if piece.side == self.side_to_play.other() {
                let pin = Play::new(
                    t,
                    axis.other(),
                    away_from_edge
                ).to();
                if let Some(p) = self.board.get_piece(pin) {
                    if p.side == self.side_to_play {
                        wall.insert(t);
                    } else {
                        // Piece is pinned against edge by friendly piece (no shieldwall)
                        return None
                    }
                } else {
                    // Piece isn't pinned against the wall by any piece (no shieldwall)
                    return None
                }
            }
            if (piece.side == self.side_to_play) ||
                (self.board.corners.contains(&t) && sw_rule.corners_may_close) {
                // We've found a friendly piece or a corner that may close.
                return if wall.len() < 2 { None } else { Some(wall) };
            }
        }
    }

    /// Detect whether the given move has created a shieldwall according to the applicable rules.
    /// Returns `None` if no shieldwall is detected; otherwise returns a set of tiles that have been
    /// captured in the shieldwall.
    fn detect_shieldwall(&self, play: Play) -> Option<HashSet<Tile>> {
        let sw_rule = self.rules.shieldwall?;
        let to = play.to();
        let (axis, away_from_edge) = if to.row == 0 {
            (Horizontal, 1i8)
        } else if to.row == self.board.side_len - 1 {
            (Horizontal, -1)
        } else if to.col == 0 {
            (Vertical, 1)
        } else if to.col == self.board.side_len - 1 {
            (Vertical, -1)
        } else {
            // The move that leads to a shieldwall capture must be flanking the shieldwall,
            // therefore must be a move to the edge.
            return None
        };
        let mut wall = self.dir_sw_search(play, sw_rule, axis, away_from_edge, -1);
        if wall.is_none() {
            wall = self.dir_sw_search(play, sw_rule, axis, away_from_edge, 1);
        }
        if let Some(w) = wall {
            if w.len() < 2 {
                // Can't capture 0 or 1 pieces with a shieldwall
                return None
            }
            // We've found a shieldwall. Filter out tiles which contain pieces which cannot
            // be captured in a shieldwall.
            Some(w.into_iter().filter(|t| sw_rule.captures.contains(
                self.board.get_piece(*t)
                    .expect("Tile in shieldwall should be occupied."))
            ).collect())
        } else {
            None
        }
    }
    
    /// Detect whether the king is in an exit fort.
    pub fn detect_exit_fort(&self) -> bool {
        let king_tile = self.board.get_king();
        
        // King is at edge
        if !self.board.tile_in_bounds(king_tile) {
            return false
        }
        
        // Check king is enclosed by his own pieces
        if let Some(encl) = self.board.find_enclosure(
            king_tile,
            PieceSet::from(King),
            PieceSet::from(Defender),
            false,
            true
        ) {
            // King has space to move
            if !self.board.neighbors(king_tile).iter().any(|t| !self.board.tile_occupied(*t)) {
                return false
            }
            // Check enclosing pieces are all themselves safe
            if !self.enclosure_secure(&encl, true, false) {
                return false
            }
            true
        } else {
            false
        }
        
    }

    /// Get the outcome of a move (number of captures, whether it ends the game, etc).
    pub fn get_play_outcome(&self, play: Play) -> PlayOutcome {
        let mut captures: HashSet<Tile> = HashSet::new();
        let occupant = self.board.get_piece(play.from);
        if occupant.is_none() {
            return PlayOutcome { captures, game_outcome: None }
        }
        let mover = occupant.expect("Moving piece must not be None");
        let to = play.to();
        
        // Detect normal captures
        if mover.piece_type != King 
            || self.rules.king_attack == Armed 
            || self.rules.king_attack == Hammer {
            for n in self.board.neighbors(to) {
                if let Some(other_piece) = self.board.get_piece(n) {
                    if other_piece.side == mover.side {
                        // Friendly neighbour so no possibility for capture
                        continue
                    }
                    let signed_to_row = to.row as i8;
                    let signed_to_col = to.col as i8;
                    let signed_n_row = n.row as i8;
                    let signed_n_col = n.col as i8;
                    let signed_far_row = signed_to_row + ((signed_n_row - signed_to_row) * 2);
                    let signed_far_col = signed_to_col + ((signed_n_col - signed_to_col) * 2);
                    let far_coords = Coords { row: signed_far_row, col: signed_far_col };
                    // Check if the tile on the other side of the neighbour is a hostile tile, or if
                    // the neighbour is on the edge and the edge is treated as hostile to that piece
                    if self.coords_hostile(far_coords, other_piece) {
                        // We know that the neighbouring opposing piece is surrounded by the
                        // moving piece and another hostile tile. So it is captured, *unless* it
                        // is a strong king.
                        if (other_piece.piece_type == King) && self.king_is_strong() {
                            // Get the tiles surrounding `n` on the perpendicular axis.
                            let n_coords = Coords::from(n);
                            let perp_hostile= if to.row == n.row {
                                self.coords_hostile(n_coords + (1, 0), other_piece)
                                    && self.coords_hostile(n_coords + (-1, 0), other_piece)
                            } else {
                                self.coords_hostile(n_coords + (0, 1), other_piece)
                                    && self.coords_hostile( n_coords + (0, -1), other_piece)
                            };
                            if !perp_hostile {
                                continue
                            }
                        }
                        captures.insert(n);
                    }
                }
            }

        }

        // Detect shieldwall captures
        if let Some(walled) = self.detect_shieldwall(play) {
            captures.extend(walled);
        }

        let game_outcome = self.get_game_outcome(play, &captures);
        PlayOutcome { captures, game_outcome }
    }
    
    /// Actually "do" a move, checking validity, getting outcome, applying outcome to board state,
    /// switching side to play and returning a description of the game status following the move.
    pub fn do_move(&mut self, play: Play) -> Result<GameStatus, InvalidMove> {
        if let Invalid(v) = self.check_move_validity(play) {
            return Err(v)
        };
        let play_outcome = self.get_play_outcome(play);
        self.board.move_piece(play.from, play.to());
        for &c in &play_outcome.captures {
            self.board.remove_piece(c)
        }

        self.turn += 1;
        self.side_to_play = self.side_to_play.other();
        self.play_record.push(PlayWithCaptures { play, captures: play_outcome.captures });
        Ok(match play_outcome.game_outcome {
            Some(outcome) => Over(outcome),
            None => Ongoing
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::game::GameOutcome::Winner;
    use crate::game::GameStatus::Over;
    use crate::game::InvalidMove::{
        BlockedByPiece,
        MoveOntoBlockedTile,
        MoveThroughBlockedTile,
        NoPiece,
        OutOfBounds,
        TooFar
    };
    use crate::game::MoveValidity::{Invalid, Valid};
    use crate::game::{Game, GameStatus, PlayOutcome};
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::King;
    use crate::pieces::Side::{Attacker, Defender};
    use crate::rules::ThroneRule::NoPass;
    use crate::rules::{Ruleset, ShieldwallRules, COPENHAGEN_HNEFATAFL, FEDERATION_BRANDUBH};
    use crate::tiles::Tile;
    use crate::PieceType::Soldier;
    use crate::{hashset, HostilityRules, MediumBoardState, Piece, SmallBoardState};
    use std::collections::HashSet;
    use std::fs;
    use std::path::PathBuf;
    use std::str::FromStr;
    use crate::play::{Play, PlayWithCaptures};

    const TEST_RULES: Ruleset = Ruleset {
        slow_pieces: PieceSet::from_piece_type(King),
        throne_movement: NoPass,
        ..FEDERATION_BRANDUBH
    };
    
    #[test]
    fn test_check_move_validity() {
        let mut fb_game: Game<SmallBoardState> = Game::new(
            FEDERATION_BRANDUBH,
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();

        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(3, 1),
                Tile::new(4, 1)
            ).unwrap()),
            Valid
        );
        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 0)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );
        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(1, 1),
                Tile::new(2, 1)
            ).unwrap()),
            Invalid(NoPiece)
        );
        
        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 7)
            ).unwrap()),
            Invalid(OutOfBounds)
        );

        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(2, 3)
            ).unwrap()),
            Invalid(BlockedByPiece)
        );
        
        fb_game.do_move(Play::from_tiles(
            Tile::new(3, 1),
            Tile::new(4, 1)
        ).unwrap()).unwrap();
        
        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(3, 3),
                Tile::new(3, 2)
            ).unwrap()),
            Valid
        );

        fb_game.board.move_piece(Tile::new(3, 2), Tile::new(4, 2));
        fb_game.board.move_piece(Tile::new(3, 3), Tile::new(3, 2));

        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(2, 3),
                Tile::new(3, 3)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );

        assert_eq!(
            fb_game.check_move_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 3)
            ).unwrap()),
            Valid
        );
        
        let mut test_game: Game<SmallBoardState> = Game::new(
            TEST_RULES,
            ".......\n.....Tt\n..T....\n..t..t.\nTt....T\n..t....\n..T..K."
        ).unwrap();
        
        test_game.side_to_play = Defender;
        
        assert_eq!(
            test_game.check_move_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 3)
            ).unwrap()),
            Invalid(TooFar)
        );
        
        assert_eq!(
            test_game.check_move_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 4)
            ).unwrap()),
            Valid
        );
        
        test_game.side_to_play = Attacker;
        
        assert_eq!(
            test_game.check_move_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 4)
            ).unwrap()),
            Invalid(MoveThroughBlockedTile)
        );
    }
    
    #[test]
    fn test_check_play_outcome() {
        let mut test_game: Game<SmallBoardState> = Game::new(
            TEST_RULES,
            "....t..\n.....Tt\n..T....\n..t..t.\nTt....T\n..t....\n..T..K."
        ).unwrap();
        
        assert_eq!(
            test_game.get_play_outcome(Play::from_tiles(
                Tile::new(0, 4),
                Tile::new(6, 4)
            ).unwrap()),
            PlayOutcome {
                captures: [Tile::new(6, 5)].into(),
                game_outcome: Some(Winner(Attacker))
            }
        );
        
        assert_eq!(
            test_game.get_play_outcome(Play::from_tiles(
                Tile::new(4, 6),
                Tile::new(4, 2)
            ).unwrap()),
            PlayOutcome {
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
            test_game.get_play_outcome(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 6)
            ).unwrap()),
            PlayOutcome {
                captures: [].into(),
                game_outcome: Some(Winner(Defender))
            }
        );
        
        assert_eq!(
            test_game.get_play_outcome(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(5, 5)
            ).unwrap()),
            PlayOutcome::default()
        )
    }

    #[test]
    fn test_shieldwalls() {

        let no_corner_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::from(Soldier)
            }),
            ..COPENHAGEN_HNEFATAFL
        };

        let king_capture_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::all()
            }),
            ..COPENHAGEN_HNEFATAFL
        };

        let corner_sw = [
            ".........",
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            ".......tT",
            ".......tT",
            ".........",
        ].join("\n");
        let regular_sw = [
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            ".......tT",
            ".......tT",
            "........t",
            ".........",
        ].join("\n");
        let regular_sw_king = [
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            ".......tK",
            ".......tT",
            "........t",
            ".........",
        ].join("\n");
        let no_sw_gap = [
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            "........T",
            ".......tT",
            "........t",
            ".........",
        ].join("\n");
        let no_sw_friend = [
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            "......tTT",
            ".......tT",
            "........t",
            ".........",
        ].join("\n");
        let no_sw_small = [
            ".........",
            ".........",
            ".........",
            "......t..",
            ".......tT",
            "........t",
            ".........",
            ".........",
            ".........",
        ].join("\n");

        let cm = Play::from_tiles(
            Tile::new(4, 6),
            Tile::new(4, 8)
        ).unwrap();
        let m = Play::from_tiles(
            Tile::new(3, 6),
            Tile::new(3, 8)
        ).unwrap();
        let n = Play::from_tiles(
            Tile::new(3, 6),
            Tile::new(3, 7)
        ).unwrap();

        let g_corner: Game<MediumBoardState> = Game::new(COPENHAGEN_HNEFATAFL, &corner_sw).unwrap();
        assert_eq!(g_corner.detect_shieldwall(n), None);
        assert_eq!(g_corner.detect_shieldwall(cm), Some(hashset!(
            Tile::new(5, 8),
            Tile::new(6, 8),
            Tile::new(7, 8)
        )));
        
        let g_no_corner: Game<MediumBoardState> = Game::new(no_corner_rules, &corner_sw).unwrap();
        assert_eq!(g_no_corner.detect_shieldwall(m), None);
        
        let g_regular: Game<MediumBoardState> = Game::new(no_corner_rules, &regular_sw).unwrap();
        assert_eq!(g_regular.detect_shieldwall(m), Some(hashset!(
            Tile::new(4, 8),
            Tile::new(5, 8),
            Tile::new(6, 8)
        )));
        
        let g_king: Game<MediumBoardState> = Game::new(no_corner_rules, &regular_sw_king).unwrap();
        assert_eq!(g_king.detect_shieldwall(m), Some(hashset!(
            Tile::new(4, 8),
            Tile::new(6, 8)
        )));
        
        let g_gap: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_gap).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);

        let g_friend: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_friend).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);

        let g_small: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_small).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);
    }
    
    #[test]
    fn test_encl_secure() {
        let setup_1 = [
            ".......",
            "..ttt..",
            ".t.K.t.",
            "..ttt..",
            "......."
        ].join("\n");
        let setup_2 = [
            ".......",
            ".tttt..",
            ".t.K.t.",
            "..tttt.",
            "......."
        ].join("\n");
        let setup_3 = [
            "..t.t..",
            ".t.t.t.",
            ".t.K.t.",
            "..ttt..",
            "......."
        ].join("\n");
        let setup_4 = [
            "..t..t.",
            ".t...t.",
            ".t.K.t.",
            "..ttt..",
            "......."
        ].join("\n");
        
        let safe_corners = Ruleset {
            hostility: HostilityRules {
                corners: PieceSet::none(),
                edge: PieceSet::none(),
                throne: PieceSet::none()
            },
            ..COPENHAGEN_HNEFATAFL
        };
        
        let candidates = [
            // string, inside_safe, outside_safe, is_secure, rules
            (&setup_1, false, true, true, COPENHAGEN_HNEFATAFL),
            (&setup_1, false, false, false, COPENHAGEN_HNEFATAFL),
            (&setup_2, false, true, true, COPENHAGEN_HNEFATAFL),
            (&setup_2, true, false, true, COPENHAGEN_HNEFATAFL),
            (&setup_3, false, true, false, COPENHAGEN_HNEFATAFL),
            (&setup_4, false, true, false, COPENHAGEN_HNEFATAFL),
            (&setup_4, false, true, true, safe_corners),
            (&setup_4, true, false, true, COPENHAGEN_HNEFATAFL),
        ];
        for (string, inside_safe, outside_safe, is_secure, rules) in candidates {
            let g: Game<MediumBoardState> = Game::new(rules, string).unwrap();
            let encl_opt = g.board.find_enclosure(
                Tile::new(2, 3),
                PieceSet::from(King),
                PieceSet::from(Piece::new(Soldier, Attacker)),
                false, false
            );
            assert!(encl_opt.is_some());
            let encl = encl_opt.unwrap();
            assert_eq!(g.enclosure_secure(&encl, inside_safe, outside_safe), is_secure)
        }

    }
    
    #[test]
    fn test_exit_forts() {
        let exit_fort_flat = [
            ".........",
            ".........",
            "........t",
            ".......tT",
            ".......T.",
            "......tT.",
            ".......TK",
            ".......tT",
            ".........",
        ].join("\n");
        let exit_fort_bulge = [
            ".........",
            ".........",
            ".........",
            ".........",
            ".........",
            ".....TTTT",
            ".....T..K",
            "......TTT",
            ".........",
        ].join("\n");
        let no_fort_enemy = [
            ".........",
            ".........",
            ".........",
            "........T",
            ".......Tt",
            ".......T.",
            ".......TK",
            "........T",
            ".........",
        ].join("\n");
        let no_fort_unfree = [
            ".........",
            ".........",
            ".........",
            "........T",
            ".......TT",
            ".......TT",
            ".......TK",
            "........T",
            ".........",
        ].join("\n");
        let no_fort_gap = [
            ".........",
            ".........",
            ".........",
            "........T",
            ".........",
            "....t..T.",
            ".......TK",
            "........T",
            ".........",
        ].join("\n");
        let no_fort_vuln = [
            ".........",
            ".........",
            ".........",
            ".........",
            ".........",
            "......TTT",
            ".....T..K",
            "......TTT",
            ".........",
        ].join("\n");
        for s in [exit_fort_flat, exit_fort_bulge] {
            let g: Game<MediumBoardState> = Game::new(COPENHAGEN_HNEFATAFL, &s).unwrap();
            assert!(g.detect_exit_fort());
        }
        for s in [no_fort_enemy, no_fort_unfree, no_fort_gap] {
            let g: Game<MediumBoardState> = Game::new(COPENHAGEN_HNEFATAFL, &s).unwrap();
            assert!(!g.detect_exit_fort());
        }
    }
    
    fn test_real_games(rules: Ruleset, starting_posn: &str, fname: &str) {
        let f: PathBuf = [
            env!("CARGO_MANIFEST_DIR"),
            "resources",
            "test",
            "games",
            fname
        ].iter().collect();
        let s = fs::read_to_string(f).unwrap();
        let lines = s.split('\n');
        let mut last_game_status: GameStatus = GameStatus::Ongoing;
        for line in lines {
            if line.starts_with('#') {
                continue
            }
            let mut g: Game<MediumBoardState> = Game::new(
                rules,
                starting_posn
            ).unwrap();
            let cols = line.split(',').collect::<Vec<&str>>();
            let outcome = cols.last().unwrap();
            if outcome.is_empty() {
                continue
            }
            let plays = cols[0].split(' ').collect::<Vec<&str>>();
            for p_str in plays {
                let pr_opt = PlayWithCaptures::from_str(p_str);
                if let Ok(PlayWithCaptures { play: p, captures: c }) = pr_opt {
                    assert_eq!(g.check_move_validity(p), Valid);
                    let m_outcome = g.get_play_outcome(p);
                    if !c.is_empty() {
                        // Test data doesn't report capture of king as a capture using "x" notation
                        let without_king: HashSet<Tile> = m_outcome.captures.iter()
                            .filter(|t| !g.board.is_king(**t))
                            .map(|t| t.to_owned())
                            .collect();
                        assert_eq!(without_king, c);
                    }
                    
                    let game_status_res = g.do_move(p);
                    assert!(game_status_res.is_ok());
                    last_game_status = game_status_res.unwrap();
                } else {
                    assert_eq!(p_str, "timeout")
                }
            }

            if let Over(Winner(side)) = last_game_status {
                let expected = match side {
                    Attacker => "Black",
                    Defender => "White"
                };
                assert_eq!(&expected, outcome);
            }
        }
    }
    
    #[test]
    fn test_real_copenhagen() {
        test_real_games(
            COPENHAGEN_HNEFATAFL,
            &[
                "...ttttt...",
                ".....t.....",
                "...........",
                "t....T....t",
                "t...TTT...t",
                "tt.TTKTT.tt",
                "t...TTT...t",
                "t....T....t",
                "...........",
                ".....t.....",
                "...ttttt..."
            ].join("\n"),
            "copenhagen_hnefatafl.csv"
        )
    }
    
    #[test]
    fn test_real_brandubh() {
        test_real_games(
            FEDERATION_BRANDUBH,
            &[
                "...t...",
                "...t...",
                "...T...",
                "ttTKTtt",
                "...T...",
                "...t...",
                "...t..."
            ].join("\n"),
            "brandubh.csv"
        )
    }

}