use crate::board::{Board, Enclosure};
use crate::board_state::BoardState;
use crate::error::InvalidPlay::{GameOver, WrongPlayer};
use crate::error::{BoardError, InvalidPlay, ParseError};
use crate::game::GameOutcome::Winner;
use crate::game::GameStatus::{Ongoing, Over};
use crate::game::InvalidPlay::{BlockedByPiece, MoveOntoBlockedTile, MoveThroughBlockedTile, NoCommonAxis, NoPiece, OutOfBounds, TooFar};
use crate::game::MoveValidity::{Invalid, Valid};
use crate::game::WinReason::{AllCaptured, Enclosed, ExitFort, KingCaptured, KingEscaped, NoMoves};
use crate::pieces::PieceType::King;
use crate::pieces::Side::{Attacker, Defender};
use crate::pieces::{Piece, Side};
use crate::play::{Play, PlayRecord};
use crate::rules::EnclosureWinRules::WithoutEdgeAccess;
use crate::rules::KingAttack::{Anvil, Armed, Hammer};
use crate::rules::KingStrength::{Strong, StrongByThrone, Weak};
use crate::rules::ThroneRule::{KingEntry, KingPass, NoEntry, NoPass, NoThrone};
use crate::rules::{RepetitionRule, Ruleset, ShieldwallRules};
use crate::tiles::{AxisOffset, Coords, RowColOffset, Tile};
use crate::Axis::{Horizontal, Vertical};
use crate::GameOutcome::Draw;
use crate::PieceType::Soldier;
use crate::{Axis, PieceSet};
use std::cmp::PartialEq;
use std::collections::HashSet;
use std::str::FromStr;
use crate::game_state::RepetitionTracker;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum WinReason {
    /// King has escaped in the "normal" way, ie, by reaching an edge or corner.
    KingEscaped,
    /// King has escaped through an exit fort.
    ExitFort,
    /// King has been captured.
    KingCaptured,
    /// All the other side's pieces have been captured.
    AllCaptured,
    /// The other side is completely enclosed (with or without edge or corner access, depending on
    /// the rules).
    Enclosed,
    /// The other side has no legal moves available.
    NoMoves,
    /// The other side has repeated a move too many times.
    Repetition
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum DrawReason {
    /// A move has been repeated too many times.
    Repetition
}

/// The outcome of a single game.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum GameOutcome {
    /// Game has been won by the specified side.
    Winner(WinReason, Side),
    /// Game has ended in a draw.
    Draw(DrawReason)
}

/// A struct describing the outcome of a single move.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct PlayOutcome {
    /// Tiles containing pieces that have been captured by the move.
    pub captures: HashSet<Tile>,
    /// The outcome of the game, if the move has brought the game to an end.
    pub game_outcome: Option<GameOutcome>
}

/// The current status of the game.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
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
    Invalid(InvalidPlay)
}

/// A struct representing a single game, including all state and associated information (such as
/// rules) needed to play.
#[derive(Clone)]
pub struct Game<T: BoardState> {
    pub board: Board<T>,
    pub rules: Ruleset,
    pub turn: u32,
    pub side_to_play: Side,
    pub play_history: Vec<PlayRecord>,
    pub status: GameStatus,
    pub repetition_tracker: RepetitionTracker
}

impl<T: BoardState> Game<T> {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        Ok(Self {
            board: Board::from_str(starting_board)?,
            rules,
            turn: 0,
            side_to_play: rules.starting_side,
            play_history: Vec::new(),
            status: Ongoing,
            repetition_tracker: RepetitionTracker::default(),
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

    /// Whether the given piece can occupy or pass the given tile according to the game rules and
    /// current board state. Returns a pair of `bool`s indicating whether the piece can occupy or
    /// pass, respectively.
    pub fn can_occupy_or_pass(&self, play: Play, piece: Piece) -> (bool, bool) {
        let validity = self.check_play_validity_for_side(play, piece.side);
        match validity {
            Valid => (true, true),
            Invalid(MoveOntoBlockedTile) => {
                // Generally, the only way you could be unable to move onto a tile but be able to
                // move past it is if the tile is a throne and the rules permit passing through, but
                // not occupying, the throne. Of course, this will differ for knights and commanders
                // when implemented.
                if play.to() == self.board.throne {
                    match self.rules.throne_movement {
                        NoThrone => (true, true),
                        NoPass => (false, false),
                        NoEntry => (false, true),
                        KingPass => {
                            let is_king = piece.piece_type == King;
                            (is_king, is_king)
                        },
                        KingEntry => (piece.piece_type == King, true),
                    }
                } else {
                    // If special tile is not a throne, it must be a corner, so cannot be passed.
                    (false, false)
                }
            },
            _ => {
                (false, false)
            }
        }
    }

    /// Check whether a play is valid for the given side.
    pub fn check_play_validity_for_side(&self, play: Play, side: Side) -> MoveValidity {
        if self.status != Ongoing {
            return Invalid(GameOver)
        }
        let from = play.from;
        let to = play.to();
        let maybe_piece = self.board.get_piece(from);
        match maybe_piece {
            None => Invalid(NoPiece),
            Some(piece) => {
                if piece.side != side {
                    return Invalid(WrongPlayer);
                }
                if !(self.board.tile_in_bounds(from) && self.board.tile_in_bounds(to)) {
                    return Invalid(OutOfBounds)
                }
                if (from.row != to.row) && (from.col != to.col) {
                    return Invalid(NoCommonAxis)
                }
                if self.board.tile_occupied(to) {
                    return Invalid(BlockedByPiece)
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
                    (self.rules.throne_movement == NoPass)
                        || ((self.rules.throne_movement == KingPass)
                        && piece.piece_type != King)
                ) && between.contains(&self.board.throne) {
                    return Invalid(MoveThroughBlockedTile)
                }
                if ((self.rules.throne_movement == NoEntry)
                    || ((self.rules.throne_movement == KingEntry)
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

    /// Check whether a move is valid.
    pub fn check_play_validity(&self, play: Play) -> MoveValidity {
        self.check_play_validity_for_side(play, self.side_to_play)
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

    /// Whether the tile (if any) at the given [`Coords`] can theoretically be occupied by the given
    /// piece according to the rules of the game. Does not take account of whether the tile is
    /// already occupied or actually accessible.
    pub fn coords_occupiable(&self, coords: Coords, piece: Piece) -> bool {
        if !self.board.coords_in_bounds(coords) {
            return false
        }
        let t = Tile::new(coords.row as u8, coords.col as u8);
        if self.board.throne == t && (
            (self.rules.throne_movement == NoEntry)
                || (self.rules.throne_movement == KingEntry && piece.piece_type != King)
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
                    let n_coords = Play::new(*t, AxisOffset::new(axis, d)).to_coords();
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
            let step = Play::new(t, AxisOffset::new(axis, dir));
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
                let pin = Play::new(t, AxisOffset::new(axis.other(), away_from_edge)).to();
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
        if !self.board.tile_at_edge(king_tile) {
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

    /// Get the tiles containing pieces captured by the given play.
    pub fn get_captures(&self, play: Play, moving_piece: Piece) -> HashSet<Tile> {
        let mut captures: HashSet<Tile> = HashSet::new();
        let to = play.to();
        
        // Detect normal captures
        if moving_piece.piece_type != King 
            || self.rules.king_attack == Armed 
            || self.rules.king_attack == Hammer {
            for n in self.board.neighbors(to) {
                if let Some(other_piece) = self.board.get_piece(n) {
                    if other_piece.side == moving_piece.side {
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
                                self.coords_hostile(
                                    n_coords + RowColOffset::new(1, 0),
                                    other_piece
                                ) && self.coords_hostile(
                                    n_coords + RowColOffset::new(-1, 0),
                                    other_piece
                                )
                            } else {
                                self.coords_hostile(
                                    n_coords + RowColOffset::new(0, 1),
                                    other_piece
                                ) && self.coords_hostile( 
                                    n_coords + RowColOffset::new(0, -1),
                                    other_piece
                                )
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
        captures

    }

    /// Get the outcome of the game, if any. If None, the game is still ongoing.
    pub fn get_game_outcome(
        &self,
        play: Play,
        moving_piece: Piece,
        caps: &HashSet<Tile>
    ) -> Option<GameOutcome> {
        if self.board.state.count_pieces(self.side_to_play.other()) == 0 {
            // All opposing pieces have been captured.
            return Some(Winner(AllCaptured, self.side_to_play))
        }
        if self.side_to_play == Attacker {
            // This test relies on the fact that even once the king has been removed from the board,
            // the bits at the end that encode its position remain set.
            if caps.contains(&self.board.state.get_king()) {
                // Attacker has captured the king.
                return Some(Winner(KingCaptured, Attacker))
            }
            if let Some(encl_win) = self.rules.enclosure_win {
                if let Some(encl) = self.board.find_enclosure(
                    self.board.get_king(),
                    PieceSet::from(Defender),
                    PieceSet::from(Attacker),
                    encl_win == WithoutEdgeAccess,
                    true
                ) {
                    if encl.occupied.len() == self.board.state.count_pieces(Defender) as usize
                        && self.enclosure_secure(&encl, false, true) {
                        return Some(Winner(Enclosed, Attacker))
                    }
                }
            }
        } else {
            if moving_piece.piece_type == King && (
                (self.rules.edge_escape && self.board.tile_at_edge(play.to()))
                    || (!self.rules.edge_escape && self.board.corners.contains(&play.to()))
            ) {
                // King has escaped.
                return Some(Winner(KingEscaped, Defender))
            }
            if self.rules.exit_fort && self.detect_exit_fort() {
                // King has escaped through exit fort.
                return Some(Winner(ExitFort, Defender))
            }
        }
        
        if let Some(RepetitionRule { n_repetitions, is_loss }) = self.rules.repetition_rule {
            if self.repetition_tracker.get_repetitions(self.side_to_play) >= n_repetitions {
                // Loss or draw as a result of repeated moves.
                return if is_loss {
                    Some(Winner(WinReason::Repetition, self.side_to_play.other()))
                } else {
                    Some(Draw(DrawReason::Repetition))
                }
            } 
        }
        
        if !self.side_can_play(self.side_to_play.other()) {
            // Other side has no playable moves.
            return Some(Winner(NoMoves, self.side_to_play))
        } 
        
        None
    }


    /// Actually "do" a move, checking validity, getting outcome, applying outcome to board state,
    /// switching side to play and returning a description of the game status following the move.
    pub fn do_move(&mut self, play: Play) -> Result<GameStatus, InvalidPlay> {
        if let Invalid(v) = self.check_play_validity(play) {
            return Err(v)
        };
        
        // First move the piece on the board
        let moving_piece = self.board.move_piece(play.from, play.to());
        // Then remove captured pieces
        let captures = self.get_captures(play, moving_piece);
        for &c in &captures {
            self.board.remove_piece(c)
        }
        self.repetition_tracker.track_play(self.side_to_play, play, !captures.is_empty());
        // Then assess the game outcome
        let game_outcome = self.get_game_outcome(play, moving_piece, &captures);
        
        self.turn += 1;
        let game_status = match game_outcome {
            Some(game_outcome) => Over(game_outcome),
            None => Ongoing
        };
        let record = PlayRecord {
            side: self.side_to_play,
            play,
            outcome: PlayOutcome { captures, game_outcome },
        };
        
        self.side_to_play = self.side_to_play.other();
        self.play_history.push(record);
        self.status = game_status;
        Ok(game_status)
    }

    /// Iterate over the possible plays that can be made by the piece at the given tile. Returns an
    /// error if there is no piece at the given tile. Order of iteration is not guaranteed.
    pub fn iter_plays(&self, tile: Tile) -> Result<PlayIterator<T>, BoardError> {
        PlayIterator::new(self, tile)
    }
    
    /// Whether the given side could make any play given the current board.
    pub fn side_can_play(&self, side: Side) -> bool {
        for tile in self.board.state.iter_occupied(side) {
            if self.iter_plays(tile).expect("Tile must not be empty.").next().is_some() {
                return true
            }
        }
        false
    }
}

/// An iterator over the possible plays that can be made by the piece at the given tile. Note that
/// because this struct holds a reference to the [`Game`], the game may not be mutated while the
/// iterator exists. Order of iteration is not guaranteed.
pub struct PlayIterator<'a, T: BoardState> {
    game: &'a Game<T>,
    start_tile: Tile,
    piece: Piece,
    movement: AxisOffset,
}

impl<'a, T: BoardState> PlayIterator<'a, T> {

    pub fn new(game: &'a Game<T>, tile: Tile) -> Result<Self, BoardError> {
        if let Some(piece) = game.board.get_piece(tile) {
            Ok(Self {
                game,
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

impl<'a, T: BoardState> Iterator for PlayIterator<'a, T> {
    type Item = Play;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            //let dest_coords = Coords::from(self.current_tile) + self.direction;
            let play = Play::new(self.start_tile, self.movement);
            if let Ok(dest_tile) = self.game.board.coords_to_tile(play.to_coords()) {
                // New tile is in bounds

                // Increase the step for the next iteration.
                self.movement.displacement +=
                    if self.movement.displacement.is_positive() { 1 } else { -1 };
                let (can_occupy, can_pass) = self.game.can_occupy_or_pass(play, self.piece);
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

#[cfg(test)]
mod tests {
    use crate::game::Game;
    use crate::game::GameOutcome::Winner;
    use crate::game::InvalidPlay::{
        BlockedByPiece,
        MoveOntoBlockedTile,
        MoveThroughBlockedTile,
        NoPiece,
        OutOfBounds,
        TooFar
    };
    use crate::game::MoveValidity::{Invalid, Valid};
    use crate::game::WinReason::{KingCaptured, KingEscaped, Repetition};
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::King;
    use crate::pieces::Side::{Attacker, Defender};
    use crate::play::Play;
    use crate::preset::{boards, rules};
    use crate::rules::ThroneRule::NoPass;
    use crate::rules::{Ruleset, ShieldwallRules};
    use crate::tiles::Tile;
    use crate::GameStatus::{Ongoing, Over};
    use crate::PieceType::Soldier;
    use crate::{hashset, HostilityRules, MediumBoardState, Piece, SmallBoardState};
    use std::collections::HashSet;
    use std::str::FromStr;
    use crate::board_state::{BoardState, HugeBoardState, LargeBoardState};

    const TEST_RULES: Ruleset = Ruleset {
        slow_pieces: PieceSet::from_piece_type(King),
        throne_movement: NoPass,
        ..rules::BRANDUBH
    };
    

    fn generic_test_play_validity<T: BoardState>() {
        let mut fb_game: Game<T> = Game::new(
            rules::BRANDUBH,
            boards::BRANDUBH
        ).unwrap();

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 1),
                Tile::new(4, 1)
            ).unwrap()),
            Valid
        );
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 0)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(1, 1),
                Tile::new(2, 1)
            ).unwrap()),
            Invalid(NoPiece)
        );
        
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 7)
            ).unwrap()),
            Invalid(OutOfBounds)
        );

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(2, 3)
            ).unwrap()),
            Invalid(BlockedByPiece)
        );

        fb_game.do_move(Play::from_tiles(
            Tile::new(3, 1),
            Tile::new(4, 1)
        ).unwrap()).unwrap();

        let play = Play::from_tiles(
            Tile::new(3, 3),
            Tile::new(3, 2)
        ).unwrap();
        assert_eq!(
            fb_game.check_play_validity(play),
            Invalid(BlockedByPiece)
        );

        fb_game.board.move_piece(Tile::new(3, 2), Tile::new(4, 2));
        fb_game.board.move_piece(Tile::new(3, 3), Tile::new(3, 2));

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(2, 3),
                Tile::new(3, 3)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 3)
            ).unwrap()),
            Valid
        );
        
        let mut test_game: Game<T> = Game::new(
            TEST_RULES,
            "7/5Tt/2T4/2t2t1/Tt4T/2t4/2T2K1"
        ).unwrap();
        
        test_game.side_to_play = Defender;
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 3)
            ).unwrap()),
            Invalid(TooFar)
        );
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 4)
            ).unwrap()),
            Valid
        );
        
        test_game.side_to_play = Attacker;
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 4)
            ).unwrap()),
            Invalid(MoveThroughBlockedTile)
        );
    }

    #[test]
    fn test_play_validity() {
        generic_test_play_validity::<SmallBoardState>();
        generic_test_play_validity::<MediumBoardState>();
        generic_test_play_validity::<LargeBoardState>();
        generic_test_play_validity::<HugeBoardState>();
    }

    fn generic_test_play_outcome<T: BoardState>() {

        let test_game: Game<T> = Game::new(
            TEST_RULES,
            "4t2/5Tt/2T4/2t2t1/Tt4T/2t4/2T2K1"
        ).unwrap();

        // First, move the piece on the board directly and check that it picks up the correct
        // captures. Then, reverse the move, and call `do_move` to check that the correct game
        // outcome is detected.

        let mut game = test_game.clone();
        let play = Play::from_tiles(Tile::new(0, 4), Tile::new(6, 4)).unwrap();
        let piece = game.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [Tile::new(6, 5)].into()
        );
        game.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Over(Winner(KingCaptured, Attacker))));

        let mut game = test_game.clone();
        game.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(4, 6), Tile::new(4, 2)).unwrap();
        let piece = game.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [
                Tile::new(4, 1),
                Tile::new(3, 2),
                Tile::new(5, 2),
            ].into()
        );
        game.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Ongoing));

        let mut game = test_game.clone();
        game.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(6, 5), Tile::new(6, 6)).unwrap();
        let piece = game.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [].into(),
        );
        game.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Over(Winner(KingEscaped, Defender))));

        let mut game = test_game.clone();
        game.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(6, 5), Tile::new(5, 5)).unwrap();
        let piece = game.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [].into()
        );
        game.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Ongoing));
    }

    #[test]
    fn test_play_outcome() {
        generic_test_play_outcome::<SmallBoardState>();
        generic_test_play_outcome::<MediumBoardState>();
        generic_test_play_outcome::<LargeBoardState>();
        generic_test_play_outcome::<HugeBoardState>();
    }

    #[test]
    fn test_shieldwalls() {

        let no_corner_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::from(Soldier)
            }),
            ..rules::COPENHAGEN
        };

        let king_capture_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::all()
            }),
            ..rules::COPENHAGEN
        };

        let corner_sw = "9/9/9/9/6t2/7tT/7tT/7tT/9";
        let regular_sw = "9/9/9/6t2/7tT/7tT/7tT/8t/9";
        let regular_sw_king = "9/9/9/6t2/7tT/7tK/7tT/8t/9";
        let no_sw_gap = "9/9/9/6t2/7tT/8T/7tT/8t/9";
        let no_sw_friend = "9/9/9/6t2/7tT/6tTT/7tT/8t/9";
        let no_sw_small = "9/9/9/6t2/7tT/8t/9/9/9";

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

        let g_corner: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &corner_sw).unwrap();
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
        let setup_1 = "7/2ttt2/1t1K1t1/2ttt2/7";
        let setup_2 = "7/1tttt2/1t1K1t1/2tttt1/7";
        let setup_3 = "2t1t2/1t1t1t1/1t1K1t1/2ttt2/7";
        let setup_4 = "2t2t1/1t3t1/1t1K1t1/2ttt2/7";
        
        let safe_corners = Ruleset {
            hostility: HostilityRules {
                corners: PieceSet::none(),
                edge: PieceSet::none(),
                throne: PieceSet::none()
            },
            ..rules::COPENHAGEN
        };
        
        let candidates = [
            // string, inside_safe, outside_safe, is_secure, rules
            (&setup_1, false, true, true, rules::COPENHAGEN),
            (&setup_1, false, false, false, rules::COPENHAGEN),
            (&setup_2, false, true, true, rules::COPENHAGEN),
            (&setup_2, true, false, true, rules::COPENHAGEN),
            (&setup_3, false, true, false, rules::COPENHAGEN),
            (&setup_4, false, true, false, rules::COPENHAGEN),
            (&setup_4, false, true, true, safe_corners),
            (&setup_4, true, false, true, rules::COPENHAGEN),
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
        let exit_fort_flat = "9/9/8t/7tT/7T1/6tT1/7TK/7tT/9";
        let exit_fort_bulge = "9/9/9/9/9/5TTTT/5T2K/6TTT/9";
        let no_fort_enemy = "9/9/9/8T/7Tt/7T1/7TK/8T/9";
        let no_fort_unfree = "9/9/9/8T/7TT/7TT/7TK/8T/9";
        let no_fort_gap = "9/9/9/8T/9/4t2T1/7TK/8T/9";
        let no_fort_vuln = "9/9/9/9/9/6TTT/5T2K/6TTT/9";
        for s in [exit_fort_flat, exit_fort_bulge] {
            let g: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &s).unwrap();
            assert!(g.detect_exit_fort());
        }
        for s in [no_fort_enemy, no_fort_unfree, no_fort_gap, no_fort_vuln] {
            let g: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &s).unwrap();
            assert!(!g.detect_exit_fort());
        }
    }
    
    #[test]
    fn test_iter_plays() {
        let game: Game<SmallBoardState> = Game::new(rules::BRANDUBH, boards::BRANDUBH).unwrap();
        assert!(game.iter_plays(Tile::new(0, 0)).is_err());
        assert!(game.iter_plays(Tile::new(1, 0)).is_err());
        let outer_att_tile = Tile::new(0, 3);
        let outer_att_iter = game.iter_plays(outer_att_tile);
        assert!(outer_att_iter.is_ok());
        assert_eq!(
            outer_att_iter.unwrap().collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(outer_att_tile, Tile::new(0, 1)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 2)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 4)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 5)).unwrap()
            )
        );
        let inner_att_tile = Tile::new(1, 3);
        let inner_att_iter = game.iter_plays(inner_att_tile);
        assert!(inner_att_iter.is_ok());
        assert_eq!(
            inner_att_iter.unwrap().collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(inner_att_tile, Tile::new(1, 0)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 1)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 2)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 4)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 5)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 6)).unwrap()
            )
        );
        let outer_def_tile = Tile::new(2, 3);
        let outer_def_iter = game.iter_plays(outer_def_tile);
        assert!(outer_def_iter.is_ok());
        assert_eq!(
            outer_def_iter.unwrap().collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(outer_def_tile, Tile::new(2, 0)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 1)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 2)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 4)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 5)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 6)).unwrap()
            )
        );
        let king_tile = Tile::new(3, 3);
        let king_iter = game.iter_plays(king_tile);
        assert!(king_iter.is_ok());
        assert_eq!(king_iter.unwrap().collect::<HashSet<Play>>(), HashSet::new());
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "1T5/7/7/1t3K1/7/7/7"
        ).unwrap();

        // Test moving through (but not onto) throne and blocking by piece
        let test_tile = Tile::new(3, 1);
        let iter = game.iter_plays(test_tile);
        assert!(iter.is_ok());
        assert_eq!(
            iter.unwrap().collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(test_tile, Tile::new(1, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(2, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(4, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(5, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(6, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 0)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 2)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 4)).unwrap()
            )
        )
    }
    
    #[test]
    fn test_can_play() {
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "2tt3/1tTKt2/2tt3/7/7/7/7"
        ).unwrap();
        assert!(game.side_can_play(Attacker));
        assert!(!game.side_can_play(Defender));
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "2tKt2/3t3/7/7/7/7/7"
        ).unwrap();
        assert!(game.side_can_play(Attacker));
        assert!(!game.side_can_play(Defender));
    } 
    
    #[test]
    fn test_repetitions() {
        let mut game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            boards::BRANDUBH
        ).unwrap();
        for _ in 0..3 {
            game.do_move(Play::from_str("d6-f6").unwrap()).unwrap();
            game.do_move(Play::from_str("d5-f5").unwrap()).unwrap();
            game.do_move(Play::from_str("f6-d6").unwrap()).unwrap();
            game.do_move(Play::from_str("f5-d5").unwrap()).unwrap();
        }
        assert_eq!(game.status, Ongoing);
        game.do_move(Play::from_str("d6-f6").unwrap()).unwrap();

        assert_eq!(game.status, Over(Winner(Repetition, Defender)));
    }

}