use crate::board_geo::BoardGeometry;
use crate::board_state::BoardState;
use crate::error::BoardError;
use crate::game::MoveValidity::{Invalid, Valid};
use crate::game::WinReason::{AllCaptured, Enclosed, ExitFort, KingCaptured, KingEscaped, NoMoves};
use crate::game::{DrawReason, MoveValidity, WinReason};
use crate::game_state::GameState;
use crate::play_iter::PlayIterator;
use crate::rules::EnclosureWinRules::WithoutEdgeAccess;
use crate::rules::KingAttack::{Anvil, Armed, Hammer};
use crate::rules::{RepetitionRule, ShieldwallRules};
use crate::tiles::{AxisOffset, RowColOffset};
use crate::utils::UniqueStack;
use crate::Axis::{Horizontal, Vertical};
use crate::GameOutcome::{Draw, Winner};
use crate::GameStatus::{Ongoing, Over};
use crate::InvalidPlay::{BlockedByPiece, GameOver, MoveOntoBlockedTile, MoveThroughBlockedTile, NoCommonAxis, NoPiece, OutOfBounds, TooFar, WrongPlayer};
use crate::PieceType::{King, Soldier};
use crate::Side::{Attacker, Defender};
use crate::ThroneRule::{KingEntry, KingPass, NoEntry, NoPass};
use crate::{error, Axis, Coords, GameOutcome, InvalidPlay, KingStrength, Piece, PieceSet, Play, PlayOutcome, Ruleset, Side, ThroneRule, Tile};
use std::collections::HashSet;
use crate::play::PlayRecord;

/// A space on the board that is enclosed by pieces.
#[derive(Debug, Default)]
pub struct Enclosure {
    /// A set of all occupied enclosed tiles.
    pub occupied: HashSet<Tile>,
    /// A set of all unoccupied enclosed tiles.
    pub unoccupied: HashSet<Tile>,
    /// A set of tiles representing the boundary of the enclosure, ie, the enclosing pieces.
    pub boundary: HashSet<Tile>
}

impl Enclosure {
    pub fn contains(&self, tile: &Tile) -> bool {
        self.occupied.contains(tile) || self.unoccupied.contains(tile)
    }
}

/// This struct contains the information necessary to implement the game logic, including the game
/// rules and information about the geometry of the board (size, positions of special tiles, etc).
/// It provides methods for evaluating a given play or board based on that logic.
/// 
/// The information stored in this struct is not expected to change over the course of a game. It
/// does not contain the current game state (piece placement, number of repetitions, etc), but
/// rather, its methods take references to such state where necessary.
#[derive(Clone)]
pub struct GameLogic {
    pub rules: Ruleset,
    pub board_geo: BoardGeometry
}

impl GameLogic {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, board_length: u8) -> Self {
        Self { rules, board_geo: BoardGeometry::new(board_length) }
    }

    /// Determine whether the given tile is hostile specifically by reference to the rules regarding
    /// hostility of special tiles.
    pub fn special_tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        (self.rules.hostility.throne.contains(piece) && tile == self.board_geo.special_tiles.throne)
            || (self.rules.hostility.corners.contains(piece)
            && self.board_geo.special_tiles.corners.contains(&tile))
            || (self.rules.hostility.edge.contains(piece)
            && !self.board_geo.tile_in_bounds(tile))
    }

    /// Determine whether the given tile is hostile to the given piece.
    pub fn tile_hostile<T: BoardState>(&self, tile: Tile, piece: Piece, board: &T) -> bool {
        if let Some(other_piece) = board.get_piece(tile) {
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
    pub fn coords_hostile<T: BoardState>(
        &self,
        coords: Coords,
        piece: Piece,
        board: &T
    ) -> bool {
        if self.board_geo.coords_in_bounds(coords) {
            self.tile_hostile(Tile::new(coords.row as u8, coords.col as u8), piece, board)
        } else {
            self.rules.hostility.edge.contains(piece)
        }
    }

    /// Whether the given piece can occupy or pass the given tile according to the game rules and
    /// current board state. Returns a pair of `bool`s indicating whether the piece can occupy or
    /// pass, respectively.
    pub fn can_occupy_or_pass<T: BoardState>(
        &self,
        play: Play,
        piece: Piece,
        state: &GameState<T>
    ) -> (bool, bool) {
        let validity = self.check_play_validity_for_side(play, piece.side, state);
        match validity {
            Valid => (true, true),
            Invalid(error::InvalidPlay::MoveOntoBlockedTile) => {
                // Generally, the only way you could be unable to move onto a tile but be able to
                // move past it is if the tile is a throne and the rules permit passing through, but
                // not occupying, the throne. Of course, this will differ for knights and commanders
                // when implemented.
                if play.to() == self.board_geo.special_tiles.throne {
                    match self.rules.throne_movement {
                        ThroneRule::NoThrone => (true, true),
                        ThroneRule::NoPass => (false, false),
                        ThroneRule::NoEntry => (false, true),
                        ThroneRule::KingPass => {
                            let is_king = piece.piece_type == King;
                            (is_king, is_king)
                        },
                        ThroneRule::KingEntry => (piece.piece_type == King, true),
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
    pub fn check_play_validity_for_side<T: BoardState>(
        &self,
        play: Play,
        side: Side,
        state: &GameState<T>
    ) -> MoveValidity {
        if state.status != Ongoing {
            return Invalid(GameOver)
        }
        let from = play.from;
        let to = play.to();
        let maybe_piece = state.board.get_piece(from);
        match maybe_piece {
            None => Invalid(NoPiece),
            Some(piece) => {
                if piece.side != side {
                    return Invalid(WrongPlayer);
                }
                if !(self.board_geo.tile_in_bounds(from) && self.board_geo.tile_in_bounds(to)) {
                    return Invalid(OutOfBounds)
                }
                if (from.row != to.row) && (from.col != to.col) {
                    return Invalid(NoCommonAxis)
                }
                if state.board.tile_occupied(to) {
                    return Invalid(BlockedByPiece)
                }
                let between = self.board_geo.tiles_between(from, to);
                if between.iter().any(|t| state.board.tile_occupied(*t)) {
                    return Invalid(BlockedByPiece)
                }
                if !self.rules.may_enter_corners.contains(piece) &&
                    self.board_geo.special_tiles.corners.contains(&to) {
                    return Invalid(MoveOntoBlockedTile)
                }
                if (
                    (self.rules.throne_movement == NoPass)
                        || ((self.rules.throne_movement == KingPass)
                        && piece.piece_type != King)
                ) && between.contains(&self.board_geo.special_tiles.throne) {
                    return Invalid(MoveThroughBlockedTile)
                }
                // println!("throne_movement: {:?}", self.rules.throne_movement);
                // println!("piece_type: {:?}", piece.piece_type);
                // println!("to: {to}");
                // println!("throne: {}", self.board_geo.special_tiles.throne);
                if ((self.rules.throne_movement == NoEntry)
                    || ((self.rules.throne_movement == KingEntry)
                    && piece.piece_type != King)
                ) && (to == self.board_geo.special_tiles.throne) {
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
    pub fn check_play_validity<T: BoardState>(&self, play: Play, state: &GameState<T>) -> MoveValidity {
        self.check_play_validity_for_side(play, state.side_to_play, state)
    }

    /// Check whether the king is *currently* strong (must be surrounded on all four sides to be
    /// captured), considering the game rules and the king's current position (for example, the
    /// rules may provide that the king is only strong on or beside the throne).
    pub fn king_is_strong<T: BoardState>(&self, board: &T) -> bool {
        match self.rules.king_strength {
            KingStrength::Strong => true,
            KingStrength::Weak => false,
            KingStrength::StrongByThrone => {
                let k = board.get_king();
                self.board_geo.special_tiles.corners.contains(&k)
                    || self.board_geo.special_tiles.throne == k
            }
        }
    }

    /// Whether the tile (if any) at the given [`Coords`] can theoretically be occupied by the given
    /// piece according to the rules of the game. Does not take account of whether the tile is
    /// already occupied or actually accessible.
    pub fn coords_occupiable(&self, coords: Coords, piece: Piece) -> bool {
        if !self.board_geo.coords_in_bounds(coords) {
            return false
        }
        let t = Tile::new(coords.row as u8, coords.col as u8);
        if self.board_geo.special_tiles.throne == t && (
            (self.rules.throne_movement == NoEntry)
                || (self.rules.throne_movement == KingEntry && piece.piece_type != King)
        ) {
            return false
        }
        if !self.rules.may_enter_corners.contains(piece)
            && self.board_geo.special_tiles.corners.contains(&t) {
            return false
        }
        true
    }

    /// Check whether the tile at the given row and column is part of an enclosure.
    /// Used by [`Self::find_enclosure`].
    fn row_col_enclosed<T: BoardState>(
        &self,
        row: i8,
        col: i8,
        enclosed_piece_types: PieceSet,
        enclosing_piece_types: PieceSet,
        enclosure: &mut Enclosure,
        board: &T,
    ) -> Option<bool> {
        let coords = Coords { row, col };
        if let Ok(tile) = self.board_geo.coords_to_tile(coords) {
            if let Some(p) = board.get_piece(tile) {
                if enclosed_piece_types.contains(p) {
                    enclosure.occupied.insert(tile);
                    Some(true)
                } else if enclosing_piece_types.contains(p) {
                    enclosure.boundary.insert(tile);
                    Some(false)
                } else {
                    // Tile occupied by piece that can neither enclose nor be enclosed, so no enclosure
                    // is present.
                    None
                }
            } else {
                // Empty tiles can be enclosed.
                enclosure.unoccupied.insert(tile);
                Some(true)
            }
        } else {
            Some(false)
        }
    }

    /// Find an area surrounding the tile at `start`, containing only the pieces in
    /// `enclosed_pieces` and which is fully surrounded by pieces other than those in
    /// `enclosed_pieces`. If `abort_on_edge` or `abort_on_corner` is `true` and an edge or corner,
    /// respectively, is encountered, no enclosure is found.
    ///
    /// Uses a flood fill algorithm based on https://en.wikipedia.org/wiki/Flood_fill#Span_filling
    pub fn find_enclosure<T: BoardState>(
        &self,
        tile: Tile,
        enclosed_pieces: PieceSet,
        enclosing_pieces: PieceSet,
        abort_on_edge: bool,
        abort_on_corner: bool,
        board: &T,
    ) -> Option<Enclosure> {
        let Coords { row, col } = Coords::from(tile);
        let mut enclosure = Enclosure::default();
        if !self.row_col_enclosed(
            row, col,
            enclosed_pieces, enclosing_pieces,
            &mut enclosure,
            board
        )? {
            return None
        }
        // In theory, I don't think we would need a UniqueStack if our flood fill logic worked
        // perfectly, but we were getting infinite loops on certain grids as it seemed certain
        // values were being pushed to the stack repeatedly. I couldn't debug it so this is an easy
        // option to address it.
        let mut stack: UniqueStack<(i8, i8, i8, i8)> = UniqueStack::default();
        stack.push((col, col, row, 1));
        stack.push((col, col, row - 1, -1));
        while let Some((mut c1, c2, r, dr)) = stack.pop() {
            let mut c = c1;
            if self.row_col_enclosed(r, c, enclosed_pieces, enclosing_pieces, &mut enclosure, board)? {
                while self.row_col_enclosed(
                    r,
                    c - 1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure,
                    board
                )? {
                    let t= Tile::new(r as u8, (c - 1) as u8);
                    if (abort_on_edge && self.board_geo.tile_at_edge(t))
                        || (abort_on_corner && self.board_geo.special_tiles.corners.contains(&t)) {
                        return None
                    }
                    c -= 1
                }

                if c < c1 {
                    stack.push((c, c1 - 1, r - dr, -dr))
                }
            }
            while c1 <= c2 {
                while self.row_col_enclosed(
                    r,
                    c1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure,
                    board
                )? {
                    let t= Tile::new(r as u8, c1 as u8);
                    if abort_on_edge && self.board_geo.tile_at_edge(t) {
                        return None
                    }
                    if abort_on_corner && self.board_geo.special_tiles.corners.contains(&t) {
                        return None
                    }
                    c1 += 1
                }

                if c1 > c {
                    stack.push((c, c1 - 1, r + dr, dr));
                }
                if c1 - 1 > c2 {
                    stack.push((c2 + 1, c1 - 1, r - dr, -dr))
                }

                c1 += 1;

                while (c1 < c2) && !self.row_col_enclosed(
                    r,
                    c1,
                    enclosed_pieces,
                    enclosing_pieces,
                    &mut enclosure,
                    board
                )? {
                    c1 += 1
                }
                c = c1
            }

        }
        Some(enclosure)
    }

    /// Check whether the given [`Enclosure`] is "secure", ie, no piece on its boundary is
    /// vulnerable to capture. If `inside_safe` is `true`, then any square inside the enclosure is
    /// considered not to be threatening to a boundary piece. Similarly, if `outside_safe` is
    /// `true`, then any square not inside the enclosure is considered not to be threatening to a
    /// boundary piece.
    pub fn enclosure_secure<T: BoardState>(
        &self,
        encl: &Enclosure,
        inside_safe: bool,
        outside_safe: bool,
        board: &T
    ) -> bool {
        if inside_safe && outside_safe {
            // If both inside and outside the enclosure are safe, then the enclosure must be secure.
            return true
        }
        for t in &encl.boundary {
            let piece = board.get_piece(*t)
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
                    if let Ok(n_tile) = self.board_geo.coords_to_tile(n_coords) {
                        let is_inside = encl.contains(&n_tile);
                        if (inside_safe && is_inside) || (outside_safe && !is_inside) {
                            // Tile is on a side of the boundary that is known to be safe (ie, no
                            // enemies). Therefore, it is safe unless it is a hostile tile.
                            if !self.special_tile_hostile(n_tile, piece) {
                                continue 'axisloop;
                            }
                        }
                        if (!self.tile_hostile(n_tile, piece, board)) && (
                            board.tile_occupied(n_tile)
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
    fn dir_sw_search<T: BoardState>(
        &self,
        play: Play,
        sw_rule: ShieldwallRules,
        axis: Axis,
        away_from_edge: i8,
        dir: i8,
        state: &GameState<T>
    ) -> Option<HashSet<Tile>> {
        let mut t = play.to();
        // Key is an occupied tile at edge of the board (which is threatened with capture);
        // value is the tile, on the opposite side to the edge, occupied by the opposing piece.
        let mut wall: HashSet<Tile> = HashSet::new();
        loop {
            let step = Play::new(t, AxisOffset::new(axis, dir));
            // Move one tile along the edge
            t = step.to();
            if !self.board_geo.tile_in_bounds(t) {
                // We have reached the edge of the board without finding a closing piece.
                // No shieldwall.
                return None
            }
            if !(
                state.board.tile_occupied(t)
                    || sw_rule.corners_may_close
                    && self.board_geo.special_tiles.corners.contains(&t)
            ) {
                // We have encountered a tile that is not occupied and is not a corner that may
                // close. No shieldwall.
                return None
            }
            let piece_opt = state.board.get_piece(t);
            if piece_opt.is_none() {
                // We have already broken out of this loop if the tile is unoccupied, unless it
                // is a closing corner. Therefore, if `piece` is `None`, we must be at a
                // closing corner.
                return if wall.len() < 2 { None } else { Some(wall) };
            }
            let piece = piece_opt.expect("Tile should be occupied.");
            if piece.side == state.side_to_play.other() {
                let pin = Play::new(t, AxisOffset::new(axis.other(), away_from_edge)).to();
                if let Some(p) = state.board.get_piece(pin) {
                    if p.side == state.side_to_play {
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
            if (piece.side == state.side_to_play) ||
                (self.board_geo.special_tiles.corners.contains(&t) && sw_rule.corners_may_close) {
                // We've found a friendly piece or a corner that may close.
                return if wall.len() < 2 { None } else { Some(wall) };
            }
        }
    }

    /// Detect whether the given move has created a shieldwall according to the applicable rules.
    /// Returns `None` if no shieldwall is detected; otherwise returns a set of tiles that have been
    /// captured in the shieldwall.
    pub fn detect_shieldwall<T: BoardState>(&self, play: Play, state: &GameState<T>) -> Option<HashSet<Tile>> {
        let sw_rule = self.rules.shieldwall?;
        let to = play.to();
        let (axis, away_from_edge) = if to.row == 0 {
            (Horizontal, 1i8)
        } else if to.row == self.board_geo.side_len - 1 {
            (Horizontal, -1)
        } else if to.col == 0 {
            (Vertical, 1)
        } else if to.col == self.board_geo.side_len - 1 {
            (Vertical, -1)
        } else {
            // The move that leads to a shieldwall capture must be flanking the shieldwall,
            // therefore must be a move to the edge.
            return None
        };
        let mut wall = self.dir_sw_search(play, sw_rule, axis, away_from_edge, -1, state);
        if wall.is_none() {
            wall = self.dir_sw_search(play, sw_rule, axis, away_from_edge, 1, state);
        }
        if let Some(w) = wall {
            if w.len() < 2 {
                // Can't capture 0 or 1 pieces with a shieldwall
                return None
            }
            // We've found a shieldwall. Filter out tiles which contain pieces which cannot
            // be captured in a shieldwall.
            Some(w.into_iter().filter(|t| sw_rule.captures.contains(
                state.board.get_piece(*t)
                    .expect("Tile in shieldwall should be occupied."))
            ).collect())
        } else {
            None
        }
    }

    /// Detect whether the king is in an exit fort.
    pub fn detect_exit_fort<T: BoardState>(&self, board: &T) -> bool {
        let king_tile = board.get_king();

        // King is at edge
        if !self.board_geo.tile_at_edge(king_tile) {
            return false
        }

        // Check king is enclosed by his own pieces
        if let Some(encl) = self.find_enclosure(
            king_tile,
            PieceSet::from(King),
            PieceSet::from(Defender),
            false,
            true,
            board,
        ) {
            // King has space to move
            if !self.board_geo.neighbors(king_tile).iter().any(|t| !board.tile_occupied(*t)) {
                return false
            }
            // Check enclosing pieces are all themselves safe
            if !self.enclosure_secure(&encl, true, false, board) {
                return false
            }
            true
        } else {
            false
        }
    }

    /// Get the tiles containing pieces captured by the given play.
    pub fn get_captures<T: BoardState>(&self, play: Play, moving_piece: Piece, state: &GameState<T>) -> HashSet<Tile> {
        let mut captures: HashSet<Tile> = HashSet::new();
        let to = play.to();

        // Detect normal captures
        if moving_piece.piece_type != King
            || self.rules.king_attack == Armed
            || self.rules.king_attack == Hammer {
            for n in self.board_geo.neighbors(to) {
                if let Some(other_piece) = state.board.get_piece(n) {
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
                    if self.coords_hostile(far_coords, other_piece, &state.board) {
                        // We know that the neighbouring opposing piece is surrounded by the
                        // moving piece and another hostile tile. So it is captured, *unless* it
                        // is a strong king.
                        if (other_piece.piece_type == King) && self.king_is_strong(&state.board) {
                            // Get the tiles surrounding `n` on the perpendicular axis.
                            let n_coords = Coords::from(n);
                            let perp_hostile= if to.row == n.row {
                                self.coords_hostile(
                                    n_coords + RowColOffset::new(1, 0),
                                    other_piece,
                                    &state.board,
                                ) && self.coords_hostile(
                                    n_coords + RowColOffset::new(-1, 0),
                                    other_piece,
                                    &state.board,
                                )
                            } else {
                                self.coords_hostile(
                                    n_coords + RowColOffset::new(0, 1),
                                    other_piece,
                                    &state.board,
                                ) && self.coords_hostile(
                                    n_coords + RowColOffset::new(0, -1),
                                    other_piece,
                                    &state.board,
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
        if let Some(walled) = self.detect_shieldwall(play, state) {
            captures.extend(walled);
        }
        captures

    }

    /// Get the outcome of the game, if any. If None, the game is still ongoing.
    pub fn get_game_outcome<T: BoardState>(
        &self,
        play: Play,
        moving_piece: Piece,
        caps: &HashSet<Tile>,
        state: &GameState<T>,
    ) -> Option<GameOutcome> {
        if state.board.count_pieces(state.side_to_play.other()) == 0 {
            // All opposing pieces have been captured.
            return Some(Winner(AllCaptured, state.side_to_play))
        }
        if state.side_to_play == Attacker {
            // This test relies on the fact that even once the king has been removed from the board,
            // the bits at the end that encode its position remain set.
            if caps.contains(&state.board.get_king()) {
                // Attacker has captured the king.
                return Some(Winner(KingCaptured, Attacker))
            }
            if let Some(encl_win) = self.rules.enclosure_win {
                if let Some(encl) = self.find_enclosure(
                    state.board.get_king(),
                    PieceSet::from(Defender),
                    PieceSet::from(Attacker),
                    encl_win == WithoutEdgeAccess,
                    true,
                    &state.board
                ) {
                    if encl.occupied.len() == state.board.count_pieces(Defender) as usize
                        && self.enclosure_secure(&encl, false, true, &state.board) {
                        return Some(Winner(Enclosed, Attacker))
                    }
                }
            }
        } else {
            if moving_piece.piece_type == King && (
                (self.rules.edge_escape && self.board_geo.tile_at_edge(play.to()))
                    || (!self.rules.edge_escape && self.board_geo.special_tiles.corners.contains(&play.to()))
            ) {
                // King has escaped.
                return Some(Winner(KingEscaped, Defender))
            }
            if self.rules.exit_fort && self.detect_exit_fort(&state.board) {
                // King has escaped through exit fort.
                return Some(Winner(ExitFort, Defender))
            }
        }

        if let Some(RepetitionRule { n_repetitions, is_loss }) = self.rules.repetition_rule {
            if state.repetitions.get_repetitions(state.side_to_play) >= n_repetitions {
                // Loss or draw as a result of repeated moves.
                return if is_loss {
                    Some(Winner(WinReason::Repetition, state.side_to_play.other()))
                } else {
                    Some(Draw(DrawReason::Repetition))
                }
            }
        }

        if !self.side_can_play(state.side_to_play.other(), state) {
            // Other side has no playable moves.
            return Some(Winner(NoMoves, state.side_to_play))
        }

        None
    }


    /// Actually "do" a move, checking validity, getting outcome, applying outcome to board state,
    /// switching side to play and returning a description of the game status following the move.
    pub fn do_move<T: BoardState>(
        &self,
        play: Play,
        mut state: GameState<T>
    ) -> Result<(GameState<T>, PlayRecord), InvalidPlay> {
        if let Invalid(v) = self.check_play_validity(play, &state) {
            return Err(v)
        };
        
        // First move the piece on the board
        let moving_piece = state.board.move_piece(play.from, play.to());
        // Then remove captured pieces
        let captures = self.get_captures(play, moving_piece, &state);
        for &c in &captures {
            state.board.clear_tile(c)
        }
        // Update records of repetitions and non-capturing plays
        state.repetitions.track_play(state.side_to_play, play, !captures.is_empty());
        if captures.is_empty() {
            state.plays_since_capture += 1;
        }
        // Then assess the game outcome
        let game_outcome = self.get_game_outcome(play, moving_piece, &captures, &state);

        state.turn += 1;
        let game_status = match game_outcome {
            Some(game_outcome) => Over(game_outcome),
            None => Ongoing
        };

        let outcome = PlayOutcome { captures, game_outcome };
        let record = PlayRecord {
            side: state.side_to_play, play, outcome
        };

        state.side_to_play = state.side_to_play.other();
        state.status = game_status;
        
        Ok((state, record))
    }

    /// Iterate over the possible plays that can be made by the piece at the given tile. Returns an
    /// error if there is no piece at the given tile. Order of iteration is not guaranteed.
    pub fn iter_plays<'logic, 'state, T: BoardState>(&'logic self, tile: Tile, state: &'state GameState<T>) -> Result<PlayIterator<'logic, 'state, T>, BoardError> {
        PlayIterator::new(self, state, tile)
    }

    /// Whether the given side could make any play given the current board.
    pub fn side_can_play<T: BoardState>(&self, side: Side, state: &GameState<T>) -> bool {
        for tile in state.board.iter_occupied(side) {
            if self.iter_plays(tile, state).expect("Tile must not be empty.").next().is_some() {
                return true
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::board_state::BoardState;
    use crate::game_logic::GameLogic;
    use crate::preset::rules;
    use crate::utils::check_tile_vec;
    use crate::PieceType::{King, Soldier};
    use crate::Side::{Attacker, Defender};
    use crate::{Piece, PieceSet, SmallBoardState, Tile};
    use std::str::FromStr;

    #[test]
    fn test_enclosures() {
        let full_enclosure = "2ttt2/1t1K1t1/2tttt1/7/7/7/7";
        let encl_with_edge = "2t1t2/1t1K1t1/2tttt1/7/7/7/7";
        let encl_with_corner = "5t1/4tK1/4ttt/7/7/7/7";
        let encl_with_soldier = "2ttt2/1t1KTt1/2tttt1/7/7/7/7";
        let encl_edge_2 = "1t2t2/1t1K1t1/2tttt1/7/7/7/7";
        let state = SmallBoardState::from_str(full_enclosure).unwrap();
        let game_logic = GameLogic::new(rules::BRANDUBH, state.side_len());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            true,
            true,
            &state
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 3)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(1, 2), Tile::new(1, 4)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 3), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );

        let state = SmallBoardState::from_str(encl_with_edge).unwrap();
        let game_logic = GameLogic::new(rules::BRANDUBH, state.side_len());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            true,
            true,
            &state
        );
        assert!(encl_res.is_none());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            true,
            &state
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 3)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(0, 3), Tile::new(1, 2), Tile::new(1, 4)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );

        let state = SmallBoardState::from_str(encl_with_corner).unwrap();
        let game_logic = GameLogic::new(rules::BRANDUBH, state.side_len());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            true,
            &state
        );
        assert!(encl_res.is_none());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 5),
            PieceSet::from(King),
            PieceSet::from(Soldier),
            false,
            false,
            &state
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(encl.occupied.into_iter().collect(), vec![Tile::new(1, 5)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(0, 6), Tile::new(1, 6)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 5), Tile::new(1, 4),
                Tile::new(2, 5), Tile::new(2, 6)
            ]
        );

        let state = SmallBoardState::from_str(encl_with_soldier).unwrap();
        let game_logic = GameLogic::new(rules::BRANDUBH, state.side_len());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            true,
            true,
            &state
        );
        assert!(encl_res.is_none());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(vec![Piece::new(King, Defender), Piece::new(Soldier, Defender)]),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            true,
            true,
            &state
        );
        assert!(encl_res.is_some());
        let encl = encl_res.unwrap();
        check_tile_vec(
            encl.occupied.into_iter().collect(),
            vec![Tile::new(1, 3), Tile::new(1, 4)]);
        check_tile_vec(
            encl.unoccupied.into_iter().collect(),
            vec![Tile::new(1, 2)]
        );
        check_tile_vec(
            encl.boundary.into_iter().collect(),
            vec![
                Tile::new(0, 2), Tile::new(0, 3), Tile::new(0, 4),
                Tile::new(1, 1), Tile::new(1, 5),
                Tile::new(2, 2), Tile::new(2, 3), Tile::new(2, 4)
            ]
        );

        let state = SmallBoardState::from_str(encl_edge_2).unwrap();
        let game_logic = GameLogic::new(rules::BRANDUBH, state.side_len());
        let encl_res = game_logic.find_enclosure(
            Tile::new(1, 3),
            PieceSet::from(King),
            PieceSet::from(Piece::new(Soldier, Attacker)),
            false,
            false,
            &state
        );
        assert!(encl_res.is_some());
    }
}