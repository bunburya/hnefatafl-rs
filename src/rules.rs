use std::cmp::PartialEq;
use crate::board::{Board, Side};
use crate::rules::ThroneRule::{KingEntry, KingPass, NoEntry, NoPass};
use crate::tiles::Tile;

#[derive(PartialEq)]
enum ThroneRule {
    /// Board has no throne
    NoThrone,
    /// No piece may pass through the throne.
    NoPass,
    /// Only the king may pass through the throne.
    KingPass,
    /// No piece may enter the throne (but any piece may pass through it).
    NoEntry,
    /// Only the king may enter the throne (other pieces may pass through it).
    KingEntry
}

struct MoveOutcome {
    captures: Vec<Tile>,
    winner: Option<Side>
}

struct Ruleset {
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    strong_king: bool,
    /// Whether the king is armed (can participate in captures).
    armed_king: bool,
    /// Whether the throne blocks movement.
    throne_rule: ThroneRule,
    /// Whether the throne is hostile.
    hostile_throne: bool,
    /// Whether the king is slow, ie, can only move one space at a time.
    slow_king: bool
}

impl Ruleset {
    fn is_valid_move(&self, from: Tile, to: Tile, board: &Board, is_king: bool) -> bool {
        if !(board.tile_in_bounds(from) && board.tile_in_bounds(to)) {
            // Tile out of bounds
            return false
        }
        if (from.row() != to.row()) && (from.col() != to.col()) {
            // Tiles not on same row or column
            return false
        }
        let between = board.tiles_between(from, to);
        if between.iter().any(|t| board.tile_occupied(*t)) {
            // Move is blocked by a piece
            return false
        }
        if ((self.throne_rule == NoPass) || ((self.throne_rule == KingPass) && !is_king))
            && between.contains(&board.throne) {
            // Move is blocked by the throne
            return false
        }
        if ((self.throne_rule == NoEntry) || ((self.throne_rule == KingEntry) && !is_king))
            && (to == board.throne) {
            // Illegal move on to the throne
            return false
        }
        if self.slow_king && is_king && (from.distance_to(to) > 1) {
            // Slow king can't move more than one space at a time
            return false
        }
        true
    }
}