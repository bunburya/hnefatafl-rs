use std::cmp::PartialEq;
use crate::board::{Board, Side};
use crate::rules::Hostility::HostileToNonKing;
use crate::rules::KingStrength::Weak;
use crate::rules::MovementRule::{SlowAll, SlowKing, Unrestricted};
use crate::rules::ThroneRule::{KingEntry, KingPass, NoEntry, NoPass};
use crate::tiles::Tile;



enum SpecialTileType {
    Throne,
    Corner,
    BaseCamp
}

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

#[derive(PartialEq)]
enum MovementRule {
    /// All pieces can move as many unblocked tiles as they want.
    Unrestricted,
    /// King may only move one tile at a time. Other pieces are unrestricted.
    SlowKing,
    /// Pieces may only move one tile at a time.
    SlowAll
}

enum KingStrength {
    /// King must be surrounded by four hostile pieces or tiles to be captured.
    Strong,
    /// King can be captured by two hostile pieces or tiles, except on or near the throne when four
    /// are necessary.
    StrongByThrone,
    /// King may be captured by two hostile pieces or tiles, in the same way as other pieces.
    Weak
}

struct MoveOutcome {
    captures: Vec<Tile>,
    winner: Option<Side>
}

enum Hostility {
    HostileToKing,
    HostileToNonKing,
    HostileToSide(Side),
    HostileToAll
}

struct Ruleset {
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    king_strength: KingStrength,
    /// Whether the edge counts as hostile to the king.
    hostile_edge: bool,
    /// Whether the king is armed (can participate in captures).
    armed_king: bool,
    /// Whether the throne blocks movement.
    throne_movement: ThroneRule,
    /// Whether the throne is hostile.
    throne_hostility: Hostility,
    /// Whether pieces' movement is restricted.
    movement_rule: MovementRule,
    /// Whether attacker goes first (if `false`, defender goes first)
    attacker_starts: bool
}

impl Ruleset {
    fn is_valid_move(&self, from: Tile, to: Tile, board: &Board) -> bool {
        let is_king = board.is_king(from);
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
        if ((self.throne_movement == NoPass) || ((self.throne_movement == KingPass) && !is_king))
            && between.contains(&board.throne) {
            // Move is blocked by the throne
            return false
        }
        if ((self.throne_movement == NoEntry) || ((self.throne_movement == KingEntry) && !is_king))
            && (to == board.throne) {
            // Illegal move on to the throne
            return false
        }
        if ((self.movement_rule == SlowAll) || ((self.movement_rule == SlowKing) && is_king))
            && from.distance_to(to) > 1 {
            // Slow piece can't move more than one space at a time
            return false
        }
        true
    }
}

/// Rules for Federation Brandubh
const FED_BRAN: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Weak,
    hostile_edge: false,
    armed_king: true,
    throne_movement: KingEntry,
    throne_hostility: HostileToNonKing,
    movement_rule: Unrestricted,
    attacker_starts: true
};

#[cfg(test)]
mod tests {
    use crate::board::Board;
    use crate::rules::FED_BRAN;
    use crate::tiles::Tile;

    #[test]
    fn test_fed_bran() {
        let mut b = Board::try_from("...A...\n...A...\n...D...\nAADKDAA\n...D...\n...A...\n...A...")
            .unwrap();
        assert!(FED_BRAN.is_valid_move(
            Tile::new(3, 2),
            Tile::new(4, 2),
            &b
        ));
        assert!(FED_BRAN.is_valid_move(
            Tile::new(3, 3),
            Tile::new(3, 2),
            &b
        ));

        // Invalid because blocked
        assert!(!FED_BRAN.is_valid_move(
            Tile::new(0, 3),
            Tile::new(2, 3),
            &b
        ));

        b.move_piece(Tile::new(3, 2), Tile::new(4, 2));
        b.move_piece(Tile::new(3, 3), Tile::new(3, 2));

        // Invalid because non-king move onto throne
        assert!(!FED_BRAN.is_valid_move(
            Tile::new(2, 3),
            Tile::new(3, 3),
            &b
        ));

        // Valid because king move onto throne
        assert!(FED_BRAN.is_valid_move(
            Tile::new(3, 2),
            Tile::new(3, 3),
            &b
        ))
    }

}