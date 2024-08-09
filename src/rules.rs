use std::cmp::PartialEq;
use crate::board::Board;
use crate::pieces::{PieceSet, Side};
use crate::pieces::PieceType::{King, Soldier};
use crate::rules::KingStrength::Weak;
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

enum KingStrength {
    /// King must be surrounded by four hostile pieces or tiles to be captured.
    Strong,
    /// King can be captured by two hostile pieces or tiles, except on or near the throne when four
    /// are necessary.
    StrongByThrone,
    /// King may be captured by two hostile pieces or tiles, in the same way as other pieces.
    Weak
}

enum GameOutcome {
    Winner(Side),
    Draw
}

struct MoveOutcome {
    captures: Vec<Tile>,
    outcome: GameOutcome
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
    /// Types of piece to which the throne is hostile.
    throne_hostile_to: PieceSet,
    /// Types of piece whose movement is restricted to one tile per move.
    slow_pieces: PieceSet,
    /// Whether attacker goes first (if `false`, defender goes first)
    attacker_starts: bool
}

impl Ruleset {
    fn is_valid_move(&self, from: Tile, to: Tile, board: &Board) -> bool {
        let maybe_piece = board.get_piece(from);
        match maybe_piece {
            None => return false,
            Some(piece) => {
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
                if (
                    (self.throne_movement == NoPass)
                        || ((self.throne_movement == KingPass) && piece.piece_type != King)
                ) && between.contains(&board.throne) {
                    // Move is blocked by the throne
                    return false
                }
                if (
                    (self.throne_movement == NoEntry)
                        || ((self.throne_movement == KingEntry) && piece.piece_type != King)
                ) && (to == board.throne) {
                    // Illegal move on to the throne
                    return false
                }
                if self.slow_pieces.contains(piece.piece_type) && from.distance_to(to) > 1 {
                    // Slow piece can't move more than one space at a time
                    return false
                }
                true
            }
        }
    }


}

/// Rules for Federation Brandubh
const FED_BRAN: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Weak,
    hostile_edge: false,
    armed_king: true,
    throne_movement: KingEntry,
    throne_hostile_to: PieceSet::from_piece_type(Soldier),
    slow_pieces: PieceSet::none(),
    attacker_starts: true
};

#[cfg(test)]
mod tests {
    use crate::board::Board;
    use crate::rules::FED_BRAN;
    use crate::tiles::{Move, Tile};

    #[test]
    fn test_fed_bran() {
        let mut b = Board::try_from(
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();
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

        b.do_move(Move::new(Tile::new(3, 2), Tile::new(4, 2)).unwrap());
        b.do_move(Move::new(Tile::new(3, 3), Tile::new(3, 2)).unwrap());

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