use crate::pieces::PieceType::Soldier;
use crate::pieces::{PieceSet, Side};
use crate::rules::KingStrength::Weak;
use crate::rules::ThroneRule::KingEntry;
use crate::tiles::Tile;
use std::cmp::PartialEq;

enum SpecialTileType {
    Throne,
    Corner,
    BaseCamp
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub(crate) enum ThroneRule {
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

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub(crate) enum KingStrength {
    /// King must be surrounded by four hostile pieces or tiles to be captured.
    Strong,
    /// King can be captured by two hostile pieces or tiles, except on or near the throne when four
    /// are necessary.
    StrongByThrone,
    /// King may be captured by two hostile pieces or tiles, in the same way as other pieces.
    Weak
}

pub(crate) enum GameOutcome {
    Winner(Side),
    Draw
}


pub(crate) struct MoveOutcome {
    pub(crate) captures: Vec<Tile>,
    pub(crate) game_outcome: Option<GameOutcome>
}

/// A struct describing what pieces certain special tiles are considered hostile to.
#[derive(Copy, Clone, Debug)]
pub(crate)struct HostilityRules {
    pub(crate) throne: PieceSet,
    pub(crate) corners: PieceSet,
    pub(crate) edge: PieceSet
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Ruleset {
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    pub(crate) edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    pub(crate) king_strength: KingStrength,
    /// Whether the edge counts as hostile to the king.
    hostile_edge: bool,
    /// Whether the king is armed (can participate in captures).
    pub(crate) armed_king: bool,
    /// Whether the throne blocks movement.
    pub(crate) throne_movement: ThroneRule,
    /// What special tiles are hostile to what pieces.
    pub(crate) hostility: HostilityRules,
    /// Types of piece whose movement is restricted to one tile per move.
    pub(crate) slow_pieces: PieceSet,
    /// Whether attacker goes first (if `false`, defender goes first)
    pub(crate) attacker_starts: bool
}

/// Rules for Federation Brandubh
pub(crate) const FED_BRAN: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Weak,
    hostile_edge: false,
    armed_king: true,
    throne_movement: KingEntry,
    hostility: HostilityRules {
        throne: PieceSet::from_piece_type(Soldier),
        corners: PieceSet::all(),
        edge: PieceSet::none()
    },
    slow_pieces: PieceSet::none(),
    attacker_starts: true
};

#[cfg(test)]
mod tests {
    use crate::game::Game;
    use crate::rules::FED_BRAN;
    use crate::tiles::{Move, Tile};

    #[test]
    fn test_fed_bran() {
        let mut g = Game::new(
            FED_BRAN, 
            "...t...\n...t...\n...T...\nttTKTtt\n...T...\n...t...\n...t..."
        ).unwrap();
        assert!(g.is_valid_move(Move::new(
            Tile::new(3, 2),
            Tile::new(4, 2)
        ).unwrap()));
        assert!(g.is_valid_move(Move::new(
            Tile::new(3, 3),
            Tile::new(3, 2)).unwrap()));

        // Invalid because blocked
        assert!(!g.is_valid_move(Move::new(
            Tile::new(0, 3), 
            Tile::new(2, 3)
        ).unwrap()));

        g.board.do_move(Move::new(Tile::new(3, 2), Tile::new(4, 2)).unwrap());
        g.board.do_move(Move::new(Tile::new(3, 3), Tile::new(3, 2)).unwrap());

        // Invalid because non-king move onto throne
        assert!(!g.is_valid_move(
            Move::new(Tile::new(2, 3), Tile::new(3, 3)).unwrap(),
        ));

        // Valid because king move onto throne
        assert!(g.is_valid_move(
            Move::new(Tile::new(3, 2), Tile::new(3, 3)).unwrap(),
        ))
    }

}