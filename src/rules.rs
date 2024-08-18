use crate::pieces::PieceType::Soldier;
use crate::pieces::PieceSet;
use crate::rules::KingStrength::Weak;
use crate::rules::ThroneRule::KingEntry;
use std::cmp::PartialEq;
use crate::KingStrength::Strong;
use crate::PieceType::King;

enum SpecialTileType {
    Throne,
    Corner,
    BaseCamp
}

/// Rules relating to who may occupy/pass through the throne.
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum ThroneRule {
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

/// Rules relating to whether and when the king is strong (must be surrounded by hostile tiles on
/// all four sides to be captured).
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum KingStrength {
    /// King must be surrounded by four hostile pieces or tiles to be captured.
    Strong,
    /// King can be captured by two hostile pieces or tiles, except on or near the throne when four
    /// are necessary.
    StrongByThrone,
    /// King may be captured by two hostile pieces or tiles, in the same way as other pieces.
    Weak
}


/// A struct describing what pieces certain special tiles are considered hostile to.
#[derive(Copy, Clone, Debug)]
pub struct HostilityRules {
    pub(crate) throne: PieceSet,
    pub(crate) corners: PieceSet,
    pub(crate) edge: PieceSet
}


struct ShieldwallRules {
    corners_may_close: bool,
    may_capture: PieceSet
}

/// A set of rules for a tafl game.
#[derive(Copy, Clone, Debug)]
pub struct Ruleset {
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    pub edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    pub king_strength: KingStrength,
    /// Whether the edge counts as hostile to the king.
    pub hostile_edge: bool,
    /// Whether the king is armed (can participate in captures).
    pub armed_king: bool,
    /// Whether the throne blocks movement.
    pub throne_movement: ThroneRule,
    /// What pieces may enter the corners.
    pub may_enter_corners: PieceSet,
    /// What special tiles are hostile to what pieces.
    pub hostility: HostilityRules,
    /// Types of piece whose movement is restricted to one tile per move.
    pub slow_pieces: PieceSet,
    /// Whether attacker goes first (if `false`, defender goes first)
    pub attacker_starts: bool,
}

/// Rules for Federation Brandubh.
pub const FEDERATION_BRANDUBH: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Weak,
    hostile_edge: false,
    armed_king: true,
    throne_movement: KingEntry,
    may_enter_corners: PieceSet::from_piece_type(King),
    hostility: HostilityRules {
        throne: PieceSet::from_piece_type(Soldier),
        corners: PieceSet::all(),
        edge: PieceSet::none()
    },
    slow_pieces: PieceSet::none(),
    attacker_starts: true
};

pub const COPENHAGEN_HNEFATAFL: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Strong,
    hostile_edge: false,
    armed_king: true,
    throne_movement: KingEntry,
    may_enter_corners: PieceSet::from_piece_type(King),
    hostility: HostilityRules {
        throne: PieceSet::all(),
        corners: PieceSet::from_piece_type(Soldier),
        edge: PieceSet::none()
    },
    slow_pieces: PieceSet::none(),
    attacker_starts: true
};
