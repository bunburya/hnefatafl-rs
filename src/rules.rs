use crate::pieces::PieceSet;
use crate::pieces::PieceType::Soldier;
use crate::rules::ThroneRule::KingEntry;
use crate::KingStrength::{Strong, StrongByThrone};
use crate::PieceType::King;
use std::cmp::PartialEq;
use crate::rules::KingAttack::Armed;

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

/// Whether king may participate in captures.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum KingAttack {
    /// King can participate in captures in same way as normal pieces.
    Armed,
    /// King may be the passive capturing piece (ie, may be captured against) but cannot initiate
    /// captures.
    Anvil,
    /// King may initiate captures but cannot be captured against.
    Hammer
}


/// A struct describing what pieces certain special tiles are considered hostile to.
#[derive(Copy, Clone, Debug)]
pub struct HostilityRules {
    pub(crate) throne: PieceSet,
    pub(crate) corners: PieceSet,
    pub(crate) edge: PieceSet
}

/// Rules relating to shieldwall captures.
#[derive(Clone, Copy, Debug)]
pub struct ShieldwallRules {
    /// Whether a shieldwall may be closed at one end by a corner.
    pub corners_may_close: bool,
    /// The pieces that may be captured by a shieldwall.
    pub captures: PieceSet
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
    /// Rules relating to the king's ability to participate in captures.
    pub king_attack: KingAttack,
    /// Rules relating to shieldwall captures.
    pub shieldwall: Option<ShieldwallRules>,
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
    king_strength: StrongByThrone,
    hostile_edge: false,
    king_attack: Armed,
    shieldwall: None,
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

/// Rules for Copenhagen Hnefatafl.
pub const COPENHAGEN_HNEFATAFL: Ruleset = Ruleset {
    edge_escape: false,
    king_strength: Strong,
    hostile_edge: false,
    king_attack: Armed,
    shieldwall: Some(ShieldwallRules {
        corners_may_close: true,
        captures: PieceSet::from_piece_type(Soldier)
    }),
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
