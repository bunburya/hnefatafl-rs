use crate::pieces::{PieceSet, Side};
use std::cmp::PartialEq;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Rules relating to whether and when the king is strong (must be surrounded by hostile tiles on
/// all four sides to be captured).
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum KingStrength {
    /// King must be surrounded by four hostile pieces or tiles to be captured.
    Strong,
    /// King can be captured by two hostile pieces or tiles, except on or near the throne when four
    /// are necessary.
    StrongByThrone,
    /// King may be captured by two hostile pieces or tiles, in the same way as other pieces.
    Weak,
}

/// Whether king may participate in captures.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum KingAttack {
    /// King can participate in captures in same way as normal pieces.
    Armed,
    /// King may be the passive capturing piece (ie, may be captured against) but cannot initiate
    /// captures.
    Anvil,
    /// King may initiate captures but cannot be captured against.
    Hammer,
}

/// What pieces certain special tiles are considered hostile to.
///
/// **NOTE:** Generally speaking, hostility rules are applied to empty tiles only. A tile will
/// not be considered hostile if occupied by a friendly piece.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HostilityRules {
    pub throne: PieceSet,
    pub corners: PieceSet,
    pub edge: PieceSet,
}

/// What pieces may occupy special tiles.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct OccupyRules {
    pub throne: PieceSet,
    pub corners: PieceSet,
}

/// What pieces may pass through special tiles.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PassRules {
    pub throne: PieceSet,
}

/// Rules relating to shieldwall captures.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ShieldwallRules {
    /// Whether a shieldwall may be closed at one end by a corner.
    pub corners_may_close: bool,
    /// The pieces that may be captured by a shieldwall.
    pub captures: PieceSet,
}

/// Circumstances in which attacker wins as a result of enclosing all defenders.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum EnclosureWinRules {
    /// Attacker wins if defender is entirely surrounded, even if defender has edge access.
    WithEdgeAccess,
    /// Attacker wins if defender is entirely surrounded without edge access.
    WithoutEdgeAccess,
}

/// Consequence of repeated plays.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RepetitionRule {
    /// Number of repetitions that will trigger the rule.
    pub n_repetitions: usize,
    /// Whether repetitions result in a loss for the repeating player. If this is `false`, then
    /// repetitions will result in a draw.
    pub is_loss: bool,
}

/// A set of rules for a tafl game.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Ruleset {
    /// The pieces that may be used in the game.
    pub pieces: PieceSet,
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    pub edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    pub king_strength: KingStrength,
    /// Rules relating to the king's ability to participate in captures.
    pub king_attack: KingAttack,
    /// Rules relating to shieldwall captures.
    pub shieldwall: Option<ShieldwallRules>,
    /// Whether the king can escape through an exit fort.
    pub exit_fort: bool,
    /// What special tiles are hostile to which pieces.
    pub hostile_tiles: HostilityRules,
    /// What pieces may occupy special tiles.
    pub occupiable_tiles: OccupyRules,
    /// What pieces may pass through special tiles.
    pub passable_tiles: PassRules,
    /// Types of piece whose movement is restricted to one tile per move.
    pub slow_pieces: PieceSet,
    /// Which side goes first.
    pub starting_side: Side,
    /// Whether attacker can win by enclosing all defending pieces.
    pub enclosure_win: Option<EnclosureWinRules>,
    /// Whether repeated moves result in a loss or draw.
    pub repetition_rule: Option<RepetitionRule>,
    /// Whether the game is drawn when one player has no legal plays available to it. If `false`,
    /// the player with no available plays loses.
    pub draw_on_no_plays: bool,
    /// Whether the game supports "Linnaean capture" (if the king is on the throne, surrounded by
    /// three enemies and one friendly soldier, that friendly soldier may be captured against the
    /// occupied throne).
    pub linnaean_capture: bool,
}
