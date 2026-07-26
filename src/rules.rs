use crate::pieces::{Piece, PieceSet, PieceType, Side, KING};
use std::cmp::PartialEq;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use crate::board::geometry::{SpecialTilePlacementRules, SpecialTiles};
use crate::collections::{PieceDict, PieceMap};
use crate::collections::PieceTypeDict;
use crate::pieces::PieceType::{Commander, King, Knight, Soldier};

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
    /// King is captured if all neighbouring tiles are hostile (ie, if the king is by the edge,
    /// it can be captured if surrounded by three hostile tiles).
    Middleweight,
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
    /// King may not participate in captures
    Unarmed
}

/// Associates each type of special tile with a set of pieces. Used to define rules around how
/// special tiles interact with different pieces.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct SpecialTilePieceSets {
    pub throne: PieceSet,
    pub corners: PieceSet,
    pub attacker_fortresses: PieceSet,
    pub defender_fortresses: PieceSet,
}

impl SpecialTilePieceSets {
    const fn none() -> Self {
        Self {
            throne: PieceSet::none(),
            corners: PieceSet::none(),
            attacker_fortresses: PieceSet::none(),
            defender_fortresses: PieceSet::none(),
        }
    }
}

/// What pieces certain special tiles are considered hostile to.
///
/// **NOTE:** Generally speaking, hostility rules are applied to empty tiles only. A tile will
/// not be considered hostile if occupied by a friendly piece.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct HostilityRules {
    pub special_tiles: SpecialTilePieceSets,
    pub edge: PieceSet,
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
    // TODO: Need to allow for wins for repeating player as well.
}

/// Describe a single piece type's ability to jump over other pieces.
#[derive(PartialEq, Eq, Clone, Copy, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct JumpAbility {
    /// Whether the piece may only jump to or from special tiles.
    pub special_only: bool,
    /// Whether the piece may capture a piece by jumping over it.
    pub can_capture: bool
}

/// Rules governing when pieces may jump over other (opposing) pieces.
///
/// **NOTE:** In order for a piece to be able to make a capturing jump, it must be present in both
/// `can_jump` *and* `can_capture`. A piece appearing in `can_capture` but not `can_jump` has no
/// effect.
#[derive(PartialEq, Eq, Clone, Copy, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct JumpRules {
    /// What pieces can jump.
    pub can_jump: PieceTypeDict<Option<JumpAbility>>,
    /// Pieces that may be jumped over.
    pub can_be_jumped: PieceSet,
}

impl JumpRules {
    /// Create a [`JumpRules`] that does not permit any jumping.
    pub const fn none() -> Self {
        Self {
            can_jump: PieceTypeDict::new(None),
            can_be_jumped: PieceSet::none(),
        }
    }
}

/// Rules governing berserk mode.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BerserkRule {
    /// A capturing piece can (and must) make a further capture if a capture is available.
    MustCapture,
    /// A capturing piece can (and must) make another play if a legal play is available.
    MustPlay
}

/// The [`JumpRules`] that generally apply in berserk games.
pub const BERSERK_JUMP_RULES: JumpRules = JumpRules {
    can_jump: PieceTypeDict::new(None)
        .with(King, Some(JumpAbility { special_only: true, can_capture: false }))
        .with(Knight, Some(JumpAbility { special_only: false, can_capture: true }))
        .with(Commander, Some(JumpAbility { special_only: false, can_capture: false })),
    can_be_jumped: PieceSet::from_piece_type(Soldier)
};

impl Default for PieceTypeDict<Option<JumpAbility>> {
    fn default() -> Self {
        let mut d = PieceTypeDict::new(None);
        d.set(PieceType::Knight, Some(JumpAbility { special_only: false, can_capture: true }));
        d.set(PieceType::Commander, Some(JumpAbility { special_only: false, can_capture: false }));
        d
    }
}

/// A set of rules for a tafl game.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Ruleset {
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
    pub occupiable_tiles: SpecialTilePieceSets,
    /// What pieces may pass through special tiles.
    pub passable_tiles: SpecialTilePieceSets,
    /// How many tiles each piece can move in a single play.
    pub speed: PieceDict<Option<u8>>,
    /// Which side goes first.
    pub starting_side: Side,
    /// Whether attacker can win by enclosing all defending pieces.
    pub enclosure_win: Option<EnclosureWinRules>,
    /// Whether repeated moves result in a loss or draw.
    pub repetition_rule: Option<RepetitionRule>,
    /// Whether the game is drawn when one player has no legal plays available to it. If `false`,
    /// the player with no available plays loses.
    pub draw_on_no_plays: bool,
    /// Whether the game should end in a draw after a certain number of consecutive plays without
    /// any captures.
    pub draw_after_captureless_plays: Option<usize>,
    /// Whether the game supports "Linnaean capture" (if the king is on the throne, surrounded by
    /// three enemies and one friendly soldier, that friendly soldier may be captured against the
    /// occupied throne).
    pub linnaean_capture: bool,
    /// Rules governing when pieces may jump over other (opposing) pieces. A jump is only possible
    /// over a single tile that is occupied by an enemy piece.
    pub jump_rules: JumpRules,
    /// Rule governing berserk mode (circumstances in which capturing pieces may make further
    /// moves).
    pub berserk_rule: Option<BerserkRule>
}

impl Default for Ruleset {
    fn default() -> Self {
        // Based on the default values in the OpenTafl Notation spec.
        Self {
            edge_escape: false,
            enclosure_win: Some(EnclosureWinRules::WithoutEdgeAccess),
            starting_side: Side::Attacker,
            repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: false }),
            king_attack: KingAttack::Armed,
            king_strength: KingStrength::Strong,
            jump_rules: JumpRules {
                can_jump: PieceTypeDict::default(),
                can_be_jumped: PieceSet::all()
            },
            speed: PieceDict::new(None),
            hostile_tiles: HostilityRules {
                special_tiles: SpecialTilePieceSets {
                    corners: PieceSet::all(),
                    throne: PieceSet::from(Side::Attacker),
                    attacker_fortresses: PieceSet::from(Side::Defender),
                    defender_fortresses: PieceSet::from(Side::Attacker)
                },
                edge: PieceSet::none()
            },
            passable_tiles: SpecialTilePieceSets {
                corners: PieceSet::from_piece(KING),
                throne: PieceSet::all(),
                attacker_fortresses: PieceSet::all(),
                defender_fortresses: PieceSet::from(Side::Defender)
            },
            occupiable_tiles: SpecialTilePieceSets {
                corners: PieceSet::from_piece(KING),
                throne: PieceSet::all(),
                attacker_fortresses: PieceSet::all(),
                defender_fortresses: PieceSet::all()
            },
            shieldwall: None,
            exit_fort: false,
            linnaean_capture: false,
            berserk_rule: None,
            draw_on_no_plays: false,
            draw_after_captureless_plays: None
        }
    }
}

/// All the information needed to initialise a game.
pub struct GameSetup<P: PieceMap> {
    pub ruleset: Ruleset,
    pub special_tile_rules: SpecialTilePlacementRules<P::BitField>,
    pub starting_position: P
}