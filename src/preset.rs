pub mod rules {
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::{King, Soldier};
    use crate::pieces::Side::Attacker;
    use crate::rules::KingAttack::Armed;
    use crate::rules::{HostilityRules, RepetitionRule, ShieldwallRules};
    use crate::rules::EnclosureWinRules::WithoutEdgeAccess;
    use crate::rules::KingStrength::{Strong, StrongByThrone};
    use crate::rules::ThroneRule::KingEntry;
    use crate::Ruleset;

    /// Rules for Copenhagen Hnefatafl.
    pub const COPENHAGEN: Ruleset = Ruleset {
        edge_escape: false,
        king_strength: Strong,
        king_attack: Armed,
        shieldwall: Some(ShieldwallRules {
            corners_may_close: true,
            captures: PieceSet::from_piece_type(Soldier)
        }),
        exit_fort: true,
        throne_movement: KingEntry,
        may_enter_corners: PieceSet::from_piece_type(King),
        hostility: HostilityRules {
            throne: PieceSet::all(),
            corners: PieceSet::from_piece_type(Soldier),
            edge: PieceSet::none()
        },
        slow_pieces: PieceSet::none(),
        starting_side: Attacker,
        enclosure_win: Some(WithoutEdgeAccess),
        repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: true })
    };

    /// Rules for Federation Brandubh.
    pub const BRANDUBH: Ruleset = Ruleset {
        edge_escape: false,
        king_strength: StrongByThrone,
        king_attack: Armed,
        shieldwall: None,
        exit_fort: false,
        throne_movement: KingEntry,
        may_enter_corners: PieceSet::from_piece_type(King),
        hostility: HostilityRules {
            throne: PieceSet::from_piece_type(Soldier),
            corners: PieceSet::all(),
            edge: PieceSet::none()
        },
        slow_pieces: PieceSet::none(),
        starting_side: Attacker,
        enclosure_win: Some(WithoutEdgeAccess),
        repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: true })
    };

    pub const MAGPIE: Ruleset = Ruleset {
        edge_escape: false,
        king_strength: Strong,
        king_attack: Armed,
        shieldwall: None,
        exit_fort: false,
        throne_movement: KingEntry,
        may_enter_corners: PieceSet::from_piece_type(King),
        hostility: HostilityRules {
            throne: PieceSet::all(),
            corners: PieceSet::all(),
            edge: PieceSet::none(),
        },
        slow_pieces: PieceSet::from_piece_type(King),
        starting_side: Attacker,
        enclosure_win: None,
        repetition_rule: None
    };
}

pub mod boards {
    pub const COPENHAGEN: &str =
        "3ttttt3/5t5/11/t4T4t/t3TTT3t/tt1TTKTT1tt/t3TTT3t/t4T4t/11/5t5/3ttttt3";
    
    pub const BRANDUBH: &str = "3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3";

    pub const MAGPIE: &str = "3t3/1t3t1/3T3/t1TKT1t/3T3/1t3t1/3t3";
}