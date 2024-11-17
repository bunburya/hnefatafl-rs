pub mod rules {
    use crate::KingStrength::{Strong, StrongByThrone};
    use crate::rules::KingAttack::Armed;
    use crate::rules::ShieldwallRules;
    use crate::{HostilityRules, PieceSet, Ruleset};
    use crate::PieceType::{King, Soldier};
    use crate::rules::EnclosureWinRules::WithoutEdgeAccess;
    use crate::Side::Attacker;
    use crate::ThroneRule::KingEntry;

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
        enclosure_win: Some(WithoutEdgeAccess)
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
        enclosure_win: Some(WithoutEdgeAccess)
    };
}

pub mod boards {
    pub const COPENHAGEN: &str = "\
        ...ttttt...\n\
        .....t.....\n\
        ...........\n\
        t....T....t\n\
        t...TTT...t\n\
        tt.TTKTT.tt\n\
        t...TTT...t\n\
        t....T....t\n\
        ...........\n\
        .....t.....\n\
        ...ttttt...\
    ";
    
    pub const BRANDUBH: &str = "\
        ...t...\n\
        ...t...\n\
        ...T...\n\
        ttTKTtt\n\
        ...T...\n\
        ...t...\n\
        ...t...\
    ";
}