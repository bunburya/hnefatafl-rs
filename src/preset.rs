pub mod rules {
    use crate::pieces::{PieceSet, BASIC_PIECES, KING};
    use crate::pieces::PieceType::{King, Soldier};
    use crate::pieces::Side::Attacker;
    use crate::rules::KingAttack::Armed;
    use crate::rules::{HostilityRules, OccupyRules, PassRules, RepetitionRule, Ruleset, ShieldwallRules};
    use crate::rules::EnclosureWinRules::WithoutEdgeAccess;
    use crate::rules::KingStrength::{Strong, StrongByThrone};

    /// Rules for Copenhagen Hnefatafl.
    pub const COPENHAGEN: Ruleset = Ruleset {
        pieces: BASIC_PIECES,
        edge_escape: false,
        king_strength: Strong,
        king_attack: Armed,
        shieldwall: Some(ShieldwallRules {
            corners_may_close: true,
            captures: PieceSet::from_piece_type(Soldier)
        }),
        exit_fort: true,
        hostile_tiles: HostilityRules {
            throne: PieceSet::all(),
            corners: PieceSet::from_piece_type(Soldier),
            edge: PieceSet::none()
        },
        occupiable_tiles: OccupyRules {
            throne: PieceSet::from_piece(KING),
            corners: PieceSet::from_piece(KING)
        },
        passable_tiles: PassRules {
            throne: PieceSet::all()
        },
        slow_pieces: PieceSet::none(),
        starting_side: Attacker,
        enclosure_win: Some(WithoutEdgeAccess),
        repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: true }),
        draw_on_no_plays: false,
        linnaean_capture: false,
    };

    /// Rules for Federation Brandubh.
    pub const BRANDUBH: Ruleset = Ruleset {
        pieces: BASIC_PIECES,
        edge_escape: false,
        king_strength: StrongByThrone,
        king_attack: Armed,
        shieldwall: None,
        exit_fort: false,
        hostile_tiles: HostilityRules {
            throne: PieceSet::from_piece_type(Soldier),
            corners: PieceSet::all(),
            edge: PieceSet::none()
        },
        occupiable_tiles: OccupyRules {
            throne: PieceSet::from_piece(KING),
            corners: PieceSet::from_piece(KING)
        },
        passable_tiles: PassRules {
            throne: PieceSet::all()
        },
        slow_pieces: PieceSet::none(),
        starting_side: Attacker,
        enclosure_win: Some(WithoutEdgeAccess),
        repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: true }),
        draw_on_no_plays: false,
        linnaean_capture: false
    };

    /// Rules for Magpie.
    pub const MAGPIE: Ruleset = Ruleset {
        pieces: BASIC_PIECES,
        edge_escape: false,
        king_strength: Strong,
        king_attack: Armed,
        shieldwall: None,
        exit_fort: false,
        hostile_tiles: HostilityRules {
            throne: PieceSet::all(),
            corners: PieceSet::all(),
            edge: PieceSet::none(),
        },
        occupiable_tiles: OccupyRules {
            throne: PieceSet::from_piece(KING),
            corners: PieceSet::from_piece(KING)
        },
        passable_tiles: PassRules {
            throne: PieceSet::from_piece(KING)
        },
        slow_pieces: PieceSet::from_piece_type(King),
        starting_side: Attacker,
        enclosure_win: None,
        repetition_rule: None,
        draw_on_no_plays: false,
        linnaean_capture: false
    };

    /// Rules for Linnaeus Tablut.
    pub const TABLUT: Ruleset = Ruleset {
        pieces: BASIC_PIECES,
        edge_escape: true,
        king_strength: StrongByThrone,
        king_attack: Armed,
        shieldwall: None,
        exit_fort: false,
        hostile_tiles: HostilityRules {
            throne: PieceSet::all(),
            corners: PieceSet::none(),
            edge: PieceSet::none()
        },
        occupiable_tiles: OccupyRules {
            throne: PieceSet::none(),
            corners: PieceSet::all()
        },
        passable_tiles: PassRules {
            throne: PieceSet::none()
        },
        slow_pieces: PieceSet::none(),
        starting_side: Attacker,
        enclosure_win: None,
        repetition_rule: Some(RepetitionRule { n_repetitions: 3, is_loss: false }),
        draw_on_no_plays: true,
        linnaean_capture: true
    };
}

pub mod boards {
    pub const COPENHAGEN: &str =
        "3ttttt3/5t5/11/t4T4t/t3TTT3t/tt1TTKTT1tt/t3TTT3t/t4T4t/11/5t5/3ttttt3";
    
    pub const BRANDUBH: &str = "3t3/3t3/3T3/ttTKTtt/3T3/3t3/3t3";

    pub const MAGPIE: &str = "3t3/1t3t1/3T3/t1TKT1t/3T3/1t3t1/3t3";
    
    pub const TABLUT: &str = "3ttt3/4t4/4T4/t3T3t/ttTTKTTtt/t3T3t/4T4/4t4/3ttt3";
}