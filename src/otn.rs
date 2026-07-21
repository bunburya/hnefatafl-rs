use std::fmt;
use crate::collections::PieceSet;
use crate::error::ParseError;
use crate::pieces::{Piece, PieceType};
use crate::rules::{KingAttack, KingStrength};

/// Something that is represented by a key:value pair in an OTN string.
pub trait OtnKey {
    const KEY: &'static str;
}

/// Something that can be written as, and parsed from, a *value* in an appropriate OTN string
/// (eg, the `m` in `ks:m`).
pub trait OtnValue
where Self: Sized {
    fn from_otn_value(otn: &str) -> Result<Self, ParseError>;
    fn fmt_otn_value(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result;
}

const PIECE_ORDER: [Piece; 12] = [
    Piece::attacker(PieceType::Soldier),
    Piece::attacker(PieceType::Commander),
    Piece::attacker(PieceType::Knight),
    Piece::attacker(PieceType::King),
    Piece::attacker(PieceType::Mercenary),
    Piece::attacker(PieceType::Guard),
    Piece::defender(PieceType::Soldier),
    Piece::defender(PieceType::Commander),
    Piece::defender(PieceType::Knight),
    Piece::defender(PieceType::King),
    Piece::defender(PieceType::Mercenary),
    Piece::defender(PieceType::Guard)
];

impl OtnValue for PieceSet {
    fn from_otn_value(otn: &str) -> Result<Self, ParseError> {
        let mut ps = PieceSet::none();
        for c in otn.chars() {
            ps.set_piece(Piece::try_from(c)?);
        }
        Ok(ps)
    }

    fn fmt_otn_value(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for pt in PIECE_ORDER {
            if self.contains(pt) {
                write!(f, "{}", char::from(pt))?;
            }
        }
        Ok(())
    }
}

impl OtnKey for KingStrength { const KEY: &'static str = "ks"; }

impl OtnValue for KingStrength {
    fn from_otn_value(otn: &str) -> Result<Self, ParseError> {
        match otn {
            "s" => Ok(KingStrength::Strong),
            "c" => Ok(KingStrength::StrongByThrone),
            "m" => Ok(KingStrength::Middleweight),
            "w" => Ok(KingStrength::Weak),
            _ => Err(ParseError::BadOtn)
        }
    }

    fn fmt_otn_value(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        match self {
            KingStrength::Strong => write!(f, "s"),
            KingStrength::StrongByThrone => write!(f, "c"),
            KingStrength::Middleweight => write!(f, "m"),
            KingStrength::Weak => write!(f, "w")
        }
    }
}

impl OtnKey for KingAttack { const KEY: &'static str = "ka"; }

impl OtnValue for KingAttack {
    fn from_otn_value(otn: &str) -> Result<Self, ParseError> {
        match otn {
            "y" => Ok(Self::Armed),
            "a" => Ok(Self::Anvil),
            "h" => Ok(Self::Hammer),
            "n" => Ok(Self::Unarmed),
            _ => Err(ParseError::BadOtn)
        }
    }

    fn fmt_otn_value(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Armed => write!(f, "y"),
            Self::Anvil => write!(f, "a"),
            Self::Hammer => write!(f, "h"),
            Self::Unarmed => write!(f, "n"),
        }
    }
}