use std::ops::BitOr;

#[derive(Copy, Clone)]
enum PieceType {
    King = 0x01,
    Soldier = 0x02,
    Knight = 0x04,
    Commander = 0x08,
    Guard = 0x10,
    Mercenary = 0x20
}

struct PieceSet(u8);

impl Default for PieceSet {
    fn default() -> Self {
        Self { 0: 0 }
    }
}

impl PieceSet {

    pub(crate) fn from_piece_types<T: IntoIterator<Item = PieceType>>(piece_types: T) -> Self {
        PieceSet{ 0: piece_types.into_iter().fold(0, |a, p| a | (p as u8)) }
    }
    
    pub(crate) fn set(&mut self, piece: PieceType) {
        self.0 |= piece as u8
    }

    pub(crate) fn unset(&mut self, piece: PieceType) {
        self.0 &= !(piece as u8)
    }

    pub(crate) fn get(&self, piece: PieceType) -> bool {
        self.0 & (piece as u8) > 0
    }
    
}

#[cfg(test)]
mod tests {
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::{Commander, Guard, King, Knight, Mercenary, Soldier};

    #[test]
    fn test_piece_set() {
        let mut ps = PieceSet::from_piece_types(vec![
            King,
            Soldier,
            Guard
        ]);
        assert!(ps.get(King));
        assert!(ps.get(Soldier));
        assert!(ps.get(Guard));
        assert!(!ps.get(Commander));
        assert!(!ps.get(Knight));
        assert!(!ps.get(Mercenary));

        ps.unset(King);
        assert!(!ps.get(King));
        assert!(ps.get(Soldier));
        assert!(ps.get(Guard));
        assert!(!ps.get(Commander));
        assert!(!ps.get(Knight));
        assert!(!ps.get(Mercenary));

        ps.set(Commander);
        assert!(ps.get(Commander));
        assert!(!ps.get(King));
        assert!(ps.get(Soldier));
        assert!(ps.get(Guard));
        assert!(!ps.get(Knight));
        assert!(!ps.get(Mercenary));
    }

}