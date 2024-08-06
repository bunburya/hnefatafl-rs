enum PieceType {
    King = 0x00,
    Soldier = 0x01,
    Knight = 0x02,
    Commander = 0x04,
    Guard = 0x08,
    Mercenary = 0x10
}

struct PieceSet(u8);

impl Default for PieceSet {
    fn default() -> Self {
        Self { 0: 0 }
    }
}

impl PieceSet {

    pub(crate) fn from_piece_types(piece_types: Vec<PieceType>) -> Self {
        let mut ps = PieceSet { 0: 0 };
        for t in piece_types {
            ps.set(t)
        }
        ps
    }
    
    pub(crate) fn set(&mut self, piece: PieceType) {
        self.0 |= piece as u8
    }

    pub(crate) fn unset(&mut self, piece: PieceType) {
        self.0 &= !(piece as u8)
    }

    pub(crate) fn get(&self, piece: PieceType) -> bool {
        self.0 & piece as u8 > 0
    }
    
}


