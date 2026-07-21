use crate::board::geometry::BoardGeometry;
use crate::collections::PieceMap;
use crate::rules::Ruleset;

pub struct Variant<P: PieceMap> {
    rules: Ruleset,
    board_geometry: BoardGeometry<P>,
    starting_position: P
}