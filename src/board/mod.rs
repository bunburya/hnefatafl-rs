use crate::board::geometry::BoardGeometry;
use crate::board::state::BoardState;

pub mod geometry;
pub mod state;

pub struct Board<T: BoardState> {
    state: T,
    geo: BoardGeometry
}