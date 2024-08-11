#[derive(Debug)]
pub(crate) enum ParseError {
    BadStringLen(usize),
    BadLineLen(usize),
    BadChar(char)
}

#[derive(Debug)]
pub(crate) enum MoveError {
    DisjointTiles
}
