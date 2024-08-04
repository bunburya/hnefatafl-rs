struct Ruleset {
    /// Whether defender wins by getting king to edge of board (otherwise, corner escape is
    /// assumed).
    edge_escape: bool,
    /// Whether the king is strong (must be surrounded by four opponents or hostile tiles to be
    /// captured).
    strong_king: bool,
    /// Whether the king is armed (can participate in captures).
    armed_king: bool
}