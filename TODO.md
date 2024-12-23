## Immediate priorities

- [X] Implement `Coords` as unbounded row-col pair and use where appropriate.
- [X] Testing for exit fort detection.
- [X] Iterate occupied squares.
- [X] Iterate possible moves for a piece.
- [X] Advanced win conditions (enclosures, repetition rule)
- [X] Ensure everything is documented, including overview of project, what is/is not supported and assumptions.
- [X] Clean up `lib.rs` to expose clean public interface.
- [X] Larger boards.
- [ ] Review against https://rust-lang.github.io/api-guidelines/.
- [X] Refactor to allow more efficient tree generation.
  - BoardState: Current state of the board.
  - BoardGeometry: Contains information regarding the immutable physical properties of the board (size, etc).
  - GameState: Small struct containing mutable game state that is necessary to evaluate plays/positions.
  - GameLogic: Game rules and immutable state. Implements functions for evaluating plays/positions. Does not hold
    GameState internally, but rather, these functions take an instance of GameState as an argument.

## Support for extra rules/pieces

- [ ] Implement berserk rule.
- [ ] Implement support for other piece types.

## OpenTafl integration

- [ ] Implement `Ruleset` <-> OpenTafl rule notation.
- [ ] Implement `Board` <-> OpenTafl board notation.
- [ ] Implement server/client using OpenTafl protocol.

## Proof of concept client

- [X] CLI client.
- [ ] GUI client.
- [ ] Implement AI for Brandubh.

## Beyond

- [ ] Python interface