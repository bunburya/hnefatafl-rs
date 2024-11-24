## Immediate priorities

- [X] Implement `Coords` as unbounded row-col pair and use where appropriate.
- [X] Testing for exit fort detection.
- [ ] Remove references to non-standard units for now.
- [X] Iterate occupied squares.
- [X] Iterate possible moves for a piece.
- [X] Advanced win conditions (enclosures, repetition rule)
- [ ] Ensure everything is documented, including overview of project, what is/is not supported and assumptions.
- [ ] Clean up `lib.rs` to expose clean public interface.
- [X] Larger boards.
- [ ] Review against https://rust-lang.github.io/api-guidelines/.

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