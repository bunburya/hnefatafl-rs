## Immediate priorities

- [ ] Review against https://rust-lang.github.io/api-guidelines/.
- [ ] Improve center square logic.
  - [ ] Remove special case for strong king beside throne from game logic. The king can be captured by three pieces
    beside the throne if the empty throne is hostile to the king.
- [ ] Game protocol (JSON?)
  - [ ] Textual representation of game rules.
  - [ ] JSON representation of game state.

## Support for extra rules/pieces

- [ ] Implement berserk rule.
- [ ] Specify maximum move distance for each piece type.
- [ ] Implement support for other piece types.

## OpenTafl integration

- [ ] Implement `Ruleset` <-> OpenTafl rule notation.
- [ ] Implement `Board` <-> OpenTafl board notation.
- [ ] Implement server/client using OpenTafl protocol.

## Beyond

- [ ] Implement bigints to remove dependency.
- [ ] Python interface
- [ ] tafl server
- [ ] `no_std` support