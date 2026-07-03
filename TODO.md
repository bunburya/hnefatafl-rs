## Immediate priorities

- [ ] Review against https://rust-lang.github.io/api-guidelines/.
- [ ] Game protocol (JSON?)
  - [ ] Textual representation of game rules.
  - [ ] JSON representation of game state.

## Support for extra rules/pieces

- [ ] Specify maximum move distance for each piece type.
- [ ] Implement support for other piece types.
- [ ] Implement middleweight king strength

## OpenTafl integration

- [ ] Implement `Ruleset` <-> OpenTafl rule notation.
- [ ] Implement `Board` <-> OpenTafl board notation.
- [ ] Implement server/client using OpenTafl protocol.

## Beyond

- [ ] Python interface
- [ ] tafl server
- [ ] `no_std` support