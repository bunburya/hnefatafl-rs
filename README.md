# hnefatafl-rs

`hnefatafl` is a Rust crate intended to provide functionality for creating software related to the [tafl](https://en.wikipedia.org/wiki/Tafl_games) family
of board games. It provides structs and traits that encapsulate game data and logic, helping to build applications, AIs, etc.

It is not a goal of this crate to provide any concrete implementations of game clients or AIs.

## What is implemented

Currently the crate includes a number of structs, traits and enums to describe various aspects of a tafl game:

- game rules;
- the board and its current state;
- current game state;
- move validation; and
- determining the outcome of a move.

Many of the most popular rule variants can be constructed, including Copenhagen Hnefatafl, Tablut, Brandubh and Sea
Battle.  Boards up to 21x21 are supported.

A very basic demo (a terminal-based Brandubh game) is also implemented (run `cargo run --features demo` to play).

## What is not (yet) implemented

This crate is an early work in progress and I plan to add more features and expand support for the tafl family of games
in future.

There are many different variants of tafl, and this crate does not support all possible rules. In particular, the
"berserk" rules are not yet implemented, nor are any of the other variants which involve extra pieces beyond soldiers
and the king.
