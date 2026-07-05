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

There are many different variants of tafl, and this crate does not support all possible rules. For example, guards and
mercenaries (which are used in some variants such as Alea Evangelii) are not yet supported.

It is intended in the future to implement conversion between `hnefatafl-rs` rules, game states, etc, and 
[OpenTafl notation](https://github.com/jslater89/OpenTafl/blob/master/opentafl-notation-spec.txt) (or some other,
similar notation).
