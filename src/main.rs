#![cfg(feature = "demo")]

use hnefatafl::preset;
use std::io::stdin;
use std::str::FromStr;
use hnefatafl::game::GameOutcome::{Draw, Win};
use hnefatafl::game::GameStatus::Over;
use hnefatafl::game::Game;
use hnefatafl::play::Play;
use hnefatafl::aliases::SmallBasicGame;

fn input(prompt: &str) -> std::io::Result<String> {
    println!("{prompt}");
    let mut s: String = String::new();
    stdin().read_line(&mut s)?;
    Ok(s.trim().to_string())
}

fn get_play() -> Play {
    loop {
        if let Ok(m_str) = input("Please enter your move:") {
            match Play::from_str(&m_str) {
                Ok(play) => return play,
                Err(e) => println!("Invalid move ({e:?}). Try again.")
            }
        } else {
            println!("Error reading input. Try again.");
        }
        
    }
}

fn main() {
    println!("hnefatafl-rs demo");
    let mut game: SmallBasicGame = Game::new(
        preset::rules::BRANDUBH, 
        preset::boards::BRANDUBH,
    ).expect("Could not create game.");
    loop {
        println!("Board:");
        println!("{}", game.state.board);
        println!("{:?} to play.", game.state.side_to_play);

        let play = get_play();
        match game.do_play(play) {
            Ok(status) => {
                if let Over(outcome) = status {
                    match outcome {
                        Draw(reason) => println!("Game over. Draw {reason:?}."),
                        Win(reason, side) => println!("Game over. Winner is {side:?} ({reason:?})."),
                    }
                    println!("Final board:");
                    println!("{}", game.state.board);
                    return
                }
            },
            Err(e) => println!("Invalid move ({e:?}). Try again.")
        }
    }
}