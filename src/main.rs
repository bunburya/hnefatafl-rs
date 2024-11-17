use std::io::stdin;
use std::str::FromStr;
use hnefatafl::{Game, Play, SmallBoardState};
use hnefatafl::GameStatus::Over;
use hnefatafl::GameOutcome::{Draw, Winner};
use hnefatafl::preset;

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
    let mut game: Game<SmallBoardState> = Game::new(
        preset::rules::BRANDUBH, 
        preset::boards::BRANDUBH,
    ).expect("Could not create game.");
    loop {
        println!("Board:");
        println!("{}", game.board);
        println!("{:?} to play.", game.side_to_play);

        let play = get_play();
        match game.do_move(play) {
            Ok(status) => {
                if let Over(outcome) = status {
                    match outcome {
                        Draw => println!("Game over. Draw."),
                        Winner(side) => println!("Game over. Winner is {side:?}."),
                    }
                    println!("Final board:");
                    println!("{}", game.board);
                    return
                }
            },
            Err(e) => println!("Invalid move ({e:?}). Try again.")
        }
    }
}