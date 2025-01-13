use hnefatafl::board::state::{BoardState, MediumBasicBoardState};
use hnefatafl::error::ParseError;
use hnefatafl::error::ParseError::EmptyString;
use hnefatafl::game::Game;
use hnefatafl::game::{GameOutcome, GameStatus};
use hnefatafl::pieces::Side;
use hnefatafl::play::Play;
use hnefatafl::preset::{boards, rules};
use hnefatafl::rules::Ruleset;
use hnefatafl::tiles::Tile;
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

fn play_captures_from_str(s: &str) -> Result<(Play, HashSet<Tile>), ParseError> {
    if s.is_empty() {
        return Err(EmptyString);
    }
    let tokens = s.split('x').collect::<Vec<&str>>();
    let play = Play::from_str(tokens[0])?;
    let mut captures: HashSet<Tile> = HashSet::new();
    for c in tokens[1..].iter() {
        captures.insert(Tile::from_str(c)?);
    }
    Ok((play, captures))

}


fn test_real_games(rules: Ruleset, starting_posn: &str, fname: &str) {
    let f: PathBuf = [
        env!("CARGO_MANIFEST_DIR"),
        "resources",
        "test",
        "games",
        fname
    ].iter().collect();
    let s = fs::read_to_string(f).unwrap();
    let lines = s.split('\n');
    let mut last_game_status: GameStatus = GameStatus::Ongoing;
    for line in lines {
        if line.starts_with('#') {
            continue
        }
        let mut g: Game<MediumBasicBoardState> = Game::new(
            rules,
            starting_posn
        ).unwrap();
        let cols = line.split(',').collect::<Vec<&str>>();
        let outcome = cols.last().unwrap();
        if outcome.is_empty() {
            continue
        }
        let plays = cols[0].split(' ').collect::<Vec<&str>>();
        //println!("{line}");
        for p_str in plays {
            //println!("{p_str}");
            //println!("{}", g.board);
            if let Ok((p, c)) = play_captures_from_str(p_str) {
                assert!(g.logic.validate_play(p, &g.state).is_ok());
                let piece = g.state.board.move_piece(p.from, p.to());
                let captures = g.logic.get_captures(p, piece, &g.state);
                g.state.board.move_piece(p.to(), p.from);
                if !c.is_empty() {
                    // Test data doesn't report capture of king as a capture using "x" notation
                    let without_king: HashSet<Tile> = captures.iter()
                        .map(|c| c.tile)
                        .filter(|t| !g.state.board.is_king(*t))
                        .collect();
                    assert_eq!(without_king, c);
                }

                let game_status_res = g.do_play(p);
                assert!(game_status_res.is_ok());
                last_game_status = game_status_res.unwrap();
            } else {
                assert_eq!(p_str, "timeout")
            }
        }

        if let GameStatus::Over(GameOutcome::Win(_, side)) = last_game_status {
            let expected = match side {
                Side::Attacker => "Black",
                Side::Defender => "White"
            };
            assert_eq!(&expected, outcome);
        }
    }
}

#[test]
fn test_real_copenhagen() {
    test_real_games(
        rules::COPENHAGEN,
        boards::COPENHAGEN,
        "copenhagen.csv"
    )
}

#[test]
fn test_real_brandubh() {
    test_real_games(
        rules::BRANDUBH,
        boards::BRANDUBH,
        "brandubh.csv"
    )
}