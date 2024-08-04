use crate::board::{Board, Tile};
use crate::board::Side::{Attacker, Defender};

mod board;
mod rules;

fn main()  {
    let test_board_str = "...A...\n...A...\n...D...\nAADKDAA\n...D...\n...A...\n...A...";
    let b = Board::try_from(test_board_str);
    match b {
        Ok(mut board) => {
            println!("{board}");
            board.place_piece(&Tile::new(1, 5), Attacker);
            board.place_piece(&Tile::new(4, 1), Defender(false));
            board.move_piece(&Tile::new(3, 3), &Tile::new(0, 4));
            println!("{board}");
            println!("King is at {:?}", board.get_king());
        },
        Err(error) => println!("ERROR: {error:?}"),
    }
}
