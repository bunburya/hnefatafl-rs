pub mod logic;
pub mod state;

use crate::board::state::{BoardState, HugeBasicBoardState, LargeBasicBoardState, MediumBasicBoardState, SmallBasicBoardState};
use crate::error::{BoardError, PlayInvalid, ParseError};
use crate::game::logic::GameLogic;
use crate::game::state::GameState;
use crate::pieces::{PlacedPiece, Side};
use crate::play::{Play, PlayRecord, ValidPlayIterator};
use crate::rules::Ruleset;
use crate::tiles::Tile;
use std::cmp::PartialEq;
use std::collections::HashSet;

/// The reason why a game has been won.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum WinReason {
    /// King has escaped in the "normal" way, ie, by reaching an edge or corner.
    KingEscaped,
    /// King has escaped through an exit fort.
    ExitFort,
    /// King has been captured.
    KingCaptured,
    /// All the other side's pieces have been captured.
    AllCaptured,
    /// The other side is completely enclosed (with or without edge or corner access, depending on
    /// the rules).
    Enclosed,
    /// The other side has no legal plays available.
    NoPlays,
    /// The other side has repeated a move too many times.
    Repetition
}

/// The reason why a game has been drawn.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum DrawReason {
    /// A move has been repeated too many times.
    Repetition,
    /// Player has no legal plays available.
    NoPlays
}

/// The outcome of a single game.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum GameOutcome {
    /// Game has been won by the specified side.
    Win(WinReason, Side),
    /// Game has ended in a draw.
    Draw(DrawReason)
}

/// The effects of a single play, including captures and the game outcome caused by the play, if
/// any.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct PlayEffects {
    /// Tiles containing pieces that have been captured by the move.
    pub captures: HashSet<PlacedPiece>,
    /// The outcome of the game, if the move has brought the game to an end.
    pub game_outcome: Option<GameOutcome>
}

/// The current status of the game.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum GameStatus {
    /// Game is still ongoing.
    Ongoing,
    /// Game is over, with the given outcome.
    Over(GameOutcome)
}

/// A struct representing a single game, including all state and associated information (such as
/// rules) needed to play. This struct also keeps a record of all previous plays and the game state
/// after each turn (to allow undoing plays).
#[derive(Clone)]
pub struct Game<T: BoardState> {
    pub logic: GameLogic,
    pub state: GameState<T>,
    pub play_history: Vec<PlayRecord>,
    pub state_history: Vec<GameState<T>>
}

impl<T: BoardState> Game<T> {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        let state: GameState<T> = GameState::new(starting_board, rules.starting_side)?;
        let logic = GameLogic::new(rules, state.board.side_len());
            
        Ok(Self { state, logic, play_history: vec![], state_history: vec![state] })
    }
    
    /// Actually "do" a play, checking validity, getting outcome, applying outcome to board state,
    /// switching side to play and returning a description of the game status following the move.
    pub fn do_play(&mut self, play: Play) -> Result<GameStatus, PlayInvalid> {
        let (state, play_record) = self.logic.do_play(play, self.state)?.into();
        self.state_history.push(self.state);
        self.state = state;
        self.play_history.push(play_record);
        Ok(self.state.status)
    }
    
    pub fn undo_last_play(&mut self) {
        if let Some(state) = self.state_history.pop() {
            self.state = state;
            self.play_history.pop();
        }
    }

    /// Iterate over the possible plays that can be made by the piece at the given tile. Returns an
    /// error if there is no piece at the given tile. Order of iteration is not guaranteed.
    pub fn iter_plays(&self, tile: Tile) -> Result<ValidPlayIterator<T>, BoardError> {
        ValidPlayIterator::new(&self.logic, &self.state, tile)
    }
    
}

/// Game supporting basic pieces (soldier and king), suitable for boards up to 7x7.
pub type SmallBasicGame = Game<SmallBasicBoardState>;
/// Game supporting basic pieces (soldier and king), suitable for boards up to 11x11.
pub type MediumBasicGame = Game<MediumBasicBoardState>;
/// Game supporting basic pieces (soldier and king), suitable for boards up to 15x15.
pub type LargeBasicGame = Game<LargeBasicBoardState>;
/// Game supporting basic pieces (soldier and king), suitable for boards up to 21x21.
pub type HugeBasicGame = Game<HugeBasicBoardState>;

#[cfg(test)]
mod tests {
    use crate::board::state::SmallBasicBoardState;
    use crate::game::Game;
    use crate::play::Play;
    use crate::preset::{boards, rules};
    use crate::tiles::Tile;
    use std::collections::HashSet;

    #[test]
    fn test_iter_plays() {
        let game: Game<SmallBasicBoardState> = Game::new(rules::BRANDUBH, boards::BRANDUBH).unwrap();
        assert!(game.iter_plays(Tile::new(0, 0)).is_err());
        assert!(game.iter_plays(Tile::new(1, 0)).is_err());
        let outer_att_tile = Tile::new(0, 3);
        let outer_att_iter = game.iter_plays(outer_att_tile);
        assert!(outer_att_iter.is_ok());
        assert_eq!(
            outer_att_iter.unwrap().map(|vp| vp.play).collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(outer_att_tile, Tile::new(0, 1)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 2)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 4)).unwrap(),
                Play::from_tiles(outer_att_tile, Tile::new(0, 5)).unwrap()
            )
        );
        let inner_att_tile = Tile::new(1, 3);
        let inner_att_iter = game.iter_plays(inner_att_tile);
        assert!(inner_att_iter.is_ok());
        assert_eq!(
            inner_att_iter.unwrap().map(|vp| vp.play).collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(inner_att_tile, Tile::new(1, 0)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 1)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 2)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 4)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 5)).unwrap(),
                Play::from_tiles(inner_att_tile, Tile::new(1, 6)).unwrap()
            )
        );
        let outer_def_tile = Tile::new(2, 3);
        let outer_def_iter = game.iter_plays(outer_def_tile);
        assert!(outer_def_iter.is_ok());
        assert_eq!(
            outer_def_iter.unwrap().map(|vp| vp.play).collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(outer_def_tile, Tile::new(2, 0)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 1)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 2)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 4)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 5)).unwrap(),
                Play::from_tiles(outer_def_tile, Tile::new(2, 6)).unwrap()
            )
        );
        let king_tile = Tile::new(3, 3);
        let king_iter = game.iter_plays(king_tile);
        assert!(king_iter.is_ok());
        assert_eq!(king_iter.unwrap().map(|vp| vp.play).collect::<HashSet<Play>>(), HashSet::new());
        let game: Game<SmallBasicBoardState> = Game::new(
            rules::BRANDUBH,
            "1T5/7/7/1t3K1/7/7/7"
        ).unwrap();

        // Test moving through (but not onto) throne and blocking by piece
        let test_tile = Tile::new(3, 1);
        let iter = game.iter_plays(test_tile);
        assert!(iter.is_ok());
        assert_eq!(
            iter.unwrap().map(|vp| vp.play).collect::<HashSet<Play>>(),
            hashset!(
                Play::from_tiles(test_tile, Tile::new(1, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(2, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(4, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(5, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(6, 1)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 0)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 2)).unwrap(),
                Play::from_tiles(test_tile, Tile::new(3, 4)).unwrap()
            )
        )
    }
    
    #[test]
    fn test_undo() {
        let mut g: Game<SmallBasicBoardState> = Game::new(rules::BRANDUBH, boards::BRANDUBH).unwrap();
        let state_0 = g.state.clone();
        g.do_play(Play::from_tiles(Tile::new(0, 3), Tile::new(0, 2)).unwrap()).unwrap();
        let state_1 = g.state.clone();
        assert_ne!(state_0, state_1);
        g.do_play(Play::from_tiles(Tile::new(2, 3), Tile::new(2, 1)).unwrap()).unwrap();
        let state_2 = g.state.clone();
        assert_ne!(state_0, state_2);
        g.do_play(Play::from_tiles(Tile::new(1, 3), Tile::new(1, 1)).unwrap()).unwrap();
        let state_3 = g.state.clone();
        assert_ne!(state_0, state_3);
        g.undo_last_play();
        assert_eq!(g.state, state_2);
        g.undo_last_play();
        assert_eq!(g.state, state_1);
        g.undo_last_play();
        assert_eq!(g.state, state_0);
        g.undo_last_play();
        assert_eq!(g.state, state_0);

    }
    

}