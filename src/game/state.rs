use std::cmp::PartialEq;
use crate::board::state::{BoardState, HugeBasicBoardState, LargeBasicBoardState, MediumBasicBoardState, SmallBasicBoardState};
use crate::error::ParseError;
use crate::game::GameStatus;
use crate::game::GameStatus::Ongoing;
use crate::pieces::Side;
use crate::play::{Play, PlayRecord};
use crate::utils::FixedSizeQueue;

/// A short (fixed-size) record of the relevant information about a play we need to figure out
/// if it is a repetition of a previous play.
#[derive(Debug, Copy, Clone, PartialEq)]
#[derive(Eq)]
#[derive(Hash)]
struct ShortPlayRecord {
    side: Side,
    play: Play,
    captures: bool
}

impl From<&PlayRecord> for ShortPlayRecord {
    fn from(play_record: &PlayRecord) -> Self {
        Self {
            side: play_record.side,
            play: play_record.play,
            captures: !play_record.effects.captures.is_empty()
        }
    }
}

/// Keeps track of the number of consecutive times each side has repeated its last move.
///
/// A move is considered to be a repetition if
/// (1) it does not capture any pieces;
/// (2) it is the same as the fourth last move in the game record; and
/// (3) it is not merely a reversal of a previous repeating move.
///
/// To explain (3), for example, if a player moves (`a1-b1`, `b1-a1`, `a1-b1`, `b1-a1`), the second
/// `a1-b1` would count as a repetition but the second `b1-a1` would not (but would not force
/// a reset of the repetition counter).
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct RepetitionTracker {
    pub(crate) attacker_reps: usize,
    pub(crate) defender_reps: usize,
    attacker_mid_pair: bool,
    defender_mid_pair: bool,
    recent_plays: FixedSizeQueue<Option<ShortPlayRecord>, 4>
}

impl RepetitionTracker {

    fn is_mid_pair(&self, side: Side) -> bool {
        match side {
            Side::Attacker => self.attacker_mid_pair,
            Side::Defender => self.defender_mid_pair
        }
    }

    fn toggle_mid_pair(&mut self, side: Side) {
        match side {
            Side::Attacker => self.attacker_mid_pair = !self.attacker_mid_pair,
            Side::Defender => self.defender_mid_pair = !self.defender_mid_pair,
        }
    }

    /// Check whether play is a repetition (same move as four plays ago) and is not the second leg
    /// of a pair.
    ///
    /// Returns a tuple of `bool`s. The first bool is whether to increment the repetition counter.
    /// The second is whether to reset the repetition counter.
    fn check_repetition(&mut self, record: ShortPlayRecord) -> (bool, bool) {

        if (!record.captures) && (Some(record) == *self.recent_plays.first()) {
            let is_rep = !self.is_mid_pair(record.side);
            self.toggle_mid_pair(record.side);
            (is_rep, false)
        } else {
            (false, true)
        }
    }

    /// Return the number of consecutive repetitions recorded for the given side.
    pub fn get_repetitions(&self, side: Side) -> usize {
        match side {
            Side::Attacker => self.attacker_reps,
            Side::Defender => self.defender_reps,
        }
    }

    /// Track the given play, adding to the given player's repetition count if it is a repetition
    /// and resetting both counters back to zero otherwise.
    pub fn track_play(&mut self, side: Side, play: Play, captures: bool) {
        let record = ShortPlayRecord { side, play, captures };
        let (incr, reset) = self.check_repetition(record);
        if incr {
            match record.side {
                Side::Attacker => self.attacker_reps += 1,
                Side::Defender => self.defender_reps += 1,
            }
        } else if reset {
            match record.side {
                Side::Attacker => {
                    self.attacker_reps = 0;
                    self.attacker_mid_pair = false;
                },
                Side::Defender => {
                    self.defender_reps = 0;
                    self.defender_mid_pair = false;
                },
            }
        }
        self.recent_plays.push(Some(record));
    }
}

/// This strict contains all state that can be used to evaluate play outcomes and board positions
/// and that changes regularly. The idea is to keep this struct as small as possible to facilitate
/// efficient play evaluation.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GameState<T: BoardState> {
    /// Board state, ie, the current pieces on the board.
    pub board: T,
    /// The side whose turn it is.
    pub side_to_play: Side,
    /// Tracker for repetitions.
    pub repetitions: RepetitionTracker,
    /// Number of plays since a piece was last captured.
    pub plays_since_capture: usize,
    /// Current status of the game.
    pub status: GameStatus,
    /// Number of plays that have been taken by either side.
    pub turn: usize
}

impl <T: BoardState> GameState<T> {
    pub fn new(fen_str: &str, side_to_play: Side) -> Result<Self, ParseError> {
        Ok(Self {
            board: T::from_fen(fen_str)?,
            side_to_play,
            repetitions: RepetitionTracker::default(),
            plays_since_capture: 0,
            status: Ongoing,
            turn: 0
        })
    }
}

/// Game state supporting basic pieces (soldier and king), suitable for boards up to 7x7.
pub type SmallBasicGameState = GameState<SmallBasicBoardState>;
/// Game state supporting basic pieces (soldier and king), suitable for boards up to 11x11.
pub type MediumBasicGameState = GameState<MediumBasicBoardState>;
/// Game state supporting basic pieces (soldier and king), suitable for boards up to 15x15.
pub type LargeBasicGameState = GameState<LargeBasicBoardState>;
/// Game state supporting basic pieces (soldier and king), suitable for boards up to 21x21.
pub type HugeBasicGameState = GameState<HugeBasicBoardState>;

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use crate::game::state::RepetitionTracker;
    use crate::pieces::Side;
    use crate::play::Play;

    #[test]
    fn test_repetition_tracker() {
        let mut tracker = RepetitionTracker::default();
        for i in 0..5 {
            tracker.track_play(Side::Attacker, Play::from_str("a1-b1").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Attacker), i);
            tracker.track_play(Side::Defender, Play::from_str("a2-b2").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Defender), i);
            tracker.track_play(Side::Attacker, Play::from_str("b1-a1").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Attacker), i);
            tracker.track_play(Side::Defender, Play::from_str("b2-a2").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Defender), i);
        }
        for i in 0..5 {
            tracker.track_play(Side::Attacker, Play::from_str("f1-g1").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Attacker), i);
            tracker.track_play(Side::Defender, Play::from_str("f2-g2").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Defender), i);
            tracker.track_play(Side::Attacker, Play::from_str("g1-f1").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Attacker), i);
            tracker.track_play(Side::Defender, Play::from_str("g2-f2").unwrap(), false);
            assert_eq!(tracker.get_repetitions(Side::Defender), i);
        }
    }
}