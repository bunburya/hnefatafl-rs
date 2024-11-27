use crate::board_state::BoardState;
use crate::error::{BoardError, InvalidPlay};
use crate::game_logic::{Enclosure, GameLogic};
use crate::game_state::GameState;
use crate::pieces::{Piece, Side};
use crate::play::{Play, PlayRecord};
use crate::play_iter::PlayIterator;
use crate::rules::Ruleset;
use crate::tiles::{Coords, Tile};
use crate::ParseError;
use std::cmp::PartialEq;
use std::collections::HashSet;
use std::str::FromStr;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
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
    /// The other side has no legal moves available.
    NoMoves,
    /// The other side has repeated a move too many times.
    Repetition
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum DrawReason {
    /// A move has been repeated too many times.
    Repetition
}

/// The outcome of a single game.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum GameOutcome {
    /// Game has been won by the specified side.
    Winner(WinReason, Side),
    /// Game has ended in a draw.
    Draw(DrawReason)
}

/// A struct describing the outcome of a single move.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct PlayOutcome {
    /// Tiles containing pieces that have been captured by the move.
    pub captures: HashSet<Tile>,
    /// The outcome of the game, if the move has brought the game to an end.
    pub game_outcome: Option<GameOutcome>
}

/// The current status of the game.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum GameStatus {
    /// Game is still ongoing.
    Ongoing,
    /// Game is over, with the given outcome.
    Over(GameOutcome)
}

/// Whether a move is valid.
#[derive(Eq, PartialEq, Debug)]
pub enum MoveValidity {
    /// Move is valid.
    Valid,
    /// Move is invalid, for the given reason.
    Invalid(InvalidPlay)
}

/// A struct representing a single game, including all state and associated information (such as
/// rules) needed to play.
#[derive(Clone)]
pub struct Game<T: BoardState> {
    pub logic: GameLogic,
    pub state: GameState<T>,
    pub play_history: Vec<PlayRecord>,
}

impl<T: BoardState> Game<T> {

    /// Create a new [`Game`] from the given rules and starting positions.
    pub fn new(rules: Ruleset, starting_board: &str) -> Result<Self, ParseError> {
        let state: GameState<T> = GameState::new(starting_board, rules.starting_side)?;
        let logic = GameLogic::new(rules, state.board.side_len());
            
        Ok(Self { state, logic, play_history: vec![] })
    }
    
    /// Determine whether the given tile is hostile specifically by reference to the rules regarding
    /// hostility of special tiles.
    pub fn special_tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        self.logic.special_tile_hostile(tile, piece)
    }

    /// Determine whether the given tile is hostile to the given piece.
    pub fn tile_hostile(&self, tile: Tile, piece: Piece) -> bool {
        self.logic.tile_hostile(tile, piece, &self.state.board)
    }
    
    /// Determine whether the position at the given coordinates is hostile to the given piece,
    /// including whether the position is a hostile edge. 
    pub fn coords_hostile(&self, coords: Coords, piece: Piece) -> bool {
        self.logic.coords_hostile(coords, piece, &self.state.board)
    }

    /// Whether the given piece can occupy or pass the given tile according to the game rules and
    /// current board state. Returns a pair of `bool`s indicating whether the piece can occupy or
    /// pass, respectively.
    pub fn can_occupy_or_pass(&self, play: Play, piece: Piece) -> (bool, bool) {
        self.logic.can_occupy_or_pass(play, piece, &self.state)
    }

    /// Check whether a play is valid for the given side.
    pub fn check_play_validity_for_side(&self, play: Play, side: Side) -> MoveValidity {
        self.logic.check_play_validity_for_side(play, side, &self.state)
    }

    /// Check whether a move is valid.
    pub fn check_play_validity(&self, play: Play) -> MoveValidity {
        self.logic.check_play_validity(play, &self.state)
    }

    /// Check whether the king is *currently* strong (must be surrounded on all four sides to be
    /// captured), considering the game rules and the king's current position (for example, the
    /// rules may provide that the king is only strong on or beside the throne). 
    pub fn king_is_strong(&self) -> bool {
        self.logic.king_is_strong(&self.state.board)
    }

    /// Whether the tile (if any) at the given [`Coords`] can theoretically be occupied by the given
    /// piece according to the rules of the game. Does not take account of whether the tile is
    /// already occupied or actually accessible.
    pub fn coords_occupiable(&self, coords: Coords, piece: Piece) -> bool {
        self.logic.coords_occupiable(coords, piece)
    }

    /// Check whether the given [`Enclosure`] is "secure", ie, no piece on its boundary is
    /// vulnerable to capture. If `inside_safe` is `true`, then any square inside the enclosure is
    /// considered not to be threatening to a boundary piece. Similarly, if `outside_safe` is
    /// `true`, then any square not inside the enclosure is considered not to be threatening to a
    /// boundary piece.
    pub fn enclosure_secure(&self, encl: &Enclosure, inside_safe: bool, outside_safe: bool) -> bool {
        self.logic.enclosure_secure(encl, inside_safe, outside_safe, &self.state.board)
    }
    
    
    /// Detect whether the given move has created a shieldwall according to the applicable rules.
    /// Returns `None` if no shieldwall is detected; otherwise returns a set of tiles that have been
    /// captured in the shieldwall.
    fn detect_shieldwall(&self, play: Play) -> Option<HashSet<Tile>> {
        self.logic.detect_shieldwall(play, &self.state)
    }
    
    /// Detect whether the king is in an exit fort.
    pub fn detect_exit_fort(&self) -> bool {
        self.logic.detect_exit_fort(&self.state.board)
    }

    /// Get the tiles containing pieces captured by the given play.
    pub fn get_captures(&self, play: Play, moving_piece: Piece) -> HashSet<Tile> {
        self.logic.get_captures(play, moving_piece, &self.state)

    }

    /// Get the outcome of the game, if any. If None, the game is still ongoing.
    pub fn get_game_outcome(
        &self,
        play: Play,
        moving_piece: Piece,
        caps: &HashSet<Tile>
    ) -> Option<GameOutcome> {
        self.logic.get_game_outcome(play, moving_piece, caps, &self.state)
    }


    /// Actually "do" a move, checking validity, getting outcome, applying outcome to board state,
    /// switching side to play and returning a description of the game status following the move.
    pub fn do_move(&mut self, play: Play) -> Result<GameStatus, InvalidPlay> {
        let (state, play_record) = self.logic.do_move(play, self.state)?;
        self.state = state;
        self.play_history.push(play_record);
        Ok(self.state.status)
    }

    /// Iterate over the possible plays that can be made by the piece at the given tile. Returns an
    /// error if there is no piece at the given tile. Order of iteration is not guaranteed.
    pub fn iter_plays(&self, tile: Tile) -> Result<PlayIterator<T>, BoardError> {
        PlayIterator::new(&self.logic, &self.state, tile)
    }
    
    /// Whether the given side could make any play given the current board.
    pub fn side_can_play(&self, side: Side) -> bool {
        self.logic.side_can_play(side, &self.state)
    }
}

#[cfg(test)]
mod tests {
    use crate::board_state::{BoardState, HugeBoardState, LargeBoardState};
    use crate::game::Game;
    use crate::game::GameOutcome::Winner;
    use crate::game::InvalidPlay::{
        BlockedByPiece,
        MoveOntoBlockedTile,
        MoveThroughBlockedTile,
        NoPiece,
        OutOfBounds,
        TooFar
    };
    use crate::game::MoveValidity::{Invalid, Valid};
    use crate::game::WinReason::{KingCaptured, KingEscaped, Repetition};
    use crate::pieces::PieceSet;
    use crate::pieces::PieceType::King;
    use crate::pieces::Side::{Attacker, Defender};
    use crate::play::Play;
    use crate::preset::{boards, rules};
    use crate::rules::ThroneRule::NoPass;
    use crate::rules::{Ruleset, ShieldwallRules};
    use crate::tiles::Tile;
    use crate::GameStatus::{Ongoing, Over};
    use crate::PieceType::Soldier;
    use crate::{hashset, HostilityRules, MediumBoardState, Piece, SmallBoardState};
    use std::collections::HashSet;
    use std::str::FromStr;

    const TEST_RULES: Ruleset = Ruleset {
        slow_pieces: PieceSet::from_piece_type(King),
        throne_movement: NoPass,
        ..rules::BRANDUBH
    };
    

    fn generic_test_play_validity<T: BoardState>() {
        let mut fb_game: Game<T> = Game::new(
            rules::BRANDUBH,
            boards::BRANDUBH
        ).unwrap();

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 1),
                Tile::new(4, 1)
            ).unwrap()),
            Valid
        );
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 0)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(1, 1),
                Tile::new(2, 1)
            ).unwrap()),
            Invalid(NoPiece)
        );
        
        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(0, 7)
            ).unwrap()),
            Invalid(OutOfBounds)
        );

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(0, 3),
                Tile::new(2, 3)
            ).unwrap()),
            Invalid(BlockedByPiece)
        );

        fb_game.do_move(Play::from_tiles(
            Tile::new(3, 1),
            Tile::new(4, 1)
        ).unwrap()).unwrap();

        let play = Play::from_tiles(
            Tile::new(3, 3),
            Tile::new(3, 2)
        ).unwrap();
        assert_eq!(
            fb_game.check_play_validity(play),
            Invalid(BlockedByPiece)
        );

        
        //println!("{}", fb_game.state.board);
        fb_game.state.board.move_piece(Tile::new(3, 2), Tile::new(4, 2));
        fb_game.state.board.move_piece(Tile::new(3, 3), Tile::new(3, 2));
        //println!("{}", fb_game.state.board);

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(2, 3),
                Tile::new(3, 3)
            ).unwrap()),
            Invalid(MoveOntoBlockedTile)
        );

        assert_eq!(
            fb_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 3)
            ).unwrap()),
            Valid
        );
        
        let mut test_game: Game<T> = Game::new(
            TEST_RULES,
            "7/5Tt/2T4/2t2t1/Tt4T/2t4/2T2K1"
        ).unwrap();
        
        test_game.state.side_to_play = Defender;
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 3)
            ).unwrap()),
            Invalid(TooFar)
        );
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(6, 5),
                Tile::new(6, 4)
            ).unwrap()),
            Valid
        );
        
        test_game.state.side_to_play = Attacker;
        
        assert_eq!(
            test_game.check_play_validity(Play::from_tiles(
                Tile::new(3, 2),
                Tile::new(3, 4)
            ).unwrap()),
            Invalid(MoveThroughBlockedTile)
        );
    }

    #[test]
    fn test_play_validity() {
        generic_test_play_validity::<SmallBoardState>();
        generic_test_play_validity::<MediumBoardState>();
        generic_test_play_validity::<LargeBoardState>();
        generic_test_play_validity::<HugeBoardState>();
    }

    fn generic_test_play_outcome<T: BoardState>() {

        let test_game: Game<T> = Game::new(
            TEST_RULES,
            "4t2/5Tt/2T4/2t2t1/Tt4T/2t4/2T2K1"
        ).unwrap();

        // First, move the piece on the board directly and check that it picks up the correct
        // captures. Then, reverse the move, and call `do_move` to check that the correct game
        // outcome is detected.

        let mut game = test_game.clone();
        let play = Play::from_tiles(Tile::new(0, 4), Tile::new(6, 4)).unwrap();
        let piece = game.state.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [Tile::new(6, 5)].into()
        );
        game.state.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Over(Winner(KingCaptured, Attacker))));

        let mut game = test_game.clone();
        game.state.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(4, 6), Tile::new(4, 2)).unwrap();
        let piece = game.state.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [
                Tile::new(4, 1),
                Tile::new(3, 2),
                Tile::new(5, 2),
            ].into()
        );
        game.state.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Ongoing));

        let mut game = test_game.clone();
        game.state.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(6, 5), Tile::new(6, 6)).unwrap();
        let piece = game.state.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [].into(),
        );
        game.state.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Over(Winner(KingEscaped, Defender))));

        let mut game = test_game.clone();
        game.state.side_to_play = Defender;
        let play = Play::from_tiles(Tile::new(6, 5), Tile::new(5, 5)).unwrap();
        let piece = game.state.board.move_piece(play.from, play.to());
        assert_eq!(
            game.get_captures(play, piece),
            [].into()
        );
        game.state.board.move_piece(play.to(), play.from);
        assert_eq!(game.do_move(play), Ok(Ongoing));
    }

    #[test]
    fn test_play_outcome() {
        generic_test_play_outcome::<SmallBoardState>();
        generic_test_play_outcome::<MediumBoardState>();
        generic_test_play_outcome::<LargeBoardState>();
        generic_test_play_outcome::<HugeBoardState>();
    }

    #[test]
    fn test_shieldwalls() {

        let no_corner_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::from(Soldier)
            }),
            ..rules::COPENHAGEN
        };

        let king_capture_rules = Ruleset{
            shieldwall: Some(ShieldwallRules{
                corners_may_close: false,
                captures: PieceSet::all()
            }),
            ..rules::COPENHAGEN
        };

        let corner_sw = "9/9/9/9/6t2/7tT/7tT/7tT/9";
        let regular_sw = "9/9/9/6t2/7tT/7tT/7tT/8t/9";
        let regular_sw_king = "9/9/9/6t2/7tT/7tK/7tT/8t/9";
        let no_sw_gap = "9/9/9/6t2/7tT/8T/7tT/8t/9";
        let no_sw_friend = "9/9/9/6t2/7tT/6tTT/7tT/8t/9";
        let no_sw_small = "9/9/9/6t2/7tT/8t/9/9/9";

        let cm = Play::from_tiles(
            Tile::new(4, 6),
            Tile::new(4, 8)
        ).unwrap();
        let m = Play::from_tiles(
            Tile::new(3, 6),
            Tile::new(3, 8)
        ).unwrap();
        let n = Play::from_tiles(
            Tile::new(3, 6),
            Tile::new(3, 7)
        ).unwrap();

        let g_corner: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &corner_sw).unwrap();
        assert_eq!(g_corner.detect_shieldwall(n), None);
        assert_eq!(g_corner.detect_shieldwall(cm), Some(hashset!(
            Tile::new(5, 8),
            Tile::new(6, 8),
            Tile::new(7, 8)
        )));
        
        let g_no_corner: Game<MediumBoardState> = Game::new(no_corner_rules, &corner_sw).unwrap();
        assert_eq!(g_no_corner.detect_shieldwall(m), None);
        
        let g_regular: Game<MediumBoardState> = Game::new(no_corner_rules, &regular_sw).unwrap();
        assert_eq!(g_regular.detect_shieldwall(m), Some(hashset!(
            Tile::new(4, 8),
            Tile::new(5, 8),
            Tile::new(6, 8)
        )));
        
        let g_king: Game<MediumBoardState> = Game::new(no_corner_rules, &regular_sw_king).unwrap();
        assert_eq!(g_king.detect_shieldwall(m), Some(hashset!(
            Tile::new(4, 8),
            Tile::new(6, 8)
        )));
        
        let g_gap: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_gap).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);

        let g_friend: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_friend).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);

        let g_small: Game<MediumBoardState> = Game::new(no_corner_rules, &no_sw_small).unwrap();
        assert_eq!(g_gap.detect_shieldwall(m), None);
    }
    
    #[test]
    fn test_encl_secure() {
        let setup_1 = "7/2ttt2/1t1K1t1/2ttt2/7";
        let setup_2 = "7/1tttt2/1t1K1t1/2tttt1/7";
        let setup_3 = "2t1t2/1t1t1t1/1t1K1t1/2ttt2/7";
        let setup_4 = "2t2t1/1t3t1/1t1K1t1/2ttt2/7";
        
        let safe_corners = Ruleset {
            hostility: HostilityRules {
                corners: PieceSet::none(),
                edge: PieceSet::none(),
                throne: PieceSet::none()
            },
            ..rules::COPENHAGEN
        };
        
        let candidates = [
            // string, inside_safe, outside_safe, is_secure, rules
            (&setup_1, false, true, true, rules::COPENHAGEN),
            (&setup_1, false, false, false, rules::COPENHAGEN),
            (&setup_2, false, true, true, rules::COPENHAGEN),
            (&setup_2, true, false, true, rules::COPENHAGEN),
            (&setup_3, false, true, false, rules::COPENHAGEN),
            (&setup_4, false, true, false, rules::COPENHAGEN),
            (&setup_4, false, true, true, safe_corners),
            (&setup_4, true, false, true, rules::COPENHAGEN),
        ];
        for (string, inside_safe, outside_safe, is_secure, rules) in candidates {
            let g: Game<MediumBoardState> = Game::new(rules, string).unwrap();
            let encl_opt = g.logic.find_enclosure(
                Tile::new(2, 3),
                PieceSet::from(King),
                PieceSet::from(Piece::new(Soldier, Attacker)),
                false, false,
                &g.state.board,
            );
            assert!(encl_opt.is_some());
            let encl = encl_opt.unwrap();
            assert_eq!(g.enclosure_secure(&encl, inside_safe, outside_safe), is_secure)
        }

    }
    
    #[test]
    fn test_exit_forts() {
        let exit_fort_flat = "9/9/8t/7tT/7T1/6tT1/7TK/7tT/9";
        let exit_fort_bulge = "9/9/9/9/9/5TTTT/5T2K/6TTT/9";
        let no_fort_enemy = "9/9/9/8T/7Tt/7T1/7TK/8T/9";
        let no_fort_unfree = "9/9/9/8T/7TT/7TT/7TK/8T/9";
        let no_fort_gap = "9/9/9/8T/9/4t2T1/7TK/8T/9";
        let no_fort_vuln = "9/9/9/9/9/6TTT/5T2K/6TTT/9";
        for s in [exit_fort_flat, exit_fort_bulge] {
            let g: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &s).unwrap();
            assert!(g.detect_exit_fort());
        }
        for s in [no_fort_enemy, no_fort_unfree, no_fort_gap, no_fort_vuln] {
            let g: Game<MediumBoardState> = Game::new(rules::COPENHAGEN, &s).unwrap();
            assert!(!g.detect_exit_fort());
        }
    }
    
    #[test]
    fn test_iter_plays() {
        let game: Game<SmallBoardState> = Game::new(rules::BRANDUBH, boards::BRANDUBH).unwrap();
        assert!(game.iter_plays(Tile::new(0, 0)).is_err());
        assert!(game.iter_plays(Tile::new(1, 0)).is_err());
        let outer_att_tile = Tile::new(0, 3);
        let outer_att_iter = game.iter_plays(outer_att_tile);
        assert!(outer_att_iter.is_ok());
        assert_eq!(
            outer_att_iter.unwrap().collect::<HashSet<Play>>(),
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
            inner_att_iter.unwrap().collect::<HashSet<Play>>(),
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
            outer_def_iter.unwrap().collect::<HashSet<Play>>(),
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
        assert_eq!(king_iter.unwrap().collect::<HashSet<Play>>(), HashSet::new());
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "1T5/7/7/1t3K1/7/7/7"
        ).unwrap();

        // Test moving through (but not onto) throne and blocking by piece
        let test_tile = Tile::new(3, 1);
        let iter = game.iter_plays(test_tile);
        assert!(iter.is_ok());
        assert_eq!(
            iter.unwrap().collect::<HashSet<Play>>(),
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
    fn test_can_play() {
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "2tt3/1tTKt2/2tt3/7/7/7/7"
        ).unwrap();
        assert!(game.side_can_play(Attacker));
        assert!(!game.side_can_play(Defender));
        let game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            "2tKt2/3t3/7/7/7/7/7"
        ).unwrap();
        assert!(game.side_can_play(Attacker));
        assert!(!game.side_can_play(Defender));
    } 
    
    #[test]
    fn test_repetitions() {
        let mut game: Game<SmallBoardState> = Game::new(
            rules::BRANDUBH,
            boards::BRANDUBH
        ).unwrap();
        for _ in 0..3 {
            game.do_move(Play::from_str("d6-f6").unwrap()).unwrap();
            game.do_move(Play::from_str("d5-f5").unwrap()).unwrap();
            game.do_move(Play::from_str("f6-d6").unwrap()).unwrap();
            game.do_move(Play::from_str("f5-d5").unwrap()).unwrap();
        }
        assert_eq!(game.state.status, Ongoing);
        game.do_move(Play::from_str("d6-f6").unwrap()).unwrap();

        assert_eq!(game.state.status, Over(Winner(Repetition, Defender)));
    }

}