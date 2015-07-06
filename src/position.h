/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2010 Marco Costalba, Joona Kiiski, Tord Romstad

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#if !defined(POSITION_H_INCLUDED)
#define POSITION_H_INCLUDED

#include "osl_position.h"

#include "osl_eval.h"
typedef osl::OpenMidEndingEval eval_t; 
#include <iostream>
#include "move.h"
#include "types.h"

/// Maximum number of plies per game (220 should be enough, because the
/// maximum search depth is 100, and during position setup we reset the
/// move counter for every non-reversible move).
const int MaxGameLength = 220;

class Position;

/// Game phase
enum Phase {
  PHASE_ENDGAME = 0,
  PHASE_MIDGAME = 128
};


/// The StateInfo struct stores information we need to restore a Position
/// object to its previous state when we retract a move. Whenever a move
/// is made on the board (by calling Position::do_move), an StateInfo object
/// must be passed as a parameter.

struct StateInfo {
  int gamePly, pliesFromNull;
  Key key;
  PieceType capturedType;
  StateInfo* previous;
};

/// The position data structure. A position consists of the following data:
///
///    * For each piece type, a bitboard representing the squares occupied
///      by pieces of that type.
///    * For each color, a bitboard representing the squares occupied by
///      pieces of that color.
///    * A bitboard of all occupied squares.
///    * A bitboard of all checking pieces.
///    * A 64-entry array of pieces, indexed by the squares of the board.
///    * The current side to move.
///    * Information about the castling rights for both sides.
///    * The initial files of the kings and both pairs of rooks. This is
///      used to implement the Chess960 castling rules.
///    * The en passant square (which is SQ_NONE if no en passant capture is
///      possible).
///    * The squares of the kings for both sides.
///    * Hash keys for the position itself, the current pawn structure, and
///      the current material situation.
///    * Hash keys for all previous positions in the game for detecting
///      repetition draws.
///    * A counter for detecting 50 move rule draws.

class Position {

  Position(); // No default or copy c'tor allowed
  Position(const Position& pos);

public:
  enum GamePhase {
      MidGame,
      EndGame
  };

  // Constructors
  Position(const Position& pos, int threadID);
  Position(const std::string& fen, int threadID);

  // Text input/output
  void from_fen(const std::string& fen);
  const std::string to_fen() const;
  void print(Move m = MOVE_NONE) const;

  // Copying
  void flip();

  // The piece on a given square
  PtypeO piece_on(Square s) const;
  PieceType type_of_piece_on(Square s) const;
  Player color_of_piece_on(Square s) const;
  bool square_is_empty(Square s) const;
  bool square_is_occupied(Square s) const;
  Value type_value_of_piece_on(Square s) const;
  Value promote_value_of_piece_on(Square s) const;
  Value midgame_value_of_piece_on(Square s) const;
  Value endgame_value_of_piece_on(Square s) const;

  // Side to move
  Player side_to_move() const;


  // Number of pieces of each color and type
  int piece_count(Player c, PieceType pt) const;


  // Current king position for each color
  Square king_square(Player c) const;


  bool in_check() const;

  // Piece lists
  Square piece_list(Player c, PieceType pt, int index) const;
  const Square* piece_list_begin(Player c, PieceType pt) const;


  // Properties of moves
  bool pl_move_is_legal(Move m) const;
  bool pl_move_is_evasion(Move m) const;
  bool move_is_legal(const Move m) const;
  bool move_gives_check(Move m) const;
  bool move_is_capture(Move m) const;
  bool move_is_capture_or_promotion(Move m) const;
  bool move_is_passed_pawn_push(Move m) const;
  bool move_attacks_square(Move m, Square s) const;

  // Piece captured with previous moves
  PieceType captured_piece_type() const;

  // Information about pawns
  bool pawn_is_passed(Player c, Square s) const;

  // Weak squares
  bool square_is_weak(Square s, Player c) const;

  // Doing and undoing moves
  void do_setup_move(Move m);
  void do_move(Move m, StateInfo& st);
  template<typename F>
  void do_undo_move(Move m, StateInfo& st,F const& f);
  template<typename F>
  void do_undo_null_move(StateInfo& st, F const& f);

  // Static exchange evaluation
  int see(Square from, Square to) const;
  int see(Move m) const;
  int see_sign(Move m) const;

  // Accessing hash keys
  Key get_key() const;
  Key get_exclusion_key() const;


  // Game termination checks
  bool is_mate() const;
  // if is_draw return false, and ret==-1 -> continous check by side_to_move
  // if is_draw return false, and ret==1  -> continous check by opposit_color
  bool is_draw(int& ret) const; 

  // Number of plies from starting position
  int startpos_ply_counter() const;

  // Other properties of the position
  bool opposite_colored_bishops() const;

  // Current thread ID searching on the position
  int thread() const;
  int pliesFromNull() const;

  int64_t nodes_searched() const;
  void set_nodes_searched(int64_t n);

  // Position consistency check, for debugging
  bool is_ok(int* failedStep = NULL) const;
  bool eval_is_ok() const;

  // Static member functions
  static void init_zobrist();
  static void init_piece_square_tables();

#ifdef COPY_STATE
  osl::NumEffectState osl_start_state;
  osl::NumEffectState *osl_state;
#else
  osl::NumEffectState osl_state;
#endif
  std::array<int,2> continuous_check; // number of a player's continuous check moves
  eval_t *eval;
private:

  // Initialization helper functions (used while setting up a position)
  void clear();
  void detach();
  void put_piece(PtypeO p, Square s);

  // Helper functions for doing and undoing moves
  void do_capture_move(Key& key, PieceType capture, Player them, Square to, bool ep);
  void find_checkers();

  // Computing hash keys from scratch (for initialization and debugging)
  Key compute_key() const;

  Key history[MaxGameLength];
  StateInfo startState;
  int startPosPlyCounter;
  int threadID;
  int64_t nodes;
  StateInfo* st;

  // Static variables
#if 0
  static osl::misc::CArray3d<Key,2,osl::PTYPE_SIZE,osl::Square_SIZE> zobrist;
#else
  static std::array<std::array<std::array<Key,osl::Square_SIZE>,osl::PTYPE_SIZE>,2> zobrist;
#endif
  static Key zobSideToMove;
  static Key zobExclusion;
  static const Value seeValues[osl::PTYPE_SIZE];
  static const Value PieceValueType[osl::PTYPE_SIZE];
  static const Value PieceValueMidgame[osl::PTYPE_SIZE];
  static const Value PieceValueEndgame[osl::PTYPE_SIZE];
  static const Value PromoteValue[osl::PTYPE_SIZE];
};

inline int64_t Position::nodes_searched() const {
  return nodes;
}

inline void Position::set_nodes_searched(int64_t n) {
  nodes = n;
}

inline PtypeO Position::piece_on(Square s) const {
#ifdef COPY_STATE
  return osl::ptypeO(osl_state->pieceAt(s));
#else
  return osl::ptypeO(osl_state.pieceAt(s));
#endif
}

inline Player Position::color_of_piece_on(Square s) const {
#ifdef COPY_STATE
  return owner(osl_state->pieceAt(s));
#else
  return owner(osl_state.pieceAt(s));
#endif
}

inline PieceType Position::type_of_piece_on(Square s) const {
#ifdef COPY_STATE
  return ptype(osl_state->pieceAt(s));
#else
  return ptype(osl_state.pieceAt(s));
#endif
}

inline bool Position::square_is_empty(Square s) const {
#ifdef COPY_STATE
  return isEmpty(osl_state->pieceAt(s));
#else
  return isEmpty(osl_state.pieceAt(s));
#endif
}

inline bool Position::square_is_occupied(Square s) const {
#ifdef COPY_STATE
  return !isEmpty(osl_state->pieceAt(s));
#else
  return !isEmpty(osl_state.pieceAt(s));
#endif
}

inline Value Position::midgame_value_of_piece_on(Square s) const {
  return PieceValueMidgame[I(type_of_piece_on(s))];
}

inline Value Position::endgame_value_of_piece_on(Square s) const {
  return PieceValueEndgame[I(type_of_piece_on(s))];
}
inline Value Position::promote_value_of_piece_on(Square s) const {
  return PromoteValue[I(type_of_piece_on(s))];
}

inline Value Position::type_value_of_piece_on(Square s) const {
  return PieceValueType[I(type_of_piece_on(s))];
}

inline Player Position::side_to_move() const {
#ifdef COPY_STATE
  return osl_state->turn();
#else
  return osl_state.turn();
#endif
}

inline Square Position::king_square(Player c) const {
#ifdef COPY_STATE
  return osl_state->kingSquare(c);
#else
  return osl_state.kingSquare(c);
#endif
}

inline bool Position::in_check() const {
#ifdef COPY_STATE
  return osl_state->inCheck();
#else
  return osl_state.inCheck();
#endif
}

inline Key Position::get_key() const {
  return st->key;
}

inline Key Position::get_exclusion_key() const {
  return st->key ^ zobExclusion;
}

inline int Position::startpos_ply_counter() const {
  return startPosPlyCounter;
}


inline bool Position::move_is_capture(Move m) const {
  return isCapture(m);
}

inline bool Position::move_is_capture_or_promotion(Move m) const {
  return isCaptureOrPromotion(m);
}

inline PieceType Position::captured_piece_type() const {
  
  return st->capturedType;
}

inline int Position::thread() const {
  return threadID;
}

inline int Position::pliesFromNull() const {
  return st->pliesFromNull;
}

#endif // !defined(POSITION_H_INCLUDED)
