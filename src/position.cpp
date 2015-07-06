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

#include <cassert>
#include <cstring>
#include <fstream>
#include <map>
#include <iostream>
#include <sstream>

#include "movegen.h"
#include "position.h"
#include "rkiss.h"
#include "thread.h"
#include "tt.h"
#include "ucioption.h"

#include "osl_position.h"

#include "usi.h"
// #include "see.h"
//#include "safeMove.h"
#include "evaluate.h"
//#include "check_.h"
#include "osl_move.h"

using std::string;
using std::cout;
using std::endl;


#if 0
osl::misc::CArray3d<Key,2,osl::PTYPE_SIZE,osl::Square_SIZE> Position::zobrist;
#else
std::array<std::array<std::array<Key,osl::Square_SIZE>,osl::PTYPE_SIZE>,2> Position::zobrist;
#endif
Key Position::zobSideToMove;
Key Position::zobExclusion;


// Material values arrays, indexed by Piece

using osl::evVal;
const Value Position::PieceValueMidgame[osl::PTYPE_SIZE] = {
  VALUE_ZERO,VALUE_ZERO, Value(evVal[2]), Value(evVal[3]), 
  Value(evVal[4]), Value(evVal[5]), Value(evVal[6]), Value(evVal[7]), 
  Value(evVal[8]), Value(evVal[9]), Value(evVal[10]), Value(evVal[11]), 
  Value(evVal[12]), Value(evVal[13]), Value(evVal[14]), Value(evVal[15]),
};

const Value Position::PieceValueEndgame[osl::PTYPE_SIZE] = {
  VALUE_ZERO,VALUE_ZERO,
  Value(evVal[2] + evVal[10]), Value(evVal[3] + evVal[11]), 
  Value(evVal[4] + evVal[12]), Value(evVal[5] + evVal[13]), 
  Value(evVal[6] + evVal[14]), Value(evVal[7] + evVal[15]), 
  Value(evVal[8] * 2), Value(evVal[9] * 2),
  Value(evVal[10] * 2), Value(evVal[11] * 2),
  Value(evVal[12] * 2), Value(evVal[13] * 2),
  Value(evVal[14] * 2), Value(evVal[15] * 2),
  };

const Value Position::PromoteValue[osl::PTYPE_SIZE] = {
  VALUE_ZERO,VALUE_ZERO,
  VALUE_ZERO,VALUE_ZERO,
  VALUE_ZERO,VALUE_ZERO,
  VALUE_ZERO,VALUE_ZERO,
  VALUE_ZERO,VALUE_ZERO,
  Value(evVal[2] - evVal[10]), Value(evVal[3] - evVal[11]), 
  Value(evVal[4] - evVal[12]), Value(evVal[5] - evVal[13]), 
  Value(evVal[6] - evVal[14]), Value(evVal[7] - evVal[15]), 
};

const Value Position::PieceValueType[osl::PTYPE_SIZE] = {
  VALUE_ZERO,VALUE_ZERO,
  Value(4), Value(8), 
  Value(12), Value(16), 
  Value(24), Value(26), 
  Value(18), Value(26), 
  Value(2), Value(6), 
  Value(10), Value(14), 
  Value(20), Value(22), 
};




// Material values array used by SEE, indexed by PieceType

const Value Position::seeValues[] = {
  VALUE_ZERO,VALUE_ZERO,
  Value(evVal[2]), Value(evVal[3]), Value(evVal[4]), Value(evVal[5]),
  Value(evVal[6]), Value(evVal[7]), Value(evVal[8]), Value(evVal[9]),
  Value(evVal[10]), Value(evVal[11]), Value(evVal[12]), Value(evVal[13]),
  Value(evVal[14]), Value(evVal[15]),
};


namespace {


  struct PieceLetters : public std::map<char, PtypeO> {

    PieceLetters() {
      operator[]('K') = PtypeO::B_KING;
      operator[]('k') = PtypeO::W_KING;
      operator[]('R') = PtypeO::B_ROOK;
      operator[]('r') = PtypeO::W_ROOK;
      operator[]('B') = PtypeO::B_BISHOP;
      operator[]('b') = PtypeO::W_BISHOP;
      operator[]('G') = PtypeO::B_GOLD;
      operator[]('g') = PtypeO::W_GOLD;
      operator[]('S') = PtypeO::B_SILVER;
      operator[]('s') = PtypeO::W_SILVER;
      operator[]('N') = PtypeO::B_KNIGHT;
      operator[]('n') = PtypeO::W_KNIGHT;
      operator[]('L') = PtypeO::B_LANCE;
      operator[]('l') = PtypeO::W_LANCE;
      operator[]('P') = PtypeO::B_PAWN;
      operator[]('p') = PtypeO::W_PAWN;
      operator[]('.') = PtypeO::EMPTY;
    }


    string from_piece(PtypeO p) const {

      std::map<char, PtypeO>::const_iterator it;
      for (it = begin(); it != end(); ++it)
	if (it->second == unpromote(p))
	  return (isPromoted(getPtype(p)) ? string("+") : string(""))+string(1,it->first);

      assert(false);
      return 0;
    }
  };

  PieceLetters pieceLetters;
}




/// Position c'tors. Here we always create a copy of the original position
/// or the FEN string, we want the new born Position object do not depend
/// on any external data so we detach state pointer from the source one.

Position::Position(const Position& pos, int th) 
#ifndef COPY_STATE
  :osl_state(pos.osl_state)
#else
   :osl_state(&osl_start_state)
#endif
{
  memcpy(this, &pos, sizeof(Position));
  detach(); // Always detach() in copy c'tor to avoid surprises
  threadID = th;
  nodes = 0;
  eval=0;
}


Position::Position(const string& fen, int th) 
{

  eval=NULL;
  osl_state = &osl_start_state;
  from_fen(fen);
  threadID = th;
}


/// Position::detach() copies the content of the current state and castling
/// masks inside the position itself. This is needed when the st pointee could
/// become stale, as example because the caller is about to going out of scope.

void Position::detach() {

  startState = *st;
  st = &startState;
  st->previous = NULL; // as a safe guard
#ifdef COPY_STATE
  osl_start_state = *osl_state;
  osl_state = &osl_start_state;
  osl_state->previous = 0;
#endif
}


/// Position::from_fen() initializes the position object with the given FEN
/// string. This function is not very robust - make sure that input FENs are
/// correct (this is assumed to be the responsibility of the GUI).

void Position::from_fen(const string& fen) {
/*
   A FEN string defines a particular position using only the ASCII character set.

   A FEN string contains six fields. The separator between fields is a space. The fields are:

   1) Piece placement (from white's perspective). Each rank is described, starting with rank 8 and ending
      with rank 1; within each rank, the contents of each square are described from file A through file H.
      Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter taken
      from the standard English names. White pieces are designated using upper-case letters ("PNBRQK")
      while Black take lowercase ("pnbrqk"). Blank squares are noted using digits 1 through 8 (the number
      of blank squares), and "/" separate ranks.

   2) Active color. "w" means white moves next, "b" means black.

   3) Castling availability. If neither side can castle, this is "-". Otherwise, this has one or more
      letters: "K" (White can castle kingside), "Q" (White can castle queenside), "k" (Black can castle
      kingside), and/or "q" (Black can castle queenside).

   4) En passant target square in algebraic notation. If there's no en passant target square, this is "-".
      If a pawn has just made a 2-square move, this is the position "behind" the pawn. This is recorded
      regardless of whether there is a pawn in position to make an en passant capture.

   5) Halfmove clock: This is the number of halfmoves since the last pawn advance or capture. This is used
      to determine if a draw can be claimed under the fifty-move rule.

   6) Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
*/

  clear();
  osl::usi::parse(string("sfen ")+fen, *osl_state);
  std::istringstream ss(fen);
  int fmn;

  // 6. Fullmove number
  if (ss >> fmn)
    startPosPlyCounter = (fmn - 1) * 2 + int(side_to_move() == BLACK);


  st->key = compute_key();
#if 0
  if(eval!=NULL) *eval=eval_t(*osl_state,false);
#else
  if(eval!=NULL) *eval=eval_t(*osl_state);
#endif
  return;

}



/// Position::to_fen() returns a FEN representation of the position. In case
/// of Chess960 the Shredder-FEN notation is used. Mainly a debugging function.

const string Position::to_fen() const {

  string fen;
  Square sq=Square(0);
  char emptyCnt = '0';

  for (Rank rank = RANK_1; rank <= RANK_9; rank++, fen += '/')
  {
      for (File file = FILE_9; file >= FILE_1; file--)
      {
          sq = make_square(file, rank);

          if (square_is_occupied(sq))
          {
              if (emptyCnt != '0')
              {
                  fen += emptyCnt;
                  emptyCnt = '0';
              }
              fen += pieceLetters.from_piece(piece_on(sq));
          } else
              emptyCnt++;
      }

      if (emptyCnt != '0')
      {
          fen += emptyCnt;
          emptyCnt = '0';
      }
  }

  fen += (side_to_move() == WHITE ? " w " : " b ");

  return fen;
}


/// Position::print() prints an ASCII representation of the position to
/// the standard output. If a move is given then also the san is printed.

void Position::print(Move move) const {

  const char* dottedLine = "\n+---+---+---+---+---+---+---+---+\n";

  if (isValid(move))
  {
      Position p(*this, thread());
      string dd = (color_of_piece_on(move_from(move)) == BLACK ? ".." : "");
      cout << "\nMove is: " << dd << move_to_san(p, move);
  }
  cout << *osl_state << endl;
  cout << dottedLine << "Fen is: " << to_fen() << "\nKey is: " << st->key << endl;
}


/// Position::pl_move_is_legal() tests whether a pseudo-legal move is legal

bool Position::pl_move_is_legal(Move m) const {
  if(!osl_state->isAlmostValidMove(m,false)) return false;
  if(isDrop(m)) return true;
  if(side_to_move()==BLACK)
    return osl::SafeMove<BLACK>::isMember(*osl_state,ptype(m),from(m),to(m));
  else
    return osl::SafeMove<WHITE>::isMember(*osl_state,ptype(m),from(m),to(m));
}


/// Position::pl_move_is_evasion() tests whether a pseudo-legal move is a legal evasion

bool Position::pl_move_is_evasion(Move /* m */) const
{
  return true;
}
/// Position::move_is_legal() takes a position and a (not necessarily pseudo-legal)
/// move and tests whether the move is legal. This version is not very fast and
/// should be used only in non time-critical paths.

bool Position::move_is_legal(const Move m) const {
  return isNormal(m) && pl_move_is_legal(m);
}


/// Fast version of Position::move_is_legal() that takes a position a move and
/// a bitboard of pinned pieces as input, and tests whether the move is legal.



/// Position::move_gives_check() tests whether a pseudo-legal move is a check

bool Position::move_gives_check(Move m) const {
  if(side_to_move()==BLACK)
    return osl::Check<BLACK>::isMember(*osl_state,ptype(m),from(m),to(m));
  else 
    return osl::Check<WHITE>::isMember(*osl_state,ptype(m),from(m),to(m));
}


/// Position::do_setup_move() makes a permanent move on the board. It should
/// be used when setting up a position on board. You can't undo the move.

void Position::do_setup_move(Move m) {

  StateInfo newSt;

  do_move(m, newSt);
  if(eval)
#if 0
    *eval=eval_t(*osl_state,false);
#else
    *eval=eval_t(*osl_state);
#endif

  // Reset "game ply" in case we made a non-reversible move.
  // "game ply" is used for repetition detection.
  if(st->gamePly>16){
    for(int i=0;i<16;i++){
      history[i]=history[i+st->gamePly-16];
    }
    st->gamePly=16;
  }

  // Update the number of plies played from the starting position
  startPosPlyCounter++;

  // Our StateInfo newSt is about going out of scope so copy
  // its content before it disappears.
  detach();
}


/// Position::do_move() makes a move, and saves all information necessary
/// to a StateInfo object. The move is assumed to be legal. Pseudo-legal
/// moves should be filtered out before this function is called.

void Position::do_move(Move m, StateInfo& newSt) {

  assert(is_ok());
  assert(!isPass(m));
  nodes++;
  Key key = st->key;
  struct ReducedStateInfo {
    int gamePly, pliesFromNull;
    Key key;
  };
  memcpy(&newSt, st, sizeof(ReducedStateInfo));

  newSt.previous = st;
  st = &newSt;
  history[st->gamePly++] = key;

  // Update side to move
  key ^= zobSideToMove;

  st->pliesFromNull++;

  Player us = side_to_move();
  Player them = opposite_color(us);
  Square from = move_from(m);
  Square to = move_to(m);

  PieceType pt=ptype(m);
  osl::Ptype capture = capturePtype(m);
  st->capturedType = capture;
  if(capture!=osl::Ptype::EMPTY){
    key -= zobrist[I(them)][I(capture)][I(to)];
    key += zobrist[I(us)][I(unpromote(capture))][I(Square::STAND)];
  }
  // Update hash key
  if(move_is_promotion(m))
    key += zobrist[I(us)][I(pt)][I(to)]-zobrist[I(us)][I(unpromote(pt))][I(from)];
  else
    key += zobrist[I(us)][I(pt)][I(to)]-zobrist[I(us)][I(pt)][I(from)];

  prefetch((char*)TT.first_entry(key));
  st->key = key;
  osl_state->makeMove(m);
  if(osl_state->inCheck()) continuous_check[I(us)]++;
  else continuous_check[I(us)]=0;
  assert(is_ok());
}

/// Position::see() is a static exchange evaluator: It tries to estimate the
/// material gain or loss resulting from a move. There are three versions of
/// this function: One which takes a destination square as input, one takes a
/// move, and one which takes a 'from' and a 'to' square. The function does
/// not yet understand promotions captures.

int Position::see(Move m) const {

  assert(move_is_ok(m));
  return osl_state->see(m);
}

int Position::see_sign(Move m) const {

  assert(move_is_ok(m));

  Square from = move_from(m);
  Square to = move_to(m);

  // Early return if SEE cannot be negative because captured piece value
  // is not less then capturing one. Note that king moves always return
  // here because king midgame value is set to 0.
  if (midgame_value_of_piece_on(to) >= midgame_value_of_piece_on(from))
      return 1;
  return osl_state->see(m);
}



/// Position::clear() erases the position object to a pristine state, with an
/// empty board, white to move, and no castling rights.

void Position::clear() {

  st = &startState;
  memset(st, 0, sizeof(StateInfo));
  startPosPlyCounter = 0;
  nodes = 0;


  osl_state = &osl_start_state;
  osl_state->init();
  osl_state->setTurn(BLACK);
  continuous_check[I(BLACK)]=continuous_check[I(WHITE)]=0;
}


/// Position::put_piece() puts a piece on the given square of the board,
/// updating the board array, pieces list, bitboards, and piece counts.



/// Position::compute_key() computes the hash key of the position. The hash
/// key is usually updated incrementally as moves are made and unmade, the
/// compute_key() function is only used when a new position is set up, and
/// to verify the correctness of the hash key when running in debug mode.

Key Position::compute_key() const {

  Key result = 0;
  for(int num=0;num<osl::Piece_SIZE;num++){
    osl::Piece p=osl_state->pieceOf(num);
    if(test(osl_state->usedMask(), num))
      result += zobrist[I(owner(p))][I(ptype(p))][I(square(p))];
  }

  if (side_to_move() == BLACK)
      result ^= zobSideToMove;

  return result;
}


/// Position::compute_pawn_key() computes the hash key of the position. The
/// hash key is usually updated incrementally as moves are made and unmade,
/// the compute_pawn_key() function is only used when a new position is set
/// up, and to verify the correctness of the pawn hash key when running in
/// debug mode.



/// Position::compute_value() compute the incremental scores for the middle
/// game and the endgame. These functions are used to initialize the incremental
/// scores when a new position is set up, and to verify that the scores are correctly
/// updated by do_move and undo_move when the program is running in debug mode.
/// Position::compute_non_pawn_material() computes the total non-pawn middle
/// game material value for the given side. Material values are updated
/// incrementally during the search, this function is only used while
/// initializing a new Position object.


/// Position::is_draw() tests whether the position is drawn by material,
/// repetition, or the 50 moves rule. It does not detect stalemates, this
/// must be done by the search.

bool Position::is_draw(int& ret) const {
  ret=0;
  for (int i = 4, e = Min(st->gamePly,st->pliesFromNull); i <= e; i += 2)
    if (history[st->gamePly - i] == st->key){
      Player us = side_to_move();
      Player them = opposite_color(us);
      if(continuous_check[I(us)]*2>=i) {ret= -1; return false;}
      else if(continuous_check[I(them)]*2>=i) {ret= 1; return false;}
      else return true;
    }
  return false;
}


/// Position::is_mate() returns true or false depending on whether the
/// side to move is checkmated.

bool Position::is_mate() const {

  MoveStack moves[MAX_MOVES];
  return in_check() && generate<MV_LEGAL>(*this, moves) == moves;
}


/// Position::init_zobrist() is a static member function which initializes at
/// startup the various arrays used to compute hash keys.

void Position::init_zobrist() {

  int i,j, k;
  RKISS rk;

  for (i = 0; i < 2; i++) for (j = 0; j < osl::PTYPE_SIZE; j++) for (k = 0; k < osl::Square_SIZE; k++)
      zobrist[i][j][k] = rk.rand<Key>() & ~1;

  zobSideToMove = 1;
  zobExclusion  = rk.rand<Key>() & ~1;
}


/// Position::init_piece_square_tables() initializes the piece square tables.
/// This is a two-step operation: First, the white halves of the tables are
/// copied from the MgPST[][] and EgPST[][] arrays. Second, the black halves
/// of the tables are initialized by mirroring and changing the sign of the
/// corresponding white scores.



/// Position::flip() flips position with the white and black sides reversed. This
/// is only useful for debugging especially for finding evaluation symmetry bugs.


/// Position::is_ok() performs some consitency checks for the position object.
/// This is meant to be helpful when debugging.

bool Position::is_ok(int* /* failedStep */) const {

  return true;
}

bool Position::eval_is_ok() const {
  if(!is_ok()) return false;
  if(!eval) return true;
#if 0
  int ret1=eval_t(*osl_state,false).value();
#else
  int ret1=eval_t(*osl_state).value();
#endif
  int ret2=eval->value();
#if 1
  if(ret1 != ret2) {
#if 0
    std::cerr << "eval_t = "; eval_t(*osl_state,false).show(std::cerr); 
#else
    std::cerr << "eval_t = "; eval_t(*osl_state).show(std::cerr); 
#endif
    std::cerr << "\neval = "; (*eval).show(std::cerr);
  }
#endif
  return ret1==ret2;
}

