#if !defined(POSITION_TCC_INCLUDED)
#define POSITION_TCC_INCLUDED
#include "position.h"
#include "tt.h"
#include "move.h"
template<typename F>
void Position::do_undo_move(Move m, StateInfo& newSt,F const& f){
  assert(is_ok());
  assert(move_is_ok(m));
  assert(&newSt != st);
  assert(move_is_legal(m) || (std::cerr << m << "\n" << *osl_state << std::endl,0));

  nodes++;
  Key key = st->key;
  struct ReducedStateInfo {
    int gamePly, pliesFromNull;
    Key key;
  };
  memcpy(&newSt, st, sizeof(ReducedStateInfo));

  newSt.previous = st;
  st = &newSt;

#if 1
  osl::NumEffectState new_osl_state(*osl_state);
  new_osl_state.previous = osl_state;
  osl_state = &new_osl_state;    
#endif
  // Save the current key to the history[] array, in order to be able to
  // detect repetition draws.
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
  int old_cont=continuous_check[I(us)];
  continuous_check[I(us)]=(move_gives_check(m) ? old_cont+1 : 0);
#if 0
  osl_state->makeUnmakeMove(m,f);
#else
  osl_state->makeMove(m);
  f(to);	
#endif
  continuous_check[I(us)]=old_cont;
  st = st->previous;
#if 1
  osl_state = const_cast<osl::NumEffectState*>(osl_state->previous);
#endif
}

template<typename F>
void Position::do_undo_null_move(StateInfo& backupSt, F const& f){
  assert(is_ok());
  backupSt.key      = st->key;
  backupSt.previous = st->previous;
  backupSt.pliesFromNull = st->pliesFromNull;
  st->previous = &backupSt;
#if 0
  osl::NumEffectState new_osl_state(*osl_state);
  new_osl_state.previous = osl_state;
  osl_state = &new_osl_state;    
#endif
  history[st->gamePly++] = st->key;
  st->key ^= zobSideToMove;
  prefetch((char*)TT.first_entry(st->key));
  st->pliesFromNull = 0;
  Player us = side_to_move();
  int old_cont=continuous_check[I(us)];
  continuous_check[I(us)]=0;
#if 0
  osl_state->makeUnmakePass(f);
#else
  osl_state->makeMovePass();
  f(Square::STAND);	
#endif
  continuous_check[I(us)]=old_cont;
  st->key      = backupSt.key;
  st->previous = backupSt.previous;
#if 0
  osl_state = osl_state->previous;
#else
  osl_state->setTurn(us);
#endif
  st->pliesFromNull = backupSt.pliesFromNull;

  // Update the necessary information
  st->gamePly--;
}
#endif // !defined(POSITION_TCC_INCLUDED)
