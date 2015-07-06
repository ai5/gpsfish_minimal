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

#include "osl_types.h"
#include "osl_movegen.h"

#include "movegen.h"
using osl::MoveMask;


/// generate<MV_CAPTURE> generates all pseudo-legal captures and queen
/// promotions. Returns a pointer to the end of the move list.
///
/// generate<MV_NON_CAPTURE> generates all pseudo-legal non-captures and
/// underpromotions. Returns a pointer to the end of the move list.
///
/// generate<MV_NON_EVASION> generates all pseudo-legal captures and
/// non-captures. Returns a pointer to the end of the move list.

struct Store{
  MoveStack* mlist;
  Store(MoveStack* mlist_) :mlist(mlist_){}
  void operator()(Move m, MoveMask /* mt */= MoveMask::ZERO){ (*mlist++).move = m; }
};
struct NoCaptureStore{
  MoveStack* mlist;
  NoCaptureStore(MoveStack* mlist_) :mlist(mlist_){}
  void operator()(Move m, MoveMask mt = MoveMask::ZERO){
    if(isDrop(mt) ||
       (!isPromotion(mt) && !isCapture(mt) && (noCapture(mt) || !isCapture(m))))
      (*mlist++).move = m;
  }
};
template<osl::Player P,MoveType Type>
MoveStack* generateC(const Position& pos, MoveStack* mlist) {
  constexpr osl::Player altP=osl::alt(P);
  if(Type==MV_CAPTURE){
    Store store(mlist);
    for(osl::Piece p : (pos.osl_state)->allPiece((pos.osl_state)->attackedMask(altP))){
      osl::Capture::generate(P, *(pos.osl_state),square(p),store);
    }
    osl::Promote::generate(P, *(pos.osl_state),store);
    return store.mlist;
  }
  else if(Type==MV_NON_CAPTURE){
    Store store(mlist);
    osl::AllMoves::generate<true>(P, *(pos.osl_state),store);
    return store.mlist;
  }
  else if(Type==MV_NON_EVASION){
    Store store(mlist);
    osl::AllMoves::generate<false>(P, *(pos.osl_state),store);
    return store.mlist;
  }
  else if(Type==MV_NON_CAPTURE_CHECK){
    NoCaptureStore store(mlist);
    osl::AddEffectToKing::generate<P>(*(pos.osl_state),pos.king_square(altP),store);
    return store.mlist;
  }
  else{
    assert(Type==MV_EVASION);
    Store store(mlist);
    osl::EscapeKing::generate(P, *(pos.osl_state),store);
    return store.mlist;
  }
}

template<MoveType Type>
MoveStack* generate(const Position& pos, MoveStack* mlist) {
  if(pos.side_to_move()==BLACK)
    return generateC<BLACK,Type>(pos,mlist);
  else
    return generateC<WHITE,Type>(pos,mlist);
}
// Explicit template instantiations
template MoveStack* generate<MV_CAPTURE>(const Position& pos, MoveStack* mlist);
template MoveStack* generate<MV_NON_CAPTURE>(const Position& pos, MoveStack* mlist);
template MoveStack* generate<MV_NON_EVASION>(const Position& pos, MoveStack* mlist);


/// generate_non_capture_checks() generates all pseudo-legal non-captures and knight
/// underpromotions that give check. Returns a pointer to the end of the move list.
template MoveStack* generate<MV_NON_CAPTURE_CHECK>(const Position& pos, MoveStack* mlist);
template MoveStack* generate<MV_EVASION>(const Position& pos, MoveStack* mlist);


/// generate<MV_LEGAL / MV_PSEUDO_LEGAL> computes a complete list of legal
/// or pseudo-legal moves in the current position.
template<>
MoveStack* generate<MV_PSEUDO_LEGAL>(const Position& pos, MoveStack* mlist) {

  assert(pos.is_ok());

  return pos.in_check() ? generate<MV_EVASION>(pos, mlist)
                        : generate<MV_NON_EVASION>(pos, mlist);
}

template<>
MoveStack* generate<MV_LEGAL>(const Position& pos, MoveStack* mlist) {

  return generate<MV_PSEUDO_LEGAL>(pos, mlist);
}
