#ifndef OSL_CHECKMATE_H
#define OSL_CHECKMATE_H

#include "osl_types.h"
#include "osl_position.h"
#include "osl_move.h"
#include "osl_movegen.h"

/* immediateCheckmateTable.h
 */

namespace osl
{
  class ImmediateCheckmateTable
  {
  private:
    std::array<uint8_t, 0x10000u> dropPtypeMasks;
    std::array<std::array<unsigned char,PTYPE_SIZE>,0x100u> ptypeDropMasks;
    std::array<std::array<unsigned char,8>,PTYPE_SIZE> blockingMasks;
    std::array<std::array<unsigned short,8>,PTYPE_SIZE> noEffectMasks;
  public:
    ImmediateCheckmateTable();
    unsigned char dropPtypeMaskOf(unsigned int liberty_drop_mask) const
    {
      return dropPtypeMasks[int(liberty_drop_mask)];
    }
    unsigned char dropPtypeMask(King8Info canMoveMask) const
    {
      return dropPtypeMaskOf(int(libertyDropMask(canMoveMask)));
    }
    unsigned int ptypeDropMask(Ptype ptype,King8Info canMoveMask) const
    {
      return ptypeDropMasks[int(liberty(canMoveMask))][I(ptype)];
    }
    unsigned int blockingMask(Ptype ptype,Direction dir) const
    {
      assert(static_cast<int>(dir)<8);
      return blockingMasks[I(ptype)][I(dir)];
    }
    unsigned int noEffectMask(Ptype ptype,Direction dir) const
    {
      assert(static_cast<int>(dir)<8);
      return noEffectMasks[I(ptype)][I(dir)];
    }
  };
  extern const ImmediateCheckmateTable Immediate_Checkmate_Table;

/* immediateCheckmate.h
 */

  class ImmediateCheckmate
  {
  private:
    template<Player P,bool setBestMove>
    static bool hasCheckmateDrop(NumEffectState const& state,Square target,
				 King8Info mask,Move& bestMove);

  public:
    template<Player P,bool setBestMove>
    static bool slowHasCheckmateMoveDirPiece(NumEffectState const& state,Square target,
					     King8Info mask,Direction d,Square pos,Piece p,Ptype ptype,Move& bestMove);

    template<Player P,bool setBestMove>
    static bool hasCheckmateMoveDirPiece(NumEffectState const& state,Square target,
					 King8Info mask,Direction d,Square pos,Piece p,Move& bestMove);

    template<Player P,bool setBestMove>
    static bool hasCheckmateMoveDir(NumEffectState const& state,Square target,
				    King8Info mask,Direction d,Move& bestMove);

    template<Player P,bool setBestMove>
    static bool hasCheckmateMove(NumEffectState const& state,Square target,
				 King8Info mask,Move& bestMove);

    /**
     * 一手詰めがある局面かどうか判定(move).
     * 手番の側に王手がかかっている場合は除く
     * 長い利きによる王手は生成しない．
     * pinされている駒の利きがないために詰みになる例も扱わない．
     * @param P(template) - 攻撃側(手番側)のプレイヤー
     * @param state - 局面
     */
    template<Player P>
    static bool hasCheckmateMove(NumEffectState const& state);
    template<Player P>
    static bool hasCheckmateMove(NumEffectState const& state, King8Info);

    /**
     * 一手詰めがある局面かどうか判定(move).
     * 手番の側に王手がかかっている場合は除く
     * 長い利きによる王手は生成しない．
     * pinされている駒の利きがないために詰みになる例も扱わない．
     * @param P(template) - 攻撃側(手番側)のプレイヤー
     * @param state - 局面
     * @param best_move - ある場合に詰めの手を返す
     */
    template<Player P>
    static bool hasCheckmateMove(NumEffectState const& state,Move &bestMove);
    template<Player P>
    static bool hasCheckmateMove(NumEffectState const& state, 
				 King8Info canMoveMask,
				 Square king, Move& bestMove);
    /**
     *
     */
    static bool hasCheckmateMove(Player pl,NumEffectState const& state);
    static bool hasCheckmateMove(Player pl,NumEffectState const& state,Move& bestMove);

  };

  namespace {
//      using osl::misc::BitOp;
    template<Player P>
    bool blockingVerticalAttack(NumEffectState const& state,Square pos)
    {
      PieceMask mask=state.longEffect(P, pos) & state.longEffect(P, pos + newOffset(P, U));
      if(!any(mask & pieceMask(LANCE))){
	for(Square from : state.allSquare(mask & pieceMask(ROOK))){
	  assert(isOnBoard(from));
	  if(isU(P,from,pos)) goto found;
	}
	return false;
      found:;
      }
      const Offset offset=newOffset(P,U);
      pos+=offset;
      constexpr Player altP=alt(P);
      for(int i=0;i<3;i++,pos+=offset){
	Piece p=state.pieceAt(pos);
	if(canMoveOn(altP,p)){ // 自分の駒か空白
	  if(state.countEffect(P,pos)==1) return true;
	  if(!isEmpty(p)) return false;
	}
	else return false;
      }
      return false;
    }
    template<Player P>
    bool
#ifdef __GNUC__
    __attribute__ ((pure))
#endif
    blockingDiagonalAttack(NumEffectState const& state,Square pos,Square target,
			   King8Info canMoveMask)
    {
      constexpr Player altP=alt(P);
      Square to=target-newOffset(P,U);
      // Uに相手の駒がある
      if((V(canMoveMask)&(0x10000<<U))==0) return false;
      PieceMask mask=state.longEffect(P, to) & state.longEffect(P, pos);
      for(Square from : state.allSquare(mask & pieceMask(BISHOP))){
	assert(isOnBoard(from));
	Offset offset=Board_Table.getShort8OffsetUnsafe(to,from);
	if(to+offset != pos) continue;
	if(state.countEffect(P,to)==1) return true;
	// Uがspaceだと絡んでくる
	if(!isEmpty(state.pieceAt(to))) return false;
	Square pos1=to-offset;
	// BISHOPの利き一つで止めていた
	Piece p=state.pieceAt(pos1);
	if(canMoveOn(altP,p) &&
	   state.countEffect(P,pos1)==1){
	  return true;
	}
      }
      return false;
    }
    template<Player P,bool canDrop,bool setBestMove>
    bool hasKnightCheckmate(NumEffectState const& state, 
			    Square target, 
			    Square pos,
			    King8Info canMoveMask,
			    Move& bestMove, 
			    PieceMask mask1
      )
    {
      if(!isOnBoard(pos)) return false;
      constexpr Player altP=alt(P);
      Piece p=state.pieceAt(pos);
      if(canMoveOn(P,p) && 
	 !state.hasEffectByNotPinned(altP,pos)
	){
	PieceMask mask = state.effect(pos) & mask1;
	if(any(mask)){
	  if(blockingVerticalAttack<P>(state,pos) ||
	     blockingDiagonalAttack<P>(state,pos,target,canMoveMask)) return false;
	  if(setBestMove){
	    int num=bsf(mask);
	    Piece p1=state.pieceOf(num);
	    Square from=square(p1);
	    bestMove=newMove(from,pos,KNIGHT,osl::ptype(p),false,P);
	  }
	  return true;
	}
	else if(canDrop && isEmpty(p)){
	  if(blockingVerticalAttack<P>(state,pos) ||
	     blockingDiagonalAttack<P>(state,pos,target,canMoveMask)) return false;
	  if(setBestMove)
	    bestMove=newMove(pos,KNIGHT,P);
	  return true;
	}
      }
      return false;
    }
    // KNIGHT
    // KNIGHTのdropは利きを遮ることはない
    template<Player P,bool setBestMove>
    bool hasCheckmateMoveKnight(NumEffectState const& state, Square target, 
				King8Info canMoveMask,Move& bestMove)
    {
      // 8近傍に移動できる時は桂馬の一手詰めはない
      if((V(canMoveMask)&0xff00)!=0) return false;
      PieceMask mask = state.piecesOnBoardStrict(P, KNIGHT) & 
	~state.pinOrOpen(P);
      if(state.hasPieceOnStand(P, KNIGHT)){
	Square pos=target-newOffset(P,UUR);
	if(hasKnightCheckmate<P,true,setBestMove>(state,target,pos,canMoveMask,bestMove,mask))
	  return true;
	pos=target-newOffset(P,UUL);
	return hasKnightCheckmate<P,true,setBestMove>(state,target,pos,canMoveMask,bestMove,mask);
      }
      else{
	Square pos=target-newOffset(P,UUR);
	if(hasKnightCheckmate<P,false,setBestMove>(state,target,pos,canMoveMask,bestMove,mask))
	  return true;
	pos=target-newOffset(P,UUL);
	return hasKnightCheckmate<P,false,setBestMove>(state,target,pos,canMoveMask,bestMove,mask);
      }
      return false;
    }
    template<Player P,bool setBestMove>
    bool slowCheckDrop(NumEffectState const& state,Square target,
		       Ptype ptype,King8Info canMoveMask,Move& bestMove)
    {
      unsigned int dropMask=(V(canMoveMask) & 0xff)
	&Immediate_Checkmate_Table.ptypeDropMask(ptype,canMoveMask);
      // dropMaskが0ならここに来ない
      assert(dropMask!=0);
      while(dropMask!=0){
	int i=takeOneBit(dropMask);
	Direction d=static_cast<Direction>(i);
	unsigned int blockingMask=Immediate_Checkmate_Table.blockingMask(ptype,d) &
	  (V(canMoveMask)>>16);
	Square drop=target-newOffset(P,d);
	if(blockingMask!=0){
	  PieceMask longEffect=state.longEffect(P, drop);
	  if(any(longEffect)){
	    do{
	      int j=osl::takeOneBit(blockingMask);
	      Direction d1=static_cast<Direction>(j);
	      Square pos=target-newOffset(P,d1);
	      if(state.countEffect(P, pos) > 1) continue;
	      PieceMask longEffect1 = state.effect(pos) & longEffect;
	      if(!any(longEffect1)) continue;
	      int num=bsf(longEffect1);
	      if(Board_Table.isBetween(drop,square(state.pieceOf(num)),pos))
		goto tryNext;
	    }while(blockingMask!=0);
	  }
	}
	// blockingMaskの点がすべてOKならOK
	if(setBestMove)
	  bestMove=newMove(drop,ptype,P);
	return true;
      tryNext:;
      }
      return false;
    }
  } // detail

/* fixedDepthSearcher.h
 */

  /**
   * 深さ固定で，その深さまで depth first searchで読む詰将棋.
   * 深さ0で詰み状態かどうか(攻め手の手番の場合)，王手をかける手がないかを判定可能
   * 深さ1で通常の一手詰みを判定(攻め手の手番の場合)
   * 使うのは深さ3位まで?
   * NumEffectState専用
   * 1 - checkmate 
   * 0 - timeout
   * -1 - nomate
   */
  class FixedDepthSearcher
  {
  private:
    /*
     * if P has an check move, attack returns true and set best_move.
     * otherwise returns false.
     */
    template<Player P>
    static bool attack(int depth, NumEffectState const& state, Move& best_move){
      if(depth <= 0){
	return (! state.inCheck() &&
		ImmediateCheckmate::hasCheckmateMove(state.turn(), state, best_move));
      }
      bool dummy;
      MoveVector moves;
      auto helper = [&](Move m, MoveMask /* mt */){ moves.push_back(m); };
      AddEffectToKing::generate<P>(state, state.kingSquare(alt(P)),helper,dummy);
      for(Move m : moves){
	  NumEffectState newstate(state);
	  newstate.previous = &state;
	  newstate.makeMove(m);
	  if(defense<alt(P)>(depth - 1, newstate, m)) {
	    best_move = m;
	    return true;
	  }
      }
      return false;
    }
    template<Player P>
    static int defense(int depth, NumEffectState const& state, Move last_move){
      if (depth <= 0) return 0;
      MoveVector moves;
      auto helper = [&](Move m, MoveMask /* mt */){ moves.push_back(m); };
      EscapeKing::generate(P, state, helper);
      if(moves.size() == 0)
	return (isDrop(last_move) && oldPtype(last_move) == PAWN ? -1 : 1);
      for(Move m : moves){
	NumEffectState newstate(state);
	newstate.previous = &state;
	newstate.makeMove(m);
	Move dummy;
	if(!attack<alt(P)>(depth - 1, newstate, dummy)) return false;
      }
      return true;
    }
  public:
    static bool hasCheckmate(NumEffectState const& state, Player P, int depth, Move& best_move){
      return (P == BLACK ? attack<BLACK>(depth, state, best_move) 
	      : attack<WHITE>(depth, state, best_move));
    }
  };
} // osl

// not KNIGHT
template<osl::Player P,bool setBestMove>
bool osl::ImmediateCheckmate::
hasCheckmateDrop(NumEffectState const& state, Square target,
		 King8Info canMoveMask,Move& bestMove)
{
  uint32_t dropPtypeMask=uint32_t(Immediate_Checkmate_Table.dropPtypeMask(canMoveMask));
  while(dropPtypeMask){
    Ptype ptype=static_cast<Ptype>(Ptype::BASIC_MIN+takeOneBit(dropPtypeMask));
    if(state.hasPieceOnStand(P,ptype) &&
       slowCheckDrop<P,setBestMove>(state,target,ptype,canMoveMask,
				    bestMove))
      return true;
  }
  return false;
}

template<osl::Player P,bool setBestMove>
bool osl::ImmediateCheckmate::
slowHasCheckmateMoveDirPiece(NumEffectState const& state, Square target,
			     King8Info canMoveMask,Direction d,Square pos,Piece p,Ptype ptype,Move& bestMove){
  constexpr Player altP=alt(P);
  // ptypeがPROOKの時は，更なるチェックが必要
  if(ptype==PROOK){
    int dx=X(target)-X(pos);
    int dy=Y(target)-Y(pos);
    if(abs(dx)==1 && abs(dy)==1){
      {
	Square pos1=pos+newOffset(dx,0);
	Piece p1=state.pieceAt(pos1);
	if(!isEmpty(p1)){
	  {
	    //  * -OU *
	    // (A)(B)(D)
	    //  * (C) *
	    // (E) *  *
	    // +RY (C) -> (A), (E) -> (A)
	    // -?? - (B)
	    // (D) - 竜以外の利きなし 
	    Square pos2=pos+newOffset(2*dx,0);
	    if(canMoveOn(altP,state.pieceAt(pos2))){
	      if(!state.hasEffectNotBy(P, p, pos2)) return false;
	    }
	  }
	  {
	    //  * -OU *
	    // (A)(B) *
	    //  * (C) *
	    // +RY (C) -> (A)
	    // -?? - (B)竜でpinされているが実はAへの利き持つ
	    if(square(p)==target-newOffset(0,2*dy) &&
	       state.hasEffectByPiece(p1,pos))
	      return false;
	  }
	}
      }
      {
	Square pos1=pos+newOffset(0,dy);
	Piece p1=state.pieceAt(pos1);
	if(!isEmpty(p1)){
	  Square pos2=pos+newOffset(0,2*dy);
	  {
	    if(canMoveOn(altP,state.pieceAt(pos2))){
	      if(!state.hasEffectNotBy(P, p, pos2)) return false;
	    }
	    {
	      // (C)(B)-OU
	      //  * (A) *
	      // +RY (C) -> (A)
	      // -?? - (B)竜でpinされているが実はAへの利き持つ
	      if(square(p)==target-newOffset(2*dx,0) &&
		 state.hasEffectByPiece(p1,pos))
		return false;
	    }
	  }
	}
      }
    }
  }
  // 元々2つの利きがあったマスが，
  // block & 自分の利きの除去で利きがなくなることはあるか?
  // -> ある．
  // +KA  *   *
  //  *  (A) +KI
  //  *  -OU (B)
  //  *   *   *
  // で金がAに移動して王手をかけると，Bの利きが2から0になる．
  uint32_t mask = (V(canMoveMask) >> 16) & Immediate_Checkmate_Table.noEffectMask(ptype, d);
  if(mask != 0){
    int num=number(p);
#if 0
    PieceMask longEffect2=state.longEffect(P, pos);
    reset(longEffect2, num);
#else
    PieceMask longEffect2=state.longEffect(P, pos) & ~newPieceMask(num);
#endif
    do {
      Direction d1=Direction(takeOneBit(mask));
      Square pos1=target-newOffset(P,d1);
      int count=state.countEffect(P, pos1);
      PieceMask effect1 = state.effect(pos1);
      // 自分の利きの除去
      if(test(effect1, num)) count--;
      if(count==0) return false;
      // blockしている利きの除去
      for(Square sq : state.allSquare(effect1 & longEffect2)){
	if(Board_Table.isBetween(pos,sq,pos1))
	  count--;
	if(count==0) return false;
      }
    } 
    while (mask != 0);
  }
  // 自殺手でないことのチェックを入れる
  if(KingOpenMove<P>::isMember(state,ptype,square(p),pos)) return false;
  if(setBestMove){
    bestMove=newMove(square(p),pos,ptype,
		     osl::ptype(state.pieceAt(pos)),
		     ptype!=osl::ptype(p),P);
  }
  return true;
}

template<osl::Player P,bool setBestMove>
bool osl::ImmediateCheckmate::
hasCheckmateMoveDirPiece(NumEffectState const& state, Square target,
			 King8Info canMoveMask,Direction d,Square pos,Piece p,Move& bestMove){
  Square from=square(p);
  Ptype ptype=osl::ptype(p);
  // 相手の利きが伸びてしまって移動後も利きがついてくる可能性
  {
    constexpr Player altP=alt(P);
    Direction d1=Board_Table.getShort8Unsafe(P, from, pos);
    if(d1!=Direction::INVALID_VALUE){ // not knight move
      int num=state.longEffectNumTable()[number(p)][P==BLACK ? d1 : rotate180Short(d1)];
      if(num != EMPTY_NUM && isOnBoardByOwner(altP,state.pieceOf(num)))
	return false;
    }
  }
  if(canPromote(ptype) &&
     (canPromote(P,from) || canPromote(P,pos))){
    Ptype pptype=promote(ptype);
    if((((V(canMoveMask)>>8)|0x100)&
	Immediate_Checkmate_Table.noEffectMask(pptype,d))==0){
      if(slowHasCheckmateMoveDirPiece<P,setBestMove>(state,target,canMoveMask,d,pos,p,pptype,bestMove)) return true;
    }
    if (ptype==PAWN || /*basic because canpromote*/isMajorBasic(ptype)) 
      return false;
  }
  if((((V(canMoveMask)>>8)|0x100)&
      Immediate_Checkmate_Table.noEffectMask(ptype,d))==0){
    if(slowHasCheckmateMoveDirPiece<P,setBestMove>(state,target,canMoveMask,d,pos,p,ptype,bestMove)) return true;
  }
  return false;
}

template<osl::Player P,bool setBestMove>
bool osl::ImmediateCheckmate::
hasCheckmateMoveDir(NumEffectState const& state, Square target,
		    King8Info canMoveMask,Direction d,Move& bestMove){
  Square pos=target-newOffset(P,d);
  if(state.countEffect(P,pos)<2 &&
     !state.hasAdditionalEffect(P, pos)) return false;
  PieceMask pm=state.effect(P, pos) & ~pieceMask(KING);
  assert(isOnBoard(pos));
  // 玉で王手をかけない
//  pieceMask.reset(kingIndex(P));
  for(Piece p : state.allPiece(pm)){
    if(hasCheckmateMoveDirPiece<P,setBestMove>(state, target, canMoveMask, d, pos, p, bestMove)) return true;
  }
  return false;
}

// not KNIGHT
template<osl::Player P,bool setBestMove>
bool osl::ImmediateCheckmate::
hasCheckmateMove(NumEffectState const& state, Square target,
		 King8Info canMoveMask,Move& bestMove)
{
  assert(! state.inCheck());
  uint32_t mask2=uint32_t((V(canMoveMask)>>24)&0xff);
  while(mask2){
    Direction d=static_cast<Direction>(takeOneBit(mask2));
    if(hasCheckmateMoveDir<P,setBestMove>(state,target,canMoveMask,d,bestMove)) return true;
  }
  return false;
}

template<osl::Player P>
bool osl::ImmediateCheckmate::
hasCheckmateMove(NumEffectState const& state, King8Info canMoveMask)
{
  constexpr Player altP=alt(P);
  const Square target=state.kingSquare(altP);
  assert(isOnBoard(target));
  // 相手からの王手がかかっていない
  Move dummy;
  if(hasCheckmateMove<P,false>(state,target,canMoveMask,dummy)) return true;
  if(hasCheckmateMoveKnight<P,false>(state,target,canMoveMask,dummy)) return true;
  return hasCheckmateDrop<P,false>(state,target,canMoveMask,dummy);
}

template<osl::Player P>
bool osl::ImmediateCheckmate::
hasCheckmateMove(NumEffectState const& state)
{
  constexpr Player altP=alt(P);
#ifndef NDEBUG
  const Square target=state.kingSquare(altP);
#endif
  assert(isOnBoard(target));
  King8Info canMoveMask = state.king8Info(altP);
  return hasCheckmateMove<P>(state, canMoveMask);
}

template<osl::Player P>
bool osl::ImmediateCheckmate::
hasCheckmateMove(NumEffectState const& state, King8Info canMoveMask,
		 Square target, Move& bestMove)
{
  assert(! state.inCheck());
  assert(isOnBoard(target));

  if(hasCheckmateMove<P,true>(state,target,canMoveMask,bestMove)) return true;
  if(hasCheckmateMoveKnight<P,true>(state,target,canMoveMask,bestMove)) return true;
  return  hasCheckmateDrop<P,true>(state,target,canMoveMask,bestMove);
}

template<osl::Player P>
bool osl::ImmediateCheckmate::
hasCheckmateMove(NumEffectState const& state,Move& bestMove)
{
  constexpr Player altP=alt(P);
  const Square target=state.kingSquare(altP);
  King8Info canMoveMask = state.king8Info(altP);
  return hasCheckmateMove<P>(state, canMoveMask, target, bestMove);
}

#endif // OSL_CHECKMATE_H
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:

