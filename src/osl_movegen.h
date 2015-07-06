#ifndef OSL_MOVEGEN_H
#define OSL_MOVEGEN_H
#include "osl_types.h"
#include "osl_position.h"
#include "osl_move.h"
#include "osl_position.h"

namespace osl
{
  class PieceOnBoard
  {
    template <bool useDirMask,bool noCapturePromotion,class Action>
    static inline void generateLance(Player P, const NumEffectState& state, Piece p,Square from,int dirMask,Action& action)
    {
      if(!useDirMask || (dirMask&(1<<U))==0){
	const Offset offset=newOffset(P,U);
	Square to=state.mobilityOf((P==BLACK ? U : D),number(p));
	Piece p1=state.pieceAt(to);
	if(!noCapturePromotion && canMoveOn(P, p1)){
	  if(canPromote(P, to))
	    action(newMove(from, to, p1, LANCE, true, P), 
		   MoveMask::PROMOTION | MoveMask::CAPTURE);
	  if(P == BLACK ? Y(to) >= 3 : Y(to) <= 7)
	    action(newMove(from, to, p1, LANCE, false, P), 
		   MoveMask::CAPTURE);
	}
	for(to -= offset; to != from; to -= offset){
	  if(canPromote(P, to)){
	    if(!noCapturePromotion)
	      action(newMove(from, to, LANCE, Ptype::EMPTY, true, P), 
		     MoveMask::PROMOTION | MoveMask::NO_CAPTURE);
	    if(P == BLACK ? Y(to) < 3 : Y(to) > 7) continue;
	  }
	  action(newMove(from, to, LANCE, Ptype::EMPTY, false, P), 
		 MoveMask::NO_CAPTURE);
	}
      }
    }
    template <bool useDirMask,bool noCapturePromotion,class Action>
    static inline void generatePawn(Player P, const NumEffectState& state, Piece
#ifndef NDEBUG
p
#endif
,Square from,int dirMask,Action& action)
    {
      assert(from == square(p));
      if(!useDirMask || (dirMask&(1<<U))==0){
	Square to=from + newOffset(P,U);
	if(noCapturePromotion){
	  if(!canPromote(P, to) && isEmpty(state[to]))
	    action(newMove(from,to,PAWN,Ptype::EMPTY,false,P), MoveMask::NO_CAPTURE);
	  return;
	}
	Piece p1 = state[to];
	if(canMoveOn(P, p1)){
	  if(canPromote(P, to))
	    action(newMove(from,to,p1, PAWN,true,P), MoveMask::PROMOTION);
	  else
	    action(newMove(from,to,p1, PAWN,false,P), MoveMask::ZERO);
	}
      }
    }
    template <bool noCapture, class Action>
    static inline void
    generateKingDir(Player P, NumEffectState const& state, Square from, Move const& moveBase, Direction Dir, Action& action)
    {
      King8Info king8info = state.king8Info(P);
      unsigned int lib = liberty(king8info);
      if((lib&(1<<Dir)) != 0){
	Offset offset=newOffset(P,Dir);
	Move m=newAddTo(moveBase,offset);
	Piece p1 = state[from + offset];
	assert(canMoveOn(P,p1));
	if(noCapture && !isEmpty(p1)) return;
	m=newAddCapture(m,p1);
	action(m, (noCapture ? MoveMask::NO_CAPTURE : MoveMask::ZERO));
      }
    }
  public:
    template<bool useDirMask, bool noCapture, class Action>
    static inline void generateKing(Player P, const NumEffectState& state,Square pos,int dirMask, Action& action)
    {
      Move moveBase=newMove(pos,pos,KING,(Ptype)0,false,P);
      for(Direction Dir : PRIM_DIRECTIONS){
	if(!useDirMask || (dirMask&(1<<Dir))==0){
	  generateKingDir<noCapture>(P, state,pos,moveBase, Dir,action);
	  generateKingDir<noCapture>(P, state,pos,moveBase, rotate180Short(Dir),action);
	}
      }
    }

    template<typename Action>
    static void generatePiecePtype(Player P, Ptype T, const NumEffectState& state,Piece p, Square target, Piece p1,Action& action)
    {
      assert(owner(p) == P);
      if(T==KING){
	assert(!state.hasEffect(alt(P),square(p)));
	if(state.hasEffect(alt(P), target)) return;
      }
      else if(state.isPinOrOpen(P, p) && (T == KNIGHT || !state.pinnedCanMoveTo(P, p, target)))
	return;
      assert(state.hasEffectByPiece(p, target));
      assert(ptype(p)==T);
//	Ptype ptype=ptype(p);
      Square from=square(p);
      if(canPromote(T)){
	if(canPromote(P,target)){
	  action(newMove(from,target,p1, T, true,P), 
		 MoveMask::PROMOTION);
	  if(isBetterToPromote(T)) return; // PAWN, BISHOP, ROOK
	  int y=(P==BLACK ? Y(target) : 10-Y(target));
	  if((M(T) & (M(LANCE) | M(KNIGHT))) != 0  && y != 3) return;
	}
	else if(canPromote(P,from)){
	  action(newMove(from,target,p1,T,true,P), 
		 MoveMask::PROMOTION);
	  if(isBetterToPromote(T)) return;
	}
      }
      action(newMove(from,target,p1,T,false,P), MoveMask::ZERO);
    }
    template<typename Action>
    static void generatePiece(Player P,const NumEffectState& state,Piece p, Square target, Piece p1,Action& action)
    {
      generatePiecePtype(P, ptype(p), state, p, target, p1, action);
    }
    /**
     * ROOK, BISHOP, PROOK, PBISHOPのlong方向の手生成
     * CanPはNoPromoteかCanPromote, CheckPromoteのみ
     * NoPromoteはpromoteできない点からの後ろ，横のdirection
     * CanPromoteはpromoteできる点から
     * CheckPromoteはpromoteできない点からの前向き direction
     */
    template <bool noCapturePromotion, class Action>
    static inline void
    generateLong(Player P,Ptype T,PromoteType CanP,NumEffectState const& state,Piece p,const Piece *ptr, Square from,Move moveBase,Ptype 
#ifndef NDEBUG
ptype
#endif
,Direction Dir,Action& action)
    {
      bool cd=osl::canPromoteTo[I(Dir)];
      if(hasMove(T,Dir)){
	int num=number(p);
	const Direction shortDir=longToShort(Dir);
	Square limit=state.mobilityOf((P==BLACK ? shortDir : rotate180Short(shortDir)),num);
	const Piece *limitPtr=state.getPiecePtr(limit);
	assert(ptype != LANCE);
	const Offset offset=newOffset(P,Dir);
	assert(offset != Offset::ZERO);
	ptr+=int(offset);
	Square to=from+offset;
	Move m=newAddTo(moveBase,offset);
	if(CanP==CanPromoteType || (CanP==MayPromoteType && cd) ){
	  if(CanP==MayPromoteType){
	    // promoteできない数
	    int count=(P==BLACK ? Y(from) - 4 : 6 - Y(from)); 
	    for(int i=0;i<count;i++){
	      if(ptr==limitPtr){
		Piece p1= *limitPtr;
		if(!noCapturePromotion && canMoveOn(P,p1))
		  action(newAddCapture(m,p1), MoveMask::CAPTURE);
		return;
	      }
	      action(m, MoveMask::NO_CAPTURE);
	      ptr+=int(offset);
	      to+=offset; m=newAddTo(m,offset);
	    }
	  }
	  if(noCapturePromotion) return;
	  while(ptr!=limitPtr){
	    assert(canPromote(P,from) || canPromote(P,to));
	    action(promote(m), (MoveMask::PROMOTION | MoveMask::NO_CAPTURE));
	    ptr+=int(offset);
	    to+=offset;
	    m=newAddTo(m,offset);
	  }
	  Piece p1= *limitPtr;
	  if(canMoveOn(P,p1)){
	    m=newAddCapture(m,p1);
	    assert(canPromote(P,from) || canPromote(P,to));
	    action(promote(m), (MoveMask::PROMOTION | MoveMask::CAPTURE));
	  }
	}
	else{ // NoPromote
	  while(ptr!=limitPtr){
	    action(m, MoveMask::NO_CAPTURE);
	    ptr+=int(offset);
	    to+=offset; m=newAddTo(m,offset);
	  }
	  if(noCapturePromotion) return;
	  Piece p1= *limitPtr;
	  if(canMoveOn(P,p1)){
	    m=newAddCapture(m,p1);
	    action(m, MoveMask::CAPTURE);
	  }
	}
      }
    }

    /**
     * 短い利きの動き
     * CanPromoteType - promote可能な動きの時
     * MustPromoteType - 2段目の歩，3,4段目の桂馬
     */
    template <bool noCapturePromotion, class Action>
    static void 
    generateShort(Player P,Ptype T,PromoteType CanP,NumEffectState const& state,Square from,Move moveBase,Ptype /*ptype*/,Direction Dir,Action& action)
    {
      if(hasMove(T, Dir)){
	const Offset offset = newOffset(P, Dir);
	Piece p1 = state[from + offset];	
	if ((noCapturePromotion ? isEmpty(p1) : canMoveOn(P, p1))){
	  Move m=newAddCapture(newAddTo(moveBase,offset),p1);
	  if (!noCapturePromotion && 
	      (CanP==CanPromoteType || CanP==MustPromoteType || 
	       (CanP == MayPromoteType && canPromoteDir(Dir))))
	    action(promote(m), MoveMask::PROMOTION);
	  if (CanP != MustPromoteType)
	    action(m, MoveMask::ZERO);
	}
      }
    }

    template <bool useDirMask,bool noCapturePromotion,class Action>
    static void
    generatePtypePromote(Player P,Ptype T,PromoteType CanP,const NumEffectState& state,Piece p,Square from,int dirMask, Action& action)
    {
      const Ptype ptype=(T==GOLD ? osl::ptype(p) : T);
      Move moveBase=newMove(from,from,ptype,(Ptype)0,false,P);
      const Piece *ptr=state.getPiecePtr(from);
      for(Direction Dir : PRIM_DIRECTIONS1){
	if(!useDirMask || (dirMask&(1<<Dir))==0){
	  generateShort<noCapturePromotion>(P,T,CanP,state,from,moveBase,ptype,Dir,action);
	  generateShort<noCapturePromotion>(P,T,CanP,state,from,moveBase,ptype,rotate180Short(Dir),action);
	  generateLong<noCapturePromotion>(P,T,CanP,state,p,ptr,from,moveBase,ptype,shortToLong(Dir),action);
	  generateLong<noCapturePromotion>(P,T,CanP,state,p,ptr,from,moveBase,ptype,shortToLong(rotate180Short(Dir)),action);
	}
      }
      if(T == KNIGHT){
	generateShort<noCapturePromotion>(P,KNIGHT,CanP,state,from,moveBase,ptype,UUL,action);
	generateShort<noCapturePromotion>(P,KNIGHT,CanP,state,from,moveBase,ptype,UUR,action);
      }
    }

    /**
     * Generate moves without stating the Ptype as template param.
     * pinでないことが判明している時に呼び出す
     * @param T - moveTypeがTの駒
     * @param state - 手を作成する局面，手番はPと一致
     * @param p - 盤面上に存在するPの駒
     * @param action - 手生成用のAction
     */
    template <bool useDirMask,bool noCapturePromotion,typename Action>
    static void generatePtypeUnsafe(Player P,Ptype T,const NumEffectState& state,Piece p,int dirMask, Action& action)
    {
      Square from=square(p);
      if(T==KING){
	PieceOnBoard::generateKing<useDirMask,noCapturePromotion>(P,state,from,dirMask,action);
      }
      else if(T==LANCE){
	generateLance<useDirMask,noCapturePromotion>(P,state,p,from,dirMask,action);
      }
      else if(T==PAWN){
	generatePawn<useDirMask,noCapturePromotion>(P,state,p,from,dirMask,action);
      }
      else if(canPromote(T)){
	generatePtypePromote<useDirMask,noCapturePromotion>(P,T,promoteType(P,T,from),state,p,from,dirMask,action);
      }
      else
	generatePtypePromote<useDirMask,noCapturePromotion>(P,T,NoPromoteType,state,p,from,dirMask,action);
    }

    template <bool noCapturePromotion,typename Action>
    static void generatePtypeUnsafe(Player P,Ptype T,const NumEffectState& state,Piece p, Action& action)
    {
      int dummy=0;
      generatePtypeUnsafe<false,noCapturePromotion>(P,T,state,p,dummy,action);
    }
    /**
     * Generate moves without stating the Ptype as template param.
     * pinの場合はそれに応じた手を生成する
     * @param T - moveTypeがTの駒
     * @param state - 手を作成する局面，手番はPと一致
     * @param p - 盤面上に存在するPの駒
     * @param action - 手生成用のAction
     */
    template <bool useDirMask,bool noCapturePromotion, typename Action>
    static void generatePtype(Player P,Ptype T, const NumEffectState& state,Piece p,int dirMask, Action& action){
      int num=number(p);
      assert(P == owner(state.pieceOf(num)));
      if(state.isPinOrOpen(P, num)){
	if(T==KNIGHT) return;
	Direction d=state.pinnedDir(P, p);
	dirMask|=(~(1<<primDir(d)));
	PieceOnBoard::generatePtypeUnsafe<true,noCapturePromotion>(P,T,state,p,dirMask,action);
      }
      else{
	PieceOnBoard::generatePtypeUnsafe<useDirMask,noCapturePromotion>(P,T,state,p,dirMask,action);
      }
    }

    template <bool noCapturePromotion, typename Action>
    static void generatePtype(Player P,Ptype T,const NumEffectState& state,Piece p, Action& action)
    {
      generatePtype<false,noCapturePromotion>(P, T, state,p,0,action);
    }
    template<bool useDirMask,bool noCapturePromotion, typename Action>
    static void generate(Player P,const NumEffectState& state,Piece p,int dirMask, Action& action)
    {
      PieceOnBoard::generatePtype<useDirMask,noCapturePromotion>(P,moveTypes[I(ptype(p))],state,p,dirMask,action);
    }
  }; // PieceOnBoard

  class Drop
  {
  public:
    template<class Action>
    static void generate(Player P, const NumEffectState& state,Action& action){
      Move move_base[7];
      int limit = 0;
      for(Ptype pt : PieceStandOrder)
	if(state.hasPieceOnStand(P, pt))
	  move_base[limit++] = newMove(Square::STAND, pt, P);
      if( limit == 0) return;
      int limit2 = limit - state.hasPieceOnStand(P, PAWN)
	- state.hasPieceOnStand(P, LANCE);
      int limit1 = limit2 - state.hasPieceOnStand(P, KNIGHT);
      for(int x = 9; x > 0; x--){
	int limit3 = limit - (state.hasPieceOnStand(P, PAWN) &&
			      state.isPawnMaskSet(P, x));
	if(limit3 == 0) continue;
	Square sq = newSquare(x, (P == BLACK ? 1 : 9));
	if(isEmpty(state[sq]))
	  for(int i = 0; i< limit1; i++) 
	    action(newAddTo(move_base[i], sq), MoveMask::DROP);
	sq = nextSquare(P, sq, D);
	if(isEmpty(state[sq])){
	  for(int i = 0; i< limit1; i++) 
	    action(newAddTo(move_base[i], sq),  MoveMask::DROP);
	  for(int i = limit2; i< limit3; i++) 
	    action(newAddTo(move_base[i], sq),  MoveMask::DROP);
	}
	for(int j = 0; j < 7; j++){
	  sq = nextSquare(P, sq, D);
	  if(isEmpty(state[sq]))
	    for(int i = 0; i< limit3; i++) 
	      action(newAddTo(move_base[i], sq),  MoveMask::DROP);
	}
      }
    }
  }; // class Drop
  /**
   * Move::ignoreUnpromote() でないすべての手を生成
   * @param Action move_action
   */
  class AllMoves
  {
  public:
    /**
     * すべての手を生成する
     */
    template<bool noCapturePromotion=false,typename Action>
    static void generate(Player P, const NumEffectState& state, Action& action){
      for(Ptype T : BasicNonKings){
	for(int num=indexMin(T);num<indexLimit(T);++num){
	  Piece p=state.pieceOf(num);
	  if(isOnBoardByOwner(P,p)){
	    if(canPromote(T) && isPromoted(p)){
	      Ptype PT=moveTypes[I(promote(T))];
	      PieceOnBoard::generatePtype<noCapturePromotion>(P,PT,state,p,action);
	    }
	    else{
	      PieceOnBoard::generatePtype<noCapturePromotion>(P,T,state,p,action);
	    }
	  }
	}
      }
      PieceOnBoard::generateKing<false, noCapturePromotion>(P, state, state.kingSquare(P),0, action);
      Drop::generate<Action>(P, state,action);
    }
  };
  class Capture
  {
  public:
    template<bool isCapture = true, class Action>
    static void generate(Player P,const NumEffectState& state,Square target,Action& action,PieceMask pieces)
    {
      constexpr MoveMask mt = (isCapture ? MoveMask::CAPTURE : MoveMask::NO_CAPTURE);
      Piece p1=state.pieceAt(target);
      assert((isCapture ? owner(p1) == alt(P) : isEmpty(p1)) ||
	     (std::cerr << "isCaputre=" << isCapture << ",p1=" << p1 << ",P=" << P << std::endl,0)
	);
      for(int num : state.allNum(pieces)){
	Piece p=state.pieceOf(num);
	if(num == kingIndex(P)){
	  if(!state.hasEffect(alt(P), target)){
	    action(newMove(state.kingSquare(P), target, p1, KING, false, P), mt);
	  }
	  continue;
	}
	if(state.isPinOrOpen(P, num) && !state.pinnedCanMoveTo(P, p, target))
	  continue;
	Ptype T = osl::ptype(p);
	Square from=square(p);
	if(canPromote(T)){
	  if(canPromote(P,target)){
	    action(newMove(from,target,p1, T, true,P), 
		   mt | MoveMask::PROMOTION);
	    if(isBetterToPromote(T)) continue; // PAWN, BISHOP, ROOK
	    int y=(P==BLACK ? Y(target) : 10-Y(target));
	    if((T == LANCE || T == KNIGHT) && y != 3) continue;
	  }
	  else if(canPromote(P,from)){
	    action(newMove(from,target,p1,T,true,P), 
		   mt | MoveMask::PROMOTION);
	    if(isBetterToPromote(T)) continue;
	  }
	}
	action(newMove(from,target,p1,T,false,P), mt);
      }
    }
    /**
     * @param target 取る駒の位置 (can be empty)
     * no by king
     */
    template<bool isCapture,typename Action>
    static void escapeByCapture(Player P, const NumEffectState& state,Square target,Action& action)
    {
      PieceMask pieces=state.effect(P, target) & ~pieceMask(KING);
      Capture::generate<isCapture>(P,state,target,action,pieces);
    }
  public:
    /**
     * @param target (will be captured) square (can be empty)
     */
    template<typename Action>
    static void generate(Player P, const NumEffectState& state,Square target,
			 Action& action){
      PieceMask pieces=state.effect(P, target);
      Capture::generate<true>(P,state,target,action,pieces);
    }
  };

  /**
   * generate promote moves
   * each of promote moves is unique
   * no suicide move is generated
   * @param  noCapture - when this parameter is true, don't generate capture moves
   */
  class Promote
  {
  public:
    template<class Action>
    static void generate(Player P,const NumEffectState& state, Action& action)
    {
      // the order is sorted by values of promotion
      for(Ptype T : PROMOTE_ORDER){
	for(Piece p : state.allPieceStrict(P, T)){
	  Square from=square(p);
	  PromoteType pt = promoteType(P, T, from);
	  if(pt >= CanPromoteType){
	    for(Direction Dir : DIRECTIONS){
	      if(!hasMove(T,Dir)) continue;
	      if(state.isPinOrOpen(P, p) && 
		 primDir(Dir) != primDir(state.pinnedDir(P, p)))
		continue;
	      Offset offset=newOffset(P, Dir);
	      if(isLong(Dir)){
		Direction sdir = longToShort(Dir);
		Square last = state.mobilityOf((P==BLACK ? sdir : rotate180Short(sdir)), number(p));
		for (Square to = from + offset; to != last; to += offset) {
		  action(newMove(from, to, T, Ptype::EMPTY, true, P), 
			 MoveMask::PROMOTION | MoveMask::NO_CAPTURE);
		}
	      }
	      else{
		Square to = from + offset;
		if(isEmpty(state[to]))
		  action(newMove(from, to, T, Ptype::EMPTY, true, P), 
			 MoveMask::PROMOTION | MoveMask::NO_CAPTURE);
	      }
	    }
          }
	  else if(pt == MayPromoteType){
	    for(Direction Dir : UP_DIRECTIONS){
	      if(!hasMove(T, Dir)) continue;
	      if(state.isPinOrOpen(P, p) && 
		 primDir(Dir) != primDir(state.pinnedDir(P, p)))
		continue;
	      Offset offset=newOffset(P,Dir);
	      if(isLong(Dir)){
		Direction sdir = longToShort(Dir);
		Square last = state.mobilityOf((P==BLACK ? sdir : rotate180Short(sdir)), number(p));
		for (Square to = last - offset; to != from; to -= offset) {
		  if (! canPromote(P,to)) break;
		  action(newMove(from, to, T, Ptype::EMPTY, true, P), 
			 MoveMask::NO_CAPTURE | MoveMask::PROMOTION);
		}
	      }
	      else{
		Square to = from + offset;
		if (isEmpty(state[to])){
		  action(newMove(from, to, T, Ptype::EMPTY, true, P), 
			 MoveMask::NO_CAPTURE | MoveMask::PROMOTION);
		}
	      }
	    }
          }
        }
      }
    }
  };
  /**
   * generate king escape moves
   * each of generated moves is unique.
   */
  class EscapeKing
  {
  public:
    template<typename Action>
    static void generate(Player P, const NumEffectState& state,Action& action){
      Square king = state.kingSquare(P);
      assert(state.inCheck(P)); 
      Piece attacker=Piece(0);
      state.findCheckPiece(P, attacker);
      if(attacker==Piece_EMPTY){ // multiple attack
	PieceOnBoard::generateKing<false,false>(P, state, king, 0,action);
	return;
      }
      Square from=square(attacker);
      Capture::escapeByCapture<true>(P, state, from,action);
      PieceOnBoard::generateKing<false,false>(P, state, king, 0,action);
      Offset offset = Board_Table.getShortOffset(king, from);
      assert(offset != Offset::ZERO);
      for(Square pos = king + offset; pos != from; pos += offset){
	assert(isEmpty(state.pieceAt(pos))); 
	Capture::escapeByCapture<false>(P, state, pos, action);
	// drop pieces to block
	for(Ptype T : BasicNonKings){
	  if(state.hasPieceOnStand(P, T) &&
	     (T!=PAWN || !state.isPawnMaskSet(P, X(pos))) && canDropTo(P, T, pos))
	    action(newMove(pos,T,P), MoveMask::DROP);
	}
      }
    }

  };

  /**
   * 利きをつける手を生成 利きを持つstateでしか使えない.
   * アルゴリズム:
   * \li 利きをつけたいマスから8近傍方向(長い利きも)，桂馬近傍の自分の利きをチェック
   * \li 自分の利きがあった時に，そこに移動したら問題のマスに利きをつけられる駒の種類かをチェックする
   * 特徴:
   * \li 相手玉の自由度が小さく，近傍に自分の利きがない時は高速に判定
   * isAttackToKing == true の時
   *  既に王手がかかっている状態は扱わない
   *  自殺手は生成しない?
   * isAttackToKing == false の時
   *  Additional Effect(利きが付いている方向の後ろに長い利きを足す)はいくつでも扱う．
   *  Shadow Effect(相手の利きが付いている方向の後ろに味方の長い利きを足す)は相手が1つの時だけ足す．
   *  自殺手は生成しない．
   *
   * no capture check
   */
  class AddEffectToKing
  {
    /**
     * あるマスに利きを持つすべての駒の中で，
     * ptypeMaskで指定されたptypeになる場合は移動する手を生成する
     * @param state - 盤面
     * @param to - 目的のマス
     * @param toP - 目的のマスに現在ある駒(又は空白)
     * @param action - 手生成のaction(典型的にはstoreかfilterつきstore)
     * @param ptypeMask - 移動後の駒のptypeに対応するbitが1なら手を生成する
     * pinnedの場合は移動する手が1手もない場合もある．
     */
    template<class Action>
    static void generateMoveToPtypeMask(Player P,const NumEffectState& state,Square to,Piece toP,unsigned int ptypeMask,Action& action)
    {
      PieceMask mask = state.effect(P, to) & ~pieceMask(KING) & ~state.pinOrOpen(alt(P));
      for (Piece p : state.allPiece(mask)){
	if(state.isPinOrOpen(P, p) && !state.pinnedCanMoveTo(P, p, to))
	  continue;
	Ptype T = ptype(p);
	Square from = square(p);
	if(canPromote(T) && (canPromote(P, to) || canPromote(P, from))){
	  Ptype pptype = osl::promote(T);
	  if((M(pptype) & ptypeMask) != 0)
	    action(newMove(from, to, toP, T, true, P), MoveMask::PROMOTION);
	  if(ignoreUnpromote(P, T, from,to)) continue;
	}
	if((M(T) & ptypeMask) != 0)
	  action(newMove(square(p), to, toP, T, false, P), MoveMask::ZERO);
      }
    }

#ifndef GENERATE_PAWNDROP_CHECKMATE
    /**
     * 敵玉の前に歩を置いた場合に遮った利きで敵玉にlibertyが生まれるかどうか?
     */
    static bool
    blockingU(Player P, const NumEffectState& state,Square pos)
    {
      const osl::Player altP=alt(P);
      for(Square from : state.allSquare(state.longEffect(P, pos))){
	if( (P==BLACK ? Y(from)>=Y(pos) : Y(pos)>=Y(from)) ){
	  Square shadowPos=pos+Board_Table.getShortOffset(from,pos);
	  assert((P==BLACK ? Y(shadowPos)<=Y(pos) : Y(pos)<=Y(shadowPos)) );
	  Piece p=state.pieceAt(shadowPos);
	  if(canMoveOn(altP,p) && !state.hasMultipleEffectAt(P,shadowPos)){
	    return true;
	  }
	}
      }
      return false;
    }
#endif
    /**
     * int DirType : 0  - U
     *               1  - LRD
     *               2  - UL,UR,DL,DR
     * dirOffset = newOffset(P,Dir)
     */
    template<class Action>
    static void generateDir(Player P, Direction Dir, const NumEffectState& state,Square target,Action& action,bool& hasPawnCheckmate)
    {
      int mask = ptypeMaskNotKing(Dir);
      Offset dirOffset = newOffset(P, Dir);
      Direction pDir = primDir(Dir);
      Player altP=alt(P);
      Square pos=target-dirOffset;
      if(!isOnBoard(pos)) return;
      Piece p=state.pieceAt(pos);
      if(isOnBoardByOwner(P,p)){
	if(Dir == U && state.hasLongEffect(P, pos, LANCE)){
	  PieceOnBoard::generate<true,false>(P,state,p,1<<pDir,action);
	}
	return;
      }
      if((V(state.king8Info(altP))&(1ull<<(40+int(Dir))))!=0){
	// - posに利きがある
	// TODO safe moveではない
	generateMoveToPtypeMask(P, state,pos,p,
				mask,action);
      }
      if(Dir != U) return;
      if(isEmpty(p)){
	Square pos1=state.kingMobilityOfPlayer(altP,Dir);
	PieceMask lance_mask = state.longEffect(P, pos1, LANCE);
	if(any(lance_mask)){
	  Piece p1=state.pieceAt(pos1);
	  if(isOnBoardByOwner(P,p1)){
	    PieceOnBoard::generate<true,false>(P,state,p1,1<<pDir,action);
	    // 
	    if(state.hasEffectByPiece(p1,pos)){
	      PieceOnBoard::generatePiece(P, state,p1,pos,Piece_EMPTY,action);
	    }
	  }
	  else if(isOnBoardByOwner(altP,p1)){
	    int num=bsf(lance_mask);
	    Piece p2=state.pieceOf(num);
	    if(!state.isPinOrOpen(P, num) ||
	       isUD(state.kingSquare(P),square(p2))){
	      action(newMove(square(p2),pos1,p1,LANCE,false,P), MoveMask::ZERO);
	    }
	  }
	}
	// - PAWN, LANCEはここで調べる?
	//  + ただしPAWNはつみは禁止
	if(! state.isPawnMaskSet(P, X(target)) &&
	   state.hasPieceOnStand(P, PAWN)){
	  // 利きをさえぎるパターンの検証
#ifndef GENERATE_PAWNDROP_CHECKMATE
	  if(((V(state.king8Info(altP))&(0xff00ull|(1ull<<(V(U)+24))))^(1ull<<(V(U)+24)))!=0 || blockingU(P,state,pos))
	    action(newMove(pos,PAWN,P), MoveMask::DROP);
	  else
	    hasPawnCheckmate=true;
#else
	  action(newMove(pos,PAWN,P), MoveMask::DROP);
#endif
	}
	if(state.hasPieceOnStand(P, LANCE)){
	  action(newMove(pos,LANCE,P), MoveMask::DROP);
	  for(pos-=newOffset(P,U);
	      pos!=pos1;pos-=newOffset(P,U)){
	    action(newMove(pos,LANCE,P), MoveMask::DROP);
	  }
	}
      }
    }

    template<class Action>
    static void generateKnightAll(Player P, const NumEffectState& state,Square target,Action& action)
    {
      for(Direction Dir : DIRECTIONS_UUL_UUR){
	Square pos = target - newOffset(P, Dir);
	if(!isOnBoard(pos)) continue;
	Piece p = state[pos];
	if(!canMoveOn(P,p)) continue;
	PieceMask mask = state.effectStrict(P, pos, KNIGHT) & ~state.pinOrOpen(P);
	for(Square sq : state.allSquare(mask))
	  action(newMove(sq, pos, p, KNIGHT, false, P), MoveMask::ZERO);
	if(state.hasPieceOnStand(P, KNIGHT) && isEmpty(p))
	  action(newMove(pos,KNIGHT,P), MoveMask::DROP);
      }
    }
    template<Player P,class Action,bool mustCareSilver>
    static void generateOpenOrCapture(const NumEffectState& state,Square target,Piece p,int num,Action& action)
    {
      // TODO: pin, captureを作る
      Direction d=Board_Table.getShort8(P, square(p), target);
      Square mid=state.mobilityOf((P==BLACK ? d : rotate180Short(d)),num);
      assert(isOnBoard(mid));
      const Player altP=alt(P);
      Square mid1=state.kingMobilityOfPlayer(altP,d);
      if(mid==mid1){
	Piece p1=state.pieceAt(mid);
	assert(isPiece(p1));
	Square target_next=target-Board_Table.getShort8OffsetUnsafe(square(p),target);
	if((P==BLACK ? pieceIsBlack(p1) : !pieceIsBlack(p1))){
	  // open attack
	  PieceOnBoard::generate<true,false>(P,state,p1,(1<<primDir(d)),action);
	  // p1がtarget_nextに利きを持つ
	  if(state.hasEffectByPiece(p1,target_next)){
	    // silverが斜め下に利きを持つ場合は「成らず」しか生成しない
	    if(mustCareSilver && osl::ptype(p1)==SILVER && 
	       (P==BLACK ? Y(target)>Y(mid) : Y(target)<Y(mid))){
	      // pinの場合は動ける可能性はない 
	      if(!state.isPinOrOpen(P, p1)){
		assert(state[target_next]==Piece_EMPTY);
		action(newMove(mid,target_next,SILVER,Ptype::EMPTY,false,P),MoveMask::NO_CAPTURE);
	      }
	    }
	    else
	      PieceOnBoard::generatePiece(P, state,p1,target_next,Piece_EMPTY,action);
	  }
	}
	else{
	  // 隣の場合はすでに作っている
	  if(mid==target_next)
	    return;
	  PieceOnBoard::generatePiece(P, state,p,mid,p1,action);
	}
      }
    }

    template<osl::Player P,class Action>
    static void generateRookLongMove(const NumEffectState& state,Square target,Action& action)
    {
      const Player altP=alt(P);
      for(int num=indexMin(ROOK);num<indexLimit(ROOK);num++){
	// pinの場合はすでに作っている
	if(state.isPinOrOpen(altP, num)) continue;
	Piece p=state.pieceOf(num);
	if(!isOnBoardByOwner(P,p)) continue;
	if(isULRD(target,square(p))){
	  generateOpenOrCapture<P,Action,false>(state,target,p,num,action);
	  continue;
	}
	int target_x=X(target);
	int target_y=Y(target);
	int rook_x=X(square(p));
	int rook_y=Y(square(p));
	if(isPromoted(p)){
	  if((unsigned int)(target_x-rook_x+1)>2u){ // abs(target_x-rook_x)>1
	    if((unsigned int)(target_y-rook_y+1)>2u){ // abs(target_y-rook_y)>1
	      {
		Square pos=newSquare(rook_x,target_y);
		Piece p1=state.pieceAt(pos);
		if(test(state.effect(pos), num) &&
		   canMoveOn(P,p1) &&
		   I(state.kingMobilityAbs(altP,R)) >= I(pos) &&
		   I(pos) >= I(state.kingMobilityAbs(altP,L)) &&
		   (!state.isPinOrOpen(P, num) ||
		    isUD(square(p),state.kingSquare(P)))
		  ){
		  action(newMove(square(p),pos,p1,PROOK,false,P), MoveMask::ZERO);
		}
	      }
	      {
		Square pos=newSquare(target_x,rook_y);
		Piece p1=state.pieceAt(pos);
		if(test(state.effect(pos), num) &&
		   canMoveOn(P,p1) &&
		   I(state.kingMobilityAbs(altP,U)) >= I(pos) &&
		   I(pos) >= I(state.kingMobilityAbs(altP,D)) &&
		   (!state.isPinOrOpen(P, num) ||
		    isLR(square(p),state.kingSquare(P)))
		  ){
		  action(newMove(square(p),pos,p1,PROOK,false,P), MoveMask::ZERO);
		}
	      }
	    }
	    else{ // (abs(target_x-rook_x)>1 && abs(target_y-rook_y)==1
	      int min_x=X(state.kingMobilityAbs(altP,L));
	      int max_x=X(state.kingMobilityAbs(altP,R));
	      if(target_x>rook_x) max_x=target_x-2;
	      else min_x=target_x+2;
	      min_x=std::max(min_x,rook_x-1);
	      max_x=std::min(max_x,rook_x+1);
	      for(int x=min_x;x<=max_x;x++){
		Square pos=newSquare(x,target_y);
		Piece p1=state.pieceAt(pos);
		if(canMoveOn(P,p1))
		  PieceOnBoard::generatePiecePtype(P,PROOK, state,p,pos,p1,action);
	      }
	    }
	  }
	  else if((unsigned int)(target_y-rook_y+1)>2u){ // abs(target_y-rook_y)>1, abs(target_x-rook_x)==1
	    int min_y=Y(state.kingMobilityAbs(altP,D));
	    int max_y=Y(state.kingMobilityAbs(altP,U));
	    if(target_y>rook_y) max_y=target_y-2;
	    else min_y=target_y+2;
	    min_y=std::max(min_y,rook_y-1);
	    max_y=std::min(max_y,rook_y+1);
	    for(int y=min_y;y<=max_y;y++){
	      Square pos=newSquare(target_x,y);
	      Piece p1=state.pieceAt(pos);
	      if(canMoveOn(P,p1))
		PieceOnBoard::generatePiecePtype(P,PROOK,state,p,pos,p1,action);
	    }
	  }
	}
	else{ // ROOK
	  // vertical move
	  if((unsigned int)(target_x-rook_x+1)>2u){ // abs(target_x-rook_x)>1
	    Square pos=newSquare(rook_x,target_y);
	    Piece p1=state.pieceAt(pos);
	    if(test(state.effect(pos), num) &&
	       canMoveOn(P,p1) &&
	       I(state.kingMobilityAbs(altP,R)) >= I(pos) &&
	       I(pos) >= I(state.kingMobilityAbs(altP,L)) &&
	       (!state.isPinOrOpen(P, num) ||
		isUD(square(p),state.kingSquare(P)))
	      ){
	      if(osl::canPromoteY(P,rook_y) || osl::canPromoteY(P,target_y)){
		action(newMove(square(p),pos,p1,ROOK,true,P), MoveMask::PROMOTION);
	      }
	      else 
		action(newMove(square(p),pos,p1,ROOK,false,P), MoveMask::ZERO);
	    }
	  }
	  // horizontal move
	  if((unsigned int)(target_y-rook_y+1)>2u){ // abs(target_y-rook_y)>1
	    Square pos=newSquare(target_x,rook_y);
	    Piece p1=state.pieceAt(pos);
	    if(test(state.effect(pos), num) &&
	       canMoveOn(P,p1) &&
	       I(state.kingMobilityAbs(altP,U)) >= I(pos) &&
	       I(pos) >= I(state.kingMobilityAbs(altP,D)) &&
	       (!state.isPinOrOpen(P, num) ||
		isLR(square(p),state.kingSquare(P)))
	      ){
	      if(canPromoteY(P,rook_y)){
		action(newMove(square(p),pos,p1,ROOK,true,P), MoveMask::PROMOTION);
	      }
	      else
		action(newMove(square(p),pos,p1,ROOK,false,P), MoveMask::ZERO);
	    }
	  }
	}
      }
    }
    template<Player P,Ptype T,class Action>
    static void generateBishopLongMove(const NumEffectState& state,Square target,Action& action,Piece p,int num)
    {
      const Player altP=alt(P);
      int target_x=X(target);
      int target_y=Y(target);
      int target_xPy=target_x+target_y;
      int target_xMy=target_x-target_y;
      int bishop_x=X(square(p));
      int bishop_y=Y(square(p));
      int bishop_xPy=bishop_x+bishop_y;
      int bishop_xMy=bishop_x-bishop_y;
      if(((target_xPy^bishop_xPy)&1)!=0){
	if(T==BISHOP) return;
	// 市松模様のparityの違う場合も，隣ならOK?
	if((unsigned int)(target_xPy-bishop_xPy+1)<=2u){ // abs(target_xPy-bishop_xPy)==1
	  Square ul=state.kingMobilityAbs(altP,UL);
	  Square dr=state.kingMobilityAbs(altP,DR);
	  int min_xMy=X(ul)-Y(ul);
	  int max_xMy=X(dr)-Y(dr);
	  if(target_xMy>bishop_xMy) max_xMy=target_xMy-4;
	  else min_xMy=target_xMy+4;
	  min_xMy=std::max(min_xMy,bishop_xMy-1);
	  max_xMy=std::min(max_xMy,bishop_xMy+1);
	  for(int xMy=min_xMy;xMy<=max_xMy;xMy+=2){
	    int pos_x=(target_xPy+xMy)>>1;
	    int pos_y=(target_xPy-xMy)>>1;
	    Square pos=newSquare(pos_x,pos_y);
	    Piece p1=state.pieceAt(pos);
	    if(canMoveOn(P,p1))
	      PieceOnBoard::generatePiecePtype(P,T,state,p,pos,p1,action);
	  }
	}
	else if((unsigned int)(target_xMy-bishop_xMy+1)<=2u){ // abs(target_xMy-bishop_xMy)==1
	  Square dl=state.kingMobilityAbs(altP,DL);
	  Square ur=state.kingMobilityAbs(altP,UR);
	  int min_xPy=X(dl)+Y(dl);
	  int max_xPy=X(ur)+Y(ur);
	  if(target_xPy>bishop_xPy) max_xPy=target_xPy-4;
	  else min_xPy=target_xPy+4;
	  min_xPy=std::max(min_xPy,bishop_xPy-1);
	  max_xPy=std::min(max_xPy,bishop_xPy+1);
	  for(int xPy=min_xPy;xPy<=max_xPy;xPy+=2){
	    int pos_x=(xPy+target_xMy)>>1;
	    int pos_y=(xPy-target_xMy)>>1;
	    Square pos=newSquare(pos_x,pos_y);
	    Piece p1=state.pieceAt(pos);
	    if(canMoveOn(P,p1))
	      PieceOnBoard::generatePiecePtype(P,T, state,p,pos,p1,action);
	  }
	}
	return;
      }
      //  / 方向(dx==dy)から王手をかける
      if((unsigned int)(target_xPy-bishop_xPy+2)>4u){ // abs(target_xPy-bishop_xPy)>2
	int pos_x=(bishop_xPy+target_xMy)>>1;
	int pos_y=(bishop_xPy-target_xMy)>>1;
	Square pos=newSquare(pos_x,pos_y);
	if(isOnBoard(pos)){
	  Piece p1=state.pieceAt(pos);
	  if(test(state.effect(pos), num) &&
	     canMoveOn(P,p1) &&
	     I(state.kingMobilityAbs(altP,UR)) >= I(pos) &&
	     I(pos) >= I(state.kingMobilityAbs(altP,DL))
	    ){
	    PieceOnBoard::generatePiecePtype(P,T, state,p,pos,p1,action);
	  }
	}
      }
      else if(target_xPy==bishop_xPy){
	generateOpenOrCapture<P,Action,true>(state,target,p,num,action);
	return;
      }
      //  \ 方向(dx== -dy)から王手をかける
      if((unsigned int)(target_xMy-bishop_xMy+2)>4u){ // abs(target_xMy-bishop_xMy)>2
	int pos_x=(target_xPy+bishop_xMy)>>1;
	int pos_y=(target_xPy-bishop_xMy)>>1;
	Square pos=newSquare(pos_x,pos_y);
	if(isOnBoard(pos)){
	  Piece p1=state.pieceAt(pos);
	  if(test(state.effect(pos), num) &&
	     canMoveOn(P,p1) &&
	     I(state.kingMobilityAbs(altP,DR)) >= I(pos) &&
	     I(pos) >= I(state.kingMobilityAbs(altP,UL))
	    ){
	    PieceOnBoard::generatePiecePtype(P,T, state,p,pos,p1,action);
	  }
	}
      }
      else if(target_xMy==bishop_xMy){
	generateOpenOrCapture<P,Action,true>(state,target,p,num,action);
	return;
      }

    }
    template<osl::Player P,class Action>
    static void generateKing(const NumEffectState& state,Square target,Action& action,bool &hasPawnCheckmate)
    {
	
      const Player altP=alt(P);
      assert(target==state.kingSquare(altP));
      generateDir(P, U, state,target,action,hasPawnCheckmate);
      generateKnightAll(P, state,target,action);
      for(Direction Dir : SHORT8_DIRECTIONS_NOT_U)
	generateDir(P, Dir, state, target, action, hasPawnCheckmate);
      generateRookLongMove<P,Action>(state,target,action);
      for(int num=indexMin(BISHOP);num<indexLimit(BISHOP);num++){
	// pinの場合はすでに作っている
	if(state.isPinOrOpen(altP, num)) continue;
	Piece p=state.pieceOf(num);
	if(!isOnBoardByOwner(P,p)) continue;
	if(isPromoted(p))
	  generateBishopLongMove<P,PBISHOP,Action>(state,target,action,p,num);
	else
	  generateBishopLongMove<P,BISHOP,Action>(state,target,action,p,num);
      }
      int sp = spaces(state.king8Info(altP));
      // gold
      if(state.hasPieceOnStand(P, GOLD)){
	unsigned int mask = sp & moveMasks[I(GOLD)];
	if(mask!=0){
	  for(Direction Dir : GOLD_DIRECTIONS){
	    if((mask & M(Dir)) != 0)
	      action(newMove(target - newOffset(P, Dir), GOLD, P), MoveMask::DROP);
	  }
	}
      }
      // silver
      if(state.hasPieceOnStand(P, SILVER)){
	unsigned int mask = sp & moveMasks[I(SILVER)];
	if(mask != 0){
	  for(Direction Dir : SILVER_DIRECTIONS)
	    if((mask & M(Dir)) != 0)
	      action(newMove(target - newOffset(P, Dir), SILVER, P), MoveMask::DROP);
	}
      }
      // bishop
      if(state.hasPieceOnStand(P, BISHOP)){
	for(Direction Dir : BISHOP_DIRECTIONS){
	  Square pos = state.kingMobilityOfPlayer(altP, Dir);
	  Offset offset = newOffset(P, Dir);
	  for(pos += offset; pos != target; pos += offset)
	    action(newMove(pos, BISHOP, P), MoveMask::DROP);
	}
      }
      // rook
      if(state.hasPieceOnStand(P, ROOK)){
	for(Direction Dir : ROOK_DIRECTIONS){
	  Square pos = state.kingMobilityOfPlayer(altP, Dir);
	  Offset offset = newOffset(P, Dir);
	  for(pos += offset; pos != target; pos += offset)
	    action(newMove(pos, ROOK, P), MoveMask::DROP);
	}
      }
    }
  public:
    template <Player P, typename Action>
    static void generate(const NumEffectState& state,Square target,Action& action,bool &hasPawnCheckmate)
    {
      generateKing<P,Action>(state,target,action,hasPawnCheckmate);
    }
    template<Player P, typename Action>
    static void generate(const NumEffectState& state,Square target,Action& action){
      bool dummy;
      generate<P>(state,target,action,dummy);
    }
  };
} // namespace osl

#endif /* OSL_MOVEGEN_H */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
