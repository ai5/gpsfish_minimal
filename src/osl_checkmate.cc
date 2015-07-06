/* immediateCheckmateTable.cc
 */
#include "osl_checkmate.h"


namespace
{
  bool canCheckmate(osl::Ptype ptype,osl::Direction dir,unsigned int mask)
  {
    using namespace osl;
    // 王はdropできない, 打ち歩詰め
    if(ptype==osl::KING || ptype==osl::PAWN) return false;
    // ptypeがdir方向に利きを持たない == 王手をかけられない
    if(!(osl::moveMasks[I(ptype)]&
	 (osl::M(dir) | osl::M(osl::shortToLong(dir))))) return false;
    int dx=blackDxs[I(dir)];
    int dy=blackDys[I(dir)];
    for(int l=0;l<8;l++){
      if((mask&(1<<l))==0) continue;
      osl::Direction dir1=static_cast<osl::Direction>(l);
      int dx1=blackDxs[I(dir1)];
      int dy1=blackDys[I(dir1)];
      osl::Offset32 o32=newOffset32(dx-dx1,dy-dy1);
      if(!hasEffect(osl::Ptype_Table.getEffect(osl::newPtypeO(osl::BLACK,ptype),o32)))
	return false;
    }
    return true;
  }
}

osl::ImmediateCheckmateTable::ImmediateCheckmateTable()
{
#if 1
  for(auto&v : ptypeDropMasks) v.fill(0);
  for(auto&v : blockingMasks) v.fill(0);
  for(auto&v : noEffectMasks) v.fill(0);
#endif
  // ptypeDropMaskの初期化
  for(int i=0;i<0x100;i++){
    for(Ptype k=Ptype::BASIC_MIN;k<=Ptype::MAX;++k){
      unsigned char mask=0;
      Ptype ptype=static_cast<Ptype>(k);
      for(int j=0;j<8;j++){
	// 玉の逃げ道がある
	if((i&(0x1<<j))!=0)continue;
	Direction dir=static_cast<Direction>(j);
	if(canCheckmate(ptype,dir,i))
	  mask|=(1<<j);
      }
      ptypeDropMasks[i][I(ptype)]=mask;
    }
  }
  // dropPtypeMaskの初期化
  for(int i=0;i<0x10000;i++){
    unsigned char ptypeMask=0;
    for(Ptype k=Ptype::BASIC_MIN;k<=Ptype::MAX;++k){
      Ptype ptype=static_cast<Ptype>(k);
      for(int j=0;j<8;j++){
	// 空白でない
	if((i&(0x1<<j))==0) continue;
	// 玉の逃げ道がある
	if((i&(0x100<<j))!=0)continue;
	Direction dir=static_cast<Direction>(j);
	if(canCheckmate(ptype,dir,(i>>8)&0xff)){
	  ptypeMask|=1u<<(k-Ptype::BASIC_MIN);
	  goto nextPtype;
	}
      }
    nextPtype:;
    }
    dropPtypeMasks[i]=ptypeMask;
  }
  // blockingMaskの初期化
  for(Ptype k=Ptype::BASIC_MIN;k<=Ptype::MAX;++k){
    Ptype ptype=static_cast<Ptype>(k);
    for(int j=0;j<8;j++){
      unsigned int mask=0;
      Direction dir=static_cast<Direction>(j);
      if(canMove(ptype,dir)){
	int dx=blackDxs[I(dir)];
	int dy=blackDys[I(dir)];
	for(int l=0;l<8;l++){
	  Direction dir1=static_cast<Direction>(l);
	  int dx1=blackDxs[I(dir1)];
	  int dy1=blackDys[I(dir1)];
	  Offset32 o32=newOffset32(dx-dx1,dy-dy1);
	  if(!hasEffect(Ptype_Table.getEffect(newPtypeO(BLACK,ptype),o32))){
	    if(Board_Table.getShortOffsetNotKnight(o32) != Offset::ZERO &&
	       !(dx==-dx1 && dy==-dy1)
	       ){
	      mask|=1<<l;
	    }
	  }
	}
      }
      blockingMasks[I(ptype)][I(dir)]=mask;
    }
  }
  // effectMaskの初期化
  for(Ptype k=Ptype::PIECE_MIN;k<=Ptype::MAX;++k){
    Ptype ptype=static_cast<Ptype>(k);
    for(int j=0;j<8;j++){
      unsigned int mask=0x1ff;
      Direction dir=static_cast<Direction>(j);
      if(canMove(ptype,dir)){// 王手をかけられる
	mask=0;
	int dx=blackDxs[I(dir)];
	int dy=blackDys[I(dir)];
	for(int l=0;l<8;l++){
	  Direction dir1=static_cast<Direction>(l);
	  int dx1=blackDxs[I(dir1)];
	  int dy1=blackDys[I(dir1)];
	  Offset32 o32=newOffset32(dx-dx1,dy-dy1);
	  if(dir!= dir1 &&
	     !hasEffect(Ptype_Table.getEffect(newPtypeO(BLACK,ptype),o32))){
	    mask|=1<<l;
	  }
	}
      }
      noEffectMasks[I(ptype)][I(dir)]=mask;
    }
  }
}


/* immediateCheckmate.cc
 */

namespace osl
{
    template 
    bool ImmediateCheckmate::
    hasCheckmateMove<BLACK>(NumEffectState const&, King8Info, Square, Move&);
    template 
    bool osl::ImmediateCheckmate::
    hasCheckmateMove<WHITE>(NumEffectState const&, King8Info, Square, Move&);

    template 
    bool ImmediateCheckmate::
    hasCheckmateMove<BLACK>(NumEffectState const&, Move&);
    template 
    bool osl::ImmediateCheckmate::
    hasCheckmateMove<WHITE>(NumEffectState const&, Move&);

    template 
    bool ImmediateCheckmate::
    hasCheckmateMove<BLACK>(NumEffectState const&);
    template 
    bool osl::ImmediateCheckmate::
    hasCheckmateMove<WHITE>(NumEffectState const&);
}

bool osl::ImmediateCheckmate::
hasCheckmateMove(Player pl,NumEffectState const& state)
{
  if(pl==BLACK)
    return hasCheckmateMove<BLACK>(state);
  else
    return hasCheckmateMove<WHITE>(state);

}
bool osl::ImmediateCheckmate::
hasCheckmateMove(Player pl,NumEffectState const& state,Move& bestMove)
{
  if(pl==BLACK)
    return hasCheckmateMove<BLACK>(state,bestMove);
  else
    return hasCheckmateMove<WHITE>(state,bestMove);
}

/* ------------------------------------------------------------------------- */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:

