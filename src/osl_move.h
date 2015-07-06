#ifndef OSL_MOVE_H
#define OSL_MOVE_H

#include "osl_types.h"
#include "osl_position.h"

/* kingOpenMove.h
 */
namespace osl
{
  
  /**
   * Pの王をopen checkにする手でないことをチェック.
   * - P==move playerの時は自殺手かどうかのチェックに使う.
   *      王が動く場合には呼べない
   * - P!=move playerの時は通常のopen checkかどうかに使う.
   * - DropMoveの時には呼べない
   */
  template <Player P>
  struct KingOpenMove
  {
    /**
     * king が59
     * rookが51->61の時，差は
     * OFFSET -8 -> U
     * OFFSET +8 -> D
     * とはなるので，一直線のような気がする．ただし，そもとも，
     * 59 - 51はpinにはならないし，今は U -> DはopenではないとしているのでOK
     */
    static bool isMember(const NumEffectState& state, 
			 Ptype /*ptype*/,Square from,Square to)
    {
      if(!state.isPinOrOpen(P, state.pieceAt(from))) return false;
      // from to kingが一直線に並べば false
      Square king=state.kingSquare(P);
      return Board_Table.getShort8Unsafe(P, king, to)
	!= Board_Table.getShort8(P, king,from);
    }
  };

/* safeMove.h
 */
  /**
   * 元々，手番の玉に王手がかかっていない状態で自殺手でないことをチェック.
   * DropMoveの時には呼べない
   */
  template <Player P>
  struct SafeMove
  {
    static bool isMember(const NumEffectState& state, 
			 Ptype ptype,Square from,Square to)
    {
      assert(! isPieceStand(from));
      assert(owner(state[from]) == P);
      /**
       * 元々王手がかかっていないと仮定しているので，自分を
       * 取り除いた上でhasEffectByを呼ばなくても良い
       */
      if (ptype==KING)
	return ! state.hasEffect(alt(P), to);
      return ! KingOpenMove<P>::isMember(state,ptype,from,to);
    }
  };

/* directCheck.h
 */
  template <Player P>
  struct DirectCheck
  {
    static bool isMember(const NumEffectState& state, Ptype ptype, Square to)
    {
      /**
       * 最初から王手ということはない．
       */
      assert(!state.hasEffect(P, state.kingSquare(alt(P))));
      /**
       * stateを動かしていないので，fromにある駒がtoからの利きを
       * blockすることは
       * あるが，blockされた利きが王手だったとすると，動かす前から王手
       * だったとして矛盾するのでOK
       */
      return state.hasEffectIf(newPtypeO(P,ptype),to,
			       state.kingSquare(alt(P)));
    }

    template<class State>
    static bool isMember(const State& state, Ptype ptype, Square /*from*/, Square to)
    {
      return isMember(state, ptype, to);
    }
  };
/* openCheck.h
 */
  template <Player P>
  struct OpenCheck
  {
    static bool isMember(const NumEffectState& state, 
			 Ptype ptype,Square from,Square to)
    {
      return KingOpenMove<alt(P)>::isMember
	(state,ptype,from,to);
    }
  };

/* check_.h
 */
  /**
   * @param 指す側, alt(P)に王手をかけられるか判定
   */
  template <Player P>
  struct Check
  {
    /**
     * promote move の時 ptypeはpromote後のもの
     */
    static bool isMember(const NumEffectState& state, 
			 Ptype ptype,Square from,Square to){
      if (DirectCheck<P>::isMember(state,ptype,to)) 
	return true;
      if (isPieceStand(from)) 
	return false;
      return OpenCheck<P>::isMember(state,ptype,from,to);
    }
  };
}

#endif // OSL_MOVE_H
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
