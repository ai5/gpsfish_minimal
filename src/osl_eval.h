#ifndef OSL_EVAL_H
#define OSL_EVAL_H

#include "osl_types.h"
#include "osl_position.h"
#include <algorithm>
#include <cassert>
#if 0
#include "move.h"
#endif
#include <iosfwd>
#include <iostream>
#include <cstring>
#include <valarray>
/* midgame.h
 */

namespace osl
{
  const int NStages = 4;
  const int EvalStages = 4;
  typedef QuadInt MultiInt;
  typedef QuadIntPair MultiIntPair;
  const int EndgameIndex = EvalStages-1;
  constexpr int STAGES[4]={0, 1, 2, EndgameIndex};

/* evalStagePair.h
 */

  typedef std::array<std::array<int,EvalStages>,2> EvalStagePair;
  typedef std::array<std::array<int,2>,EvalStages> PairEvalStage;

/* weights.h 
 */

  struct Weights
  {
  protected:
    std::valarray<signed short> values;
    size_t dim;
  public:
    explicit Weights(size_t dim=0);
    virtual ~Weights();

    void resetDimension(size_t new_dim);
    int value(size_t index) const { assert(index < dim); return values[index]; }

    size_t dimension() const { return dim; }

    void setValue(size_t index, int v) 
    {
      assert(index < dim);
      values[index] = v;
      assert(values[index] == v);
    }
  };


  struct NewProgressData
  {
    MultiIntPair non_pawn_ptype_attacked_pair_eval;
//    std::array<QuadIntPair, 2> non_pawn_attacked_turn;
    MultiInt promotion37_eval;
    int progress_all;
    std::array<int, 2> king_progress;
    std::array<int, 2> attack5x5_progresses;
    std::array<int, 2> piecestand7_mask;
    int pawn_facing_count;
    std::array<std::array<uint8_t, 16>, 2> promotion37_x;
    std::array<std::array<uint8_t, 8>, 2> promotion37_count;
    std::array<int, 40> nonpawn_indices;
    std::array<PieceMask, 2> attack55_mask;
    std::array<PieceMask, 2> mask5x5;
    std::array<int, 2> attackCount, defenseCount;
    std::array<std::array<uint8_t, 8>, 2> vertical_x, king_vertical_x;
  };
  class NewProgress : private NewProgressData
  {
  public:
    enum { ProgressScale = 2 };
  private:
    static bool initialized_flag;
    static std::array<int, Piece_SIZE> stand_weight;
    static std::array<int, 1125> attack5x5_weight;
    static std::array<int, 5625> attack5x5_x_weight;
    static std::array<int, 10125> attack5x5_y_weight;
    static std::array<int, 75> effectstate_weight;
    static std::array<int, 81 * 15 * 11 * 2> effect_relative;
    static std::array<int, 4284> king_relative_weight;
    static std::array<int, 262144> attacked_ptype_pair_weight;
    static std::array<int, 10> pawn_facing_weight;
    static std::array<int, 16> promotion37_weight;
    static std::array<int, 128> piecestand7_combination;
    static int max_progress;
    void updatePieceKingRelativeBonus(Player P, const NumEffectState &state, int& result);
    void updateNonPawnAttackedPtypePair(const NumEffectState& state, NewProgress const& previous);
    void setNonPawnAttackedPtypePairOne(Player Owner, const NumEffectState& state);
    void updatePawnFacing(const NumEffectState& state);
    void promotion37One(Player Attack, const NumEffectState& state);
    void updatePromotion37(const NumEffectState& state);
    void updatePieceStand7(const NumEffectState& state);
    static void progressOne(Player P, const NumEffectState &state,
			    int &attack_defense);
    static void updateOne(Player P, const NumEffectState &state,
			    int &attack_defense);
    void setAttack5x5PiecesAndState(Player P, const NumEffectState &state);
    void updateAttack5x5PiecesAndState(Player P, const NumEffectState &state, NewProgress const& previous, Move last_move);
    void setAttack5x5Mask(Player P, const NumEffectState &state);
    void setAttack5x5Pieces(Player P,PieceMask, const NumEffectState&);
    void setAttack5x5Value(Player P, const NumEffectState &state);
    void updateAttack5x5Value(Player P, const NumEffectState &state, NewProgress const& previous,Move last_move);
    static int index(Player P, Square king, Square target)
    {
      const int x_diff = std::abs(X(king) - X(target)); // [0, 4]
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      return x_diff * 5 + y_diff;
    }
    static int indexX(Player P, Square king, Square target)
    {
      int target_x = (X(king) > 5 ? 10 - X(king) : X(king)); // [1, 5]
      int x_diff = X(king) - X(target); // [-4, 4]
      if (P == BLACK && X(king) >= 6)
      {
	x_diff = -x_diff;
      }
      else if (P == WHITE && X(king) >= 5)
      {
	x_diff = -x_diff;
      }
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      return ((x_diff + 4) * 5 + y_diff) * 5 + target_x - 1;
    }
    static int indexY(Player P, Square king, Square target)
    {
      const int x_diff = std::abs(X(king) - X(target)); // [0, 4]
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      const int king_y = (P == BLACK ? Y(king) : 10 - Y(king)); // [1, 9]
      return (x_diff * 5 + y_diff) * 9 + king_y - 1;
    }
    static int indexE(Player P, Square king, Square target)
    {
      Square kingB = forBlack(P, king), targetB = forBlack(P, target);
      int king_index = ((X(kingB) - 1) * 9 + (Y(kingB) - 1));
      Square center = Centering5x3::adjustCenter(kingB);
      int dx = X(targetB) - X(center) + 2, dy = Y(targetB) - Y(center) + 1;
      return 22 * (king_index * 15 + dx * 3 + dy);
    }
    static int index5x5(int rook, int bishop, int gold, int silver,
			int promoted)
    {
      assert(0 <= promoted && promoted <= 4);
      return promoted + 5 * (silver + 5 * (gold + 5 * (bishop + 3 * rook)));
    }
    static int index5x5x(int rook, int bishop, int gold, int silver,
			 int promoted, int king_x)
    {
      assert(0 <= promoted && promoted <= 4);
      return king_x - 1 +
	5 * (promoted + 5 * (silver + 5 * (gold + 5 * (bishop + 3 * rook))));
    }
    static int index5x5y(int rook, int bishop, int gold, int silver,
			 int promoted, int king_y)
    {
      assert(0 <= promoted && promoted <= 4);
      return king_y - 1 +
	9 * (promoted + 5 * (silver + 5 * (gold + 5 * (bishop + 3 * rook))));
    }
    static int indexPerEffect(Player P,Square king, Square target,
			      int count)
    {
      const int x_diff = std::abs(X(king) - X(target)); // [0, 4]
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      return x_diff * 5 + y_diff + std::min(8, count) * 25;
    }

    static int indexPerEffectY(Player P, Square king, Square target,
			       int count)
    {
      const int king_y = (P == BLACK ? Y(king) : 10 - Y(king));
      const int x_diff = std::abs(X(king) - X(target)); // [0, 4]
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      return king_y - 1 + 9 * (x_diff * 5 + y_diff + std::min(8, count) * 25);
    }
    static int indexPerEffectX(Player P, Square king, Square target,
			       int count)
    {
      const int king_x = (X(king) > 5 ? 10 - X(king) : X(king));
      int x_diff = X(king) - X(target); // [-4, 4]
      if ((P == BLACK && (X(king) > 5)) ||
	  (P == WHITE && (X(king) >= 5)))
	x_diff = -x_diff;
      const int y_diff = (P == BLACK ? Y(king) - Y(target) :
			  Y(target) - Y(king)) + 2; // [-2, 2] + 2
      return king_x - 1 + 5 * (x_diff + 4 +
			       9 * (y_diff + 5 *  std::min(8, count)));
    }
    static int indexRelative(Player P, const Square king,
			     const Ptype ptype, const Square pos)
    {
      const int x = std::abs(X(pos) - X(king));
      const int y = (Y(king) - Y(pos)) *
	(P == BLACK ? 1 : -1) + 8;
      return PI(ptype) * 17 * 9 + (x * 17 + y);
    }
    static int indexRelative(const Player player, const Square king,
			     const Piece piece)
    {
      return indexRelative(player, king, ptype(piece),  square(piece));
    }
  public:
    NewProgress(const NumEffectState &state);
    int progress() const
    {
      return
	std::max(std::min(
		   king_progress[0] + king_progress[1] +
		   attack5x5_progresses[0] +
		   attack5x5_progresses[1] +
		   progress_all,
		   max_progress-ProgressScale), 0) / ProgressScale;
    }
    static int maxProgress() { return max_progress / ProgressScale; }
    template<Player P>
    void update(const NumEffectState &new_state, Move last_move, NewProgress const& previous);
  public:
    static bool initialized()
    {
      return initialized_flag;
    }
    static bool setUp(const char *filename);
    static bool setUp();
    static std::string defaultFilename();
    const NewProgressData rawData() const { return *this; }
    void show(std::ostream& os) const;
  };
  bool operator==(const NewProgressData& l, const NewProgressData& r);
  inline bool operator==(const NewProgress& l, const NewProgress& r) 
  {
    return l.rawData() == r.rawData();
  }

// NewProgressが学習可能な場合に定義 (現在はosl側に変更はないので常に定義)

  struct OpenMidEndingEvalDebugInfo
  {
    enum StageFeature
    {
      KING_PIECE_RELATIVE,
      PIECE_STAND,
      KING25_EFFECT_EACH,
      PTYPEX,
      PTYPEY,
      ROOK_MOBILITY,
      BISHOP_MOBILITY,
      LANCE_MOBILITY,
      ROOK_EFFECT,
      BISHOP_EFFECT,
      PIECE_STAND_COMBINATION,
      PIECE_STAND_TURN,
      ROOK_PAWN,
      PAWN_DROP,
      PIECE_STAND_Y,
      KNIGHT_CHECK,
      PAWN_ADVANCE,
      PAWN_PTYPEO,
      PROMOTED_MINOR_PIECE,
      KING_PIECE_RELATIVE_NOSUPPORT,
      NON_PAWN_ATTACKED,
      NON_PAWN_ATTACKED_PTYPE,
      PTYPE_YY,
      KING3PIECES,
      BISHOP_HEAD,
      KNIGHT_HEAD,
      ROOK_PROMOTE_DEFENSE,
      PTYPE_COUNT,
      LANCE_EFFECT_PIECE,
      PTYPE_Y_PAWN_Y,
      BISHOP_AND_KING,
      PIECE_FORK_TURN,
      ROOK_SILVER_KNIGHT,
      BISHOP_SILVER_KNIGHT,
      KING25_EFFECT_SUPPORTED,
      KING_ROOK_BISHOP,
      KING_X_BLOCKED3,
      GOLD_RETREAT,
      SILVER_RETREAT,
      ALL_GOLD,
      ALL_MAJOR,
      KING25_EFFECT_DEFENSE,
      ANAGUMA_EMPTY,
      NO_PAWN_ON_STAND,
      NON_PAWN_PIECE_STAND,
      PIN_PTYPE_ALL,
      KING_MOBILITY,
      GOLD_AND_SILVER_NEAR_KING,
      PTYPE_COMBINATION,
      KING25_BOTH_SIDE,
      KING25_MOBILITY,
      BISHOP_STAND_FILE5,
      MAJOR_CHECK_WITH_CAPTURE,
      SILVER_ADVANCE26,
      KING25_EFFECT3,
      BISHOP_BISHOP_PIECE,
      ROOK_ROOK,
      ROOK_ROOK_PIECE,
      KING25_EFFECT_COUNT_COMBINATION,
      NON_PAWN_ATTACKED_PTYPE_PAIR,
      ATTACK_MAJORS_IN_BASE,
      STAGE_FEATURE_LIMIT
    };
    enum ProgressIndependentFeature
    {
      PIECE,
      BISHOP_EXCHANGE_SILVER_KING, // recalculated_value
      ENTER_KING_DEFENSE,
      KING25_EFFECT_ATTACK, // end recalculated_value
      PIECE_PAIR,
      PIECE_PAIR_KING,
      PROGRESS_INDEPENDENT_FEATURE_LIMIT
    };
    int value;
    int progress;
    std::array<int, PROGRESS_INDEPENDENT_FEATURE_LIMIT> progress_independent_values;
    std::array<MultiInt, STAGE_FEATURE_LIMIT> stage_values;

    static const char *name(ProgressIndependentFeature);
    static const char *name(StageFeature);
  };

  class OpenMidEndingEval
  {
  private:
    enum { INVALID=250000001 };
    enum {
      /** one should attack king after when he captured almost all pieces */
      ProgressIndependentValueLimit = 4000
    };
    enum LoadStatus { Zero=0, Loaded, Random };
    static volatile LoadStatus initialized_flag;
    static Weights piece_pair_weights;
    typedef osl::NewProgress progress_t;
    progress_t progress;
    MultiIntPair kingx_blocked, king25_effect_each;
    MultiIntPair king25_both_side,king_rook_bishop;
    std::array<MultiIntPair, 2> non_pawn_attacked;
    MultiIntPair piece_stand_turn,
      non_pawn_attacked_ptype, piece_fork_turn;
    MultiIntPair king_piece;
    MultiIntPair ptype_count_value;
    MultiInt updatable_value;
    MultiInt ptypey, ptypex;
    MultiInt piece_stand_value, recalculated_stage_value, pawn_advance;
    MultiInt rook_mobility, bishop_mobility, lance_mobility;
    MultiInt knight_advance, pawn_drop, promoted_minor_piece, rook_pawn,
      rook_effect, bishop_effect, bishop_head, king3pieces;
    MultiInt rook_promote_defense;
    MultiInt piece_stand_combination, piece_stand_y, knight_check,
      knight_head, pawn_ptypeo, lance_effect_piece,
      ptype_y_pawn_y, bishop_and_king, rook_silver_knight, bishop_silver_knight;
    std::array<BoardMask, 2> knight_fork_squares;
    std::array<PieceMask, 2> effected_mask;
    std::array<PieceMask, 2> effected_mask_for_attacked;
    // flat
    std::array<int,2> piece_pair_king_value;
    std::array<int, 2> non_pawn_stand_count;
    std::array<std::array<int, 3>, 2> gs_near_king_count;
    std::array<std::array<int, PTYPE_SIZE>, 2> ptype_count, ptype_board_count;
    int recalculated_value, piece_pair_value;
    mutable int cache;
    Player turn;
    unsigned int ptypeo_mask;
    std::array<bool, 2> can_check; // king is defense
  public:
    OpenMidEndingEval *previous;
  private:
    static const int ROUND_UP = 2;
    static int roundUp(int v)
    {
      return v & (~(ROUND_UP-1)); 
    }
    void updateGoldSilverNearKingOne(Player P, const NumEffectState &state){
      gs_near_king_count[I(P)].fill(0);
      for(Square gold : state.allSquare(P, GOLD)){
	int y_diff = std::abs(Y(gold) - Y(state.kingSquare(P)));
	int x_diff = std::abs(X(gold) - X(state.kingSquare(P)));
	if (y_diff <= 2 && x_diff <= 3)
	  ++gs_near_king_count[I(P)][std::max(x_diff, y_diff) - 1];
      }
      for(Square silver : state.allSquare(P, SILVER)){
	int y_diff = std::abs(Y(silver) - Y(state.kingSquare(P)));
	int x_diff = std::abs(X(silver) - X(state.kingSquare(P)));
	if (y_diff <= 2 && x_diff <= 3)
	  ++gs_near_king_count[I(P)][std::max(x_diff, y_diff) - 1];
      }
    }
    void updateGoldSilverNearKing(const NumEffectState &state)
    {
      for(Player P : COLORS) updateGoldSilverNearKingOne(P, state);
    }
  public:
    explicit OpenMidEndingEval
    (const NumEffectState &state
      );
    OpenMidEndingEval& operator=(const OpenMidEndingEval& src)
    {
      if (this != &src)
	memcpy(this, &src, sizeof(OpenMidEndingEval));
      return *this;
    }
    void changeTurn() { turn = alt(turn); }
    static bool initialized()
    {
      return initialized_flag;
    }
    static bool setUp(const char *filename);
    static bool setUp();
    static std::string defaultFilename();
    int progressIndependentValue() const 
    {
      return 
	recalculated_value + piece_pair_value
	+ piece_pair_king_value[I(BLACK)] + piece_pair_king_value[I(WHITE)];
    }
    void debug() const;
    MultiInt stageValue() const 
    {
      return 
	king_piece[I(BLACK)] + king_piece[I(WHITE)] +
	piece_stand_value +
	king25_effect_each[I(BLACK)] + king25_effect_each[I(WHITE)] +
	ptypex + ptypey + rook_mobility + bishop_mobility + lance_mobility +
	rook_effect + bishop_effect +
	piece_stand_combination + piece_stand_turn[I(turn)] +
	rook_pawn + pawn_drop + piece_stand_y + knight_check +
	pawn_advance + pawn_ptypeo + promoted_minor_piece +
	non_pawn_attacked[I(turn)][0] + non_pawn_attacked[I(alt(turn))][1] +
	non_pawn_attacked_ptype[I(turn)] +
	king3pieces + bishop_head + knight_head
	+ rook_promote_defense +
	ptype_count_value[0] + ptype_count_value[1] +
	updatable_value +
	lance_effect_piece + ptype_y_pawn_y +
	bishop_and_king + piece_fork_turn[I(turn)] + rook_silver_knight + bishop_silver_knight +
	recalculated_stage_value;
    }
    void invalidateCache() { cache=INVALID; }
    int composeOpenMid2Endgame() const
    {
      const int progress_max = NewProgress::maxProgress();
      const int progress_val = this->progress.progress();
      const int c0 = progress_max/3, c1 = c0*2;
#ifndef NDEBUG
      const int w2 = progress_max - c1;
#endif
      assert(c0 == w2);
      int progress_independent = progressIndependentValue();
      int sum = progress_independent * c0;
      const MultiInt stage_sum = stageValue();
      if (progress_val < c0) 
      {
	sum += stage_sum[0] * (c0 - progress_val);
	sum += stage_sum[1] * progress_val;
      }
      else if (progress_val < c1) 
      {
	sum += stage_sum[1] * (c1 - progress_val);
	sum += stage_sum[2] * (progress_val-c0);
      }
      else 
      {
	sum += stage_sum[2] * (progress_max - progress_val);
	sum += stage_sum[3] * (progress_val - c1);
      }
      return sum;
    }
    int value() const
    {
      if (cache==INVALID) 
      {
	cache = roundUp(composeOpenMid2Endgame());
      }
      return cache;
    }
    template<Player P>
    void updateSub(const NumEffectState &new_state, Move last_move);
    void update(const NumEffectState &new_state, Move last_move);
    int progressValue() const { return progress.progress(); }
    int progressMax() const { return progress.maxProgress(); }
  public:
    static int infty()
    {
      assert(NewProgress::maxProgress() % 3 == 0);
      return 57984 * (NewProgress::maxProgress()/3);
    }
    static int seeScale() {
      assert(NewProgress::maxProgress() % 3 == 0);
      return (NewProgress::maxProgress()/3);
    }

    OpenMidEndingEvalDebugInfo debugInfo(const NumEffectState &state);

    static void resetWeights(const int *w, size_t length);
//	static OpenMidEndingPtypeTable Piece_Value;
    std::ostream& show(std::ostream& os) const;
  private:
    template <class Reader>
    static void doResetWeights(Reader& reader);
  };
  std::ostream& operator<<(std::ostream& os,OpenMidEndingEval const& eval);
  class PieceValues;

}


#endif /* OSL_EVAL_H */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
