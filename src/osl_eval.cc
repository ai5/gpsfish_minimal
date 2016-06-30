#include "osl_types.h"
#include "osl_position.h"
#include "osl_eval.h"
#include "misc.h"
#include <iostream>
#include <algorithm>
#include <mutex>
#include <fstream>

using osl::MultiInt;
using osl::MultiIntPair;

namespace osl
{
#define INT_READER(CLASS_NAME,SIZE)\
  class CLASS_NAME\
  {\
  public:\
    enum { DIM = SIZE };\
    typedef std::array<int, SIZE> table_t;\
    static table_t table;\
    static void setUp(const Weights &weights){\
      for(size_t i = 0; i < SIZE; i++) \
	table[i] = weights.value(i);\
    }\
  };\
  CLASS_NAME::table_t CLASS_NAME::table;

  INT_READER(PieceEval, 16)
  INT_READER(King25EffectAttack, 17 * 128)
#undef INT_READER

#define MULTI_INT_READER_STAGE(CLASS_NAME,SIZE)\
  class CLASS_NAME\
  {\
  public:\
    enum { DIM = SIZE };\
    typedef std::array<MultiInt, SIZE> table_t;\
    static table_t table;\
    static void setUp(const Weights &weights,int stage){\
      for(size_t i = 0; i < SIZE; i++) \
	table[i][stage] = weights.value(i);\
    }\
  };\
  CLASS_NAME::table_t CLASS_NAME::table;

  MULTI_INT_READER_STAGE(PawnDrop, 18)
  MULTI_INT_READER_STAGE(PawnDropY, 81 * 2)
  MULTI_INT_READER_STAGE(KingXBlockedBoth, 100)
  MULTI_INT_READER_STAGE(AnagumaEmpty, 4)
  MULTI_INT_READER_STAGE(GoldRetreat, 9)
  MULTI_INT_READER_STAGE(SilverRetreat, 9)
  MULTI_INT_READER_STAGE(KingPieceRelative, 4284)
  MULTI_INT_READER_STAGE(AllMajor, 1)
  MULTI_INT_READER_STAGE(AllGold, 1)
  MULTI_INT_READER_STAGE(King25EffectDefense, 17 * 128)
#undef MULTI_INT_READER_STAGE

#define MULTI_INT_READER(CLASS_NAME,SIZE)\
  class CLASS_NAME\
  {\
  public:\
    enum { DIM = SIZE * NStages };\
    typedef std::array<MultiInt, SIZE> table_t;\
    static table_t table;\
    static void setUp(const Weights &weights){\
      for(size_t i = 0; i < SIZE; i++) \
        for(int s = 0; s < NStages; s++)\
	  table[i][s] = weights.value(i + SIZE * s);\
    }\
  };\
  CLASS_NAME::table_t CLASS_NAME::table;

  MULTI_INT_READER(PawnDropX, 90)
  MULTI_INT_READER(PawnDropPawnStand, 18)
  MULTI_INT_READER(PawnDropPawnStandX, 90)
  MULTI_INT_READER(PawnDropPawnStandY, 162)
  MULTI_INT_READER(PawnDropNonDrop, 10)
  MULTI_INT_READER(PawnStateKingRelative, 36)
  
  MULTI_INT_READER(KingXBothBlocked, 5)
  MULTI_INT_READER(KingXBothBlockedY, 45)
  MULTI_INT_READER(KingXBlocked3, 80)
  MULTI_INT_READER(KingXBlocked3Y, 720)
  MULTI_INT_READER(GoldKnightKingRelative, 153)
  MULTI_INT_READER(GoldSideMove, 14)
  MULTI_INT_READER(SilverHeadPawnKingRelative, 153)
  MULTI_INT_READER(KingPieceRelativeNoSupport, 4284)
  MULTI_INT_READER(Promotion37, 16)
  MULTI_INT_READER(PtypeCount, 160)
  MULTI_INT_READER(PtypeCountXY, 2240)
  MULTI_INT_READER(PtypeCountXYAttack, 2240)
  MULTI_INT_READER(PinPtype, 80)
  MULTI_INT_READER(PinPtypeDistance, 560)
  MULTI_INT_READER(PinPtypePawnAttack, 48)
  MULTI_INT_READER(PtypeYY, 2592)
  MULTI_INT_READER(King25EffectSupported, 17 * 17)
#undef MULTI_INT_READER

  class King8All
  {
  public:
    static void setUp();
    static MultiIntPair eval(const NumEffectState &state);
    static MultiInt evalOne(Player P, const NumEffectState &state);
    static void
    evalWithUpdateBang(const NumEffectState &new_state, Move last_move,
		       MultiIntPair& last_values_and_out);
  private:
//    static std::array<MultiInt, 90> table;
    static std::array<MultiInt, 45> table2;
    static std::array<MultiInt, 720> table3;
    static_assert((M(UL)|M(U)|M(UR)|M(L)|(M(R)>>3)) <= 32, "table size missmatch");
    static std::array<MultiInt, 32> table4;

    static bool isBlocked(Player P,const NumEffectState &state,
			  bool is_l);
    static int index2(Player P, Square king);
    static int index3(Player P, const Square king, bool is_l,
		      bool u_blocked, bool opp_u_blocked,
		      bool opp_blocked);
  };

  class BishopRookFork
  {
  public:
    enum { 
      DROP_DIM = PTYPE_SIZE*PTYPE_SIZE, 
      ONE_DIM = 2*DROP_DIM*2, DIM = ONE_DIM * EvalStages 
    };
    static void setUp(const Weights &weights);
    static MultiIntPair evalOne(Player Defense, const NumEffectState& state, PieceMask target);
    static MultiIntPair eval(const NumEffectState& state);
  private:
    static std::array<MultiInt, ONE_DIM> table;
    static bool isBishopForkSquare(const NumEffectState& state, Player defense, const Square a, const Square b);
    static bool isRookForkSquare(const NumEffectState& state, Player defense, const Square a, const Square b);
    static int bishopIndex(Ptype a, Ptype b) { return I(a) * PTYPE_SIZE + I(b); }
    static int rookIndex(Ptype a, Ptype b) { return bishopIndex(a,b) + DROP_DIM; }

    static bool findDropInLine(const NumEffectState& state, Player defense, 
			       const Square a, const Square b);
    static bool testCenter(const NumEffectState& state, Player defense, 
			   const Square a, const Square b, Square center);
  };

  class KingPiece
  {
  public:
    static void setUp();
    static MultiIntPair eval(const NumEffectState &state);
    static void evalOne(Player P, const NumEffectState &state, MultiIntPair& results);
    template<Player P>
    static void evalWithUpdate(const NumEffectState &state, Move last_move,
			       MultiIntPair &values);
    static void adjustSupport(Player P, const NumEffectState &state, PieceMask except_mask,  MultiIntPair &values);
    static std::array<MultiInt, 14 * 17 * 9 * 4> table;
    static std::array<MultiInt, 2592> yy_table;
    // P == player(king), alt(P) == player(p)
    static int index(Player P, Square king, Piece p, bool supported){
      Square sq = square(p);
      Ptype pt = ptype(p);
      int x_index = std::abs(X(sq) - X(king));
      int y_index = (P == WHITE ? Y(king) - Y(sq) : Y(sq) - Y(king)) + 8;
      return ((PI(pt) * 9 + x_index) * 17 + y_index) * 2 + (supported ? 0 : 1);
    }
    static int dindex(Player P, Square king, Piece p, bool supported){
      assert(owner(p) == P);
      return index(alt(P), king, p, supported) + 4284;
    }
    static int yyindex(Player P, Square king, Piece p){
      Square sq = square(p);
      Ptype pt = ptype(p);
      int king_y = (P == BLACK ? Y(king) : 10 - Y(king));
      int piece_y = (P == BLACK ? Y(sq) : 10 - Y(sq));
      return (king_y - 1) * 9 * 32 + (piece_y - 1) * 32 + I(pt);
    }
    static int d_yyindex(Player P, Square king, Piece p){
      Square sq = square(p);
      Ptype pt = ptype(p);
      int king_y = (P == BLACK ? Y(king) : 10 - Y(king));
      int piece_y = (P == BLACK ? Y(sq) : 10 - Y(sq));
      return (king_y - 1) * 9 * 32 + (piece_y - 1) * 32 + I(pt) + 16;
    }
    // owner(king) == P, owner(p) == P
    static void add_defense(Player P, Square king, Piece p, bool supported, MultiIntPair& values){
      assert(owner(p) == P);
      values[I(P)].addFor(P, table[dindex(P, king, p, supported)]);
      values[I(P)].addFor(P, yy_table[d_yyindex(P, king, p)]);
    }
    // owner(king) == P, owner(p) == P
    static void sub_defense(Player P, Square king, Piece p, bool supported, MultiIntPair& values){
      assert(owner(p) == P);
      values[I(P)].subFor(P, table[dindex(P, king, p, supported)]);
      values[I(P)].subFor(P, yy_table[d_yyindex(P, king, p)]);
    }
    static void sub_defense(Player P, NumEffectState const& state, Square king, Square sq, MultiIntPair& values){
      Piece p = state[sq];
      assert(owner(p) == P);
      bool supported = state.hasEffect(P, sq);
      sub_defense(P, king, p, supported, values);

    }
    static void add_d_support(Player P, Square king, Piece p, MultiIntPair& values){
      assert(owner(p) == P);
      values[I(P)].addFor(P, table[dindex(P, king, p, true)] - table[dindex(P, king, p, false)]);
    }
    static void sub_d_support(Player P, Square king, Piece p, MultiIntPair& values){
      assert(owner(p) == P);
      values[I(P)].addFor(P, table[dindex(P, king, p, false)] - table[dindex(P, king, p, true)]);
    }
    // owner(king) == P, owner(p) == alt(P)
    static void add_attack(Player P, Square king, Piece p, bool supported, MultiIntPair& values){
      assert(owner(p) == alt(P));
      values[I(P)].addFor(P, table[index(P, king, p, supported)]);
      values[I(P)].addFor(P, yy_table[yyindex(P, king, p)]);
    }
    // owner(king) == P, owner(p) == alt(P)
    static void sub_attack(Player P, Square king, Piece p, bool supported, MultiIntPair& values){
      assert(owner(p) == alt(P));
      values[I(P)].subFor(P, table[index(P, king, p, supported)]);
      values[I(P)].subFor(P, yy_table[yyindex(P, king, p)]);
    }
    static void add_a_support(Player P, Square king, Piece p, MultiIntPair& values){
      assert(owner(p) == alt(P));
      values[I(P)].addFor(P, table[index(P, king, p, true)] - table[index(P, king, p, false)]);
    }
    static void sub_a_support(Player P, Square king, Piece p, MultiIntPair& values){
      assert(owner(p) == alt(P));
      values[I(P)].addFor(P, table[index(P, king, p, false)] - table[index(P, king, p, true)]);
    }
  };
  std::array<MultiInt, 14 * 17 * 9 * 4> KingPiece::table;
  std::array<MultiInt, 2592> KingPiece::yy_table;

  void KingPiece::setUp(){
    for (int i = 0; i < 2142; ++i){
      int pt = 2 + (i / 17 / 9); // , c0 = (NewProgress::maxProgress() / 3);
      MultiInt val;
      for(int j = 0; j < 4; j++) val[j] = (pt == 8 ? 0 : PieceEval::table[pt]);
      table[i * 2] = -KingPieceRelative::table[i];
      table[i * 2 + 1] = -(KingPieceRelative::table[i] + KingPieceRelativeNoSupport::table[i]);
      table[i * 2 + 4284] = val + KingPieceRelative::table[i + 2142];
      table[i * 2 + 4285] = val + KingPieceRelative::table[i + 2142] + KingPieceRelativeNoSupport::table[i + 2142];
    }
    yy_table = PtypeYY::table;
  }

  void KingPiece::evalOne(Player P, const NumEffectState &state, MultiIntPair& values){
    Square king = state.kingSquare(P);
    PieceMask d_pieces = state.piecesOnBoard(P) & ~pieceMask(KING);
    PieceMask d_pieces_supported = d_pieces & state.effectedMask(P);
    for(Piece p : state.allPiece(d_pieces_supported))
      add_defense(P, king, p, true, values);
    for(Piece p : state.allPiece(d_pieces - d_pieces_supported))
      add_defense(P, king, p, false, values);
    PieceMask a_pieces = state.piecesOnBoard(alt(P)) & ~pieceMask(KING);
    PieceMask a_pieces_supported = a_pieces & state.effectedMask(alt(P));
    for(Piece p : state.allPiece(a_pieces_supported))
      add_attack(P, king, p, true, values);
    for(Piece p : state.allPiece(a_pieces - a_pieces_supported))
      add_attack(P, king, p, false, values);
  }
  MultiIntPair KingPiece::eval(const NumEffectState &state)
  {
    MultiIntPair values;
    for(Player P : COLORS) evalOne(P, state, values);
    return values;
  }
  void KingPiece::adjustSupport(Player P, const NumEffectState &state, PieceMask except_mask,
      MultiIntPair &values){
    Square my_king = state.kingSquare(P);
    PieceMask both_d = state.piecesOnBoard(P) & state.previous->piecesOnBoard(P)& ~(pieceMask(KING) + except_mask);
    PieceMask add_d = both_d & state.effectedMask(P) & ~state.previous->effectedMask(P);
    for(Piece p : state.allPiece(add_d))
      add_d_support(P, my_king, p, values);
    PieceMask sub_d = both_d & ~state.effectedMask(P) & state.previous->effectedMask(P);
    for(Piece p : state.allPiece(sub_d))
      sub_d_support(P, my_king, p, values);

    PieceMask both_a = state.piecesOnBoard(alt(P)) & state.previous->piecesOnBoard(alt(P)) & ~(pieceMask(KING) + except_mask);
    PieceMask add_a = both_a & state.effectedMask(alt(P)) & ~state.previous->effectedMask(alt(P));
    for(Piece p : state.allPiece(add_a))
      add_a_support(P, my_king, p, values);
    PieceMask sub_a = both_a & ~state.effectedMask(alt(P)) & state.previous->effectedMask(alt(P));
    for(Piece p : state.allPiece(sub_a))
      sub_a_support(P, my_king, p, values);
  }

  template<Player P>
  void KingPiece::evalWithUpdate(const NumEffectState &state, Move last_move,
 				 MultiIntPair &values){
    Square my_king = state.kingSquare(P);
    Square op_king = state.kingSquare(alt(P));
    PieceMask except_mask = PieceMask(0);
    if(oldPtype(last_move) == KING){
      values[I(P)].clear();
      evalOne(P, state, values);
      if(isCapture(last_move))
	sub_defense(alt(P), *state.previous, op_king, to(last_move), values);
    }
    else{
      Square to_sq = to(last_move);
      Piece to_piece = state[to_sq];
      bool to_supported = state.hasEffect(P, to_sq);
      add_defense(P, my_king, to_piece, to_supported, values);
      add_attack(alt(P), op_king, to_piece, to_supported, values);
      if(!isDrop(last_move)){
        Square from_sq = from(last_move);
	Piece from_piece = (*state.previous)[from_sq];
	bool from_supported = state.previous->hasEffect(P, from_sq);
	sub_defense(P, my_king, from_piece, from_supported, values);
	sub_attack(alt(P), op_king, from_piece, from_supported, values);
	set(except_mask, number(from_piece));
        if(isCapture(last_move)){
    	  Piece captured_piece = (*state.previous)[to_sq];
	  bool captured_supported = state.previous->hasEffect(alt(P), to_sq);
	  sub_attack(P, my_king, captured_piece, captured_supported, values);
	  sub_defense(alt(P), op_king, captured_piece, captured_supported, values);
        }
      }
      adjustSupport(P, state, except_mask, values);
    }
    adjustSupport(alt(P), state, except_mask, values);
  }

  class King25Effect3Y
  {
  public:
    enum { ONE_DIM = 21600, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class King25Effect3
  {
    friend class King25Effect3Y;
  public:
    enum { ONE_DIM = 2400, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<PieceMask, 2> &king25_mask);
  private:
    static int index(int piece_count, bool with_knight,
		     int stand_count, bool with_knight_on_stand,
		     int attacked_count)
    {
      assert(piece_count >= 0 && piece_count <= 9);
      assert(stand_count >= 0 && stand_count <= 9);
      assert(attacked_count >= 0 && attacked_count <= 5);
      return (piece_count + 10 *
	      ((with_knight ? 1 : 0) + 2 *
	       (stand_count + 10 * ((with_knight_on_stand ? 1 : 0) +
				    2 * attacked_count))));
    }
    static int indexY(int piece_count, bool with_knight,
		      int stand_count, bool with_knight_on_stand,
		      int attacked_count, int king_y)
    {
      assert(piece_count >= 0 && piece_count <= 9);
      assert(stand_count >= 0 && stand_count <= 9);
      assert(attacked_count >= 0 && attacked_count <= 5);
      return ((piece_count + 10 *
	       ((with_knight ? 1 : 0) + 2 *
		(stand_count + 10 * ((with_knight_on_stand ? 1 : 0) +
				     2 * attacked_count))))) * 9 +
	king_y - 1;
    }
    template <osl::Player Attack>
    static MultiInt evalOne(const NumEffectState &state,
			    PieceMask king25);
    static std::array<MultiInt, 2400> table;
    static std::array<MultiInt, 21600> y_table;
  };


  class PiecePairKing
  {
  public:
    enum
    {
      ONE_DIM = 1488375,
      DIM = ONE_DIM
    };
    /* in use */
    static void setUp(const Weights &weights);
    static std::array<int,2> eval(const NumEffectState&);
    template <Player P>
    static void evalWithUpdateBang(const NumEffectState& state, Move moved, std::array<int,2>& last_value);

    template <Player King>
    static int evalOne(const NumEffectState&);
  private:
    template <Player King>
    static int add(const NumEffectState& state, Square to, Ptype ptype);
    template <Player King>
    static int sub(const NumEffectState& state, Square from, Ptype ptype);
    template <Player King>
    static int addSub(const NumEffectState& state, Square to, Ptype ptype, Square from);
    static int composeIndex(int king, int i0, int i1)
    {
      return king + i0*45*7 + i1;
    }
    static int indexWhite(Square p)
    {
      return X(p)-1 + (Y(p)-1)*9;
    }
    static int indexKing(Player owner, Square king, bool& flipx)
    {
      if (owner == BLACK)
	king = rotate180(king);
      assert(Y(king) <= 3);
      if (X(king) > 5)
      {
	king = flipHorizontal(king);
	flipx = true;
      }
      else
	flipx = false;
      return (X(king)-1 + (Y(king)-1)*5)*45*7*45*7;
    }
    template <bool FlipX>
    static int indexPiece(Player owner, Square position, Ptype ptype)
    {
      assert(! isPromoted(ptype));
      if (owner == BLACK)
	position = rotate180(position);
      if (FlipX)
	position = flipHorizontal(position);
      assert(Y(position) <= 5);
      return indexWhite(position)*7 + BI(ptype)-1;
    }
    static std::array<int16_t, ONE_DIM> table;
  };

  class King25EffectYAttack
  {
  private:
    static int index(int king_y, int effect, int piece_count)
    {
      return effect + 128 * piece_count + (king_y - 1) * 128 * 17;
    }
  public:
    enum { DIM = 17 * 128 * 9};
    static std::array<int, 17 * 128 * 9> table;
    /* in use */
    static void setUp(const Weights &weights)
    {
      for (size_t i = 0; i < weights.dimension(); ++i)
      {
	table[i] = weights.value(i);
      }
      for(int king_y = 1; king_y <= 9; king_y++)
	for(int piece_count = 0; piece_count < 17; piece_count++)
	  for(int effect = 0; effect < 128; effect++){
	    table[effect + 128 * piece_count + (king_y - 1) * 128 * 17] +=
	      King25EffectAttack::table[effect + 128 * piece_count];
	  }
    }
    static int eval(const NumEffectState &state,
		    int black_effect, int black_piece,
		    int white_effect, int white_piece)
    {
      // ugly hack.  -1 is attack.  0 >= is defense
      return table[index(10 - Y(state.kingSquare(WHITE)),
			 black_effect, black_piece)] -
	table[index(Y(state.kingSquare(BLACK)),
		    white_effect, white_piece)];
    }
  };

  class King25EffectYDefense
  {
  private:
    static int index(int king_y, int effect, int piece_count)
    {
      return effect + 128 * piece_count + (king_y - 1) * 128 * 17;
    }
  public:
    enum { DIM = 17 * 128 * 9};
    static std::array<MultiInt, 17 * 128 * 9> table;
    /* in use */
    static void setUp(const Weights &weights,int stage)
    {
      for (size_t i = 0; i < weights.dimension(); ++i)
      {
	table[i][stage] = weights.value(i);
      }
      for(int king_y = 1; king_y <= 9; king_y++)
	for(int piece_count = 0; piece_count < 17; piece_count++)
	  for(int effect = 0; effect < 128; effect++){
	    table[effect + 128 * piece_count + (king_y - 1) * 128 * 17][stage] +=
	      King25EffectDefense::table[effect + 128 * piece_count][stage];
	  }
    }
    static MultiInt eval(const NumEffectState &state,
			 int black_effect, int black_piece,
			 int white_effect, int white_piece)
    {
      return table[index(Y(state.kingSquare(BLACK)),
			 black_effect, black_piece)] -
	table[index(10 - Y(state.kingSquare(WHITE)),
		    white_effect, white_piece)];
    }
  };

  struct King25EffectSupportedY
  {
    enum { ONE_DIM = 17 * 17 * 9, DIM = ONE_DIM * EvalStages };
    static int index(int piece_count, int supported, int y)
    {
      return (supported * 17 + piece_count) * 9 + y - 1;
    }
    static std::array<MultiInt, ONE_DIM> table;
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(
      int black_attack_piece,
      int white_attack_piece,
      int black_attack_supported_piece, int white_attack_supported_piece,
      int black_king_y, int white_king_y)
    {
      const int black_index = index(black_attack_piece,
				    black_attack_supported_piece,
				    10 - white_king_y);
      const int white_index = index(white_attack_piece,
				    white_attack_supported_piece,
				    black_king_y);
      return table[black_index] - table[white_index];
    }
  };
  enum EffectState
  {
    NO_ATTACK_DEFENSE_0,
    NO_ATTACK_DEFENSE_1,
    NO_ATTACK_DEFENSE_2,
    ATTACK_DIFF_N2,
    ATTACK_DIFF_N1,
    ATTACK_DIFF_0,
    ATTACK_DIFF_1,
    ATTACK_DIFF_2,
    STATE_MAX, // 8
  };

  class King25EffectEachBoth
  {
    enum EffectState
    {
      NO_ATTACK_DEFENSE_0,
      NO_ATTACK_DEFENSE_1,
      NO_ATTACK_DEFENSE_2,
      ATTACK_DIFF_N2,
      ATTACK_DIFF_N1,
      ATTACK_DIFF_0,
      ATTACK_DIFF_1,
      ATTACK_DIFF_2,
      STATE_MAX, // 8
    };
    friend class King25EffectEachXY;
    friend class King25EffectEachKXY;
  public:
    enum { DIM = 5 * 3 * 8 * 3 };
    static void setUp(const Weights &weights, int stage){
      for(size_t i = 0; i < DIM; i++) 
	table[i][stage] = weights.value(i);
    }
  private:
    static std::array<MultiInt, 5 * 3 * 8 * 3> table;
    static std::array<MultiInt, 3000> x_table;
    static std::array<MultiInt, 3240> y_table;
    static std::array<MultiInt, 27000> xy_table;
    static std::array<int, 256> effect_state_table;
    template <Player Defense>
    static int effectStateIndex3(const NumEffectState &state,
				 Square target);
    template <Player Defense>
    static void index(const NumEffectState &state, 
		      Square target,
		      int &index_xy,
		      int rel_y, int king_x, int king_y, int x_diff
      );
    template <osl::Player Defense>
    static void evalOne(const NumEffectState &state,
			MultiInt& out);
  public:
    static void eval(const NumEffectState &state,
		     MultiIntPair &out);
    static void
    evalWithUpdate(const NumEffectState &state, Move last_move,
		   MultiIntPair & values);
  };

  class King25EffectEachXY
  {
  public:
    enum { X_DIM = 3000, Y_DIM = 3240, DIM = (X_DIM + Y_DIM) * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
  };

  class King25EffectEachKXY
  {
  public:
    enum { ONE_DIM = 27000, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weigths);
  };

  class King3Pieces
  {
    friend class King3PiecesXY;
  public:
    enum { ONE_DIM = 3072, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
    static MultiInt evalWithUpdate(const NumEffectState &state,
				   Move last_move,
				   MultiInt &last_value);
  private:
    enum Direction
    {
      HORIZONTAL = 0,
      VERTICAL,
      DIAGONAL,
    };
    template <Player King, Direction Dir>
    static int index(PtypeO p1, PtypeO p2)
    {
      if (King == WHITE)
      {
	p1 = altIfPiece(p1);
	p2 = altIfPiece(p2);
      }
      return I(p1) * 32 + I(p2) + 1024 * Dir;
    }
    template <Player King, Direction Dir>
    static int indexY(const Square king_position,
		      PtypeO p1, PtypeO p2)
    {
      if (King == WHITE)
      {
	p1 = altIfPiece(p1);
	p2 = altIfPiece(p2);
      }
      const int king_y = (King == BLACK ? Y(king_position) :
			  10 - Y(king_position));
      return I(p1) * 32 + I(p2) + 1024 * Dir
	+ (king_y - 1) * 32 * 32 * 3;
    }
    template <Player King, Direction Dir>
    static int indexX(const Square king_position,
		      PtypeO p1, PtypeO p2)
    {
      if (King == WHITE)
      {
	p1 = altIfPiece(p1);
	p2 = altIfPiece(p2);
      }
      const int king_x = (X(king_position) > 5 ? 10 - X(king_position) :
			  X(king_position));
      if (Dir == HORIZONTAL &&
	  ((King == BLACK && X(king_position) >= 6) ||
	   (King == WHITE && X(king_position) <= 4)))
      {
	PtypeO tmp = p1;
	p1 = p2; p2 = tmp;
      }
      return I(p1) * 32 + I(p2) + 1024 * Dir
	+ (king_x - 1) * 32 * 32 * 3;
    }
    static MultiInt value(int vertical_index, int horizontal_index,
			  int diagonal_index1, int diagonal_index2,
			  int vertical_index_x,  int horizontal_index_x,
			  int diagonal_index1_x, int diagonal_index2_x,
			  int vertical_index_y , int horizontal_index_y,
			  int diagonal_index1_y, int diagonal_index2_y) 
    {
      return table[vertical_index] + table[horizontal_index] +
	table[diagonal_index1] + table[diagonal_index2] +
	x_table[vertical_index_x] + x_table[horizontal_index_x] +
	x_table[diagonal_index1_x] + x_table[diagonal_index2_x] +
	y_table[vertical_index_y] + y_table[horizontal_index_y] +
	y_table[diagonal_index1_y] + y_table[diagonal_index2_y];
    }
	
    template <Player King>
    static void evalOne(const NumEffectState &state,
			MultiInt &result);
    static std::array<MultiInt, 3072> table;
    static std::array<MultiInt, 15360> x_table;
    static std::array<MultiInt, 27648> y_table;
  };

  class King3PiecesXY
  {
  public:
    enum
    {
      X_DIM = 32 * 32 * 3 * 5,
      Y_DIM = 32 * 32 * 3 * 9,
      ONE_DIM = X_DIM + Y_DIM,
      DIM = ONE_DIM * EvalStages,
    };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class KingMobilityVal
  {
    friend class KingMobilityWithRook;
    friend class KingMobilityWithBishop;
  public:
    enum { ONE_DIM = 3240, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    template <Player P>
    static MultiInt evalOne(const NumEffectState &state);
    template<Direction Dir>
    static int mobilityDir(Square king,Square target)
    {
      if(Dir==L) return X(king)-X(target)-1;
      else if(Dir==R) return X(target)-X(king)-1;
      else if(Dir==UL || Dir==U || Dir==UR) return Y(target)-Y(king)-1;
      else return Y(king)-Y(target)-1;
    }
    static std::array<MultiInt, 3240> table;
    static std::array<MultiInt, 3240> rook_table;
    static std::array<MultiInt, 3240> bishop_table;
    static std::array<MultiInt, 3240> rook_bishop_table;
  };

  class KingMobilityWithRook
  {
  public:
    enum { ONE_DIM = 3240, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class KingMobilityWithBishop
  {
  public:
    enum { ONE_DIM = 3240, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class KingMobilitySum
  {
  public:
    enum { ONE_DIM = 2925, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    template <Player P>
    static MultiInt evalOne(const NumEffectState &state);
    static std::array<MultiInt, 45*33> table;
  };

  class King25BothSide
  {
    friend class King25BothSideX;
    friend class King25BothSideY;
  public:
    enum { ONE_DIM = 8192, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    template<Player P>
    static MultiInt evalOne(const NumEffectState &state,
			    const std::array<uint8_t, 8> &effects
);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<uint8_t, 8> &black,
			 const std::array<uint8_t, 8> &white
);
  private:
    static int index(int effect1, int effect2, int i)
    {
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return effect1 + 32 * (effect2 + 32 * i);
    }
    template <Player P>
    static int indexX(Square king, int effect1, int effect2,
		      int i, int j)
    {
      const int king_x = (X(king) >= 6 ? 10 - X(king) : X(king));
      if ((P == BLACK && X(king) > 5) ||
	  (P == WHITE && X(king) < 5))
      {
	const int tmp = effect1;
	effect1 = effect2;
	effect2 = tmp;
	const int tmp2 = i;
	i = 4 - j;
	j = 4 - tmp2;
      }
      if (i == 2)
	--j;
      const int combination = (i * 3 + j - 2);
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return king_x - 1 + 5 * (effect1 + 32 *
			       (effect2 + 32 * combination));
    }
    static int indexX(int king_x,int effect1,int effect2, int i){
      return king_x - 1 + 5 * (effect1 + 32 *
			       (effect2 + 32 * i));
    }
    template <Player P>
    static int indexY(Square king, int effect1, int effect2, int i)
    {
      const int king_y = (P == BLACK ? Y(king) : 10 - Y(king));
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return king_y - 1 + 9 *(effect1 + 32 * (effect2 + 32 * i));
    }
    static int indexY(int king_y,int effect1,int effect2, int i){
      return king_y - 1 + 9 *(effect1 + 32 * (effect2 + 32 * i));
    }
    static std::array<MultiInt, 8192> table;
    static std::array<MultiInt, 40960> x_table;
    static std::array<MultiInt, 73728> y_table;
  };
  class King25BothSideX
  {
  public:
    enum { ONE_DIM = 40960, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class King25BothSideY
  {
  public:
    enum { ONE_DIM = 73728, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class King25Mobility
  {
    friend class King25MobilityX;
    friend class King25MobilityY;
  public:
    enum { ONE_DIM = 4096, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<uint8_t, 8> &black,
			 const std::array<uint8_t, 8> &white
);
  private:
    static int index(int effect1, int effect2, int i)
    {
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return effect1 + 32 * (effect2 + 32 * i);
    }
    template <Player Defense>
    static int indexX(Square king, int effect1, int effect2, int i)
    {
      const int king_x = (X(king) > 5 ? 10 - X(king) : X(king));
      if ((Defense == BLACK && X(king) > 5) ||
	  (Defense == WHITE && X(king) < 5))
      {
	const int tmp = effect1;
	effect1 = effect2;
	effect2 = tmp;
	i = 3 - i;
      }
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return king_x - 1 + 5 * (effect1 + 32 * (effect2 + 32 * i));
    }
    template <Player Defense>
    static int indexY(Square king, int effect1, int effect2, int i)
    {
      const int king_y = (Defense == BLACK ? Y(king) : 10 - Y(king));
      assert(0 <= effect1 && effect1 < 32);
      assert(0 <= effect2 && effect2 < 32);
      return king_y - 1 + 9 * (effect1 + 32 * (effect2 + 32 * i));
    }
    static std::array<MultiInt, 4096> table;
    static std::array<MultiInt, 20480> x_table;
    static std::array<MultiInt, 36864> y_table;
  };
  class King25MobilityX
  {
  public:
    enum { ONE_DIM = 20480, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class King25MobilityY
  {
  public:
    enum { ONE_DIM = 36864, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class King25EffectCountCombination
  {
    friend class King25EffectCountCombinationY;
  public:
    enum { ONE_DIM = 100, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<PieceMask, 2> &king25);
  private:
    template <osl::Player Attack>
    static MultiInt evalOne(const NumEffectState &state,
			    PieceMask king25);
    static std::array<MultiInt, 100> table;
    static std::array<MultiInt, 900> y_table;
  };
  class King25EffectCountCombinationY
  {
  public:
    enum { ONE_DIM = 900, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class BishopExchangeSilverKing
  {
    static int indexKing(Square king) 
    {
      const int y = Y(king);
      if (y >= 3)
	return -1;
      return (y-1)*9 + X(king)-1;
    }
    static int indexRook(Square rook) 
    {
      assert(isOnBoard(rook));
      const int y = Y(rook);
      if (y >= 6)
	return -1;
      return (y-1)*9 + X(rook)-1;
    }
    static int indexSilver(Square silver) 
    {
      return (Y(silver)-1)*9 + X(silver)-1;
    }
  public:
    enum { BISHOP_ONE_DIM = 18 * 81 * (45*2), DIM = BISHOP_ONE_DIM*3 };
    /* in use */
    static void setUp(const Weights &weights);
    static int eval(const NumEffectState& state);
  private:
    template <Player KingOwner>
    static int evalOne(const NumEffectState &state, int offset);
    static std::array<int, DIM> table;
  };

#ifdef BISHOP_IN_DANGER
  class BishopInDanger
  {
  public:
    static bool bishopCanMove(Player P, Square bishop_sq, const NumEffectState &state){
      const Player altP = alt(P);
      Piece p = state.pieceAt(bishop_sq);
      if (isPromoted(p)){
	for(Direction d : ROOK_DIRECTIONS){
	  Square sq = bishop_sq + blackOffset(d);
	  Piece p1 = state.pieceAt(sq);
	  if(isOnBoard(sq) && canMoveOn(P, p1) &&
	     (!state.hasEffect(altP, sq) || unpromoteSafe(getPtype(ptypeO(p1))) == ROOK || 
	      unpromoteSafe(getPtype(ptypeO(p1))) == BISHOP)) return true;
	}
      }
      int num = number(p);
      for(Direction d : BISHOP_DIRECTIONS){
	Offset o = blackOffset(d);
	Square lim = state.mobilityOf(d, num);
	for(Square sq = bishop_sq + o; sq != lim; sq += o){
	  if(!state.hasEffect(altP, sq)) return true;
	}
	Piece p1 = state.pieceAt(lim);
	if(isOnBoard(lim) && (canMoveOn(altP, p1) || 
			     (canMoveOn(P, p1) &&
			      (!state.hasEffect(altP, lim) || unpromoteSafe(getPtype(ptypeO(p1))) == ROOK || 
			       unpromoteSafe(getPtype(ptypeO(p1))) == BISHOP)))) return true;
      }
      return false;
    }
    static int eval(const NumEffectState &state){
      int r = 0;
      for(Player P : COLORS){
        if(any(state.standMask(P))) continue;
	for(Square bishop_sq : state.allSquare(P, BISHOP)){
	  if(!canPromote(P, bishop_sq)) continue;
	  if(bishopCanMove(P, bishop_sq, state)) continue;
	  r += valueFor(P, -300);
	}
      }
      return r;
    }
  };
  class RookInDanger
  {
  public:
    static bool rookCanMove(Player P, Square rook_sq, const NumEffectState &state){
      const Player altP = alt(P);
      Piece p = state.pieceAt(rook_sq);
      if (isPromoted(p)){
	for(Direction d : BISHOP_DIRECTIONS){
	  Square sq = rook_sq + blackOffset(d);
	  Piece p1 = state.pieceAt(sq);
	  if(isOnBoard(sq) && canMoveOn(P, p1) &&
	     (!state.hasEffect(altP, sq) || unpromoteSafe(getPtype(ptypeO(p1))) == ROOK)) return true;
	}
      }
      int num = number(p);
      for(Direction d : ROOK_DIRECTIONS){
	Offset o = blackOffset(d);
	Square lim = state.mobilityOf(d, num);
	for(Square sq = rook_sq + o; sq != lim; sq += o){
	  if(!state.hasEffect(altP, sq)) return true;
	}
	Piece p1 = state.pieceAt(lim);
	if(isOnBoard(lim) && (canMoveOn(altP, p1) || 
			     (canMoveOn(P, p1) &&
			      (!state.hasEffect(altP, lim) || unpromoteSafe(getPtype(ptypeO(p1))) == ROOK)))) return true;
      }
      return false;
    }
    static int eval(const NumEffectState &state){
      int r = 0;
      for(Player P : COLORS){
//        if(any(state.standMask(P))) continue;
	for(Square rook_sq : state.allSquare(P, ROOK)){
	  if(!canPromote(P, rook_sq)) continue;
	  if(rookCanMove(P, rook_sq, state)) continue;
	  r += valueFor(P, -200);
	}
      }
      return r;
    }
  };
#endif

  class EnterKingDefense
  {
  public:
    enum { DIM = (8+8+8+8)*3 };
    /* in use */
    static void setUp(const Weights &weights);
    static int eval(const NumEffectState &state);
  private:
    template <Player KingOwner>
    static int evalOne(const NumEffectState &state);
    static std::array<int, DIM> table;
  };

  class PawnDropAll
  {
  private:
    enum { BOTH_ON_BOARD, SELF_ON_BOARD, OPP_ON_BOARD, BOTH_ON_STAND };
    static std::array<MultiInt, 81> attack_y_table, defense_y_table;
    static std::array<MultiInt, 90> x_table;
    static std::array<MultiInt, 90> x_stand_table;
    static std::array<MultiInt, 162> y_stand_table;
    static std::array<MultiInt, 81 * 4> xx_table;
    static std::array<MultiInt, 9 * 17 * 4> yy_table;
    static std::array<MultiInt, 9> drop_non_drop_table;
    static std::array<MultiInt, 36> state_king_relative_table;
    static int indexY(Player Owner, Square king, int x);
    static int indexX(Player Owner, bool Attack, Square king, int x);
  public:
    static void setUp();
    static MultiInt value(
      int attack_index_y, int defense_index_y,
      int attack_index_x, int defense_index_x);
    static MultiInt standValue(
      int attack_index_y, int defense_index_y,
      int attack_index_x, int defense_index_x);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(const NumEffectState &state,
				   Move moved,
				   MultiInt &last_value);
  };

  class NoPawnOnStand
  {
  public:
    enum { DIM = 1 };
  private:
    static MultiInt weight;
  public:
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state, int black_pawn_count)
    {
      if (black_pawn_count > 9 && !state.hasPieceOnStand(WHITE, PAWN))
	return -weight;
      else if (black_pawn_count < 9 && !state.hasPieceOnStand(BLACK, PAWN))
	return weight;

      return MultiInt();
    }
  };

  struct PawnAdvanceUtil
  {
    static int index(Player P, Square pos)
    {
      return (P == BLACK ? (Y(pos) - 1) : (9 - Y(pos)));
    }
    static bool cantAdvance(const NumEffectState &state, const Piece pawn)
    {
      return cantAdvance(state, osl::ptypeO(pawn), square(pawn));
    }
    static bool cantAdvance(const NumEffectState &state,
			    const PtypeO ptypeO, const Square position)
    {
      assert(getPtype(ptypeO) == PAWN);
      return isOnBoardByOwner(getOwner(ptypeO),
			      state.pieceAt(nextSquare(getOwner(ptypeO),position,U)));
    }
  };
  struct PawnAdvanceAll : public PawnAdvanceUtil
  {
    template <osl::Player P> 
    static void adjust(int index, MultiInt& values);
    template<Player P>
    static void evalWithUpdateBang(const NumEffectState &state, Move moved,
				   MultiInt& last_value);
  };

  class PawnAdvance : public PawnAdvanceUtil
  {
  public:
    enum { DIM = 9 };
  private:
    static std::array<MultiInt, 9> table;
    friend struct PawnAdvanceAll;
  public:
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
  };

  class SilverFeatures
  {
  public:
    static void setUp(){
      head_table = SilverHeadPawnKingRelative::table;
      retreat_table = SilverRetreat::table;
    }
    static MultiInt eval(const NumEffectState &state){
      MultiInt result;
      for(Player P : COLORS)
	for(Square silver : state.allSquareStrict(P, SILVER))
	  result.addFor(P, evalOne(P, state, silver));
      return result;
    }
  protected:
    static int indexRetreat(Player P, Square pos)
    {
      return (P == BLACK ? (Y(pos) - 1) : (9 - Y(pos)));
    }
    static bool canRetreat(Player P, const NumEffectState &state,
			   Square silver){
      Square dr = nextSquare(P, silver, DR);
      Square dl = nextSquare(P, silver, DL);
      return (canMoveOn(P, state[dr]) && !state.hasEffect(alt(P), dr)) ||
	(canMoveOn(P, state[dl]) && !state.hasEffect(alt(P), dl));
    }
    static MultiInt evalOne(Player P, const NumEffectState &state,
			    Square silver)
    {
      MultiInt result;
      if (!canRetreat(P, state,silver))
	result = retreat_table[indexRetreat(P, silver)];
      const Square up =	nextSquare(P, silver, U);
      if (isOnBoard(up)){
	if (isEmpty(state[up]) &&
	    (state.hasEffectStrict(alt(P), up, PAWN) ||
	     !state.isPawnMaskSet(alt(P), X(silver))))
	{
	  Square king = state.kingSquare(P);
	  const int x_diff = std::abs(X(king) - X(silver));
	  const int y_diff = (P == BLACK ?
			      Y(silver) - Y(king) :
			      Y(king) - Y(silver));
	  result += head_table[x_diff + 9 * (y_diff + 8)];
	}
      }
      return result;
    }
    static std::array<MultiInt, 153> head_table;
    static std::array<MultiInt, 9> retreat_table;
  };
  std::array<osl::MultiInt, 153> SilverFeatures::head_table;
  std::array<osl::MultiInt, 9> SilverFeatures::retreat_table;


  class GoldFeatures
  {
  public:
    static void setUp(){
      knight_table = GoldKnightKingRelative::table;
      retreat_table = GoldRetreat::table;
      side_table = GoldSideMove::table;
    }
    static MultiInt eval(const NumEffectState &state){
      MultiInt result;
      for(Player P : COLORS)
	for(Square gold : state.allSquare(P, GOLD))
	  result.addFor(P, evalOne(P, state, gold));
      return result;
    }
  protected:
    static int indexRetreat(Player P, Square pos)
    {
      return (P == BLACK ? (Y(pos) - 1) : (9 - Y(pos)));
    }
    static int indexSideX(Square pos)
    {
      return (X(pos) > 5 ? 9 - X(pos) : X(pos) - 1);
    }
    static int indexSideY(Player P, Square pos)
    {
      return (P == BLACK ? (Y(pos) - 1) : (9 - Y(pos))) + 5;
    }
    static bool canRetreat(Player P, const NumEffectState &state,
			   Square gold){
      Square d = nextSquare(P, gold, D);
      return canMoveOn(P, state[d]) && !state.hasEffect(alt(P), d);
    }
    static bool canMoveToSide(Player P, const NumEffectState &state,
			      Square gold)
    {
      Square r = nextSquare(P, gold, R);
      Square l = nextSquare(P, gold, L);
      return (canMoveOn(P, state[r]) && !state.hasEffect(alt(P), r)) ||
	(canMoveOn(P, state[l]) && !state.hasEffect(alt(P), l));
    }
    static MultiInt evalOne(Player P, const NumEffectState &state,
			    Square gold)
    {
      MultiInt result;
      if (!canRetreat(P, state, gold))
	result += retreat_table[indexRetreat(P, gold)];
      if (!canMoveToSide(P, state, gold))
	result += side_table[indexSideX(gold)] + side_table[indexSideY(P, gold)];
      const Square uur = nextSquare(P, gold, UUR);
      const Square uul = nextSquare(P, gold, UUL);
      if ((isEmpty(state.pieceAt(uul)) && !state.hasEffect(P, uul))
	  || (isEmpty(state.pieceAt(uur)) && !state.hasEffect(P, uur)))
      {
	const Square king = state.kingSquare(P);
	const int x_diff = std::abs(X(king) - X(gold));
	const int y_diff = (P == BLACK ?
			    Y(gold) - Y(king) :
			    Y(king) - Y(gold));
	result += knight_table[x_diff + 9 * (y_diff + 8)];
      }
      return result;
    }
    static std::array<MultiInt, 153> knight_table;
    static std::array<MultiInt, 9> retreat_table;
    static std::array<MultiInt, 14> side_table;
  };
  std::array<osl::MultiInt, 153> GoldFeatures::knight_table;
  std::array<osl::MultiInt, 9> GoldFeatures::retreat_table;
  std::array<osl::MultiInt, 14> GoldFeatures::side_table;


  class KnightAdvance
  {
  public:
    enum { DIM = 9 };
  private:
    static std::array<MultiInt, 9> table;
    static int index(Player P, Square pos)
    {
      return (P == BLACK ? (Y(pos) - 1) : (9 - Y(pos)));
    }
    template<Player P>
    static bool cantAdvance(const NumEffectState &state,
			    const Piece knight);
  public:
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
  };


  class PtypeY
  {
  public:
    enum { DIM = PTYPE_SIZE * 9 };
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(const NumEffectState &, Move moved,
				   MultiInt const& last_value);
  private:
    static std::array<MultiInt, 144> table;
    static int index(const Piece piece)
    {
      return index(owner(piece), ptype(piece), square(piece));
    }
    static int index(const Player player, const Ptype ptype, const Square pos)
    {
      const int y = (player == BLACK ? Y(pos) : 10 - Y(pos)) - 1;
      return I(ptype) * 9 + y;
    }
  };

  class PtypeX
  {
  public:
    enum { DIM = PTYPE_SIZE * 5 };
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(const NumEffectState &, Move moved,
				   MultiInt const& last_value);
  private:
    static std::array<MultiInt, 80> table;
    static int index(const Piece piece)
    {
      return index(owner(piece), ptype(piece), square(piece));
    }
    static int index(const Player, const Ptype ptype, const Square pos)
    {
      const int x = (X(pos) > 5 ? 10 - X(pos) : X(pos)) - 1;
      return I(ptype) * 5 + x;
    }
  };

  class KnightCheck
  {
    friend class KnightCheckY;
  public:
    enum { DIM = 1 };
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
    template <Player Defense>
    static bool canCheck(const NumEffectState &state)
    {
      const Square king = state.kingSquare(Defense);
      constexpr Player offense = alt(Defense);
      const Square ul =
	king + newOffset(Defense,UUL);
      const Square ur =
	king + newOffset(Defense,UUR);
      if (isOnBoard(ul))
      {
	const Piece p = state.pieceAt(ul);
	if (!state.hasEffect(Defense, ul) &&
	    ((isEmpty(p) && state.hasPieceOnStand(offense, KNIGHT)) ||
	     (!isOnBoardByOwner(offense,p) &&
	      state.hasEffectStrict(offense, ul, KNIGHT))))
	  return true;
      }
      if (isOnBoard(ur))
      {
	const Piece p = state.pieceAt(ur);
	if (!state.hasEffect(Defense, ur) &&
	    ((isEmpty(p) && state.hasPieceOnStand(offense, KNIGHT)) ||
	     (!isOnBoardByOwner(offense,p) &&
	      state.hasEffectStrict(offense, ur, KNIGHT))))
	  return true;
      }
      return false;
    }
    static MultiInt value(int index_y) { return weight + y_table[index_y]; }
  private:
    static MultiInt weight;
    template <Player King>
    static int indexY(int y) 
    {
      return (King == BLACK ? y - 1 : 9 - y) ;
    }
    static std::array<MultiInt, 9> y_table;
  };

  class KnightCheckY
  {
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class KnightHead
  {
    friend class KnightHeadOppPiecePawnOnStand;
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, 9> table;
    static std::array<MultiInt, 144> opp_table;
  };

  class KnightHeadOppPiecePawnOnStand
  {
  public:
    enum { ONE_DIM = 9 * 16, DIM = ONE_DIM * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
  private:
  };

  class PawnPtypeOPtypeO
  {
    friend class PawnPtypeOPtypeOY;
  public:
    enum { ONE_DIM = 1024, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(const NumEffectState &state,
				   Move moved,
				   const MultiInt &last_value);
  private:
    static int index(Player P, PtypeO up, PtypeO up_up)
    {
      if (P == WHITE)
      {
	up = altIfPiece(up);
	up_up = altIfPiece(up_up);
      }
      return I(up) * 32 + I(up_up);
    }
    static int indexY(Player P, PtypeO up, PtypeO up_up, int y)
    {
      const int y_index = (P == BLACK ? y - 1 : 9 - y);
      return index(P, up, up_up) + 1024 * y_index;
    }
    static std::array<MultiInt, 1024> table;
    static std::array<MultiInt, 9216> y_table;
  };

  class PromotedMinorPieces
  {
    friend class PromotedMinorPiecesY;
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
    static MultiInt evalWithUpdate(
      const NumEffectState &state,
      Move moved,
      const MultiInt &last_values);
    template <int Sign>
    static void adjust(int index, int index_attack, int index_defense,
		       MultiInt &result);
  private:
    template <Player P>
    static void evalOne(const NumEffectState &state,
			const PieceMask promoted,
			MultiInt &result);
    template <bool attack, Player owner>
    static int indexY(const Square king, int x_diff)
    {
      const int y = (owner == BLACK ? Y(king) : 10 - Y(king));
      return x_diff + (y - 1) * 9 + (attack ? 0 : 81);
    }
    static std::array<MultiInt, 9> table;
    static std::array<MultiInt, 162> y_table;
  };
  class NonPawnAttackedKingRelative
  {
  public:
    enum { ONE_DIM = 19584, DIM = ONE_DIM * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
  };
  class NonPawnAttacked
  {
    friend class NonPawnAttackedKingRelative;
  public:
    enum { ONE_DIM = 64, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static void eval(const NumEffectState &state, std::array<MultiIntPair, 2>& out);
    static void update(Player P, const NumEffectState &state, Move last_move, std::array<MultiIntPair, 2>& out);

  private:
    static void evalOne(Player P, const NumEffectState &state, MultiIntPair& out);
    static void updateOne(Player MovePlayer, Player King, const NumEffectState &state, Move last_move, MultiIntPair& out);
    static void updateBoth(Player MovePlayer, const NumEffectState &state, Move last_move, std::array<MultiIntPair, 2>& out);
    static int indexNew(Player King, Player P, Square king, Piece piece, bool has_support)
    {
      Square position = square(piece);
      int x_diff = std::abs(X(position) - X(king));
      int y_diff = (P == BLACK ? Y(position) - Y(king) : Y(king) - Y(position));
      return ((I(ptype(piece)) + (has_support ? 0 : PTYPE_SIZE)) * 9 + x_diff) * 17 +
	      y_diff + 8 + (King != P ? 0 : 4896 );
    }

    static std::array<MultiInt, 64> table;
    static std::array<MultiIntPair, 9792> king_table;
  };
  std::array<MultiInt, 64> osl::NonPawnAttacked::table;
  std::array<MultiIntPair, 9792> osl::NonPawnAttacked::king_table;
  void osl::NonPawnAttacked::setUp(const Weights &weights)
  {
    for (size_t i = 0; i < ONE_DIM; ++i)
    {
      for (int s=0; s<NStages; ++s)
	table[i][s] = weights.value(i + ONE_DIM*s);
    }
  }
  void osl::NonPawnAttackedKingRelative::setUp(
    const Weights &weights)
  {
      std::array<MultiInt, 19584> table;
      for (size_t i = 0; i < ONE_DIM; ++i)
      {
      for (int s=0; s<NStages; ++s)
	table[i][s] = weights.value(i + ONE_DIM*s);
    }
      for(int x_diff=0;x_diff<9;x_diff++)
	for(int y_diff= -8;y_diff<=8;y_diff++)
	  for(int has_support=0;has_support<2;has_support++)
	    for(int same_turn=0;same_turn<2;same_turn++)
	      for(int ptype=0;ptype<PTYPE_SIZE;ptype++){
      int index=((ptype + (same_turn ? 0 : PTYPE_SIZE) +
	(has_support ? 0 : PTYPE_SIZE*2))* 9 + x_diff) * 17 +
	y_diff + 8;
      int index0=ptype + (same_turn ? 0 : PTYPE_SIZE) +
	(has_support ? 0 : PTYPE_SIZE * 2);
      table[index] = -(table[index] + NonPawnAttacked::table[index0]);
    }
      for(int x_diff=0;x_diff<9;x_diff++)
	for(int y_diff= -8;y_diff<=8;y_diff++)
	  for(int has_support=0;has_support<2;has_support++)
	    for(int attack_defense = 0; attack_defense < 2; attack_defense++)
	      for(int king_turn=0;king_turn<2;king_turn++)
		for(int ptype=0;ptype<PTYPE_SIZE;ptype++){
      bool piece_turn = (king_turn == 0 ? attack_defense == 1 : attack_defense == 0);
      int index0 = ((ptype + (piece_turn ? 0 : PTYPE_SIZE) +
	(has_support ? 0 : PTYPE_SIZE*2))* 9 + x_diff) * 17 +
	y_diff + 8 + attack_defense * 9792;
      int index = ((ptype + (has_support ? 0 : PTYPE_SIZE))* 9 + x_diff) * 17 +
	y_diff + 8 + attack_defense * 4896;
      NonPawnAttacked::king_table[index][king_turn] = table[index0];
    }
    }


  void osl::
  NonPawnAttacked::evalOne(Player King, const NumEffectState &state, MultiIntPair& result)
  {
    Square king = state.kingSquare(King);
    for(Player P : COLORS){
      PieceMask attacked = state.attackedNotPawnKing(P);
      PieceMask with_support = state.effectedMask(P) & attacked;
      for(Piece p : state.allPiece(with_support)){
        int index = indexNew(King, P, king, p, true);
        result.addFor(King, king_table[index]);
      }
      PieceMask without_support = attacked - with_support;
      for(Piece p : state.allPiece(without_support)) {
        int index = indexNew(King, P, king, p, false);
        result.addFor(King, king_table[index]);
      }
    }
  }
  void osl::
  NonPawnAttacked::eval(const NumEffectState &state, std::array<MultiIntPair, 2>& result)
  {
    for(Player King : COLORS){
      result[I(King)]=MultiIntPair();
      evalOne(King, state, result[I(King)]);
    }
  }
  void osl::
    NonPawnAttacked::updateOne(Player MovePlayer, Player King, const NumEffectState &state, Move last_move, MultiIntPair& result)
  {
    Square king = state.kingSquare(King);
    for(Player P : COLORS){
      PieceMask attacked = state.attackedNotPawnKing(P);
      PieceMask old_attacked = state.previous->attackedNotPawnKing(P);
      PieceMask both = attacked & old_attacked;
      for(Piece p : state.previous->allPiece(old_attacked - both)){
        bool supported = state.previous->hasEffect(P, square(p));
        int old_index = indexNew(King, P, king, p, supported);
        result.subFor(King, king_table[old_index]);
      }
      for(Piece p : state.allPiece(attacked - both)){
        int index = indexNew(King, P, king, p, state.hasEffect(P, square(p)));
        result.addFor(King, king_table[index]);
      }
      PieceMask changed = both & (state.effectedMask(P) ^ state.previous->effectedMask(P));
      if(MovePlayer == P && !isDrop(last_move)){
        Piece p = state[to(last_move)];
        PieceMask m = newPieceMask(number(p));
	if(any(both & m)){
          Piece old_p = (*state.previous)[from(last_move)];
          int old_index = indexNew(King, P, king, old_p, state.previous->hasEffect(P, from(last_move)));
          result.subFor(King, king_table[old_index]);
          int index = indexNew(King, P, king, p, state.hasEffect(P, to(last_move)));
          result.addFor(King, king_table[index]);
          changed &= ~m;
        }
      }
      PieceMask support_added = changed & state.effectedMask(P);
      for(Piece p : state.allPiece(support_added)){
        int old_index = indexNew(King, P, king, p, false);
        result.subFor(King, king_table[old_index]);
        int index = indexNew(King, P, king, p, true);
        result.addFor(King, king_table[index]);
      }
      for(Piece p : state.allPiece(changed - support_added)){
        int old_index = indexNew(King, P, king, p, true);
        result.subFor(King, king_table[old_index]);
        int index = indexNew(King, P, king, p, false);
        result.addFor(King, king_table[index]);
      }
    }
  }

  void osl::
    NonPawnAttacked::updateBoth(Player MovePlayer, const NumEffectState &state, Move last_move, std::array<MultiIntPair, 2>& result)
  {
    for(Player P : COLORS){
      PieceMask attacked = state.attackedNotPawnKing(P);
      PieceMask old_attacked = state.previous->attackedNotPawnKing(P);
      PieceMask both = attacked & old_attacked;
      for(Piece p : state.previous->allPiece(old_attacked - both)){
        bool supported = state.previous->hasEffect(P, square(p));
	for(Player King : COLORS){
          Square king = state.kingSquare(King);
          int old_index = indexNew(King, P, king, p, supported);
          result[I(King)].subFor(King, king_table[old_index]);
        }
      }
      for(Piece p : state.allPiece(attacked - both)){
        bool supported = state.hasEffect(P, square(p));
	for(Player King : COLORS){
          Square king = state.kingSquare(King);
          int index = indexNew(King, P, king, p, supported);
          result[I(King)].addFor(King, king_table[index]);
        }
      }
      PieceMask changed = both & (state.effectedMask(P) ^ state.previous->effectedMask(P));
      if(MovePlayer == P && !isDrop(last_move)){
        Piece p = state[to(last_move)];
        PieceMask m = newPieceMask(number(p));
	if(any(both & m)){
          Piece old_p = (*state.previous)[from(last_move)];
	  for(Player King : COLORS){
            Square king = state.kingSquare(King);
            int old_index = indexNew(King, P, king, old_p, state.previous->hasEffect(P, from(last_move)));
            int index = indexNew(King, P, king, p, state.hasEffect(P, to(last_move)));
            result[I(King)].addFor(King, king_table[index] - king_table[old_index]);
          }
          changed &= ~m;
        }
      }
      PieceMask support_added = changed & state.effectedMask(P);
      for(Piece p : state.allPiece(support_added)){
        for(Player King : COLORS){
            Square king = state.kingSquare(King);
          int old_index = indexNew(King, P, king, p, false);
//          result.subFor(King, king_table[old_index]);
          int index = indexNew(King, P, king, p, true);
          result[I(King)].addFor(King, king_table[index] - king_table[old_index]);
        }
      }
      for(Piece p : state.allPiece(changed - support_added)){
	for(Player King : COLORS){
            Square king = state.kingSquare(King);
        int old_index = indexNew(King, P, king, p, true);
//        result.subFor(King, king_table[old_index]);
        int index = indexNew(King, P, king, p, false);
        result[I(King)].addFor(King, king_table[index]-king_table[old_index]);
        }     
 }
    }
  }
  void NonPawnAttacked::update(Player P, const NumEffectState &state, Move last_move, std::array<MultiIntPair, 2>& result)
  {
    if(oldPtype(last_move) == KING){
      result[I(P)] = MultiIntPair();
      evalOne(P, state, result[I(P)]);
      updateOne(P, alt(P), state, last_move, result[I(alt(P))]);    
    }
    else
      updateBoth(P, state, last_move, result);
  }

  class PromotedMinorPiecesY
  {
  public:
    enum { ONE_DIM = 162, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class PawnPtypeOPtypeOY
  {
  public:
    enum { ONE_DIM = 9216, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  class NonPawnAttackedPtype
  {
  public:
    enum { ONE_DIM = 1024, DIM = ONE_DIM * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
    static void eval(const NumEffectState &state, MultiIntPair& out);
  private:
    static int index(bool has_support, Ptype ptype,
		     Ptype attack_ptype)
    {
      return (I(ptype) +
	      (has_support ? 0 : PTYPE_SIZE)) * 16 + I(attack_ptype);
    }
    static std::array<MultiIntPair, 512> table;
  };
  std::array<MultiIntPair, 512> osl::NonPawnAttackedPtype::table;
  void osl::
    NonPawnAttackedPtype::setUp(const Weights &weights)
  {
    std::array<MultiInt, 1024> tmp;
   for (size_t i = 0; i < ONE_DIM; ++i)
      {
      for (int s=0; s<NStages; ++s)
	tmp[i][s] = weights.value(i + ONE_DIM*s);
    }
    for(Ptype ptype = Ptype::MIN; ptype <= Ptype::MAX; ptype++)    
      for(Ptype a_ptype = Ptype::MIN; a_ptype <= Ptype::MAX; a_ptype++)
	for(int has_support = 0; has_support < 2; has_support++)
	  for(int same_turn = 0; same_turn < 2; same_turn ++){
            int old_index = (I(ptype) + same_turn * PTYPE_SIZE + has_support * PTYPE_SIZE * 2) * 16 + I(a_ptype);
            int index = (I(ptype) + has_support * PTYPE_SIZE) * 16 + I(a_ptype);
            table[index][same_turn] = tmp[old_index];
          }

  }

  void osl::
    NonPawnAttackedPtype::eval(const NumEffectState &state,
	MultiIntPair &result)
  {
    result = MultiIntPair();
    for(Player P : COLORS){
      PieceMask attacked = state.attackedNotPawnKing(P);
      PieceMask with_support = attacked & state.effectedMask(P);
      for(Piece p : state.allPiece(with_support)){
        PieceMask attacking = state.effect(alt(P), p);
        for(Piece attack : state.allPiece(attacking)){
          int i =  index(true, ptype(p), ptype(attack));
          result[0].addFor(P, table[i][P == BLACK ? 0 : 1]);
          result[1].addFor(P, table[i][P == BLACK ? 1 : 0]);
        }
      }
      for(Piece p : state.allPiece(attacked - with_support)){
        PieceMask attacking = state.effect(alt(P), p);
        for(Piece attack : state.allPiece(attacking)){
          int i =  index(false, ptype(p), ptype(attack));
          result[0].addFor(P, table[i][P == BLACK ? 0 : 1]);
          result[1].addFor(P, table[i][P == BLACK ? 1 : 0]);
        }
      }
    }
  }

  class NonPawnAttackedPtypePair
  {
  public:
    enum {
      ONE_DIM = (PTYPE_SIZE * 2 * PTYPE_SIZE)*(PTYPE_SIZE * 2 * PTYPE_SIZE),
      DIM = ONE_DIM * EvalStages,     
    };
    /* in use */
    static void setUp(const Weights &weights);
    static int index1(const NumEffectState &state, Piece piece)
    {
      const Ptype attack_ptype
	= ptype(state.findCheapAttack(alt(owner(piece)), square(piece)));
      const bool has_support = state.hasEffect(owner(piece),
						 square(piece));
      return (I(ptype(piece)) + 
	      (has_support ? 0 : PTYPE_SIZE)) * PTYPE_SIZE + I(attack_ptype);
    }
    static int index2(int i0, int i1) 
    {
      return i0 * PTYPE_SIZE * 2 * PTYPE_SIZE + i1;
    }	  
    static std::array<MultiInt, ONE_DIM> table;
  };      

  class PtypeCountAll
  {
  public:
    /* in use */
    static void setUp();
    static void evalOne(Player P, const NumEffectState &state,
		     const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
		     const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
		     MultiInt &out);
    static void eval(const NumEffectState &state,
		     const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
		     const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
		     MultiIntPair &out, MultiInt& out1);
    template<Player P>
    static void evalWithUpdateBang(
      const NumEffectState &state,
      Move last_move,
      std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
      std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
      MultiIntPair &out, MultiInt &out1, unsigned int &ptypeo_mask);
  private:
    static int indexCount(Ptype ptype, int count)
    {
      return indexMin(unpromote(ptype)) +
	(isPromoted(ptype) ? 40 : 0) +
	count - 1;
    }
    static int indexBoardCount(Ptype ptype, int count)
    {
      return indexMin(unpromote(ptype)) +
	(isPromoted(ptype) ? 40 : 0) + 80 +
	count - 1;
    }
    static int indexCountX(Ptype ptype, int count, int x)
    {
      return x - 1 + 5 *
	(indexMin(unpromote(ptype)) +
	 (isPromoted(ptype) ? 40 : 0) +
	 count - 1);
    }
    static int indexCountY(Ptype ptype, int count, int y)
    {
      return y - 1 + 9 *
	(indexMin(unpromote(ptype)) +
	 (isPromoted(ptype) ? 40 : 0) +
	 count - 1) + 800;
    }
    static int indexBoardCountX(Ptype ptype, int count, int x)
    {
      return x - 1 + 5 *
	(indexMin(unpromote(ptype)) +
	 (isPromoted(ptype) ? 40 : 0) +
	 count - 1) + 400;
    }
    static int indexBoardCountY(Ptype ptype, int count, int y)
    {
      return y - 1 + 9 *
	(indexMin(unpromote(ptype)) +
	 (isPromoted(ptype) ? 40 : 0) +
	 count - 1) + 720 + 800;
    }
    static std::array<MultiInt, 160> table;
    static std::array<MultiInt, 2240> xy_table;
    static std::array<MultiInt, 2240> xy_attack_table;
    static std::array<MultiInt, 2240> xy_table_diff;
    static std::array<MultiInt, 2240> xy_attack_table_diff;
    static std::array<MultiInt, 81> king_yy;
    static MultiInt all_major, all_gold;
  };

  class LanceEffectPieceKingRelative
  {
  public:
    enum { ONE_DIM = 9792, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static int index(Player p, Square pos, Square king,
		     PtypeO ptypeO, bool attack)
    {
      const int y_diff = (p == BLACK ? Y(king) - Y(pos) : Y(pos) - Y(king));
      const int x_diff = std::abs(X(king) - X(pos));
      if (p == WHITE)
      {
	ptypeO = alt(ptypeO);
      }
      return y_diff + 8 + x_diff * 17 + I(ptypeO) * 17 * 9 +
	(attack ? 0 : 4896);
    }
    static std::array<MultiInt, 9792> table;
  };
  std::array<MultiInt, 9792> osl::LanceEffectPieceKingRelative::table;
  void osl::
  LanceEffectPieceKingRelative::setUp(const Weights &weights)
  {
    for (size_t i = 0; i < ONE_DIM; ++i)
      for (int s=0; s<NStages; ++s){
	table[i][s] = weights.value(i + ONE_DIM*s);
      }
    table[I(PtypeO::EDGE) * 17 * 9] += table[I(PtypeO::EDGE) * 17 * 9 + 4896];
  }

  MultiInt osl::
  LanceEffectPieceKingRelative::eval(const NumEffectState &state)
  {
    MultiInt result;
    for (Player P : COLORS){
      for(int num : state.allNum(state.piecesOnBoardStrict(P, LANCE))){
	Square sq = state.mobilityOf(P == BLACK ? U : D, num);
	if (!isOnBoard(sq))
	  result.addFor(P, table[I(PtypeO::EDGE) * 17 * 9]);
	else{
	  int index1 = index(P, sq, state.kingSquare(alt(P)), ptypeO(state[sq]), true);
	  int index2 = index(P, sq, state.kingSquare(P), ptypeO(state[sq]), false);
	  result.addFor(P, table[index1] + table[index2]);
	}
      }
    }
    return result;
  }

  class PtypeYPawnY
  {
  public:
    enum { ONE_DIM = 1440, DIM = ONE_DIM * EvalStages };
    static MultiInt eval(const NumEffectState &state
      );
    template<Player P>
    static void evalWithUpdateBang(const NumEffectState &state,
				   Move moved,
				   MultiInt& last_value);
    /* in use */
    static void setUp(const Weights &weights);
  private:
    static int index(Player player, Ptype ptype, int y, int pawn_y)
    {
      if (player == WHITE)
      {
	y = 10 - y;
	pawn_y = (pawn_y == 0 ? 0 : 10 - pawn_y);
      }
      return pawn_y + 10 * (y - 1 + 9 * I(ptype));
    }
    static std::array<MultiInt, 1440> table;
  };

  class GoldAndSilverNearKing
  {
    friend class GoldAndSilverNearKingCombination;
  public:
    enum { ONE_DIM = 1215, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<std::array<int, 3>, 2> &gs_count);
  private:
    template <Player Defense>
    static int index(const Square king, int distance0, int count)
    {
      int king_x = (X(king) > 5 ? 10 - X(king) : X(king));
      int king_y = (Defense == WHITE ? 10 - Y(king) : Y(king));
      return king_x - 1 + 5 * (king_y - 1+ 9 * (distance0 + 3 * count));
    }
    template <Player P>
    static MultiInt evalOne(const NumEffectState &state,
			    const std::array<std::array<int, 3>, 2> &gs_count);
    template <Player Defense>
    static int indexCombination(const Square king, int count0,
				int count1, int count2)
    {
      int king_x = (X(king) > 5 ? 10 - X(king) : X(king));
      int king_y = (Defense == WHITE ? 10 - Y(king) : Y(king));
      return king_x + 5 * (king_y + 9 * (std::min(5,count0) + 6 *
					 (std::min(5,count1) + 6 * std::min(5,count2))));
    }
    static std::array<MultiInt, 1215> table;
    static std::array<MultiInt, 9720> combination_table;
  };

  class GoldAndSilverNearKingCombination
  {
  public:
    enum { ONE_DIM = 9720, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  private:
  };

  class PtypeCombination
  {
  public:
    enum { ONE_DIM = 8192, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(unsigned int ptypeo_mask);
  private:
    template <Player P>
    static MultiInt evalOne(unsigned int ptypeo_mask)
    {
      int index = 0;
      if (P==BLACK) index=((ptypeo_mask>>19)&0x1fc0)|((ptypeo_mask>>18)&0x3f);
      else index=((ptypeo_mask>>3)&0x1fc0)|((ptypeo_mask>>2)&0x3f);
      if (P == BLACK)
	return table[index];
      else
	return -table[index];
    }
    static std::array<MultiInt, 8192> table;
  };
  class SilverFork
  {
  public:
    enum { ONE_DIM = 5*2, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiIntPair eval(const NumEffectState& state);
    static std::array<MultiInt, ONE_DIM> table;
  };
  class KnightFork
  {
  public:
    enum { 
      DROP_DIM = PTYPE_SIZE*PTYPE_SIZE, ONE_DIM = DROP_DIM*2*2, 
      DIM = ONE_DIM * EvalStages
    };
    /* in use */
    static void setUp(const Weights &weights);
    template <Player Defense>
    static MultiIntPair evalOne(const NumEffectState& state,
				bool has_knight,
				BoardMask& knight_fork_squares);
    static MultiIntPair eval(const NumEffectState& state, 
			     std::array<BoardMask,2>& knight_fork_squares);
    template <Player P>
    static MultiIntPair evalWithUpdate(const NumEffectState& state, 
				       Move moved,
				       std::array<BoardMask,2>& knight_fork_squares);
    static std::array<MultiInt, ONE_DIM> table;

    static bool isForkSquare(const NumEffectState& state, Player defense, 
			     int y, int x0, int x1);
    static int index(Ptype a, Ptype b)
    {
      return I(a) * PTYPE_SIZE + I(b);
    }
    static bool isTarget(Ptype ptype) 
    {
      ptype = unpromote(ptype);
      return ptype != PAWN && ptype != LANCE && ptype != KNIGHT;
    }
  private:
    template <Player P, Player Defense>
    static void updateSquares
    (const NumEffectState& state, Move moved,
     BoardMask& knight_fork_squares);
    template <osl::Player Defense>
    static MultiIntPair accumulate
    (const NumEffectState& state,
     bool has_knight, const BoardMask& knight_fork_squares);
  };

  class SilverAdvance26
  {
  public:
    enum { ONE_DIM = 1, DIM = ONE_DIM*EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, ONE_DIM> table;
  };

  class RookPawnY
  {
    friend class RookPawnYX;
  public:
    enum { ONE_DIM = 180, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state
      );
  private:
    static int index(const Piece rook, const int pawn_y)
    {
      const int rook_y =
	(owner(rook) == BLACK ? Y(square(rook)) : 10 - Y(square(rook)));
      return (rook_y - 1) * 10 + pawn_y + (isPromoted(rook) ? 90 : 0);
    }
    static int indexY(const Square king,
		      const Piece rook, int pawn_y)
    {
      const int x_diff = std::abs(X(square(rook)) - X(king));
      const int rook_y =
	(owner(rook) == BLACK ? Y(square(rook)) : 10 - Y(square(rook)));
      return x_diff * 10 * 9 + (rook_y - 1) * 10 + pawn_y + (isPromoted(rook) ? 810 : 0);
    }
    static std::array<MultiInt, 180> table;
    static std::array<MultiInt, 1620> y_attack_table;
    static std::array<MultiInt, 1620> y_defense_table;
  };

  class RookPawnYX
  {
  public:
    enum { ONE_DIM = 1620, DIM = ONE_DIM * 2*EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };


  class RookEffectBase
  {
    friend class RookEffectPiece;
  public:
    enum { ONE_DIM = 612, DIM = ONE_DIM * 2 };
    static MultiInt eval(const NumEffectState &state);
  protected:
    template<Player P>
    static MultiInt evalOne(const NumEffectState& state,
			    Square rook,
			    Square myKing,
			    Square opKing,
			    Square up,
			    Square dp,
			    Square rp,
			    Square lp,
			    bool isP);
    /**
     * (abs_x_diff, y_diff) - 邇峨ｒ蜴溽せ縺ｨ縺励◆譎ゅ・遨ｺ繝槭せ縺ｮ逶ｸ蟇ｾ菴咲ｽｮ
     * horizontal - 鬟幄ｻ翫・讓ｪ蛻ｩ縺阪°繧吶≠繧句ｴ蜷・
     * is_promoted - 遶懊・蝣ｴ蜷・
     */
    static int index(int abs_x_diff, int y_diff, bool horizontal, bool is_promoted)
    {
      return y_diff + 8 + abs_x_diff * 17 + (horizontal ? 153 : 0) +
	(is_promoted ? 306 : 0);
    }
    /**
     * 鮟偵・鬟幄ｻ・遶・縺九ｉ蛻ｩ縺阪・縺ゅｋ鬧・
     * (abs_x_diff, y_diff) - 鬧偵ｒ蝓ｺ貅悶↓縺励◆邇峨・逶ｸ蟇ｾ菴咲ｽｮ
     *                        abs_x_diff縺ｯ邨ｶ蟇ｾ蛟､
     * ptypeO - 鬧偵・ptypeO, 逋ｽ縺九ｉ縺ｮ蝣ｴ蜷医・蜿崎ｻ｢
     * horizontal - 鬟幄ｻ翫・讓ｪ蛻ｩ縺阪°繧吶≠繧句ｴ蜷・
     * is_promoted - 遶懊・蝣ｴ蜷・
     */
    static int index0(int abs_x_diff,int y_diff,
		      PtypeO ptypeO,
		      bool horizontal, bool promoted){
      return y_diff+8+abs_x_diff*17+I(ptypeO) * 17 * 9 +
	(horizontal ? 4896 : 0) + (promoted ? 9792 : 0);
    }
    /**
     * 鮟偵・鬟幄ｻ・遶・縺九ｉ縺ｮ蛻ｩ縺阪・縺ゅｋ鬧偵・index
     * {attack,defense}_{u,r,l,d} 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ縺ｫ菴ｿ縺・
     * from - 鬧偵・菴咲ｽｮ
     * king - 邇峨・菴咲ｽｮ
     * ptypeO - 鬧偵・遞ｮ鬘橸ｼ檎區縺九ｉ縺ｮ蛻ｩ縺阪・蝣ｴ蜷医・蜿崎ｻ｢・・
     *          (BLACK,PTYPE_Ptype::EDGE)繧ゅ≠繧雁ｾ励ｋ
     * isP - 遶懊・蝣ｴ蜷・
     */
    static int index1(Square king,Square from,PtypeO ptypeO,bool isP)
    {
      int y_diff=Y(from)-Y(king);
      int x_diff=X(from)-X(king);
      return index1(x_diff,y_diff,ptypeO,isP);
    }
    /**
     * 鮟偵・鬟幄ｻ・遶・縺九ｉ縺ｮ蛻ｩ縺阪・縺ゅｋ鬧偵・index
     * {attack,defense}_{u,r,l,d} 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ縺ｫ菴ｿ縺・
     * (x_diff, y_diff) - 邇峨ｒ蝓ｺ貅悶↓隕九◆鬧偵・逶ｸ蟇ｾ菴咲ｽｮ
     * ptypeO - 鬧偵・遞ｮ鬘橸ｼ檎區縺九ｉ縺ｮ蛻ｩ縺阪・蝣ｴ蜷医・蜿崎ｻ｢・・
     *          (BLACK,PTYPE_Ptype::EDGE)繧ゅ≠繧雁ｾ励ｋ
     * isP - 遶懊・蝣ｴ蜷・
     */
    static int index1(int x_diff,int y_diff,PtypeO ptypeO,bool isP){
      assert(-9 <= y_diff && y_diff <= 9);
      assert(-9 <= x_diff && x_diff <= 9);
      assert(getPtype((PtypeO)ptypeO)!=Ptype::EMPTY);
      int index=I(ptypeO)+32*((y_diff+9)+19*(x_diff+9+19*(isP ? 1 : 0)));
      assert(0<=index && index<32*19*19*2);
      return index;
    }
    /**
     * 鮟偵・鬟幄ｻ・遶・縺九ｙ縺ゅｋ蝣ｴ謇縺ｯ遨ｺ繝槭せ縺ｦ繧吶↑縺・・縺ｦ繧吶◎縺ｮ蛻・ｒ陬懈ｭ｣縺吶ｋ繝・・繝輔ｙ繝ｫ
     * {attack,defense}_nospace 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ
     * king - 邇峨・菴咲ｽｮ
     * from - 鬟幄ｻ・遶・縺ｮ菴咲ｽｮ
     * isP - 遶懊・蝣ｴ蜷・
     */
    static int index2(Square king,Square from,bool isP)
    {
      int y_diff=Y(from)-Y(king);
      int x_diff=X(from)-X(king);
      return index2(x_diff,y_diff,isP);
    }
    /**
     * 鮟偵・鬟幄ｻ・遶・縺九ｙ縺ゅｋ蝣ｴ謇縺ｯ遨ｺ繝槭せ縺ｦ繧吶↑縺・・縺ｦ繧吶◎縺ｮ蛻・ｒ陬懈ｭ｣縺吶ｋ繝・・繝輔ｙ繝ｫ
     * {attack,defense}_nospace 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ
     * (x_diff, y_diff)  - 邇峨ｒ蝓ｺ貅悶↓縺励※縺ｿ縺滄｣幄ｻ・遶・縺ｮ逶ｸ蟇ｾ菴咲ｽｮ
     * isP - 遶懊・蝣ｴ蜷・
     */
    static int index2(int x_diff,int y_diff,bool isP){
      assert(-9 <= y_diff && y_diff <= 9);
      assert(-9 <= x_diff && x_diff <= 9);
      int index=(y_diff+9)+19*(x_diff+9+19*(isP ? 1 : 0));
      assert(0<=index && index<19*19*2);
      return index;
    }
    static std::array<MultiInt, 612> attack_table;
    static std::array<MultiInt, 612> defense_table;
    static std::array<MultiInt, 32> piece_table;
    static std::array<MultiInt, 23104> attack_u;
    static std::array<MultiInt, 23104> attack_d;
    static std::array<MultiInt, 23104> attack_l;
    static std::array<MultiInt, 23104> attack_r;
    static std::array<MultiInt, 23104> defense_u;
    static std::array<MultiInt, 23104> defense_d;
    static std::array<MultiInt, 23104> defense_l;
    static std::array<MultiInt, 23104> defense_r;
    static std::array<MultiInt, 722> attack_nospace;
    static std::array<MultiInt, 722> defense_nospace;
  };
  class RookEffect : public RookEffectBase
  {
  public:
    /* in use */
    static void setUp(const Weights &weights,int stage);
  };

  class RookEffectPiece
  {
  public:
    enum { DIM = 32 * EvalStages };
    static void setUp(const Weights &weights);
  };
  class RookEffectPieceKingRelative : RookEffectBase
  {
  public:
    enum { ONE_DIM = 19584, DIM = ONE_DIM * 2*EvalStages };
    /* in use */
    static void setUp(const Weights & weights);
  };

  class RookPromoteDefense : public RookEffectBase
  {
    friend class RookPromoteDefenseRookH;
  public:
    enum { ONE_DIM = 256, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, 256> promote_defense_table;
    static std::array<MultiInt, 144> promote_defense_rook_table;
  };

  class RookPromoteDefenseRookH : public RookEffectBase
  {
  public:
    enum { ONE_DIM = 144, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  private:
  };

  class BishopEffectBase
  {
    friend class BishopEffectPiece;
  public:
    enum { ONE_DIM = 612, DIM = ONE_DIM * 2 };
    static MultiInt eval(const NumEffectState &state);
  protected:
    template<Player P>
    static MultiInt evalOne(const NumEffectState& state,
			    Square bishop,
			    Square myKing,
			    Square opKing,
			    Square ulp,
			    Square urp,
			    Square dlp,
			    Square drp,
			    bool isP);
    static int index(int x_diff, int y_diff, bool ur, bool promoted)
    {
      if (x_diff<0)
	ur = !ur;
      return y_diff + 8 + std::abs(x_diff) * 17 + (ur ? 153 : 0) + (promoted ? 306 : 0);
    }
    static int index0(int x_diff, int y_diff,PtypeO ptypeO,bool ur, bool promoted)
    {
      if (x_diff>0)
	ur = !ur;
      return -y_diff + 8 + std::abs(x_diff) * 17 + I(ptypeO) * 17 * 9 +
	(ur ? 4896 : 0) + (promoted ? 9792 : 0);
    }
    /**
     * 鮟偵・隗・鬥ｬ)縺九ｉ縺ｮ蛻ｩ縺阪・縺ゅｋ鬧偵・index
     * {attack,defense}_{ul,ur,dl,dr} 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ縺ｫ菴ｿ縺・
     * from - 鬧偵・菴咲ｽｮ
     * king - 邇峨・菴咲ｽｮ
     * ptypeO - 鬧偵・遞ｮ鬘橸ｼ檎區縺九ｉ縺ｮ蛻ｩ縺阪・蝣ｴ蜷医・蜿崎ｻ｢・・
     *          (BLACK,PTYPE_Ptype::EDGE)繧ゅ≠繧雁ｾ励ｋ
     * isP - 鬥ｬ縺ｮ蝣ｴ蜷・
     */
    static int index1(Square king,Square from,PtypeO ptypeO,bool isP)
    {
      int y_diff=Y(from)-Y(king);
      int x_diff=X(from)-X(king);
      return index1(x_diff,y_diff,ptypeO,isP);
    }
    /**
     * 鮟偵・隗・鬥ｬ)縺九ｉ縺ｮ蛻ｩ縺阪・縺ゅｋ鬧偵・index
     * {attack,defense}_{ul,ur,dl,dr} 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ縺ｫ菴ｿ縺・
     * (x_diff, y_diff) - 邇峨ｒ蝓ｺ貅悶↓隕九◆鬧偵・逶ｸ蟇ｾ菴咲ｽｮ
     * ptypeO - 鬧偵・遞ｮ鬘橸ｼ檎區縺九ｉ縺ｮ蛻ｩ縺阪・蝣ｴ蜷医・蜿崎ｻ｢・・
     *          (BLACK,PTYPE_Ptype::EDGE)繧ゅ≠繧雁ｾ励ｋ
     * isP - 鬥ｬ縺ｮ蝣ｴ蜷・
     */
    static int index1(int x_diff,int y_diff,PtypeO ptypeO,bool isP){
      assert(-9 <= y_diff && y_diff <= 9);
      assert(-9 <= x_diff && x_diff <= 9);
      assert(getPtype((PtypeO)ptypeO)!=Ptype::EMPTY);
      int index=I(ptypeO)+32*((y_diff+9)+19*(x_diff+9+19*(isP ? 1 : 0)));
      assert(0<=index && index<32*19*19*2);
      return index;
    }
    /**
     * 鮟偵・隗・鬥ｬ)縺九ｙ縺ゅｋ蝣ｴ謇縺ｯ遨ｺ繝槭せ縺ｦ繧吶↑縺・・縺ｦ繧吶◎縺ｮ蛻・ｒ陬懈ｭ｣縺吶ｋ繝・・繝輔ｙ繝ｫ
     * {attack,defense}_nospace 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ
     * king - 邇峨・菴咲ｽｮ
     * from - 隗・鬥ｬ)縺ｮ菴咲ｽｮ
     * isP - 鬥ｬ縺ｮ蝣ｴ蜷・
     */
    static int index2(Square king,Square from,bool isP)
    {
      int y_diff=Y(from)-Y(king);
      int x_diff=X(from)-X(king);
      return index2(x_diff,y_diff,isP);
    }
    /**
     * 鮟偵・隗・鬥ｬ)縺九ｙ縺ゅｋ蝣ｴ謇縺ｯ遨ｺ繝槭せ縺ｦ繧吶↑縺・・縺ｦ繧吶◎縺ｮ蛻・ｒ陬懈ｭ｣縺吶ｋ繝・・繝輔ｙ繝ｫ
     * {attack,defense}_nospace 縺ｸ縺ｮ繧｢繧ｯ繧ｻ繧ｹ
     * (x_diff, y_diff)  - 邇峨ｒ蝓ｺ貅悶↓縺励※縺ｿ縺溯ｧ・鬥ｬ)縺ｮ逶ｸ蟇ｾ菴咲ｽｮ
     * isP - 鬥ｬ縺ｮ蝣ｴ蜷・
     */
    static int index2(int x_diff,int y_diff,bool isP){
      assert(-9 <= y_diff && y_diff <= 9);
      assert(-9 <= x_diff && x_diff <= 9);
      int index=(y_diff+9)+19*(x_diff+9+19*(isP ? 1 : 0));
      assert(0<=index && index<19*19*2);
      return index;
    }
    static std::array<MultiInt, 612> attack_table;
    static std::array<MultiInt, 612> defense_table;
    static std::array<MultiInt, 32> piece_table;
    static std::array<MultiInt, 23104> attack_ur;
    static std::array<MultiInt, 23104> attack_ul;
    static std::array<MultiInt, 23104> attack_dr;
    static std::array<MultiInt, 23104> attack_dl;
    static std::array<MultiInt, 23104> defense_ur;
    static std::array<MultiInt, 23104> defense_ul;
    static std::array<MultiInt, 23104> defense_dr;
    static std::array<MultiInt, 23104> defense_dl;
    static std::array<MultiInt, 722> attack_nospace;
    static std::array<MultiInt, 722> defense_nospace;
  };
  class BishopEffect : public BishopEffectBase
  {
  public:
    /* in use */
    static void setUp(const Weights &weights,int stage);
  };
  class BishopEffectPiece
  {
  public:
    enum { DIM = 32*EvalStages };
    static void setUp(const Weights &weights);
  };

  class BishopEffectPieceKingRelative : BishopEffectBase
  {
  public:
    enum { ONE_DIM = 19584, DIM = ONE_DIM * 2*EvalStages };
    /* in use */
    static void setUp(const Weights & weights);
  };

  class BishopHead
  {
    friend class BishopHeadKingRelative;
    friend class BishopHeadX;
  public:
    enum { ONE_DIM = 32, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static int indexK(Player player, PtypeO ptypeO, int x_diff, int y_diff)
    {
      if (player == WHITE)
      {
	ptypeO=(PtypeO)(static_cast<int>(ptypeO)^(~15));
      }
      if (player == WHITE)
      {
	y_diff = -y_diff;
      }
      return (I(ptypeO) * 9 + x_diff) * 17 + y_diff + 8;
    }
//	template <Player P>
    static int indexX(Player P, PtypeO ptypeO, int x)
    {
      if (x > 5)
      {
	x = 10 - x;
      }
      if (P == WHITE)
      {
	ptypeO = altIfPiece(ptypeO);
      }
      return x - 1 + 5 * I(ptypeO);
    }
    static std::array<MultiInt, 32> table;
    static std::array<MultiInt, 4896> king_table;
    static std::array<MultiInt, 160> x_table;
  };

  class BishopHeadKingRelative
  {
  public:
    enum { ONE_DIM = 4896, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class BishopHeadX
  {
  public:
    enum { ONE_DIM = 160, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class KingRookBishop
  {
  public:
    enum { ONE_DIM = 374544, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    template<Player King>
    static MultiInt evalOne(const NumEffectState &state);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, 374544> table;
    template <Player King>
    static int index(const Square king, const Piece rook, const Piece bishop)
    {
      const int rook_x = std::abs(X(king) - X(square(rook)));
      const int bishop_x = std::abs(X(king) - X(square(bishop)));
      const int rook_y = (King == BLACK ? Y(square(rook)) - Y(king) : Y(king) - Y(square(rook)));
      const int bishop_y = (King == BLACK ? Y(square(bishop)) - Y(king) : Y(king) - Y(square(bishop)));
      return bishop_y + 8 + 17 * (bishop_x + 9 * (rook_y + 8 + 17 * (rook_x + 9 * ((owner(bishop) == King ? 1 : 0) + 2 * ((owner(rook) == King ? 1 : 0) + 2 * (2 * (isPromoted(bishop) ? 1 : 0) + (isPromoted(rook) ? 1 : 0)))))));
    }
  };

  class NumPiecesBetweenBishopAndKing
  {
    friend class NumPiecesBetweenBishopAndKingSelf;
    friend class NumPiecesBetweenBishopAndKingOpp;
    friend class NumPiecesBetweenBishopAndKingAll;
  public:
    static MultiInt eval(const NumEffectState &state);
  private:
    static void countBetween(const NumEffectState &state,
			     Square king, Piece bishop,
			     int &self_count, int &opp_count,
			     int &total_count);
    static std::array<MultiInt, 9> self_table;
    static std::array<MultiInt, 9> opp_table;
    static std::array<MultiInt, 9> all_table;
  };
  class NumPiecesBetweenBishopAndKingSelf
  {
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class NumPiecesBetweenBishopAndKingOpp
  {
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class NumPiecesBetweenBishopAndKingAll
  {
  public:
    enum { ONE_DIM = 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class BishopBishopPiece
  {
  public:
    enum { ONE_DIM = 64, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static int index(Ptype ptype, bool self_with_support,
		     bool opp_with_support)
    {
      return I(ptype) + PTYPE_SIZE * ((self_with_support ? 1 : 0) +
				      2 * (opp_with_support ? 1 : 0));
    }
    static std::array<MultiInt, 64> table;
  };

  class RookRook
  {
  public:
    enum { ONE_DIM = 800, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    template <bool SamePlayer, Player P>
    static int index(Piece rook1, Piece rook2)
    {
      const int y1 = (isOnBoard(rook1) ? Y(square(rook1)) : 0);
      const int y2 = (isOnBoard(rook2) ? Y(square(rook2)) : 0);
      if (SamePlayer)
      {
	if (P == BLACK)
	{
	  return y1 + 10 *
	    (y2 + 10 * ((isPromoted(rook1) ? 1 : 0) + 2 *
			((isPromoted(rook2) ? 1 : 0) + 2 *
			 (SamePlayer ? 1 : 0))));
	}
	else
	{
	  if (y1 == 0 || y2 == 0 || y1 == y2)
	  {
	    return (10 - y1) % 10 + 10 *
	      ((10 - y2) % 10 + 10 * ((isPromoted(rook1) ? 1 : 0) + 2 *
				      ((isPromoted(rook2) ? 1 : 0) + 2 *
				       (SamePlayer ? 1 : 0))));
	  }
	  else
	  {
	    return (10 - y2) % 10 + 10 *
	      ((10 - y1) % 10 + 10 * ((isPromoted(rook2) ? 1 : 0) + 2 *
				      ((isPromoted(rook1) ? 1 : 0) + 2 *
				       (SamePlayer ? 1 : 0))));
	  }
	}
      }
      else
      {
	return y1 + 10 *
	  (y2 + 10 * ((isPromoted(rook1) ? 1 : 0) + 2 *
		      ((isPromoted(rook2) ? 1 : 0) + 2 *
		       (SamePlayer ? 1 : 0))));
      }
    }
    static int index(bool same_player, bool promoted1,
		     bool promoted2, int y1, int y2)
    {
      return y1 + 10 *
	(y2 + 10 * ((promoted1 ? 1 : 0) + 2 *
		    ((promoted2 ? 1 : 0) + 2 *
		     (same_player ? 1 : 0))));
    }
    static std::array<MultiInt, 800> table;
  };

  class RookRookPiece
  {
  public:
    enum { ONE_DIM = 128, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static int index(Ptype ptype, bool self_with_support,
		     bool opp_with_support, bool vertical)
    {
      return I(ptype) + PTYPE_SIZE * ((self_with_support ? 1 : 0) +
				      2 * (opp_with_support ? 1 : 0)) +
	(vertical ? PTYPE_SIZE * 2 * 2 : 0);
    }
    static std::array<MultiInt, 128> table;
  };

  class BishopStandFile5
  {
  public:
    enum { ONE_DIM = 32, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, 32> table;
  };

  class MajorCheckWithCapture
  {
  public:
    enum {
      ONE_DIM = PTYPE_SIZE * 2/*bishop or rook*/ * 2 /*promotable*/,
      DIM = ONE_DIM * EvalStages
    };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, ONE_DIM> table;	
    template <Player Owner>
    static MultiInt addOne(const NumEffectState &state);
    static size_t index(Ptype ptype, bool is_rook, bool can_promote) 
    {
      return I(ptype) * 4 + is_rook * 2 + can_promote;
    }
  };

  class RookSilverKnight
  {
  public:
    enum {
      ONE_DIM = 5 * 9 * 9 * 9 * 9 * 9,
      DIM = ONE_DIM * EvalStages
    };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, ONE_DIM> table;	
    static size_t index(int rook_x, int rook_y, int silver_x, int silver_y,
			int knight_x, int knight_y)
    {
      return knight_y + 9 * (knight_x + 9 * (silver_y + 9 * (silver_x + 9 * (rook_y + 9 * rook_x))));
    }
  };

  class BishopSilverKnight
  {
  public:
    enum {
      ONE_DIM = 5 * 9 * 9 * 9 * 9 * 9,
      DIM = ONE_DIM * EvalStages
    };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, ONE_DIM> table;	
    static size_t index(int bishop_x, int bishop_y, int silver_x, int silver_y,
			int knight_x, int knight_y)
    {
      return knight_y + 9 * (knight_x + 9 * (silver_y + 9 * (silver_x + 9 * (bishop_y + 9 * bishop_x))));
    }
  };

  class AttackMajorsInBase
  {
  public:
    enum {
      ONE_DIM = PTYPE_SIZE * PTYPE_SIZE * 2 * 2 * 2,
      DIM = ONE_DIM * EvalStages
    };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, ONE_DIM> table;	
    size_t maxActive() const { return 4; }
    static int index(Ptype support, Ptype attack, bool has_gold, 
		     bool rook_support, bool bishop_support)
    {
      return (I(unpromoteSafe(support))*16 + I(unpromoteSafe(attack)))*8+has_gold*4
	+rook_support*2+bishop_support;
    }
    static void addOne(Player Owner, const NumEffectState &state, Piece rook, MultiInt&);
  };

/* kingPieceTable.h
 */

  class PieceValues;
  class KingPieceTable;
  bool operator==(const KingPieceTable& l, KingPieceTable& r);
  /**
   * 邇峨→莉悶・鬧偵・髢｢菫ゅｒ菫晄戟
   */
  class KingPieceTable
  {
  public:
    enum { EffectiveDimension = 81*2*82*PTYPE_SIZE };
  protected:
    std::array<std::array<int,Square_SIZE*PTYPE_SIZE>,Square_SIZE*2> data;
    KingPieceTable() { for(auto& v : data) v.fill(0); }
  public:
    static int otherIndex(Square other, Ptype ptype)
    {
      return I(other)*PTYPE_SIZE + static_cast<int>(ptype);
    }
    static int kingIndex(Square king, Player defense)
    {
      return I(king)*2+I(defense);
    }
    int& valueOf(Square king, Player defense, Square other, Ptype ptype)
    {
      return data[kingIndex(king,defense)][otherIndex(other,ptype)];
    }
    int valueOf(Square king, Player defense, Square other, Ptype ptype) const
    {
      return data[kingIndex(king,defense)][otherIndex(other, ptype)];
    }
    static int effectiveIndexOf(Square king, Player defense, Square other, Ptype ptype) 
    {
      int base = (((X(king)-1)*9+Y(king)-1)*2+I(defense));
      int s = isPieceStand(other) ? 0 : ((X(other)-1)*9+Y(other));
      return base*82*PTYPE_SIZE + s*PTYPE_SIZE + static_cast<int>(ptype);
    }
    void saveText(const char *filename) const;
    void loadText(const char *filename);
    void resetWeights(const int *w);
    void randomize();
    void clear();
    static int dimension() { return EffectiveDimension; }
    friend bool operator==(const KingPieceTable& l, KingPieceTable& r);
  };

/**
 * attackKing.h
 */

  /**
   * 邇峨・菴咲ｽｮ*謾ｻ謦・ｧ偵・菴咲ｽｮ*ptype
   */
  class AttackKing
  {
    struct Table : public KingPieceTable
    {
      Table();
    private:
      void adhoc_edge_king_1(const Player player,
			     const Square king,
			     const Square attack);
      void adhoc_edge_king_2(const Player player,
			     const Square king,
			     const Square attack);
    };
    static Table table;
  public:
    static int valueOf(const Piece king, const Piece attacker)
    {
      return valueOf(king, osl::ptypeO(attacker), square(attacker));
    }
    static int valueOf(Piece king, PtypeO ptypeo, Square position)
    {
      assert(ptype(king) == KING);
      if (getOwner(ptypeo) == owner(king))
	return 0;
      return table.valueOf(square(king), owner(king), 
			   position, getPtype(ptypeo));
    }
    static void saveText(const char *filename);
    static void loadText(const char *filename) { table.loadText(filename); }
    static void resetWeights(const int *w) { table.resetWeights(w); }
  };

/**
 * defenseKing.h
 */

  /**
   * 邇峨・菴咲ｽｮ*螳亥ｙ鬧偵・菴咲ｽｮ*ptype
   */
  class DefenseKing
  {
    struct Table : public KingPieceTable
    {
      Table();
    };
    static Table table;
  public:
    static int valueOf(const Piece king, const Piece defender)
    {
      return valueOf(king, osl::ptypeO(defender), square(defender));
    }
    static int valueOf(Piece king, PtypeO ptypeo, Square position)
    {
      assert(ptype(king) == KING);
      if (getOwner(ptypeo) != owner(king))
	return 0;
      return table.valueOf(square(king), owner(king), 
			   position, getPtype(ptypeo));
    }
    static void saveText(const char *filename) { table.saveText(filename); }
    static void loadText(const char *filename) { table.loadText(filename); }
    static void resetWeights(const int *w) { table.resetWeights(w); }
  };

  class PieceStand
  {
    static std::array<MultiInt, osl::Piece_SIZE> table;
  public:
    enum { DIM = osl::Piece_SIZE };
    PieceStand() { };
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(const NumEffectState &state,
				   Move moved, MultiInt last_value)
    {
      assert(player(moved)==P);
      osl::Ptype captured = capturePtype(moved);
      if (isDrop(moved))
      {
	const int count =
	  state.countPiecesOnStand(P, ptype(moved)) + 1;
	const MultiInt value =
	  table[indexMin(ptype(moved)) + count - 1];
	if(P==BLACK) 
	  return last_value - value;
	else
	  return last_value + value;
      }
      else if (captured != Ptype::EMPTY)
      {
	Ptype ptype = unpromote(captured);
	const int count = state.countPiecesOnStand(P, ptype);
	const MultiInt value = table[(indexMin(ptype) + count - 1)];
	if(P==BLACK)
	  return last_value + value;
	else
	  return last_value - value;
      }
      else
	return last_value;
    }
  };

  class NonPawnPieceStand
  {
    static std::array<MultiInt, 21> table;
  public:
    enum { DIM = 21 };
    NonPawnPieceStand() { };
    /* in use */
    static void setUp(const Weights &weights,int stage);
    static MultiInt eval(int black_count, int white_count);
  };

  class NonPawnPieceStandCombination
  {
    friend class CanCheckNonPawnPieceStandCombination;
  public:
    enum { ONE_DIM = 5625, DIM = ONE_DIM * EvalStages};
    NonPawnPieceStandCombination() { };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state,
			 const std::array<bool, 2> &can_check);
    static MultiInt evalWithUpdate(
      const NumEffectState &state,
      Move moved, const MultiInt &last_value,
      const std::array<bool, 2> &could_check,
      const std::array<bool, 2> &can_check);
  private:
    static MultiInt sumUp(const std::array<int, 6> &indices,
			  const std::array<MultiInt, 5625> &values);
    static int index(int rook, int bishop, int gold, int silver,
		     int knight, int lance)
    {
      return lance +
	5 * (knight + 5 * (silver + 5 * (gold + 5 * (3 * bishop + rook))));
    }
    static std::array<MultiInt, 5625> table;
    static std::array<MultiInt, 5625> check_table;
  };

  class NonPawnPieceStandTurn
  {
  public:
    enum { ONE_DIM = 44, DIM = ONE_DIM * EvalStages };
    NonPawnPieceStandTurn() { };
    /* in use */
    static void setUp(const Weights &weights);
    static void eval(const NumEffectState &state, MultiIntPair& out);
    template<Player P>
    static void evalWithUpdateBang(
      const NumEffectState &state,
      Move moved, MultiIntPair &last_value_and_out);
  private:
    static std::array<MultiInt, 44> table;
    static int index(Player player, Player turn, Ptype ptype, int count)
    {
      return indexMin(ptype) - 18 + count +
	(turn == player ? 22 : 0);
    }
  };
  class PieceStandY
  {
  private:
    static std::array<MultiInt, 360> y_attack_table;
    static std::array<MultiInt, 360> y_defense_table;
    static std::array<MultiInt, 9*7*19> y_attack_table_sum;
    static std::array<MultiInt, 9*7*19> y_defense_table_sum;
    static int index(Ptype ptype, Player player, Square king, int count)
    {
      const int king_y = (player == BLACK ? Y(king) : 10 - Y(king));
      return (king_y - 1) * 40 + indexMin(ptype) + count;
    }
    static int index(int i, Player player, Square king, int count)
    {
      const int king_y = (player == BLACK ? Y(king) : 10 - Y(king));
      return (king_y - 1) * 7*19 + i*19 + count;
    }
    static void updateResult(NumEffectState const& state, MultiInt &result,int i, Ptype ptype, std::array<Square,2> const&kings);
  public:
    enum { ONE_DIM = osl::Piece_SIZE * 9, DIM = ONE_DIM * 2*EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
    template<Player P>
    static MultiInt evalWithUpdate(
      const NumEffectState &state, Move moved,
      const MultiInt &last_value);
  };

  class CanCheckNonPawnPieceStandCombination
  {
  public:
    enum { ONE_DIM = 5625, DIM = ONE_DIM * EvalStages};
    /* in use */
    static void setUp(const Weights &weights);
    template <Player Defense>
    static bool canCheck(const NumEffectState &state)
    {
      constexpr Player Attack=alt(Defense);
      const King8Info king = state.king8Info(Defense);
      return (dropCandidate(king) != 0 ||
	      hasMoveCandidate(king, Attack, state) ||
	      KnightCheck::canCheck<Defense>(state));
    }
  };
  class PieceStandCombinationBoth
  {
  public:
    enum { ONE_DIM = 16384, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
  private:
    static std::array<MultiInt, 16384> table;
  };

  class PiecePair
  {
  public:
    enum { 
      plain_table_size = 1338,
      x_table_size = 4901,
      y_table_size = 7057,
      DIM = plain_table_size + x_table_size + y_table_size, // 14 * 12 * PTYPEO_SIZE * PTYPEO_SIZE
    };
	
    static int eval(const NumEffectState&, const Weights&);
    template<int Direction, int Offset>
    static int sum12One(const Piece *basePtr,const int *xbase, const int *ybase);
    static int sum12(NumEffectState const& state,Square base,PtypeO ptypeO);
    template<int Direction, int Offset>
    static int adjust12One(const Piece *basePtr,const int *xbase1, const int *ybase1,const int *xbase2, const int *ybase2);
    static int adjust12(NumEffectState const& state,Square base,PtypeO pos,PtypeO neg);

    static int evalWithUpdate(const NumEffectState& state, Move moved, int last_value, const Weights& values);
    static int evalWithUpdateCompiled(const NumEffectState& state, Move moved, int last_value);

    static int pieceValue(const NumEffectState& state, Piece p, const Weights& values);
    static int pieceValueDouble(const NumEffectState& state, Piece p, const Weights&);
    static int weight(Player attack, int index, const Weights& values)
    {
      return (attack == BLACK ? values.value(index) : -values.value(index));
    }
    typedef std::array<int,3> index_t;
    static index_t index(int offset_id, Piece p, Piece q);
    static index_t index(int offset_id, Square p0, PtypeO o0, Square p1, PtypeO o1);
	
    static int value(int offset_id, Piece p, Piece q, const Weights& values)
    {
      assert(isOnBoard(p) && isOnBoard(q));
      return value(offset_id, square(p), osl::ptypeO(p), square(q), osl::ptypeO(q), values);
    }
    static int value(int offset_id, Piece p, Square p1, PtypeO o1, const Weights& values)
    {
      return value(offset_id, square(p), osl::ptypeO(p), p1, o1, values);
    }
    static int value(int offset_id, Square p0, PtypeO o0, Square p1, PtypeO o1, const Weights& values)
    {
      assert(p0 != p1);
      index_t idx = index(offset_id, p0, o0, p1, o1);
      assert(idx[0] != 0);	// do not forget to call init()
      int ret = 0;
      for (int i=0; i<3; ++i)
	ret += (idx[i] > 0) ? values.value(idx[i]) : -values.value(-idx[i]);
      return ret;
    }

    static void init();
    static void sanitize(Weights& values);
    /** values 繧貞ｱ暮幕縺励※繧ｯ繝ｩ繧ｹ蜈ｨ菴薙※繧吩ｽｿ縺・*/
    static void compile(const Weights& values);
    static int valueCompiled(int offset_id, Piece p, Square p1, PtypeO o1)
    {
      return valueCompiled(offset_id, square(p), osl::ptypeO(p), p1, o1);
    }
    static int valueCompiled(int offset_id, Square p0, PtypeO o0, Square p1, PtypeO o1);

    // 蜀・Κ逕ｨ
    struct IndexTable : public std::array<std::array<std::array<signed short, PTYPEO_SIZE>, PTYPEO_SIZE>, 12>
    {
      IndexTable();
      void fillBW(int index, int dir, Ptype p0, Ptype p1);
      /** for same owner */
      void fillSame(int index, int dir, Ptype p0, Ptype p1);
      /** for different owner */
      void fillDiffer(int index, int dir, Ptype p0, Ptype p1);
      static int pindex(Player player, Ptype ptype) 
      {
	return I(newPtypeO(player, ptype));
      }
      void amplify(int base);
    };
    static IndexTable plain_table;
    static std::array<IndexTable, 10> x_table, y_table;
    static const std::array<const Offset, 12> offsets;	// offset_id -> Offset
  };

/* pin.h
 */

  class PinPtypeAll
  {
  public:
    static void setUp();
    static MultiInt eval(const NumEffectState &state);
  private:
    static MultiInt evalOne(Player Defense, const NumEffectState &state);
    static bool pawnAttack(Player Defense, const NumEffectState &state, Square sq){
      Square pawn = state.pawnSquare(alt(Defense), X(sq));
      return pawn == sq + 2 * newOffset(Defense, U) ||
	(isPieceStand(pawn) && isEmpty(state[nextSquare(Defense, sq, U)]));
    }
  protected:
    static std::array<MultiInt, 16 * 7 * 8> all_table;
  };
  std::array<MultiInt, 16 * 7 * 8> osl::PinPtypeAll::all_table;

  void PinPtypeAll::setUp()
  {
    for(int i = 0; i < 5; i++)
      for(int pt = 0; pt < 16; pt++)
	for(int d1 = 0; d1 < 7; d1++){
	  int di = (pt + i * 16) * 7 + d1;
	  all_table[di] = PinPtypeDistance::table[di] + PinPtype::table[pt + i * 16];
	  if( 1 <= i && i <= 3){
	    all_table[di + (16 * 7 * 4)] = all_table[di] + PinPtypePawnAttack::table[pt + (i - 1) * 16];
	  }
	}
  }
  MultiInt PinPtypeAll::evalOne(Player Defense, const NumEffectState &state)
  {
    MultiInt result;
    const Square king = state.kingSquare(Defense);
    for(Piece piece : state.allPiece(state.pin(Defense) & state.effectedMask(Defense))){
      Square sq = square(piece);
      Ptype T = ptype(piece);
      if (isUD(king, sq)){
        int i = I(T) * 7 + std::abs(Y(king) - Y(sq)) - 1 +
                (state.hasEffectStrict(alt(Defense), sq, LANCE) ? 16 * 4 * 7 : 0);
	result += all_table[i];
      }
      else{
	int i = I(T) * 7 + std::abs(X(king) - X(sq)) - 1 +
	  (pawnAttack(Defense, state, sq) ? 4 * 7 * 16 : 0) +
          (isLR(king, sq) ? 7 * 16 :
	    (yForBlack(Defense, sq) < yForBlack(Defense, king) ?
				      2 * 7 * 16 : 3 * 7 * 16));
	result += all_table[i];
      }
    }
    return result;
  }

  MultiInt PinPtypeAll::eval(const NumEffectState &state)
  {
    return evalOne(BLACK, state) - evalOne(WHITE, state);
  }

  class CheckShadowPtype 
  {
  public:
    enum { 
      // rook v, rook h, bishop u, bishop d, lance
      ONE_DIM = PTYPE_SIZE * 5, 
      DIM = ONE_DIM * EvalStages 
    };
    /* in use */
    static void setUp(const Weights &weights);
    static MultiInt eval(const NumEffectState &state);
    template <Player King>
    static MultiInt evalOne(const NumEffectState &state);
    static std::array<MultiInt, ONE_DIM> table;
  };

/* piecePairTable.h
 */

  class RookMobilityAll
  {
    friend class RookMobility;
    friend class RookMobilityX;
    friend class RookMobilityY;
    friend class RookMobilitySum;
    friend class RookMobilitySumKingX;
    friend class RookMobilityXKingX;
  public:
//	template<int Sign>
    static void adjust(Player pl,const NumEffectState&, bool promoted,
		       int vertical, int horizontal,
		       Square pos, 
		       MultiInt& value);
    static void eval(const NumEffectState&, MultiInt& out);
  private:
    static int indexX(Square rook, bool promoted,
		      int count, bool vertical)
    {
      const int x = (X(rook) > 5 ?
		     10 - X(rook) : X(rook));
      return x - 1 + 5 * ((promoted ? 1 : 0) +
			  2 * ((vertical ? 1 : 0) + 2 * count));
    }
//	template <int Sign>
    static int indexY(Player pl, Square rook, bool promoted,
		      int count, bool vertical)
    {
      const int y = (pl == BLACK ? Y(rook) : 10 - Y(rook));
      return y - 1 + 9 * ((promoted ? 1 : 0) +
			  2 * ((vertical ? 1 : 0) + 2 * count));
    }
//	template <int Sign>
    static int indexXKingX(Player pl, Square rook, Square king, int count, bool vertical)
    {
      const Square r = (pl == BLACK ? rook : rotate180(rook));
      const Square k = (pl == BLACK ? king : rotate180(king));
      const bool flip = X(r) > 5;
      const int x = (flip ? 10 - X(r) : X(r));
      const int king_x = (flip ? 10 - X(k) : X(k));
      return king_x - 1 + 9 * (x - 1 + 5 * ((vertical ? 1 : 0) + 2 * count));
    }
    static std::array<MultiInt, 18> rook_vertical_table;
    static std::array<MultiInt, 18> rook_horizontal_table;
    static std::array<MultiInt, 34> sum_table;
    static std::array<MultiInt, 324> x_table;
    static std::array<MultiInt, 324> y_table;
    static std::array<MultiInt, 17 * 9> sumkingx_table;
    static std::array<MultiInt, 9 * 2 * 5 * 9> xkingx_table;
  };

  class RookMobility
  {
  public:
    enum { DIM = 36 };
    /* in use */
    static void setUp(const Weights &weights,int stage);
  };

  class RookMobilitySum
  {
  public:
    enum { ONE_DIM = 34, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class RookMobilityX
  {
  public:
    enum { ONE_DIM = 180, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class RookMobilityY
  {
  public:
    enum { ONE_DIM = 324, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class RookMobilitySumKingX
  {
  public:
    enum { ONE_DIM = 17 * 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  class RookMobilityXKingX
  {
  public:
    enum { ONE_DIM = 9 * 2 * 5 * 9, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };

  struct BishopMobilityAll
  {
    friend class BishopMobility;
    friend class BishopMobilityEach;
  public:
    static void adjust(Player pl, bool promoted, int mobility1, int mobility2,
		       MultiInt& value);
    static void eval(const NumEffectState&, MultiInt& out);
  private:
    static std::array<MultiInt, 36> bishop_table;
    static std::array<MultiInt, 18> each_table;
  };
  class BishopMobility
  {
  public:
    enum { DIM = 36 };
    /* in use */
    static void setUp(const Weights &weights,int stage);
  };
  class BishopMobilityEach
  {
  public:
    enum { ONE_DIM = 18, DIM = ONE_DIM * EvalStages };
    /* in use */
    static void setUp(const Weights &weights);
  };
  struct LanceMobilityAll
  {
    static void adjust(Player pl, int index, MultiInt& value);
    static void eval(const NumEffectState&, MultiInt& out);
  };
  class LanceMobility
  {
    static std::array<MultiInt, 9> lance_table;
    friend struct LanceMobilityAll;
  public:
    enum { DIM = 9 };
    LanceMobility() { };
    /* in use */
    static void setUp(const Weights &weights,int stage);
  };
}

/* weights.cc
 */

osl::Weights::Weights(size_t idim)
  : values(idim), dim(idim)
{
  std::fill(&values[0], &values[0]+dim, 0);
}

osl::Weights::~Weights()
{
}

void osl::Weights::resetDimension(size_t new_dim)
{
  dim = new_dim;
  values.resize(new_dim);
  std::fill(&values[0], &values[0]+dim, 0);
}

std::array<MultiInt, 45> osl::King8All::table2;
std::array<MultiInt, 720> osl::King8All::table3;
std::array<MultiInt, 32> osl::King8All::table4;

void osl::King8All::setUp()
{
  std::array<MultiInt, 90> table;
  for(int z = 0; z < 2; z++)
    for(int x = 0; x < 5; x++)
      for(int y = 0; y < 9; y++){
	int orig_x = x + z * 5;
	int orig_i = orig_x * 9 + y;
	int i = y * 5 + x + 45 * z;
	table[i] =  KingXBlockedBoth::table[orig_x] + KingXBlockedBoth::table[orig_i + 10];
      }

  for (int y = 0; y < 9; y++)
    for(int x = 0; x < 5; x++){
      int i = y * 5 + x;
      table2[i] = KingXBothBlocked::table[x] + KingXBothBlockedY::table[i];
    }
  for(int x=1;x<=5;x++)
    for(int y=1;y<=9;y++)
      for(int is_l=0;is_l<2;is_l++)
	for(int u_blocked=0;u_blocked<2;u_blocked++)
	  for(int opp_u_blocked=0;opp_u_blocked<2;opp_u_blocked++)
	    for(int opp_blocked=0;opp_blocked<2;opp_blocked++){
	      int indexY=x - 1 + 5 * (y - 1 + 9 * ((is_l ? 1 : 0) + 2 * ((u_blocked ? 1 : 0) + 2 * ((opp_u_blocked ? 1  : 0) + 2 * (opp_blocked ? 1 : 0)))));
	      int index0=x - 1 + 5 * ((is_l ? 1 : 0) + 2 * ((u_blocked ? 1 : 0) + 2 * ((opp_u_blocked ? 1  : 0) + 2 * (opp_blocked ? 1 : 0))));
	      int index1 = x - 1 + 5 * (y - 1) + 45 * (is_l ? 0 : 1);
	      table3[indexY] = KingXBlocked3Y::table[indexY] + KingXBlocked3::table[index0] + table[index1];
	    }

  for(int i = 0; i < 32; i++){
    table4[i].clear();
    if ((i & (M(L) | (M(R) >> 3))) != 0) 
      table4[i] += AnagumaEmpty::table[1];
    if ((i & M(U)) != 0) 
      table4[i] += AnagumaEmpty::table[2];
    if ((i & (M(UL) | M(UR))) != 0) 
      table4[i] += AnagumaEmpty::table[3];
  }
}

bool osl::King8All::isBlocked(Player P, const NumEffectState &state, bool is_l)
{
  const King8Info info = state.king8Info(P);
  return (is_l ? 
	  (liberty(info) & (M(UL) | M(L) | M(DL))) == 0 :
	  (liberty(info) & (M(UR) | M(R) | M(DR))) == 0);
}

int osl::King8All::index2(Player P, Square king)
{
  int x = X(king);
  int y = (P == BLACK ? Y(king) : 10 - Y(king));
  return (y - 1) * 5 + (x > 5 ? 9 - x : x - 1);
}

int osl::King8All::index3(Player P, const Square king, bool is_l,
			  bool u_blocked, bool opp_u_blocked,
			  bool opp_blocked)
{
  if( (P == BLACK && X(king) > 5) || (P == WHITE && X(king) <= 4)) is_l = !is_l;
  int i = index2(P, king);
  return i + 45 * ((is_l ? 1 : 0) + 2 * ((u_blocked ? 1 : 0) + 2 * ((opp_u_blocked ? 1 : 0) + 2 * (opp_blocked ? 1 : 0))));
}

constexpr bool FT[2]={false, true};
MultiInt osl::
King8All::evalOne(Player P, const osl::NumEffectState &state)
{
  MultiInt ret; // MultiInt constructor fill zeros, no need to clear
  Square king = state.kingSquare(P);
  King8Info info = state.king8Info(P);
  if ((X(king) == 1 || X(king) == 9) &&
      ((P == BLACK && Y(king) == 9) ||
       (P == WHITE && Y(king) == 1))){
    int i = ((spaces(info) & 0x80) >> 3) + (spaces(info) & 0xf);
    ret += valueFor(P, table4[i]);
  }
  bool both=true;
  for(bool is_l : FT){
    if(isBlocked(P, state, is_l)){
      ret += valueFor(P, table3[index3(P, state.kingSquare(P), is_l,
				       (liberty(info) & M(U)) == 0,
				       (liberty(info) & M(is_l ? UR : UL)) == 0,
				       (liberty(info) & M(is_l ? R : L)) == 0)]);
    }
    else both=false;
  }
  if(both) ret += valueFor(P, table2[index2(P, king)]);
  return ret;
}

MultiIntPair osl::
King8All::eval(const osl::NumEffectState &state)
{
  MultiIntPair ret;
  for(Player P : COLORS) ret[I(P)] = evalOne(P, state);
  return ret;
}

inline
void
osl::
King8All::evalWithUpdateBang(const NumEffectState &new_state,
			     Move /* last_move */,
			     MultiIntPair& ret)
{
  BoardMask mask = new_state.changedSquare();
  for(Player P : COLORS)
    if (mask.anyInRange(Board_Mask_Table3x3.mask(new_state.kingSquare(P))))
      ret[I(P)] = evalOne(P, new_state);
}

std::array<osl::MultiInt, 256*2*2> osl::BishopRookFork::table;

void osl::BishopRookFork::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
    for (int s = 0; s < NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM * s);
  for (Ptype i = Ptype::MIN; i <= Ptype::MAX; ++i)
    for (Ptype j = i + 1; j <= Ptype::MAX; ++j)
      for(int k = 0; k < 2; k++){
	table[bishopIndex(j, i) * 2 + k] = table[bishopIndex(i, j) * 2 + k];
	table[rookIndex(j, i) * 2 + k] = table[rookIndex(i, j) * 2 + k];
      }
}

inline bool osl::BishopRookFork::
findDropInLine(const NumEffectState& state, Player defense, Square a, Square b)
{
  if(!state.isEmptyBetween(a, b)) return false;
  Offset offset = Board_Table.getShortOffset(b, a);
  for(Square sq = b + offset; sq != a; sq += offset)
    if (! state.canTakeBack(defense, sq))
      return true;
  return false;
}

inline bool osl::BishopRookFork::
testCenter(const NumEffectState& state, Player defense, 
	   Square a, Square b, Square center)
{
  if (!isEmpty(state[center]) || state.canTakeBack(defense, center))
    return false;
  return state.isEmptyBetween(center, a) && state.isEmptyBetween(center, b);
}

bool osl::
BishopRookFork::isBishopForkSquare(const NumEffectState& state, Player defense, 
				   const Square a, const Square b)
{
  const int cx = X(b) - X(a), cy = Y(b) - Y(a);
  const int p = (cx + cy) / 2, q = (cx - cy) / 2;
  if (p == 0 || q == 0)
    return findDropInLine(state, defense, a, b);
  const Square sqs[2] = {b + newOffset(-p, -p), b + newOffset(-q, q)};
  for(Square sq : sqs)
    if (isOnBoard(sq) && testCenter(state, defense, a, b, sq)) return true;
  return false;
}

inline
bool osl::
BishopRookFork::isRookForkSquare(const NumEffectState& state, Player defense, 
				 const Square a, const Square b)
{
  if(isULRD(a, b)) return findDropInLine(state, defense, a, b);
  std::array<Square,2> centers = {newSquare(X(a), Y(b)), newSquare(X(b), Y(a))};
  for(Square sq : centers)
    if (testCenter(state, defense, a, b, sq)) return true;
  return false;
}

osl::MultiIntPair osl::
BishopRookFork::evalOne(Player Defense, const NumEffectState &state, PieceMask target)
{
  MultiIntPair result;
  if (state.hasPieceOnStand(alt(Defense), BISHOP)){
    PieceMask masks[2], target2 = target;
    masks[0] = masks[1] = PieceMask(0);
    while(any(target2)){
      PieceMask m0 = lowestMask(target2);
      int i = takeOneBit(target2);
      Square sq = square(state.pieceOf(i));
      int parity = (X(sq) + Y(sq)) & 1;
      masks[parity] |= m0;
    }
    for(int j = 0; j < 2; j++){
      target2 = masks[j];
      while(any(target2)){
	Piece pi = state.pieceOf(takeOneBit(target2));
	PieceMask target1 = target2;
	while(any(target1)){
	  Piece pj = state.pieceOf(takeOneBit(target1));
	  if (isBishopForkSquare(state, Defense, square(pi), square(pj))){
	    const int index = bishopIndex(ptype(pi), ptype(pj)) * 2;
	    result[I(alt(Defense))].addFor(Defense, table[index]);
	    result[I(Defense)].addFor(Defense, table[index + 1]);
	  }
	}
      }
    }
  }
  if (state.hasPieceOnStand(alt(Defense), ROOK)){
    while(any(target)){
      Piece pi = state.pieceOf(takeOneBit(target));
      PieceMask target1 = target;
      while(any(target1)){
	Piece pj = state.pieceOf(takeOneBit(target1));
	if (isRookForkSquare(state, Defense, square(pi), square(pj))){
	  const int index = rookIndex(osl::ptype(pi), osl::ptype(pj))*2;
	  result[I(alt(Defense))].addFor(Defense, table[index]);
	  result[I(Defense)].addFor(Defense, table[index + 1]);
	}
      }
    }
  }
  return result;
}

osl::MultiIntPair osl::
BishopRookFork::eval(const NumEffectState &state)
{
  MultiIntPair result;
  if(state.countPiecesOnStand(BLACK, BISHOP) + 
     state.countPiecesOnStand(WHITE, BISHOP) + 
     state.countPiecesOnStand(BLACK, ROOK) + 
     state.countPiecesOnStand(WHITE, ROOK) == 0) return result;
  PieceMask notcovered = pieceMask(KING) | 
    ~(state.effectedMask(BLACK) | state.effectedMask(WHITE) | pieceMask(PAWN)); 
  for(Player P : COLORS){
    if (state.countPiecesOnStand(P, BISHOP) + 
	state.countPiecesOnStand(P, ROOK) != 0){
      PieceMask target = notcovered & state.piecesOnBoard(alt(P));
      result += evalOne(alt(P), state, target);
    }
  }
  return result;
}

std::array<MultiInt, osl::King25EffectSupportedY::ONE_DIM>
osl::King25EffectSupportedY::table;

void osl::King25EffectSupportedY::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int y = 1; y <= 9; y++)
    for(int piece_count = 0; piece_count < 17; piece_count++)
      for(int supported = 0; supported < 17; supported++)
	table[(supported * 17 + piece_count) * 9 + y - 1] += King25EffectSupported::table[supported * 17 + piece_count];
}









std::array<MultiInt, 5 * 3 * 8 * 3>
osl::King25EffectEachBoth::table;
std::array<MultiInt, 3000>
osl::King25EffectEachBoth::x_table;
std::array<MultiInt, 3240>
osl::King25EffectEachBoth::y_table;
std::array<MultiInt, 27000>
osl::King25EffectEachBoth::xy_table;
std::array<int, 256>
osl::King25EffectEachBoth::effect_state_table;

void osl::
King25EffectEachXY::setUp(const Weights &weights)
{
  for(int rel_y_2=0;rel_y_2<5;rel_y_2++)
    for(int x_diff_2=0;x_diff_2<5;x_diff_2++)
      for(int es=0;es<8;es++)
	for(int po=0;po<3;po++)
	  for(int king_x_1=0;king_x_1<5;king_x_1++){
	    int oldIndex=(rel_y_2+x_diff_2*5+es*5*5+po*5*5*8)*5+king_x_1;
	    int newIndex=po+3*(es+8*(rel_y_2+5*(x_diff_2+5*king_x_1)));
	    for (int s=0; s<NStages; ++s)
	      King25EffectEachBoth::x_table[newIndex][s] = weights.value(oldIndex + X_DIM*s);
	  }
  for(int rel_y_2=0;rel_y_2<5;rel_y_2++)
    for(int rel_x=0;rel_x<3;rel_x++)
      for(int es=0;es<8;es++)
	for(int po=0;po<3;po++)
	  for(int king_y_1=0;king_y_1<9;king_y_1++){
	    int oldIndex=(rel_y_2+rel_x*5+es*5*3+po*5*3*8)*9+king_y_1;
	    int newIndex=po+3*(es+8*(rel_y_2+5*(rel_x+3*king_y_1)));
	    for (int s=0; s<NStages; ++s)
	      King25EffectEachBoth::y_table[newIndex][s] = weights.value(oldIndex+ X_DIM * EvalStages + Y_DIM*s)+King25EffectEachBoth::table[oldIndex/9][s];
	  }
  for(int d_effect=0;d_effect<16;d_effect++){
    for(int a_effect=0;a_effect<16;a_effect++){
      if(a_effect==0){
	King25EffectEachBoth::effect_state_table[a_effect*16+d_effect]=3*(std::min(2, d_effect));
      }
      else{
	int diff=d_effect-a_effect;
	King25EffectEachBoth::effect_state_table[a_effect*16+d_effect]=
	  3*(std::max(-2, std::min(2, diff)) + ATTACK_DIFF_0);
      }
    }
  }
}

void osl::
King25EffectEachKXY::setUp(const Weights &weights)
{
  for(int rel_y_2=0;rel_y_2<5;rel_y_2++)
    for(int x_diff_2=0;x_diff_2<5;x_diff_2++){
      int rel_x=std::abs(x_diff_2-2);
      for(int es=0;es<8;es++)
	for(int po=0;po<3;po++)
	  for(int king_x_1=0;king_x_1<5;king_x_1++)
	    for (int king_y_1=0;king_y_1<9;king_y_1++) {
	      int oldIndex=((rel_y_2+x_diff_2*5+es*5*5+po*5*5*8)*9+king_y_1)*5 + king_x_1;
	      int newIndexX=po+3*(es+8*(rel_y_2+5*(x_diff_2+5*king_x_1)));
	      int newIndexY=po+3*(es+8*(rel_y_2+5*(rel_x+3*king_y_1)));
	      int newIndex=po+3*(es+8*(rel_y_2+5*(x_diff_2+5*(king_x_1+5*king_y_1))));
	      for (int s=0; s<NStages; ++s)
		King25EffectEachBoth::xy_table[newIndex][s] = weights.value(oldIndex + ONE_DIM*s)+King25EffectEachBoth::x_table[newIndexX][s]+King25EffectEachBoth::y_table[newIndexY][s];
	    }
    }
}

template <osl::Player Defense>
int
osl::King25EffectEachBoth::effectStateIndex3(
  const NumEffectState &state, Square target)
{
  const int d_effect=state.countEffect(Defense, target);
  const int a_effect=state.countEffect(alt(Defense), target);
  return effect_state_table[a_effect*16+d_effect];
}

template <osl::Player Defense>
void osl::King25EffectEachBoth::index(
  const NumEffectState &state, 
  Square target, int &index_xy,
  int rel_y, int king_x, int king_y, int x_diff)
{
  const Piece piece = state.pieceAt(target);
  // piece_owner: 0 - empty, 1 - defense, 2 - attack
  int piece_owner;
  PtypeO ptypeO=osl::ptypeO(piece);
  if(Defense==BLACK){
#ifdef __INTEL_COMPILER
    piece_owner = (unsigned int)((int)(ptypeO)>>30);
    piece_owner &= 0x2;
    piece_owner |= (int(ptypeO)+14)>>4;
#else
    piece_owner=((int(ptypeO)+14)>>4)|(((unsigned int)ptypeO>>30)&0x2);
#endif
  }
  else{
    piece_owner=(((int(ptypeO)+14)>>3)&0x2)|((unsigned int)ptypeO>>31);
  }
  assert(piece_owner >= 0 && piece_owner < 3);
  int effect_state_index = effectStateIndex3<Defense>(state, target);

  index_xy=piece_owner+effect_state_index+3*(8*((rel_y+2)+5*((x_diff+2)+5*(king_x-1+5*(king_y-1)))));
}

template <osl::Player Defense>
void osl::King25EffectEachBoth::evalOne(
  const NumEffectState &state, MultiInt &out)
{
  out.clear();
  const Square king = state.kingSquare(Defense);
  const int min_dx = std::max(1, X(king) - 2)-X(king);
  const int max_dx = std::min(9, X(king) + 2)-X(king);
  const int min_dy = std::max(1, Y(king) - 2)-Y(king);
  const int max_dy = std::min(9, Y(king) + 2)-Y(king);
  const int king_x = (X(king) > 5 ? 10 - X(king) : X(king));
  const int king_y = (Defense == BLACK ? Y(king) : 10 - Y(king));
  if ((Defense == BLACK && X(king) >= 6) ||
      (Defense == WHITE && X(king) >= 5)){
    for (int dx = min_dx; dx <= max_dx; ++dx)
    {
      // [0, 2]
      // const int rel_x = std::abs(dx);
      // [-2, 2]
      int x_diff = dx;
      for (int dy = min_dy; dy <= max_dy; ++dy)
      {
	const Square target=newSquare(X(king)+dx, Y(king)+dy);
	// [-2, +2]
	const int rel_y = dy * (Defense == BLACK ? 1 : -1);
	int index_xy;
	index<Defense>(state, target, index_xy,
		       rel_y,king_x,king_y,x_diff);
	out += xy_table[index_xy];
      }
    }
  }
  else {
    for (int dx = min_dx; dx <= max_dx; ++dx)
    {
      // [0, 2]
      // const int rel_x = std::abs(dx);
      // [-2, 2]
      int x_diff = -dx;
      for (int dy = min_dy; dy <= max_dy; ++dy)
      {
	const Square target=newSquare(X(king)+dx, Y(king)+dy);
	// [-2, +2]
	const int rel_y = dy * (Defense == BLACK ? 1 : -1);
	int index_xy;
	index<Defense>(state, target, index_xy,
		       rel_y,king_x,king_y,x_diff);
	out +=  xy_table[index_xy];
      }
    }
  }
  if (Defense != BLACK)
  {
    out = -out;
  }
}

void osl::
King25EffectEachBoth::eval(const NumEffectState &state,
			   MultiIntPair &out)
{
  evalOne<BLACK>(state, out[I(BLACK)]);
  evalOne<WHITE>(state, out[I(WHITE)]);
}

void
osl::King25EffectEachBoth::evalWithUpdate(
  const NumEffectState &state, Move /* last_move */,
  MultiIntPair &values)
{
  const Square kb = state.kingSquare(BLACK), kw = state.kingSquare(WHITE);
  BoardMask mask = state.changedSquare();
  const bool update_black = mask.anyInRange(Board_Mask_Table5x5.mask(kb));
  const bool update_white = mask.anyInRange(Board_Mask_Table5x5.mask(kw));
  if (update_black)
  {
    evalOne<BLACK>(state, values[I(BLACK)]);
  }
  if (update_white)
  {
    evalOne<WHITE>(state, values[I(WHITE)]);
  }
}



std::array<MultiInt, 3072> osl::King3Pieces::table;
std::array<MultiInt, 15360> osl::King3Pieces::x_table;
std::array<MultiInt, 27648> osl::King3Pieces::y_table;

void osl::King3Pieces::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King3PiecesXY::setUp(const Weights &weights)
{
  for (int i = 0; i < X_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King3Pieces::x_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for (int i = 0; i < Y_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King3Pieces::y_table[i][s] = weights.value(i + ONE_DIM*s + X_DIM);
  }
}

template <osl::Player King>
void osl::King3Pieces::evalOne(const NumEffectState &state,
			       MultiInt &result)
{
  const Square king = state.kingSquare(King);
  const int vertical_index =
    index<King, VERTICAL>(
      osl::ptypeO(state.pieceAt(king + newOffset(King,U))),
      osl::ptypeO(state.pieceAt(king + newOffset(King,D))));
  const int vertical_index_x =
    indexX<King, VERTICAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset(King,U))),
      osl::ptypeO(state.pieceAt(king + newOffset(King,D))));
  const int vertical_index_y =
    indexY<King, VERTICAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset(King,U))),
      osl::ptypeO(state.pieceAt(king + newOffset(King,D))));
  const int horizontal_index =
    index<King, HORIZONTAL>(
      osl::ptypeO(state.pieceAt(king + newOffset(King,L))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,R))));
  const int horizontal_index_x =
    indexX<King, HORIZONTAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,L))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,R))));
  const int horizontal_index_y =
    indexY<King, HORIZONTAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,L))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,R))));
  const int diagonal_index1 =
    index<King, DIAGONAL>(
      osl::ptypeO(state.pieceAt(king + newOffset( King,UL))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DR))));
  const int diagonal_index2 =
    index<King, DIAGONAL>(
      osl::ptypeO(state.pieceAt(king + newOffset( King,UR))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DL))));
  const int diagonal_index1_x =
    indexX<King, DIAGONAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,UL))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DR))));
  const int diagonal_index2_x=
    indexX<King, DIAGONAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,UR))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DL))));
  const int diagonal_index1_y =
    indexY<King, DIAGONAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,UL))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DR))));
  const int diagonal_index2_y =
    indexY<King, DIAGONAL>(
      king,
      osl::ptypeO(state.pieceAt(king + newOffset( King,UR))),
      osl::ptypeO(state.pieceAt(king + newOffset( King,DL))));
  const MultiInt v = value(vertical_index, horizontal_index,
			   diagonal_index1, diagonal_index2,
			   vertical_index_x,  horizontal_index_x,
			   diagonal_index1_x, diagonal_index2_x,
			   vertical_index_y , horizontal_index_y,
			   diagonal_index1_y, diagonal_index2_y);
  result.addFor(King, v);
}

MultiInt
osl::King3Pieces::eval(const NumEffectState &state)
{
  MultiInt result;
  evalOne<BLACK>(state, result);
  evalOne<WHITE>(state, result);
  return result;
}

MultiInt
osl::King3Pieces::evalWithUpdate(
  const NumEffectState &state,
  Move last_move,
  MultiInt &last_value)
{
  if ((std::abs(X(to(last_move)) - X(state.kingSquare(BLACK))) <= 1 &&
       std::abs(Y(to(last_move)) - Y(state.kingSquare(BLACK))) <= 1) ||
      (std::abs(X(to(last_move)) - X(state.kingSquare(WHITE))) <= 1 &&
       std::abs(Y(to(last_move)) - Y(state.kingSquare(WHITE))) <= 1))
    return eval(state);
  if (!isDrop(last_move))
  {
    if ((std::abs(X(from(last_move)) - X(state.kingSquare(BLACK))) <= 1 &&
	 std::abs(Y(from(last_move)) - Y(state.kingSquare(BLACK))) <= 1) ||
	(std::abs(X(from(last_move)) - X(state.kingSquare(WHITE))) <= 1 &&
	 std::abs(Y(from(last_move)) - Y(state.kingSquare(WHITE))) <= 1))
      return eval(state);
  }
  return last_value;
}



std::array<int, 17 * 128 * 9> osl::King25EffectYAttack::table;
std::array<MultiInt, 17 * 128 * 9> osl::King25EffectYDefense::table;


std::array<MultiInt, 3240> osl::KingMobilityVal::table;
std::array<MultiInt, 3240> osl::KingMobilityVal::rook_table;
std::array<MultiInt, 3240> osl::KingMobilityVal::bishop_table;
std::array<MultiInt, 3240> osl::KingMobilityVal::rook_bishop_table;

void osl::KingMobilityVal::setUp(const Weights &weights)
{
  static std::array<MultiInt, 3240> old_table;
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      old_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int king_x=0;king_x<5;king_x++)
    for(int king_y=0;king_y<9;king_y++)
      for(int dir=0;dir<8;dir++)
	for(int mobility=0;mobility<9;mobility++){
	  int oldIndex=king_x + 5 * (king_y + 9 * (dir + 8 * mobility));
	  int newIndex=mobility+9*(dir+8*(king_y+9*king_x));
	  for (int s=0; s<NStages; ++s)
	    table[newIndex][s]=old_table[oldIndex][s];
	}
}

void osl::
KingMobilityWithRook::setUp(const Weights &weights)
{
  static std::array<MultiInt, 3240> old_table;
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      old_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int king_x=0;king_x<5;king_x++)
    for(int king_y=0;king_y<9;king_y++)
      for(int dir=0;dir<8;dir++)
	for(int mobility=0;mobility<9;mobility++){
	  int oldIndex=king_x + 5 * (king_y + 9 * (dir + 8 * mobility));
	  int newIndex=mobility+9*(dir+8*(king_y+9*king_x));
	  for (int s=0; s<NStages; ++s)
	    KingMobilityVal::rook_table[newIndex][s]=old_table[oldIndex][s];
	}
}

void osl::
KingMobilityWithBishop::setUp(const Weights &weights)
{
  static std::array<MultiInt, 3240> old_table;
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      old_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int king_x=0;king_x<5;king_x++)
    for(int king_y=0;king_y<9;king_y++)
      for(int dir=0;dir<8;dir++)
	for(int mobility=0;mobility<9;mobility++){
	  int oldIndex=king_x + 5 * (king_y + 9 * (dir + 8 * mobility));
	  int newIndex=mobility+9*(dir+8*(king_y+9*king_x));
	  for (int s=0; s<NStages; ++s)
	    KingMobilityVal::bishop_table[newIndex][s]=old_table[oldIndex][s];
	}
  for(int i=0;i<3240;i++){
    KingMobilityVal::rook_bishop_table[i]=
      KingMobilityVal::table[i]+KingMobilityVal::rook_table[i]+KingMobilityVal::bishop_table[i];
    KingMobilityVal::rook_table[i]+=KingMobilityVal::table[i];
    KingMobilityVal::bishop_table[i]+=KingMobilityVal::table[i];
  }
}

template <osl::Player P>
osl::MultiInt osl::
KingMobilityVal::evalOne(const NumEffectState &state)
{
  MultiInt result;
  const Square king = state.kingSquare(P);
  const int king_x = (X(king) > 5 ? 10 - X(king) : X(king)) - 1;
  const int king_y = (P == BLACK ? Y(king) : 10 - Y(king)) - 1;
  int indexBase=9*8*(king_y+9*king_x);
  if(P==BLACK){
    if (state.hasPieceOnStand(alt(P), ROOK))
    {
      if(state.hasPieceOnStand(alt(P), BISHOP)){
	result =
	  rook_bishop_table[indexBase+0*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	  rook_bishop_table[indexBase+1*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	  rook_bishop_table[indexBase+2*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	  rook_bishop_table[indexBase+3*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	  rook_bishop_table[indexBase+4*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	  rook_bishop_table[indexBase+5*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	  rook_bishop_table[indexBase+6*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	  rook_bishop_table[indexBase+7*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))];
      }
      else{
	result =
	  rook_table[indexBase+0*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	  rook_table[indexBase+1*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	  rook_table[indexBase+2*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	  rook_table[indexBase+3*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	  rook_table[indexBase+4*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	  rook_table[indexBase+5*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	  rook_table[indexBase+6*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	  rook_table[indexBase+7*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))];
      }
    }
    else if(state.hasPieceOnStand(alt(P), BISHOP)){
      result = 
	bishop_table[indexBase+0*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	bishop_table[indexBase+1*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	bishop_table[indexBase+2*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	bishop_table[indexBase+3*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	bishop_table[indexBase+4*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	bishop_table[indexBase+5*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	bishop_table[indexBase+6*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	bishop_table[indexBase+7*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))];
    }
    else{
      result = 
	table[indexBase+0*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	table[indexBase+1*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	table[indexBase+2*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	table[indexBase+3*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	table[indexBase+4*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	table[indexBase+5*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	table[indexBase+6*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	table[indexBase+7*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))];
    }
  }
  else{
    if (state.hasPieceOnStand(alt(P), ROOK))
    {
      if(state.hasPieceOnStand(alt(P), BISHOP)){
	result = -(
	  rook_bishop_table[indexBase+7*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	  rook_bishop_table[indexBase+6*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	  rook_bishop_table[indexBase+5*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	  rook_bishop_table[indexBase+4*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	  rook_bishop_table[indexBase+3*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	  rook_bishop_table[indexBase+2*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	  rook_bishop_table[indexBase+1*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	  rook_bishop_table[indexBase+0*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))]);
      }
      else{
	result = -(
	  rook_table[indexBase+7*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	  rook_table[indexBase+6*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	  rook_table[indexBase+5*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	  rook_table[indexBase+4*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	  rook_table[indexBase+3*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	  rook_table[indexBase+2*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	  rook_table[indexBase+1*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	  rook_table[indexBase+0*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))]);
      }
    }
    else if(state.hasPieceOnStand(alt(P), BISHOP)){
      result = -(
	bishop_table[indexBase+7*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	bishop_table[indexBase+6*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	bishop_table[indexBase+5*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	bishop_table[indexBase+4*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	bishop_table[indexBase+3*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	bishop_table[indexBase+2*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	bishop_table[indexBase+1*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	bishop_table[indexBase+0*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))]);
    }
    else{
      result = -(
	table[indexBase+7*9+mobilityDir<UL>(king,state.kingMobilityAbs(P, UL))]+
	table[indexBase+6*9+mobilityDir<U>(king,state.kingMobilityAbs(P, U))]+
	table[indexBase+5*9+mobilityDir<UR>(king,state.kingMobilityAbs(P, UR))]+
	table[indexBase+4*9+mobilityDir<L>(king,state.kingMobilityAbs(P, L))]+
	table[indexBase+3*9+mobilityDir<R>(king,state.kingMobilityAbs(P, R))]+
	table[indexBase+2*9+mobilityDir<DL>(king,state.kingMobilityAbs(P, DL))]+
	table[indexBase+1*9+mobilityDir<D>(king,state.kingMobilityAbs(P, D))]+
	table[indexBase+0*9+mobilityDir<DR>(king,state.kingMobilityAbs(P, DR))]);
    }
  }
  return result;
}  

osl::MultiInt osl::
KingMobilityVal::eval(const NumEffectState &state)
{
  MultiInt result = evalOne<BLACK>(state) + evalOne<WHITE>(state);
  return result;
}  


std::array<MultiInt, 45*33> osl::KingMobilitySum::table;

void osl::KingMobilitySum::setUp(const Weights &weights)
{
  std::array<MultiInt, 2925> old_table;
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      old_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int king_x=0;king_x<5;king_x++)
    for(int king_y=0;king_y<9;king_y++)
      for(int mobility=0;mobility<=32;mobility++){
	int oldIndex=king_x+5*(king_y+9*(mobility+8));
	int newIndex=mobility+33*(king_y+9*king_x);
	for (int s=0; s<NStages; ++s)
	  table[newIndex]=old_table[oldIndex];
      }
}

template <osl::Player P>
osl::MultiInt osl::
KingMobilitySum::evalOne(const NumEffectState &state)
{
  MultiInt result;
  const Square king = state.kingSquare(P);
  int sum=Y(state.kingMobilityAbs(P, UL))+Y(state.kingMobilityAbs(P, U))+
    Y(state.kingMobilityAbs(P, UR))+X(state.kingMobilityAbs(P, R))-
    Y(state.kingMobilityAbs(P, DL))-Y(state.kingMobilityAbs(P, D))-
    Y(state.kingMobilityAbs(P, DR))-X(state.kingMobilityAbs(P, L));
  const int king_x = (X(king) > 5 ? 10 - X(king) : X(king)) - 1;
  const int king_y = (P == BLACK ? Y(king) : 10 - Y(king)) - 1;
  int mobility=sum-8;
  result = table[mobility+33*(king_y+9*king_x)];
  return (P == BLACK ? result : -result);
}  

osl::MultiInt osl::
KingMobilitySum::eval(const NumEffectState &state)
{
  MultiInt result = evalOne<BLACK>(state) + evalOne<WHITE>(state);
  return result;
}  


std::array<MultiInt, 8192>
osl::King25BothSide::table;
std::array<MultiInt, 40960>
osl::King25BothSide::x_table;
std::array<MultiInt, 73728>
osl::King25BothSide::y_table;

void osl::King25BothSide::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25BothSideX::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25BothSide::x_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25BothSideY::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25BothSide::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int king_y=1;king_y<=9;king_y++)
    for(int effect1=0;effect1<32;effect1++)
      for(int effect2=0;effect2<32;effect2++)
	for(int i=0;i<8;i++){
	  int index0=effect1 + 32 * (effect2 + 32 * i);
	  int index1=king_y - 1 + 9 *(effect1 + 32 * (effect2 + 32 * i));
	  King25BothSide::y_table[index1] += King25BothSide::table[index0];
	}
}

template<osl::Player P>
osl::MultiInt osl::
King25BothSide::evalOne(const NumEffectState &state,
			const std::array<uint8_t, 8> &effects
)
{
  const Square king=state.kingSquare(P);
  const int king_y = (P==BLACK ? Y(king) : 10 - Y(king));
  const int king_x = (X(king) >= 6 ? 10 - X(king) : X(king));
  if ((P== BLACK && X(king) > 5) || (P==WHITE && X(king) < 5)){
    return
      x_table[indexX(king_x,effects[2],effects[0],(2*3)+1)]+ // (0,2)
      y_table[indexY(king_y,effects[0],effects[2],(0*3)+0)]+
      x_table[indexX(king_x,effects[3],effects[0],(1*3)+2)]+ // (0,3)
      y_table[indexY(king_y,effects[0],effects[3],(0*3)+1)]+
      x_table[indexX(king_x,effects[4],effects[0],(0*3)+2)]+ // (0,4)
      y_table[indexY(king_y,effects[0],effects[4],(0*3)+2)]+
      x_table[indexX(king_x,effects[2],effects[1],(2*3)+0)]+ // (1,2)
      y_table[indexY(king_y,effects[1],effects[2],(1*3)+0)]+
      x_table[indexX(king_x,effects[3],effects[1],(1*3)+1)]+ // (1,3)
      y_table[indexY(king_y,effects[1],effects[3],(1*3)+1)]+
      x_table[indexX(king_x,effects[4],effects[1],(0*3)+1)]+ // (1,4)
      y_table[indexY(king_y,effects[1],effects[4],(1*3)+2)]+
      x_table[indexX(king_x,effects[3],effects[2],(1*3)+0)]+ // (2,3)
      y_table[indexY(king_y,effects[2],effects[3],(2*3)+0)]+
      x_table[indexX(king_x,effects[4],effects[2],(0*3)+0)]+ // (2,4)
      y_table[indexY(king_y,effects[2],effects[4],(2*3)+1)];
  }
  else{
    return
      x_table[indexX(king_x,effects[0],effects[2],(0*3)+0)]+ // (0,2)
      y_table[indexY(king_y,effects[0],effects[2],(0*3)+0)]+
      x_table[indexX(king_x,effects[0],effects[3],(0*3)+1)]+ // (0,3)
      y_table[indexY(king_y,effects[0],effects[3],(0*3)+1)]+
      x_table[indexX(king_x,effects[0],effects[4],(0*3)+2)]+ // (0,4)
      y_table[indexY(king_y,effects[0],effects[4],(0*3)+2)]+
      x_table[indexX(king_x,effects[1],effects[2],(1*3)+0)]+ // (1,2)
      y_table[indexY(king_y,effects[1],effects[2],(1*3)+0)]+
      x_table[indexX(king_x,effects[1],effects[3],(1*3)+1)]+ // (1,3)
      y_table[indexY(king_y,effects[1],effects[3],(1*3)+1)]+
      x_table[indexX(king_x,effects[1],effects[4],(1*3)+2)]+ // (1,4)
      y_table[indexY(king_y,effects[1],effects[4],(1*3)+2)]+
      x_table[indexX(king_x,effects[2],effects[3],(2*3)+0)]+ // (2,3)
      y_table[indexY(king_y,effects[2],effects[3],(2*3)+0)]+
      x_table[indexX(king_x,effects[2],effects[4],(2*3)+1)]+ // (2,4)
      y_table[indexY(king_y,effects[2],effects[4],(2*3)+1)];
  }
}

osl::MultiInt osl::
King25BothSide::eval(const NumEffectState &state,
		     const std::array<uint8_t, 8> &black,
		     const std::array<uint8_t, 8> &white
  )
{
  return evalOne<BLACK>(state,black)-evalOne<WHITE>(state,white);
}

std::array<MultiInt, 2400>
osl::King25Effect3::table;
std::array<MultiInt, 21600>
osl::King25Effect3::y_table;

void osl::King25Effect3::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25Effect3Y::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25Effect3::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template <osl::Player Attack>
osl::MultiInt osl::
King25Effect3::evalOne(const NumEffectState &state,
		       PieceMask king25)
{
  king25 = king25 & state.piecesOnBoard(Attack);
  const bool with_knight =
    any(selectPtype(king25 & ~state.promotedPieces(), KNIGHT));
  king25 &= ~(~state.promotedPieces() & (pieceMask(KNIGHT) | pieceMask(LANCE) | pieceMask(PAWN)));
  const int piece_count = std::min(9, countBit(king25));
  const int stand_count = std::min(9,
				   state.countPiecesOnStand(Attack, ROOK) +
				   state.countPiecesOnStand(Attack, BISHOP) +
				   state.countPiecesOnStand(Attack, GOLD) +
				   state.countPiecesOnStand(Attack, SILVER));
  const bool stand_with_knight = state.hasPieceOnStand(Attack, KNIGHT);
  constexpr Player Defense = alt(Attack);
  PieceMask attacked =
    state.effectedMask(Attack) & state.piecesOnBoard(Defense);
  attacked &= ~(pieceMask(KNIGHT) | pieceMask(LANCE) | pieceMask(PAWN));
  PieceMask attacking=PieceMask(0);
  while (any(attacked))
  {
    const Piece piece = state.pieceOf(takeOneBit(attacked));
    attacking = attacking | state.effect(square(piece));
  }
  attacking = (attacking & state.piecesOnBoard(Attack) & ~king25);
  const int attacked_count = std::min(5, countBit(attacking));
  if (Attack == BLACK)
  {
    return table[index(piece_count, with_knight,
		       stand_count, stand_with_knight, attacked_count)] +
      y_table[indexY(piece_count, with_knight,
		     stand_count, stand_with_knight, attacked_count,
		     Y(state.kingSquare(WHITE)))];
  }
  else
  {
    return -(table[index(piece_count, with_knight,
			 stand_count, stand_with_knight, attacked_count)] +
	     y_table[indexY(piece_count, with_knight,
			    stand_count, stand_with_knight, attacked_count,
			    10 - Y(state.kingSquare(BLACK)))]);
  }
}

osl::MultiInt osl::
King25Effect3::eval(const NumEffectState &state,
		    const std::array<PieceMask, 2> &king25_mask)
{
  return evalOne<BLACK>(state, king25_mask[I(WHITE)]) +
    evalOne<WHITE>(state, king25_mask[I(BLACK)]);
}


std::array<MultiInt, 4096>
osl::King25Mobility::table;
std::array<MultiInt, 20480>
osl::King25Mobility::x_table;
std::array<MultiInt, 36864>
osl::King25Mobility::y_table;

void osl::King25Mobility::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25MobilityX::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25Mobility::x_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25MobilityY::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25Mobility::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
King25Mobility::eval(const NumEffectState &state,
		     const std::array<uint8_t, 8> &black,
		     const std::array<uint8_t, 8> &white
)
{
  const Square king_b=state.kingSquare(BLACK);
  const Square king_w=state.kingSquare(WHITE);
  MultiInt result;
  for (size_t i = 1; i < 5; ++i)
  {
    result += (table[index(black[i - 1], black[i], i - 1)] +
	       x_table[indexX<BLACK>(king_b,
				     black[i - 1], black[i], i - 1)] +
	       y_table[indexY<BLACK>(king_b,
				     black[i - 1], black[i], i - 1)]);
    result -= (table[index(white[i - 1], white[i], i - 1)] +
	       x_table[indexX<WHITE>(king_w,
				     white[i - 1], white[i], i - 1)] +
	       y_table[indexY<WHITE>(king_w,
				     white[i - 1], white[i], i - 1)]);
  }
  return result;
}


std::array<MultiInt, 100>
osl::King25EffectCountCombination::table;
std::array<MultiInt, 900>
osl::King25EffectCountCombination::y_table;

void osl::King25EffectCountCombination::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::King25EffectCountCombinationY::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      King25EffectCountCombination::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template <osl::Player Attack>
osl::MultiInt osl::
King25EffectCountCombination::evalOne(const NumEffectState &state,
				      PieceMask king25)
{
  constexpr Player Defense = alt(Attack);
  PieceMask attack = king25 & state.piecesOnBoard(Attack);
  PieceMask defense =
    king25 & state.piecesOnBoard(Defense);
  attack &= ~(~state.promotedPieces() & (pieceMask(KNIGHT) | pieceMask(LANCE) | pieceMask(PAWN)));
  defense &= ~(~state.promotedPieces() & (pieceMask(KNIGHT) | pieceMask(LANCE) | pieceMask(PAWN)));
  const int attack_count = std::min(9,
				    countBit(attack) +
				    state.countPiecesOnStand(Attack, ROOK) +
				    state.countPiecesOnStand(Attack, BISHOP) +
				    state.countPiecesOnStand(Attack, GOLD) +
				    state.countPiecesOnStand(Attack, SILVER));
  const int defense_count = std::min(9,
				     countBit(defense) +
				     state.countPiecesOnStand(Defense, ROOK) +
				     state.countPiecesOnStand(Defense, BISHOP) +
				     state.countPiecesOnStand(Defense, GOLD) +
				     state.countPiecesOnStand(Defense, SILVER));
  int y = (Attack == BLACK ? Y(state.kingSquare(Defense)) :
	   10 - Y(state.kingSquare(Defense)));
  MultiInt result = table[attack_count + 10 * defense_count] +
    y_table[y - 1 + 9 * (attack_count + 10 * defense_count)];
  if (Attack == BLACK)
    return result;
  else
    return -result;
}

osl::MultiInt osl::
King25EffectCountCombination::eval(const NumEffectState &state,
				   const std::array<PieceMask, 2> &king25_mask)
{
  return evalOne<BLACK>(state, king25_mask[I(WHITE)]) +
    evalOne<WHITE>(state, king25_mask[I(BLACK)]);
}


std::array<int, 18*81*(45*2)*3> osl::BishopExchangeSilverKing::table;
void osl::
BishopExchangeSilverKing::setUp(const Weights & weights)
{
  for (size_t i=0; i<weights.dimension(); ++i)
    table[i] = weights.value(i);
}

int osl::
BishopExchangeSilverKing::eval(const NumEffectState &state)
{
  if (any(state.promotedPieces()))
    return 0;
  PieceMask stand_all = state.standMask(BLACK) | state.standMask(WHITE);
  stand_all &= ~(pieceMask(BISHOP) | pieceMask(PAWN));
  if (any(stand_all))
    return 0;
  if (owner(state.nth(BISHOP, 0)) == owner(state.nth(BISHOP, 1)))
    return 0;
  if (isOnBoard(state.nth(BISHOP, 0)) != isOnBoard(state.nth(BISHOP, 1)))
    return 0;
  int offset = 0;
  if (isOnBoard(state.nth(BISHOP, 0))) {
    offset += BISHOP_ONE_DIM;
    if (state.hasEffectByPiece(state.nth(BISHOP, 0),
			       square(state.nth(BISHOP, 1))))
      offset += BISHOP_ONE_DIM;
  }
  return evalOne<BLACK>(state, offset) + evalOne<WHITE>(state, offset);
}

template <osl::Player KingOwner>
int osl::
BishopExchangeSilverKing::evalOne(const NumEffectState &state, int offset)
{
  Square king = state.kingSquare(KingOwner);
  int king_index = indexKing(forBlack(alt(KingOwner),king)); // rotate if king is black
  if (king_index < 0)
    return 0;
  std::array<Piece,2> rook;
  rook[0]=state.nth(ROOK, 0);
  rook[1]=state.nth(ROOK, 1);
  if (owner(rook[0]) == owner(rook[1]))
    return 0;
  if (owner(rook[0]) == KingOwner)
    std::swap(rook[0], rook[1]);
  int rook_index0 = indexKing(forBlack(KingOwner,square(rook[0]))); // rotate if attaking rook is black
  if (rook_index0 < 0)
    return 0;
  int rook_index1 = indexKing(forBlack(alt(KingOwner),square(rook[1]))); // rotate if defending rook is black
  if (rook_index1 < 0)
    return 0;  
  FixedCapacityVector<Square, 4> silver;
  for(Square sq : state.allSquare(alt(KingOwner), SILVER)){
    silver.push_back(forBlack(KingOwner, sq));
  }
  if (silver.size() != 2 || X(silver[0]) == X(silver[1]))
    return 0;
  int silver_index
    = indexSilver((X(silver[0]) > X(silver[1])) ? silver[0] : silver[1]);
  int index = offset + (king_index*81+silver_index)*90;
  return table[index + rook_index0] * sign(KingOwner)
    + table[index + 45 + rook_index1] * sign(KingOwner);
}




template <osl::Player KingOwner>
int osl::
EnterKingDefense::evalOne(const NumEffectState &state)
{
  Square king = state.kingSquare(KingOwner);
  if (Y(king) < 4 || Y(king) > 6) // target: [4,6]
    return 0;
  std::array<std::array<int, 2>, 2> count; 
  for(auto& v: count) v.fill(0);
  for (int x=std::max(1, X(king)-2); x<=std::min(9, X(king)+2); ++x) {
    for (int y=Y(king)-2*sign(KingOwner); 1; y-=sign(KingOwner)) {
      const Piece p = state.pieceAt(newSquare(x, y));
      if (isEdge(p))
	break;
      if (isEmpty(p))
	continue;
      count[owner(p) == KingOwner ? 1 : 0 ][osl::ptype(p) == PAWN ? 1 : 0]++;
    }
  }
  const int c = Y(forBlack(KingOwner,king)) - 4;
  return table[c*(std::min(7, count[0][0]))] * sign(KingOwner)
    + table[c*(8 +std::min(7, count[0][1]))] * sign(KingOwner)
    + table[c*(16+std::min(7, count[1][0]))] * sign(KingOwner)
    + table[c*(24+std::min(7, count[1][1]))] * sign(KingOwner);
}

int osl::
EnterKingDefense::eval(const NumEffectState &state)
{
  return evalOne<BLACK>(state) + evalOne<WHITE>(state);
}

std::array<int, (8+8+8+8)*3> osl::EnterKingDefense::table;
void osl::
EnterKingDefense::setUp(const Weights & weights)
{
  for (size_t i=0; i<weights.dimension(); ++i)
    table[i] = weights.value(i);
}



namespace osl
{

  template MultiInt King25BothSide::
  evalOne<BLACK>(const NumEffectState &state, 
    const std::array<uint8_t, 8> &effects
);
  template MultiInt King25BothSide::
  evalOne<WHITE>(const NumEffectState &state, 
const std::array<uint8_t, 8> &effects
);
}


std::array<osl::MultiInt, 81>
osl::PawnDropAll::attack_y_table;
std::array<osl::MultiInt, 81>
osl::PawnDropAll::defense_y_table;
std::array<osl::MultiInt, 90>
osl::PawnDropAll::x_table;
std::array<osl::MultiInt, 90>
osl::PawnDropAll::x_stand_table;
std::array<osl::MultiInt, 81 * 4> osl::PawnDropAll::xx_table;
std::array<osl::MultiInt, 9 * 17 * 4> osl::PawnDropAll::yy_table;
std::array<osl::MultiInt, 162>
osl::PawnDropAll::y_stand_table;
std::array<osl::MultiInt, 9>
osl::PawnDropAll::drop_non_drop_table;
std::array<osl::MultiInt, 36>
osl::PawnDropAll::state_king_relative_table;

void osl::PawnDropAll::setUp()
{
  for(int y = 1; y <= 9; y++)
    for(int abs_x = 0; abs_x <= 8; abs_x++){
      int i = abs_x, i_y = abs_x * 9 + y - 1;
      attack_y_table[i_y] = PawnDropY::table[i_y] + PawnDrop::table[i + 9];
      defense_y_table[i_y] = PawnDropY::table[i_y + 81] + PawnDrop::table[i];
    }
  for (size_t i = 0; i < PawnDropX::table.size(); ++i){
    x_table[i] = PawnDropX::table[i];
  }
  for (size_t i = 0; i < PawnDropPawnStandX::table.size(); ++i){
    x_stand_table[i] = PawnDropPawnStandX::table[i];
  }
  for (int king_x = 1; king_x <= 9; king_x++)
    for (int x = 1; x <= 9; x++){
      int i = (king_x - 1) * 9 + (x - 1);
      int target_x = (king_x > 5 ? 10 - king_x : king_x);
      int orig_i = ((king_x > 5 ? 10 - x : x) - 1) * 5 + target_x - 1;
      xx_table[i] = PawnDropX::table[orig_i];
      xx_table[i + 81] = PawnDropX::table[orig_i + 45];
      xx_table[i + 81 * 2] = PawnDropX::table[orig_i] + PawnDropPawnStandX::table[orig_i];
      xx_table[i + 81 * 3] = PawnDropX::table[orig_i + 45] + PawnDropPawnStandX::table[orig_i + 45];
    }
  for(int j = 0; j < 2; j++)
    for(int y = 1; y <= 9; y++){
      for(int abs_x = 0; abs_x <= 8; abs_x++){
	int i = abs_x + j * 9, i_y = abs_x * 9 + y - 1 + j * 81;
	y_stand_table[i_y] = PawnDropPawnStandY::table[i_y] + PawnDropPawnStand::table[i];
      }
      for(int dx = -8 ; dx <= 8; dx++){
	int abs_x = abs(dx);
	int i = abs_x + j * 9, i_y = abs_x * 9 + y - 1 + j * 81;
	int i_y_base = (y - 1) * 17 + dx + 8;
	yy_table[i_y_base + 17 * 9 * j] = PawnDropY::table[i_y] + PawnDrop::table[abs_x + 9 - 9 * j];
	yy_table[i_y_base + 17 * 9 * (j + 2)] = yy_table[i_y_base + 17 * 9 * j] + PawnDropPawnStandY::table[i_y] + PawnDropPawnStand::table[i];
      }
    }
  for(int x = 1; x <= 9; x++){
    int i = (x > 5 ? 10 - x : x) - 1;
    drop_non_drop_table[x - 1] = PawnDropNonDrop::table[i] - PawnDropNonDrop::table[i + 5];
  }
  for (size_t i = 0; i < PawnStateKingRelative::table.size(); ++i){
    state_king_relative_table[i] = PawnStateKingRelative::table[i];
  }
}


int osl::PawnDropAll::indexY(Player Owner, Square king, int x)
{
  const int king_y = (Owner == BLACK ?
		      Y(king) : 10 - Y(king));
  return std::abs(x - X(king)) * 9 + king_y - 1;
}

int osl::PawnDropAll::indexX(Player Owner, bool Attack, Square king, int x)
{
  const int king_x = X(king);
  const int target_x = (king_x > 5 ? 10 - king_x : king_x);
  if (king_x >= 6 || (Owner == WHITE && king_x == 5))
    x = 10 - x;
  return (x - 1) * 5 + target_x - 1 + (Attack ? 0 : 45);
}

MultiInt osl::PawnDropAll::value(
  int attack_index_y, int defense_index_y,
  int attack_index_x, int defense_index_x)
{
  return (attack_y_table[attack_index_y] +
	  defense_y_table[defense_index_y] +
	  x_table[attack_index_x] +
	  x_table[defense_index_x]);
}

MultiInt osl::PawnDropAll::standValue(
  int attack_index_y, int defense_index_y,
  int attack_index_x, int defense_index_x)
{
  return (y_stand_table[attack_index_y] +
	  y_stand_table[defense_index_y + 81] +
	  x_stand_table[attack_index_x] +
	  x_stand_table[defense_index_x]);
}
template<osl::Player P>
MultiInt osl::PawnDropAll::evalWithUpdate(const NumEffectState &state,
					  Move moved,
					  MultiInt &last_value)
{
  constexpr Player altP=alt(P);
  Ptype captured = capturePtype(moved);
  if (ptype(moved) == KING ||
      (isDrop(moved) && ptype(moved) == PAWN &&
       !state.hasPieceOnStand(P, PAWN)) ||
      (captured != Ptype::EMPTY &&
       unpromote(captured) == PAWN &&
       state.countPiecesOnStand(P,PAWN) == 1))
  {
    return eval(state);
  }
  
  MultiInt result(last_value);
  if (oldPtype(moved) == PAWN) 
  {
    if (isDrop(moved))
    {
      const int attack_index_x =
	indexX(alt(P), true, state.kingSquare(alt(P)), X(to(moved)));
      const int defense_index_x =
	indexX(P, false, state.kingSquare(P), X(to(moved)));
      
      const int attack_index_y = indexY(altP, state.kingSquare(alt(P)), X(to(moved)));
      const int defense_index_y = indexY(P, state.kingSquare(P), X(to(moved)));
      if (state.isPawnMaskSet(altP, X(to(moved))))
      {
	result.addFor(P, drop_non_drop_table[X(to(moved)) - 1]);
	result.addFor(P,
		      state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) - X(to(moved))) +
						SELF_ON_BOARD * 9]);
	result.addFor(alt(P),
		      state_king_relative_table[std::abs(X(state.kingSquare(P)) - X(to(moved))) +
						OPP_ON_BOARD * 9]);
	for(Player P1 : COLORS){
	  result.addFor(P1,
			state_king_relative_table[std::abs(X(state.kingSquare(P1)) -
					     X(to(moved))) +
						  BOTH_ON_BOARD * 9]);
	}
      }
      else
      {
	for(Player P1 : COLORS){
	  result.addFor(alt(P1),
			state_king_relative_table[std::abs(X(state.kingSquare(P1)) -
							   X(to(moved))) +
						  BOTH_ON_STAND * 9]);
	}
	result.addFor(P, drop_non_drop_table[X(to(moved)) - 1]);
	result.addFor(P, 
	    state_king_relative_table[std::abs(X(state.kingSquare(P)) - X(to(moved))) +
				      SELF_ON_BOARD * 9]);
	result.addFor(alt(P),
		      state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) - X(to(moved))) +
						OPP_ON_BOARD * 9]);
	
      }
      result.addFor(alt(P),value(attack_index_y, 
				defense_index_y, attack_index_x, defense_index_x));
      if (state.hasPieceOnStand(P, PAWN))
      {
	result.addFor(alt(P), standValue(attack_index_y, 
					 defense_index_y, attack_index_x, defense_index_x));
      }
    }
    if (isPromotion(moved))
    {
      const int attack_index_x =
	indexX(alt(P), true, state.kingSquare(alt(P)), X(to(moved)));
      const int defense_index_x =
	indexX(P, false, state.kingSquare(P), X(to(moved)));
      const int attack_index_y = indexY(altP, state.kingSquare(alt(P)), X(to(moved)));
      const int defense_index_y = indexY(P, state.kingSquare(P), X(to(moved)));
      result.addFor(P, value(attack_index_y, 
			     defense_index_y, attack_index_x, defense_index_x));
      if (state.hasPieceOnStand(P, PAWN))
      {
	result.addFor(P, standValue(attack_index_y, 
				    defense_index_y, attack_index_x, defense_index_x));
      }
      if (state.isPawnMaskSet(altP, X(to(moved))))
      {
	for(Player P1 : COLORS)
	  result.addFor(alt(P1),
	  state_king_relative_table[std::abs(X(state.kingSquare(P1)) - X(to(moved))) +
				    BOTH_ON_BOARD * 9]);
	result.addFor(alt(P), drop_non_drop_table[X(to(moved)) - 1]);
	result.addFor(P,
	    state_king_relative_table[std::abs(X(state.kingSquare(P)) - X(to(moved))) +
				      OPP_ON_BOARD * 9]);
	result.addFor(alt(P),
		      state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) - X(to(moved))) +
						SELF_ON_BOARD * 9]);
      }
      else
      {
	for(Player P1 : COLORS)
	  result.addFor(P1,
			state_king_relative_table[std::abs(X(state.kingSquare(P1)) - X(to(moved))) +
						  BOTH_ON_STAND * 9]);
	if (captured == PAWN)
	{
	for(Player P1 : COLORS)
	  result.addFor(alt(P1), state_king_relative_table[std::abs(X(state.kingSquare(P1)) - X(to(moved))) +
							   BOTH_ON_BOARD * 9]);
	}
	else
	{
	  result.addFor(alt(P), drop_non_drop_table[X(to(moved)) - 1]);
	  result.addFor(alt(P),
	      state_king_relative_table[std::abs(X(state.kingSquare(P)) - X(to(moved))) +
					SELF_ON_BOARD * 9]);
	  result.addFor(P,
			state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) - X(to(moved))) +
						  OPP_ON_BOARD * 9]);
	}
      }
    }
  }

  if (captured == PAWN)
  {
    const int attack_index_x =
      indexX(P, true, state.kingSquare(P), X(to(moved)));
    const int defense_index_x =
      indexX(alt(P), false, state.kingSquare(alt(P)), X(to(moved)));
    const int attack_index_y = indexY(P, state.kingSquare(P), X(to(moved)));
    const int defense_index_y = indexY(altP, state.kingSquare(alt(P)), X(to(moved)));
    result.addFor(alt(P),value(attack_index_y, 
			       defense_index_y, attack_index_x, defense_index_x));
    if (state.hasPieceOnStand(alt(P), PAWN))
    {
      result.addFor(alt(P), standValue(attack_index_y, 
				       defense_index_y, attack_index_x, defense_index_x));
    }
    if (!(ptype(moved) == PPAWN && isPromotion(moved))) // promote is already handled above
    {
      if (state.isPawnMaskSet(P, X(to(moved))))
      {
	for(Player P1 : COLORS)
	  result.addFor(alt(P1),
			state_king_relative_table[std::abs(X(state.kingSquare(P1)) -
					     X(to(moved))) +
						  BOTH_ON_BOARD * 9]);
	result.addFor(P, drop_non_drop_table[X(to(moved)) - 1]);
	result.addFor(P,
	  state_king_relative_table[std::abs(X(state.kingSquare(P)) -
					     X(to(moved))) +
				    SELF_ON_BOARD * 9]);
	result.addFor(alt(P),
		      state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) -
					     X(to(moved))) +
						OPP_ON_BOARD * 9]);
      }
      else
      {
	result.addFor(P, drop_non_drop_table[X(to(moved)) - 1]);
	result.addFor(alt(P),
	    state_king_relative_table[std::abs(X(state.kingSquare(P)) - X(to(moved))) +
				      OPP_ON_BOARD * 9]);
	result.addFor(P,
		      state_king_relative_table[std::abs(X(state.kingSquare(alt(P))) - X(to(moved))) +
						SELF_ON_BOARD * 9]);
	for(Player P1 : COLORS)
	  result.addFor(P1,
			state_king_relative_table[std::abs(X(state.kingSquare(P1)) - X(to(moved))) +
						  BOTH_ON_STAND * 9]);
      }
    }
  }
  return result;
}

osl::MultiInt osl::PawnDropAll::eval(
  const NumEffectState &state)
{
  osl::MultiInt result;
  for(Player P : COLORS){
    int offset_xx = (state.hasPieceOnStand(P, PAWN) ? 81 * 2 : 0); 
    int offset_yy = (state.hasPieceOnStand(P, PAWN) ? 17 * 9 * 2 : 0); 
    int attack_index_yy = (Y(state.kingSquare(alt(P))) - 1) * 17 - X(state.kingSquare(alt(P))) + 8;
    attack_index_yy = (alt(P) == BLACK ? attack_index_yy : 17 * 9 - 1 - attack_index_yy) + offset_yy;
    int defense_index_yy = (Y(state.kingSquare(P)) - 1) * 17 - X(state.kingSquare(P)) + 8 ;
    defense_index_yy = (P == BLACK ? defense_index_yy : 17 * 9 - 1 - defense_index_yy) + 17 * 9 + offset_yy;
    int attack_index_xx = (X(state.kingSquare(alt(P))) - 1) * 9 - 1;
    attack_index_xx = (alt(P) == BLACK ? attack_index_xx : 80 - attack_index_xx) + offset_xx;
    int defense_index_xx = (X(state.kingSquare(P)) - 1) * 9 - 1;
    defense_index_xx = (P == BLACK ? defense_index_xx: 80 - defense_index_xx) + 81 + offset_xx;
    for (int x = 1; x <= 9; ++x){
      if (!state.isPawnMaskSet(P, x)){
	result.addFor(P,
		      xx_table[attack_index_xx  + (alt(P) == BLACK ? x : -x)] +
		      xx_table[defense_index_xx + (P == BLACK ? x : -x)] +
		      yy_table[attack_index_yy + (alt(P) == BLACK ? x : -x)] +
		      yy_table[defense_index_yy + (P == BLACK ? x : -x)]);
      }
    }
  }
  for (int x = 1; x <= 9; ++x)
  {
    if(state.isPawnMaskSet(BLACK, x)){
      if(state.isPawnMaskSet(WHITE, x)){
	for(Player P : COLORS)
	  result.addFor(P, state_king_relative_table[std::abs(X(state.kingSquare(P)) - x) + BOTH_ON_BOARD * 9]);
      }
      else{
	result += drop_non_drop_table[x - 1];
	result +=
	  state_king_relative_table[std::abs(X(state.kingSquare(BLACK)) - x) +
				    SELF_ON_BOARD * 9];
	result -=
	  state_king_relative_table[std::abs(X(state.kingSquare(WHITE)) - x) +
				    OPP_ON_BOARD * 9];
      }
    }
    else if(state.isPawnMaskSet(WHITE, x)){
      result -= drop_non_drop_table[x - 1];
      result +=
	state_king_relative_table[std::abs(X(state.kingSquare(BLACK)) - x) +
				  OPP_ON_BOARD * 9];
      result -=
	state_king_relative_table[std::abs(X(state.kingSquare(WHITE)) - x) +
				  SELF_ON_BOARD * 9];
    }
    else{
      for(Player P : COLORS)
	result.addFor(P,state_king_relative_table[std::abs(X(state.kingSquare(P)) - x) +
						  BOTH_ON_STAND * 9]);
    }
  }
  return result;
}



osl::MultiInt osl::NoPawnOnStand::weight;

void osl::
NoPawnOnStand::setUp(const Weights &weights,int stage)
{
  weight[stage] = weights.value(0);
}



std::array<osl::MultiInt, 9> osl::PawnAdvance::table;

void osl::
PawnAdvance::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i) {
    table[i][stage] = weights.value(i);
  }
}

osl::MultiInt osl::PawnAdvance::eval(
  const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(PAWN);
       i < indexLimit(PAWN); ++i)
  {
    const Piece pawn = state.pieceOf(i);
    if (isOnBoard(pawn) && !isPromoted(pawn) &&
	cantAdvance(state, pawn))
    {
      result.addFor(owner(pawn), table[index(owner(pawn), square(pawn))]);
    }
  }
  return result;
}

template <osl::Player P> inline
void osl::
PawnAdvanceAll::adjust(int index, MultiInt& values)
{
  if(P==BLACK)
    values += PawnAdvance::table[index];
  else
    values -= PawnAdvance::table[index];
}

template <osl::Player P>
void osl::
PawnAdvanceAll::evalWithUpdateBang(const osl::NumEffectState &state,
				   osl::Move moved, MultiInt& values)
{
  assert(player(moved) == P);
  if (osl::ptype(moved) == PAWN)
  {
    if (cantAdvance(state, osl::ptypeO(moved), to(moved)))
    {
      adjust<P>(index(P, to(moved)), values);
      return;
    }
  }
  constexpr Player Opponent = alt(P);
  Ptype captured = capturePtype(moved);
  if (captured == PAWN)
  {
    if (cantAdvance(state, capturePtypeO(moved), to(moved)))
      adjust<P>(index(Opponent, to(moved)), values);
  }
  else if (captured != Ptype::EMPTY)
  {
    const Piece piece = state.pieceAt(
      to(moved) + newOffset( Opponent,D));
    if (isPlayerPtype(piece,Opponent,PAWN))
      adjust<P>(index(Opponent, square(piece)), values);
  }
  if (!isDrop(moved))
  {
    const Piece piece = state.pieceAt(
      from(moved) + newOffset( P,D));
    if (isPlayerPtype(piece,P,PAWN))
      adjust<Opponent>(index(P, square(piece)), values);
  }
  {
    const Piece piece = state.pieceAt(
      to(moved)+newOffset(P,D));
    if (isPlayerPtype(piece,P,PAWN))
      adjust<P>(index(P, square(piece)), values);
  }
}

std::array<MultiInt, 9> osl::KnightAdvance::table;

template<osl::Player P>
inline
bool osl::
KnightAdvance::cantAdvance(const osl::NumEffectState &state,
			   const osl::Piece knight)
{
  // knight 縺九ｙ謨ｵ髯｣荳谿ｵ逶ｮ縺ｫ縺・↑縺・→莉ｮ螳・
  // 繧ゅ＠縺・ｋ蝣ｴ蜷医・Square(1,1)縺ｮUUR縺九ｙ鬧貞床縺ｫ陦晉ｪ・
  assert(P==owner(knight));
  Square uul = square(knight)+newOffset(P,UUL);
  const Piece puul = state.pieceAt(uul);
  if (!canMoveOn(P,puul))
  {
    Square uur = square(knight)+newOffset(P,UUR);
    const Piece puur = state.pieceAt(uur);
    if (!canMoveOn(P,puur))
      return true;
  }
  return false;
}

void osl::
KnightAdvance::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i) {
    table[i][stage] = weights.value(i);
  }
}

MultiInt osl::KnightAdvance::eval(
  const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(KNIGHT);
       i < indexLimit(KNIGHT); ++i)
  {
    const Piece knight = state.pieceOf(i);
    if (!isOnBoard(knight) || isPromoted(knight)) continue;
    if (owner(knight) == BLACK){
      if(cantAdvance<BLACK>(state,knight))
	result += table[index(BLACK, square(knight))];
    }
    else if(cantAdvance<WHITE>(state,knight)){
      result -= table[index(WHITE, square(knight))];
    }
  }
  return result;
}




std::array<MultiInt, 144> osl::PtypeY::table;

void osl::
PtypeY::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i)
  {
    table[i][stage] = weights.value(i);
  }
}

MultiInt osl::PtypeY::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = 0; i < Piece_SIZE; ++i)
  {
    const Piece p = state.pieceOf(i);
    if (!isOnBoard(p))
      continue;
    if (owner(p) == BLACK)
      result += table[index(BLACK,osl::ptype(p),square(p))];
    else
      result -= table[index(WHITE,osl::ptype(p),square(p))];
  }
  return result;
}

template<osl::Player P>
MultiInt osl::
PtypeY::evalWithUpdate(const NumEffectState &, Move moved,
		       MultiInt const& last_value)
{
  MultiInt result(last_value);

  if (!isDrop(moved))
  {
    if (P == BLACK)
      result -= table[index(BLACK, oldPtype(moved), from(moved))];
    else
      result += table[index(WHITE, oldPtype(moved), from(moved))];
  }
  Ptype captured = capturePtype(moved);
  if (captured != Ptype::EMPTY)
  {
    const MultiInt weight =
      table[index(alt(P), captured, to(moved))];
    if (P == BLACK)
      result += weight;
    else
      result -= weight;
  }
  {
    if (P == BLACK)
      result += table[index(BLACK, osl::ptype(moved), to(moved))];
    else
      result -= table[index(WHITE, osl::ptype(moved), to(moved))];
  }

  return result;
}


std::array<MultiInt, 80> osl::PtypeX::table;

void osl::
PtypeX::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i)
  {
    table[i][stage] = weights.value(i);
  }
}

MultiInt osl::PtypeX::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = 0; i < Piece_SIZE; ++i)
  {
    const Piece p = state.pieceOf(i);
    if (!isOnBoard(p))
      continue;
    if (owner(p) == BLACK)
      result += table[index(BLACK,osl::ptype(p),square(p))];
    else
      result -= table[index(WHITE,osl::ptype(p),square(p))];
  }
  return result;
}

template<osl::Player P>
MultiInt osl::
PtypeX::evalWithUpdate(const NumEffectState &, Move moved,
		       MultiInt const& last_value)
{
  MultiInt result(last_value);

  if (!isDrop(moved))
  {
    if (P == BLACK)
      result -= table[index(BLACK, oldPtype(moved), from(moved))];
    else
      result += table[index(WHITE, oldPtype(moved), from(moved))];
    Ptype captured = capturePtype(moved);
    if (captured != Ptype::EMPTY)
    {
      if (P == BLACK)
	result += table[index(WHITE, captured, to(moved))];
      else
	result -= table[index(BLACK, captured, to(moved))];
    }
  }
  if (P == BLACK)
    result += table[index(BLACK, osl::ptype(moved), to(moved))];
  else
    result -= table[index(WHITE, osl::ptype(moved), to(moved))];
  return result;
}


MultiInt osl::KnightCheck::weight;
std::array<MultiInt, 9> osl::KnightCheck::y_table;

void osl::KnightCheck::setUp(const Weights &weights,int stage)
{
  KnightCheck::weight[stage] = weights.value(0);
}

void osl::
KnightCheckY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      KnightCheck::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
KnightCheck::eval(const NumEffectState &state)
{
  MultiInt result;
  if (canCheck<BLACK>(state))
  {
    const int index_y = indexY<BLACK>(Y(state.kingSquare(BLACK)));
    result += value(index_y);
  }
  if (canCheck<WHITE>(state))
  {
    const int index_y = indexY<WHITE>(Y(state.kingSquare(WHITE)));
    result -= value(index_y);
  }
  return result;
}

std::array<MultiInt, 1024> osl::PawnPtypeOPtypeO::table;
std::array<MultiInt, 9216> osl::PawnPtypeOPtypeO::y_table;

void osl::
PawnPtypeOPtypeO::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
PawnPtypeOPtypeOY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      PawnPtypeOPtypeO::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
PawnPtypeOPtypeO::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(PAWN);
       i < indexLimit(PAWN); ++i)
  {
    Piece pawn = state.pieceOf(i);
    if (isOnBoard(pawn) && !isPromoted(pawn))
    {
      const Square up = nextSquare(owner(pawn), square(pawn), U);
      const Square up_up = nextSquare(owner(pawn), up, U);
      PtypeO up_p =
	(isOnBoard(up) ? osl::ptypeO(state.pieceAt(up)) : PtypeO::EDGE);
      PtypeO up_up_p =
	(isOnBoard(up_up) ? osl::ptypeO(state.pieceAt(up_up)) : PtypeO::EDGE);
      const int idx = index(owner(pawn), up_p, up_up_p);
      const int idx_y = indexY(owner(pawn), up_p, up_up_p,
			       Y(square(pawn)));
      if (owner(pawn) == BLACK)
	result += table[idx] + y_table[idx_y];
      else
	result -= table[idx] + y_table[idx_y];
    }
  }
  return result;
}

template<osl::Player P>
MultiInt
osl::
PawnPtypeOPtypeO::evalWithUpdate(const NumEffectState &state, Move moved,
				 const MultiInt &last_value)
{
  assert(player(moved)==P);
  MultiInt result(last_value);
  if (!isDrop(moved))
  {
    if (oldPtype(moved) == PAWN)
    {
      const Square up_up = to(moved) + newOffset(P,U);
      const PtypeO up_up_p =
	(isOnBoard(up_up) ? osl::ptypeO(state.pieceAt(up_up)) : PtypeO::EDGE);
      const int i = index(P, capturePtypeOSafe(moved), up_up_p);
      const int i_y = indexY(P, capturePtypeOSafe(moved),
			     up_up_p, Y(from(moved)));
      if (P == BLACK)
	result -= table[i]+y_table[i_y];
      else
	result += table[i]+y_table[i_y];
    }
      if(state.isPawnMaskSet(BLACK, X(from(moved))))
    {
	if(nextSquare(BLACK, state.pawnSquare(BLACK,X(from(moved))), U) ==
	   from(moved))
      {
	const Square up_up = from(moved) + newOffset(BLACK,U);
	const PtypeO up_up_p =
	  (isOnBoard(up_up) ? (up_up == to(moved) ? capturePtypeOSafe(moved) :
			       osl::ptypeO(state.pieceAt(up_up))) :
	   PtypeO::EDGE);
	const int i = index(BLACK, oldPtypeO(moved), up_up_p);
	const int i_y = indexY(BLACK, oldPtypeO(moved), up_up_p,
			       Y(from(moved)) + 1);
	result -= table[i]+y_table[i_y];
	if (up_up != to(moved))
	{
	  const int new_i = index(BLACK, PtypeO::EMPTY, up_up_p);
	  const int new_i_y = indexY(BLACK, PtypeO::EMPTY, up_up_p,
				     Y(from(moved)) + 1);
	  result += table[new_i]+y_table[new_i_y];
	}
      }
	if(nextSquare(BLACK, nextSquare(BLACK, state.pawnSquare(BLACK,X(from(moved))), U), U) ==
	   from(moved))
      {
	const Square up = from(moved) + newOffset(BLACK,D);
	const PtypeO up_p =
	  (isOnBoard(up) ? (up == to(moved) ? capturePtypeOSafe(moved) :
			    osl::ptypeO(state.pieceAt(up))) : PtypeO::EDGE);
	const int i = index(BLACK, up_p, oldPtypeO(moved));
	const int i_y = indexY(BLACK, up_p, oldPtypeO(moved),
			       Y(from(moved)) + 2);
	result -= table[i]+y_table[i_y];
	if (to(moved) != up)
	{
	  const int new_i = index(BLACK, up_p, PtypeO::EMPTY);
	  const int new_i_y = indexY(BLACK, up_p, PtypeO::EMPTY,
				     Y(from(moved)) + 2);
	  result += table[new_i]+y_table[new_i_y];
	}
      }
    }
      if(state.isPawnMaskSet(WHITE, X(from(moved))))
    {
	if(nextSquare(WHITE, state.pawnSquare(WHITE, X(from(moved))), U) ==
	   from(moved))
      {
	const Square up_up = from(moved) + newOffset(WHITE,U); 	
	const PtypeO up_up_p =
	  (isOnBoard(up_up) ? (up_up == to(moved) ? capturePtypeOSafe(moved) :
			       osl::ptypeO(state.pieceAt(up_up))) :
	   PtypeO::EDGE);
	const int i = index(WHITE, oldPtypeO(moved), up_up_p);
	const int i_y = indexY(WHITE, oldPtypeO(moved), up_up_p,
			       Y(from(moved)) - 1);
	result += table[i]+y_table[i_y];
	if (to(moved) != up_up)
	{
	  const int new_i = index(WHITE, PtypeO::EMPTY, up_up_p);
	  const int new_i_y = indexY(WHITE, PtypeO::EMPTY, up_up_p,
				     Y(from(moved)) - 1);
	  result -= table[new_i]+y_table[new_i_y];
	}
      }
	if(nextSquare(WHITE, nextSquare(WHITE, state.pawnSquare(WHITE, X(from(moved))), U), U) ==
	   from(moved))
      {
	const Square up = from(moved) + newOffset(WHITE,D); 	
	const PtypeO up_p =
	  (isOnBoard(up) ? (up == to(moved) ? capturePtypeOSafe(moved) :
			    osl::ptypeO(state.pieceAt(up))) : PtypeO::EDGE);
	const int i = index(WHITE, up_p, oldPtypeO(moved));
	const int i_y = indexY(WHITE, up_p, oldPtypeO(moved),
			       Y(from(moved)) - 2);
	result += table[i]+y_table[i_y];
	if (to(moved) != up)
	{
	  const int new_i = index(WHITE, up_p, PtypeO::EMPTY);
	  const int new_i_y = indexY(WHITE, up_p, PtypeO::EMPTY,
				     Y(from(moved)) - 2);
	  result -= table[new_i]+y_table[new_i_y];
	}
      }
    }
  }
  Ptype captured = capturePtype(moved);
  if (captured == PAWN)
  {
    const Square up = to(moved) + newOffset(P,D); 	
    const Square up_up = up + newOffset(P,D); 	
    const PtypeO up_p =
      (isOnBoard(up) ? (up == from(moved) ? oldPtypeO(moved) :
			osl::ptypeO(state.pieceAt(up))) : PtypeO::EDGE);
    const PtypeO up_up_p =
      (isOnBoard(up_up) ? (up_up == from(moved) ? oldPtypeO(moved) :
			   osl::ptypeO(state.pieceAt(up_up))) : PtypeO::EDGE);
    const int i = index(alt(P), up_p, up_up_p);
    const int i_y = indexY(alt(P), up_p, up_up_p,
			   Y(to(moved)));
    if (P == BLACK)
    {
      result += table[i]+y_table[i_y];
    }
    else
    {
      result -= table[i]+y_table[i_y];
    }
  }
  if (osl::ptype(moved) == PAWN)
  {
    const Square up = to(moved) + newOffset(P,U);
    const Square up_up = up + newOffset(P,U);
    const PtypeO up_p =
      (isOnBoard(up) ? (up == from(moved) ? oldPtypeO(moved) :
			osl::ptypeO(state.pieceAt(up))) : PtypeO::EDGE);
    const PtypeO up_up_p =
      (isOnBoard(up_up) ? (up_up == from(moved) ? oldPtypeO(moved) :
			   osl::ptypeO(state.pieceAt(up_up))) : PtypeO::EDGE);
    const int i = index(P, up_p, up_up_p);
    const int i_y = indexY(P, up_p, up_up_p, Y(to(moved)));
    if (P == BLACK)
    {
      result += table[i]+y_table[i_y];
    }
    else
    {
      result -= table[i]+y_table[i_y];
    }
  }
    if(state.isPawnMaskSet(BLACK, X(to(moved))))
  {
	if(nextSquare(BLACK, state.pawnSquare(BLACK,X(to(moved))), U) ==
	   to(moved))
    {
      const Square up_up = to(moved) + newOffset(BLACK,U);
      const PtypeO up_up_p =
	(isOnBoard(up_up) ? osl::ptypeO(state.pieceAt(up_up)) :
	 PtypeO::EDGE);
      const int i = index(BLACK, osl::ptypeO(moved), up_up_p);
      const int i_y = indexY(BLACK, osl::ptypeO(moved), up_up_p,
			     Y(to(moved)) + 1);
      result += table[i]+y_table[i_y];
      if (isDrop(moved) || from(moved) != up_up)
      {
	const int old_i = index(BLACK, capturePtypeOSafe(moved), up_up_p);
	const int old_i_y = indexY(BLACK, capturePtypeOSafe(moved),
				   up_up_p, Y(to(moved)) + 1);
	result -= table[old_i]+y_table[old_i_y];
      }
    }
	if(nextSquare(BLACK, nextSquare(BLACK, state.pawnSquare(BLACK,X(to(moved))), U), U) ==
	   to(moved))
    {
      const Square up = to(moved) + newOffset(BLACK,D);
      const PtypeO up_p =
	(isOnBoard(up) ? osl::ptypeO(state.pieceAt(up)) : PtypeO::EDGE);
      const int i = index(BLACK, up_p, osl::ptypeO(moved));
      const int i_y = indexY(BLACK, up_p, osl::ptypeO(moved), Y(to(moved)) + 2);
      result += table[i]+y_table[i_y];
      if (isDrop(moved) || up != from(moved))
      {
	const int old_i = index(BLACK, up_p, capturePtypeOSafe(moved));
	const int old_i_y = indexY(BLACK, up_p, capturePtypeOSafe(moved),
				   Y(to(moved)) + 2);
	result -= table[old_i]+y_table[old_i_y];
      }
    }
  }
    if(state.isPawnMaskSet(WHITE, X(to(moved))))
  {
      if(nextSquare(WHITE, state.pawnSquare(WHITE, X(to(moved))), U) ==
	   to(moved))
    {
      const Square up_up = to(moved) + newOffset(WHITE,U);
      const PtypeO up_up_p =
	(isOnBoard(up_up) ? osl::ptypeO(state.pieceAt(up_up)) :
	 PtypeO::EDGE);
      const int i = index(WHITE, osl::ptypeO(moved), up_up_p);
      const int i_y = indexY(WHITE, osl::ptypeO(moved), up_up_p,
			     Y(to(moved)) - 1);
      result -= table[i]+y_table[i_y];
      if (up_up != from(moved))
      {
	const int old_i = index(WHITE, capturePtypeOSafe(moved), up_up_p);
	const int old_i_y = indexY(WHITE, capturePtypeOSafe(moved), up_up_p,
				   Y(to(moved)) - 1);
	result += table[old_i]+y_table[old_i_y];
      }
    }
      if(nextSquare(WHITE, nextSquare(WHITE, state.pawnSquare(WHITE, X(to(moved))), U), U) ==
	   to(moved))
    {
      const Square up = to(moved) + newOffset(WHITE,D);
      const PtypeO up_p =
	(isOnBoard(up) ? osl::ptypeO(state.pieceAt(up)) : PtypeO::EDGE);
      const int i = index(WHITE, up_p, osl::ptypeO(moved));
      const int i_y = indexY(WHITE, up_p, osl::ptypeO(moved), Y(to(moved)) - 2);
      result -= table[i]+y_table[i_y];
      if (isDrop(moved) || up != from(moved))
      {
	const int old_i = index(WHITE, up_p, capturePtypeOSafe(moved));
	const int old_i_y = indexY(WHITE, up_p, capturePtypeOSafe(moved),
				   Y(to(moved)) - 2);
	result += table[old_i]+y_table[old_i_y];
      }
    }
  }
  return result;
}



std::array<MultiInt, 9> osl::PromotedMinorPieces::table;
std::array<MultiInt, 162> osl::PromotedMinorPieces::y_table;

void osl::
PromotedMinorPieces::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
PromotedMinorPiecesY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      PromotedMinorPieces::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template <int Sign>
inline void osl::
PromotedMinorPieces::adjust(int index, int index_attack, int index_defense,
			    MultiInt &result)
{
  if(Sign>0)
    result+= table[index] + y_table[index_attack] + y_table[index_defense];
  else
    result-= table[index] + y_table[index_attack] + y_table[index_defense];
}
template <osl::Player P>
void osl::
PromotedMinorPieces::evalOne(const NumEffectState &state,
			     const PieceMask promoted,
			     MultiInt &result)
{
  PieceMask attack = promoted & state.piecesOnBoard(P);
  const Square king = state.kingSquare(alt(P));
  const Square self_king = state.kingSquare(P);
  int min_left = -10;
  int min_right = 10;
  while (any(attack))
  {
    const Piece p = state.pieceOf(takeOneBit(attack));
    const int x_diff = (P == BLACK ? X(square(p)) - X(king) :
			X(king) - X(square(p)));
    if (x_diff <= 0)
    {
      if (x_diff > min_left)
      {
	if (min_left != -10)
	{
	  if (P == BLACK)
	    adjust<1>(-min_left, indexY<true, P>(king, -min_left),
		      indexY<false, P>(self_king, -min_left), result);
	  else
	    adjust<-1>(-min_left, indexY<true, P>(king, -min_left),
		       indexY<false, P>(self_king, -min_left), result);
	}
	min_left = x_diff;
      }
      else
      {
	if (P == BLACK)
	  adjust<1>(-x_diff, indexY<true, P>(king, -x_diff),
		    indexY<false, P>(self_king, -x_diff),
		    result);
	else
	  adjust<-1>(-x_diff, indexY<true, P>(king, -x_diff),
		     indexY<false, P>(self_king, -x_diff),
		     result);
      }
    }
    if (x_diff >= 0)
    {
      if (x_diff < min_right)
      {
	if (min_right != 10)
	{
	  if (P == BLACK)
	    adjust<1>(min_right, indexY<true, P>(king, min_right),
		      indexY<false, P>(self_king, min_right),
		      result);
	  else
	    adjust<-1>(min_right, indexY<true, P>(king, min_right),
		       indexY<false, P>(self_king, min_right),
		       result);
	}
	min_right = x_diff;
      }
      else if (x_diff != 0)
      {
	if (P == BLACK)
	  adjust<1>(x_diff, indexY<true, P>(king, x_diff),
		    indexY<false, P>(self_king, x_diff),
		    result);
	else
	  adjust<-1>(x_diff, indexY<true, P>(king, x_diff),
		     indexY<false, P>(self_king, x_diff),
		     result);
      }
    }
  }
}

MultiInt osl::
PromotedMinorPieces::eval(const NumEffectState &state)
{
  MultiInt result;
  PieceMask promoted_pieces = state.promotedPieces();
  promoted_pieces &= ~(pieceMask(ROOK) | pieceMask(BISHOP));
  if (!any(promoted_pieces))
    return result;

  evalOne<BLACK>(state, promoted_pieces, result);
  evalOne<WHITE>(state, promoted_pieces, result);
  return result;
}

MultiInt osl::
PromotedMinorPieces::evalWithUpdate(const NumEffectState &state,
				    Move moved,
				    const MultiInt &last_values)
{
  Ptype captured = capturePtype(moved);
  if (osl::ptype(moved) == KING ||
      (isPromoted(osl::ptype(moved)) && !isMajor(osl::ptype(moved))) ||
      (captured != Ptype::EMPTY && isPromoted(captured) &&
       !isMajor(captured)))
    return eval(state);

  return last_values;
}




std::array<MultiInt, 9> osl::KnightHead::table;
std::array<MultiInt, 144> osl::KnightHead::opp_table;

void osl::
KnightHead::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
KnightHeadOppPiecePawnOnStand::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      KnightHead::opp_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
KnightHead::eval(const NumEffectState &state)
{
  MultiInt result;
  for(Player P : COLORS){
    for(Square knight : state.allSquareStrict(P, KNIGHT)){
      const Square up = nextSquare(P, knight, U);
      const Piece up_piece = state[up];
      if ((isEmpty(up_piece) && state.hasPieceOnStand(alt(P), PAWN) &&
	   !state.isPawnMaskSet(alt(P), X(knight)) &&
	   state.countEffect(P, up) <= state.countEffect(alt(P), up)) ||
	  (state.hasEffectStrict(alt(P), up, PAWN) &&
	   canMoveOn(alt(P), up_piece) &&
	   state.countEffect(P, up) < state.countEffect(alt(P), up))){
	result += (P == BLACK ? table[Y(knight) - 1] : -table[9 - Y(knight)]);
      }
      else if (isOnBoardByOwner(alt(P), up_piece) &&
	       state.hasPieceOnStand(alt(P), PAWN)){
	const int y = (P == BLACK ? Y(knight) :
		       10 - Y(knight));
	const int index = I(ptype(up_piece)) * 9 + y - 1;
	result += (P == BLACK ? opp_table[index] : -opp_table[index]);
      }
    }
  }
  return result;
}



std::array<osl::MultiInt, 512*512>
osl::NonPawnAttackedPtypePair::table;
void osl::NonPawnAttackedPtypePair::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for (int i=0; i<PTYPE_SIZE*2*PTYPE_SIZE; ++i)
    for (int j=i+1; j<PTYPE_SIZE*2*PTYPE_SIZE; ++j) {
      table[index2(j,i)] = table[index2(i,j)];
    }
}

std::array<MultiInt, 160>
osl::PtypeCountAll::table;
std::array<MultiInt, 2240>
osl::PtypeCountAll::xy_table;
std::array<MultiInt, 2240>
osl::PtypeCountAll::xy_attack_table;
std::array<MultiInt, 2240>
osl::PtypeCountAll::xy_table_diff;
std::array<MultiInt, 2240> osl::PtypeCountAll::xy_attack_table_diff;
std::array<MultiInt, 81> osl::PtypeCountAll::king_yy;
MultiInt osl::PtypeCountAll::all_gold;
MultiInt osl::PtypeCountAll::all_major;
void osl::PtypeCountAll::setUp()
{
  table = PtypeCount::table;
  xy_table = PtypeCountXY::table;
  xy_attack_table = PtypeCountXYAttack::table;
  for(Ptype i=Ptype::BASIC_MIN;i<=Ptype::MAX;++i){
    Ptype ptype=static_cast<Ptype>(i);
    int index_min=indexMin(ptype);
    int size=indexLimit(ptype)-index_min;
    for(int x=0;x<5;x++){
      for(int j=0;j<size;j++){
	for(int k=0;k<160;k+=40){
	  xy_table[(index_min+j+k)*5+x] += table[index_min+j+k];
	}
      }
    }
  }
  for(Ptype i=Ptype::BASIC_MIN;i<=Ptype::MAX;++i){
    Ptype ptype=static_cast<Ptype>(i);
    int index_min=indexMin(ptype);
    int size=indexLimit(ptype)-index_min;
    for(int x=0;x<5;x++){
      for(int k=0;k<160;k+=40)
	xy_table_diff[(index_min+k)*5+x]=xy_table[(index_min+k)*5+x];
      for(int j=1;j<size;j++){
	for(int k=0;k<160;k+=40)
	  xy_table_diff[(index_min+k+j)*5+x]=xy_table[(index_min+k+j)*5+x]-xy_table[(index_min+k+j-1)*5+x];
      }
    }
    for(int y=0;y<9;y++){
      for(int k=0;k<160;k+=40)
	xy_table_diff[800+(index_min+k)*9+y]=xy_table[800+(index_min+k)*9+y];
      for(int j=1;j<size;j++){
	for(int k=0;k<160;k+=40)
	  xy_table_diff[800+(index_min+k+j)*9+y]=xy_table[800+(index_min+k+j)*9+y]-xy_table[800+(index_min+k+j-1)*9+y];
      }
    }
  }
  for(Ptype i=Ptype::BASIC_MIN;i<=Ptype::MAX;++i){
    Ptype ptype=static_cast<Ptype>(i);
    int index_min=indexMin(ptype);
    int size=indexLimit(ptype)-index_min;
    for(int x=0;x<5;x++){
      for(int k=0;k<160;k+=40)
	xy_attack_table_diff[(index_min+k)*5+x]=xy_attack_table[(index_min+k)*5+x];
      for(int j=1;j<size;j++){
	for(int k=0;k<160;k+=40)
	  xy_attack_table_diff[(index_min+k+j)*5+x]=xy_attack_table[(index_min+k+j)*5+x]-xy_attack_table[(index_min+k+j-1)*5+x];
      }
    }
    for(int y=0;y<9;y++){
      for(int k=0;k<160;k+=40)
	xy_attack_table_diff[800+(index_min+k)*9+y]=xy_attack_table[800+(index_min+k)*9+y];
      for(int j=1;j<size;j++){
	for(int k=0;k<160;k+=40)
	  xy_attack_table_diff[800+(index_min+k+j)*9+y]=xy_attack_table[800+(index_min+k+j)*9+y]-xy_attack_table[800+(index_min+k+j-1)*9+y];
      }
    }
  }
  all_major = AllMajor::table[0];
  all_gold = AllGold::table[0];
  for(int wy = 1; wy <= 9; wy++) 
    for(int by = 1; by <= 9; by++) {
      king_yy[(wy - 1) * 9 + (by - 1)] = 
	PtypeYY::table[(by - 1) * 9 * 32 + (wy - 1) * 32 + 8] 
	+ PtypeYY::table[(by - 1) * 9 * 32 + (by - 1) * 32 + 24] 
	- PtypeYY::table[(9 - wy) * 9 * 32 + (9 - by) * 32 + 8] 
	- PtypeYY::table[(9 - wy) * 9 * 32 + (9 - wy) * 32 + 24];
    }
 }

void
osl::PtypeCountAll::evalOne(Player King,
  const NumEffectState &state,
  const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
  const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
  MultiInt &out)
{
  out.clear();
  int kings_x = X(state.kingSquare(King));
  int kings_y = Y(state.kingSquare(King));
  if (kings_x > 5) kings_x = 10 - kings_x;
  if (King == WHITE) kings_y = 10 - kings_y;
  for(Ptype T = Ptype::PIECE_MIN; T <= Ptype::MAX; T++){
    int count = ptype_count[I(King)][I(T)];
    int bcount = ptype_board_count[I(King)][I(T)];
    if (count != 0){
      int index_x = indexCountX(T, count, kings_x);
      int index_y = indexCountY(T, count, kings_y);
      out.addFor(King, xy_table[index_x] + xy_table[index_y]);
      if (bcount != 0){
	index_x = indexBoardCountX(T, bcount, kings_x);
	index_y = indexBoardCountY(T, bcount, kings_y);
	out.addFor(King, xy_table[index_x] + xy_table[index_y]);
      }
    }
  }
  for(Ptype T = Ptype::PIECE_MIN; T <= Ptype::MAX; T++){
    int count = ptype_count[I(alt(King))][I(T)];
    int bcount = ptype_board_count[I(alt(King))][I(T)];
    if (count != 0){
      int index_x_attack = indexCountX(T, count, kings_x);
      int index_y_attack = indexCountY(T, count, kings_y);
      out.subFor(King, xy_attack_table[index_x_attack] + 
		 xy_attack_table[index_y_attack]);
      if (bcount != 0){
	index_x_attack = indexBoardCountX(T, bcount, kings_x);
	index_y_attack = indexBoardCountY(T, bcount, kings_y);
	out.subFor(King, xy_attack_table[index_x_attack] + 
		   xy_attack_table[index_y_attack]);
      }
    }
  }
}

void
osl::PtypeCountAll::eval(
  const NumEffectState &state,
  const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
  const std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
  MultiIntPair &out, MultiInt& out1)
{
  for(Player P : COLORS) evalOne(P, state, ptype_count, ptype_board_count, out[I(P)]);
  int black_gold_count = ptype_count[I(BLACK)][I(GOLD)];
  if(black_gold_count == 0) out1 -= all_gold;
  else if(black_gold_count == 4) out1 += all_gold;
  int black_major_count = 
    ptype_count[I(BLACK)][I(BISHOP)] + ptype_count[I(BLACK)][I(PBISHOP)] +
    ptype_count[I(BLACK)][I(ROOK)] + ptype_count[I(BLACK)][I(PROOK)];
  if(black_major_count == 0) out1 -= all_major;
  else if(black_major_count == 4) out1 += all_major;
  out1 += king_yy[(Y(state.kingSquare(WHITE)) - 1) * 9 + Y(state.kingSquare(BLACK)) - 1];
}

template<osl::Player P>
void osl::PtypeCountAll::evalWithUpdateBang(
  const NumEffectState &state,
  Move last_move,
  std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
  std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
  MultiIntPair &out, MultiInt& out1,
  unsigned int &ptypeo_mask)
{
  assert(player(last_move)==P);
  Square my_king = forBlack(P, state.kingSquare(P));
  Square op_king = forBlack(alt(P), state.kingSquare(alt(P)));
  int my_king_x = X(my_king), my_king_y = Y(my_king);
  int op_king_x = X(op_king), op_king_y = Y(op_king);
  my_king_x = (my_king_x > 5 ? 10 - my_king_x : my_king_x);
  op_king_x = (op_king_x > 5 ? 10 - op_king_x : op_king_x);

  if (osl::ptype(last_move) == KING)
  {
    const Ptype capturedPtype = capturePtype(last_move);
    if (capturedPtype != Ptype::EMPTY)
    {
      PtypeO capturedPtypeO = capturePtypeO(last_move);
//      Ptype capturedPtype = getPtype(capturedPtypeO);
      int count = --ptype_count[I(alt(P))][I(capturedPtype)];
      if(count==0)
	ptypeo_mask &= ~(1<<I(capturePtypeO(last_move)));
      int board_count = --ptype_board_count[I(alt(P))][I(capturedPtype)];
      Ptype base_captured = unpromote(capturedPtype);
      int c_count = ++ptype_count[I(P)][I(base_captured)];
      ptypeo_mask |= (1<<I(captured(capturedPtypeO)));
      out[I(alt(P))].addFor(
      P, 	
      xy_table_diff[indexCountX(capturedPtype, count + 1, op_king_x)]+
      xy_table_diff[indexCountY(capturedPtype, count + 1, op_king_y)]+
      xy_table_diff[indexBoardCountX(capturedPtype, board_count + 1, op_king_x)]+
      xy_table_diff[indexBoardCountY(capturedPtype, board_count + 1, op_king_y)]+
      xy_attack_table_diff[indexCountX(base_captured, c_count, op_king_x)]+
      xy_attack_table_diff[indexCountY(base_captured, c_count, op_king_y)]);
      if(isMajor(capturedPtype)){
	int major_count = ptype_count[I(P)][I(BISHOP)] + ptype_count[I(P)][I(PBISHOP)] +
	  ptype_count[I(P)][I(ROOK)] + ptype_count[I(P)][I(PROOK)];
      // major_count == 1 || major_count == 4
	if(((1 << major_count) & 0x12) != 0) out1.addFor(P, all_major);
      }
      else if(capturedPtype == GOLD){
	int gold_count = ptype_count[I(P)][I(GOLD)];
	// gold_count == 1 || gold_count == 4
	if(((1 << gold_count) & 0x12) != 0) out1.addFor(P, all_gold);
      }
    }
    evalOne(P, state, ptype_count, ptype_board_count, out[I(P)]);
    out1 += king_yy[(Y(state.kingSquare(WHITE)) - 1) * 9 + Y(state.kingSquare(BLACK)) - 1] -
      king_yy[(Y(state.previous->kingSquare(WHITE)) - 1) * 9 + Y(state.previous->kingSquare(BLACK)) - 1];
    return;
  }
  
  if (isDrop(last_move))
  {
    const int count = ++ptype_board_count[I(P)][I(osl::ptype(last_move))];
    out[I(P)].addFor(P, xy_table_diff[indexBoardCountX(ptype(last_move), count, my_king_x)]+ 
			      xy_table_diff[indexBoardCountY(ptype(last_move), count, my_king_y)]);
    out[I(alt(P))].addFor(P, xy_attack_table_diff[indexBoardCountX(ptype(last_move), count, op_king_x)] + 
			      xy_attack_table_diff[indexBoardCountY(ptype(last_move), count, op_king_y)]);
  }
  else{
    Ptype capturedPtype = capturePtype(last_move);
    if (capturedPtype != Ptype::EMPTY)
    {
      const int count = --ptype_count[I(alt(P))][I(capturedPtype)];
      if(count==0)
	ptypeo_mask &= ~(1<<I(capturePtypeO(last_move)));
      const int board_count = --ptype_board_count[I(alt(P))][I(capturedPtype)];
      const Ptype base_captured = unpromote(capturedPtype);
      const int c_count = ++ptype_count[I(P)][I(base_captured)];
      ptypeo_mask |= 1<<I(captured(capturePtypeO(last_move)));
      out[I(P)].addFor(
      P, 	
      xy_attack_table_diff[indexCountX(capturedPtype, count + 1, my_king_x)]+
      xy_attack_table_diff[indexCountY(capturedPtype, count + 1, my_king_y)]+
      xy_attack_table_diff[indexBoardCountX(capturedPtype, board_count + 1, my_king_x)]+
      xy_attack_table_diff[indexBoardCountY(capturedPtype, board_count + 1, my_king_y)]+
      xy_table_diff[indexCountX(base_captured, c_count, my_king_x)]+
      xy_table_diff[indexCountY(base_captured, c_count, my_king_y)]);
      out[I(alt(P))].addFor(
      P, 	
      xy_table_diff[indexCountX(capturedPtype, count + 1, op_king_x)]+
      xy_table_diff[indexCountY(capturedPtype, count + 1, op_king_y)]+
      xy_table_diff[indexBoardCountX(capturedPtype, board_count + 1, op_king_x)]+
      xy_table_diff[indexBoardCountY(capturedPtype, board_count + 1, op_king_y)]+
      xy_attack_table_diff[indexCountX(base_captured, c_count, op_king_x)]+
      xy_attack_table_diff[indexCountY(base_captured, c_count, op_king_y)]);
      if(isMajor(capturedPtype)){
	int major_count = ptype_count[I(P)][I(BISHOP)] + ptype_count[I(P)][I(PBISHOP)] +
	  ptype_count[I(P)][I(ROOK)] + ptype_count[I(P)][I(PROOK)];
	// major_count == 1 || major_count == 4
	if(((1 << major_count) & 0x12) != 0) out1.addFor(P, all_major);
      }
      else if(capturedPtype == GOLD){
	int gold_count = ptype_count[I(P)][I(GOLD)];
	// gold_count == 1 || gold_count == 4
	if(((1 << gold_count) & 0x12) != 0) out1.addFor(P, all_gold);
      }
    }
    if (isPromotion(last_move))
    {
      const Ptype old_ptype = oldPtype(last_move);
      const Ptype new_ptype = osl::ptype(last_move);
      const int base_count = --ptype_count[I(P)][I(old_ptype)];
      const int base_board_count = --ptype_board_count[I(P)][I(old_ptype)];
      const int count = ++ptype_count[I(P)][I(new_ptype)];
      const int board_count = ++ptype_board_count[I(P)][I(new_ptype)];
      if(base_count==0)
	ptypeo_mask &= ~(1<<I(oldPtypeO(last_move)));
      ptypeo_mask |= (1<<I(osl::ptypeO(last_move)));
      out[I(P)].addFor(
      P, 	
      xy_table_diff[indexCountX(new_ptype, count, my_king_x)] +
      xy_table_diff[indexCountY(new_ptype, count, my_king_y)] +
      xy_table_diff[indexBoardCountX(new_ptype, board_count, my_king_x)] +
      xy_table_diff[indexBoardCountY(new_ptype, board_count, my_king_y)] -
      xy_table_diff[indexCountX(old_ptype, base_count + 1, my_king_x)] -
      xy_table_diff[indexCountY(old_ptype, base_count + 1, my_king_y)] -
      xy_table_diff[indexBoardCountX(old_ptype, base_board_count + 1, my_king_x)] -
      xy_table_diff[indexBoardCountY(old_ptype, base_board_count + 1, my_king_y)]);
      out[I(alt(P))].addFor(
      P, 	
      xy_attack_table_diff[indexCountX(new_ptype, count, op_king_x)] +
      xy_attack_table_diff[indexCountY(new_ptype, count, op_king_y)] +
      xy_attack_table_diff[indexBoardCountX(new_ptype, board_count, op_king_x)] +
      xy_attack_table_diff[indexBoardCountY(new_ptype, board_count, op_king_y)] -
      xy_attack_table_diff[indexCountX(old_ptype, base_count + 1, op_king_x)] -
      xy_attack_table_diff[indexCountY(old_ptype, base_count + 1, op_king_y)] -
      xy_attack_table_diff[indexBoardCountX(old_ptype, base_board_count + 1, op_king_x)] -
      xy_attack_table_diff[indexBoardCountY(old_ptype, base_board_count + 1, op_king_y)]);
    }
  }
}


std::array<MultiInt, 1440>
osl::PtypeYPawnY::table;

void osl::PtypeYPawnY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s = 0; s < NStages; ++s)
    {
      table[i][s] = weights.value(i + ONE_DIM*s);
    }
  }
}

osl::MultiInt osl::
PtypeYPawnY::eval(const NumEffectState &state
)
{
  MultiInt result;
  for (int i = 0; i < Piece_SIZE; ++i)
  {
    const Piece piece = state.pieceOf(i);
    // only skip pawn, not ppawns
    if (osl::ptype(piece) == PAWN)
      continue;
    if (!isOnBoard(piece))
      continue;

    int idx = index(owner(piece), osl::ptype(piece), Y(square(piece)),
		    state.pawnY(owner(piece), square(piece)));
    if (owner(piece) == BLACK)
    {
      result += table[idx];
    }
    else
    {
      result -= table[idx];
    }
  }

  return result;
}

template<osl::Player P>
void osl::
PtypeYPawnY::evalWithUpdateBang(const NumEffectState &state,
				Move moved,
				MultiInt& last_value)
{
  Ptype captured = capturePtype(moved);
  assert(P==player(moved));

  if (oldPtype(moved) == PAWN)
  {
    const int x = X(to(moved));
    const int old_pawn_y = (isDrop(moved) ? 0 : Y(from(moved)));
    int new_pawn_y = state.pawnY(P, to(moved));
    for (int y = 1; y <= 9; ++y)
    {
      const Piece p = state.pieceAt(newSquare(x, y));
      if (y == Y(to(moved)))
      {
	if (osl::ptype(p) == PPAWN)
	{
	  const int idx_new = index(P, osl::ptype(p), y, new_pawn_y);
	  if (P == BLACK)
	  {
	    last_value += table[idx_new];
	  }
	  else
	  {
	    last_value -= table[idx_new];
	  }   
	}
      }
      else if (!isEmpty(p) && owner(p) == P)
      {
	const int idx_old = index(P, osl::ptype(p), y, old_pawn_y);
	const int idx_new = index(P, osl::ptype(p), y, new_pawn_y);
	if (P == BLACK)
	{
	  last_value -= table[idx_old];
	  last_value += table[idx_new];
	}
	else
	{
	  last_value += table[idx_old];
	  last_value -= table[idx_new];
	}   
      }
    }
  }
  else
  {
    if (!isDrop(moved))
    {
      int pawn_y = state.pawnY(P, from(moved));
      const int idx = index(P, oldPtype(moved), Y(from(moved)),
			    pawn_y);
      if (P == BLACK)
      {
	last_value -= table[idx];
      }
      else
      {
	last_value += table[idx];
      }
    }
    {
      int pawn_y = state.pawnY(P, to(moved));
      const int idx = index(P, osl::ptype(moved), Y(to(moved)),
			    pawn_y);
      if (P == BLACK)
      {
	last_value += table[idx];
      }
      else
      {
	last_value -= table[idx];
      }
    }
  }

  if (captured != Ptype::EMPTY)
  {
    if (captured == PAWN)
    {
      const int old_pawn_y = Y(to(moved));
      const int new_pawn_y = 0;
      const int x = X(to(moved));
      for (int y = 1; y <= 9; ++y)
      {
	const Piece p = state.pieceAt(newSquare(x, y));
	if (!isEmpty(p) && owner(p) == alt(P))
	{
	  const int idx_old = index(alt(P), osl::ptype(p), y,
				    old_pawn_y);
	  const int idx_new = index(alt(P), osl::ptype(p), y,
				    new_pawn_y);
	  if (P == BLACK)
	  {
	    last_value += table[idx_old];
	    last_value -= table[idx_new];
	  }
	  else
	  {
	    last_value -= table[idx_old];
	    last_value += table[idx_new];
	  }   
	}
      }
    }
    else
    {
      int pawn_y = state.pawnY(alt(P), to(moved));
      const int idx = index(alt(P), captured, Y(to(moved)),
			    pawn_y);
      if (P == BLACK)
      {
	last_value += table[idx];
      }
      else
      {
	last_value -= table[idx];
      }
    }
  }
}

std::array<osl::MultiInt, 1215>
osl::GoldAndSilverNearKing::table;
std::array<osl::MultiInt, 9720>
osl::GoldAndSilverNearKing::combination_table;

void osl::
GoldAndSilverNearKing::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
GoldAndSilverNearKingCombination::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      GoldAndSilverNearKing::combination_table[i][s] =
	weights.value(i + ONE_DIM*s);
  }
}

template <osl::Player P>
osl::MultiInt osl::
GoldAndSilverNearKing::evalOne(const NumEffectState &state,
			       const std::array<std::array<int, 3>, 2> &gs_count)
{
  MultiInt result;
  int total = 0;
  const Square king = state.kingSquare(P);
  for (size_t i = 0; i < gs_count[0].size(); ++i)
  {
    total += gs_count[I(P)][i];
    if (total != 0)
    {
      result += table[index<P>(king, i, total)];
    }
  }
  result += combination_table[
    indexCombination<P>(king, gs_count[I(P)][0],
			gs_count[I(P)][1], gs_count[I(P)][2])];
  return P == BLACK ? result : -result;
}

osl::MultiInt osl::
GoldAndSilverNearKing::eval(const NumEffectState &state,
			    const std::array<std::array<int, 3>, 2> &gs_count)
{
  return evalOne<BLACK>(state, gs_count) + evalOne<WHITE>(state, gs_count);
}


std::array<osl::MultiInt, 8192>
osl::PtypeCombination::table;

void osl::
PtypeCombination::setUp(const Weights &weights)
{
  static std::array<MultiInt, 8192> orig_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s = 0; s < NStages; ++s)
    {
      orig_table[i][s] = weights.value(i + ONE_DIM*s);
    }
  }
  for(int i=0;i<8192;i++){
    int pawn=(i>>12)&1;
    int ppawn=(i>>6)&1;
    int lance=(i>>11)&1;
    int plance=(i>>5)&1;
    int knight=(i>>10)&1;
    int pknight=(i>>4)&1;
    int silver=(i>>9)&1;
    int psilver=(i>>3)&1;
    int bishop=(i>>8)&1;
    int pbishop=(i>>2)&1;
    int rook=(i>>7)&1;
    int prook=(i>>1)&1;
    int gold=(i>>0)&1;
    int newIndex=ppawn|(plance<<1)|(pknight<<2)|(psilver<<3)|(pbishop<<4)|
      (prook<<5)|(gold<<6)|(pawn<<7)|(lance<<8)|(knight<<9)|(silver<<10)|
      (bishop<<11)|(rook<<12);
    table[newIndex]=orig_table[i];
  }
}

osl::MultiInt osl::
PtypeCombination::eval(unsigned int ptypeo_mask)
{
  return evalOne<BLACK>(ptypeo_mask) + evalOne<WHITE>(ptypeo_mask);
}

std::array<osl::MultiInt, 5*2>
osl::SilverFork::table;

osl::MultiIntPair osl::
SilverFork::eval(const NumEffectState& state)
{
  MultiIntPair result;		// by turn
  for(Player P : COLORS){
    if(!state.hasPieceOnStand(P, SILVER)) continue;
    for(Square rook : state.allSquareStrict(alt(P), ROOK)){
      if(!canPromote(P, rook)) continue;
      for(Direction Dir : DIRECTIONS_DL_DR){
	Square next = rook + newOffset(P, Dir);
	if (!isEmpty(state[next]) || state.hasEffect(alt(P), next))
	  continue;
	Square next2 = next + newOffset(P, Dir);
	Piece p = state[next2];
	if(!isOnBoardByOwner(alt(P), p)) continue;
	if(ptype(p) == ROOK){
	  result[I(alt(P))].addFor(alt(P), table[1]);
	  result[I(P)].addFor(alt(P), table[0]);
	  break;
	}
	else if(ptype(p) == GOLD){
	  if(state.hasEffect(alt(P), next2)){
	    result[I(alt(P))].addFor(alt(P), table[3]);
	    result[I(P)].addFor(alt(P), table[2]);
	  }
	  else{
	    result[I(alt(P))].addFor(alt(P), table[5]);
	    result[I(P)].addFor(alt(P), table[4]);
	  }
	  break;
	}
      }
    }
    for(Square gold : state.allSquare(alt(P), GOLD)){
      if(!canPromote(P, gold)) continue;
      for(Direction Dir : DIRECTIONS_L_R){
	Square next_down = gold + newOffset(P, Dir) + newOffset(P, U);
	if(!isEmpty(state[next_down]) || state.hasEffect(alt(P), next_down))
	  continue;
	Square next = gold + newOffset(P, Dir);
	Square next2 = next + newOffset(P, Dir);
	Piece p = state[next2];
	if(!isOnBoardByOwner(alt(P), p)) continue;
	if(ptype(p) == ROOK){
	  if(state.hasEffect(alt(P), gold) ||
	     (canMoveOn(alt(P), state[next]) && !state.hasEffect(P, next))){
	    result[I(alt(P))].addFor(alt(P), table[9]);
	    result[I(P)].addFor(alt(P), table[8]);
	  }
	  else{
	    result[I(alt(P))].addFor(alt(P), table[7]);
	    result[I(P)].addFor(alt(P), table[6]);
	  }
	  break;
	}
	else if(ptype(p) == GOLD){
	  if(state.hasEffect(alt(P), gold) ||
	     state.hasEffect(alt(P), next2) ||
	     (canMoveOn(alt(P), state[next]) && !state.hasEffect(P, next))){
	    result[I(alt(P))].addFor(alt(P), table[9]);
	    result[I(P)].addFor(alt(P), table[8]);
	  }
	  else{
	    result[I(alt(P))].addFor(alt(P), table[7]);
	    result[I(P)].addFor(alt(P), table[6]);
	  }
	  break;
	}
      }
    }
  }
  return result;
}

void osl::SilverFork::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}




std::array<osl::MultiInt, 256*2*2>
osl::KnightFork::table;
void osl::KnightFork::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for (int i=0; i<PTYPE_SIZE; ++i)
    for (int j=i+1; j<PTYPE_SIZE; ++j) {
      table[index((Ptype)j,(Ptype)i)*2] = table[index((Ptype)i,(Ptype)j)*2];
      table[index((Ptype)j,(Ptype)i)*2+1] = table[index((Ptype)i,(Ptype)j)*2+1];
      table[(index((Ptype)j,(Ptype)i)+DROP_DIM)*2] = table[(index((Ptype)i,(Ptype)j)+DROP_DIM)*2];
      table[(index((Ptype)j,(Ptype)i)+DROP_DIM)*2+1] = table[(index((Ptype)i,(Ptype)j)+DROP_DIM)*2+1];
    }
}

template <osl::Player Defense>
osl::MultiIntPair osl::
KnightFork::evalOne(const NumEffectState &state, bool has_knight, 
		    BoardMask& knight_fork_squares)
{
  knight_fork_squares.clear();
  const int z = I(Defense);
  const int y_min = 3-z*2, y_max = 9-z*2;
  std::array<PieceVector,10> pieces;
  {
    PieceMask target = state.piecesOnBoard(Defense);
    target &= ~(pieceMask(PAWN) | pieceMask(LANCE) | pieceMask(KNIGHT));
    while (any(target)) {
      const Piece p = state.pieceOf(takeOneBit(target));
      const int y = Y(square(p));
      pieces[y].push_back(p);
    }
  }
  MultiIntPair result;
  for (int y=y_min; y<=y_max; ++y){
    if (pieces[y].size() < 2)
      continue;
    const int y_drop = y - sign(Defense)*2;
    for (size_t i=0; i<pieces[y].size(); ++i) 
    {
      const Piece pi = pieces[y][i];
      assert(isOnBoardByOwner(Defense,pi));
      assert(Y(square(pi)) == y);
      const int xi = X(square(pi));
      for (size_t j=i+1; j<pieces[y].size(); ++j) 
      {
	const Piece pj = pieces[y][j];
	assert(isOnBoardByOwner(Defense,pj));
	assert(Y(square(pj)) == y);
	const int xj = X(square(pj));
	if (abs(xi -xj) != 2)
	  continue;
	const Square drop = newSquare((xi+xj)/2, y_drop);
	assert(isOnBoard(drop));
	knight_fork_squares.set(drop); 
	if (! isEmpty(state[drop]) || state.hasEffect(Defense, drop))
	  continue;
 	int found = index(osl::ptype(pi), osl::ptype(pj));
	if (! has_knight)
	  found += DROP_DIM;
	found *= 2;
	const MultiInt value_attack = table[found];
	if (Defense == BLACK)
	{
	  result[I(BLACK)] += table[found+1];
	  result[I(WHITE)] += value_attack;
	}
	else
	{
	  result[I(BLACK)] -= value_attack;
	  result[I(WHITE)] -= table[found+1];
	}
      }
    }
  }
  return result;
}

osl::MultiIntPair osl::
KnightFork::eval(const NumEffectState &state,
		 std::array<BoardMask,2>& knight_fork_squares)
{
  MultiIntPair result;
  std::array<bool,2> has_knight;
  has_knight[0]=state.hasPieceOnStand(BLACK, KNIGHT);
  has_knight[1]=state.hasPieceOnStand(WHITE, KNIGHT);
  
  std::array<bool,2> may_have_knight;
  may_have_knight[0]=has_knight[I(BLACK)] 
    || any(pieceMask(KNIGHT) & state.effectedMask(BLACK) 
	   & ~state.effectedMask(WHITE)
	   & state.piecesOnBoard(WHITE));
  may_have_knight[1]=has_knight[I(WHITE)] 
    || any(pieceMask(KNIGHT)  & state.effectedMask(WHITE)
	   & ~state.effectedMask(BLACK)
	   & state.piecesOnBoard(BLACK));
  if (has_knight[I(BLACK)] + has_knight[I(WHITE)]
      + may_have_knight[I(BLACK)] + may_have_knight[I(WHITE)] == 0) {
    knight_fork_squares[I(BLACK)].invalidate();
    knight_fork_squares[I(WHITE)].invalidate();
    return result;
  }
  {
    constexpr Player Defense = BLACK;
    if (has_knight[I(alt(Defense))] + may_have_knight[I(alt(Defense))] > 0)
      result += evalOne<Defense>(state, has_knight[I(alt(Defense))],
				 knight_fork_squares[I(alt(Defense))]);
    else
      knight_fork_squares[I(alt(Defense))].invalidate();
  }
  {
    constexpr Player Defense = WHITE;
    if (has_knight[I(alt(Defense))] + may_have_knight[I(alt(Defense))] > 0)
      result += evalOne<Defense>(state, has_knight[I(alt(Defense))],
				 knight_fork_squares[I(alt(Defense))]);
    else
      knight_fork_squares[I(alt(Defense))].invalidate();
  }
  return result;
}

template <osl::Player P, osl::Player Defense>
void osl::
KnightFork::updateSquares(const NumEffectState& state, Move moved,
			  BoardMask& knight_fork_squares)
{
  assert(! knight_fork_squares.isInvalid());
  const Square to = osl::to(moved);
  if (P != Defense) {
    if (! isCapture(moved))
      return;
    if ((Defense == BLACK && Y(to) >= 3)
	|| (Defense == WHITE && Y(to) <= 7)) {
      if(isOnBoard(nextSquare(Defense, to, UUL)))
	knight_fork_squares.reset(nextSquare(Defense, to, UUL));
      if(isOnBoard(nextSquare(Defense, to, UUR)))
	knight_fork_squares.reset(nextSquare(Defense, to, UUR));
    }
    return;
  }
  if (! isDrop(moved)) {
    if ((P == BLACK && Y(from(moved)) >= 3)
	|| (P == WHITE && Y(from(moved)) <= 7)) {
      if(isOnBoard(nextSquare(P, from(moved), UUL)))
	knight_fork_squares.reset(nextSquare(P, from(moved), UUL));
      if(isOnBoard(nextSquare(P, from(moved), UUR)))
	knight_fork_squares.reset(nextSquare(P, from(moved), UUR));
    }
  }
  if (! isTarget(osl::ptype(moved))
      || (P == BLACK && Y(to) < 3) || (P == WHITE && Y(to) > 7))
    return;
  if (X(to) <= 7)
  {
    const Square l = nextSquare(BLACK, to, L), l2 = nextSquare(BLACK, l, L);
    if (isOnBoardByOwner(P,state[l2])) {
      if(isOnBoard(nextSquare(P, nextSquare(P, l, U), U)))
	knight_fork_squares.set(nextSquare(P, nextSquare(P, l, U), U));
    }
  }
  if (X(to) >= 3)
  {
    const Square r = nextSquare(BLACK, to, R), r2 = nextSquare(BLACK, r, R);
    if (isOnBoardByOwner(P,state[r2])){
      if(isOnBoard(nextSquare(P, nextSquare(P, r, U), U)))
	knight_fork_squares.set(nextSquare(P, nextSquare(P, r, U), U));
    }
  }
}

template <osl::Player Defense>
osl::MultiIntPair osl::
KnightFork::accumulate(const NumEffectState& state,
		       bool has_knight,
		       const BoardMask& knight_fork_squares)
{
  MultiIntPair result;
  BoardMask mask = knight_fork_squares;
  while (any(mask)) {
    Square sq = takeOneBit(mask);
    assert( isOnBoard(sq) );
    if (! isEmpty(state[sq]) || state.hasEffect(Defense, sq))
      continue;
    const Piece pi = state[back(Defense,UUL,sq)];
    const Piece pj = state[back(Defense,UUR,sq)];
    if (! isOnBoardByOwner(Defense,pi) || ! isOnBoardByOwner(Defense,pj))
      std::cerr << state << Defense << ' ' << pi << ' ' << pj << "\n";
    assert(isOnBoardByOwner(Defense,pi));
    assert(isOnBoardByOwner(Defense,pj));
    int found = index(osl::ptype(pi), osl::ptype(pj));
    if (! has_knight)
      found += DROP_DIM;
    found *= 2;
    const MultiInt value_attack = table[found];
    if (Defense == BLACK)
    {
      result[I(BLACK)] += table[found+1];
      result[I(WHITE)] += value_attack;
    }
    else
    {
      result[I(BLACK)] -= value_attack;
      result[I(WHITE)] -= table[found+1];
    }
  }
  return result;
}

template <osl::Player P>
osl::MultiIntPair osl::
KnightFork::evalWithUpdate(const NumEffectState &state, Move moved,
			   std::array<BoardMask,2>& knight_fork_squares)
{
  MultiIntPair result;
  std::array<bool,2> has_knight;
  has_knight[0]=state.hasPieceOnStand(BLACK, KNIGHT);
  has_knight[1]=state.hasPieceOnStand(WHITE, KNIGHT);
  std::array<bool,2> may_have_knight;
  may_have_knight[0]=has_knight[I(BLACK)] 
    || any(pieceMask(KNIGHT) & state.effectedMask(BLACK)
	   & ~state.effectedMask(WHITE)
	   & state.piecesOnBoard(WHITE));
  may_have_knight[1]=has_knight[I(WHITE)]
    || any(pieceMask(KNIGHT) & state.effectedMask(WHITE)
	   & ~state.effectedMask(BLACK)
	   & state.piecesOnBoard(BLACK));
  if (has_knight[I(BLACK)] + has_knight[I(WHITE)]
      + may_have_knight[I(BLACK)] + may_have_knight[I(WHITE)] == 0) {
    knight_fork_squares[I(BLACK)].invalidate();
    knight_fork_squares[I(WHITE)].invalidate();
    return result;
  }
  {
    constexpr Player Defense = BLACK;
    if (has_knight[I(alt(Defense))] + may_have_knight[I(alt(Defense))] > 0) {
      if (knight_fork_squares[I(alt(Defense))].isInvalid())
	result += evalOne<Defense>(state, has_knight[I(alt(Defense))],
				   knight_fork_squares[I(alt(Defense))]);
      else {
	updateSquares<P,Defense>(state, moved, knight_fork_squares[I(alt(Defense))]);
	result += accumulate<Defense>(state, has_knight[I(alt(Defense))],
				      knight_fork_squares[I(alt(Defense))]);
      }
    }
    else
      knight_fork_squares[I(alt(Defense))].invalidate();
  }
  {
    constexpr Player Defense = WHITE;
    if (has_knight[I(alt(Defense))] + may_have_knight[I(alt(Defense))] > 0) {
      if (knight_fork_squares[I(alt(Defense))].isInvalid())
	result += evalOne<Defense>(state, has_knight[I(alt(Defense))],
				   knight_fork_squares[I(alt(Defense))]);
      else {
	updateSquares<P,Defense>(state, moved, knight_fork_squares[I(alt(Defense))]);
	result += accumulate<Defense>(state, has_knight[I(alt(Defense))],
				      knight_fork_squares[I(alt(Defense))]);
      }
    }
    else
      knight_fork_squares[I(alt(Defense))].invalidate();
  }
  return result;
}


std::array<osl::MultiInt, 1>
osl::SilverAdvance26::table;
void osl::SilverAdvance26::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}
osl::MultiInt osl::
SilverAdvance26::eval(const NumEffectState &state) 
{
  std::array<std::pair<Square,Ptype>,5> pattern;
  pattern[0]=std::make_pair( newSquare(2,6), SILVER );
  pattern[1]=std::make_pair( newSquare(1,5), PAWN );
  pattern[2]=std::make_pair( newSquare(3,7), KNIGHT );
  pattern[3]=std::make_pair( newSquare(2,5), PAWN );
  pattern[4]=std::make_pair( newSquare(3,6), PAWN );
  MultiInt sum;
  bool match = X(state.kingSquare(BLACK)) >= 5;
  if (match) {
    for (size_t i=0; i<pattern.size(); ++i) {
      const Piece p = state.pieceAt(pattern[i].first);
      if (osl::ptype(p) != pattern[i].second || owner(p) != BLACK) {
	match = false;
	break;
      }
    }
    if (match)
      sum += table[0];
  }
  match = X(state.kingSquare(WHITE)) <= 5;
  if (match) {
    for (size_t i=0; i<pattern.size(); ++i) {
      const Piece p = state.pieceAt(rotate180(pattern[i].first));
      if (osl::ptype(p) != pattern[i].second || owner(p) != WHITE) {
	match = false;
	break;
      }
    }
    if (match)
      sum += -table[0];
  }
  return sum;
}




namespace osl
{
  template void PawnAdvanceAll::
  evalWithUpdateBang<BLACK>(const NumEffectState &, Move,MultiInt&);
  template void PawnAdvanceAll::
  evalWithUpdateBang<WHITE>(const NumEffectState &, Move,MultiInt&);
  template MultiInt PtypeY::
  evalWithUpdate<BLACK>(const NumEffectState &, Move, MultiInt const&);
  template MultiInt PtypeY::
  evalWithUpdate<WHITE>(const NumEffectState &, Move, MultiInt const&);
  template MultiInt PtypeX::
  evalWithUpdate<BLACK>(const NumEffectState &, Move, MultiInt const&);
  template MultiInt PtypeX::
  evalWithUpdate<WHITE>(const NumEffectState &, Move, MultiInt const&);
  template MultiInt PawnPtypeOPtypeO::
  evalWithUpdate<BLACK>(const NumEffectState &, Move
, MultiInt const&);
  template MultiInt PawnPtypeOPtypeO::
  evalWithUpdate<WHITE>(const NumEffectState &, Move, 
MultiInt const&);

  template void osl::PtypeYPawnY::
  evalWithUpdateBang<BLACK>(const NumEffectState &state,
			    Move moved,
			    MultiInt& last_value);
  template void osl::PtypeYPawnY::
  evalWithUpdateBang<WHITE>(const NumEffectState &state,
			    Move moved,
			    MultiInt& last_value);
  template void PtypeCountAll::
  evalWithUpdateBang<BLACK>(const NumEffectState &state,Move last_move,
			    std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
			    std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
	MultiIntPair &out, MultiInt& out1,
			    unsigned int &ptypeo_mask);
  template void PtypeCountAll::
  evalWithUpdateBang<WHITE>(const NumEffectState &state,Move last_move,
			    std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_count,
			    std::array<std::array<int, PTYPE_SIZE>, 2> &ptype_board_count,
	MultiIntPair &out, MultiInt& out1,
			    unsigned int &ptypeo_mask);

  template MultiIntPair KnightFork::
  evalWithUpdate<BLACK>(const NumEffectState&, Move, std::array<BoardMask,2>&);
  template MultiIntPair KnightFork::
  evalWithUpdate<WHITE>(const NumEffectState&, Move, std::array<BoardMask,2>&);
}



std::array<MultiInt, 180> osl::RookPawnY::table;
std::array<MultiInt, 1620> osl::
RookPawnY::y_attack_table;
std::array<MultiInt, 1620> osl::
RookPawnY::y_defense_table;

void osl::
RookPawnYX::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s) 
    {  
      RookPawnY::y_attack_table[i][s] = weights.value(i + ONE_DIM * 2 * s);
      RookPawnY::y_defense_table[i][s] = weights.value(i + ONE_DIM * 2 * s + ONE_DIM);
    }
  }
}


void osl::
RookPawnY::setUp(const Weights &weights)
{
  for (int i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
RookPawnY::eval(const NumEffectState &state
  )
{
  MultiInt result;
  std::array<Square,2> kings={Square(0),Square(0)};
  kings[I(BLACK)]=state.kingSquare(BLACK);
  kings[I(WHITE)]=state.kingSquare(WHITE);
  for (int i = indexMin(ROOK);
       i < indexLimit(ROOK);
       ++i)
  {
    const Piece piece = state.pieceOf(i);
    if (isOnBoard(piece))
    {
      if (owner(piece) == BLACK)
      {
	int pawn_y = state.pawnY(BLACK, square(piece));
	result +=
	  table[index(piece, pawn_y)] +
	  y_attack_table[indexY(kings[I(WHITE)], piece, pawn_y)] +
	  y_defense_table[indexY(kings[I(BLACK)], piece, pawn_y)];
      }
      else
      {
	int y = state.pawnY(WHITE, square(piece));
	if (y != 0)
	  y = 10 - y;
	result -=
	  table[index(piece, y)] +
	  y_attack_table[indexY(kings[I(BLACK)], piece, y)] +
	  y_defense_table[indexY(kings[I(WHITE)], piece, y)];
      }
    }
  }
  return result;
}





std::array<MultiInt, 612> osl::RookEffectBase::attack_table;
std::array<MultiInt, 612> osl::RookEffectBase::defense_table;
std::array<MultiInt, 32> osl::RookEffectBase::piece_table;

std::array<MultiInt, 23104> osl::RookEffectBase::attack_u;
std::array<MultiInt, 23104> osl::RookEffectBase::attack_d;
std::array<MultiInt, 23104> osl::RookEffectBase::attack_r;
std::array<MultiInt, 23104> osl::RookEffectBase::attack_l;
std::array<MultiInt, 23104> osl::RookEffectBase::defense_u;
std::array<MultiInt, 23104> osl::RookEffectBase::defense_d;
std::array<MultiInt, 23104> osl::RookEffectBase::defense_r;
std::array<MultiInt, 23104> osl::RookEffectBase::defense_l;
std::array<MultiInt, 722> osl::RookEffectBase::attack_nospace;
std::array<MultiInt, 722> osl::RookEffectBase::defense_nospace;


template<osl::Player P>
inline
MultiInt osl::RookEffectBase::evalOne(
  const NumEffectState& state,
  Square rook,
  Square myKing,
  Square opKing,
  Square up,
  Square dp,
  Square rp,
  Square lp,
  bool isP)
{
  MultiInt result;
  PtypeO uPtypeO=osl::ptypeO(state.pieceAt(up));
  PtypeO dPtypeO=osl::ptypeO(state.pieceAt(dp));
  PtypeO rPtypeO=osl::ptypeO(state.pieceAt(rp));
  PtypeO lPtypeO=osl::ptypeO(state.pieceAt(lp));
  if(P==WHITE){
    uPtypeO=(PtypeO)(static_cast<int>(uPtypeO)^(~15));
    dPtypeO=(PtypeO)(static_cast<int>(dPtypeO)^(~15));
    rPtypeO=(PtypeO)(static_cast<int>(rPtypeO)^(~15));
    lPtypeO=(PtypeO)(static_cast<int>(lPtypeO)^(~15));
    up=rotate180(up);
    dp=rotate180(dp);
    rp=rotate180(rp);
    lp=rotate180(lp);
    rook=rotate180(rook);
    myKing=rotate180(myKing);
    opKing=rotate180(opKing);
  }
  assert((Y(myKing)-Y(dp))<(Y(myKing)-Y(up)));
  assert((X(myKing)-X(lp))<(X(myKing)-X(rp)));
  result+=attack_u[index1(opKing,up,uPtypeO,isP)]+
    attack_d[index1(opKing,dp,dPtypeO,isP)]+
    attack_l[index1(opKing,lp,lPtypeO,isP)]+
    attack_r[index1(opKing,rp,rPtypeO,isP)]+
    defense_u[index1(myKing,up,uPtypeO,isP)]+
    defense_d[index1(myKing,dp,dPtypeO,isP)]+
    defense_l[index1(myKing,lp,lPtypeO,isP)]+
    defense_r[index1(myKing,rp,rPtypeO,isP)]+
    attack_nospace[index2(opKing,rook,isP)]+
    defense_nospace[index2(myKing,rook,isP)];
  return result;
}

MultiInt osl::
RookEffectBase::eval(const NumEffectState &state)
{
  std::array<Square,2> kings={Square(0),Square(0)};
  kings[0]=state.kingSquare(BLACK);
  kings[1]=state.kingSquare(WHITE);
  MultiInt result;
  for (int i = indexMin(ROOK); i < indexLimit(ROOK);
       ++i)
  {
    const Piece p = state.pieceOf(i);
    if (! isOnBoard(p)) continue;
    const Square pos=square(p);
    Square up=state.mobilityOf(U,i);
    Square dp=state.mobilityOf(D,i);
    Square lp=state.mobilityOf(L,i);
    Square rp=state.mobilityOf(R,i);
    const bool isP=isPromoted(p);
    if(owner(p)==BLACK)
      result+=evalOne<BLACK>(state,pos,kings[0],kings[1],up,dp,rp,lp,isP);
    else
      result-=evalOne<WHITE>(state,pos,kings[1],kings[0],dp,up,lp,rp,isP);
  }
  return result;
}

void osl::RookEffect::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    attack_table[i][stage] = weights.value(i);
    defense_table[i][stage] = weights.value(i + ONE_DIM);
  }
}

void osl::RookEffectPiece::setUp(const Weights &weights)
{
  for (size_t i = 0; i < 32; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookEffectBase::piece_table[i][s] = weights.value(i + 32*s);
  }
}

void osl::
RookEffectPieceKingRelative::setUp(const Weights &weights)
{
  std::array<MultiInt, 19584> piece_attack_table;
  std::array<MultiInt, 19584> piece_defense_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
    {
      piece_attack_table[i][s] = weights.value(i + ONE_DIM*2*s);
      piece_defense_table[i][s] = weights.value(i + ONE_DIM*2*s + ONE_DIM);
    }
  }
  for(int isP=0;isP<2;isP++)
    for(int y_diff=-9;y_diff<=9;y_diff++)
      for(int x_diff= -9;x_diff<=9;x_diff++){
	int i2=index2(x_diff,y_diff,isP);
	if(abs(x_diff)<9 && abs(y_diff)<9){
	  attack_nospace[i2]= 
	    -(attack_table[index(abs(x_diff),y_diff,true,isP)]+
	      attack_table[index(abs(x_diff),y_diff,false,isP)]);
	  defense_nospace[i2]= 
	    -(defense_table[index(abs(x_diff),y_diff,true,isP)]+
	      defense_table[index(abs(x_diff),y_diff,false,isP)]);
	}
	for(PtypeO ptypeo= PtypeO::MIN;ptypeo<=PtypeO::MAX;ptypeo++){
	  if(getPtype((PtypeO)ptypeo)==Ptype::EMPTY) continue;
	  int i1=index1(x_diff,y_diff,(PtypeO)ptypeo,isP);
	  int indexPieceH,indexPieceV;
	  PtypeO table_ptypeo=ptypeo;
	  if(getPtype((PtypeO)ptypeo)==Ptype::EDGE) table_ptypeo=PtypeO::EDGE;
	  if(getPtype((PtypeO)ptypeo)==Ptype::EDGE || abs(x_diff)==9 || abs(y_diff)==9){
	    indexPieceH= 0+0+I(PtypeO::EDGE)*17*9+4896+isP*9792;
	    indexPieceV= 0+0+I(PtypeO::EDGE)*17*9+ isP*9792;
	  }
	  else{
	    indexPieceH= index0(abs(x_diff),-y_diff,(PtypeO)ptypeo,true,isP);
	    indexPieceV= index0(abs(x_diff),-y_diff,(PtypeO)ptypeo,false,isP);
	  }
	  attack_u[i1]=piece_attack_table[indexPieceV]+piece_table[I(table_ptypeo)];
	  defense_u[i1]=piece_defense_table[indexPieceV];
	  attack_d[i1]=piece_attack_table[indexPieceV]+piece_table[I(table_ptypeo)];
	  defense_d[i1]=piece_defense_table[indexPieceV];
	  if(abs(x_diff)<=8){
	    for(int y_diff_1=y_diff+1;y_diff_1<=8;y_diff_1++){
	      int i=index(abs(x_diff),y_diff_1,false,isP);
	      attack_u[i1]+=attack_table[i];
	      defense_u[i1]+=defense_table[i];
	    }
	    for(int y_diff_1=std::max(-8,y_diff);y_diff_1<=8;y_diff_1++){
	      int i=index(abs(x_diff),y_diff_1,false,isP);
	      attack_d[i1]-=attack_table[i];
	      defense_d[i1]-=defense_table[i];
	    }
	  }
	  attack_l[i1]=piece_attack_table[indexPieceH]+piece_table[I(table_ptypeo)];
	  defense_l[i1]=piece_defense_table[indexPieceH];
	  attack_r[i1]=piece_attack_table[indexPieceH]+piece_table[I(table_ptypeo)];
	  defense_r[i1]=piece_defense_table[indexPieceH];
	  if(abs(y_diff)<=8){
	    for(int x_diff_1=x_diff+1;x_diff_1<=8;x_diff_1++){
	      int i=index(abs(x_diff_1),y_diff,true,isP);
	      attack_r[i1]+=attack_table[i];
	      defense_r[i1]+=defense_table[i];
	    }
	    for(int x_diff_1=std::max(-8,x_diff);x_diff_1<=8;x_diff_1++){
	      int i=index(abs(x_diff_1),y_diff,true,isP);
	      attack_l[i1]-=attack_table[i];
	      defense_l[i1]-=defense_table[i];
	    }
	  }
	}
      }
}



std::array<MultiInt, 256> osl::RookPromoteDefense::promote_defense_table;
std::array<MultiInt, 144> osl::RookPromoteDefense::promote_defense_rook_table;

void osl::RookPromoteDefense::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      promote_defense_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::RookPromoteDefenseRookH::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookPromoteDefense::promote_defense_rook_table[i][s] =
	weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
RookPromoteDefense::eval(const NumEffectState &state)
{
  MultiInt result;
  for(Player P : COLORS){
    const Player altP = alt(P);
    for(int i : state.allNumStrict(P, ROOK)){
      Piece rook = state.pieceOf(i);
      Square rookPos=square(rook);
      if(P == BLACK ? Y(rookPos) >= 4 : Y(rookPos)<=6){
	Square pos=state.mobilityOf((P == BLACK ? U : D), i);
	const Piece attacked = state.pieceAt(pos);
	if (canMoveOn(P,attacked)){
	  if (state.countEffect(altP, pos) == 1){
	    PieceMask mask = state.effect(altP, pos);
	    Piece effect_piece = state.pieceOf(bsf(mask));
	    const int index = I(osl::ptype(attacked)) * 16 + I(osl::ptype(effect_piece));
	    result.addFor(P, promote_defense_table[index]);
	    if (osl::ptype(effect_piece) == ROOK &&
		!isUD(square(effect_piece), rookPos)
	      )
	    {
	      result.addFor(P, promote_defense_rook_table[I(osl::ptype(attacked)) * 9 +
							  state.rookMobilityHorizontal(P, i)]);
	    }
	  }
	}
      }
    }
  }
  return result;
}



std::array<MultiInt, 612> osl::BishopEffectBase::attack_table;
std::array<MultiInt, 612> osl::BishopEffectBase::defense_table;
std::array<MultiInt, 32> osl::BishopEffectBase::piece_table;
std::array<MultiInt, 23104> osl::BishopEffectBase::attack_ul;
std::array<MultiInt, 23104> osl::BishopEffectBase::attack_ur;
std::array<MultiInt, 23104> osl::BishopEffectBase::attack_dl;
std::array<MultiInt, 23104> osl::BishopEffectBase::attack_dr;
std::array<MultiInt, 23104> osl::BishopEffectBase::defense_ul;
std::array<MultiInt, 23104> osl::BishopEffectBase::defense_ur;
std::array<MultiInt, 23104> osl::BishopEffectBase::defense_dl;
std::array<MultiInt, 23104> osl::BishopEffectBase::defense_dr;
std::array<MultiInt, 722> osl::BishopEffectBase::attack_nospace;
std::array<MultiInt, 722> osl::BishopEffectBase::defense_nospace;


template<osl::Player P>
inline
MultiInt osl::BishopEffectBase::evalOne(
  const NumEffectState& state,
  Square bishop,
  Square myKing,
  Square opKing,
  Square ulp,
  Square urp,
  Square dlp,
  Square drp,
  bool isP)
{
  MultiInt result;
  PtypeO ulPtypeO=osl::ptypeO(state.pieceAt(ulp));
  PtypeO urPtypeO=osl::ptypeO(state.pieceAt(urp));
  PtypeO dlPtypeO=osl::ptypeO(state.pieceAt(dlp));
  PtypeO drPtypeO=osl::ptypeO(state.pieceAt(drp));
  if(P==WHITE){
    ulPtypeO=(PtypeO)(static_cast<int>(ulPtypeO)^(~15));
    urPtypeO=(PtypeO)(static_cast<int>(urPtypeO)^(~15));
    dlPtypeO=(PtypeO)(static_cast<int>(dlPtypeO)^(~15));
    drPtypeO=(PtypeO)(static_cast<int>(drPtypeO)^(~15));
    ulp=rotate180(ulp);
    urp=rotate180(urp);
    dlp=rotate180(dlp);
    drp=rotate180(drp);
    bishop=rotate180(bishop);
    myKing=rotate180(myKing);
    opKing=rotate180(opKing);
  }
  result+=attack_ul[index1(opKing,ulp,ulPtypeO,isP)]+
    attack_ur[index1(opKing,urp,urPtypeO,isP)]+
    attack_dl[index1(opKing,dlp,dlPtypeO,isP)]+
    attack_dr[index1(opKing,drp,drPtypeO,isP)]+
    defense_ul[index1(myKing,ulp,ulPtypeO,isP)]+
    defense_ur[index1(myKing,urp,urPtypeO,isP)]+
    defense_dl[index1(myKing,dlp,dlPtypeO,isP)]+
    defense_dr[index1(myKing,drp,drPtypeO,isP)]+
    attack_nospace[index2(opKing,bishop,isP)]+
    defense_nospace[index2(myKing,bishop,isP)];
  return result;
}

MultiInt osl::
BishopEffectBase::eval(const NumEffectState &state)
{
  std::array<Square,2> kings={Square(0),Square(0)};
  kings[0]=state.kingSquare(BLACK);
  kings[1]=state.kingSquare(WHITE);

  MultiInt result;
  for (int i = indexMin(BISHOP); i < indexLimit(BISHOP);
       ++i)
  {
    const Piece p = state.pieceOf(i);
    if (! isOnBoard(p)) continue;
    const Square pos=square(p);
    Square ulp=state.mobilityOf(UL,i);
    Square urp=state.mobilityOf(UR,i);
    Square dlp=state.mobilityOf(DL,i);
    Square drp=state.mobilityOf(DR,i);
    const bool isP=isPromoted(p);
    if(owner(p)==BLACK)
      result+=evalOne<BLACK>(state,pos,kings[0],kings[1],ulp,urp,dlp,drp,isP);
    else
      result-=evalOne<WHITE>(state,pos,kings[1],kings[0],drp,dlp,urp,ulp,isP);
  }
  return result;
}

void osl::BishopEffect::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    attack_table[i][stage] = weights.value(i);
    defense_table[i][stage] = weights.value(i + ONE_DIM);
  }
}

void osl::BishopEffectPiece::setUp(const Weights &weights)
{
  for (size_t i = 0; i < 32; ++i)
  {
    for (int s=0; s<NStages; ++s)
      BishopEffectBase::piece_table[i][s] = weights.value(i + 32*s);
  }
}


void osl::
BishopEffectPieceKingRelative::setUp(const Weights &weights)
{
  std::array<MultiInt, 19584> piece_attack_table;
  std::array<MultiInt, 19584> piece_defense_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s) 
    {
      piece_attack_table[i][s] = weights.value(i + ONE_DIM * 2 * s);
      piece_defense_table[i][s] = weights.value(i + ONE_DIM * 2 * s + ONE_DIM);
    }
  }
  for(int isP=0;isP<2;isP++)
    for(int y_diff=-9;y_diff<=9;y_diff++)
      for(int x_diff= -9;x_diff<=9;x_diff++){
	int i2=index2(x_diff,y_diff,isP);
	if(abs(x_diff)<9 && abs(y_diff)<9){
	  attack_nospace[i2]= 
	    -(attack_table[index(x_diff,y_diff,true,isP)]+
	      attack_table[index(x_diff,y_diff,false,isP)]);
	  defense_nospace[i2]= 
	    -(defense_table[index(x_diff,y_diff,true,isP)]+
	      defense_table[index(x_diff,y_diff,false,isP)]);
	}
	for(PtypeO ptypeo= PtypeO::MIN;ptypeo<=PtypeO::MAX;ptypeo++){
	  if(getPtype((PtypeO)ptypeo)==Ptype::EMPTY) continue;
	  int i1=index1(x_diff,y_diff,(PtypeO)ptypeo,isP);
	  int indexPieceUR,indexPieceUL;
	  PtypeO table_ptypeo=ptypeo;
	  if(getPtype((PtypeO)ptypeo)==Ptype::EDGE) table_ptypeo=PtypeO::EDGE;
	  if(getPtype((PtypeO)ptypeo)==Ptype::EDGE || abs(x_diff)==9 || abs(y_diff)==9){
	    indexPieceUR= 0+0+I(PtypeO::EDGE)*17*9+4896+isP*9792;
	    indexPieceUL= 0+0+I(PtypeO::EDGE)*17*9+ isP*9792;
	  }
	  else{
	    indexPieceUR= index0(x_diff,y_diff,(PtypeO)ptypeo,true,isP);
	    indexPieceUL= index0(x_diff,y_diff,(PtypeO)ptypeo,false,isP);
	  }
	  attack_ul[i1]=piece_attack_table[indexPieceUL]+piece_table[I(table_ptypeo)];
	  defense_ul[i1]=piece_defense_table[indexPieceUL];
	  attack_dr[i1]=piece_attack_table[indexPieceUL]+piece_table[I(table_ptypeo)];
	  defense_dr[i1]=piece_defense_table[indexPieceUL];
	  {
	    int y_diff_1=y_diff+1, x_diff_1=x_diff-1;
	    for(;y_diff_1<=8 && x_diff_1>=-8;y_diff_1++,x_diff_1--){
	      if(std::abs(x_diff_1)<=8 && std::abs(y_diff_1)<=8){
		int i=index(x_diff_1,y_diff_1,false,isP);
		attack_ul[i1]+=attack_table[i];
		defense_ul[i1]+=defense_table[i];
	      }
	    }
	  }
	  {
	    int y_diff_1=y_diff, x_diff_1=x_diff;
	    for(;y_diff_1<=8 && x_diff_1>=-8;y_diff_1++,x_diff_1--){
	      if(std::abs(x_diff_1)<=8 && std::abs(y_diff_1)<=8){
		int i=index(x_diff_1,y_diff_1,false,isP);
		attack_dr[i1]-=attack_table[i];
		defense_dr[i1]-=defense_table[i];
	      }
	    }
	  }
	  attack_ur[i1]=piece_attack_table[indexPieceUR]+piece_table[I(table_ptypeo)];
	  defense_ur[i1]=piece_defense_table[indexPieceUR];
	  attack_dl[i1]=piece_attack_table[indexPieceUR]+piece_table[I(table_ptypeo)];
	  defense_dl[i1]=piece_defense_table[indexPieceUR];
	  {
	    int y_diff_1=y_diff+1, x_diff_1=x_diff+1;
	    for(;y_diff_1<=8 && x_diff_1<=8;y_diff_1++,x_diff_1++){
	      if(std::abs(x_diff_1)<=8 && std::abs(y_diff_1)<=8){
		int i=index(x_diff_1,y_diff_1,true,isP);
		attack_ur[i1]+=attack_table[i];
		defense_ur[i1]+=defense_table[i];
	      }
	    }
	  }
	  {
	    int y_diff_1=y_diff, x_diff_1=x_diff;
	    for(;y_diff_1<=8 && x_diff_1<=8;y_diff_1++,x_diff_1++){
	      if(std::abs(x_diff_1)<=8 && std::abs(y_diff_1)<=8){
		int i=index(x_diff_1,y_diff_1,true,isP);
		attack_dl[i1]-=attack_table[i];
		defense_dl[i1]-=defense_table[i];
	      }
	    }
	  }
	}
      }
}

std::array<MultiInt, 32> osl::BishopHead::table;
std::array<MultiInt, 4896> osl::BishopHead::king_table;
std::array<MultiInt, 160> osl::BishopHead::x_table;

void osl::BishopHead::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
BishopHeadKingRelative::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      BishopHead::king_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for(int x_diff=0;x_diff<=8;x_diff++)
    for(int y_diff=-8;y_diff<=8;y_diff++){
      for(int i=0;i<32;i++)
	BishopHead::king_table[(i*9+x_diff)*17+y_diff+8]+=BishopHead::table[i];
    }
  const PtypeO EMPTY_R=newPtypeO(WHITE,Ptype::EMPTY);
  const PtypeO EDGE_R=newPtypeO(BLACK,Ptype::EDGE);
  for(int x_diff=0;x_diff<=8;x_diff++)
    for(int y_diff=-8;y_diff<=8;y_diff++){
      BishopHead::king_table[(I(EMPTY_R)*9+x_diff)*17+y_diff+8]=
	BishopHead::king_table[(I(PtypeO::EMPTY)*9+x_diff)*17+y_diff+8];
      BishopHead::king_table[(I(EDGE_R)*9+x_diff)*17+y_diff+8]=
	BishopHead::king_table[(I(PtypeO::EDGE)*9+x_diff)*17+y_diff+8];
    }
}

void osl::BishopHeadX::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      BishopHead::x_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

MultiInt osl::
BishopHead::eval(const NumEffectState &state)
{
  MultiInt result;
  for(Player pl : COLORS){
    for(Square pos : state.allSquareStrict(pl,BISHOP)){
      if(pl == BLACK ? Y(pos) >= 2 : Y(pos) <= 8){
	const Square up = nextSquare(pl, pos, U);
	if (!state.hasEffect(pl, up)){
	  Square king = state.kingSquare(pl);
	  PtypeO ptypeo = osl::ptypeO(state.pieceAt(up));
	  int index_k = indexK(pl, ptypeo,
			       std::abs(X(pos) - X(king)),
			       Y(pos) - Y(king));
	  if(pl == BLACK){
	    result += king_table[index_k];
	    result += x_table[indexX(pl, ptypeo, X(pos))];
	  }
	  else{
	    result -= king_table[index_k];
	    result -= x_table[indexX(pl, ptypeo, X(pos))];
	  }
	}
      }
    }
  }
  return result;
}


std::array<MultiInt, 374544> osl::KingRookBishop::table;

void osl::KingRookBishop::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template<osl::Player P>
MultiInt osl::
KingRookBishop::evalOne(const NumEffectState &state)
{
  const Square king=state.kingSquare(P);
  MultiInt result;
  for (int i = indexMin(ROOK);
       i < indexLimit(ROOK);
       ++i)
  {
    const Piece rook = state.pieceOf(i);
    if (!isOnBoard(rook))
    {
      continue;
    }
    for (int j = indexMin(BISHOP);
	 j < indexLimit(BISHOP);
	 ++j)
    {
      const Piece bishop = state.pieceOf(j);
      if (!isOnBoard(bishop))
      {
	continue;
      }
      result += table[index<P>(king, rook, bishop)];
    }
  }
  return result;
}

MultiInt osl::
KingRookBishop::eval(const NumEffectState &state)
{
  return evalOne<BLACK>(state)-evalOne<WHITE>(state);
}


std::array<MultiInt, 9> osl::NumPiecesBetweenBishopAndKing::self_table;
std::array<MultiInt, 9> osl::NumPiecesBetweenBishopAndKing::opp_table;
std::array<MultiInt, 9> osl::NumPiecesBetweenBishopAndKing::all_table;

void osl::
NumPiecesBetweenBishopAndKingSelf::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      NumPiecesBetweenBishopAndKing::self_table[i][s] =
	weights.value(i + ONE_DIM*s);
  }
}

void osl::
NumPiecesBetweenBishopAndKingOpp::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      NumPiecesBetweenBishopAndKing::opp_table[i][s] =
	weights.value(i + ONE_DIM*s);
  }
}

void osl::
NumPiecesBetweenBishopAndKingAll::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      NumPiecesBetweenBishopAndKing::all_table[i][s] =
	weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
NumPiecesBetweenBishopAndKing::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(BISHOP);
       i < indexLimit(BISHOP);
       ++i)
  {
    const Piece bishop = state.pieceOf(i);
    if (!isOnBoard(bishop))
    {
      continue;
    }
    int self, opp, all;
    countBetween(state,
		 state.kingSquare(alt(owner(bishop))),
		 bishop, self, opp, all);
    if (owner(bishop) == BLACK)
    {
      result += (self_table[self] + opp_table[opp] + all_table[all]);
    }
    else
    {
      result -= (self_table[self] + opp_table[opp] + all_table[all]);
    }
  }
  return result;
}

void osl::
NumPiecesBetweenBishopAndKing::countBetween(
  const NumEffectState &state, Square king, Piece bishop,
  int &self_count, int &opp_count, int &total_count)
{
  assert(isOnBoard(bishop));
  if ((X(king) + Y(king) != X(square(bishop)) + Y(square(bishop))) &&
      (X(king) - Y(king) != X(square(bishop)) - Y(square(bishop))))
  {
    self_count = opp_count = total_count = 8;
    return;
  }
  Direction dir;
  assert(X(king) != X(square(bishop)));
  assert(Y(king) != Y(square(bishop)));
  if (X(king) < X(square(bishop)))
  {
    if (Y(king) < Y(square(bishop)))
    {
      dir = UR;
    }
    else
    {
      dir = DR;
    }
  }
  else
  {
    if (Y(king) < Y(square(bishop)))
    {
      dir = UL;
    }
    else
    {
      dir = DL;
    }
  }
  const Player player = owner(bishop);
  const Direction move_dir = (player == BLACK ? dir : rotate180Short(dir));
  self_count = opp_count = total_count = 0;
  for (Square pos = state.mobilityOf(dir, number(bishop));
       pos != king; pos = nextSquare(player, pos, move_dir))
  {
    assert(isOnBoard(pos));
    const Piece piece = state.pieceAt(pos);
    if (!isEmpty(piece))
    {
      ++total_count;
      if (owner(piece) == player)
	++self_count;
      else
	++opp_count;
    }
  }
}


std::array<MultiInt, 64>
osl::BishopBishopPiece::table;

void osl::
BishopBishopPiece::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
BishopBishopPiece::eval(const NumEffectState &state)
{
  MultiInt result;
  const Piece bishop1 = state.pieceOf(indexMin(BISHOP));
  const Piece bishop2 = state.pieceOf(indexMin(BISHOP) + 1);
  if (!isOnBoard(bishop1) || !isOnBoard(bishop2) ||
      owner(bishop1) == owner(bishop2))
    return result;
  if (X(square(bishop1)) + Y(square(bishop1)) !=
      X(square(bishop2)) + Y(square(bishop2)) &&
      X(square(bishop1)) - Y(square(bishop1)) !=
      X(square(bishop2)) - Y(square(bishop2)))
    return result;

  if (state.hasEffect(owner(bishop2), square(bishop1), BISHOP))
    return result;

  Direction dir;
  if (X(square(bishop1)) < X(square(bishop2)))
  {
    if (Y(square(bishop1)) < Y(square(bishop2)))
    {
      dir = UR;
    }
    else
    {
      dir = DR;
    }
  }
  else
  {
    if (Y(square(bishop1)) < Y(square(bishop2)))
    {
      dir = UL;
    }
    else
    {
      dir = DL;
    }
  }
  Square p1 = state.mobilityOf(rotate180Short(dir), number(bishop1));
  Square p2 = state.mobilityOf(dir, number(bishop2));
  if (p1 == p2)
  {
    const Piece p = state.pieceAt(p1);
    const bool black_with_support =
      state.hasEffect(BLACK, owner(bishop1) == BLACK ?
			square(bishop1) : square(bishop2));
    const bool white_with_support =
      state.hasEffect(WHITE, owner(bishop1) == WHITE ?
			square(bishop1) : square(bishop2));
    if (owner(p) == BLACK)
    {
      result += table[index(osl::ptype(p), black_with_support,
			    white_with_support)];
    }
    else
    {
      result -= table[index(osl::ptype(p), white_with_support,
			    black_with_support)];
    }
  }
  return result;
}

std::array<MultiInt, 800>
osl::RookRook::table;

void osl::
RookRook::setUp(const Weights &weights)
{
  std::array<MultiInt, 800> orig_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      orig_table[i][s] = weights.value(i + ONE_DIM*s);
  }
  for (int owner = 0; owner < 2; ++owner)
  {
    const bool same_player = (owner == 0);
    for (int y1 = 0; y1 < 10; ++y1)
    {
      for (int y2 = 0; y2 < 10; ++y2)
      {
	for (int promoted1 = 0; promoted1 < 2; ++promoted1)
	{
	  for (int promoted2 = 0; promoted2 < 2; ++promoted2)
	  {
	    if (same_player)
	    {
	      int y1p = y1;
	      int y2p = y2;
	      int promoted1p = promoted1;
	      int promoted2p = promoted2;
	      if (y1 > y2 || (y1 == y2 && !promoted1 && promoted2))
	      {
		std::swap(y1p, y2p);
		std::swap(promoted1p, promoted2p);
	      }
	      table[index(same_player, promoted1, promoted2,
			  y1, y2)] =
		orig_table[index(same_player, promoted1p, promoted2p,
				 y1p, y2p)];
	    }
	    else
	    {
	      if (y1 + y2 > 10 || y1 == 0 ||
		  (y1 + y2 == 10 && promoted1))
	      {
		const int idx = index(same_player, promoted1, promoted2,
				      y1, y2);
		table[idx] = orig_table[idx];
	      }
	      else
	      {
		table[index(same_player, promoted1, promoted2,
			    y1, y2)] =
		  -orig_table[index(same_player, promoted2, promoted1,
				    (10 - y2) % 10, (10 - y1) % 10)];
	      }
	    }
	  }
	}
      }
    }
  }
}

osl::MultiInt osl::
RookRook::eval(const NumEffectState &state)
{
  MultiInt result;
  Piece rook1 = state.pieceOf(indexMin(ROOK));
  Piece rook2 = state.pieceOf(indexMin(ROOK) + 1);
  if (owner(rook1) == owner(rook2))
  {
    if (owner(rook1) == BLACK)
    {
      result += table[index<true, BLACK>(rook1, rook2)];
    }
    else
    {
      result -= table[index<true, WHITE>(rook1, rook2)];
    }
  }
  else
  {
    if (owner(rook1) != BLACK)
    {
      std::swap(rook1, rook2);
    }
    result += table[index<false, BLACK>(rook1, rook2)];
  }
  return result;
}


std::array<MultiInt, 128>
osl::RookRookPiece::table;

void osl::
RookRookPiece::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
RookRookPiece::eval(const NumEffectState &state)
{
  MultiInt result;
  const Piece rook1 = state.pieceOf(indexMin(ROOK));
  const Piece rook2 = state.pieceOf(indexMin(ROOK) + 1);
  if (!isOnBoard(rook1) || !isOnBoard(rook2) ||
      owner(rook1) == owner(rook2))
    return result;

  if (state.hasEffect(owner(rook2), square(rook1), ROOK))
    return result;

  Direction dir;
  bool vertical = false;
  if (X(square(rook1)) == X(square(rook2)))
  {
    vertical = true;
    if (Y(square(rook1)) < Y(square(rook2)))
    {
      dir = D;
    }
    else
    {
      dir = U;
    }
  }
  else if (Y(square(rook1)) == Y(square(rook2)))
  {
    if (X(square(rook1)) < X(square(rook2)))
    {
      dir = L;
    }
    else
    {
      dir = R;
    }
  }
  else
  {
    return result;
  }

  Square p1 = state.mobilityOf(dir, number(rook1));
  Square p2 = state.mobilityOf(rotate180Short(dir), number(rook2));
  assert(isOnBoard(p1) && isOnBoard(p2));
  if (p1 == p2)
  {
    const Piece p = state.pieceAt(p1);
    const bool black_with_support =
      state.hasEffect(BLACK, owner(rook1) == BLACK ?
			square(rook1) : square(rook2));
    const bool white_with_support =
      state.hasEffect(WHITE, owner(rook1) == WHITE ?
			square(rook1) : square(rook2));
    if (owner(p) == BLACK)
    {
      result += table[index(osl::ptype(p), black_with_support,
			    white_with_support, vertical)];
    }
    else
    {
      result -= table[index(osl::ptype(p), white_with_support,
			    black_with_support, vertical)];
    }
  }
  return result;
}


std::array<MultiInt, 32>
osl::BishopStandFile5::table;

void osl::
BishopStandFile5::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
BishopStandFile5::eval(const NumEffectState &state)
{
  MultiInt result;
  if (state.hasPieceOnStand(BLACK, BISHOP))
  {
    result += table[I(osl::ptypeO(state.pieceAt(newSquare(5, 3))))];
  }
  if (state.hasPieceOnStand(WHITE, BISHOP))
  {
    PtypeO ptypeO = osl::ptypeO(state.pieceAt(newSquare(5, 7)));
    ptypeO = altIfPiece(ptypeO);
    result -= table[I(ptypeO)];
  }
  return result;
}



std::array<MultiInt, osl::MajorCheckWithCapture::ONE_DIM>
osl::MajorCheckWithCapture::table;

void osl::
MajorCheckWithCapture::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template <osl::Player Owner>
osl::MultiInt osl::
MajorCheckWithCapture::addOne(const NumEffectState &state)
{
  const Square king = state.kingSquare(Owner);
  PieceMask pieces = state.effectedMask(alt(Owner));
  pieces &= state.piecesOnBoard(Owner);
  pieces &= ~state.effectedMask(Owner);
  MultiInt sum;
  while (any(pieces)) {
    const Piece p = state.pieceOf(takeOneBit(pieces));
    const Square sq = square(p);
    if (state.hasLongEffect(alt(Owner), sq, ROOK)
	&& state.hasEffectIf(newPtypeO(BLACK,ROOK), sq, king)) {
      if (Owner == BLACK)
	sum += table[index(osl::ptype(p), true, canPromote(alt(Owner),sq))];
      else
	sum -= table[index(osl::ptype(p), true, canPromote(alt(Owner),sq))];
    }
    if (state.hasLongEffect(alt(Owner), sq, BISHOP)
	&& state.hasEffectIf(newPtypeO(BLACK,BISHOP), sq, king)) {
      if (Owner == BLACK)
	sum += table[index(osl::ptype(p), false, canPromote(alt(Owner),sq))];
      else
	sum -= table[index(osl::ptype(p), false, canPromote(alt(Owner),sq))];
    }
  }
  return sum;
}

osl::MultiInt osl::
MajorCheckWithCapture::eval(const NumEffectState &state)
{
  return addOne<BLACK>(state) + addOne<WHITE>(state);
}


std::array<MultiInt, osl::RookSilverKnight::ONE_DIM>
osl::RookSilverKnight::table;

void osl::
RookSilverKnight::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
RookSilverKnight::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(ROOK);
       i < indexLimit(ROOK);
       ++i)
  {
    const Piece rook = state.pieceOf(i);
    if (!isOnBoard(rook))
    {
      continue;
    }
    for (int j = indexMin(SILVER);
	 j < indexLimit(SILVER);
	 ++j)
    {
      const Piece silver = state.pieceOf(j);
      if (!isOnBoard(silver) || isPromoted(silver) ||
          owner(silver) != owner(rook))
      {
        continue;
      }
      for (int k = indexMin(KNIGHT);
           k < indexLimit(KNIGHT);
           ++k)
      {
        const Piece knight = state.pieceOf(k);
        if (!isOnBoard(knight) || isPromoted(knight) ||
            owner(knight) != owner(rook))
        {
          continue;
        }

        if (owner(rook) == BLACK)
        {
          if (X(square(rook)) > 5)
          {
            result += table[index(9 - X(square(rook)), Y(square(rook)) - 1,
				  9 - X(square(silver)), Y(square(silver)) - 1,
				  9 - X(square(knight)), Y(square(knight)) - 1)];
          }
          else
          {
            result += table[index(X(square(rook)) - 1, Y(square(rook)) - 1,
				  X(square(silver)) - 1, Y(square(silver)) - 1,
				  X(square(knight)) - 1, Y(square(knight)) - 1)];
          }
        }
        else
        {
          if (X(square(rook)) >= 5)
          {
	    result -= table[index(9 - X(square(rook)), 9 - Y(square(rook)),
				  9 - X(square(silver)), 9 - Y(square(silver)),
				  9 - X(square(knight)), 9 - Y(square(knight)))];
          }
          else
          {
	    result -= table[index(X(square(rook)) - 1, 9 - Y(square(rook)),
				  X(square(silver)) - 1, 9 - Y(square(silver)),
				  X(square(knight)) - 1, 9 - Y(square(knight)))];
          }
        }
      }
    }
  }
  return result;
}


std::array<MultiInt, osl::BishopSilverKnight::ONE_DIM>
osl::BishopSilverKnight::table;

void osl::
BishopSilverKnight::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

osl::MultiInt osl::
BishopSilverKnight::eval(const NumEffectState &state)
{
  MultiInt result;
  for (int i = indexMin(BISHOP);
       i < indexLimit(BISHOP);
       ++i)
  {
    const Piece bishop = state.pieceOf(i);
    if (!isOnBoard(bishop))
    {
      continue;
    }
    for (int j = indexMin(SILVER);
	 j < indexLimit(SILVER);
	 ++j)
    {
      const Piece silver = state.pieceOf(j);
      if (!isOnBoard(silver) || isPromoted(silver) ||
          owner(silver) != owner(bishop))
      {
        continue;
      }
      for (int k = indexMin(KNIGHT);
           k < indexLimit(KNIGHT);
           ++k)
      {
        const Piece knight = state.pieceOf(k);
        if (!isOnBoard(knight) || isPromoted(knight) ||
            owner(knight) != owner(bishop))
        {
          continue;
        }

        if (owner(bishop) == BLACK)
        {
          if (X(square(bishop)) > 5)
          {
            result += table[index(9 - X(square(bishop)), Y(square(bishop)) - 1,
				  9 - X(square(silver)), Y(square(silver)) - 1,
				  9 - X(square(knight)), Y(square(knight)) - 1)];
          }
          else
          {
            result += table[index(X(square(bishop)) - 1, Y(square(bishop)) - 1,
				  X(square(silver)) - 1, Y(square(silver)) - 1,
				  X(square(knight)) - 1, Y(square(knight)) - 1)];
          }
        }
        else
        {
          if (X(square(bishop)) >= 5)
          {
	    result -= table[index(9 - X(square(bishop)), 9 - Y(square(bishop)),
				  9 - X(square(silver)), 9 - Y(square(silver)),
				  9 - X(square(knight)), 9 - Y(square(knight)))];
          }
          else
          {
	    result -= table[index(X(square(bishop)) - 1, 9 - Y(square(bishop)),
				  X(square(silver)) - 1, 9 - Y(square(silver)),
				  X(square(knight)) - 1, 9 - Y(square(knight)))];
          }
        }
      }
    }
  }
  return result;
}


std::array<MultiInt, osl::AttackMajorsInBase::ONE_DIM>
osl::AttackMajorsInBase::table;

void osl::
AttackMajorsInBase::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i) {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
    if (i > 0)
      table[i] += table[0];
  }
}

// template <osl::Player P>
void osl::
AttackMajorsInBase::addOne(osl::Player P, const NumEffectState &state, Piece rook, MultiInt& result)
{
  Square sq = square(rook);
  if (state.hasEffect(alt(P), sq)
      || Y(forBlack(P,sq)) < 8)
    return;
  typedef std::pair<Offset,Square> pair_t;
  std::array<pair_t, 7> bishop_attack;
  bishop_attack[0]=pair_t(newOffset(P,U), nextSquare(P, sq, UL));
  bishop_attack[1]=pair_t(newOffset(P,U), nextSquare(P, sq, UR));
  bishop_attack[2]=pair_t(newOffset(P,L), nextSquare(P, sq, UL));
  bishop_attack[3]=pair_t(newOffset(P,R), nextSquare(P, sq, UR));
  bishop_attack[4]=pair_t(newOffset(P,D), nextSquare(P, sq, UL));
  bishop_attack[5]=pair_t(newOffset(P,D), nextSquare(P, sq, UR));
  bishop_attack[6]=pair_t(newOffset(P,U), nextSquare(P, sq, U));
  const bool has_gold = state.hasPieceOnStand(alt(P), GOLD);
  const bool rook_support = state.hasEffect(P, sq);
  for(pair_t pair : bishop_attack) {
    const Square attack_square = pair.second;
    if (! isEmpty(state[attack_square])
	|| state.countEffect(P, attack_square) > 1)
      continue;
    const Square bishop_square = attack_square + pair.first;
    Piece p = state[bishop_square];
    if (! isPlayerPtype(p,P,BISHOP)
	|| state.hasEffect(alt(P), bishop_square))
      continue;
    int a = state.countEffect(alt(P), attack_square) + has_gold;
    if (a <= state.countEffect(P, attack_square))
      continue;
    const int i = index(osl::ptype(state.findCheapAttack(P, attack_square)),
			osl::ptype(state.findCheapAttack(alt(P), attack_square)),
			has_gold, rook_support,
			state.hasEffectNotBy(P, rook, bishop_square));
    if (P == BLACK)
      result += table[i];
    else
      result -= table[i];
  }
}

osl::MultiInt osl::
AttackMajorsInBase::eval(const NumEffectState &state)
{
  MultiInt result;
  for(Player P : COLORS)
    for(Piece rook : state.allPieceStrict(P, ROOK))
      addOne(P, state, rook, result);    
  return result;
}


namespace osl
{
  template MultiInt KingRookBishop::evalOne<BLACK>(const NumEffectState &state);
  template MultiInt KingRookBishop::evalOne<WHITE>(const NumEffectState &state);
}
std::array<osl::MultiInt, osl::Piece_SIZE>
osl::PieceStand::table;

void osl::
PieceStand::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i)
  {
    table[i][stage] = weights.value(i) + PieceEval::table[int(ptypes[i])];
  }
}

osl::MultiInt osl::PieceStand::eval(
  const osl::NumEffectState &state)
{
  MultiInt result;
  for(Ptype ptype : osl::PieceStandOrder)
  {
    const int black_count =
      state.countPiecesOnStand(BLACK, ptype);
    const int white_count =
      state.countPiecesOnStand(WHITE, ptype);
    for (int j = 0; j < black_count; ++j)
    {
      result += table[indexMin(ptype) + j];
    }
    for (int j = 0; j < white_count; ++j)
    {
      result -= table[indexMin(ptype) + j];
    }
  }
  return result;
}



std::array<osl::MultiInt, 21>
osl::NonPawnPieceStand::table;

void osl::
NonPawnPieceStand::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < weights.dimension(); ++i)
  {
    table[i][stage] = weights.value(i);
  }
}

osl::MultiInt osl::
NonPawnPieceStand::eval(int black_count, int white_count)
{
  return table[black_count] - table[white_count];
}


std::array<osl::MultiInt, 5625> osl::NonPawnPieceStandCombination::table;
std::array<osl::MultiInt, 5625> osl::NonPawnPieceStandCombination::check_table;

osl::MultiInt osl::
NonPawnPieceStandCombination::sumUp(const std::array<int, 6> &indices,
				    const std::array<MultiInt, 5625> &values)
{
  osl::MultiInt result;
  for (int rook = 0; rook <= indices[0]; ++rook)
  {
    for (int bishop = 0; bishop <= indices[1]; ++bishop)
    {
      for (int gold = 0; gold <= indices[2]; ++gold)
      {
	for (int silver = 0; silver <= indices[3]; ++silver)
	{
	  for (int knight = 0; knight <= indices[4]; ++knight)
	  {
	    for (int lance = 0; lance <= indices[5]; ++lance)
	    {
	      if (rook + bishop + gold + silver + knight + lance == 0)
	      {
		continue;
	      }
	      result += values[index(rook, bishop,
				     gold, silver, knight, lance)];
	    }
	  }
	}
      }
    }
  }
  return result;
}

void osl::
NonPawnPieceStandCombination::setUp(const Weights &weights)
{
  std::array<MultiInt, 5625> orig_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
    {
      orig_table[i][s] = weights.value(i + ONE_DIM*s);
    }
  }
  std::array<int, 6> indices;
  for (indices[0] = 0; indices[0] <= 2; ++indices[0])
  {
    for (indices[1] = 0; indices[1] <= 2; ++indices[1])
    {
      for (indices[2] = 0; indices[2] <= 4; ++indices[2])
      {
	for (indices[3] = 0; indices[3] <= 4; ++indices[3])
	{
	  for (indices[4] = 0; indices[4] <= 4; ++indices[4])
	  {
	    for (indices[5] = 0; indices[5] <= 4; ++indices[5])
	    {
	      table[index(indices[0],
			  indices[1],
			  indices[2],
			  indices[3],
			  indices[4],
			  indices[5])] = sumUp(indices, orig_table);
	    }
	  }
	}
      }
    }
  }
  table[0] = orig_table[0];
}

void osl::
CanCheckNonPawnPieceStandCombination::setUp(const Weights &weights)
{
  std::array<MultiInt, 5625> orig_table;
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
    {
      orig_table[i][s] = weights.value(i + ONE_DIM*s);
    }
  }
  std::array<int, 6> indices;
  for (indices[0] = 0; indices[0] <= 2; ++indices[0])
  {
    for (indices[1] = 0; indices[1] <= 2; ++indices[1])
    {
      for (indices[2] = 0; indices[2] <= 4; ++indices[2])
      {
	for (indices[3] = 0; indices[3] <= 4; ++indices[3])
	{
	  for (indices[4] = 0; indices[4] <= 4; ++indices[4])
	  {
	    for (indices[5] = 0; indices[5] <= 4; ++indices[5])
	    {
	      NonPawnPieceStandCombination::check_table[
		NonPawnPieceStandCombination::index(indices[0],
						    indices[1],
						    indices[2],
						    indices[3],
						    indices[4],
						    indices[5])] =
		NonPawnPieceStandCombination::sumUp(indices, orig_table);
	    }
	  }
	}
      }
    }
  }
  NonPawnPieceStandCombination::check_table[0] = orig_table[0];
}

osl::MultiInt osl::
NonPawnPieceStandCombination::eval(const NumEffectState &state,
				   const std::array<bool, 2> &can_check)
{
  const int black_index = index(state.countPiecesOnStand(BLACK, ROOK),
				state.countPiecesOnStand(BLACK, BISHOP),
				state.countPiecesOnStand(BLACK, GOLD),
				state.countPiecesOnStand(BLACK, SILVER),
				state.countPiecesOnStand(BLACK, KNIGHT),
				state.countPiecesOnStand(BLACK, LANCE));
  const int white_index = index(state.countPiecesOnStand(WHITE, ROOK),
				state.countPiecesOnStand(WHITE, BISHOP),
				state.countPiecesOnStand(WHITE, GOLD),
				state.countPiecesOnStand(WHITE, SILVER),
				state.countPiecesOnStand(WHITE, KNIGHT),
				state.countPiecesOnStand(WHITE, LANCE));
  MultiInt result;
  result = table[black_index] - table[white_index];
  if (can_check[I(WHITE)])
  {
    result += check_table[black_index];
  }
  if (can_check[I(BLACK)])
  {
    result -= check_table[white_index];
  }
  return result;
}

osl::MultiInt osl::
NonPawnPieceStandCombination::evalWithUpdate(
  const NumEffectState &state,
  Move moved,
  const MultiInt &last_value,
  const std::array<bool, 2> &could_check,
  const std::array<bool, 2> &can_check)
{
  if (!isDrop(moved) && ! isCapture(moved) &&
      could_check[0] == can_check[0] && could_check[1] == can_check[1])
  {
    return last_value;
  }
  return eval(state, can_check);
}


std::array<osl::MultiInt, 44> osl::NonPawnPieceStandTurn::table;

void osl::
NonPawnPieceStandTurn::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
NonPawnPieceStandTurn::eval(const NumEffectState &state, MultiIntPair& result)
{
  result = MultiIntPair();
  for(Ptype ptype : osl::PieceStandOrder)
  {
    if (ptype == PAWN)
      continue;
    const int black_count = state.countPiecesOnStand(BLACK, ptype);
    const int white_count = state.countPiecesOnStand(WHITE, ptype);
    for (int j = 0; j < black_count; ++j)
    {
      const int index_black = index(BLACK, BLACK, ptype, j);
      const int index_white = index(BLACK, WHITE, ptype, j);
      result[I(BLACK)] += table[index_black];
      result[I(WHITE)] += table[index_white];
    }
    for (int j = 0; j < white_count; ++j)
    {
      const int index_black = index(WHITE, BLACK, ptype, j);
      const int index_white = index(WHITE, WHITE, ptype, j);
      result[I(BLACK)] -= table[index_black];
      result[I(WHITE)] -= table[index_white];
    }
  }
}

template<osl::Player P>
void osl::
NonPawnPieceStandTurn::evalWithUpdateBang(
  const NumEffectState &state,
  Move moved, MultiIntPair &result)
{
  assert(P==player(moved));
  if (!isDrop(moved) && ! isCapture(moved))
    return;

  if (isDrop(moved))
  {
    const Ptype ptype = osl::ptype(moved);
    if (ptype == PAWN)
      return;
    const int count =
      state.countPiecesOnStand(P, osl::ptype(moved));
    const int index_black = index(P, BLACK, osl::ptype(moved), count);
    const int index_white = index(P, WHITE, osl::ptype(moved), count);
    if(P==BLACK){
      result[I(BLACK)] -= table[index_black];
      result[I(WHITE)] -= table[index_white];
    }
    else{
      result[I(BLACK)] += table[index_black];
      result[I(WHITE)] += table[index_white];
    }
  }
  if (isCapture(moved) &&
      unpromote(capturePtype(moved)) != PAWN)
  {
    Ptype ptype = unpromote(capturePtype(moved));
    const int count = state.countPiecesOnStand(P, ptype) - 1;
    const int index_black = index(P, BLACK, ptype, count);
    const int index_white = index(P, WHITE, ptype, count);
    if(P==BLACK){
      result[I(BLACK)] += table[index_black];
      result[I(WHITE)] += table[index_white];
    }
    else{
      result[I(BLACK)] -= table[index_black];
      result[I(WHITE)] -= table[index_white];
    }
  }
}


std::array<osl::MultiInt, 360> osl::PieceStandY::y_attack_table;
std::array<osl::MultiInt, 360> osl::PieceStandY::y_defense_table;
std::array<osl::MultiInt, 9*7*19> osl::PieceStandY::y_attack_table_sum;
std::array<osl::MultiInt, 9*7*19> osl::PieceStandY::y_defense_table_sum;

void osl::
PieceStandY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s) 
    {
      y_attack_table[i][s] = weights.value(i + ONE_DIM * 2 * s);
      y_defense_table[i][s] = weights.value(i + ONE_DIM * 2 * s + ONE_DIM);
    }
  }
  for (int i=0;i<7;i++){
    Ptype ptype=osl::PieceStandOrder[i];
    int ptypeSize=indexLimit(ptype)-indexMin(ptype);
    for(int king_y=1;king_y<=9;king_y++){
      MultiInt attack_sum, defense_sum;
      for(int count=0;count<=ptypeSize;count++){
	int oldIndex=index(ptype,BLACK,newSquare(5,king_y),count);
	int newIndex=index(i,BLACK,newSquare(5,king_y),count);
	y_attack_table_sum[newIndex]=attack_sum;
	y_defense_table_sum[newIndex]=defense_sum;
	if(count==ptypeSize) break;
	attack_sum += y_attack_table[oldIndex];
	defense_sum += y_defense_table[oldIndex];
      }
    }
  }
}

inline 
void osl::PieceStandY::updateResult(osl::NumEffectState const& state,osl::MultiInt &result,int i, osl::Ptype ptype, std::array<osl::Square,2> const&kings)
{
  const int black_count = state.countPiecesOnStand(BLACK, ptype);
  const int white_count = state.countPiecesOnStand(WHITE, ptype);
  const int attack_index_1 = PieceStandY::index(i, BLACK, kings[I(WHITE)], black_count);
  const int attack_index_2 = PieceStandY::index(i, WHITE, kings[I(BLACK)], white_count);
  const int defense_index_1 = PieceStandY::index(i, BLACK, kings[I(BLACK)], black_count);
  const int defense_index_2 = PieceStandY::index(i, WHITE, kings[I(WHITE)], white_count);
  result += y_attack_table_sum[attack_index_1] - y_attack_table_sum[attack_index_2] +
    y_defense_table_sum[defense_index_1] - y_defense_table_sum[defense_index_2];
}

osl::MultiInt osl::
PieceStandY::eval(const NumEffectState &state)
{
  MultiInt result;
  std::array<Square,2> kings={Square(0),Square(0)};
  kings[0]=state.kingSquare(BLACK);
  kings[1]=state.kingSquare(WHITE);
  updateResult(state,result,0,ROOK,kings);
  updateResult(state,result,1,BISHOP,kings);
  updateResult(state,result,2,GOLD,kings);
  updateResult(state,result,3,SILVER,kings);
  updateResult(state,result,4,KNIGHT,kings);
  updateResult(state,result,5,LANCE,kings);
  updateResult(state,result,6,PAWN,kings);
  return result;
}

template<osl::Player P>
osl::MultiInt osl::
PieceStandY::evalWithUpdate(
  const NumEffectState &state,
  Move moved, const MultiInt &last_value)
{
  if (osl::ptype(moved) == KING)
    return eval(state);

  MultiInt result(last_value);
  if (isDrop(moved))
  {
    const Ptype ptype = osl::ptype(moved);
    const int count =
      state.countPiecesOnStand(P, ptype);
    const int attack_index = index(ptype, P,
				   state.kingSquare(alt(P)),
				   count);
    const int defense_index = index(ptype, P,
				    state.kingSquare(P),
				    count); 
    if(P==BLACK)
      result -= y_attack_table[attack_index] +y_defense_table[defense_index];
    else
      result += y_attack_table[attack_index] +y_defense_table[defense_index];
  }
  if (isCapture(moved))
  {
    Ptype ptype = unpromote(capturePtype(moved));
    const int count = state.countPiecesOnStand(P, ptype)-1;
    const int attack_index = index(ptype, P,
				   state.kingSquare(alt(P)),
				   count);
    const int defense_index = index(ptype, P,
				    state.kingSquare(P),
				    count);
    if(P==BLACK)
      result += y_attack_table[attack_index] +y_defense_table[defense_index];
    else
      result -= y_attack_table[attack_index] +y_defense_table[defense_index];
  }
  return result;
}

std::array<osl::MultiInt, 16384> osl::PieceStandCombinationBoth::table;

void osl::
PieceStandCombinationBoth::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    int low = (i & 0x7F);
    int high = (i >> 7);
    if (low == high)
      continue;
    for (int s = 0; s < NStages; ++s)
    {
      table[i][s] = weights.value(i + ONE_DIM*s);
      if (high > low)
      {
	table[(low << 7) | high][s] = -table[i][s];
      }
    }
  }
}

osl::MultiInt osl::
PieceStandCombinationBoth::eval(const NumEffectState &state)
{
  int black_index = 0;
  int white_index = 0;
  black_index |= ((state.hasPieceOnStand(BLACK, ROOK) ? 1 : 0) << 6);
  black_index |= ((state.hasPieceOnStand(BLACK, BISHOP) ? 1 : 0) << 5);
  black_index |= ((state.hasPieceOnStand(BLACK, GOLD) ? 1 : 0) << 4);
  black_index |= ((state.hasPieceOnStand(BLACK, SILVER) ? 1 : 0) << 3);
  black_index |= ((state.hasPieceOnStand(BLACK, KNIGHT) ? 1 : 0) << 2);
  black_index |= ((state.hasPieceOnStand(BLACK, LANCE) ? 1 : 0) << 1);
  black_index |= ((state.hasPieceOnStand(BLACK, PAWN) ? 1 : 0) << 0);
  white_index |= ((state.hasPieceOnStand(WHITE, ROOK) ? 1 : 0) << 6);
  white_index |= ((state.hasPieceOnStand(WHITE, BISHOP) ? 1 : 0) << 5);
  white_index |= ((state.hasPieceOnStand(WHITE, GOLD) ? 1 : 0) << 4);
  white_index |= ((state.hasPieceOnStand(WHITE, SILVER) ? 1 : 0) << 3);
  white_index |= ((state.hasPieceOnStand(WHITE, KNIGHT) ? 1 : 0) << 2);
  white_index |= ((state.hasPieceOnStand(WHITE, LANCE) ? 1 : 0) << 1);
  white_index |= ((state.hasPieceOnStand(WHITE, PAWN) ? 1 : 0) << 0);
  return table[(black_index << 7) | white_index];
}


namespace osl
{
  template void NonPawnPieceStandTurn::evalWithUpdateBang<BLACK>(const NumEffectState &, Move, MultiIntPair &);
  template void NonPawnPieceStandTurn::evalWithUpdateBang<WHITE>(const NumEffectState &, Move, MultiIntPair &);
  template MultiInt PieceStandY::evalWithUpdate<BLACK>(const NumEffectState &, Move, const MultiInt &);
  template MultiInt PieceStandY::evalWithUpdate<WHITE>(const NumEffectState &, Move, const MultiInt &);
}
/* piecePair.cc
 */

osl::PiecePair::IndexTable osl::PiecePair::plain_table;
std::array<osl::PiecePair::IndexTable, 10> osl::PiecePair::x_table, 
  osl::PiecePair::y_table;
const std::array<const osl::Offset, 12> osl::PiecePair::offsets = 
{{
    // positive offset [0,5]
    newOffset( BLACK,UUL), 
    newOffset( BLACK,UL), 
    newOffset( BLACK,L), 
    newOffset( BLACK,DL), 
    newOffset( WHITE,UUR), 
    newOffset( BLACK,D), 
    // negative offset [6,11]
    newOffset( WHITE,UUL), 
    newOffset( BLACK,DR), 
    newOffset( BLACK,R), 
    newOffset( BLACK,UR), 
    newOffset( BLACK,UUR), 
    newOffset( BLACK,U), 
  }};

namespace osl
{
  std::array<int, 0x200> offset_index;
  PiecePair::IndexTable& plain_table = PiecePair::plain_table;
  std::array<PiecePair::IndexTable, 10>& x_table = PiecePair::x_table;
  std::array<PiecePair::IndexTable, 10>& y_table = PiecePair::y_table;

  void makeOffsetIndex()
  {
    offset_index.fill(-1);
    for (size_t i=0; i<PiecePair::offsets.size(); ++i) {
      offset_index[I(PiecePair::offsets[i])] = i;
    }
  }
  inline int inv(int offset_id)
  {
    assert(offset_id >= 0 && offset_id < 12);
    return (offset_id + 6) % 12;
  }
  inline int swaplr(int offset_id)
  {
    assert(offset_id >= 0 && offset_id < 12);
    if (offset_id == 11) 
      return 11;
    return 10 - offset_id;
  }
  inline int swapud(int offset_id)
  {
    assert(offset_id >= 0 && offset_id < 12);
    return swaplr(inv(offset_id));
  }    
  int pindex(Player player, Ptype ptype) { return PiecePair::IndexTable::pindex(player, ptype); }
  void makeTable()
  {
    int index = 0;
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=ip0; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	// same player
	{
	  ++index;
	  plain_table.fillSame(index,  0, p0, p1); // UUL
	  plain_table.fillSame(index, 10, p0, p1); // UUR
	  if (p0 != p1) {
	    ++index;
	    plain_table.fillSame(index,  0, p1, p0); // UUL
	    plain_table.fillSame(index, 10, p1, p0); // UUR
	  }

	  ++index;
	  plain_table.fillSame(index, 1, p0, p1); // UL
	  plain_table.fillSame(index, 9, p0, p1); // UR
	  if (p0 != p1) {
	    ++index;
	    plain_table.fillSame(index, 1, p1, p0); // UR
	    plain_table.fillSame(index, 9, p1, p0); // UL
	  }
	    
	  ++index;
	  plain_table.fillSame(index, 2, p0, p1); // L
	  plain_table.fillSame(index, 8, p0, p1); // R
	  if (p0 != p1) { // use the same index as L
	    plain_table.fillSame(index, 2, p1, p0); // L
	    plain_table.fillSame(index, 8, p1, p0); // R
	  }
	    
	  ++index;
	  plain_table.fillSame(index, 11, p0, p1); // U
	  if (p0 != p1) {
	    plain_table.fillSame(index, 11, p1, p0); // U
	  }
	}
	// different player
	{
	  // UUL, UUR
	  ++index;
	  plain_table.fillDiffer(index, 0, p0, p1); // UUL
	  plain_table.fillDiffer(index, 10, p0, p1); // UUR
	  ++index;
	  plain_table.fillDiffer(index, inv(0), p0, p1); // UUL^-1
	  plain_table.fillDiffer(index, inv(10), p0, p1); // UUR^-1

	  // UL, UR
	  ++index;
	  plain_table.fillDiffer(index, 1, p0, p1); // UL
	  plain_table.fillDiffer(index, 9, p0, p1); // UR
	  ++index;
	  // DR, DL
	  plain_table.fillDiffer(index, inv(1), p0, p1); // DR
	  plain_table.fillDiffer(index, inv(9), p0, p1); // DL

	  // LR
	  ++index;
	  plain_table.fillDiffer(index, 2, p0, p1); // L
	  plain_table.fillDiffer(index, inv(2), p0, p1); // R, use the same index as L

	  // UD
	  ++index;
	  plain_table.fillDiffer(index, 11, p0, p1); // U

	  ++index;
	  plain_table.fillDiffer(index, inv(11), p0, p1); // D
	}
      }
    }  
    assert(index+1 == PiecePair::plain_table_size);
  }
  void makeTableX()
  {
    // currently only for same player
    int index = 0;
    // make leftside
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0),  pi1 = pindex(BLACK, p1);
	for (int x=1; x<=5; ++x) {
	  // (UUL, DDL), (UL, DL)
	  for (int d=0; d<2; ++d) {
	    ++index;
	    x_table[x][d][pi0][pi1] = index;
	    x_table[x][swapud(d)][pi0][pi1] = index;
	  }
	  // L
	  ++index;
	  x_table[x][2][pi0][pi1] = index;
	  // U, D
	  ++index;
	  x_table[x][11][pi0][pi1] = index;
	  x_table[x][inv(11)][pi1][pi0] = index;
	  ++index;
	  x_table[x][5][pi0][pi1] = index;
	  x_table[x][inv(5)][pi1][pi0] = index;
	} // x
      }
    }
    // make rightside
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0),  pi1 = pindex(BLACK, p1);
	for (int x=2; x<=5; ++x) {
	  // (UUL, DDL), (UL, DL) => (DDR, UUR), (DR, UR)
	  for (int d=0; d<2; ++d) {
	    x_table[x-1][inv(d)][pi1][pi0]         = x_table[x][d][pi0][pi1];
	    x_table[x-1][inv(swapud(d))][pi1][pi0] = x_table[x][swapud(d)][pi0][pi1];
	  }
	  // L => R
	  x_table[x-1][swaplr(2)][pi1][pi0] = x_table[x][2][pi0][pi1];
	}
	// flip col 5
	for (size_t d=0; d<PiecePair::offsets.size(); ++d) {
	  if (swaplr(d) == (int)d || x_table[5][d][pi0][pi1] == 0)
	    continue;
	  x_table[5][swaplr(d)][pi0][pi1] = x_table[5][d][pi0][pi1];
	}
      }
    }
    // mirror to [6,9]
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
	for (int x=6; x<=9; ++x) {
	  for (size_t d=0; d<PiecePair::offsets.size(); ++d) {
	    x_table[x][d][pi0][pi1] = x_table[10-x][swaplr(d)][pi0][pi1];
	  }
	} // x
      }
    }
    // make white player
    for (int x=1; x<=9; ++x) {
      for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
	for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	  const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	  const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
	  const int pi0w = pindex(WHITE, p0), pi1w = pindex(WHITE, p1);
	  for (size_t d=0; d<PiecePair::offsets.size(); ++d) {
	    assert(x_table[x][d][pi0][pi1]);
	    x_table[10-x][inv(d)][pi0w][pi1w] = -x_table[x][d][pi0][pi1];
	  }
	}
      }
    }
    assert(PiecePair::x_table_size == index+1);
    for (int x=1; x<=9; ++x)
      x_table[x].amplify(PiecePair::plain_table_size);
  }
  int wrap9(int y) 
  {
    return (y-1)%9 + 1;
  }
  void makeTableY()
  {
    // only for same player
    int index = 0;
    // for upside direction
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
	// same player
	for (int y=1; y<=9; ++y) {
	  for (int d=0; d<2; ++d) { // (UUL, UUR), (UL, UR)
	    ++index;
	    y_table[y][d][pi0][pi1] = index;
	    y_table[y][swaplr(d)][pi0][pi1] = index;
	  }
	  // (L, R)
	  ++index;
	  y_table[y][2][pi0][pi1] = index;
	  y_table[y][2][pi1][pi0] = index;
	  y_table[y][swaplr(2)][pi0][pi1] = index;
	  y_table[y][swaplr(2)][pi1][pi0] = index;
	  // U
	  ++index;
	  y_table[y][11][pi0][pi1] = index;
	} // y
      }
    }
    // flip for downside direction
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
	for (int y=1; y<=9; ++y) {
	  // (UUL, UUR), 
	  y_table[wrap9(y+2)][inv(0)][pi1][pi0]         = y_table[y][0][pi0][pi1];
	  y_table[wrap9(y+2)][inv(swaplr(0))][pi1][pi0] = y_table[y][swaplr(0)][pi0][pi1];
	  // (UL, UR)
	  y_table[wrap9(y+1)][inv(1)][pi1][pi0]         = y_table[y][1][pi0][pi1];
	  y_table[wrap9(y+1)][inv(swaplr(1))][pi1][pi0] = y_table[y][swaplr(1)][pi0][pi1];
	  // U
	  y_table[wrap9(y+1)][inv(11)][pi1][pi0] = y_table[y][11][pi0][pi1];
	} // y
      }
    }
    // make white player
    for (Ptype ip0=Ptype::PIECE_MIN; ip0<=Ptype::MAX; ++ip0) {
      for (Ptype ip1=Ptype::PIECE_MIN; ip1<=Ptype::MAX; ++ip1) {
	const Ptype p0 = static_cast<Ptype>(ip0), p1 = static_cast<Ptype>(ip1);
	const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
	const int pi0w = pindex(WHITE, p0), pi1w = pindex(WHITE, p1);
	for (int y=1; y<=9; ++y) {
	  for (size_t d=0; d<PiecePair::offsets.size(); ++d) {
	    y_table[10-y][inv(d)][pi0w][pi1w] = -y_table[y][d][pi0][pi1];
	  }
	}
      }
    }
    assert(PiecePair::y_table_size == index+1);
    for (int y=1; y<=9; ++y)
      y_table[y].amplify(PiecePair::plain_table_size+PiecePair::x_table_size);
  }

  std::array<std::array<std::array<int, PTYPEO_SIZE>, 12>, PTYPEO_SIZE> x_values[10], y_values[10]; // plain_values 縺ｯ x縺ｫ謚倩ｾｼ
}

/* ------------------------------------------------------------------------- */
osl::
PiecePair::IndexTable::IndexTable()
{
  for(auto& v1 : *this) for(auto& v : v1) v.fill(0);
}

void osl::
PiecePair::IndexTable::amplify(int base)
{
  for (size_t d=0; d<offsets.size(); ++d) {
    for (int ip0=0; ip0<PTYPEO_SIZE; ++ip0) {
      for (int ip1=0; ip1<PTYPEO_SIZE; ++ip1) {
	signed short& target = (*this)[d][ip0][ip1];
	if (target > 0) {
	  target += base;
	}
	else if (target < 0)
	{
	  target -= base;
	}
      }
    }
  }
}
void osl::
PiecePair::IndexTable::fillBW(int index, int dir, Ptype p0, Ptype p1) 
{
  const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
  const int pi0w = pindex(WHITE, p0), pi1w = pindex(WHITE, p1);

  (*this)[dir][pi0][pi1] = index; // normal
  (*this)[inv(dir)][pi0w][pi1w] = -index; // white
}
void osl::
PiecePair::IndexTable::fillSame(int index, int dir, Ptype p0, Ptype p1) 
{
  fillBW(index, dir, p0, p1);
  fillBW(index, inv(dir), p1, p0); // swapped order
}
void osl::
PiecePair::IndexTable::fillDiffer(int index, int dir, Ptype p0, Ptype p1) 
{
  const int pi0 = pindex(BLACK, p0), pi1 = pindex(BLACK, p1);
  const int pi0w = pindex(WHITE, p0), pi1w = pindex(WHITE, p1);

  (*this)[inv(dir)][pi0][pi1w] = index;
  (*this)[dir][pi1w][pi0] = index; // swapped piece
  (*this)[inv(dir)][pi1][pi0w] = -index; // swapped player
  (*this)[dir][pi0w][pi1] = -index; // swapped player, swapped piece
}
/* ------------------------------------------------------------------------- */


void osl::
PiecePair::init()
{
  static bool initialized = false;
  if (initialized)
    return;
  initialized = true;
  makeOffsetIndex();
  makeTable();
  makeTableX();
  makeTableY();
}

void osl::
PiecePair::compile(const Weights& weights)
{
  for (int i=1; i<=9; ++i) {
    for(auto& v1 : x_values[i]) for(auto& v : v1) v.fill(0);
    for(auto& v1 : y_values[i]) for(auto& v : v1) v.fill(0);
  }
  for (size_t d=0; d<offsets.size(); ++d) {
    for (int ip0=0; ip0<PTYPEO_SIZE; ++ip0) {
      for (int ip1=0; ip1<PTYPEO_SIZE; ++ip1) {
	int plain = 0;
	if (plain_table[d][ip0][ip1] > 0)
	  plain = weights.value(plain_table[d][ip0][ip1]);
	else if (plain_table[d][ip0][ip1] < 0)
	  plain = -weights.value(-plain_table[d][ip0][ip1]);
	for (int i=1; i<=9; ++i) {
	  x_values[i][ip0][d][ip1] = plain;
	  if (x_table[i][d][ip0][ip1] > 0)
	    x_values[i][ip0][d][ip1] += weights.value(x_table[i][d][ip0][ip1]);
	  else if (x_table[i][d][ip0][ip1] < 0)
	    x_values[i][ip0][d][ip1] += -weights.value(-x_table[i][d][ip0][ip1]);
	  if (y_table[i][d][ip0][ip1] > 0)
	    y_values[i][ip0][d][ip1] = weights.value(y_table[i][d][ip0][ip1]);
	  else if (y_table[i][d][ip0][ip1] < 0)
	    y_values[i][ip0][d][ip1] = -weights.value(-y_table[i][d][ip0][ip1]);
	}
      }
    }
  }  
}

void osl::
PiecePair::sanitize(Weights& values)
{
  values.setValue(0,0);
  for (int x=1; x<=9; ++x) {
    for (int y=1; y<=9; ++y) {
      const Square pos1=newSquare(x,y);
      for (size_t i=0; i<offsets.size(); ++i) {
	const Square pos0 = pos1+offsets[i];
	if (! isOnBoard(pos0))
	  continue;
	for (Ptype p=Ptype::PIECE_MIN; p<=Ptype::MAX; ++p) {
	  const Ptype ptype = static_cast<Ptype>(p);
	  assert(isPiece(ptype));
	  index_t idx = index(i, pos0, newPtypeO(BLACK, ptype), pos1, newPtypeO(WHITE, ptype));
	  values.setValue(abs(idx[0]), 0);
	  idx = index(i, pos0, newPtypeO(WHITE, ptype), pos1, newPtypeO(BLACK, ptype));
	  values.setValue(abs(idx[0]), 0);
	}
      }
    }
  }
}

osl::PiecePair::index_t osl::
PiecePair::index(int offset_id, Square pos0, PtypeO p0, Square 
#ifndef NDEBUG
pos1
#endif
, PtypeO p1)
{
  assert(pos0 != pos1);
  assert(! isPieceStand(pos0) && ! isPieceStand(pos1));

  assert(pos0 - pos1 == offsets[offset_id]);
  index_t ret = {{
      plain_table[offset_id][I(p0)][I(p1)],
      x_table[X(pos0)][offset_id][I(p0)][I(p1)],
      y_table[Y(pos0)][offset_id][I(p0)][I(p1)],
    }};
  assert(abs(ret[0]) < plain_table_size);
  assert(abs(ret[1]) < plain_table_size + x_table_size);
  assert(abs(ret[2]) < plain_table_size + x_table_size + y_table_size);
  assert(ret[1] == 0 || abs(ret[1]) > plain_table_size);
  assert(ret[2] == 0 || abs(ret[2]) > plain_table_size + x_table_size);
  return ret;
}

osl::PiecePair::index_t osl::
PiecePair::index(int offset_id, Piece p, Piece q)
{
  assert(isPiece(p));
  assert(isPiece(q));
  assert(p != q);
  assert(isOnBoard(p) && isOnBoard(q));
  return index(offset_id, square(p), osl::ptypeO(p), square(q), osl::ptypeO(q));
}

int osl::
PiecePair::eval(const NumEffectState& state, const Weights& values)
{
  int ret = 0;
  for (int i=0; i<Piece_SIZE; i++) {
    const Piece p = state.pieceOf(i);
    ret += pieceValueDouble(state, p, values);
  }
  return ret/2;
}

int osl::
PiecePair::evalWithUpdate(const NumEffectState& state, Move moved, int last_value, const Weights& values)
{
  assert(!isPass(moved));
  
  int ret = last_value;
  const Square from = osl::from(moved);
  const Square to = osl::to(moved);  

  // adjust from
  if (! isPieceStand(from)) {
    for (size_t i=0; i<offsets.size(); ++i) {
      const Square target = from + offsets[i];
      const Piece p = state.pieceAt(target);
      if (! isPiece(p) || square(p) == to)
	continue;
      assert(!isPieceStand(target));
      ret -= value(i, p, from, oldPtypeO(moved), values);      
    }  
  }
  
  // adjust to
  if (! isCapture(moved)) 
  {  
    for (size_t i=0; i<offsets.size(); ++i) {
      const Square target = to + offsets[i];
      const Piece p = state.pieceAt(target);
      if (! isPiece(p))
	continue;
      assert(!isPieceStand(target));
      ret += value(i, p, to, osl::ptypeO(moved), values);      
    }  
    return ret;
  }

  // adjust with capture
  for (size_t i=0; i<offsets.size(); ++i) {
    const Square target = to + offsets[i];
    const Piece p = state.pieceAt(target);
    if (! isPiece(p))
      continue;
    assert(!isPieceStand(target));
    ret += value(i, p, to, osl::ptypeO(moved), values);      
    if (square(p) == to)
      continue;
    ret -= value(i, p, to, capturePtypeO(moved), values);      
  }
  const Offset diff = to - from;
  int capture_i = offset_index[I(diff)];
  if (capture_i >= 0)
    ret -= value(capture_i, to, capturePtypeO(moved), from, oldPtypeO(moved), values);

  return ret;
}

int osl::
PiecePair::valueCompiled(int offset_id, Square pos0, PtypeO p0, Square 
#ifndef NDEBUG
pos1
#endif
, PtypeO p1)
{
  assert(pos0 != pos1);
  assert(! isPieceStand(pos0) && ! isPieceStand(pos1));
  assert(pos0 - pos1 == offsets[offset_id]);

  return x_values[X(pos0)][I(p0)][offset_id][I(p1)]
    + y_values[Y(pos0)][I(p0)][offset_id][I(p1)];
}

template <int Direction, int Offset>
inline int osl::
PiecePair::sum12One(const Piece *base_ptr,const int *xbase,const int *ybase)
{
  const Piece p = *(base_ptr-Offset);
  PtypeO p1=osl::ptypeO(p);
  return 
    *(xbase+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*Direction+int(p1))
    + *(ybase+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*Direction+int(p1));
}
inline int osl::
PiecePair::sum12(NumEffectState const& state,Square base,PtypeO ptypeO)
{
  const int *xbase= &x_values[X(base)][I(ptypeO)][0][I((PtypeO)0)];
  const int *ybase= &y_values[Y(base)][I(ptypeO)][0][I((PtypeO)0)];
  const Piece* base_ptr= state.getPiecePtr(base);
  return 
    sum12One<4,18>(base_ptr,xbase,ybase)+
    + sum12One<3,17>(base_ptr,xbase,ybase)
    + sum12One<2,16>(base_ptr,xbase,ybase)
    + sum12One<1,15>(base_ptr,xbase,ybase)
    + sum12One<0,14>(base_ptr,xbase,ybase)
    + sum12One<5,1>(base_ptr,xbase,ybase)
    + sum12One<11,-1>(base_ptr,xbase,ybase)
    + sum12One<6,-14>(base_ptr,xbase,ybase)
    + sum12One<7,-15>(base_ptr,xbase,ybase)
    + sum12One<8,-16>(base_ptr,xbase,ybase)
    + sum12One<9,-17>(base_ptr,xbase,ybase)
    + sum12One<10,-18>(base_ptr,xbase,ybase);
}

template<int Direction, int Offset>
inline int osl::
PiecePair::adjust12One(const Piece *base_ptr,const int *xbase1,const int *ybase1,const int *xbase2,const int *ybase2)
{
  const Piece p = *(base_ptr-Offset);
  PtypeO p1=osl::ptypeO(p);
  return 
    *(xbase1+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*Direction+int(p1))
    + *(ybase1+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*Direction+int(p1))
    - *(xbase2+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*Direction+int(p1))
    - *(ybase2+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*Direction+int(p1));
}

inline int osl::
PiecePair::adjust12(NumEffectState const& state,Square base,PtypeO pos,PtypeO neg)
{
  const int *xbase1= &x_values[X(base)][I(pos)][0][I((PtypeO)0)];
  const int *xbase2= &x_values[X(base)][I(neg)][0][I((PtypeO)0)];
  const int *ybase1= &y_values[Y(base)][I(pos)][0][I((PtypeO)0)];
  const int *ybase2= &y_values[Y(base)][I(neg)][0][I((PtypeO)0)];
  const Piece* base_ptr= state.getPiecePtr(base);
  return
    adjust12One<4,18>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<3,17>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<2,16>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<1,15>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<0,14>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<5,1>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<11,-1>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<6,-14>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<7,-15>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<8,-16>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<9,-17>(base_ptr,xbase1,ybase1,xbase2,ybase2)
    + adjust12One<10,-18>(base_ptr,xbase1,ybase1,xbase2,ybase2);
}

int osl::
PiecePair::evalWithUpdateCompiled(const NumEffectState& state, Move moved, int last_value)
{
  int ret = last_value;
  const Square from = osl::from(moved);
  const Square to = osl::to(moved);  

  // adjust from
  if (isPieceStand(from)) {
    ret+=sum12(state,to,osl::ptypeO(moved));
    return ret;
  }
  else{
    ret-=sum12(state,from,oldPtypeO(moved));
    // adjust to
    if (! isCapture(moved)) {
      ret+=sum12(state,to,osl::ptypeO(moved));
      const Offset diff = to-from;
      int capture_i = offset_index[I(diff)];
      if (capture_i >= 0){
	PtypeO ptypeO=osl::ptypeO(moved);
	const int *xbase= &x_values[X(to)][I(ptypeO)][0][I((PtypeO)0)];
	const int *ybase= &y_values[Y(to)][I(ptypeO)][0][I((PtypeO)0)];
	PtypeO p1=oldPtypeO(moved);
	ret+=
	  *(xbase+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*capture_i+int(p1))
	  + *(ybase+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*capture_i+int(p1));
      }
      return ret;
    }
    else{
      // adjust with capture
      ret+=adjust12(state,to,osl::ptypeO(moved),capturePtypeO(moved));
      const Offset diff = to-from;
      int capture_i = offset_index[I(diff)];
      if (capture_i >= 0){
	Square base=to;
	PtypeO ptypeO1=osl::ptypeO(moved);
	PtypeO ptypeO2=capturePtypeO(moved);
	const int *xbase1= &x_values[X(base)][I(ptypeO1)][0][I((PtypeO)0)];
	const int *xbase2= &x_values[X(base)][I(ptypeO2)][0][I((PtypeO)0)];
	const int *ybase1= &y_values[Y(base)][I(ptypeO1)][0][I((PtypeO)0)];
	const int *ybase2= &y_values[Y(base)][I(ptypeO2)][0][I((PtypeO)0)];
	PtypeO p1=oldPtypeO(moved);
	ret+=
	  *(xbase1+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*capture_i+int(p1))
	  + *(ybase1+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*capture_i+int(p1))
	  - *(xbase2+(&x_values[0][0][1][0]-&x_values[0][0][0][0])*capture_i+int(p1))
	  - *(ybase2+(&y_values[0][0][1][0]-&y_values[0][0][0][0])*capture_i+int(p1));
      }
      return ret;
    }
  }
}

int osl::
PiecePair::pieceValueDouble(const NumEffectState& state, Piece p, const Weights& values)
{
  if (! isOnBoard(p))
    return 0;
  int ret = 0;
  for (size_t i=0; i<offsets.size(); ++i) {
    const Square target = square(p) + offsets[i];
    const Piece q = state.pieceAt(target);
    if (! isPiece(q)|| p == q)
      continue;
    assert(!isPieceStand(target)); 
    assert(isOnBoard(p) && isOnBoard(q));
    int v = value(i, q, p, values);
    ret += v;
  }
  return ret;
}

int osl::
PiecePair::pieceValue(const NumEffectState& state, Piece p, const Weights& values)
{
  return pieceValueDouble(state, p, values)/2;
}
/* piecePairKing.cc
 */
std::array<int16_t, 1488375> osl::PiecePairKing::table;

void osl::
PiecePairKing::setUp(const Weights &weights)
{
  for (size_t i=0; i<weights.dimension(); ++i)
    table[i] = weights.value(i);

  for (int x=1; x<=5; ++x)
  {
    for (int y=1; y<=3; ++y)
    {
      bool flipx;
      const int king = indexKing(WHITE, newSquare(x,y), flipx);
      for (int i=0; i<45*7; ++i)
	for (int j=i+1; j<45*7; ++j)
	  table[composeIndex(king, j, i)] = table[composeIndex(king, i, j)];
    }
  }
}

std::array<int,2> osl::
PiecePairKing::eval(const NumEffectState& state)
{
  std::array<int,2> ret;
  ret[I(BLACK)] = evalOne<BLACK>(state);
  ret[I(WHITE)] = evalOne<WHITE>(state);
  return ret;
}

template <osl::Player King>
int osl::
PiecePairKing::evalOne(const NumEffectState& state)
{
  FixedCapacityVector<Piece,38> pieces;
  if (Y(forBlack(King,state.kingSquare(King))) < 7)
    return 0;

  PieceMask bitset = state.piecesOnBoard(King) & ~pieceMask(KING) & ~state.promotedPieces();
  while (any(bitset)) 
  {
    const Piece p = state.pieceOf(takeOneBit(bitset));
    if (Y(forBlack(King,square(p))) >= 5){
      pieces.push_back(p);
    }
  }
  int sum = 0;
  bool flipx;
  const int index_king = indexKing(King, state.kingSquare(King), flipx);
  if (flipx)
  {
    for (size_t i=0; i<pieces.size(); ++i)
    {
      const int i0 = indexPiece<true>(King, square(pieces[i]), osl::ptype(pieces[i]));
      for (size_t j=i+1; j<pieces.size(); ++j)
      {
	const int i1 = indexPiece<true>(King, square(pieces[j]), osl::ptype(pieces[j]));
	const int index = composeIndex(index_king, i0, i1);
	sum += table[index];
      }
    }
  }
  else
  {
    for (size_t i=0; i<pieces.size(); ++i)
    {
      const int i0 = indexPiece<false>(King, square(pieces[i]), osl::ptype(pieces[i]));
      for (size_t j=i+1; j<pieces.size(); ++j)
      {
	const int i1 = indexPiece<false>(King, square(pieces[j]), osl::ptype(pieces[j]));
	const int index = composeIndex(index_king, i0, i1);
	sum += table[index];
      }
    }
  }
  return (King == BLACK) ? sum : -sum;
}

template <osl::Player King>
int osl::
PiecePairKing::add(const NumEffectState& state, Square to, Ptype ptype)
{
  const Square king = state.kingSquare(King);
  bool flipx;
  const int index_king = indexKing(King, king, flipx);
  int sum = 0;
  PieceMask bitset = state.piecesOnBoard(King) & ~state.promotedPieces() & ~pieceMask(KING);
  unsigned int i0;
  if (flipx)
  {
    i0 = indexPiece<true>(King, to, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<true>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum += table[index];
    }
  }
  else
  {
    i0 = indexPiece<false>(King, to, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<false>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum += table[index];
    }
  }
  sum -= table[composeIndex(index_king, i0, i0)];
  return (King == BLACK) ? sum : -sum;
}
template <osl::Player King>
int osl::
PiecePairKing::sub(const NumEffectState& state, Square from, Ptype ptype)
{
  const Square king = state.kingSquare(King);
  bool flipx;
  const int index_king = indexKing(King, king, flipx);
  int sum = 0;
  PieceMask bitset = state.piecesOnBoard(King) & ~state.promotedPieces() & ~pieceMask(KING);
  if (flipx)
  {
    const int i0 = indexPiece<true>(King, from, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<true>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum -= table[index];
    }
  }
  else
  {
    const int i0 = indexPiece<false>(King, from, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<false>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum -= table[index];
    }
  }
  return (King == BLACK) ? sum : -sum;
}
template <osl::Player King>
int osl::
PiecePairKing::addSub(const NumEffectState& state, Square to, Ptype ptype, Square from)
{
  const Square king = state.kingSquare(King);
  bool flipx;
  const int index_king = indexKing(King, king, flipx);
  unsigned int i0, s0;
  int sum = 0;
  PieceMask bitset = state.piecesOnBoard(King) & ~state.promotedPieces() & ~pieceMask(KING);
  FixedCapacityVector<Piece,38> pieces;
  if (flipx)
  {
    i0 = indexPiece<true>(King, to, ptype);
    s0 = indexPiece<true>(King, from, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<true>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum += table[index];
      const int sub_index = composeIndex(index_king, s0, i1);
      sum -= table[sub_index];
    }
  }
  else
  {
    i0 = indexPiece<false>(King, to, ptype);
    s0 = indexPiece<false>(King, from, ptype);
    while (any(bitset)) 
    {
      const Piece p = state.pieceOf(takeOneBit(bitset));
      if (Y(forBlack(King,square(p))) < 5)
	continue;
      const int i1 = indexPiece<false>(King, square(p), osl::ptype(p));
      const int index = composeIndex(index_king, i0, i1);
      sum += table[index];
      const int sub_index = composeIndex(index_king, s0, i1);
      sum -= table[sub_index];
    }
  }
  sum -= table[composeIndex(index_king, i0, i0)];
  sum += table[composeIndex(index_king, s0, i0)];
  return (King == BLACK) ? sum : -sum;
}

template <osl::Player P>
void osl::
PiecePairKing::evalWithUpdateBang(const NumEffectState& state, Move moved, std::array<int,2>& last_value)
{
  assert(P == player(moved));
  assert(!isPass(moved));
  constexpr Player Opponent = alt(P);
  const Ptype captured = capturePtype(moved);
  bool adjust_capture = (captured != Ptype::EMPTY)
    && ! isPromoted(captured)
    && Y(forBlack(alt(P),to(moved))) >= 5;
  if (adjust_capture)
  {
    const Square roking = forBlack(alt(P),state.kingSquare(alt(P)));
    adjust_capture = Y(roking) >= 7;
  }
  if (osl::ptype(moved) == KING)
  {
    last_value[I(P)] = evalOne<P>(state);
    if (adjust_capture)
      last_value[I(alt(P))] += sub<Opponent>(state, to(moved), captured);
    return;
  }
  const Square rking = forBlack(P,state.kingSquare(P));
  if (Y(rking) < 7) 
  {
    if (adjust_capture)
      last_value[I(alt(P))] += sub<Opponent>(state, to(moved), captured);
    return;
  }
  const Square rto = forBlack(P,to(moved));
  if (isDrop(moved))
  {
    if (Y(rto) >= 5)
      last_value[I(P)] += add<P>(state, to(moved), osl::ptype(moved));
    return;
  }
  const Square rfrom = forBlack(P,from(moved));
  if (adjust_capture)
    last_value[I(alt(P))] += sub<Opponent>(state, to(moved), captured);

  if (isPromoted(oldPtype(moved)))
    return;
  if (Y(rfrom) < 5)
  {
    if (Y(rto) >= 5 && ! isPromoted(osl::ptype(moved)))
      last_value[I(P)] += add<P>(state, to(moved), osl::ptype(moved));
    return;
  }
  if (Y(rto) < 5 || isPromoted(osl::ptype(moved)))
    last_value[I(P)] += sub<P>(state, from(moved), oldPtype(moved));
  else
    last_value[I(P)] += addSub<P>(state, to(moved), osl::ptype(moved), from(moved));
}

namespace osl
{
  template void PiecePairKing::evalWithUpdateBang<BLACK>(const NumEffectState&, Move, std::array<int,2>&);
  template void PiecePairKing::evalWithUpdateBang<WHITE>(const NumEffectState&, Move, std::array<int,2>&);
}


std::array<MultiInt, 18>
osl::RookMobilityAll::rook_vertical_table;
std::array<MultiInt, 18>
osl::RookMobilityAll::rook_horizontal_table;
std::array<MultiInt, 34>
osl::RookMobilityAll::sum_table;
std::array<MultiInt, 324>
osl::RookMobilityAll::x_table;
std::array<MultiInt, 324>
osl::RookMobilityAll::y_table;
std::array<MultiInt, 17*9>
osl::RookMobilityAll::sumkingx_table;
std::array<MultiInt, 9*2*5*9>
osl::RookMobilityAll::xkingx_table;

std::array<MultiInt, 36>
osl::BishopMobilityAll::bishop_table;
std::array<MultiInt, 18>
osl::BishopMobilityAll::each_table;

std::array<MultiInt, 9>
osl::LanceMobility::lance_table;

void osl::
RookMobility::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < 9; ++i)
  {
    RookMobilityAll::rook_vertical_table[i][stage] = weights.value(i);
  }
  for (size_t i = 0; i < 9; ++i)
  {
    RookMobilityAll::rook_horizontal_table[i][stage] = weights.value(i + 9);
  }
  for (size_t i = 0; i < 9; ++i)
  {
    RookMobilityAll::rook_vertical_table[i+9][stage] = weights.value(i + 18);
  }
  for (size_t i = 0; i < 9; ++i)
  {
    RookMobilityAll::rook_horizontal_table[i+9][stage] = weights.value(i + 27);
  }
}

void osl::
RookMobilitySum::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookMobilityAll::sum_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
RookMobilityX::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookMobilityAll::x_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
RookMobilityY::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookMobilityAll::y_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

void osl::
RookMobilityXKingX::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookMobilityAll::xkingx_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}
void osl::
RookMobilitySumKingX::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      RookMobilityAll::sumkingx_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

//template <int Sign> inline
void osl::
RookMobilityAll::adjust(Player pl, const NumEffectState& state,
			bool promoted, int vertical, int horizontal,
			Square position, MultiInt& value)
{
  int offset=0;
  if(promoted) offset=9;
  if (! promoted) {
    const Square king = state.kingSquare(pl);
    value.addFor(pl, xkingx_table[indexXKingX(pl, position, king, vertical, true)]
		 + xkingx_table[indexXKingX(pl, position, king, horizontal, false)]
		 + sumkingx_table[vertical + horizontal + 17*std::abs(X(king)-X(position))]);
  }
  value.addFor(pl, rook_vertical_table[vertical+offset]+
	       rook_horizontal_table[horizontal+offset] +
	       sum_table[vertical+horizontal+(promoted ? 17 : 0)] +
	       x_table[indexX(position, promoted, vertical, true)] +
	       x_table[indexX(position, promoted, horizontal, false)] +
	       y_table[indexY(pl, position, promoted, vertical, true)] +
	       y_table[indexY(pl, position, promoted, horizontal, false)]);
}

void osl::
RookMobilityAll::eval(const NumEffectState& state, MultiInt& out)
{
  out.clear();
  for(Player pl : COLORS){
    for(int i : state.allNum(pl, ROOK)){
      Piece rook = state.pieceOf(i);
      int vertical = state.rookMobilityVertical(pl, i);
      int horizontal = state.rookMobilityHorizontal(pl, i);
      adjust(pl, state, isPromoted(rook), vertical, horizontal, square(rook), out);
    }
  }
}




void osl::
BishopMobility::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < 18; ++i)
  {
    BishopMobilityAll::bishop_table[i][stage] = weights.value(i);
  }
  for (size_t i = 0; i < 18; ++i)
  {
    BishopMobilityAll::bishop_table[i+18][stage] = weights.value(i + 18);
  }
}

void osl::
BishopMobilityEach::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      BishopMobilityAll::each_table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

// template <int Sign> 
inline
void osl::
BishopMobilityAll::adjust(Player pl, bool promoted, int mobility1, int mobility2,
			  MultiInt& value)
{
  int count=0;
  int each_offset = 0;
  if(promoted)
  {
    count=18;
    each_offset = 9;
  }
  if(pl == BLACK)
  {
    value += (bishop_table[mobility1 + mobility2 + count] +
	      each_table[mobility1 + each_offset] +
	      each_table[mobility2 + each_offset]);
  }
  else
  {
    value -= (bishop_table[mobility1 + mobility2 + count] +
	      each_table[mobility1 + each_offset] +
	      each_table[mobility2 + each_offset]);
  }
}

void osl::
BishopMobilityAll::eval(const NumEffectState& state, MultiInt& out)
{
  out.clear();
  for(Player pl : COLORS){
    for(int i : state.allNum(pl, BISHOP)){
      Piece bishop = state.pieceOf(i);
      const int mobility1 = state.bishopMobilityULDR(pl, i);
      const int mobility2 = state.bishopMobilityURDL(pl, i);
      adjust(pl, isPromoted(bishop), mobility1, mobility2, out);
    }
  }
}



void osl::
LanceMobility::setUp(const Weights &weights,int stage)
{
  for (size_t i = 0; i < 9; ++i)
  {
    lance_table[i][stage] = weights.value(i);
  }
}

// template <int Sign> 
inline
void osl::
LanceMobilityAll::adjust(Player P, int mobility, MultiInt& value)
{
  if(P == BLACK)
    value += LanceMobility::lance_table[mobility];
  else
    value -= LanceMobility::lance_table[mobility];
}

void osl::
LanceMobilityAll::eval(const NumEffectState &state, MultiInt& out)
{
  out.clear();
  for(Player pl : COLORS){
    for(int num : state.allNumStrict(pl, LANCE)){
      Piece lance = state.pieceOf(num);
      int mobility = state.lanceMobility(pl, square(lance), num);
      adjust(pl, mobility, out);
    }
  }
}


std::array<MultiInt, 80>
osl::CheckShadowPtype::table;

void osl::
CheckShadowPtype::setUp(const Weights &weights)
{
  for (size_t i = 0; i < ONE_DIM; ++i)
  {
    for (int s=0; s<NStages; ++s)
      table[i][s] = weights.value(i + ONE_DIM*s);
  }
}

template <osl::Player Defense>
osl::MultiInt osl::
CheckShadowPtype::evalOne(const NumEffectState &state) 
{
  MultiInt result;
  const Square king = state.kingSquare(Defense);
  PieceMask open_mask = state.checkShadow(alt(Defense));
  while (any(open_mask))
  {
    const Piece piece = state.pieceOf(takeOneBit(open_mask));
    if (Y(king) == Y(square(piece))) // rook h
    {
      result += table[I(osl::ptype(piece) + PTYPE_SIZE * 1)];
    }
    else if (X(king) == X(square(piece)))
    {
      if (state.hasEffectStrict(alt(Defense),
				square(piece), LANCE)) // lance
      {
	result += table[I(osl::ptype(piece) + PTYPE_SIZE * 4)];
      }
      else // rook v
      {
	result += table[I(osl::ptype(piece) + PTYPE_SIZE * 0)];
      }
    }
    else // bishop
    {
      if ((Defense == BLACK && Y(square(piece)) < Y(king)) ||
	  (Defense == WHITE && Y(square(piece)) > Y(king))) // u
      {
	result += table[I(osl::ptype(piece) + PTYPE_SIZE * 2)];
      }
      else
      {
	result += table[I(osl::ptype(piece) + PTYPE_SIZE * 3)];
      }
    }
  }
  return result;
}

osl::MultiInt osl::
CheckShadowPtype::eval(const NumEffectState &state)
{
  return evalOne<BLACK>(state) - evalOne<WHITE>(state);
}

volatile osl::OpenMidEndingEval::LoadStatus
osl::OpenMidEndingEval::initialized_flag = osl::OpenMidEndingEval::Zero;
osl::Weights
osl::OpenMidEndingEval::piece_pair_weights;
namespace
{
  template <class Eval, class Reader>
  static int setUpOneWithDim(Reader& p, int dim)
  {
    osl::Weights weights(dim);
    // std::cerr << typeid(Eval).name() << " " << dim << "\n";
    for (size_t i = 0; i < weights.dimension(); ++i)
    {
      if (! p.hasNext())
	break;
      int val = p.read();
      weights.setValue(i, val);
    }
    Eval::setUp(weights);
    return weights.dimension();
  }
  template <class Eval, class Reader>
  static int setUpOne(Reader& p)
  {
    return setUpOneWithDim<Eval>(p, Eval::DIM);
  }
  template <class Eval, class Reader>
  static int setUpOne(Reader& p, int stage)
  {
    osl::Weights weights(Eval::DIM);
    // std::cerr << typeid(Eval).name() << " " << Eval::DIM << "\n";
    for (size_t i = 0; i < weights.dimension(); ++i)
    {
      if (!p.hasNext())
	break;
      int val = p.read();
      weights.setValue(i, val);
    }
    Eval::setUp(weights,stage);
    return weights.dimension();
  }
}

namespace osl
{
  struct IntArrayReader
  {
    size_t cur, length;
    const int *array;
    IntArrayReader(const int *a, size_t l) : cur(0), length(l), array(a)
    {
    }
    bool hasNext() const { return cur < length; }
    bool failed() const { return false; }
    int read() { return array[cur++]; }
  };
}

void osl::
OpenMidEndingEval::resetWeights(const int *w, size_t length)
{
  IntArrayReader reader(w, length);
  doResetWeights(reader);
}

#if 1
struct IntReader {
	std::ifstream& is;
	int nextVal;

	bool failed_flag;
	char buf[4096];
	int readlen;
	int pos;

	IntReader(std::ifstream& is_) :is(is_), failed_flag(false)
	{
		is.read(buf, sizeof(buf));
		readlen = is.gcount();
		pos = 0;
	}

	bool hasNext() {

		if (pos < readlen)
		{
			return true;
		}

		if (readlen == sizeof(buf))
		{
			is.read(buf, sizeof(buf));
			readlen = is.gcount();
			pos = 0;
		}

		return pos < readlen;
	}


	int read() {
		if (!hasNext()) {
			failed_flag = true; return 0;
		}

		int nextVal = buf[pos] | (int)buf[pos + 1] << 8;
		pos += 2;

		return nextVal;
	}

	bool failed() const {
		return failed_flag;
	}
};

#else
struct IntReader{
  std::ifstream& is;
  int nextVal;
  bool nextIsValid;
  bool failed_flag;
  IntReader(std::ifstream& is_) :is(is_),nextIsValid(false),failed_flag(false) {}
  bool hasNext(){
    if(nextIsValid) return true;
    return ( nextIsValid = bool(is >> nextVal) );
  }
  int read(){
    if(!hasNext()) {
      failed_flag=true; return 0;
    }
    nextIsValid=false;
    return nextVal;
  }
  bool failed() const{
    return failed_flag;
  }
};
#endif

bool osl::OpenMidEndingEval::setUp(const char *filename)
{
  static std::mutex initialize_mutex;
  std::lock_guard<std::mutex> lk(initialize_mutex);
  if (initialized_flag == Loaded)
    return true;
  typedef IntReader reader_t;
  std::ifstream is(filename, std::ios_base::binary);
  reader_t reader(is);
  if (! reader.hasNext()) {
    initialized_flag = Zero;
    std::cerr << "file " << filename << std::endl;
    return false;
  }
  doResetWeights(reader);
  return initialized_flag == Loaded;
}

template <class Reader>
void osl::
OpenMidEndingEval::doResetWeights(Reader& reader)
{
  size_t read_count = 0;

  // flat
  std::array<int, PTYPE_SIZE> piece_values; piece_values.fill(0);
  Weights weights(PTYPE_SIZE);
  for (Ptype i = Ptype::MIN; i <= Ptype::MAX; ++i)
  {
    if (! reader.hasNext())
      break;
    int val = reader.read();
    if (i == KING) {
      assert(val == 0);
      val = osl::value(KING);
    }
    weights.setValue(I(i), val);
    piece_values[I(i)] = val;
    ++read_count;
  }
  PieceEval::setUp(weights);
//  Piece_Value.reset(piece_values);

  PiecePair::init();
  piece_pair_weights.resetDimension(PiecePair::DIM);
  for (size_t i = 0; i < piece_pair_weights.dimension(); ++i)
  {
    if (! reader.hasNext())
      break;
    int val = reader.read();
    piece_pair_weights.setValue(i, val);
    ++read_count;
  }
  PiecePair::sanitize(piece_pair_weights);
  PiecePair::compile(piece_pair_weights);

  read_count += setUpOne<King25EffectAttack>(reader);
  read_count += setUpOne<King25EffectYAttack>(reader);
  read_count += setUpOne<PiecePairKing>(reader);
  read_count += setUpOne<BishopExchangeSilverKing>(reader);
  read_count += setUpOne<EnterKingDefense>(reader);

  for(int stage : STAGES){
    read_count += setUpOne<PieceStand>(reader,stage);
    read_count += setUpOne<King25EffectEachBoth>(reader, stage);
    read_count += setUpOne<PawnDrop>(reader, stage);
    read_count += setUpOne<NoPawnOnStand>(reader, stage);
    read_count += setUpOne<GoldRetreat>(reader, stage);
    read_count += setUpOne<SilverRetreat>(reader, stage);
    read_count += setUpOne<KnightAdvance>(reader, stage);
    read_count += setUpOne<AllMajor>(reader, stage);
    read_count += setUpOne<KingXBlockedBoth>(reader, stage);
    read_count += setUpOne<AllGold>(reader, stage);
    read_count += setUpOne<PtypeX>(reader, stage);
    read_count += setUpOne<PtypeY>(reader, stage);
    read_count += setUpOne<AnagumaEmpty>(reader, stage);
    read_count += setUpOne<NonPawnPieceStand>(reader, stage);
    read_count += setUpOne<King25EffectDefense>(reader, stage);
    read_count += setUpOne<King25EffectYDefense>(reader, stage);
    read_count += setUpOne<RookMobility>(reader, stage);
    read_count += setUpOne<BishopMobility>(reader, stage);
    read_count += setUpOne<LanceMobility>(reader, stage);
    read_count += setUpOne<RookEffect>(reader, stage);
    read_count += setUpOne<BishopEffect>(reader, stage);
    read_count += setUpOne<PawnAdvance>(reader, stage);
    read_count += setUpOne<PawnDropY>(reader, stage);
    read_count += setUpOne<KnightCheck>(reader, stage);
  }

  // triple
  read_count += setUpOne<KingPieceRelative>(reader,0);
  read_count += setUpOne<KingPieceRelative>(reader,1);
  read_count += setUpOne<KingPieceRelative>(reader,2);
  read_count += setUpOne<KingPieceRelative>(reader,EndgameIndex);
  read_count += setUpOne<NonPawnPieceStandTurn>(reader);
  read_count += setUpOne<King25EffectEachXY>(reader);
  read_count += setUpOne<RookPawnY>(reader);
  read_count += setUpOne<RookEffectPiece>(reader);
  read_count += setUpOne<BishopEffectPiece>(reader);
  read_count += setUpOne<PieceStandY>(reader);
  read_count += setUpOne<RookEffectPieceKingRelative>(reader);
  read_count += setUpOne<BishopEffectPieceKingRelative>(reader);
  read_count += setUpOne<RookPawnYX>(reader);
  read_count += setUpOne<PawnPtypeOPtypeO>(reader);
  read_count += setUpOne<PromotedMinorPieces>(reader);
  read_count += setUpOne<KingPieceRelativeNoSupport>(reader);
  read_count += setUpOne<NonPawnAttacked>(reader);
  read_count += setUpOne<PtypeYY>(reader);
  read_count += setUpOne<PawnPtypeOPtypeOY>(reader);
  read_count += setUpOne<PawnDropX>(reader);
  read_count += setUpOne<King3Pieces>(reader);
  read_count += setUpOne<King3PiecesXY>(reader);
  read_count += setUpOne<King25EffectEachKXY>(reader);
  read_count += setUpOne<BishopHead>(reader);
  read_count += setUpOne<BishopHeadKingRelative>(reader);
  read_count += setUpOne<KnightCheckY>(reader);
  read_count += setUpOne<KnightHead>(reader);
  read_count += setUpOne<RookPromoteDefense>(reader);
  read_count += setUpOne<PawnDropPawnStand>(reader);
  read_count += setUpOne<PawnDropPawnStandX>(reader);
  read_count += setUpOne<PawnDropPawnStandY>(reader);
  read_count += setUpOne<KnightHeadOppPiecePawnOnStand>(reader);
  read_count += setUpOne<KingXBothBlocked>(reader);
  read_count += setUpOne<KingXBothBlockedY>(reader);
  read_count += setUpOne<KingRookBishop>(reader);
  read_count += setUpOne<PromotedMinorPiecesY>(reader);
  read_count += setUpOne<King25EffectSupported>(reader);
  read_count += setUpOne<King25EffectSupportedY>(reader);
  read_count += setUpOne<NonPawnAttackedKingRelative>(reader);
  read_count += setUpOne<NonPawnAttackedPtype>(reader);
  read_count += setUpOne<PtypeCount>(reader);
  read_count += setUpOne<KingXBlocked3>(reader);
  read_count += setUpOne<KingXBlocked3Y>(reader);
  read_count += setUpOne<PtypeCountXY>(reader);
  read_count += setUpOne<PtypeCountXYAttack>(reader);
  read_count += setUpOne<LanceEffectPieceKingRelative>(reader);
  read_count += setUpOne<KingMobilityVal>(reader);
  read_count += setUpOne<KingMobilitySum>(reader);
  read_count += setUpOne<PtypeYPawnY>(reader);
  read_count += setUpOne<GoldAndSilverNearKing>(reader);
  read_count += setUpOne<PtypeCombination>(reader);
  read_count += setUpOne<PieceStandCombinationBoth>(reader);
  read_count += setUpOne<King25BothSide>(reader);
  read_count += setUpOne<King25BothSideX>(reader);
  read_count += setUpOne<King25BothSideY>(reader);
  read_count += setUpOne<GoldAndSilverNearKingCombination>(reader);
  read_count += setUpOne<KingMobilityWithRook>(reader);
  read_count += setUpOne<KingMobilityWithBishop>(reader);
  read_count += setUpOne<NumPiecesBetweenBishopAndKingSelf>(reader);
  read_count += setUpOne<NumPiecesBetweenBishopAndKingOpp>(reader);
  read_count += setUpOne<NumPiecesBetweenBishopAndKingAll>(reader);
  read_count += setUpOne<King25Effect3>(reader);
  read_count += setUpOne<SilverHeadPawnKingRelative>(reader);
  read_count += setUpOne<GoldKnightKingRelative>(reader);
  read_count += setUpOne<RookMobilitySum>(reader);
  read_count += setUpOne<RookMobilityX>(reader);
  read_count += setUpOne<RookMobilityY>(reader);
  read_count += setUpOne<RookMobilitySumKingX>(reader);
  read_count += setUpOne<RookMobilityXKingX>(reader);
  read_count += setUpOne<PinPtype>(reader);
  read_count += setUpOne<PinPtypeDistance>(reader);
  read_count += setUpOne<BishopMobilityEach>(reader);
  read_count += setUpOne<BishopBishopPiece>(reader);
  read_count += setUpOne<NonPawnPieceStandCombination>(reader);
  read_count += setUpOne<CanCheckNonPawnPieceStandCombination>(reader);
  read_count += setUpOne<King25Effect3Y>(reader);
  read_count += setUpOne<RookRook>(reader);
  read_count += setUpOne<RookRookPiece>(reader);
  read_count += setUpOne<PinPtypePawnAttack>(reader);
  read_count += setUpOne<King25Mobility>(reader);
  read_count += setUpOne<King25MobilityX>(reader);
  read_count += setUpOne<King25MobilityY>(reader);
  read_count += setUpOne<King25EffectCountCombination>(reader);
  read_count += setUpOne<GoldSideMove>(reader);
  read_count += setUpOne<King25EffectCountCombinationY>(reader);
  read_count += setUpOne<RookPromoteDefenseRookH>(reader);
  read_count += setUpOne<BishopHeadX>(reader);
  read_count += setUpOne<PawnDropNonDrop>(reader);
  read_count += setUpOne<PawnStateKingRelative>(reader);
  read_count += setUpOne<SilverFork>(reader);
  read_count += setUpOne<BishopRookFork>(reader);
  read_count += setUpOne<BishopStandFile5>(reader);
  read_count += setUpOne<KnightFork>(reader);
  read_count += setUpOne<NonPawnAttackedPtypePair>(reader);
  read_count += setUpOne<MajorCheckWithCapture>(reader);
  read_count += setUpOne<SilverAdvance26>(reader);
  read_count += setUpOne<RookSilverKnight>(reader);
  read_count += setUpOne<BishopSilverKnight>(reader);
  read_count += setUpOne<AttackMajorsInBase>(reader);
  read_count += setUpOne<CheckShadowPtype>(reader);
  read_count += setUpOne<Promotion37>(reader);

  PtypeCountAll::setUp();
  PawnDropAll::setUp();
  King8All::setUp();
  GoldFeatures::setUp();
  SilverFeatures::setUp();
  KingPiece::setUp();
  PinPtypeAll::setUp();
  initialized_flag = reader.failed() ? Zero : Loaded;
  if (initialized_flag != Loaded)
  {
    std::cerr << "Failed to load OpenMidEndingEval data "
	      << ' ' << read_count << std::endl;
  }
}

std::string osl::OpenMidEndingEval::defaultFilename()
{
#if 1
  return gpsfish_home() + "eval.bin";
#else
  return gpsfish_home() + "eval.txt";
#endif
}

bool osl::OpenMidEndingEval::setUp()
{
  return setUp(defaultFilename().c_str());  
}

osl::
OpenMidEndingEval::OpenMidEndingEval(const NumEffectState &state
)
  : progress(state)
{
  assert(initialized_flag != Zero);
  
  previous = 0;
  turn = state.turn();
  updateGoldSilverNearKing(state);

  for(auto& v : ptype_count) v.fill(0);
  ptypeo_mask=0u;
  for(auto& v : ptype_board_count) v.fill(0);
  for (int i = 0; i < Piece_SIZE; ++i)
  {
    const Piece piece = state.pieceOf(i);
    if (osl::ptype(piece) == KING)
      continue;
    ++ptype_count[I(owner(piece))][I(osl::ptype(piece))];
    ptypeo_mask |= 1<<I(osl::ptypeO(piece));
    if (isOnBoard(piece))
      ++ptype_board_count[I(owner(piece))][I(osl::ptype(piece))];
  }
  non_pawn_stand_count.fill(0);
  for(Ptype ptype : osl::PieceStandOrder)
  {
    if (ptype == PAWN)
      continue;
    non_pawn_stand_count[I(BLACK)] +=
      state.countPiecesOnStand(osl::BLACK, ptype);
    non_pawn_stand_count[I(WHITE)] += 
      state.countPiecesOnStand(osl::WHITE, ptype);
  }
  updatable_value.clear();
  piece_stand_value = PieceStand::eval(state);
  piece_pair_value = PiecePair::eval(state, piece_pair_weights);
  piece_pair_king_value = PiecePairKing::eval(state);
  RookMobilityAll::eval(state, rook_mobility);
  BishopMobilityAll::eval(state, bishop_mobility);
  LanceMobilityAll::eval(state, lance_mobility);
  knight_advance = KnightAdvance::eval(state);

  rook_effect = RookEffectBase::eval(state);
  bishop_effect = BishopEffectBase::eval(state);

  King25EffectEachBoth::eval(state, king25_effect_each);

  int p_black_attack_effect = std::min(127, progress.rawData().attackCount[I(WHITE)]);
  int p_white_attack_effect = std::min(127, progress.rawData().attackCount[I(BLACK)]);
  int p_black_attack_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & state.piecesOnBoard(BLACK)));
  int p_white_attack_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & state.piecesOnBoard(WHITE)));
  int p_black_attack_supported_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & state.piecesOnBoard(BLACK) & state.effectedMask(BLACK)));
  int p_white_attack_supported_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & state.piecesOnBoard(WHITE) & state.effectedMask(WHITE)));
  int p_black_defense_effect = std::min(127, progress.rawData().defenseCount[I(BLACK)]);
  int p_white_defense_effect = std::min(127, progress.rawData().defenseCount[I(WHITE)]);
  int p_black_defense_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & state.piecesOnBoard(BLACK)));
  int p_white_defense_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & state.piecesOnBoard(WHITE)));

  recalculated_value =
    BishopExchangeSilverKing::eval(state) + 
    EnterKingDefense::eval(state);
#ifdef BISHOP_IN_DANGER
  recalculated_value += BishopInDanger::eval(state);
  recalculated_value += RookInDanger::eval(state);
#endif
  recalculated_value +=
    King25EffectYAttack::eval(state,
			      p_black_attack_effect,
			      p_black_attack_piece,
			      p_white_attack_effect, p_white_attack_piece);
  kingx_blocked = King8All::eval(state);
  {
    MultiInt result_supported_y =
      King25EffectSupportedY::eval(p_black_attack_piece,
				   p_white_attack_piece,
				   p_black_attack_supported_piece,
				   p_white_attack_supported_piece,
				   Y(state.kingSquare(BLACK)),
				   Y(state.kingSquare(WHITE)));

    recalculated_stage_value = result_supported_y;
    king_rook_bishop[I(BLACK)]=KingRookBishop::evalOne<BLACK>(state);
    king_rook_bishop[I(WHITE)]=KingRookBishop::evalOne<WHITE>(state);
    recalculated_stage_value+=king_rook_bishop[I(BLACK)]-king_rook_bishop[I(WHITE)];
//    recalculated_stage_value+=KingXBlocked3::eval(state);
  }

 
  const MultiInt silver_retreat = SilverFeatures::eval(state);
  const MultiInt gold_retreat = GoldFeatures::eval(state);
  recalculated_stage_value += knight_advance;
  recalculated_stage_value += silver_retreat + gold_retreat;
  recalculated_stage_value += 
    King25EffectYDefense::eval(state,
			       p_black_defense_effect,
			       p_black_defense_piece,
			       p_white_defense_effect, p_white_defense_piece);
  recalculated_stage_value += kingx_blocked[I(BLACK)] + kingx_blocked[I(WHITE)];

  recalculated_stage_value += NoPawnOnStand::eval(state, ptype_count[I(BLACK)][I(PAWN)] + ptype_count[I(BLACK)][I(PPAWN)]);
  recalculated_stage_value += NonPawnPieceStand::eval(non_pawn_stand_count[I(BLACK)], non_pawn_stand_count[I(WHITE)]);
  recalculated_stage_value += PinPtypeAll::eval(state);
  recalculated_stage_value += KingMobilityVal::eval(state) + KingMobilitySum::eval(state);
  recalculated_stage_value += GoldAndSilverNearKing::eval(state,
							  gs_near_king_count);
  recalculated_stage_value += PtypeCombination::eval(ptypeo_mask);
  recalculated_stage_value += PieceStandCombinationBoth::eval(state);
  king25_both_side[I(BLACK)]=King25BothSide::evalOne<BLACK>(state,progress.rawData().vertical_x[I(BLACK)]);
  king25_both_side[I(WHITE)]=King25BothSide::evalOne<WHITE>(state,progress.rawData().vertical_x[I(WHITE)]);
  recalculated_stage_value += king25_both_side[I(BLACK)]-king25_both_side[I(WHITE)];
  recalculated_stage_value += King25Mobility::eval(state,
    progress.rawData().king_vertical_x[I(BLACK)],
    progress.rawData().king_vertical_x[I(WHITE)]);
  recalculated_stage_value += BishopStandFile5::eval(state);
  recalculated_stage_value += MajorCheckWithCapture::eval(state);
  recalculated_stage_value += SilverAdvance26::eval(state);

  pawn_drop = PawnDropAll::eval(state);

  ptypex = PtypeX::eval(state);

  ptypey = PtypeY::eval(state);

  can_check[I(BLACK)] =
    CanCheckNonPawnPieceStandCombination::canCheck<BLACK>(state);
  can_check[I(WHITE)] =
    CanCheckNonPawnPieceStandCombination::canCheck<WHITE>(state);
  piece_stand_combination = NonPawnPieceStandCombination::eval(state,
							       can_check);
  NonPawnPieceStandTurn::eval(state, piece_stand_turn);
  rook_pawn = RookPawnY::eval(state
    );
  piece_stand_y = PieceStandY::eval(state);

  pawn_advance = PawnAdvance::eval(state);
  knight_check = KnightCheck::eval(state);
  pawn_ptypeo = PawnPtypeOPtypeO::eval(state);

  promoted_minor_piece = PromotedMinorPieces::eval(state);

  effected_mask[I(BLACK)] =
    effected_mask_for_attacked[I(BLACK)] =
    state.effectedMask(BLACK);
  effected_mask[I(WHITE)] =
    effected_mask_for_attacked[I(WHITE)] =
    state.effectedMask(WHITE);
  effected_mask_for_attacked[I(BLACK)] &= ~selectPtype(~state.promotedPieces(), PAWN);
  effected_mask_for_attacked[I(WHITE)] &= ~selectPtype(~state.promotedPieces(), PAWN);
  king_piece = KingPiece::eval(state);
  NonPawnAttacked::eval(state, non_pawn_attacked);
  NonPawnAttackedPtype::eval(state, non_pawn_attacked_ptype);
  knight_head = KnightHead::eval(state);

  king3pieces = King3Pieces::eval(state);
  bishop_head = BishopHead::eval(state);
  rook_promote_defense = RookPromoteDefense::eval(state);
  PtypeCountAll::eval(state, ptype_count, ptype_board_count, ptype_count_value, updatable_value);
  lance_effect_piece = LanceEffectPieceKingRelative::eval(state);
  ptype_y_pawn_y = PtypeYPawnY::eval(state
    );
  bishop_and_king = NumPiecesBetweenBishopAndKing::eval(state);
  recalculated_stage_value += King25Effect3::eval(state, progress.rawData().mask5x5);
  recalculated_stage_value += BishopBishopPiece::eval(state);
  recalculated_stage_value += RookRook::eval(state);
  recalculated_stage_value += RookRookPiece::eval(state);
  recalculated_stage_value += King25EffectCountCombination::eval(state, progress.rawData().mask5x5);
  recalculated_stage_value += progress.rawData().non_pawn_ptype_attacked_pair_eval[I(BLACK)]
    + progress.rawData().non_pawn_ptype_attacked_pair_eval[I(WHITE)];
  rook_silver_knight = RookSilverKnight::eval(state);
  bishop_silver_knight = BishopSilverKnight::eval(state);
  recalculated_stage_value += AttackMajorsInBase::eval(state);
  recalculated_stage_value += CheckShadowPtype::eval(state);
  recalculated_stage_value += progress.rawData().promotion37_eval;
  piece_fork_turn = SilverFork::eval(state);
  piece_fork_turn += BishopRookFork::eval(state);
  piece_fork_turn += KnightFork::eval(state, knight_fork_squares);
  invalidateCache();
}

void osl::
OpenMidEndingEval::update(const NumEffectState &new_state, Move last_move)
{
  turn = alt(turn);
  assert(new_state.turn() == turn);
  if (isPass(last_move))
  {
    invalidateCache();
    return;
  }
  if(player(last_move)==BLACK)
    updateSub<BLACK>(new_state,last_move);
  else
    updateSub<WHITE>(new_state,last_move);
}
template<osl::Player P>
void osl::
OpenMidEndingEval::updateSub(const NumEffectState &new_state, Move last_move)
{
  assert(player(last_move)==P);
  const Square opp_king =
    new_state.kingSquare(alt(P));
  const Square self_king =
    new_state.kingSquare(P);
  Ptype captured = capturePtype(last_move);
  if (captured != Ptype::EMPTY)
  {
    Ptype base = unpromote(captured);
    if (base == PAWN)
    {
    }
    else
    {
      ++non_pawn_stand_count[I(P)];
    }
    if (base == GOLD || base == SILVER)
    {
      const int y_diff = std::abs(Y(to(last_move)) - Y(opp_king));
      const int x_diff = std::abs(X(to(last_move)) - X(opp_king));
      if (y_diff <= 2 && x_diff <= 3)
      {
	--gs_near_king_count[I(alt(P))][std::max(x_diff, y_diff) - 1];
      }
    }
  }
  const Ptype base_ptype = unpromote(osl::ptype(last_move));
  {
    if (base_ptype == GOLD || base_ptype == SILVER)
    {
      if (!isDrop(last_move))
      {
	const int y_diff = std::abs(Y(from(last_move)) - Y(self_king));
	const int x_diff = std::abs(X(from(last_move)) - X(self_king));
	if (y_diff <= 2 && x_diff <= 3)
	{
	  --gs_near_king_count[I(P)][std::max(x_diff, y_diff) - 1];
	}
      }
      {
	const int y_diff = std::abs(Y(to(last_move)) - Y(self_king));
	const int x_diff = std::abs(X(to(last_move)) - X(self_king));
	if (y_diff <= 2 && x_diff <= 3)
	{
	  ++gs_near_king_count[I(P)][std::max(x_diff, y_diff) - 1];
	}
      }
    }
    if (base_ptype == KING)
    {
      updateGoldSilverNearKing(new_state);
    }
  }
  if (isDrop(last_move) && osl::ptype(last_move) != PAWN)
  {
    --non_pawn_stand_count[I(P)];
  }
  progress.update<P>(new_state, last_move, previous->progress);
  king25_both_side[I(BLACK)]=King25BothSide::evalOne<BLACK>(new_state,progress.rawData().vertical_x[I(BLACK)]);
  king25_both_side[I(WHITE)]=King25BothSide::evalOne<WHITE>(new_state,progress.rawData().vertical_x[I(WHITE)]);

  piece_stand_value =
    PieceStand::evalWithUpdate<P>(new_state, last_move,
				  piece_stand_value);
  if (new_state.longEffectChanged(ROOK) || osl::ptype(last_move) == KING)
  {
    RookMobilityAll::eval(new_state, rook_mobility);
    rook_effect = RookEffectBase::eval(new_state);
  }
  if (new_state.longEffectChanged(BISHOP))
  {
    BishopMobilityAll::eval(new_state, bishop_mobility);
    bishop_effect = BishopEffectBase::eval(new_state);
  }
  else if (osl::ptype(last_move) == KING)
  {
    bishop_effect = BishopEffectBase::eval(new_state);
  }
  if (new_state.longEffectChanged(LANCE) || osl::ptype(last_move) == KING)
  {
    LanceMobilityAll::eval(new_state, lance_mobility);
    lance_effect_piece = LanceEffectPieceKingRelative::eval(new_state);
  }

  if (new_state.effectChanged(KNIGHT)) {
    knight_advance = KnightAdvance::eval(new_state);
  }
  King8All::evalWithUpdateBang(new_state, last_move, kingx_blocked);
  const MultiInt silver_features = SilverFeatures::eval(new_state);
  const MultiInt gold_retreat = GoldFeatures::eval(new_state);
  recalculated_stage_value = silver_features+gold_retreat;
  PtypeCountAll::evalWithUpdateBang<P>(new_state,
				    last_move, ptype_count, ptype_board_count,
				       ptype_count_value, updatable_value, ptypeo_mask);
  
  King25EffectEachBoth::evalWithUpdate(new_state, last_move,
				       king25_effect_each);
  
  int p_black_attack_effect = std::min(127, progress.rawData().attackCount[I(WHITE)]);
  int p_white_attack_effect = std::min(127, progress.rawData().attackCount[I(BLACK)]);
  int p_black_attack_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & new_state.piecesOnBoard(BLACK)));
  int p_white_attack_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & new_state.piecesOnBoard(WHITE)));
  int p_black_attack_supported_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & new_state.piecesOnBoard(BLACK) & new_state.effectedMask(BLACK)));
  int p_white_attack_supported_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & new_state.piecesOnBoard(WHITE) & new_state.effectedMask(WHITE)));
  int p_black_defense_effect = std::min(127, progress.rawData().defenseCount[I(BLACK)]);
  int p_white_defense_effect = std::min(127, progress.rawData().defenseCount[I(WHITE)]);
  int p_black_defense_piece = std::min(16, countBit(progress.rawData().mask5x5[I(BLACK)] & new_state.piecesOnBoard(BLACK)));
  int p_white_defense_piece = std::min(16, countBit(progress.rawData().mask5x5[I(WHITE)] & new_state.piecesOnBoard(WHITE)));
  recalculated_value =
    BishopExchangeSilverKing::eval(new_state) + 
    EnterKingDefense::eval(new_state);
#ifdef BISHOP_IN_DANGER
  recalculated_value += BishopInDanger::eval(new_state);
  recalculated_value += RookInDanger::eval(new_state);
#endif
  recalculated_value +=
    King25EffectYAttack::eval(new_state,
			      p_black_attack_effect,
			      p_black_attack_piece,
			      p_white_attack_effect, p_white_attack_piece);

  recalculated_stage_value += 
    King25EffectYDefense::eval(new_state,
			       p_black_defense_effect,
			       p_black_defense_piece,
			       p_white_defense_effect, p_white_defense_piece);
  recalculated_stage_value += knight_advance;
  recalculated_stage_value += kingx_blocked[I(BLACK)] + kingx_blocked[I(WHITE)];
  recalculated_stage_value += NoPawnOnStand::eval(new_state, ptype_count[I(BLACK)][I(PAWN)] + ptype_count[I(BLACK)][I(PPAWN)]);
  recalculated_stage_value += NonPawnPieceStand::eval(non_pawn_stand_count[I(BLACK)], non_pawn_stand_count[I(WHITE)]);
  recalculated_stage_value += PinPtypeAll::eval(new_state);
  recalculated_stage_value += KingMobilityVal::eval(new_state) + KingMobilitySum::eval(new_state);
  recalculated_stage_value += GoldAndSilverNearKing::eval(new_state,
							  gs_near_king_count);
  recalculated_stage_value += PieceStandCombinationBoth::eval(new_state);
  
  {
    MultiInt result_supported_y =
      King25EffectSupportedY::eval(p_black_attack_piece,
				   p_white_attack_piece,
				   p_black_attack_supported_piece,
				   p_white_attack_supported_piece,
				   Y(new_state.kingSquare(BLACK)),
				   Y(new_state.kingSquare(WHITE)));
    recalculated_stage_value += result_supported_y;
    if(isMajor(osl::ptype(last_move)) || 
       isMajor(capturePtype(last_move))){ // rook or bishop
      king_rook_bishop[I(BLACK)]=KingRookBishop::evalOne<BLACK>(new_state);
      king_rook_bishop[I(WHITE)]=KingRookBishop::evalOne<WHITE>(new_state);
    }
    else if(osl::ptype(last_move) == KING){
      king_rook_bishop[I(P)]=KingRookBishop::evalOne<P>(new_state);
    }
    recalculated_stage_value +=king_rook_bishop[I(BLACK)]-king_rook_bishop[I(WHITE)];
//    recalculated_stage_value += KingXBlocked3::eval(new_state);
    recalculated_stage_value += king25_both_side[I(BLACK)]-king25_both_side[I(WHITE)];
  recalculated_stage_value += King25Mobility::eval(new_state,
    progress.rawData().king_vertical_x[I(BLACK)],
    progress.rawData().king_vertical_x[I(WHITE)]);
  }
  piece_pair_value = PiecePair::evalWithUpdateCompiled(new_state,
						       last_move,
						       piece_pair_value);
  PiecePairKing::evalWithUpdateBang<P>(new_state, last_move,
				       piece_pair_king_value);
  pawn_drop = PawnDropAll::evalWithUpdate<P>(new_state,
					     last_move, pawn_drop);

  ptypex = PtypeX::evalWithUpdate<P>(new_state, last_move, ptypex);
  ptypey = PtypeY::evalWithUpdate<P>(new_state, last_move, ptypey);
  std::array<bool, 2> can_check_new;
  can_check_new[I(BLACK)] =
    CanCheckNonPawnPieceStandCombination::canCheck<BLACK>(new_state);
  can_check_new[I(WHITE)] =
    CanCheckNonPawnPieceStandCombination::canCheck<WHITE>(new_state);
  piece_stand_combination =
    NonPawnPieceStandCombination::evalWithUpdate(new_state,
						 last_move,
						 piece_stand_combination,
						 can_check,
						 can_check_new);
  can_check = can_check_new;
  NonPawnPieceStandTurn::evalWithUpdateBang<P>(new_state,
					       last_move,
					       piece_stand_turn);
  rook_pawn = RookPawnY::eval(new_state
);
  piece_stand_y = PieceStandY::evalWithUpdate<P>(new_state, last_move,
						 piece_stand_y);
  PawnAdvanceAll::evalWithUpdateBang<P>(new_state,
					last_move,
					pawn_advance);

  knight_check = KnightCheck::eval(new_state);
  pawn_ptypeo = PawnPtypeOPtypeO::template evalWithUpdate<P>(new_state, last_move,
							     pawn_ptypeo);

  promoted_minor_piece =
    PromotedMinorPieces::evalWithUpdate(new_state,
					last_move,
					promoted_minor_piece);

  KingPiece::evalWithUpdate<P>(new_state, last_move, king_piece);
  NonPawnAttacked::update(P, new_state, last_move, non_pawn_attacked);
  NonPawnAttackedPtype::eval(new_state, non_pawn_attacked_ptype);
  effected_mask[I(BLACK)] =
    effected_mask_for_attacked[I(BLACK)] =
    new_state.effectedMask(BLACK);
  effected_mask[I(WHITE)] =
    effected_mask_for_attacked[I(WHITE)] =
    new_state.effectedMask(WHITE);
  effected_mask_for_attacked[I(BLACK)] &= ~selectPtype(~new_state.promotedPieces(), PAWN);
  effected_mask_for_attacked[I(WHITE)] &= ~selectPtype(~new_state.promotedPieces(), PAWN);
  king3pieces = King3Pieces::evalWithUpdate(new_state, last_move, king3pieces);
  bishop_head = BishopHead::eval(new_state);
  knight_head = KnightHead::eval(new_state);
  rook_promote_defense = RookPromoteDefense::eval(new_state);
  PtypeYPawnY::evalWithUpdateBang<P>(new_state, last_move
, ptype_y_pawn_y);
  recalculated_stage_value += PtypeCombination::eval(ptypeo_mask);
  bishop_and_king = NumPiecesBetweenBishopAndKing::eval(new_state);
  recalculated_stage_value += King25Effect3::eval(new_state, progress.rawData().mask5x5);
  recalculated_stage_value += BishopBishopPiece::eval(new_state);
  recalculated_stage_value += RookRook::eval(new_state);
  recalculated_stage_value += RookRookPiece::eval(new_state);
  recalculated_stage_value += King25EffectCountCombination::eval(new_state, progress.rawData().mask5x5);
  recalculated_stage_value += BishopStandFile5::eval(new_state);
  recalculated_stage_value += MajorCheckWithCapture::eval(new_state);
  recalculated_stage_value += SilverAdvance26::eval(new_state);
  if (base_ptype == ROOK || osl::ptype(last_move) == SILVER ||
      osl::ptype(last_move) == KNIGHT ||
      captured == ROOK || captured == PROOK || captured == SILVER ||
      captured == KNIGHT ||
      (isPromotion(last_move) &&
       (base_ptype == SILVER || base_ptype == KNIGHT)))
  {
    rook_silver_knight = RookSilverKnight::eval(new_state);
  }
  if (base_ptype == BISHOP || osl::ptype(last_move) == SILVER ||
      osl::ptype(last_move) == KNIGHT ||
      captured == BISHOP || captured == PBISHOP || captured == SILVER ||
      captured == KNIGHT ||
      (isPromotion(last_move) &&
       (base_ptype == SILVER || base_ptype == KNIGHT)))
  {
    bishop_silver_knight = BishopSilverKnight::eval(new_state);
  }
  recalculated_stage_value += AttackMajorsInBase::eval(new_state);
  recalculated_stage_value += CheckShadowPtype::eval(new_state);
  recalculated_stage_value += progress.rawData().promotion37_eval;
  recalculated_stage_value += progress.rawData().non_pawn_ptype_attacked_pair_eval[I(BLACK)]
    + progress.rawData().non_pawn_ptype_attacked_pair_eval[I(WHITE)];
  piece_fork_turn = SilverFork::eval(new_state);
  piece_fork_turn += BishopRookFork::eval(new_state);
  piece_fork_turn += KnightFork::evalWithUpdate<P>(new_state, last_move, knight_fork_squares);
  invalidateCache();
}



bool osl::
operator==(const NewProgressData& l, const NewProgressData& r)
{
  return
    l.king_progress == r.king_progress && 
    l.attack5x5_progresses == r.attack5x5_progresses
    && l.non_pawn_ptype_attacked_pair_eval == r.non_pawn_ptype_attacked_pair_eval;
}

std::array<int, 81 * 15 * 11 * 2> osl::NewProgress::effect_relative;
std::array<int, osl::Piece_SIZE>
osl::NewProgress::stand_weight;
std::array<int, 1125>
osl::NewProgress::attack5x5_weight;
std::array<int, 5625>
osl::NewProgress::attack5x5_x_weight;
std::array<int, 10125>
osl::NewProgress::attack5x5_y_weight;
std::array<int, 75>
osl::NewProgress::effectstate_weight;
std::array<int, 4284>
osl::NewProgress::king_relative_weight;
std::array<int, 262144> 
osl::NewProgress::attacked_ptype_pair_weight;
std::array<int, 10> 
osl::NewProgress::pawn_facing_weight;
std::array<int, 16> 
osl::NewProgress::promotion37_weight;
std::array<int, 128> 
osl::NewProgress::piecestand7_combination;
int osl::NewProgress::max_progress;
bool osl::NewProgress::initialized_flag;

bool osl::NewProgress::setUp(const char *filename)
{
  if (initialized_flag)
    return true;

  static std::array<int, 25> effect_weight;
  static std::array<int, 225> effect_x_weight, effect_y_weight;
  static std::array<int, 25> effect_defense_weight;
  static std::array<int, 225> effect_per_effect;
  static std::array<int, 225> effect_per_effect_defense;
  static std::array<int, 2025> effect_per_effect_y, effect_per_effect_x;
  static std::array<int, 56> piecestand7_weight;
  std::ifstream is(filename);

  for(int& v : effect_weight) is >> v;
  for(int& v : effect_x_weight) is >> v;
  for(int& v : effect_y_weight) is >> v;
  for(int& v : effect_defense_weight) is >> v;
  for(int& v : effect_per_effect) is >> v;
  for(int& v : effect_per_effect_defense) is >> v;
  for(int& v : effect_per_effect_y) is >> v;
  for(int& v : effect_per_effect_x) is >> v;
  for(int& v : stand_weight) is >> v;
  for(int& v : attack5x5_weight) is >> v;
  for(int& v : effectstate_weight) is >> v;
  for(int& v : attack5x5_x_weight) is >> v;
  for(int& v : attack5x5_y_weight) is >> v;
  for(int& v : king_relative_weight) is >> v;
  for(int& v : attacked_ptype_pair_weight) is >> v;
  for(int& v : pawn_facing_weight) is >> v;
  for(int& v : promotion37_weight) is >> v;
  for(int& v : piecestand7_weight) is >> v;
  is >> max_progress;
  max_progress = std::min(max_progress, (max_progress/ProgressScale/3)*ProgressScale*3+ProgressScale-1);
  for(int king_x=1;king_x<=9;king_x++){
    for(int king_y=1;king_y<=9;king_y++){
      Square king=newSquare(king_x,king_y);
      int king_index=(king_x-1)*9+king_y-1;
      const Square center = Centering5x3::adjustCenter(king);
      const int min_x = X(center) - 2;
      const int min_y = Y(center) - 1;
      int i=0;
      for (int dx=0; dx<5; ++dx)
      {
	for (int dy=0; dy<3; ++dy,++i)
	{
	  const Square target=newSquare(min_x+dx,min_y+dy);
	  int index0=king_index*15+i;
	  int index_e=index0 * 22;
	  int attack_per_count = effect_weight[index(BLACK, king, target)] +
	    effect_x_weight[indexX(BLACK, king, target)] +
	    effect_y_weight[indexY(BLACK, king, target)];
	  int defense_per_count = effect_defense_weight[index(BLACK, king, target)];
	  for(int count=0;count<=8;count++){
	    effect_relative[index_e + count]=
	      attack_per_count * count +
	      effect_per_effect[indexPerEffect(BLACK, king, target, count)] +
	      effect_per_effect_y[indexPerEffectY(BLACK, king, target, count)] +
	      effect_per_effect_x[indexPerEffectX(BLACK, king, target, count)];
	    effect_relative[index_e + 11 + count]=
	      defense_per_count * count +
	      effect_per_effect_defense[indexPerEffect(BLACK, king, target, count)];
	  }
	  for(int count = 9; count < 11; count++){
	    effect_relative[index_e + count] = effect_relative[index_e + 8] +
	      attack_per_count * (count - 8);
	    effect_relative[index_e + 11 + count] = effect_relative[index_e + 11 + 8] +
	      defense_per_count * (count - 8);
	  }
	}
      }
    }
  }
  for(int king_x=1;king_x<=5;king_x++)
    for(int promoted=0;promoted<=4;promoted++)
      for(int silver=0;silver<=4;silver++)
	for(int gold=0;gold<=4;gold++)
	  for(int bishop=0;bishop<=2;bishop++)
	    for(int rook=0;rook<=2;rook++){
	      int index0=promoted + 5 * (silver + 5 * (gold + 5 * (bishop + 3 * rook)));
	      int index1=king_x - 1 + 5 * (promoted + 5 * (silver + 5 * (gold + 5 * (bishop + 3 * rook))));
	      attack5x5_x_weight[index1]+=attack5x5_weight[index0];
	    }
  for (int i=0; i<PTYPE_SIZE*2*PTYPE_SIZE; ++i)
    for (int j=i+1; j<PTYPE_SIZE*2*PTYPE_SIZE; ++j) {
      attacked_ptype_pair_weight[NonPawnAttackedPtypePair::index2(j,i)]
	= attacked_ptype_pair_weight[NonPawnAttackedPtypePair::index2(i,j)];
    }
  // set sum of [1..i] into [i], keep [0] as is.
  for (int i=2; i<10; ++i)
    pawn_facing_weight[i] += pawn_facing_weight[i-1];

  for(int j = 0; j < 128; j++){
    int piecestand7 = 0;
    std::array<int,7> stand; stand.fill(0);
    int filled = 0;
    for(Ptype ptype : PieceStandOrder)
      if ((j & (1 << (ptype - GOLD))) != 0)
	stand[filled++] = BI(ptype);
    for (int i=0; i<std::min(7,filled+1); ++i)
      piecestand7 += piecestand7_weight[stand[i] + 8*i];
    piecestand7_combination[j] = piecestand7;
  }
  initialized_flag = !is.fail();
  if (!initialized_flag)
  {
    std::cerr << "Failed to load NewProgress data from file " << filename << std::endl;
  }
  return initialized_flag;
}

bool osl::NewProgress::setUp()
{
  return setUp(defaultFilename().c_str());  
}

std::string osl::NewProgress::defaultFilename()
{
  return gpsfish_home() + "progress.txt";
}

void osl::NewProgress::progressOne(Player P,
  const NumEffectState &state, int &attack_defense)
{
  const Square king = state.kingSquare(P);
  const Square center = Centering5x3::adjustCenter(king);
  const int min_x = X(center) - 2;
  const int min_y = Y(center) - 1;

  Square kingRel=king;
  if(P==WHITE){
    kingRel=rotate180(kingRel);
  }
  int index0=((X(kingRel)-1)*9+Y(kingRel)-1)*15;
  int index_e=index0 * 22 + (P==WHITE ? 22 * 14 : 0);
  for (int dx=0; dx<5; ++dx)
  {
    for (int dy=0; dy<3; ++dy)
    {
      const Square target=newSquare(min_x+dx,min_y+dy);
      const int attack_count =
	state.countEffect(alt(P), target);
      const int defense_count =
	state.countEffect(P, target);
      assert(attack_count <= 10 && defense_count <= 10);
      attack_defense += effect_relative[index_e + attack_count] +
	effect_relative[index_e + 11 + defense_count];
      index_e += (P == BLACK ? 22 : -22);
    }
  }
}

void osl::NewProgress::updateOne(Player P,
  const NumEffectState &state, int &attack_defense)
{
  const Square king = state.kingSquare(P);
  BoardMask mask = state.changedEffects() & Board_Mask_Table5x3_Center.mask(king);
  if(!any(mask)) return;
  mask.each([&](Square sq){
    int index_e = indexE(P, king, sq);
    const int attack_count = state.countEffect(alt(P), sq);
    const int old_attack_count = state.previous->countEffect(alt(P), sq);
    if(attack_count != old_attack_count){
      attack_defense += 
	effect_relative[index_e + attack_count] -
	effect_relative[index_e + old_attack_count];
    }
    const int defense_count = state.countEffect(P, sq);
    const int old_defense_count = state.previous->countEffect(P, sq);
    if(defense_count != old_defense_count){
      attack_defense +=
	effect_relative[index_e + 11 + defense_count] -
	effect_relative[index_e + 11 + old_defense_count];
    }
  });
}
void osl::NewProgress::setAttack5x5PiecesAndState(Player P,
  const NumEffectState &state)
{
  const Square king = state.kingSquare(P);
  const int min_x = std::max(1, X(king) - 2);
  const int max_x = std::min(9, X(king) + 2);
  const int min_y = std::max(1, Y(king) - 2);
  const int max_y = std::min(9, Y(king) + 2);

  vertical_x[I(P)].fill(0x1f);
  king_vertical_x[I(P)].fill(0);
  int ac_all = 0, dc_all = 0;
  PieceMask mask=PieceMask(0);
  for (int x = min_x; x <= max_x; ++x){
    int king_x = 0, neg_vertical_x = 0;
    for (int y = min_y; y <= max_y; ++y){
      const Square target=newSquare(x, y);
      int ac = state.countEffect(alt(P), target), dc = state.countEffect(P, target);
      ac_all += ac;
      dc_all += dc;
      const int effect_diff = ac - dc;
      const int x_diff = std::abs(x - X(king));
      const int y_diff = (P == WHITE ? Y(king) - y : y - Y(king));
      int effect_index = std::max(std::min(effect_diff, 2), -2);
      int index0 = effect_index + 2 + 5 * x_diff + 5 * 3 * (y_diff + 2);
      king_progress[I(P)] += effectstate_weight[index0];
      if(ac == 0){
	int y_mask = 1 << (y_diff + 2);
	neg_vertical_x |= y_mask;
	if(canMoveOn(P, state[target])) king_x |= y_mask;
      }
      mask |= state.effect(target);
    }
    vertical_x[I(P)][(P == BLACK ? x - X(king) : X(king) - x) + 2] = neg_vertical_x ^ 0x1f;
    king_vertical_x[I(P)][(P == BLACK ? x - X(king) : X(king) - x) + 2] = king_x;
  }
  mask5x5[I(P)] = mask;
  attackCount[I(P)] = ac_all;
  defenseCount[I(P)] = dc_all;
  attack55_mask[I(P)] = mask & state.piecesOnBoard(alt(P)) & (PieceMask_RBGS | state.promotedPieces());
  setAttack5x5Value(P, state);
}

void osl::NewProgress::setAttack5x5Mask(Player P,
  const NumEffectState &state)
{
  Square king = state.kingSquare(P);
  int min_x = std::max(1, X(king) - 2);
  int max_x = std::min(9, X(king) + 2);
  int min_y = std::max(1, Y(king) - 2);
  int max_y = std::min(9, Y(king) + 2);
  PieceMask mask=PieceMask(0);
  for (int x = min_x; x <= max_x; ++x)
    for (int y = min_y; y <= max_y; ++y)
      mask |= state.effect(newSquare(x, y));
  mask5x5[I(P)] = mask;
  attack55_mask[I(P)] = mask & state.piecesOnBoard(alt(P)) & (PieceMask_RBGS | state.promotedPieces());
}

void osl::NewProgress::setAttack5x5Value(Player P, const NumEffectState &state) 
{
  PieceMask mask = attack55_mask[I(P)];
  mask |= state.standMask(alt(P)) & PieceMask_RBGS;
  int rook = countBit(mask & pieceMask(ROOK));
  int bishop = countBit(mask & pieceMask(BISHOP));
  int gold = countBit(mask & pieceMask(GOLD));
  int silver = countBit(selectPtype(mask & ~state.promotedPieces(), SILVER));
  PieceMask promoted_pieces = mask & state.promotedPieces() & ~(pieceMask(ROOK) | pieceMask(BISHOP));
  int promoted = std::min(countBit(promoted_pieces), 4);
  int king_x = X(state.kingSquare(P));
  if (king_x > 5) king_x = 10 - king_x;
  int king_y = (P == BLACK ? Y(state.kingSquare(P)) : 10 - Y(state.kingSquare(P)));
  attack5x5_progresses[I(P)] = attack5x5_x_weight[index5x5x(rook,bishop, gold, silver, promoted, king_x)] +
    attack5x5_y_weight[index5x5y(rook, bishop, gold, silver, promoted, king_y)];
}

void osl::NewProgress::updateAttack5x5Value(Player P, const NumEffectState &state, NewProgress const& previous, Move last_move) 
{
  PieceMask mask = attack55_mask[I(P)];
  mask |= state.standMask(alt(P)) & PieceMask_RBGS;
  PieceMask oldMask = previous.attack55_mask[I(P)];
  oldMask |= state.previous->standMask(alt(P)) & PieceMask_RBGS;
  if(mask != oldMask ||
	(isNormal(last_move) && isPromotion(last_move) && oldPtype(last_move) == SILVER && test(mask, number(state[to(last_move)])))) setAttack5x5Value(P, state);
}

void osl::NewProgress::updateAttack5x5PiecesAndState(Player P,
						     const NumEffectState &state, NewProgress const& previous, Move last_move)
{
  Square king = state.kingSquare(P);
  BoardMask mask = state.changedSquare() & Board_Mask_Table5x5.mask(king);
  bool mask_changed = false;
  PieceMask p_mask = (isNormal(last_move) && isPromotion(last_move) && !isMajorBasic(oldPtype(last_move)) ? newPieceMask(number(state[to(last_move)])) : PieceMask(0));
  mask.each([&](Square target){
    int ac = state.countEffect(alt(P), target), dc = state.countEffect(P, target);
    int old_ac = state.previous->countEffect(alt(P), target), old_dc = state.previous->countEffect(P, target);
    attackCount[I(P)] += ac - old_ac;
    defenseCount[I(P)] += dc - old_dc;
    if(ac != 0){
      if(old_ac == 0){
	int y_mask = 1 << ((P == WHITE ? Y(king) - Y(target) : Y(target) - Y(king)) + 2);
	vertical_x[I(P)][(P == BLACK ? X(target) - X(king) : X(king) - X(target)) + 2] |= y_mask;
	king_vertical_x[I(P)][(P == BLACK ? X(target) - X(king) : X(king) - X(target)) + 2] &= ~y_mask;
      }
    }
    else {
      int y_mask = 1 << ((P == WHITE ? Y(king) - Y(target) : Y(target) - Y(king)) + 2);
      vertical_x[I(P)][(P == BLACK ? X(target) - X(king) : X(king) - X(target)) 
+ 2] &= ~y_mask;
      if(canMoveOn(P, state[target]))
	king_vertical_x[I(P)][(P == BLACK ? X(target) - X(king) : X(king) - X(target)) + 2] |= y_mask;
      else
	king_vertical_x[I(P)][(P == BLACK ? X(target) - X(king) : X(king) - X(target)) + 2] &= ~y_mask;
    }
    int effect_diff = state.countEffect(alt(P), target) - state.countEffect(P, target);
    int effect_index = std::max(std::min(effect_diff, 2), -2);
    int old_effect_diff = state.previous->countEffect(alt(P), target) - state.previous->countEffect(P, target);
    int old_effect_index = std::max(std::min(old_effect_diff, 2), -2);
    if(effect_index !=old_effect_index){
      const int x_diff = std::abs(X(target) - X(king));
      const int y_diff = (P == WHITE ? Y(king) - Y(target) : Y(target) - Y(king));
      const int idx = 2 + 5 * x_diff + 5 * 3 * (y_diff + 2);
      king_progress[I(P)] += effectstate_weight[idx + effect_index ] -
	effectstate_weight[idx + old_effect_index ];
    }
    if(!mask_changed &&
       ((state.effect(target) & PM40) != (state.previous->effect(target) & PM40)||
	any(state.effect(alt(P), target) & p_mask)))
      mask_changed = true;
  });
  if(!mask_changed && (!isNormal(last_move) ||
	!((isDrop(last_move) && (M(ptype(last_move)) & (M(ROOK)|M(BISHOP)|M(GOLD)|M(SILVER))) != 0) || 
	(isCapture(last_move) && (M(capturePtype(last_move)) & (M(ROOK)|M(BISHOP)|M(GOLD)|M(SILVER)|M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|M(PROOK))) != 0)))) return;
  if(mask_changed) setAttack5x5Mask(P, state);
  updateAttack5x5Value(P, state, previous, last_move);
}

void
  osl::NewProgress::updatePieceKingRelativeBonus(Player P, 
						 const NumEffectState &state, int& result)
{
  Square king = state.kingSquare(P);
  for(Piece p : state.allPiece(state.piecesOnBoard(P) & ~pieceMask(KING))){
    int index_defense = indexRelative(P, king, p) + 2142;
    result += king_relative_weight[index_defense];
  }
  for(Piece p : state.allPiece(state.piecesOnBoard(alt(P)) & ~pieceMask(KING))){
    int index_attack = indexRelative(alt(P), king, p);
    result += king_relative_weight[index_attack];
  }
}

void osl::NewProgress::
setNonPawnAttackedPtypePairOne(osl::Player Owner, const NumEffectState& state)
{
  typedef NonPawnAttackedPtypePair feature_t;
  PieceMask attacked = state.attackedNotPawnKing(Owner);
  PieceMask attacked_supported = attacked & state.effectedMask(Owner);
  for (PieceMask m = attacked_supported; any(m);){
    int num = takeOneBit(m);
    Piece p = state.pieceOf(num);
    Ptype attack_ptype = ptype(state.findCheapAttack(alt(Owner), square(p)));
    nonpawn_indices[num] = I(ptype(p)) * PTYPE_SIZE + I(attack_ptype);
  }
  for (PieceMask m = attacked - attacked_supported; any(m);){
    int num = takeOneBit(m);
    Piece p = state.pieceOf(num);
    Ptype attack_ptype = ptype(state.findCheapAttack(alt(Owner), square(p)));
    nonpawn_indices[num] = (I(ptype(p)) + PTYPE_SIZE) * PTYPE_SIZE + I(attack_ptype);
  }
  int result = 0;
  MultiInt result_eval;
  for(PieceMask m0 = attacked; any(m0);){
    int num0 = takeOneBit(m0);
    int i0 = nonpawn_indices[num0];
    result += attacked_ptype_pair_weight[feature_t::index2(0, i0)];
    for(PieceMask m1 = m0; any(m1);){
      int num1 = takeOneBit(m1);
      int i1 = nonpawn_indices[num1];
      int i2 = feature_t::index2(i0, i1);
      result += attacked_ptype_pair_weight[i2];
      result_eval.addFor(Owner, feature_t::table[i2]);
    }
  }
  progress_all += result;
  non_pawn_ptype_attacked_pair_eval[I(Owner)] = result_eval;
}

void osl::NewProgress::
updateNonPawnAttackedPtypePair(const NumEffectState& state, NewProgress const& previous)
{
  typedef NonPawnAttackedPtypePair feature_t;
  for(Player P : COLORS){
    MultiInt& result_eval = non_pawn_ptype_attacked_pair_eval[I(P)];
    PieceMask old_attacked = state.previous->attackedNotPawnKing(P);
    PieceMask new_attacked = state.attackedNotPawnKing(P);
    PieceMask old_only = old_attacked & ~new_attacked;
    PieceMask new_only = ~old_attacked & new_attacked;
    PieceMask both = old_attacked & new_attacked;
    PieceMask must_check = both & (state.effectedChanged(BLACK) | state.effectedChanged(WHITE));
    PieceMask changed = PieceMask(0);
    while(any(must_check)){
      PieceMask m = lowestMask(must_check);
      int num = takeOneBit(must_check);
      Piece p = state.pieceOf(num);
      int i = feature_t::index1(state, p);
      if( i != nonpawn_indices[num] ){
	changed += m;
	nonpawn_indices[num] = i;
      }
    }
    for(PieceMask m = new_only; any(m);){
      int num = takeOneBit(m);
      Piece p = state.pieceOf(num);
      nonpawn_indices[num] = feature_t::index1(state, p);
    }
    PieceMask not_changed = both - changed;
    for(PieceMask m0 = old_only + changed; any(m0);){
      int num0 = takeOneBit(m0);
      int i0 = previous.nonpawn_indices[num0];
      progress_all -= attacked_ptype_pair_weight[feature_t::index2(0, i0)];
      for(PieceMask m1 = m0 + not_changed; any(m1);){
	int num1 = takeOneBit(m1);
	int i1 = previous.nonpawn_indices[num1];
	int i2 = feature_t::index2(i0, i1);
	progress_all -= attacked_ptype_pair_weight[i2];
	result_eval.addFor(alt(P), feature_t::table[i2]);
      }
    }
    for(PieceMask m0 = new_only + changed; any(m0);){
      int num0 = takeOneBit(m0);
      int i0 = nonpawn_indices[num0];
      progress_all += attacked_ptype_pair_weight[feature_t::index2(0, i0)];
      for(PieceMask m1 = m0 + not_changed; any(m1);){
	int num1 = takeOneBit(m1);
	int i1 = nonpawn_indices[num1];
	int i2 = feature_t::index2(i0, i1);
	progress_all += attacked_ptype_pair_weight[i2];
	result_eval.addFor(P, feature_t::table[i2]);
      }
    }
  }
}

void osl::NewProgress::
updatePawnFacing(const NumEffectState& state)
{
  pawn_facing_count = 0;
  for(int x=1;x<=9;x++) 
    if(state.pawnSquare(BLACK,x)-state.pawnSquare(WHITE,x)==Offset(1))
      pawn_facing_count++;
  progress_all += pawn_facing_weight[pawn_facing_count];
}

void osl::
NewProgress::promotion37One(Player P, const NumEffectState& state)
{
  typedef Promotion37 feature_t;
  promotion37_count[I(P)].fill(0);
  promotion37_x[I(P)].fill(0);
  BoardMask mask = Promotion37_Mask.mask(P);
  mask.andNot(state.piece_mask);
  mask.each([&](Square target){
    int a = state.countEffect(P, target);
    int d = state.countEffect(alt(P), target);
#if 0
    if(a==0 || a<d) return;
#else
    if (a > 0 && a == d)
      a += state.hasAdditionalEffect(P, target);
    if (a <= d) return;
#endif
    Ptype ptype = osl::ptype(state.findCheapAttack(P, target));
    if(! isPromoted(ptype)){
      promotion37_count[I(P)][BI(ptype)]++;
      promotion37_x[I(P)][X(target)] = I(ptype);
    }
  });
  for (Ptype p=Ptype::BASIC_MIN; p<=Ptype::MAX; ++p) {
    if (promotion37_count[I(P)][BI(p)] > 0) {
      progress_all += promotion37_weight[I(p)];
      promotion37_eval += feature_t::table[I(p)]*sign(P);
      if (promotion37_count[I(P)][BI(p)] > 1) {
	progress_all += promotion37_weight[I(p)-8]*(promotion37_count[I(P)][BI(p)]-1);
	promotion37_eval += feature_t::table[I(p)-8]*(sign(P)*(promotion37_count[I(P)][BI(p)]-1));
      }
    }
  }
}

void osl::NewProgress::
updatePromotion37(const NumEffectState& state)
{
  typedef Promotion37 feature_t;
  for(Player P : COLORS) {
#if 1
    BoardMask mask = Promotion37_Mask.mask(P);
    mask.andNot(state.piece_mask & state.previous->piece_mask); 
#else
    BoardMask mask = Promotion37_Mask.mask(P) & state.changedSquare();
#endif
    mask.each([&](Square target){
      int a = state.countEffect(P, target);
      int d = state.countEffect(alt(P), target);
#if 1
      if (a > 0 && a == d)
        a += state.hasAdditionalEffect(P, target);
#endif
      Ptype old_ptype = Ptype(promotion37_x[I(P)][X(target)]);
      Ptype new_ptype = Ptype(0);
#if 0
      if(isEmpty(state[target]) && !(a == 0 || a < d)){
#else
      if(isEmpty(state[target]) && a > d){
#endif
	Ptype ptype = osl::ptype(state.findCheapAttack(P, target));
	if(! isPromoted(ptype)) new_ptype = ptype;
      }
      if(old_ptype != new_ptype){
	promotion37_x[I(P)][X(target)] = uint8_t(new_ptype);
	if(old_ptype != Ptype(0)){
	  promotion37_count[I(P)][BI(old_ptype)]--;
	  if(promotion37_count[I(P)][BI(old_ptype)] == 0){
	    progress_all -= promotion37_weight[I(old_ptype)];
	    promotion37_eval -= feature_t::table[I(old_ptype)]*sign(P);
	  }
	  else{
	    progress_all -= promotion37_weight[I(old_ptype)-8];
	    promotion37_eval -= feature_t::table[I(old_ptype)-8]*sign(P);
	  }
	}
	if(new_ptype != Ptype(0)){
	  promotion37_count[I(P)][BI(new_ptype)]++;
	  if(promotion37_count[I(P)][BI(new_ptype)] == 1){
	    progress_all += promotion37_weight[I(new_ptype)];
	    promotion37_eval += feature_t::table[I(new_ptype)]*sign(P);
	  }
	  else{
	    progress_all += promotion37_weight[I(new_ptype)-8];
	    promotion37_eval += feature_t::table[I(new_ptype)-8]*sign(P);
	  }
	}
      }
    });
  }
}

void osl::NewProgress::
updatePieceStand7(const NumEffectState& state)
{
  for (Player z : COLORS) {
    int mask = 0;
    for(Ptype ptype = GOLD; ptype <= ROOK; ptype++)
      if (state.hasPieceOnStand(z, ptype)) mask += (1 << (ptype - GOLD));
    piecestand7_mask[I(z)] = mask;
    progress_all += piecestand7_combination[mask];
  }
}

osl::NewProgress::NewProgress(
  const NumEffectState &state)
{
  assert(initialized_flag);
  
  progress_all = 0;
  promotion37_eval = MultiInt();
  for(Player P : COLORS){
    king_progress[I(P)] = 0;
    progressOne(P, state, king_progress[I(P)]);
    setAttack5x5PiecesAndState(P, state);
    for(Ptype ptype : PieceStandOrder){
      int attack_count = state.countPiecesOnStand(alt(P), ptype);
      for (int j = 0; j < attack_count; ++j)
	progress_all += stand_weight[indexMin(ptype) + j];
    }
    updatePieceKingRelativeBonus(P, state, king_progress[I(P)]);
    promotion37One(P, state);
    setNonPawnAttackedPtypePairOne(P, state);
  }
  updatePawnFacing(state);
  updatePieceStand7(state);
}

template<osl::Player P>
inline
void osl::NewProgress::update(const NumEffectState &new_state,
			      Move last_move, NewProgress const& previous)
{
  constexpr Player altP=alt(P);
  assert(int(new_state.turn())==int(altP));
  assert(player(last_move)==P);
  const Ptype captured = capturePtype(last_move);

  if (captured != Ptype::EMPTY){
    if(captured == PAWN){
      if(oldPtype(last_move) == PAWN ||
	 ptype(new_state[nextSquare(P, to(last_move), D)]) == PAWN){
	pawn_facing_count--;
	progress_all += pawn_facing_weight[pawn_facing_count] -
	  pawn_facing_weight[pawn_facing_count + 1];
      }
    }
    Ptype ptype = unpromote(captured);
    const int count = new_state.countPiecesOnStand(P, ptype);
    const int value = stand_weight[(indexMin(ptype) + count - 1)];
    progress_all += value;
    if(count == 1){
      int oldMask = piecestand7_mask[I(P)];
      int newMask = oldMask + (1 << (ptype - GOLD));
      progress_all += piecestand7_combination[newMask] - piecestand7_combination[oldMask];
      piecestand7_mask[I(P)] = newMask;
    }
  }
  if (ptype(last_move) == KING){
    king_progress[I(P)] = 0;
    setAttack5x5PiecesAndState(P, new_state);
    updatePieceKingRelativeBonus(P, new_state, king_progress[I(P)]);
    if (captured != Ptype::EMPTY){
      int index_defense = indexRelative(altP, new_state.kingSquare(altP),
			      captured, to(last_move)) + 2142;
      king_progress[I(altP)] -=	king_relative_weight[index_defense];
    }
    progressOne(P, new_state, king_progress[I(P)]);
    updateOne(altP, new_state, king_progress[I(altP)]);
  }
  else
  {
    updateAttack5x5PiecesAndState(P, new_state, previous, Move_INVALID);
    updateOne(P, new_state, king_progress[I(P)]);
    updateOne(altP, new_state, king_progress[I(altP)]);
    {
      const int index_attack =
	indexRelative(P, new_state.kingSquare(altP), ptype(last_move), to(last_move));
      const int index_defense =
	indexRelative(P, new_state.kingSquare(P), ptype(last_move), to(last_move)) + 2142;
      king_progress[I(alt(P))] += king_relative_weight[index_attack];
      king_progress[I(P)] += king_relative_weight[index_defense];
    }
    if(ptype(last_move) == PAWN &&
       ptype(new_state[nextSquare(P, to(last_move), U)]) == PAWN){
      pawn_facing_count++;
      progress_all += pawn_facing_weight[pawn_facing_count] -
	pawn_facing_weight[pawn_facing_count - 1];
    }
    if (isDrop(last_move)){
      Ptype T = oldPtype(last_move);
      int count = new_state.countPiecesOnStand(P, T);
      int value = stand_weight[indexMin(T) + count];
      progress_all -= value;
      if(count == 0){
        int oldMask = piecestand7_mask[I(P)];
        int newMask = oldMask - (1 << (T - GOLD));
        progress_all += piecestand7_combination[newMask] - piecestand7_combination[oldMask];
        piecestand7_mask[I(P)] = newMask;
      }
    }
    else{
      int index_attack =
	indexRelative(P, new_state.kingSquare(altP),
			 oldPtype(last_move), from(last_move));
      int index_defense =
	indexRelative(P, new_state.kingSquare(P),
			 oldPtype(last_move), from(last_move)) + 2142;
      king_progress[I(alt(P))] -= king_relative_weight[index_attack];
      king_progress[I(P)] -= king_relative_weight[index_defense];
      if (captured != Ptype::EMPTY){
        index_attack = indexRelative(altP, new_state.kingSquare(P), captured, to(last_move));
        index_defense = indexRelative(altP, new_state.kingSquare(altP), captured, to(last_move)) + 2142;
        king_progress[I(P)] -= king_relative_weight[index_attack];
        king_progress[I(altP)] -= king_relative_weight[index_defense];
      }
    }
  }
  updateAttack5x5PiecesAndState(alt(P), new_state, previous, last_move);
  updateNonPawnAttackedPtypePair(new_state, previous);
  updatePromotion37(new_state);
}

void osl::NewProgress::show(std::ostream& os) const{
  os << "0 " <<  promotion37_eval << std::endl;
  os << "1 " <<  progress_all << std::endl;
  os << "2 " <<  king_progress << std::endl;
  os << "3 " <<  attack5x5_progresses << std::endl;
  os << "4 " <<  piecestand7_mask << std::endl;
  os << "5 " <<  pawn_facing_count << std::endl;
  os << "6 " <<  promotion37_x << std::endl;
  os << "7 " <<  promotion37_count << std::endl;
  os << "8 " <<  nonpawn_indices << std::endl;
  os << "9 " <<  attack55_mask << std::endl;
}

std::ostream& osl::OpenMidEndingEval::show(std::ostream& os) const{
  os << "0 \n";
  progress.show(std::cerr);
  os << "1 " << kingx_blocked << "," <<  king25_effect_each << std::endl;
  os <<	"2 " << king25_both_side << "," << king_rook_bishop << std::endl;
  os << "3 " << piece_stand_turn << "," <<  non_pawn_attacked[0] << "," << non_pawn_attacked[1] << std::endl;
  os << "4 " << non_pawn_attacked_ptype << "," <<  piece_fork_turn << std::endl;
  os << "5 " << ptypey << "," <<  ptypex << std::endl;
  os << "7 " << piece_stand_value<< "," <<  recalculated_stage_value<< "," <<  pawn_advance << std::endl;
  os << "8 " << rook_mobility<< "," <<  bishop_mobility<< "," <<  lance_mobility << std::endl;
  os << "9 " << knight_advance<< "," <<  pawn_drop<< "," <<  promoted_minor_piece<< "," <<  rook_pawn<< "," << 
    rook_effect<< "," <<  bishop_effect<< "," <<  bishop_head<< "," << "," << "," <<  king3pieces << std::endl;
  os << "10 " << rook_promote_defense << std::endl;
  os << "11 " << piece_stand_combination<< "," <<  piece_stand_y<< "," <<  knight_check<< "," << 
    knight_head<< "," <<  pawn_ptypeo<< "," <<  ptype_count_value<< "," <<  lance_effect_piece<< "," << 
    ptype_y_pawn_y<< "," <<  bishop_and_king<< "," <<  rook_silver_knight<< "," <<  bishop_silver_knight << std::endl;
  os << "12 " << knight_fork_squares << std::endl;
  os << "15 " << effected_mask << std::endl;
  os << "16 " << effected_mask_for_attacked << std::endl;
  // flat
  os << "19 " << piece_pair_king_value << std::endl;
  os << "20 " << non_pawn_stand_count << std::endl;
  os << "21 " << gs_near_king_count << std::endl;
  os << "22 " << ptype_count<< "," <<  ptype_board_count << std::endl;
  os << recalculated_value<< "," <<  piece_pair_value << std::endl;
  os << "30 " << turn << std::endl;
  os << "31 " << ptypeo_mask << std::endl;
  os << "32 " << can_check << std::endl; // king is defense
  return os;
}

namespace osl
{
  template void osl::NewProgress::update<osl::BLACK>(const NumEffectState &new_state,Move last_move, NewProgress const& previous);
  template void osl::NewProgress::update<osl::WHITE>(const NumEffectState &new_state,Move last_move, NewProgress const& previous);
}


// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
