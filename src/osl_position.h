#ifndef OSL_POSITION_H
#define OSL_POSITION_H

#include "osl_types.h"
#include "config.h"
#include <cassert>
#include <iosfwd>

#define COPY_STATE
namespace osl
{
  class NumEffectState;
} // namespace osl

namespace osl 
{
  /**
   * (2014/1/4)
   * - 0-39 :  effects of pieces ( 0-39 )
   * - 40-47 : long effect of pieces (32-39)
   * - 48-55 : count effects of black pieces
   * - 56-63 : count effects of white pieces
   */
  class NumBitmapEffect
  {
  private:
  public:
    PieceMask pm;
    NumBitmapEffect(){
      resetAll(pm);
    }
    static NumBitmapEffect playerEffect(Player pl){
      NumBitmapEffect ret;
      flip(ret.pm, pl==BLACK ? 48 : 56);
      return ret;
    }
    int countEffect(Player pl) const {
      int shift=48+(8&static_cast<int>(pl));
      return V(pm >> shift) & 0xff;
    }

    static NumBitmapEffect makeEffect(Player pl, int num){
      NumBitmapEffect effect=playerEffect(pl);
      flip(effect.pm, num);
      return effect;
    }
    enum Op{
      Add,Sub,
    };
    NumBitmapEffect& opEqual(Op OP, NumBitmapEffect const& rhs){
      if(OP == Add) pm += rhs.pm;
      else pm -= rhs.pm;
      return *this;
    }

    static NumBitmapEffect makeLongEffect(Player pl,int num){
      assert(32<=num && num<=39);
      NumBitmapEffect effect=NumBitmapEffect::playerEffect(pl);
      flip(effect.pm, num);
      flip(effect.pm, num+8);
      return effect;
    }

  };
} // namespace osl


namespace osl
{
  /**
   * farthest square of a direction (by black ) from a piece (has long effects)
   * square may be occupied by an opponent piece or a owner's piece or an edge
   * if the piece don't have an effect nor the piece is on stand. the value is 0
   * Because each piece has four long effect direction, the size of the array
   * is reduced to four.
   */
  class MobilityContent
  {
    union ALIGN(4) {
      uint32_t lv;
      std::array<uint8_t, 4> uc;
    };
  public:
    MobilityContent() {
      clear();
    }
    void clear(){
      lv = 0u;
    }
    Square get(Direction d) const{
      return Square(uc[int(d) >> 1]);
    }
    void set(Direction d, Square pos){
      uc[int(d) >> 1] = uint8_t(pos);
    }
  };
  std::ostream& operator<<(std::ostream& os,MobilityContent const& mc);

  /**
   * retrieve a MobilityContent from a piece number
   */
  class MobilityTable
  {
    ALIGN(16) std::array<MobilityContent, 8> table;
  public:
    MobilityTable(){}
    void set(Direction d, int num, Square pos){
      assert(isShort8(d));
      assert(32 <= num  && num < 40);
      table[num - 32].set(d, pos);
    }
    Square get(Direction d, int num) const{
      assert(isShort8(d));
      assert(32 <= num  && num < 40);
      return table[num - 32].get(d);
    }
    friend bool operator==(const MobilityTable& mt1,const MobilityTable& mt2);
  };
  std::ostream& operator<<(std::ostream& os,MobilityTable const& mt);
  bool operator==(const MobilityTable&,const MobilityTable&);
}

namespace osl
{
  class KingMobility{
    union ALIGN(16) {
      std::array<std::array<uint8_t, 8>,2> uc16;
      unsigned long long ul[2];
#ifdef OSL_USE_SSE
      __m128i v2;
#endif
    };
  public:
    KingMobility() {
      assert(reinterpret_cast<size_t>(this) % 16 == 0);
    }
    const std::array<uint8_t, 8>& operator[](int i) const{
      return uc16[i];
    }
    std::array<uint8_t, 8>& operator[](int i){
      return uc16[i];
    }
    KingMobility& operator=(KingMobility const& km){
#ifdef OSL_USE_SSE
      v2=km.v2;
#else
      uc16=km.uc16;
#endif
      return *this;
    }
    bool operator==(KingMobility const& km) const{
#ifdef OSL_USE_SSE41
      __m128i m = _mm_xor_si128(v2, km.v2);
      return _mm_test_all_zeros(m, m);
#else
      return ((ul[0]^km.ul[0])|(ul[1]^km.ul[1]))==0;
#endif
    }
  };
}

namespace osl
{
  union ALIGN(8) Byte8 {
    unsigned long long lv;
    std::array<unsigned char,8> uc;
  };
  /**
   * 盤面上の駒が「黒から見た」方向に長い利きをつけられている時に，
   * 利きをつけている駒の番号を得る
   * たとえば，Uの時は下から上方向の長い利きがついているものとする．
   * その方向の利きがついていない場合はEMPTY_NUM(0x80)を入れておく．
   */
  class EffectedNum
  {
  private:
    Byte8 b8;
  public:
    EffectedNum() { clear(); }
    void clear(){
#define E(n) ((static_cast<unsigned long long>(EMPTY_NUM)<<((n)*8)))
      b8.lv= E(0)|E(1)|E(2)|E(3)|E(4)|E(5)|E(6)|E(7);
#undef E
    }
    int operator[](Direction d) const{
      assert(isShort8(d));
      return b8.uc[I(d)];
    }
    unsigned char& operator[](Direction d){
      assert(isShort8(d));
      return b8.uc[I(d)];
    }
  };
  class EffectedNumTable
  {
    ALIGN(16)
    std::array<EffectedNum,40> contents;
  public:
    EffectedNumTable() { clear(); }
    const EffectedNum& operator[](int i) const {
      return contents[i];
    }
    void clear();
    EffectedNum& operator[](int i){
      return contents[i];
    }
  };
  bool operator==(const EffectedNumTable&,const EffectedNumTable&);
  std::ostream& operator<<(std::ostream&,const EffectedNumTable&);
}
/* boardMask.h
 */
namespace osl 
{
  class BoardMask;
  bool operator==(const BoardMask&, const BoardMask&);
  /** use SquareCompressor */
  class BoardMask
  {
  public:
    union  ALIGN(16) {
      std::array<uint64_t,2>  contents;
#ifdef OSL_USE_SSE
      __m128i v128;
#endif
    };
  public:
    void clear() { 
#ifdef OSL_USE_SSE
      v128 = (__m128i)_mm_setzero_ps();
#else
      contents[0]=contents[1]=0; 
#endif
    }
    void invalidate() {
#ifdef OSL_USE_SSE
      v128 = _mm_set_epi32(-1,-1,-1,-1);
#else
      contents[1] = static_cast<uint64_t>(-1ll); 
#endif
    }
    bool isInvalid() const {
      return contents[1] == static_cast<uint64_t>(-1ll); 
    }
    void set(unsigned int i) {
      assert(isOnBoard(toSquare(i)));
      int j=(i>>6);
      contents[j]|=(1ull<<(i&63));
    }
    void set(Square pos);
    void reset(unsigned int i) {
      assert(i < 82);
      int j=(i>>6);
      contents[j] &= ~(1ull<<(i&63));
    }
    void reset(Square pos);
    bool test(unsigned int i) const {
      assert(i < 82);
      int j=(i>>6);
      return (contents[j]&(1ull<<(i&63)))!=0;
    }
    bool test(Square pos) const { return test(index(pos)); }
    bool anyInRange(const BoardMask& mask) const
    {
#if defined(OSL_USE_SSE41)
      return !_mm_test_all_zeros(v128,mask.v128);
#else
      return (contents[0] & mask.contents[0]) 
	|| (contents[1] & mask.contents[1]);
#endif
    }
    template<int n>
    void slli(){
#ifdef OSL_USE_SSE
      v128 = _mm_slli_si128(v128, n);
#else
      assert(0 <= n && n <= 63);
      uint64_t c0 = contents[0];
      contents[0] = (c0 << n);
      contents[1] = (c0 >> (64 - n)) | (contents[1] << n);
#endif	
    }
    template<int n>
    void srli(){
#ifdef OSL_USE_SSE
      v128 = _mm_srli_si128(v128, n);
#else
      assert(0 <= n && n <= 63);
      uint64_t c1 = contents[1];
      contents[1] = c1 >> n;
      contents[0] = (contents[0] >> n) | (c1 << (64 - n));
#endif	
    }
    /*
     * *this = ~mask & *this
     */
    void andNot(const BoardMask& mask){
#ifdef OSL_USE_SSE
      v128 = _mm_andnot_si128(mask.v128, v128);
#else
      contents[0] &= ~mask.contents[0];
      contents[1] &= ~mask.contents[1];
#endif
    }
    BoardMask& operator|=(const BoardMask& mask)
    {
#ifdef OSL_USE_SSE
      v128 = _mm_or_si128(v128, mask.v128);
#else
      contents[0] |= mask.contents[0];
      contents[1] |= mask.contents[1];
#endif
      return *this;
    }
#if 0
    BoardMask& operator&=(const BoardMask& mask)
    {
#ifdef OSL_USE_SSE
      v128 = _mm_and_si128(v128, mask.v128);
#else
      contents[0] &= mask.contents[0];
      contents[1] &= mask.contents[1];
#endif
      return *this;
    }
#endif
    bool any() const
    {
      assert(! isInvalid());
#if defined(OSL_USE_SSE41)
      return !_mm_test_all_zeros(v128, v128);
#else
      return contents[0] || contents[1];
#endif
    }
    Square takeOneBit()
    {
      assert(! isInvalid() && any());
      if (contents[0])
	return toSquare(osl::takeOneBit(contents[0]));
      return toSquare(osl::takeOneBit(contents[1])+64);
    }
    template<typename F>
    void each(F const& f) const{
      for(uint64_t m = contents[0]; m;) f(toSquare(osl::takeOneBit(m)));
      for(uint64_t m = contents[1]; m;) f(toSquare(osl::takeOneBit(m) + 64));
    }
    static int index(int x,int y){
      assert(1 <= x && x <= 9 && 1 <= y && y <= 9);
      return SquareCompressor::compress(newSquare(x,y));
    }
    static int index(Square pos) {
      assert(isPieceStand(pos) || isOnBoard(pos));
      return SquareCompressor::compress(pos);
    }
    static int getIndexOffset(Player P, Direction Dir) {
      int blackDx = blackDxs[I(Dir)];
      int blackDy = blackDys[I(Dir)];
      int val = blackDx * 9 + blackDy;
      return (P == BLACK ? val : -val);
    }
    static Square toSquare(int n) {
      return SquareCompressor::melt(n);
    } 
    friend bool operator==(const BoardMask&, const BoardMask&);
  };
  std::ostream& operator<<(std::ostream&, const BoardMask&);
  inline const BoardMask operator|(const BoardMask& l, const BoardMask& r)
  {
    BoardMask result = l;
    result |= r;
    return result;
  }
  inline const BoardMask operator&(const BoardMask& l, const BoardMask& r)
  {
    BoardMask result;
#if defined(OSL_USE_SSE)
    result.v128 = _mm_and_si128(l.v128, r.v128);
#else
    result.contents[0] = l.contents[0] & r.contents[0];
    result.contents[1] = l.contents[1] & r.contents[1];
#endif
    return result;
  }
  inline bool operator==(const BoardMask& l, const BoardMask& r)
  {
#if defined(OSL_USE_SSE41)
    __m128i m = _mm_xor_si128(l.v128, r.v128);
    return _mm_test_all_zeros(m, m);
#else
    return l.contents[0] == r.contents[0]
      && l.contents[1] == r.contents[1];
#endif
  }
  static const BoardMask Board_Mask_Table[Square_SIZE] = 
  {
    {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, 
    {0,0}, {0,0},{1ull << 1, 0ull}, {1ull << 2, 0ull}, {1ull << 3, 0ull}, {1ull << 4, 0ull}, {1ull << 5, 0ull}, {1ull << 6, 0ull}, {1ull << 7, 0ull}, {1ull << 8, 0ull}, {1ull << 9, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 10, 0ull}, {1ull << 11, 0ull}, {1ull << 12, 0ull}, {1ull << 13, 0ull}, {1ull << 14, 0ull}, {1ull << 15, 0ull}, {1ull << 16, 0ull}, {1ull << 17, 0ull}, {1ull << 18, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 19, 0ull}, {1ull << 20, 0ull}, {1ull << 21, 0ull}, {1ull << 22, 0ull}, {1ull << 23, 0ull}, {1ull << 24, 0ull}, {1ull << 25, 0ull}, {1ull << 26, 0ull}, {1ull << 27, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 28, 0ull}, {1ull << 29, 0ull}, {1ull << 30, 0ull}, {1ull << 31, 0ull}, {1ull << 32, 0ull}, {1ull << 33, 0ull}, {1ull << 34, 0ull}, {1ull << 35, 0ull}, {1ull << 36, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 37, 0ull}, {1ull << 38, 0ull}, {1ull << 39, 0ull}, {1ull << 40, 0ull}, {1ull << 41, 0ull}, {1ull << 42, 0ull}, {1ull << 43, 0ull}, {1ull << 44, 0ull}, {1ull << 45, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 46, 0ull}, {1ull << 47, 0ull}, {1ull << 48, 0ull}, {1ull << 49, 0ull}, {1ull << 50, 0ull}, {1ull << 51, 0ull}, {1ull << 52, 0ull}, {1ull << 53, 0ull}, {1ull << 54, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{1ull << 55, 0ull}, {1ull << 56, 0ull}, {1ull << 57, 0ull}, {1ull << 58, 0ull}, {1ull << 59, 0ull}, {1ull << 60, 0ull}, {1ull << 61, 0ull}, {1ull << 62, 0ull}, {1ull << 63, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{0ull, 1ull << 0}, {0ull, 1ull << 1}, {0ull, 1ull << 2}, {0ull, 1ull << 3}, {0ull, 1ull << 4}, {0ull, 1ull << 5}, {0ull, 1ull << 6}, {0ull, 1ull << 7}, {0ull, 1ull << 8}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0}, {0,0},{0ull, 1ull << 9}, {0ull, 1ull << 10}, {0ull, 1ull << 11}, {0ull, 1ull << 12}, {0ull, 1ull << 13}, {0ull, 1ull << 14}, {0ull, 1ull << 15}, {0ull, 1ull << 16}, {0ull, 1ull << 17}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
    {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0}, {0,0}
  };
  inline void BoardMask::set(Square sq){
    assert(Board_Mask_Table[I(sq)].test(sq));
    *this |= Board_Mask_Table[I(sq)];
  }
  inline void BoardMask::reset(Square sq){
    andNot(Board_Mask_Table[I(sq)]);
  }
  class BoardMaskTable5x5 
  {
    std::array<BoardMask, Square_SIZE> data;
  public:
    BoardMaskTable5x5();
    /** p中心の5x5 の範囲のbitを立てたもの, centeringなし*/
    const BoardMask& mask(Square p) const { return data[I(p)]; }
  };
  extern const BoardMaskTable5x5 Board_Mask_Table5x5;

  class BoardMaskTable3x3 
  {
    std::array<BoardMask, Square_SIZE> data;
  public:
    BoardMaskTable3x3();
    /** p中心の3x3 の範囲のbitを立てたもの, centeringなし*/
    const BoardMask& mask(Square p) const { return data[I(p)]; }
  };
  extern const BoardMaskTable3x3 Board_Mask_Table3x3;

  class BoardMaskTable5x3Center
  {
    std::array<BoardMask, Square_SIZE> data;
  public:
    BoardMaskTable5x3Center();
    /** p中心の5x3 の範囲のbitを立てたもの, centering*/
    const BoardMask& mask(Square p) const { return data[I(p)]; }
  };
  extern const BoardMaskTable5x3Center Board_Mask_Table5x3_Center;
  inline Square takeOneBit(BoardMask& bm){
    return bm.takeOneBit();
  }
  inline bool any(BoardMask const& bm){
    return bm.any();
  }
  class BoardMaskTableDir
  {
    std::array<std::array<BoardMask, 8>, Square_SIZE> data;
  public:
    BoardMaskTableDir();
    const BoardMask& mask(Square sq, Direction d) const{
      assert(isOnBoard(sq));
      return data[I(sq)][I(d)];
    }
  };
  extern const BoardMaskTableDir Board_Mask_Table_Dir;
  class Promotion37Mask
  {
    std::array<BoardMask, 2> data;
  public:
    Promotion37Mask();
    const BoardMask& mask(Player P) const{
      return data[I(P)];
    }
  };
  extern const Promotion37Mask Promotion37_Mask;
} // namespace osl


namespace osl 
{
  class NumSimpleEffectTable;
  bool operator==(const NumSimpleEffectTable&,const NumSimpleEffectTable&);
  std::ostream& operator<<(std::ostream&, const NumSimpleEffectTable&);

  /**
   * 局面全体の利きデータ.
   */
  class NumSimpleEffectTable
  {
  protected:
    ALIGN(16)
    std::array<NumBitmapEffect, Square_SIZE> effects;
    BoardMask changed_effects; // each player
  public:
    BoardMask changed_square; // each player
  protected:
    /** set of pieces whose effect changed by previous move */
    NumBitmapEffect changed_effect_pieces; 
  public:
    std::array<PieceMask,2> effected_mask;
    std::array<PieceMask,2> effected_changed_mask;
    /** mobility */
    MobilityTable mobilityTable;
    /** effected num */
    EffectedNumTable effectedNumTable;
    /**
     * ある位置からある方向に短い利きがある時に，その方向の利きを更新する.
     * @param P(template) - ある位置にある駒の所有者
     * @param T(template) - ある位置にある駒の種類
     * @param D(template) - 駒の所有者の立場から見た方向
     * @param OP(template) - 利きを足すか，減らすか
     * @param pos - 駒の位置
     * @param num - 駒番号
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doEffectShort(Player P,Ptype T,Direction Dir, const NumEffectState& state,Square pos,int num);
    /**
     * ある位置からある方向に長い利きがある時に，その方向の利きを更新する.
     * @param P(template) - ある位置にある駒の所有者
     * @param T(template) - ある位置にある駒の種類
     * @param Dir(template) - 黒の立場から見た方向
     * @param OP(template) - 利きを足すか，減らすか
     * @param state - 盤面(動かした後)
     * @param pos - 駒の位置
     * @param num - 駒番号
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doEffectLong(Player P,Ptype T, Direction Dir, const NumEffectState& state,Square pos,int num);
    /**
     * ある種類の駒が持つ利きを更新する.
     * @param P(template) - ある位置にある駒の所有者
     * @param T(template) - ある位置にある駒の種類
     * @param OP(template) - 利きを足すか，減らすか
     * @param state - 盤面(動かした後)
     * @param pos - 駒の位置
     * @param num - 駒番号
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doEffectBy(Player P,Ptype T,const NumEffectState& state,Square pos,int num);
    /**
     * ある種類の駒が持つ利きを更新する.
     * @param OP(template) - 利きを足すか，減らすか
     * @param state - 盤面(動かした後)
     * @param ptypeo - 駒の種類
     * @param pos - 駒の位置
     * @param num - 駒番号
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doEffect(const NumEffectState& state,PtypeO ptypeo,Square pos,int num);

    /**
     * ある駒が持つ利きを更新する.
     * @param OP(template) - 利きを足すか，減らすか
     * @param state - 盤面(動かした後)
     * @param p - 駒
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doEffect(const NumEffectState& state,Piece p)
    {
      doEffect<OP,UC>(state,osl::ptypeO(p),square(p),number(p));
    }
    /**
     * 盤面のデータを元に初期化する.
     * @param state - 盤面
     */
    void init(const NumEffectState& state);
    /**
     * ある位置の利きデータを取り出す.
     * @param pos - 位置
     */
    const NumBitmapEffect effectSetAt(Square pos) const
    {
      return effects[I(pos)];
    }
    PieceMask effectMask(Square pos) const
    {
      return effects[I(pos)].pm;
    }
    /**
     * posに駒を設置/削除して長い利きをブロック/延長する際の利きデータの更新.
     * @param OP(template) - 利きを足すか，減らすか
     * @param state - 局面の状態 posに駒を置く前でも後でもよい
     * @param pos - 変化する位置
     */
    template<NumBitmapEffect::Op OP,bool UC>
    void doBlockAt(const NumEffectState& state,Square pos,int piece_num);
    friend bool operator==(const NumSimpleEffectTable& et1,const NumSimpleEffectTable& et2);
    /*
     *
     */
    const BoardMask changedEffects() const{
      return changed_effects;
    }
    const BoardMask changedSquare() const{
      return changed_square;
    }
    const NumBitmapEffect changedPieces() const {
      return changed_effect_pieces;
    }
    PieceMask effectedMask(Player pl) const {
      return effected_mask[I(pl)];
    }
    PieceMask effectedChanged(Player pl) const {
      return effected_changed_mask[I(pl)];
    }
    void setChangedPieces(PieceMask const& mask) {
      changed_effect_pieces.pm |= mask;
    }
    void clearChangedEffects(){
      changed_effects.clear();
      changed_square.clear();
      resetAll(changed_effect_pieces.pm);
    }
    void clearEffectedChanged(){
      resetAll(effected_changed_mask[0]);
      resetAll(effected_changed_mask[1]);
    }
  };

  inline bool operator!=(const NumSimpleEffectTable& et1,const NumSimpleEffectTable& et2)
  {
    return !(et1==et2);
  }
} // namespace osl


namespace osl
{
  class NumEffectState;
  std::ostream& operator<<(std::ostream& os, NumEffectState const& st);
  void csaShow(std::ostream& os, NumEffectState const& st);
  /**
   * 駒番に依存した局面（インスタンス）比較をする.
   * なお、駒番に非依存な局面比較をしたい場合は、osl::record::CompactBoardや
   * osl::hash::HashKeyを用いる.
   */
  bool operator==(const NumEffectState& st1, const NumEffectState& st2);

  /**
   * 利きを持つ局面
   * - effects (NumSimpleEffectTable) 利き
   * - pieces_onboard (PieceMask) 盤上にある駒
   */
  class NumEffectState
  {
    ALIGN(16)
    std::array<Piece,Square_SIZE> board;
    /**
     * 全てのpieceが登録されている
     */
    ALIGN(16)
    std::array<Piece,Piece_SIZE> pieces;
    std::array<PieceMask,2> stand_mask;
    std::array<std::array<uint8_t,16>,2> pawnPos;
    std::array<std::array<char,PTYPE_BASIC_SIZE>,2> stand_count;

    /** 手番 */
    Player player_to_move;
    PieceMask used_mask;
  public:
#ifdef COPY_STATE
    const NumEffectState *previous;
#endif    
    Piece pieceOf(int num) const{
      return pieces[num];
    }
    void setPieceOf(int num,Piece p) {
      pieces[num]=p;
    }
    Piece kingPiece(Player P) const{
      return pieceOf(kingIndex(P));
    }
    Square kingSquare(Player player) const{
      return square(kingPiece(player));
    }
    /**
     * returns nth piece of unpromote(PTYPE)
     * depends on piece numbers
     */
    Piece nth(Ptype ptype, int n) const {
      assert(0 <= n && n < indexLimit(ptype) - indexMin(ptype));
      return pieceOf(indexMin(ptype)+n);
    }

    void setBoard(Square sq,Piece piece)
    {
      board[I(sq)]=piece;
    }
#if 0
  protected:
    PieceMask& standMask(Player p) {
      return stand_mask[I(p)];
    }
#endif
  public:
    PieceMask standMask(Player p) const {
      return stand_mask[I(p)];
    }
    const PieceMask& usedMask() const {return used_mask;}
    // protected:
    /** (internal) */
    void clearPawn(Player pl,Square sq){
      pawnPos[I(pl)][X(sq)]=uint8_t(Square::STAND);
    }
    /** (internal) */
    void setPawn(Player pl,Square sq){
      pawnPos[I(pl)][X(sq)]=uint8_t(sq);
    }
  public:      
    bool isPawnMaskSet(Player player, int x) const
    {
      return pawnPos[I(player)][x]!=uint8_t(Square::STAND);
    }

    Square pawnSquare(Player c,int x) const{ return Square(pawnPos[I(c)][x]); }

    int pawnY(Player P, int x) const{
      Square sq = pawnSquare(P, x);
      return (sq == Square::STAND ? 0 : Y(sq));
    }
    int pawnY(Player P, Square p) const{
      return pawnY(P, X(p));
    }
    /** xの筋に歩を打てる */
    bool canDropPawnTo(Player player, int x) const
    {
      return hasPieceOnStand(player, PAWN) && ! isPawnMaskSet(player, x);
    }

    void setPiece(Player player,Square sq,Ptype ptype);
    void setPieceAll(Player player);

    /**
     * @param sq は isOnboardを満たす Square の12近傍(8近傍+桂馬の利き)
     * ! isOnBoard(sq) の場合は PIECE_Ptype::EDGE を返す
     */
    Piece pieceAt(Square sq) const { return board[I(sq)];}
    Piece operator[](Square sq) const { return pieceAt(sq);}
    const Piece* getPiecePtr(Square sq) const { return &board[I(sq)];}

    /**
     * 持駒の枚数を数える
     */
    int countPiecesOnStand(Player pl,Ptype ptype) const {
      assert(!isPromoted(ptype));
      return stand_count[I(pl)][ptype-Ptype::BASIC_MIN];
    }
    bool hasPieceOnStand(Player player,Ptype ptype) const{
      return countPiecesOnStand(player, ptype) != 0;
    }
  private:
    int countPiecesOnStandBit(Player pl,Ptype ptype) const {
      return countBit(standMask(pl) & pieceMask(ptype));
    }
  public:
    void setTurn(Player player) {
      player_to_move = player;
    }
    Player turn() const{
      return player_to_move;
    }
    /**
     * 手番を変更する
     */
    void changeTurn() {
      player_to_move = alt(player_to_move);
    }
    /**
     * 合法手かどうかを検査する．
     * isValidMoveByRule, isAlmostValidMove をおこなう．
     * 玉の素抜きや王手を防いでいるか，
     * 千日手，打歩詰かどうかは検査しない．
     */
    bool isValidMove(Move move,bool show_error=true) const;
  protected:
    template <bool show_error> bool isAlmostValidDrop(Move move) const;
    template <bool show_error> bool testValidityOtherThanEffect(Move move) const;
  public:
    /**
     * 盤面以外の部分の反則のチェック
     *
     */
    static bool isValidMoveByRule(Move move,bool show_error);

    /**
     * @param from - square
     * @param to - square
     * checks if the squares between 'from' and 'to' are empties.
     * precondition : if a queen is on 'from' square, it can attack 'to' square
     */
    bool
    isEmptyBetween(Square from, Square to) const{
      assert(osl::isOnBoard(from));
      assert(osl::isOnBoard(to));
      Direction d = Board_Table.getShort8(BLACK, from, to);
      assert(isShort8(d));
      BoardMask mask = Board_Mask_Table_Dir.mask(from, d) & 
	Board_Mask_Table_Dir.mask(to, rotate180Short(d));
      BoardMask mask1 = mask;
      mask1.andNot(piece_mask);
      return mask == mask1;
    }

    NumSimpleEffectTable effects;
    BoardMask piece_mask;
    std::array<PieceMask,2> pieces_onboard;
    /** 成駒一覧 */
    PieceMask promoted;
    std::array<PieceMask,2> pin_or_open;
    KingMobility king_mobility;
    std::array<King8Info, 2> king8infos;

    friend bool operator==(const NumEffectState& st1,const NumEffectState& st2);
    typedef NumEffectState state_t;
  public:
    // ----------------------------------------------------------------------
    // 0. 将棋以外の操作
    // ----------------------------------------------------------------------
    NumEffectState();
    ~NumEffectState();
    /** 盤面が空の状態に初期化 */
    void init();
    void initPawnPos();
    void initEffects();
    /** 局面更新に関する一貫性をテスト */
    void showEffect(std::ostream& os) const;


    // ----------------------------------------------------------------------
    // 1. 盤面全体の情報
    // ----------------------------------------------------------------------
    PieceMask piecesOnBoard(Player p) const { return pieces_onboard[I(p)]; }
    PieceMask piecesOnBoard(Player p, Ptype ptype) const { 
      return piecesOnBoard(p) & pieceMask(ptype);
    }
    PieceMask piecesOnBoardStrict(Player p, Ptype ptype) const { 
      return piecesOnBoard(p, ptype) &
	(isPromoted(ptype) ? promoted : ~promoted);
    }
    PieceMask promotedPieces() const { return promoted; }
    PieceMask pin(Player king) const {
      return pin_or_open[I(king)] & piecesOnBoard(king);
    }
    /** if an attacker piece moves, it may be an open check */
    PieceMask checkShadow(Player attack) const {
      return pin_or_open[I(alt(attack))] & piecesOnBoard(attack);
    }
    PieceMask pinOrOpen(Player king) const { return pin_or_open[I(king)]; }
    bool isPinOrOpen(Player king, int num) const { 
      return test(pinOrOpen(king), num);
    }
    bool isPinOrOpen(Player king, Piece p) const {
      return isPinOrOpen(king, number(p));
    }
    King8Info king8Info(Player king) const { return king8infos[I(king)];  }
    /** P's king is attacked */
    bool inCheck(Player P) const {
      Square king = kingSquare(P);
#ifdef ALLOW_KING_ABSENCE
      if (isPieceStand(king))
	return false;
#endif
      return hasEffect(alt(P), king);
    }
    /** king of th player in turn is attacked */
    bool inCheck() const { return inCheck(turn()); }
    const EffectedNumTable& longEffectNumTable() const {
      return effects.effectedNumTable;
    }

    /** pl からの利きが(1つ以上)ある駒一覧 */
    PieceMask effectedMask(Player pl) const {
      return effects.effected_mask[I(pl)];
    }
    /** 前の指手でeffectedMask(pl)が変化したか.
     * 取られた駒は現在の実装ではリストされないようだ.
     */
    PieceMask effectedChanged(Player pl) const
    {
      return effects.effected_changed_mask[I(pl)];
    }
    PieceMask attackedMask(Player pl) const {
      return effectedMask(alt(pl)) & piecesOnBoard(pl);
    }
    PieceMask attackedNotPawnKing(Player pl) const {
      return attackedMask(pl) & ~(pieceMask(KING) | selectPtype(~promotedPieces(), PAWN));
    }
    const BoardMask changedEffects() const{
      return effects.changedEffects();
    }
    const BoardMask changedSquare() const{
      return effects.changedSquare();
    }
    const NumBitmapEffect changedPieces() const{
      return effects.changedPieces();
    }
    bool longEffectChanged(Ptype ptype) const 
    {
      return any((changedPieces().pm >> 8) & pieceMask(ptype));
    }
    bool effectChanged(Ptype ptype) const 
    {
      return any(changedPieces().pm & pieceMask(ptype));
    }

    // ----------------------------------------------------------------------
    // 2. 駒に関する情報
    // ----------------------------------------------------------------------
    Square mobilityOf(Direction d,int num) const
    {
      return effects.mobilityTable.get(d,num);
    }
    Square mobilityOf(Direction d, Piece p) const 
    {
      return mobilityOf(d, number(p));
    }
    int lanceMobility(Player P, Square pos, int num) const
    {
      Square pos1 = mobilityOf((P==BLACK ? U : D), num);
      return (P == BLACK ? Y(pos) - Y(pos1) : Y(pos1)- Y(pos))+
	(canMoveOn(P, pieceAt(pos1)) ? 0 : -1);
    }
    int rookMobilityVertical(Player P, int num) const {
      Square posU = mobilityOf(U, num);
      Square posD = mobilityOf(D, num);
      return Y(posD) - Y(posU) +
	(canMoveOn(P,pieceAt(posU)) ? 0 : -1)+
	(canMoveOn(P,pieceAt(posD)) ? 0 : -1);
    }
    int rookMobilityHorizontal(Player P, int num) const {
      Square posR = mobilityOf(R, num);
      Square posL = mobilityOf(L, num);
      return X(posL) - X(posR) +
	(canMoveOn(P, pieceAt(posR)) ? 0 : -1)+
	(canMoveOn(P, pieceAt(posL)) ? 0 : -1);
    }
    int bishopMobilityULDR(Player P, int num) const{
      Square posUL=mobilityOf(UL,num);
      Square posDR=mobilityOf(DR,num);
      return Y(posDR)-Y(posUL)-2+
	(canMoveOn(P, pieceAt(posUL)) ? 1 : 0)+
	(canMoveOn(P, pieceAt(posDR)) ? 1 : 0);
    }
    int bishopMobilityURDL(Player P, int num) const{
      const Square posUR=mobilityOf(UR,num);
      const Square posDL=mobilityOf(DL,num);
      return  Y(posDL)-Y(posUR)-2+
	(canMoveOn(P, pieceAt(posUR)) ? 1 : 0)+
	(canMoveOn(P, pieceAt(posDL)) ? 1 : 0);
    }
    Square kingMobilityAbs(Player p, Direction d) const
    {
      return Square(king_mobility[I(p)][I(d)]);
    }
    /** 
     * 玉がd方向にどこまで動けるかを返す
     * @param p 注目する玉のプレイヤ
     * @param d piece からみた向き
     */
    Square kingMobilityOfPlayer(Player p, Direction d) const
    {
      if (p == BLACK)
	d = rotate180Short(d);
      return kingMobilityAbs(p, d);
    }
    /**
     * pinされた駒がPのKingから見てどの方向か?
     * Pから見たdirectionを返す
     */
    Direction pinnedDir(Player P, Piece p) const
    {
      assert(owner(p) == P);
      assert(isPinOrOpen(P, p));
      Square king=kingSquare(P);
      return Board_Table.getShort8(P, square(p), king);
    }
    Direction pinnedDir(Piece p) const
    {
      return pinnedDir(owner(p), p);
    }
    /**
     * pinされた駒pがtoに動けるか?
     * pinに関係がなければtoへ動けるという前提
     */
    bool pinnedCanMoveTo(Player P, Piece p,Square to) const
    {
      assert(owner(p) == P);
      Direction d=pinnedDir(P, p);
      Square from=square(p);
      return primDir(d)==primDir(Board_Table.getShort8Unsafe(P, from, to));
    }
    /**
     * Pのpinされた駒から，そのpinの原因となっている長い利きを持つ駒を得る．
     */
    template<Player P>
    Piece pinAttacker(Piece pinned) const
    {
      assert(owner(pinned) == P);
      assert(isPinOrOpen(P, pinned));
      Direction d=pinnedDir(P, pinned);
      int attacker_num=longEffectNumTable()[number(pinned)][(P==BLACK ? d : rotate180Short(d))];
      return pieceOf(attacker_num);
    }
    Piece pinAttacker(Piece pinned) const
    {
      if (owner(pinned) == BLACK)
	return pinAttacker<BLACK>(pinned);
      else
	return pinAttacker<WHITE>(pinned);
    }
    //
    // make an iterator from a PieceMask
    // 
    struct PM64{
      const NumEffectState* st;
      uint64_t mask;
      PM64(const NumEffectState* st_,uint64_t mask_):st(st_),mask(mask_){}
      PM64 begin() { return *this; }
      PM64 end() { return PM64(0,0); }
      Piece operator*() const{ return st->pieceOf(bsf64(mask)); }
      PM64& operator++(){
	mask&=(mask-1);
	return *this;
      }
      bool operator!=(PM64 const& v){
	return v.mask!=mask;
      }
    };
    PM64 allPiece(PieceMask const& pm) const{
      return PM64(this,V(pm));
    }
    PM64 allPieceStrict(Player pl, Ptype p) const{
      return allPiece(piecesOnBoardStrict(pl, p));
    }
    struct IM64{
      uint64_t mask;
      IM64(uint64_t mask_):mask(mask_){}
      IM64 begin() { return *this; }
      IM64 end() { return IM64(0); }
      int operator*() const{ return bsf64(mask); }
      IM64& operator++(){
	mask &= mask - 1;
	return *this;
      }
      bool operator!=(IM64 const& v){
	return v.mask!=mask;
      }
    };
    IM64 allNum(PieceMask const& pm) const{
      return IM64(V(pm));
    }
    IM64 allNum(Player pl, Ptype pt) const{
      return allNum(piecesOnBoard(pl, pt));
    }
    IM64 allNumStrict(Player pl, Ptype pt) const{
      return allNum(piecesOnBoardStrict(pl, pt));
    }
    struct SM64{
      const NumEffectState* st;
      uint64_t mask;
      SM64(const NumEffectState* st_,PieceMask mask_):st(st_),mask(V(mask_)){}
      SM64 begin() { return *this; }
      SM64 end() { return SM64(0,PieceMask(0)); }
      Square operator*() const{ return square(st->pieceOf(bsf64(mask))); }
      SM64& operator++(){
	mask&=(mask-1);
	return *this;
      }
      bool operator!=(SM64 const& v){
	return v.mask!=mask;
      }
    };
    SM64 allSquare(PieceMask const& mask) const{
      return SM64(this, mask);
    }
    SM64 allSquare(Player pl, Ptype pt) const{
      return allSquare(piecesOnBoard(pl, pt));
    }
    SM64 allSquareStrict(Player pl, Ptype pt) const{
      return allSquare(piecesOnBoardStrict(pl, pt));
    }
    // ----------------------------------------------------------------------
    // 3. あるSquareへの利き
    // ----------------------------------------------------------------------
    /**
     * 利きの数を数える. 
     * targetが盤をはみ出してはいけない
     */
    int countEffect(Player player,Square target) const 
    {
      assert(osl::isOnBoard(target));
      return effects.effectSetAt(target).countEffect(player);
    }
    // ----------------------------------------------------------------------
    // 3.1 集合を返す
    // ----------------------------------------------------------------------
    PieceMask effect(Square target) const 
    {
      return effects.effectMask(target);
    }
    PieceMask effect(Square target, Ptype ptype) const 
    {
      return effect(target) & pieceMask(ptype);
    }
    PieceMask effectStrict(Square target, Ptype ptype) const 
    {
      return effect(target, ptype) & 
	( isPromoted(ptype) ? promoted : ~promoted);
    }
    PieceMask effect(Player pl, Square target) const 
    {
      return effect(target) & piecesOnBoard(pl);
    }
    PieceMask effect(Player pl, Square target, Ptype ptype) const 
    {
      return effect(pl, target) & pieceMask(ptype);
    }
    PieceMask effectStrict(Player pl, Square target, Ptype ptype) const 
    {
      return effect(pl, target, ptype) & 
	( isPromoted(ptype) ? promoted : ~promoted);
    }
    PieceMask effect(Piece p) const 
    {
      return effect(square(p));
    }
    PieceMask effect(Player pl, Piece p) const 
    {
      return effect(pl, square(p));
    }
    PieceMask longEffect(Square target) const 
    {
      return (effect(target) >> 8) & longMask();
    }
    PieceMask longEffect(Player pl, Square target) const 
    {
      return longEffect(target) & piecesOnBoard(pl);
    }
    PieceMask longEffect(Square target,Ptype ptype) const 
    {
      return (effect(target) >> 8) & pieceMask(ptype);
    }
    PieceMask longEffect(Player pl, Square target,Ptype ptype) const 
    {
      return longEffect(target, ptype) & piecesOnBoard(pl);
    }

    // ----------------------------------------------------------------------
    // 3.2 returns bool
    // ----------------------------------------------------------------------
    bool hasEffect(Player player, Square target) const {
      return countEffect(player, target) != 0;
    }
    
    /**
     * player "P" has long effect at position "to" by "ptype" (or promoted(ptype)) pieces.
     */
    bool hasLongEffect(Player P, Square to, Ptype ptype) const {
      return any(longEffect(P, to, ptype));
    }

    /**
     * player "P" has effect at position "to" by "ptype" (or promoted(ptype)) pieces.
     */
    bool hasEffect(Player attack, Square target, Ptype ptype) const{
      return any(effect(attack, target, ptype));
    }
    /**
     * target に ptype の利きがあるか? 成不成を区別
     */
    bool hasEffectStrict(Player attack, Square target, Ptype PTYPE) const{
      return any(effectStrict(attack, target, PTYPE));
    }
    /** 
     * 対象とするマスにあるプレイヤーの(ただしある駒以外)利きがあるかどうか.
     * @param player 攻撃側
     * @param piece 攻撃側の駒
     * @param target 対象のマス
     */
    bool hasEffectNotBy(Player player,Piece piece,Square target) const {
      return any(effect(player, target) & ~newPieceMask(number(piece)));
    }
    /**
     * has effect by a non-king piece that is not pinned 
     * or has effect by pinned piece but the piece can move to 'pos'
     */
    bool hasEnoughEffect(Player P, Square pos) const
    {
      PieceMask pinned = pin(P);
      assert(isOnBoard(pos));
      PieceMask mask = effect(P, pos) & ~pieceMask(KING);
      if(!any(mask)) return false;
      if(any(mask & ~pinned)) return true;
      mask &= pinned;
      do {
	int num = takeOneBit(mask);
	if(pinnedCanMoveTo(P, pieceOf(num), pos)) return true;
      } while(any(mask));
      return false;
    }
    /**
     * if alt(P) drops a piece on pos, P can take back the piece.
     */
    bool canTakeBack(Player P, Square pos) const
    {
      if(!hasEffect(P, pos)) return false;
      if(hasEnoughEffect(P, pos)) return true;
      return hasEffect(P, pos, KING) && !hasEffect(alt(P), pos);
    }

    /**
     * pinされている駒以外からの利きがある.
     */
    bool hasEffectByNotPinned(Player pl,Square target) const{
      assert(osl::isOnBoard(target));
      return any(effect(pl, target) & ~pinOrOpen(pl));
    }
    /** 
     * 二つ以上の駒から利きがある.
     * そもそも利きがない場合も呼ばれる
     * @param player 攻撃側
     * @param target 対象のマス
     */
    bool hasMultipleEffectAt(Player player,Square target) const 
    {
      return countEffect(player, target) > 1;
    }

    /** 
     * 駒attack が target に利きを持つか (旧hasEffectToと統合)
     * @param target 対象のマス
     */
    bool hasEffectByPiece(Piece attack, Square target) const 
    {
      assert(isPiece(attack));
      assert(osl::isOnBoard(target));
      return test(effect(target), number(attack));
    }
    bool hasLongEffectByPiece(Piece attack, Square target) const 
    {
      assert(isPiece(attack));
      assert(osl::isOnBoard(target));
      return test(longEffect(target), number(attack));
    }


    /**
     * attackerにptypeoの駒がいると仮定した場合にtargetに利きがあるかどうか
     * を stateをupdateしないで確かめる.
     * targetSquareは空白でも良い
     * 盤上の駒を動かす事を検討しているときはに，
     * 自分自身の陰に入って利かないと見なされることがある
     */
    bool hasEffectIf(PtypeO ptypeo,Square attacker,
		     Square target) const
    {
      EffectContent e = Ptype_Table.getEffect(ptypeo,attacker,target);
      if (! osl::hasEffect(e)) 
	return false;
      if (hasUnblockableEffect(e))
	return true;
      assert(Board_Table.getShortOffset(attacker,target) == offset(e));
      return this->isEmptyBetween(attacker,target);
    }
    bool hasAdditionalEffect(Player attack, Square target) const{
      PieceMask direct = effect(attack, target) & ~(~promotedPieces() & pieceMask(KNIGHT));
      while (any(direct)) {
	const int num = takeOneBit(direct);
	const Square pos = square(pieceOf(num));
	const Direction d = Board_Table.getShort8(BLACK, pos, target);
	const int num1 = longEffectNumTable()[num][d];
	if(!isEmptyNum(num1) && owner(pieceOf(num1)) == attack) return true;
      }
      return false;
    }

    // ----------------------------------------------------------------------
    // 3.3 pieceを探す
    // ----------------------------------------------------------------------
    /** return a piece s.t. owner == attack, ptype == PTYPE, has effect on target.  return Piece_EMPTY otherwise */
    /** 
     * pieceのd方向から長い利きがある場合にその駒を返す。
     * @param d piece からみた向き
     */
    Piece findLongAttackAt(Player owner, int piece, Direction d) const
    {
      assert(isOnBoardByOwner(owner,pieceOf(piece)));
      if (owner == BLACK)
	d = rotate180Short(d);
      const int num = effects.effectedNumTable[piece][d];
      if (num == EMPTY_NUM)
	return Piece_EMPTY;
      return pieceOf(num);
    }
    Piece findLongAttackAt(Player owner, Piece piece, Direction d) const
    {
      assert(isPiece(piece));
      assert(osl::owner(piece) == owner);
      return findLongAttackAt(owner, number(piece), d);
    }
    /**
     * 利きの中から安そうな駒を選ぶ
     */
    Piece selectCheapPiece(PieceMask effect) const;
    /**
     * @param P - 利きをつけている側のプレイヤ
     * @param square - 調査する場所
     * @return 利きを付けている中で安そうな駒 (複数の場合でもEMPTYにはしない)
     */
    Piece findCheapAttack(Player P, Square square) const 
    {
      return selectCheapPiece(effect(P, square));
    }
    /**
     * @param P - 利きをつけている側のプレイヤ
     * @param square - 調査する場所
     * @return 利きを付けている中で安そうな駒 (複数の場合でもEMPTYにはしない)
     */
    Piece findCheapAttackNotBy(Player P, Square square, const PieceMask& ignore) const 
    {
      return selectCheapPiece(effect(P, square) & ~ignore);
    }
    Piece findAttackNotBy(Player P, Square square, const PieceMask& ignore) const 
    {
      PieceMask pm = effect(P, square) & ~ignore;
      return (any(pm) ? pieceOf(bsf(pm)) : Piece_EMPTY);
    }
    /**
     * 王手駒を探す
     * @return 王手かどうか
     * @param attack_piece
     * 一つの駒による王手の場合はattck_pieceにその駒を入れる
     * 複数の駒による王手の場合はPiece_EMPTYを入れる
     * @param P(template) 玉
     */
    bool findCheckPiece(Player P, Piece& attack_piece) const
    {
      return hasEffect(alt(P), kingSquare(P), attack_piece);
    }
    bool hasEffect(Player P, Square target,Piece& attackerPiece) const 
    {
      int c=countEffect(P,target);
      if(c==0) return false;
      attackerPiece = (c == 1 ? pieceOf(bsf(effect(P,target))) : Piece_EMPTY);
      return true;
    }
    /**
     * can declare win
     */
    bool canDeclareWin() const{
      const Player Turn = turn();
      const Square myKingSquare = kingSquare(Turn);
      // not in check
      if ( hasEffect(alt(Turn), myKingSquare) )
	return false;
      // check if my king is in the opponent's camp
      // Y(pos) = 1 - 3 for BLACK
      // Y(pos) = 7 - 9 for WHITE
      const int y = Y(myKingSquare);
      const int enemyCampMin = (Turn==BLACK) ? 1 : 7;
      const int enemyCampMax = enemyCampMin + 2;

      if( (y < enemyCampMin) || (y > enemyCampMax) )
	return false;
      // check if there are not less than ten pieces of mine in the ooponent's camp
      // count the score of pieces (pieces in the opponent's camp and in piece stand)
      // five for major pieces
      // 28 point is needed for BLACK, 27 point is needed for WHITE
      int countPiece = 0;
      int onEnemyCamp = -1;
      for (int i = enemyCampMin; i <= enemyCampMax; i++)
	for (int j=1; j<=9; j++){
	  Piece pieceOnEnemyCamp = pieceAt(newSquare(j, i));
	  if (isOnBoardByOwner(Turn,pieceOnEnemyCamp)) {
	    ++countPiece;
	    onEnemyCamp += 1 + 4 * isMajor(ptype(pieceOnEnemyCamp));
	  }
	}
      if (countPiece < 11)
	return false;
      int onStand =
	5 * countPiecesOnStand( Turn, ROOK)
	+ 5 * countPiecesOnStand( Turn, BISHOP)
	+ countPiecesOnStand( Turn, GOLD)
	+ countPiecesOnStand( Turn, SILVER)
	+ countPiecesOnStand( Turn, KNIGHT)
	+ countPiecesOnStand( Turn, LANCE)
	+ countPiecesOnStand( Turn, PAWN);
      if ( onEnemyCamp + onStand < 27 + (Turn==BLACK) )
	return false;
      return true;
    }

    // ----------------------------------------------------------------------
    // 4. 指手
    // ----------------------------------------------------------------------
    /**
     * 合法手かどうかを簡単に検査する．局面に依存するチェックのみ．
     * ルール上指せない手である可能性がある場合は，isValidMove を用いる．
     *
     * Pをtemplate引数にできると少しは速くなりそう
     * 局面に依存する検査でも，玉の素抜きや王手を防いでいるか，
     * 千日手，打歩詰かどうかは検査しない．
     * @param showError(template) - falseを返す時には何か表示する.
     * @param move - 手
     */
    template <bool show_error>
    bool isAlmostValidMove(Move move) const;
    bool isAlmostValidMove(Move move,bool show_error=true) const;
    void makeNormalMove(Move move);
    void makeMovePass()
    {
      changeTurn();
      effects.clearChangedEffects();
      effects.clearEffectedChanged();
    }
    void makeMove(Move move){
      if (isPass(move)) makeMovePass();
      else makeNormalMove(move);
    }

  private:
    void makeSimpleMove(Player P,Square from, Square to, bool is_promotion);
    void makeDropMove(Player P,Square to, Ptype ptype);
    void makeCaptureMove(Player P,Square from, Square to, Piece target, 
			 bool is_promotion);

    // 
    void makePinOpenDir(Direction DIR, Square target,
			PieceMask& pins, PieceMask const& onBoard,Player defense)
    {
      const Offset offset = blackOffset(DIR);
      Square sq=target-offset;
      int num;
      while(isEmptyNum(num=number(pieceAt(sq))))
	sq-=offset;
      king_mobility[I(defense)][I(DIR)]=static_cast<unsigned char>(I(sq));
      if(isEdgeNum(num)) return;
      int num1=longEffectNumTable()[num][DIR];
      if(isPieceNum(num1) && test(onBoard, num1)){
	set(pins, num);
      }
    }
    void recalcPinOpen(Square changed, Direction &lastDir, Player defense)
    {
      Square target=kingSquare(defense);
#ifdef ALLOW_KING_ABSENCE
      if (isPieceStand(target))
	return;
#endif
      const Direction longD=BoardTable::getLongDirection(BLACK,changed,target);
      if(!isLong(longD) || (lastDir!=UL && longD==lastDir)) return;
      lastDir=longD;
      Direction shortD=longToShort(longD);
      {
	// reset old pins
	Square oldPos=Square(king_mobility[I(defense)][I(shortD)]);
	int oldNum=number(pieceAt(oldPos));
	if(isPieceNum(oldNum))
	  reset(pin_or_open[I(defense)], oldNum);
      }
      const Offset offset = blackOffset(longD);
      Square sq=target-offset;
      int num;
      while(isEmptyNum(num=number(pieceAt(sq))))
	sq-=offset;
      king_mobility[I(defense)][I(shortD)]=static_cast<unsigned char>(I(sq));
      if(isEdgeNum(num)) return;
      int num1=longEffectNumTable()[num][shortD];
      if(isPieceNum(num1) && test(piecesOnBoard(alt(defense)), num1)){
	set(pin_or_open[I(defense)], num);
      }
    }
    void makePinOpen(Player defense);
    void makeKing8Info(Player P);
    void updateKing8Info(std::array<PieceMask, 2> const& pin_or_open_backup);
  public:
    int see(Move move) const;
  };

  inline bool operator!=(const NumEffectState& s1, const NumEffectState& s2)
  {
    return !(s1==s2);
  }  
  std::ostream& operator<<(std::ostream& os,const NumEffectState& state);
} // namespace osl
namespace osl
{
  /**
   * information of eight neigbors of king
   * valid whether the king is attacked or not
   * (1 << Dir) is square ( king + newOffset(Defense, Dir) )
   * 0-7 : dropCandidate,  square is empty, defender has no effect and attacker has effect
   *       (candidate squares for drop check)
   * 8-15 : liberty, king can move to the square (valid even if the king is attacked)
   * 16-23 : empty or attacker's piece
   * 24-31 : empty or defender's piece, attacker has effect and defender has no effect other than by king
   * 32-39 : empty
   * 40-47 : empty or defender's piece, attacker(including king) has effect 
   * 2014/2/10 に以下を削除
   * ( 48-51 : bit count of liberty ( not used now ) )
   */
  enum class King8Info : uint64_t {};

  constexpr uint64_t V(King8Info const& ki){ return uint64_t(ki); }

  /** Defense のking について計算 */
  inline King8Info newKing8Info(Player Defense, NumEffectState const& state){
    Square king = state.kingSquare(Defense);
//    PieceMask pinned = state.pin(Defense);
    uint64_t canMoveMask=0ull;
    const Player Attack = alt(Defense);
    for(Direction Dir : SHORT8_DIRECTIONS){
      Square pos = king + newOffset(Defense, Dir);
      Piece p = state[pos];
      if(isEdge(p)) continue;
      if(!state.hasEffect(Attack, pos)){
	if(canMoveOn(Defense, p)){ // attacker's piece or empty
	  canMoveMask += (isEmpty(p) ? 0x100010100ull : 0x10100ull) << V(Dir);
	}
	continue;
      }
      const bool has_enough_effect = state.hasEnoughEffect(Defense, pos);
      if(has_enough_effect){
	if(canMoveOn(Defense, p)) // defense's piece or empty
	  canMoveMask += (isEmpty(p) ? 0x10100010000ull : 0x10000ull) << V(Dir);
	else
	  canMoveMask += 0x10000000000ull<<V(Dir);
      }
      else{
	if(isEmpty(p))
	  canMoveMask +=  0x10101010001ull << V(Dir);
	else if(isOnBoardByOwner(Attack,p))
	  canMoveMask += 0x10000ull << V(Dir);
	else
	  canMoveMask += 0x10001000000ull << V(Dir);
      }
    }
    for(Square attacker_sq : state.allSquare(state.longEffect(alt(Defense), king))){
      Direction d = Board_Table.getShort8(Defense, attacker_sq, king);
      canMoveMask &= ~(0x100 << d); // long effect can reach to 'd', reset liverty
    }
    return King8Info(canMoveMask);
  }
  constexpr uint8_t dropCandidate(King8Info const& ki){ 
    return uint8_t(V(ki) & 0xffull); 
  }
  constexpr uint8_t liberty(King8Info const& ki){
    return uint8_t((V(ki) >> 8) & 0xffull);
  }
  /** 0-15bit */
  constexpr uint16_t libertyDropMask(King8Info const& ki){
    return uint16_t(V(ki) & 0xffffull);
  }
  constexpr uint8_t moveCandidate2(King8Info const& ki){
    return uint8_t((V(ki) >> 24) & 0xffull);
  }
  constexpr uint8_t spaces(King8Info const& ki){
    return uint8_t((V(ki) >> 32) & 0xffull);
  }
  inline bool
  hasMoveCandidateDir(King8Info const& ki, Player P, Direction Dir, NumEffectState const& state,Square target){
    if((V(ki) & (1ull<<(24+int(Dir))))==0) return false;
    Square pos=target-newOffset(P,Dir);
    if(state.countEffect(P,pos)<2 &&
       !state.hasAdditionalEffect(P, pos)) return false;
    return true;
  }

  inline bool
  hasMoveCandidate(King8Info const& ki, Player P, NumEffectState const& state){
    Player altP = alt(P);
    Square king = state.kingSquare(altP);
    for(Direction d : SHORT8_DIRECTIONS)
      if(hasMoveCandidateDir(ki, P, d, state,king) != 0) return true;
    return false;
  }

  std::ostream& operator<<(std::ostream&, King8Info);
} // namespace osl

namespace osl{
  inline int diffWithMove(const NumEffectState&, Move move)
  {
    int ret = 0;
    if (capturePtype(move) != Ptype::EMPTY)
      ret += captureV[I(capturePtypeO(move))];
    if (isPromotion(move))
      ret += promoteV[I(ptypeO(move))];
    return ret;
  }
}

#endif // OSL_POSITION_H
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
