#ifndef OSL_TYPES_H
#define OSL_TYPES_H
#include "config.h"
#include <type_traits>
using std::enable_if;
#include <cstdint>
#include <iterator>
#include <array>
#include <vector>
#include <memory>
#include <cassert>
#include <cstddef>
#include <algorithm>
#include <iostream>

namespace std{
  template<typename T1,typename T2>
  std::ostream& operator<<(std::ostream& os, std::pair<T1, T2> p){
    return os << "<" << p.first << "," << p.second << ">";
  }
}

#ifdef OSL_USE_SSE41
#  include <smmintrin.h>
#endif

#ifdef OSL_USE_SSE
#include <emmintrin.h>
#endif

#if defined(_MSC_VER)
#define ALIGN(n) __declspec(align(n))
#elif defined __GNUC__
#define ALIGN(n) __attribute__((aligned(n)))
#else
#define ALIGN(n)
#endif

/* enum types */
namespace osl{
  enum class Player : int8_t;
  enum class Ptype : uint8_t;
  enum class PtypeO : int32_t;
  enum class Direction : uint8_t;
#if 0
  enum class Progress16 : int32_t;
#endif
  enum class Offset : int16_t;
  enum class Offset32 : int32_t;
  enum class EffectContent : int32_t;
#ifdef __INTEL_COMPILER
  enum class Square : uint32_t;
#else
  enum class Square : uint16_t;
#endif
  enum class Piece : int32_t;
  enum class Move16 : int16_t;
  enum class Move : int32_t;
  enum class MoveMask : int32_t;
  enum class PieceMask : uint64_t;
  enum class King8Info : uint64_t;

#define ENABLE_OPERATORS_ON(T,DT,IT)					\
  constexpr T operator+ (const T d, DT i) { return T(IT(d) + IT(i)); }	\
  constexpr DT operator- (const T d1, const T d2) { return DT(IT(d1) - IT(d2)); } \
  constexpr T operator- (const T d1, const DT d2) { return T(IT(d1) - IT(d2)); } \
  constexpr T operator* (int i, const T d) {  return T(i * IT(d)); }	\
  constexpr T operator* (const T d, IT i) {  return T(IT(d) * i); }	\
  constexpr T operator& (const T d1, const T d2) { return T(IT(d1) & IT(d2)); } \
  constexpr T operator| (const T d1, const T d2) { return T(IT(d1) | IT(d2)); } \
  constexpr T operator^ (const T d1, const T d2) { return T(IT(d1) ^ IT(d2)); } \
  constexpr int operator<<(int i,const T d){ return i<<IT(d); }		\
  constexpr int operator<<(const T d,int i){ return IT(d)<<i; }		\
  constexpr T operator>>(const T d,int i){ return T(IT(d)>>i); }	\
  constexpr T operator/ (const T d, int i) { return T(IT(d) / i); }	\
  constexpr T operator- (const T d) { return T(-IT(d)); }		\
  constexpr bool operator<(const T d1, const T d2) {return IT(d1)<IT(d2);} \
  constexpr bool operator<=(const T d1, const T d2) {return IT(d1)<=IT(d2);} \
  inline T operator++ (T& d) {d = T(IT(d) + 1); return d; }		\
  inline T operator-- (T& d) { d = T(IT(d) - 1); return d; }		\
  inline T operator++ (T& d, int) {T old_d(d); d = T(IT(d) + 1); return old_d; } \
  inline T operator-- (T& d, int) {T old_d(d);  d = T(IT(d) - 1); return old_d; } \
  inline void operator+= (T& d1, const DT d2) { d1 = d1 + d2; }		\
  inline void operator-= (T& d1, const DT d2) { d1 = d1 - d2; }		\
  inline void operator|= (T& d1, const T d2) { d1 = d1 | d2; }		\
  inline void operator&= (T& d1, const T d2) { d1 = d1 & d2; }		\
  inline void operator^= (T& d1, const T d2) { d1 = d1 ^ d2; }		\
  inline void operator*= (T& d, int i) { d = T(IT(d) * i); }		\
  inline void operator/= (T& d, int i) { d = T(IT(d) / i); }

#define ENABLE_OPERATORS_ON_2(T,IT)					\
  constexpr T operator+ (const T d1, const T d2) { return T(IT(d1) + IT(d2)); }	\
  constexpr T operator- (const T d1, const T d2) { return T(IT(d1) - IT(d2)); } \
  constexpr T operator& (const T d1, const T d2) { return T(IT(d1) & IT(d2)); } \
  constexpr T operator| (const T d1, const T d2) { return T(IT(d1) | IT(d2)); } \
  constexpr T operator^ (const T d1, const T d2) { return T(IT(d1) ^ IT(d2)); } \
  constexpr T operator<<(const T d,int i){ return T(IT(d)<<i); }	\
  constexpr int operator<<(int i,const T d){ return i<<IT(d); }		\
  constexpr T operator>>(const T d,int i){ return T(IT(d)>>i); }	\
  constexpr T operator- (const T d) { return T(-IT(d)); }		\
  constexpr T operator~ (const T d) { return T(~IT(d)); }		\
  constexpr bool operator<(const T d1, const T d2) {return IT(d1)<IT(d2);} \
  constexpr bool operator<=(const T d1, const T d2) {return IT(d1)<=IT(d2);} \
  inline T operator++ (T& d) {d = T(IT(d) + 1); return d; }		\
  inline T operator-- (T& d) { d = T(IT(d) - 1); return d; }		\
  inline T operator++ (T& d, int) {T old_d(d); d = T(IT(d) + 1); return old_d; } \
  inline T operator-- (T& d, int) {T old_d(d);  d = T(IT(d) - 1); return old_d; } \
  inline void operator+= (T& d1, const T d2) { d1 = d1 + d2; }		\
  inline void operator-= (T& d1, const T d2) { d1 = d1 - d2; }		\
  inline void operator|= (T& d1, const T d2) { d1 = d1 | d2; }		\
  inline void operator&= (T& d1, const T d2) { d1 = d1 & d2; }		\
  inline void operator^= (T& d1, const T d2) { d1 = d1 ^ d2; } 

  ENABLE_OPERATORS_ON(Ptype,int,int)
  ENABLE_OPERATORS_ON(PtypeO,int,int)
  ENABLE_OPERATORS_ON_2(Direction,int)
#if 0
  ENABLE_OPERATORS_ON(Progress16,int, int)
#endif
  ENABLE_OPERATORS_ON(Offset,int, int)
  ENABLE_OPERATORS_ON(Offset32,int, int)
  ENABLE_OPERATORS_ON(Square,Offset, int)
  ENABLE_OPERATORS_ON(Piece,Offset, int)
  ENABLE_OPERATORS_ON_2(PieceMask, uint64_t)
  ENABLE_OPERATORS_ON_2(King8Info, uint64_t)
  ENABLE_OPERATORS_ON(MoveMask,int, int)
#undef ENABLE_OPERATORS_ON

  /* Player */

  enum class Player : int8_t {};
  constexpr Player BLACK=Player(0);
  constexpr Player WHITE=Player(-1);
  constexpr Player COLORS[2]={BLACK,WHITE};
  constexpr Player alt(Player player){ return Player(-1-int(player)); }
  constexpr int I(Player player){ return -int(player); }
  /** BLACK -> +1, WHITE -> -1 */
  constexpr int sign(Player player){ return 1+(int(player)<<1); }
  /**
   * validation check
   */
  constexpr bool isValid(Player player){
    return player==BLACK || player==WHITE; 
  }
  template<typename T>
  constexpr T valueFor(Player player, T v){
    return (player == BLACK ? v : -v);
  }

  std::ostream& operator<<(std::ostream& os,Player player);

  /* Direction */

  enum class Direction : uint8_t{
    MIN = 0, MAX = 24,
      SHORT8_MAX = 7, SHORT__MAX = 9,
      LONG__MIN = 16, LONG__MAX = 23,
      INVALID_VALUE = 24,
      };
  constexpr Direction UL=Direction(0);
  constexpr Direction U=Direction(1);
  constexpr Direction UR=Direction(2);
  constexpr Direction L=Direction(3);
  constexpr Direction DR=Direction(4);
  constexpr Direction D=Direction(5);
  constexpr Direction DL=Direction(6);
  constexpr Direction R=Direction(7);
  constexpr Direction UUL=Direction(8);
  constexpr Direction UUR=Direction(9);
  constexpr Direction DDR=Direction(8+4);
  constexpr Direction DDL=Direction(9+4);
  constexpr Direction LONG_UL=Direction(16);
  constexpr Direction LONG_U=Direction(17);
  constexpr Direction LONG_UR=Direction(18);
  constexpr Direction LONG_L=Direction(19);
  constexpr Direction LONG_DR=Direction(20);
  constexpr Direction LONG_D=Direction(21);
  constexpr Direction LONG_DL=Direction(22);
  constexpr Direction LONG_R=Direction(23);
  constexpr int SHORT8_DIRECTION_SIZE=8;
  constexpr int SHORT_DIRECTION_SIZE=10;
  constexpr int DIRECTION_SIZE=24;
  
  constexpr int V(Direction d){ return int(d); }

  constexpr int I(Direction d){ return int(d); }

  constexpr bool isShort(Direction d){
    return d <= Direction::SHORT__MAX; 
  }

  constexpr bool isShort8(Direction d){ return d <= Direction::SHORT8_MAX; }

  constexpr bool isLong(Direction d){ return d >= Direction::LONG__MIN; }

  constexpr Direction rotate180Short(Direction d){
    return Direction(V(d) ^ 4);
  }

  constexpr Direction rotate180Long(Direction d){
    return Direction(V(d) ^ 4);
  }

  /**
   * returns primitive ( 0 - 3 ) directions for short ( 0 - 7 ) directions
   */
  constexpr Direction primDir(Direction d){
    return Direction(V(d) & 0xb);
  }

  constexpr bool isValid(Direction d){
    return Direction::MIN <= d && d <= Direction::MAX;
  }
  
  constexpr Direction longToShort(Direction d){ return d - LONG_UL; }
  
  constexpr Direction shortToLong(Direction d){ return d + LONG_UL; }

  constexpr int M(Direction dir){ return (1 << V(dir)); }
  
  constexpr bool canPromoteTo[DIRECTION_SIZE]={
    true,true,true,false,false,false,false,false,
    true,true,false,false,false,false,false,false,
    true,true,true,false,false,false,false,false
  };
  constexpr bool canPromoteDir(Direction d){
    return V(d) < 3;
  }
  constexpr Direction PRIM_DIRECTIONS[4]={UL, U, UR, L};
  constexpr Direction PRIM_DIRECTIONS1[4]={UL, UR, U, L};
  constexpr Direction DIRECTIONS_UUL_UUR[2]={UUL, UUR};
  constexpr Direction DIRECTIONS[18]={
    UL, U, UR, L, R, DL, D, DR, UUL, UUR, 
    LONG_UL, LONG_U, LONG_UR, LONG_L, LONG_R, LONG_DL, LONG_D, LONG_DR
  };
  constexpr Direction UP_DIRECTIONS[8]={
    UL, U, UR, UUL, UUR, LONG_UL, LONG_U, LONG_UR
  };
  constexpr Direction SHORT_DIRECTIONS[10]={
    UL, U, UR, L, R, DL, D, DR, UUL, UUR
  };
  constexpr Direction SHORT8_DIRECTIONS[8]={
    UL, U, UR, L, R, DL, D, DR
  };
  constexpr Direction SHORT8_DIRECTIONS_NOT_U[7]={
    UL, UR, L, R, D, DL, DR
  };
  constexpr Direction GOLD_DIRECTIONS[6]={
    U, UL, UR, L, R, D
  };
  constexpr Direction SILVER_DIRECTIONS[5]={DL, DR, U, UL, UR};
  constexpr Direction BISHOP_DIRECTIONS[4]={DL, DR, UL, UR};
  constexpr Direction ROOK_DIRECTIONS[4]={U, L, R, D};
  constexpr Direction DIRECTIONS_DL_DR[2]={DL, DR};
  constexpr Direction DIRECTIONS_L_R[2]={L, R};
  constexpr Direction LONG_DIRECTIONS[8]={
    LONG_UL, LONG_U, LONG_UR, LONG_L, LONG_R, LONG_DL, LONG_D, LONG_DR
  };
  std::ostream& operator<<(std::ostream& os,const Direction d);

  /* Offset */
  enum class Offset : int16_t{
    MIN=-0x100,
      ONBOARD_MIN=-0x88,
      ZERO=0,
      ONBOARD_MAX=0x88
      };
  constexpr int BOARD_HEIGHT=16;
  constexpr int OFFSET_ONBOARD_SIZE=0x88*2+1;
  constexpr int I(Offset o){ return o - Offset::MIN; }
  constexpr int BI(Offset o){ return o - Offset::ONBOARD_MIN; }
  constexpr Offset newOffset(int dx, int dy){
    return Offset(dx*BOARD_HEIGHT + dy); 
  }
  constexpr int blackDxs[DIRECTION_SIZE]={
    1, 0, -1, 1, -1, 0, 1, -1,
    1, -1, 0, 0, -1, 1, 0, 0,
    1, 0, -1, 1, -1, 0, 1, -1,
  };
  constexpr int blackDys[DIRECTION_SIZE]={
    -1, -1, -1, 0, 1, 1, 1, 0,
    -2, -2,  0, 0, 2, 2, 0, 0,
    -1, -1, -1, 0, 1, 1, 1, 0,
  };
  constexpr Offset blackOffsets[DIRECTION_SIZE]={
    newOffset(1, -1), newOffset(0, -1), newOffset(-1, -1), newOffset(1, 0), 
    newOffset(-1, 1), newOffset(0, 1), newOffset(1, 1), newOffset(-1, 0),
    newOffset(1, -2), newOffset(-1, -2), Offset::ZERO, Offset::ZERO,
    newOffset(-1, 2), newOffset(1, 2), Offset::ZERO, Offset::ZERO,
    newOffset(1, -1), newOffset(0, -1), newOffset(-1, -1), newOffset(1, 0), 
    newOffset(-1, 1), newOffset(0, 1), newOffset(1, 1), newOffset(-1, 0),
  };
  constexpr Offset blackOffset(Direction Dir) { 
    return blackOffsets[I(Dir)];
  }
  constexpr Offset newOffset(Player c,Direction Dir) { 
    return (c==BLACK ? blackOffset(Dir) : -blackOffset(Dir)); 
  }

  std::ostream& operator<<(std::ostream&, Offset);

/* square.h */

  /**
   * Squares.
   *        can be used as a index
   * X, Y range 1-9
   * X - from right to left
   * Y - from top to bottom
   * PieceStand -> 0
   * <pre>
   * (A0)  ......................... (00)
   * (A1)  ......................... (01)
   * (A2) 92 82 72 62 52 42 32 22 12 (02)
   * (A3) 93 83 73 63 53 43 33 23 13 (03)
   * (A4) 94 84 74 64 54 44 34 24 14 (04)
   * (A5) 95 85 75 65 55 45 35 25 15 (05)
   * (A6) 96 86 76 66 56 46 36 26 16 (06)
   * (A7) 97 87 77 67 57 47 37 27 17 (07)
   * (A8) 98 88 78 68 58 48 38 28 18 (08)
   * (A9) 99 89 79 69 59 49 39 29 19 (09)
   * (AA) 9A 8A 7A 6A 5A 4A 3A 2A 1A (0A)
   * (AB) ...........................(0B)
   * (AC) ...........................(0C)
   * (AD) ...........................(0D)
   * (AE) ...........................(0E)
   * (AF) ...........................(0F) 
   * </pre>
   */
#ifdef __INTEL_COMPILER
  enum class Square : uint32_t { STAND=0 };
#else
  enum class Square : uint16_t { STAND=0 };
#endif

  constexpr int I(Square sq){ return int(sq); }
  constexpr int V(Square sq){ return int(sq); }
  constexpr Square newSquare(int x,int y){
    return Square((x * BOARD_HEIGHT) + y + 1);
  }
  constexpr int Square_SIZE = V(newSquare(10,11)) + 1;
  inline int X(Square sq){ return I(sq)>>4; }
  inline int Y(Square sq){ return (I(sq) & 0xf) - 1; }
  /**
   * checks if 1<=X(sq) && X(sq)<=9 && 1<=Y(sq) && y(sq)<=9
   * if sq is not a edge, use !isPieceStand(sa)
   */
  inline bool isOnBoard(Square sq){ 
#if 1
    return (0xffffff88u & uint32_t(V(sq) - 0x12) &
	    (uint32_t((V(sq) & 0x77) ^ 0x12) +  0xffffff77u)) == 0;
#else
    return 1 <= X(sq) && X(sq) <= 9 && 1 <= Y(sq) && Y(sq) <= 9;
#endif
  }
  /**
   * sq is between ONBOARD_MIN and ONBOARD_MAX
   * can use before sq
   */
  inline bool isOnBoardRegion(Square sq){ 
    return uint32_t(sq - newSquare(1,1)) <= 
      uint32_t(newSquare(9,9) - newSquare(1,1));
  }
  inline bool isPieceStand(Square sq){ 
    return sq == Square::STAND; 
  }
  inline bool isValid(Square sq){
    return isPieceStand(sq) || isOnBoard(sq);
  }
  inline int reverseX(int x) { return 10-x; }
  inline int reverseY(int y) { return 10-y; }
  /**
   * checkes sq is a edge 
   * sq must be one of eight neighbors of a onBoard square
   */
  inline bool isEdge(Square sq){ 
    assert(!isPieceStand(sq) && 0 <= X(sq) && X(sq) <= 10 && 0 <= Y(sq) && Y(sq) <= 10);
    return (0x88 & (int(sq) - 0x12) & ((int(sq) & 0x11) + 0xf7)) != 0;
  }
  inline Square rotate180(Square sq){
    return Square(int(newSquare(9, 9)) + int(newSquare(1, 1)) - int(sq));
  }
  inline Square rotate180Safe(Square sq){
    return (isPieceStand(sq) ? sq : rotate180(sq));
  }
  inline Square flipHorizontal(Square sq){
    if (isPieceStand(sq)) return sq;
    return newSquare(10 - (int(sq) >> 4), (int(sq) & 0xf) - 1);
  }
  inline Square forBlack(Player player,Square sq){
    return (player == BLACK ? sq : rotate180(sq));
  }
  inline int yForBlack(Player P, Square sq){
    return Y(forBlack(P, sq));
  }
  inline int yForBlack(Player P, int y){
    return (P == BLACK ? y : reverseY(y));
  }
  inline bool canPromoteY(Player P, int y) { 
    return yForBlack(P, y) <= 3;
  }
  inline bool canPromote(Player P, Square sq){
    return yForBlack(P, sq) <= 3;

  }
  /**
   * X(sq1) == X(sq2) || Y(sq1) == Y(sq2)
   * sq1 and sq2 must be on board
   */
  inline bool isULRD(Square sq1, Square sq2){
    assert(isOnBoard(sq1) && isOnBoard(sq2));
    int v = int(sq1) ^ int(sq2);
    return (((v + 0xefu) ^ v) & 0x110u) != 0x110u;
  }
  /**
   * X(sq1) == X(sq2)
   * sq1 and sq2 must be on board
   */
  inline bool isUD(Square sq1, Square sq2){
    assert(isOnBoard(sq1) && isOnBoard(sq2));
    int v = I(sq1) ^ I(sq2);
    return (v & 0xf0) == 0;
  }
  /**
   * X(sq1) == X(sq2) and Y(sq1) >= Y(sq2) if P == BLACK
   * X(sq1) == X(sq2) and Y(sq1) <= Y(sq2) if P == WHITE
   * sq1 and sq2 must be on board
   */
  inline bool isU(Player P,Square sq1,Square sq2){
    assert(isOnBoard(sq1) && isOnBoard(sq2));
    int v=I(sq1) ^ I(sq2);
    return (P == BLACK ? 
	    ((v | (I(sq1) - I(sq2))) & 0xf0) == 0 :
	    ((v | (I(sq2) - I(sq1))) & 0xf0) == 0);
  }
  /**
   * Y(sq1) == Y(sq2)
   * sq1 and sq2 must be on board
   */
  inline bool isLR(Square sq1,Square sq2){
    assert(isOnBoard(sq1) && isOnBoard(sq2));
    int v=I(sq1) ^ I(sq2);
    return (v & 0xf) == 0;
  }
  inline bool yGe7(Square sq) { return (int(sq) & 0x8) != 0;  }
  inline Square nextSquare(Player P, Square sq, Direction Dir){
    return sq + newOffset(P, Dir);
  }
  inline Square back(Player P, Direction Dir, Square sq){
    return sq - newOffset(P, Dir);
  }
  std::ostream& operator<<(std::ostream&, Square);

  /** offset32 */

  enum class Offset32 : int32_t { MIN = -(8*32+8), MAX = (8*32+8)  };
  constexpr int Offset32_SIZE = (Offset32::MAX - Offset32::MIN) + 1;
  constexpr int I(Offset32 o){ return o-Offset32::MIN; }
  inline Offset32 newOffset32(Square to, Square from){
    return Offset32((int(to) + (int(to) & 0xf0)) - (int(from) + (int(from) & 0xf0)));
  }
  constexpr Offset32 newOffset32(int dx, int dy){
    return Offset32(dx * 32 + dy);
  }
  constexpr Offset32 blackOffset32(Player pl, Offset32 o){
    return (pl==BLACK ? o : -o);
  }

/* ptype.h */

  enum class Ptype : uint8_t {
    EMPTY=0, EDGE=1, PPAWN=2, PLANCE=3, 
      PKNIGHT=4, PSILVER=5, PBISHOP=6, PROOK=7,
      KING=8, GOLD=9, PAWN=10, LANCE=11, 
      KNIGHT=12, SILVER=13, BISHOP=14, ROOK=15,
      MIN=0, PIECE_MIN=2, BASIC_MIN=8,MAX=15  
      };
  constexpr Ptype PPAWN = Ptype::PPAWN;
  constexpr Ptype PLANCE = Ptype::PLANCE;
  constexpr Ptype PKNIGHT = Ptype::PKNIGHT;
  constexpr Ptype PSILVER = Ptype::PSILVER;
  constexpr Ptype PBISHOP = Ptype::PBISHOP;
  constexpr Ptype PROOK = Ptype::PROOK;
  constexpr Ptype KING = Ptype::KING;
  constexpr Ptype GOLD = Ptype::GOLD;
  constexpr Ptype PAWN = Ptype::PAWN;
  constexpr Ptype LANCE = Ptype::LANCE;
  constexpr Ptype KNIGHT = Ptype::KNIGHT;
  constexpr Ptype SILVER = Ptype::SILVER;
  constexpr Ptype BISHOP = Ptype::BISHOP;
  constexpr Ptype ROOK = Ptype::ROOK;
  constexpr int PTYPE_BASIC_SIZE = int(Ptype::MAX - Ptype::BASIC_MIN) + 1;
  constexpr int PTYPE_PIECE_SIZE = int(Ptype::MAX - Ptype::PIECE_MIN) + 1;
  constexpr int PTYPE_SIZE = int(Ptype::MAX - Ptype::MIN) + 1;
  
  constexpr int I(Ptype ptype){ return int(ptype); }
  constexpr int BI(Ptype ptype){ return int(ptype - Ptype::BASIC_MIN); }
  constexpr int PI(Ptype ptype){ return int(ptype - Ptype::PIECE_MIN); }
  constexpr int M(Ptype ptype){ return (1 << int(ptype)); }
  constexpr bool isValid(Ptype ptype){
    return Ptype::MIN <= ptype && ptype <=Ptype::MAX;
  }
  constexpr bool isPiece(Ptype ptype){
    return Ptype::PIECE_MIN <= ptype;
  }
  constexpr bool isPromoted(Ptype ptype){ return ptype < KING; }
  constexpr bool canPromote(Ptype ptype){ return GOLD < ptype; }
  constexpr Ptype promote(Ptype ptype){ return Ptype(int(ptype) - 8); }
  constexpr Ptype unpromote(Ptype ptype){ return Ptype(int(ptype) | 8); }
  constexpr bool isMajorBasic(Ptype ptype){ return BISHOP <= ptype; }
  constexpr bool isMajor(Ptype ptype){ return isMajorBasic(unpromote(ptype)); }
  inline Ptype unpromoteSafe(Ptype ptype)
  {
    return (isPiece(ptype) ? unpromote(ptype) : ptype);
  }

  constexpr bool betterToPromote[PTYPE_SIZE]={
    false,false,true,false,false,false,true,true,
    false,false,true,false,false,false,true,true};
  constexpr bool isBetterToPromote(Ptype ptype){ return betterToPromote[I(ptype)]; }
  constexpr Ptype PieceStandOrder[7]={ ROOK, BISHOP, GOLD, SILVER, KNIGHT, LANCE, PAWN };
  constexpr Ptype BasicNonKings[7]={ PAWN, LANCE, KNIGHT, SILVER, GOLD, BISHOP, ROOK};
  constexpr Ptype PROMOTE_ORDER[7]={PAWN, ROOK, BISHOP, LANCE, KNIGHT, SILVER};
  constexpr const char *ptypeNames[PTYPE_SIZE]={
    "EMPTY","EDGE","PPAWN","PLANCE","PKNIGHT","PSILVER","PBISHOP","PROOK",
    "KING","GOLD","PAWN","LANCE","KNIGHT","SILVER","BISHOP","ROOK"};

  
  /* moves */

  constexpr int xMask=M(UL)|M(UR)|M(DL)|M(DR);
  constexpr int plusMask=M(U)|M(L)|M(R)|M(D);
  constexpr int goldMask=plusMask|M(UL)|M(UR);
  constexpr int moveMasks[PTYPE_SIZE]={
    0,0,goldMask,goldMask,goldMask,goldMask,
    plusMask|M(LONG_UL)|M(LONG_UR)|M(LONG_DL)|M(LONG_DR),
    xMask|M(LONG_U)|M(LONG_L)|M(LONG_R)|M(LONG_D),
    xMask|plusMask,goldMask,M(U),M(LONG_U),M(UUL)|M(UUR),
    xMask|M(U),M(LONG_UL)|M(LONG_UR)|M(LONG_DL)|M(LONG_DR),
    M(LONG_U)|M(LONG_L)|M(LONG_R)|M(LONG_D)};
  constexpr Ptype moveTypes[PTYPE_SIZE]={
    Ptype::EMPTY,Ptype::EDGE,GOLD,GOLD,GOLD,GOLD,PBISHOP,PROOK,
    KING,GOLD,PAWN,LANCE,KNIGHT,SILVER,BISHOP,ROOK};
  constexpr int ptypeMasks[SHORT8_DIRECTION_SIZE]={
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(SILVER)|M(BISHOP),
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(PAWN)|M(LANCE)|M(SILVER)|M(ROOK),
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(SILVER)|M(BISHOP),
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(ROOK),

    M(PBISHOP)|M(PROOK)|M(KING)|M(SILVER)|M(BISHOP),
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(ROOK),
    M(PBISHOP)|M(PROOK)|M(KING)|M(SILVER)|M(BISHOP),
    M(PPAWN)|M(PLANCE)|M(PKNIGHT)|M(PSILVER)|M(PBISHOP)|
    M(PROOK)|M(KING)|M(GOLD)|M(ROOK),
  };
  constexpr int ptypeMask(Direction Dir){ return ptypeMasks[I(Dir)]; }
  constexpr int ptypeMaskNotKing(Direction Dir){ 
    return ptypeMask(Dir) & ~M(KING); 
  }
  constexpr int mayPromoteToYs[PTYPE_SIZE]={
    0,0,0,0, 0,0,0,0,
    0,0, 4,9,5,4,9,9};
  constexpr int dropBlackFromYs[PTYPE_SIZE]={
    0,0,0,0, 0,0,0,0,
    0,1, 2,2,3,1,1,1};
  constexpr bool hasMove(Ptype T, Direction d){
    return (moveMasks[I(T)] & M(d)) != 0;    
  }
  constexpr bool canMove(Ptype T, Direction d){
    return hasMove(T, d) || (isShort8(d) && hasMove(T, shortToLong(d)));
  }
  inline bool canDropTo(Player P,Ptype T,Square pos){
    return  (dropBlackFromYs[I(T)] == 1 ? true :
	     (P==BLACK ? Y(pos) >= dropBlackFromYs[I(T)] :
	      Y(pos) <= reverseY(dropBlackFromYs[I(T)])));
    
  }
  enum PromoteType{
    NoPromoteType = 0, // can't promote
    MayPromoteType = 1, // from is not a promotable square, can promote if to is a promotable square
    CanPromoteType = 2, // from is a promotable square
    MustPromoteType = 3, // no promote move is not allowed
  };
  inline PromoteType promoteType(Player P, Ptype T, Square pos){
    if(T == PAWN){
      if(yForBlack(P, pos) <= 4) return MustPromoteType;
      else return NoPromoteType;
    }
    else if((M(T) & (M(LANCE) | M(BISHOP) | M(ROOK))) != 0){ // TODO: Y(pos) of LANCE is 4, better to returns CanPromoteType
      // ROOKs and BISHOPs are always better to promote so, MustPromoteType is 
      // considered better, but piece_on_board move generater expects to 
      // return CanPromoteType.
#if 0
      if(yForBlack(P, pos) <= 3) return MustPromoteType;
#else
      if(yForBlack(P, pos) <= 3) return CanPromoteType;
#endif
      else return MayPromoteType;
    }
    else if(T == KNIGHT){
      if(yForBlack(P, pos) <= 4) return MustPromoteType;
      else if(yForBlack(P, pos) == 5) return CanPromoteType;
      else return NoPromoteType;
    }
    else{
      assert(T == SILVER);
      if(yForBlack(P, pos) <= 3) return CanPromoteType;
      else if(yForBlack(P, pos) <= 4) return MayPromoteType;
      else return NoPromoteType;
    }
  }
  inline bool mayPromote(Player P,Ptype T,Square pos){
    return (!canPromote(T) ? false :
	    (mayPromoteToYs[I(T)] == 9 ? true :
	     (P==BLACK ? Y(pos) <= mayPromoteToYs[I(T)] :
	      Y(pos) >= reverseY(mayPromoteToYs[I(T)]))));
    
  }
  inline bool mustPromote(Player P,Ptype T,Square pos){
    return (P==BLACK ?
	    (T==PAWN || T==LANCE ? Y(pos) == 2 :
	     (T==KNIGHT ? Y(pos) <= 4 : false)) :
	    (T==PAWN || T==LANCE ? Y(pos) == 8 :
	     (T==KNIGHT ? Y(pos) >= 6 : false)));
  }
  inline bool canPromote(Player P,Ptype T,Square pos){
    return (P==BLACK ?
	    ((T==PAWN || T==LANCE) ? Y(pos) <= 4 :
	     (T==KNIGHT ? Y(pos) <= 5 : Y(pos) <= 3)) :
	    ((T==PAWN || T==LANCE) ? Y(pos) >= 6 :
	     (T==KNIGHT ? Y(pos) >= 5 : yGe7(pos))));
  }
  inline bool checkPromote(Player P,Ptype T,Square pos){
    return (P==BLACK ?
	    (T==SILVER ? Y(pos) == 4 :
	     (T==LANCE || T==ROOK || T==BISHOP)) :
	    (T==SILVER ? Y(pos) == 6 :
	     (T==LANCE || T==ROOK || T==BISHOP)));
  }
#if 0
  /* bug compatible */
  constexpr int exVal[PTYPE_SIZE]={
    0,0,  2,4,5,7,11,13,
    15,10,1,3,5,7,11,13};
#else
  constexpr int exVal[PTYPE_SIZE]={
    0,0,  2,4,6,8,12,14,
    15,10,1,3,5,7,11,13};
#endif
  constexpr int evVal[PTYPE_SIZE]={
    0,0,  768,768,768,768,1472,1664,
    12800, 768, 128,512,512,704,1024,1216};
  constexpr int value(Ptype ptype){
    return evVal[I(ptype)];
  }
  constexpr int cvVal[PTYPE_SIZE]={
    0,0,  
    evVal[2] + evVal[10], evVal[3] + evVal[11],
    evVal[4] + evVal[12], evVal[4] + evVal[13],
    evVal[6] + evVal[14], evVal[7] + evVal[14],
    evVal[8] * 2, evVal[9] * 2, 
    evVal[10] * 2, evVal[11] * 2,
    evVal[12] * 2, evVal[13] * 2,
    evVal[14] * 2, evVal[15] * 2};
  constexpr int captureValue(Ptype ptype){
    return cvVal[I(ptype)];
  }

  std::istream& operator>>(std::istream& is, Ptype& ptype);
  std::ostream& operator<<(std::ostream& os,const Ptype ptype);

  /* PtypeO [-16, 15] 
   * bit [..:4] : Player, 
   * bit [3:0] : Ptype
   */
  enum class PtypeO : int32_t {
    MIN = -16, MAX= 15,
      EDGE = -15, W_PPAWN = -14, W_PLANCE = -13, 
      W_PKNIGHT = -12, W_PSILVER = -11, W_PBISHOP = -10, W_PROOK = -9,
      W_KING = -8, W_GOLD = -7, W_PAWN = -6, W_LANCE = -5,
      W_KNIGHT = -4, W_SILVER = -3, W_BISHOP = -2, W_ROOK = -1,
      EMPTY = 0, B_PPAWN = 2, B_PLANCE = 3,
      B_PKNIGHT = 4, B_PSILVER = 5, B_PBISHOP = 6, B_PROOK = 7,
      B_KING = 8, B_GOLD = 9, B_PAWN = 10, B_LANCE = 11,
      B_KNIGHT = 12, B_SILVER = 13, B_BISHOP = 14, B_ROOK = 15,
      };
  constexpr int PTYPEO_SIZE = int(PtypeO::MAX) - int(PtypeO::MIN) + 1;
  
  constexpr int I(PtypeO ptypeo) { return int(ptypeo - PtypeO::MIN); }
  constexpr PtypeO newPtypeO(Player player,Ptype ptype) {
    return PtypeO(int(ptype) - (16 & int(player)));
  }
  constexpr Ptype getPtype(PtypeO ptypeO){ return Ptype(int(ptypeO) & 15); }
  /**
   * true if ptypeO is not (EMPTY or EDGE).
   */
  constexpr bool isPiece(PtypeO ptypeO){ return isPiece(getPtype(ptypeO)); }
  constexpr Player getOwner(PtypeO ptypeO){ return Player(int(ptypeO) >> 31); }

  inline PtypeO promote(PtypeO ptypeO)
  {
    assert(canPromote(getPtype(ptypeO)));
    return PtypeO(int(ptypeO) - 8); 
  }
  
  /** returns a promote(ptypeO) if isPromote is true */
  inline PtypeO checkPromote(PtypeO ptypeO, bool is_promotion)
  {
    return PtypeO(int(ptypeO) - (int(is_promotion) << 3));
  }
  
  /** 
   * ptypeO must be a PIECE( neighter EMPTY nor EMPTY )
   * unpromote ptypeO , if PtypeO can promote
   * do nothing       , if PtypeO cannot promote
   */
  inline PtypeO unpromote(PtypeO ptypeO)
  {
    return PtypeO(int(ptypeO)|8); 
  }
  /** negate color */
  inline PtypeO alt(PtypeO ptypeO)
  {
    assert(isPiece(ptypeO));
    return PtypeO(int(ptypeO) ^ ~15);
  }
  /** unpromote and negate color */
  inline PtypeO captured(PtypeO ptypeO)
  {
    return unpromote(alt(ptypeO));
  }
  /** negate color if ptypeO is a Piece */
  inline PtypeO altIfPiece(PtypeO ptypeO)
  {
    int v=int(ptypeO);
    return PtypeO(v ^ ((1 - (v & 15)) & ~15));
  }
  constexpr bool canPromote(PtypeO ptypeO)
  {
    return canPromote(getPtype(ptypeO));
  }
  constexpr bool isValid(PtypeO ptypeO){
    return PtypeO::MIN <= ptypeO && ptypeO <= PtypeO::MAX;
  }
  constexpr int VV(PtypeO ptypeO){
    return valueFor(getOwner(ptypeO), evVal[I(getPtype(ptypeO))]);
  }
  constexpr int valueV[PTYPEO_SIZE]={
    VV(PtypeO(-16)),VV(PtypeO(-15)),VV(PtypeO(-14)),VV(PtypeO(-13)),
    VV(PtypeO(-12)),VV(PtypeO(-11)),VV(PtypeO(-10)),VV(PtypeO(-9)),
    VV(PtypeO(-8)),VV(PtypeO(-7)),VV(PtypeO(-6)),VV(PtypeO(-5)),
    VV(PtypeO(-4)),VV(PtypeO(-3)),VV(PtypeO(-2)),VV(PtypeO(-1)),
    VV(PtypeO(0)),VV(PtypeO(1)),VV(PtypeO(2)),VV(PtypeO(3)),
    VV(PtypeO(4)),VV(PtypeO(5)),VV(PtypeO(6)),VV(PtypeO(7)),
    VV(PtypeO(8)),VV(PtypeO(9)),VV(PtypeO(10)),VV(PtypeO(11)),
    VV(PtypeO(12)),VV(PtypeO(13)),VV(PtypeO(14)),VV(PtypeO(15))
  };
  constexpr int value(PtypeO ptypeO){
    return valueV[I(ptypeO)];
  }
  constexpr int CV(PtypeO ptypeO){
    return valueFor(getOwner(ptypeO), -evVal[I(getPtype(ptypeO))]-evVal[I(unpromote(getPtype(ptypeO)))]);
  }
  constexpr int captureV[PTYPEO_SIZE]={
    CV(PtypeO(-16)),CV(PtypeO(-15)),CV(PtypeO(-14)),CV(PtypeO(-13)),
    CV(PtypeO(-12)),CV(PtypeO(-11)),CV(PtypeO(-10)),CV(PtypeO(-9)),
    CV(PtypeO(-8)),CV(PtypeO(-7)),CV(PtypeO(-6)),CV(PtypeO(-5)),
    CV(PtypeO(-4)),CV(PtypeO(-3)),CV(PtypeO(-2)),CV(PtypeO(-1)),
    CV(PtypeO(0)),CV(PtypeO(1)),CV(PtypeO(2)),CV(PtypeO(3)),
    CV(PtypeO(4)),CV(PtypeO(5)),CV(PtypeO(6)),CV(PtypeO(7)),
    CV(PtypeO(8)),CV(PtypeO(9)),CV(PtypeO(10)),CV(PtypeO(11)),
    CV(PtypeO(12)),CV(PtypeO(13)),CV(PtypeO(14)),CV(PtypeO(15))
  };
  constexpr int captureValue(PtypeO ptypeO){
    return captureV[I(ptypeO)];
  }
  constexpr int pV(PtypeO ptypeO){
    return valueFor(getOwner(ptypeO), evVal[I(getPtype(ptypeO))]-evVal[I(unpromote(getPtype(ptypeO)))]);
  }
  constexpr int promoteV[PTYPEO_SIZE]={
    pV(PtypeO(-16)),pV(PtypeO(-15)),pV(PtypeO(-14)),pV(PtypeO(-13)),
    pV(PtypeO(-12)),pV(PtypeO(-11)),pV(PtypeO(-10)),pV(PtypeO(-9)),
    pV(PtypeO(-8)),pV(PtypeO(-7)),pV(PtypeO(-6)),pV(PtypeO(-5)),
    pV(PtypeO(-4)),pV(PtypeO(-3)),pV(PtypeO(-2)),pV(PtypeO(-1)),
    pV(PtypeO(0)),pV(PtypeO(1)),pV(PtypeO(2)),pV(PtypeO(3)),
    pV(PtypeO(4)),pV(PtypeO(5)),pV(PtypeO(6)),pV(PtypeO(7)),
    pV(PtypeO(8)),pV(PtypeO(9)),pV(PtypeO(10)),pV(PtypeO(11)),
    pV(PtypeO(12)),pV(PtypeO(13)),pV(PtypeO(14)),pV(PtypeO(15))
  };
  constexpr int promoteValue(PtypeO ptypeO){
    return promoteV[I(ptypeO)];
  }
  
  std::ostream& operator<<(std::ostream& os,const PtypeO ptypeO);
  
  constexpr int indexMins[PTYPE_SIZE]={
    0,0,0,32,18,22,36,38,
    30,26,0,32,18,22,36,38};
  constexpr int indexMin(Ptype ptype){ return indexMins[I(ptype)]; }
  constexpr int indexLimits[PTYPE_SIZE]={
    0,0,18,36,22,26,38,40,
    32,30,18,36,22,26,38,40};
  constexpr int indexLimit(Ptype ptype){ return indexLimits[I(ptype)]; }
  constexpr Ptype ptypes[40]={
    PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,
    PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,PAWN,
    KNIGHT,KNIGHT,KNIGHT,KNIGHT, SILVER,SILVER,SILVER,SILVER,
    GOLD,GOLD,GOLD,GOLD, KING,KING,
    LANCE,LANCE,LANCE,LANCE, BISHOP,BISHOP,ROOK,ROOK
  };
  constexpr int kingIndex(Player P){
    return indexMin(KING)+I(P);
  }

  constexpr int EMPTY_NUM=0x80;
  constexpr int EDGE_NUM=0x40;
  enum class Piece : int32_t {};
  constexpr int Piece_SIZE=40;
  constexpr int Piece_BitOffsetPtype=16;
  constexpr int Piece_BitOffsetPromote=19;
  constexpr int Piece_BitOffsetMovePromote=23;
  constexpr Piece newPiece(Player owner, Ptype ptype, int num, Square square){
    return Piece((uint32_t(owner) << 20) + (int(ptype) << 16) + 
		 (num << 8) + I(square));
  }
  constexpr Piece Piece_EMPTY = newPiece(BLACK, Ptype::EMPTY, EMPTY_NUM, Square::STAND);
  constexpr Piece Piece_EDGE = newPiece(WHITE, Ptype::EDGE, EDGE_NUM, Square::STAND);

  inline int V(Piece p){ return int(p); }
  inline Ptype ptype(Piece p){
    return Ptype((int(p) >> 16) & 0xf);
  }
  inline PtypeO ptypeO(Piece p){
    return PtypeO(int(p) >> 16);
  }
  inline int number(Piece p){
    return ((int(p) & 0xff00) >> 8);
  }
  inline Square square(Piece p){
    return Square(int(p) & 0xff);
  }
  inline bool isOnBoard(Piece p){
    //    assert(isValid(square(p)));
#if 0
    return (int32_t(p) & 0xff) != 0;
#else
    return ! isPieceStand(square(p));
#endif
  }
  inline bool isOnBoardByOwner(Player P, Piece p){ 
    return (P==BLACK ? 
	    int32_t(uint32_t(p) & 0x800000ff) > 0 :
	    int32_t((-int(p)) & 0x800000ff) > 0);
  }
  inline Piece promote(Piece p){
    return Piece(int(p) - 0x80000);
  }
  inline Piece unpromote(Piece p){
    return Piece(int(p) | 0x80000);
  }
  /**
   * make a captured piece from a piece
   * unpromote and change the color
   */
  inline Piece captured(Piece p){
    return Piece((int(p) & 0xfff7ff00) ^ 0xfff80000);
  }

  /** check a piece is promoted or not */
  inline bool isPromoted(Piece p){ return (V(p) & (1<<19)) == 0; }
  /** promote a piece if promotedp is true */
  inline Piece checkPromote(Piece p,bool promotep){
    return Piece(V(p) - (promotep<<19));
  }
  /**
   * promoteしていないOnBoardの駒であることのチェック
   * Lance位しか使い道がない?
   */
  inline bool isOnBoardNotPromoted(Piece p){
    int mask=V(p)&((1<<19)|0xff);
    return mask>(1<<19);
  }
  inline bool isEmpty(Piece p){
    return (V(p)&0x8000)!=0;
  }
  inline bool isEmptyNum(int num) {
    return (num&0x80)!=0;
  }
  inline bool isEdge(Piece p){ 
    return (V(p)&0x4000)!=0;
  }
  inline bool isEdgeNum(int num){
    //    assert(!isEmptyNum(num));
    return (num&0x40)!=0;
  }
  inline bool isPieceNum(int num){
    return (num&0xc0)==0;
  }
  /**
   * あるpieceがPlayer pの持ち物でPtype ptypeであるかどうかをチェックする．
   * TはEMPTY, Ptype::EDGEではない．
   */
  inline bool isPlayerPtype(Piece p,Player pl,Ptype ptype){
    assert(Ptype::PIECE_MIN<=ptype && ptype<=Ptype::MAX);
    return (V(p)&0x1f0000)==((static_cast<int>(ptype)<<16)|(static_cast<int32_t>(pl)&0x100000));
  }
  inline bool isPiece(Piece p){
    return (V(p)&0xc000)==0;
  }
  inline bool pieceIsBlack(Piece p){
    assert(isPiece(p));
    return V(p)>=0;
  }
  inline Player owner(Piece p){
    assert(isPiece(p));
    return Player(V(p)>>20);
  }
  inline bool canMoveOn(Player P,Piece p){ 
    return (P==BLACK ? ((V(p)+0xe0000)&0x104000)==0 :V(p)>=0);
  }
  std::ostream& operator<<(std::ostream& os,const Piece piece);

  /* bit operations */
  inline int bsf(uint32_t mask)
  {
    assert(mask);
#if defined(__i386__) || defined(__x86_64__)  || (defined(_MSC_VER) && defined(__INTEL_COMPILER))
    int ret;
    __asm__("bsfl %1,%0" : "=r"(ret) : "r"(mask));
    return ret;
#elif defined __GNUC__
    return __builtin_ctzl(mask);
#else
    for (int i = 0; i < 32; i++)
      if (mask & (1 << i))
	return i;
    assert(0);
    return -1;
#endif
  }
  inline uint16_t bsf(uint16_t mask)
  {
    assert(mask);
#if defined(__i386__) || defined(__x86_64__)  || (defined(_MSC_VER) && defined(__INTEL_COMPILER))
    uint16_t ret;
    __asm__("bsfw %1,%0" : "=r"(ret) : "r"(mask));
    return ret;
#elif defined(__GNUC__)
    return __builtin_ctzl(mask);
#else
    for (int i = 0; i < 16; i++)
      if (mask & (1 << i))
	return i;
    assert(0);
    return -1;
#endif
  }
  inline int bsf64(uint64_t mask)
  {
    assert(mask);
#if defined(__x86_64__)  || (defined(_WIN64) && defined(_MSC_VER) && defined(__INTEL_COMPILER))
    uint64_t ret;
    __asm__("bsfq %1,%0" : "=r"(ret) : "r"(mask));
    return int(ret);
#elif defined(__GNUC__)
	  return __builtin_ctzll(mask);
#else
    uint32_t mask32 = uint32_t(mask);
    if (mask32) return bsf(mask32);
    mask32 = uint32_t(mask >> 32);
    return 32 + bsf(mask32);
#endif
  }
  inline int takeOneBit(uint32_t& mask){
    int num = bsf(mask);
    mask &= mask-1;
    return num;
  }
  inline int takeOneBit(uint64_t& mask){
    int num = bsf64(mask);
    mask &= mask-1;
    return num;
  }
  inline int countBit(uint32_t mask){
#ifndef OSL_USE_SSE41
    int count=0;
    for(; mask != 0; count++) mask &= mask - 1;
    return count;
#else
    return _mm_popcnt_u32(mask);
#endif
  }
  inline int countBit64(uint64_t mask){
#ifndef OSL_USE_SSE41
    int count=0;
    for(; mask != 0; count++) mask &= mask - 1;
    return count;
#else
    return _mm_popcnt_u64(mask);
#endif
  }

/* pieceMask.h */
  enum class PieceMask : uint64_t {};
  inline uint64_t V(PieceMask pm){ return uint64_t(pm); }
  inline int takeOneBit(PieceMask& pm){
    int r = bsf64(V(pm));
    pm=PieceMask(V(pm) & (V(pm) - 1));
    return r;
  }
  inline PieceMask lowestMask(PieceMask pm){
    return PieceMask(V(pm) & -V(pm));
  }
  constexpr PieceMask newPieceMask(int num) {
    return PieceMask(1ull << num);
  }
  inline void set(PieceMask& pm,int num){
    pm |= newPieceMask(num);
  }
  inline void reset(PieceMask& pm,int num){
    pm &= ~newPieceMask(num);
  }
  inline void flip(PieceMask& pm,int num){
    pm ^= newPieceMask(num);
  }
  inline int countBit(PieceMask const& pm){
    return countBit64(V(pm));
  }
  inline bool any(PieceMask const& pm){
    return V(pm) != 0;
  }
  inline void resetAll(PieceMask & pm){
    pm = PieceMask(0ull);
  }
  inline void setAll(PieceMask & pm){
    pm = PieceMask(0xffffffffffffffffull);
  }
  inline bool test(PieceMask const& pm, int num){
    return any(pm & newPieceMask(num));
  }
  inline int bsf(PieceMask pm){
    return bsf64(V(pm));
  }
  constexpr PieceMask pieceMasks[16]={
    PieceMask(0),PieceMask(0),PieceMask(0x3ffff),PieceMask(0xf00000000ull),
    PieceMask(0x3c0000),PieceMask(0x3c00000),PieceMask(0x3000000000ull),PieceMask(0xc000000000ull),
    PieceMask(0xc0000000),PieceMask(0x3c000000),PieceMask(0x3ffff),PieceMask(0xf00000000ull),
    PieceMask(0x3c0000),PieceMask(0x3c00000),PieceMask(0x3000000000ull),PieceMask(0xc000000000ull)
  };
  constexpr PieceMask PM40 = PieceMask(0xffffffffffull);
  constexpr PieceMask pieceMask(Ptype ptype){
    return pieceMasks[I(ptype)];
  }
  constexpr PieceMask PieceMask_RBGS = pieceMask(ROOK) | pieceMask(BISHOP) | pieceMask(SILVER) | pieceMask(GOLD);
  inline int bsf(PieceMask pm, Ptype ptype){
    return bsf64(V(pm & pieceMask(ptype)));
  }
  inline PieceMask longMask(){
    return PieceMask(0xff00000000ull);
  }
  inline PieceMask selectPtype(PieceMask pm,Ptype ptype){
    return pm & pieceMask(ptype);
  }

  std::ostream& operator<<(std::ostream& os, const PieceMask& pieceMask);

  namespace misc
  {
    template <typename T1, typename T2>
    inline
    void construct(T1* ptr, const T2& value, 
		   typename std::enable_if<std::is_pod<T1>::value >::type * =0)
    {
      assert(ptr);
      *ptr = T1(value);
    }

    template <typename T1, typename T2>
    inline
    void construct(T1* ptr, const T2& value, 
		   typename std::enable_if<!std::is_pod<T1>::value >::type * =0)
    {
      assert(ptr);
      ::new(ptr) T1(value);
    }

    template <typename T>
    inline void destroy(T *ptr) 
    {
      ptr->~T(); 
    }

    template <typename ForwardIterator>
    inline void destroy(ForwardIterator first, ForwardIterator last)
    {
      typedef typename std::iterator_traits<ForwardIterator>::value_type
	value_type;
#if defined(__INTEL_COMPILER) || (__GNUC__ >= 4 && 6 <= __GNUC_MINOR__  && __GNUC_MINOR__ <=7)
      if (std::has_trivial_default_constructor<value_type>::type::value)
	return;
#else
      if (std::is_trivially_destructible<value_type>::type::value)
	return;
#endif
      for (; first != last; ++first)
	destroy(&*first);
    }
  }
}
namespace std{
  template<typename T, size_t N1>
  std::ostream& operator<<(std::ostream& os, const std::array<T,N1>& v){
    os << "[";
    for(size_t i = 0;i < N1; i++) os << v[i] << ",";
    return os << "]";
  }
}
/* fixedCapacityVector.h
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >=4)
#  pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif

namespace osl
{
  namespace misc
  {
    template <typename T>
    class FixedCapacityVectorPushBack
    {
      T *ptr;
      T **vPtr;
    public:
      FixedCapacityVectorPushBack(T** vPtr_, T* /* limit_ */)
	: ptr(*vPtr_), vPtr(vPtr_)
      {
      }
      ~FixedCapacityVectorPushBack()
      {
	assert( *vPtr == ptr );
	*vPtr = ptr;
      }
      void push_back(const T& e) 
      {
	assert( *vPtr == ptr );
	if(std::is_pod<T>::value)
	  *ptr++ = e;
	else
	  construct(ptr++,e);
#ifndef NDEBUG
	(*vPtr)++;
#endif
      }
    };
    template <typename T, size_t Capacity>
    class FixedCapacityVector
    {
    protected:
      struct Array : public std::array<T, Capacity> {};
      typedef Array array_t;
      T* ptr;
      std::array<int64_t, (sizeof(T[Capacity])+sizeof(int64_t)-1)/sizeof(int64_t)> relements;
    private:
      const array_t &elements() const{
	return *reinterpret_cast<const array_t*>(&relements);
      }
      array_t &elements(){
	return *reinterpret_cast<array_t*>(&relements);
      }
    public:
      typedef typename array_t::value_type value_type;
      typedef typename array_t::iterator iterator;
      typedef typename array_t::const_iterator const_iterator;
    
      FixedCapacityVector() : ptr(&(elements()[0])) {}
      explicit FixedCapacityVector(size_t s) : ptr(&(elements()[0])) 
      {
	resize(s);
      }
      FixedCapacityVector(FixedCapacityVector const& rhs){
	ptr= &*begin()+rhs.size();
	std::uninitialized_copy(rhs.begin(),rhs.end(),begin());
      }
      template <class RangeIterator>
      FixedCapacityVector(const RangeIterator& first, const RangeIterator& last)
	: ptr(&(elements()[0]))
      {
	push_back(first, last);
      }
      ~FixedCapacityVector() 
      {
	destroy(begin(),end());
      }
      FixedCapacityVector& operator=(FixedCapacityVector const& rhs){
	if (this == &rhs)
	  return *this;
      
	if(size()>rhs.size()){
	  iterator it=std::copy(rhs.begin(),rhs.end(),begin());
	  destroy(it,end());
	}
	else{
	  iterator it=std::copy(&(rhs.elements()[0]),
				&(rhs.elements()[0])+size(),begin());
	  std::uninitialized_copy(&(rhs.elements()[0])+size(),
				  &(rhs.elements()[0])+rhs.size(),it);
	}
	ptr= &*begin()+rhs.size();
	return *this;
      }

      T& operator[] (size_t i)
      {
	assert(i <= size());
	return elements()[i];
      }

      iterator begin() {  return static_cast<iterator>(&elements()[0]); }
      iterator end() { return static_cast<iterator>(ptr); }

      T& front() { return *begin(); }
      T& back() { return *(end() - 1); }

      void push_back(const T& e)
      {
	assert(size() < Capacity);
	construct(ptr,e);
	++ptr;
      }
      template <class RangeIterator>
      void push_back(const RangeIterator& first, const RangeIterator& last);
      void pop_back() { 
	--ptr;
	destroy(ptr+1);
      }
      void clear() { 
	size_t s=size();
	ptr= &(elements()[0]);
	// 該当する部分のdestructorを呼ぶ
	destroy(begin(),begin()+(int)s);
      }
      void resize(size_t new_length)
      {
	while (size() < new_length)
	  push_back(T());
	if (new_length < size()) {
	  destroy(begin()+(int)new_length,end());
	  ptr= &(elements()[new_length]);
	}
      }

      void erase(const T& e)
      {
	const iterator new_end = std::remove(begin(), end(), e);
	ptr= &*new_end;
	destroy(new_end,end());
      }

      /** 重複する要素を取り除く */
      void unique()
      {
	std::sort(begin(),end());
	iterator last = std::unique(begin(), end());
	ptr = &*last;
	destroy(last,end());
      }

      size_t size() const { return ptr-&*begin(); }
      bool empty() const { return ptr==&*begin(); }
      size_t capacity() const { return Capacity; }

      T const& operator[] (size_t i) const
      {
	assert(i < size());
	return elements()[i];
      }
      const_iterator begin() const { return static_cast<const_iterator>(&elements()[0]); }
      const_iterator end()   const { return static_cast<const_iterator>(ptr); }

      const T& front() const { return *begin(); }
      const T& back() const { return *(end() - 1); }
    
      bool isMember(const T& e, const_iterator first, const_iterator last) const
      {
	return std::find(first, last, e) != last;
      }
      bool isMember(const T& e) const
      {
	return isMember(e, begin(), end());
      }
      FixedCapacityVectorPushBack<T> pushBackHelper() 
      {
	return FixedCapacityVectorPushBack<T>
	  (&ptr, &*begin()+Capacity);
      }
    };
    template <typename T, size_t C> inline
    bool operator==(const FixedCapacityVector<T,C>& l, const FixedCapacityVector<T,C>& r) 
    {
      return l.size() == r.size() && std::equal(l.begin(), l.end(), r.begin());
    }
    template <typename T, size_t C> inline
    bool operator<(const FixedCapacityVector<T,C>& l, const FixedCapacityVector<T,C>& r) 
    {
      return std::lexicographical_compare(l.begin(), l.end(), r.begin(), r.end());
    }
  } // namespace misc
  using misc::FixedCapacityVector;
  using misc::FixedCapacityVectorPushBack;
} // namespace osl

template <typename T, size_t Capacity>
template <class RangeIterator>
void osl::misc::FixedCapacityVector<T,Capacity>::push_back(const RangeIterator& first, const RangeIterator& last)
{
  iterator insert_point = end();
  std::uninitialized_copy(first, last, insert_point);
  ptr += last-first;
  assert(size() <= Capacity);
}

/* squareCompressor.h */

namespace osl
{

  constexpr int8_t sc_positionToIndex[Square_SIZE] = 
  { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 
    0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0, 0, 0, 0, 0,
    0,  0, 10, 11, 12, 13, 14, 15, 16, 17, 18,  0, 0, 0, 0, 0,
    0,  0, 19, 20, 21, 22, 23, 24, 25, 26, 27,  0, 0, 0, 0, 0,
    0,  0, 28, 29, 30, 31, 32, 33, 34, 35, 36,  0, 0, 0, 0, 0,
    0,  0, 37, 38, 39, 40, 41, 42, 43, 44, 45,  0, 0, 0, 0, 0,
    0,  0, 46, 47, 48, 49, 50, 51, 52, 53, 54,  0, 0, 0, 0, 0,
    0,  0, 55, 56, 57, 58, 59, 60, 61, 62, 63,  0, 0, 0, 0, 0,
    0,  0, 64, 65, 66, 67, 68, 69, 70, 71, 72,  0, 0, 0, 0, 0,
    0,  0, 73, 74, 75, 76, 77, 78, 79, 80, 81,  0, 0, 0, 0, 0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0
  };
  constexpr int8_t dirToOffset81[8] = {
    8, -1, -10, 9, -9, 10, 1, -8
  };
  constexpr Square scToSquare[82] = {
    Square::STAND,
    newSquare(1,1), newSquare(1,2), newSquare(1,3), newSquare(1,4), newSquare(1,5), newSquare(1,6), newSquare(1,7), newSquare(1,8), newSquare(1,9), 
    newSquare(2,1), newSquare(2,2), newSquare(2,3), newSquare(2,4), newSquare(2,5), newSquare(2,6), newSquare(2,7), newSquare(2,8), newSquare(2,9), 
    newSquare(3,1), newSquare(3,2), newSquare(3,3), newSquare(3,4), newSquare(3,5), newSquare(3,6), newSquare(3,7), newSquare(3,8), newSquare(3,9), 
    newSquare(4,1), newSquare(4,2), newSquare(4,3), newSquare(4,4), newSquare(4,5), newSquare(4,6), newSquare(4,7), newSquare(4,8), newSquare(4,9), 
    newSquare(5,1), newSquare(5,2), newSquare(5,3), newSquare(5,4), newSquare(5,5), newSquare(5,6), newSquare(5,7), newSquare(5,8), newSquare(5,9), 
    newSquare(6,1), newSquare(6,2), newSquare(6,3), newSquare(6,4), newSquare(6,5), newSquare(6,6), newSquare(6,7), newSquare(6,8), newSquare(6,9), 
    newSquare(7,1), newSquare(7,2), newSquare(7,3), newSquare(7,4), newSquare(7,5), newSquare(7,6), newSquare(7,7), newSquare(7,8), newSquare(7,9), 
    newSquare(8,1), newSquare(8,2), newSquare(8,3), newSquare(8,4), newSquare(8,5), newSquare(8,6), newSquare(8,7), newSquare(8,8), newSquare(8,9), 
    newSquare(9,1), newSquare(9,2), newSquare(9,3), newSquare(9,4), newSquare(9,5), newSquare(9,6), newSquare(9,7), newSquare(9,8), newSquare(9,9)
  };
  /**
   * Square を [0..81] に圧縮する
   * 0: 駒台，1..81 盤上
   */
  struct SquareCompressor
  {
  private:
  public:
    static int compress(Square pos)
    {
      const int result = sc_positionToIndex[I(pos)];
      assert(result >= 0);
      return result;
    }
    static Square
    melt(int index)
    {
      assert(0 <= index);
      assert(index < 82);
      return scToSquare[index];
    }
  };

} // namespace osl

/* move.h */
/** move 関係でつかまえ所のないエラーがでるときに定義する */
// #define MOVE_DEBUG
#ifdef MOVE_DEBUG
#  include <cassert>
#  define move_assert(x) assert(x)
#else
#  define move_assert(x) 
#endif
// 2009/12/10 以前のfromが下位にあるパターンと
// operator< を同じにしたい時に定義する．

namespace osl
{
  /** 16bit 表現*/
  enum class Move16 : int16_t {
    NONE = 0,
      };
  /**
   * 圧縮していない moveの表現 .
   * - invalid: isInvalid 以外の演算はできない
   * - declare_win: isInvalid 以外の演算はできない
   * - pass: from, to, ptype, oldPtype はとれる．player()はとれない．
   * 
   * Pieceとpromotepをそろえる  -> 変える． 
   * 下位から 
   * 2014/2/3から
   * - to       : 8 bit 
   * - from     : 8 bit 
   * - capture ptype    : 4 bit 
   * - dummy    : 3 bit 
   * - promote? : 1 bit  
   * - ptype    : 4 bit - promote moveの場合はpromote前のもの
   * - owner    : signed 
   * 2009/12/10から
   * - to       : 8 bit 
   * - from     : 8 bit 
   * - capture ptype    : 4 bit 
   * - dummy    : 3 bit 
   * - promote? : 1 bit  
   * - ptype    : 4 bit - promote moveの場合はpromote後のもの
   * - owner    : signed 
   */
  enum class Move : int32_t {
    INVALID_VALUE = (1<<8), DECLARE_WIN = (2<<8),
					      };
  /** 一局面辺りの合法手の最大値 
   * 重複して手を生成することがある場合は，600では不足かもしれない
   */
  constexpr unsigned int MaxUniqMoves=600;
  constexpr Move Move_INVALID=Move::INVALID_VALUE;
  constexpr Move Move_DeclareWin=Move::DECLARE_WIN;
  inline Move newMove(Square from, Square to, Ptype oldPtype,
		      Ptype capture_ptype, bool is_promotion, Player player)
  {
    move_assert(isValid(from));
    move_assert(isOnBoard(to));
    move_assert(isValid(oldPtype));
    move_assert(isValid(capture_ptype));
    move_assert(isValid(player));
    Move m=Move((I(to) & 0xff)
		+ ((I(from) & 0xff) << 8)
		+ (uint32_t(capture_ptype)<<16)
		+ (uint32_t(is_promotion) << 23)
		+ (uint32_t(oldPtype)<<24)
		+ (int32_t(player)<<28));

    move_assert(isValid(m));
    return m;
  }
  inline Move newMove(Square from, Square to, Piece capture_piece, 
		      Ptype pt, bool is_promotion, Player player){
    return newMove(from, to, pt, ptype(capture_piece), is_promotion, player);
  }
  /**
   * drop
   */
  inline Move newMove(Square to, Ptype ptype, Player player){
    
    move_assert(isOnBoard(to));
    move_assert(isValid(ptype));
    move_assert(isValid(player));
    return newMove(Square::STAND, to, ptype, Ptype::EMPTY, false, player);
  }

  inline int V(Move m){
    return int(m);
  }
  inline bool isPass(Move m){
    return (V(m) & 0xffff) == 0; 
  }
  inline bool isNormal(Move m){ 
    // PASS や INVALID は to() が 00
    return (V(m) & 0x00ff)!=0; 
  }
  inline bool isInvalid(Move m){ 
    return uint32_t(V(m)-1) < uint32_t(Move::DECLARE_WIN); 
  }
  bool isValid(Move m);
  inline bool isValidOrPass(Move m){ 
    return isPass(m) || isValid(m); 
  }
  inline Player player(Move m){
    assert(! isInvalid(m));
    const Player result = Player(V(m)>>28);
    return result;
  }
  inline Square from(Move m)
  {
    assert(! isInvalid(m));
    move_assert(isValidOrPass(m));
    Square result = Square((V(m)>>8) & 0xff);
    return result;
  }
  inline bool isDrop(Move m){ 
    assert(isNormal(m)); 
    return isPieceStand(from(m)); 
  }
  inline Square to(Move m){
    assert(! isInvalid(m));
    move_assert(isValidOrPass(m));
    Square result = Square(V(m) & 0xff);
    return result;
  }
  /** 移動後のPtype, i.e., 成る手だった場合成った後 */
  inline PtypeO ptypeO(Move m){
    assert(! isInvalid(m));
    const PtypeO result = PtypeO((V(m) >> 24)-((V(m) >> 20) & 8));
    return result;
  }
  inline Ptype ptype(Move m){
    return getPtype(ptypeO(m));
  }
  /** 移動前のPtypeO, i.e., 成る手だった場合成る前 */
  inline PtypeO oldPtypeO(Move m){
    assert(! isInvalid(m));
    const PtypeO result = PtypeO(V(m)>>24);
    return result;
  }
  /** 移動前のPtype, i.e., 成る手だった場合成る前 */
  inline Ptype oldPtype(Move m){ 
    return getPtype(oldPtypeO(m));
  }
  inline Ptype capturePtype(Move m){
    assert(isNormal(m));
    const Ptype result = static_cast<Ptype>((V(m)>>16)&0xf);
    return result;
  }
  inline bool isPromotion(Move m){ 
    assert(isNormal(m)); 
    return (V(m) & (1 << Piece_BitOffsetMovePromote)) != 0; 
  }
  inline bool isCapture(Move m){ 
    assert(isNormal(m)); 
    return capturePtype(m) != Ptype::EMPTY; 
  }
  inline PtypeO capturePtypeO(Move m){
    assert(isCapture(m));
    return newPtypeO(alt(player(m)), capturePtype(m));
  }
  inline PtypeO capturePtypeOSafe(Move m){
    if (! isCapture(m))
      return PtypeO::EMPTY;
    return capturePtypeO(m);
  }
  inline bool isCaptureOrPromotion(Move m){
    return isCapture(m) || isPromotion(m); 
  }
  /**
   * make a promote move from a unpromote move
   */
  inline Move promote(Move m){
    assert(isNormal(m));
    move_assert(!isPromotion(m) && canPromote(ptype(m)));
    return Move(V(m)^(1<<23));
  }
  /**
   * make a capture move from a non-capture move
   */
  inline Move newAddCapture(Move m,Piece capture){
    assert(! isCapture(m));
    return Move(V(m)+(V(capture)&0xf0000));
  }
  /**
   * make add offset 'o' to the 'to' field of a move
   */
  inline Move newAddTo(Move m,Offset o){
    return Move(V(m)+int(o));
  }
  /**
   * set 'to' field of a move, original 'to' must be 'PieceStand'
   */
  inline Move newAddTo(Move m,Square sq){
    assert((V(m)&0xff)==0);
    return Move(V(m)+I(sq));
  }
  inline bool ignoreUnpromote(Player P, Ptype ptype,Square from,Square to){
    switch(ptype){
    case PAWN: 
      return canPromote(P,to);
    case BISHOP: case ROOK: 
      return canPromote(P,to) || canPromote(P,from);
    case LANCE:
      return (P==BLACK ? Y(to)==2 : Y(to)==8);
    default: return false;
    }
  }
  enum class MoveMask : int32_t {
    ZERO = 0, DROP = 1, NO_CAPTURE = 2, CAPTURE = 4, PROMOTION = 8,
      };
  constexpr bool isPromotion(MoveMask mt) {
    return (mt & MoveMask::PROMOTION) != MoveMask::ZERO;
  }
  constexpr bool isDrop(MoveMask mt) {
    return (mt & MoveMask::DROP) != MoveMask::ZERO;
  }
  constexpr bool isCapture(MoveMask mt) {
    return (mt & MoveMask::CAPTURE) != MoveMask::ZERO;
  }
  constexpr bool noCapture(MoveMask mt) {
    return (mt & MoveMask::NO_CAPTURE) != MoveMask::ZERO;
  }
  Move16 toMove16(Move);
  inline Move PASS(Player P) { return Move(static_cast<int>(P)<<28); }
  std::ostream& operator<<(std::ostream& os, Move move);
}

/* offset32.h */
namespace osl 
{
} // namespace osl

/* boardTable.h */
namespace osl
{
  class BoardTable
  {
    static constexpr std::array<Direction,Offset32_SIZE> directions = {{
	Direction(18),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(20),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(18),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(20),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(18),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(20),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(18),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(20),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(18),Direction( 0),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction( 0),
	Direction(20),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(18),Direction( 0),Direction( 0),
	Direction(23),Direction( 0),Direction( 0),Direction(20),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(18),Direction( 0),
	Direction(23),Direction( 0),Direction(20),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(18),
	Direction(23),Direction(20),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(17),Direction(17),Direction(17),Direction(17),
	Direction(17),Direction(17),Direction(17),Direction(17),
	Direction( 0),Direction(21),Direction(21),Direction(21),
	Direction(21),Direction(21),Direction(21),Direction(21),
	Direction(21),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(16),
	Direction(19),Direction(22),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(16),Direction( 0),
	Direction(19),Direction( 0),Direction(22),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(16),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction(22),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(16),Direction( 0),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction( 0),
	Direction(22),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(16),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(22),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(16),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction(22),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction(16),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction(22),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(16),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(19),Direction( 0),Direction( 0),Direction( 0),
	Direction( 0),Direction( 0),Direction( 0),Direction( 0),
	Direction(22),
      }};
    std::array<signed char,OFFSET_ONBOARD_SIZE> short8Offset;
    std::array<unsigned char,OFFSET_ONBOARD_SIZE> short8Dir;
    std::array<Offset, Offset32_SIZE> short_offsets;
    std::array<Offset, Offset32_SIZE> short_offsets_not_knight;
  public:
//    static const std::array<Offset, DIRECTION_SIZE> offsets;
//    static const std::array<int, DIRECTION_SIZE> dxs;
//    static const std::array<int, DIRECTION_SIZE> dys;
  private:
    template<Direction Dir>
    void setDirections();
    template<Direction Dir>
    void setKnightDirections();
    void init();
    /** @param P どちらのPlayerにとっての方向かを指定 */
    template <Player P>
    static Direction getLongDirection(Offset32 offset32)
    {
//      assert(isValid(offset32));
      Offset32 blackOffset32 = osl::blackOffset32(P,offset32);
      Direction ret=directions[I(blackOffset32)];
      assert(isValid(ret));
      return ret;
    }
    static Direction getLongDirection(Player P, Offset32 offset32)
    {
      if (P == BLACK) 
	return getLongDirection<BLACK>(offset32);
      else
	return getLongDirection<WHITE>(offset32);
    }
    /**
     * Longの利きの可能性のあるoffsetの場合は, 反復に使う offsetを
     * Shortの利きのoffsetの場合はそれ自身を返す.
     * 利きの可能性のない場合は0を返す
     */
    Offset getShortOffset(Offset32 offset32) const{
//      assert(isValid(offset32));
      return short_offsets[I(offset32)];
    }
  public:

    /**
     * next position from pos for player P.
     * 答えが isOnBoard とは限らない
     */
    Square nextSquare(Player P, Square pos, Direction dr) const
    {
      assert(isOnBoard(pos));
      const Offset offset = newOffset(P, dr);
      return pos + offset;
    }

    BoardTable();
    /** @param P どちらのPlayerにとっての方向かを指定 */
    static Direction getLongDirection(Player P,Square from, Square to)
    {
      return getLongDirection(P,newOffset32(to,from));
    }
    Offset getShortOffset(Square from, Square to) const{
//      assert(isValid(offset32));
      return short_offsets[I(newOffset32(to,from))];
    }
    /**
     * Longの利きの可能性のあるoffsetの場合は, 反復に使う offsetを
     * Knight以外のShortの利きのoffsetの場合はそれ自身を返す.
     * Knightの利き, 利きの可能性のない場合は0を返す
     */
    Offset getShortOffsetNotKnight(Offset32 offset32) const{
//      assert(isValid(offset32));
      return short_offsets_not_knight[I(offset32)];
    }
    Offset getShortOffsetNotKnight(Square from, Square to) const{
//      assert(isValid(offset32));
      return short_offsets_not_knight[I(newOffset32(to,from))];
    }
    /**
     * 8方向にいない場合も適当なものを返す．
     */
    Offset getShort8OffsetUnsafe(Square from,Square to) const{
      assert(isOnBoard(from) && isOnBoard(to));
      Offset o=to-from;
      return Offset(short8Offset[BI(o)]);
    }
    /**
     * 8方向にいない場合も適当なものを返す．
     */
    Direction getShort8Unsafe(Player P, Square from,Square to) const{
      assert(isOnBoard(from) && isOnBoard(to));
      return Direction(short8Dir[BI(P==BLACK ? to-from : from-to)]);
    }
    Direction getShort8(Player P, Square from,Square to) const{
      assert(isOnBoard(from));
      assert(isOnBoard(to));
      assert(X(from)==X(to) || Y(from)==Y(to) || 
	     abs(X(from)-X(to))==abs(Y(from)-Y(to)));
      return getShort8Unsafe(P, from, to);
    }
    Direction getShort8(Player P, Square from,Square to,Offset& o) const{
      assert(isOnBoard(from) && isOnBoard(to));
      assert(X(from)==X(to) || Y(from)==Y(to) || 
	     abs(X(from)-X(to))==abs(Y(from)-Y(to)));
      Offset o1 = to - from;
      o = Offset(short8Offset[BI(o1)]);
      Direction d = Direction(short8Dir[BI(o1)]);
      return (P == BLACK ? d : rotate180Short(d));
    }
    /**
     * p0, p1の間にtがあるかどうか.
     * (t,p0), (t,p1)のどちらかが8方向である時にのみ呼び出すこと
     * 他方も8方向でないとしたらknightの関係
     */
    bool isBetween(Square t,Square p0,Square p1) const
    {
      assert(isOnBoard(t) && isOnBoard(p0) && isOnBoard(p1));
      Offset o1=t-p0,o2=p1-t;
      return short8Dir[BI(o1)]==short8Dir[BI(o2)];
    }
    bool isBetweenSafe(Square t,Square p0,Square p1) const
    {
      assert(isOnBoard(t) && isOnBoard(p0) && isOnBoard(p1));
      if (getShortOffsetNotKnight(newOffset32(t, p0))==Offset::ZERO)
	return false;
      return isBetween(t, p0, p1);
    }
  };

  extern const BoardTable Board_Table;
} // namespace osl

/* centering5x3.h
 */
namespace osl
{
  /**
   * 5x3が盤上におさまるように中心を調整
   */
  struct Centering5x3
  {
    struct Table 
    {
      std::array<Square,Square_SIZE> centers;
      Table();
    };
    static Square adjustCenterNaive(Square);
    static const Table table;
    static Square adjustCenter(Square src)
    {
      return table.centers[I(src)];
    }
  };

} // namespace osl

/* effectContent.h
 */
namespace osl
{
  enum class EffectContent : int32_t {
    DIRECT=1,
      };
  constexpr EffectContent newEffectDirect(Offset offset){
    return EffectContent((int(offset) << 1)+1); 
  }
  constexpr EffectContent newEffect(Offset offset){
    return EffectContent((int(offset) << 1)); 
  }
  constexpr bool hasEffect(EffectContent effect) { return int(effect)!=0; }
  constexpr bool hasUnblockableEffect(EffectContent effect) { return (int(effect) & 1)!=0; }
  constexpr Offset offset(EffectContent effect) { return Offset(int(effect) >> 1); }
  constexpr bool hasBlockableEffect(EffectContent effect) { 
    return (int(effect) & (-int(effect)) & ~1) != 0;
  }
  

} // namespace osl

namespace osl
{
  class PtypeTable
  {
  private:
    // これらの2次元配列は2^nにそろえておいた方が速い．
    std::array<std::array<EffectContent, Offset32_SIZE>, PTYPEO_SIZE> effectTable;
    void init();
  public:
    PtypeTable();
    /** 
     * fromにいるptypeoがtoに利きを持つか?
     * @param ptypeo - 駒の種類
     * @param from - 駒の位置
     * @param to - 利きをチェックするマスの位置
     */
    EffectContent getEffect(PtypeO ptypeo,Square from, Square to) const
    {
      assert(isOnBoard(from) && isOnBoard(to));
      return getEffect(ptypeo,newOffset32(to,from));
    }
    const EffectContent& getEffect(PtypeO ptypeo,Offset32 offset32) const
    {
      assert(isValid(ptypeo));
      return effectTable[I(ptypeo)][I(offset32)];
    }
  };
  
  extern const PtypeTable Ptype_Table;
} // namespace osl


#if 0
/* handicap.h
 */
namespace osl{
  enum Handicap{
    HIRATE,
    //    KYOUOCHI,
    //    KAKUOCHI,
  };
}
#endif

/* moveVector.h
 */
namespace osl
{
  namespace container
  {
    class MoveVector : public FixedCapacityVector<Move,MaxUniqMoves>
    {
    public:
    };
    std::ostream& operator<<(std::ostream& os,MoveVector const& mv);
    bool operator<(const MoveVector& l, const MoveVector& r);
  } // namespace container
  using container::MoveVector;
} // namespace osl

/* pieceVector.h
 */
namespace osl
{
  namespace container
  {
    class PieceVector : public FixedCapacityVector<Piece,Piece_SIZE>
    {
    public:
      void sort();
    };
    std::ostream& operator<<(std::ostream& os,const PieceVector&);
  } // namespace container
  using container::PieceVector;
} // namespace osl

/* quadInt.h
 */

namespace osl
{
  namespace container
  {
    struct QuadInt
    {
      ALIGN(16)
      union XMM{
	std::array<int,4> iv;
	std::array<long long,2> llv;
#ifdef OSL_USE_SSE
	__m128i v128;
#endif
      } v;
      QuadInt(){
	clear();
      }
      QuadInt(QuadInt const& si){
#if OSL_USE_SSE
	v.v128=si.v.v128;
#else
	v.llv = si.v.llv;
#endif
      }
      QuadInt& operator=(QuadInt const& si) 
      {
#if OSL_USE_SSE
	v.v128=si.v.v128;
#else
	v.llv = si.v.llv;
#endif //OSL_USE_SSE
	return *this;
      }
      void clear()
      {
#if OSL_USE_SSE
	v.v128 = _mm_setzero_si128();
#else
	v.llv[0] = v.llv[1] = 0;
#endif
      }
      int& operator[](int i) { 
	return v.iv[i]; 
      }
      const int& operator[](int i) const { 
	return v.iv[i]; 
      }
      QuadInt operator-() const{
	QuadInt ret;
	ret -= *this;
	return ret;
      }
      QuadInt& operator+=(QuadInt const& si){
#if OSL_USE_SSE
	v.v128=_mm_add_epi32(v.v128,si.v.v128);
#else
	for(int i=0;i<4;i++) v.iv[i]+=si.v.iv[i];
#endif	  
	return *this;
      }
      void addFor(Player P, QuadInt const& si){
	if(P == BLACK) *this += si;
	else *this -= si;
      }
      void subFor(Player P, QuadInt const& si){
	if(P == BLACK) *this -= si;
	else *this += si;
      }
      QuadInt& operator-=(QuadInt const& si){
#if OSL_USE_SSE
	v.v128=_mm_sub_epi32(v.v128,si.v.v128);
#else
	for(int i=0;i<4;i++) v.iv[i]-=si.v.iv[i];
#endif	  
	return *this;
      }
      QuadInt& operator*=(int scale){
#if OSL_USE_SSE41
	v.v128 = _mm_mullo_epi32(v.v128, _mm_set_epi32(scale,scale,scale,scale));
#else
	for(int i=0;i<4;i++) v.iv[i]*=scale;
#endif
	return *this;
      }
      static size_t size() { return 4; }
    };
    inline QuadInt operator+(QuadInt const& si0,QuadInt const& si1)
    {
      QuadInt ret(si0);
      ret+=si1;
      return ret;
    }
    inline QuadInt operator-(QuadInt const& si0,QuadInt const& si1)
    {
      QuadInt ret(si0);
      ret-=si1;
      return ret;
    }
    inline QuadInt operator*(QuadInt const& si0,int scale)
    {
      QuadInt ret(si0);
      ret*=scale;
      return ret;
    }
    inline bool operator==(QuadInt const& l,QuadInt const& r)
    {
      return l.v.llv[0] == r.v.llv[0] && l.v.llv[1] == r.v.llv[1];
    }
    inline bool operator!=(QuadInt const& l,QuadInt const& r)
    {
      return l.v.llv[0] != r.v.llv[0] || l.v.llv[1] != r.v.llv[1];
    }
    inline bool operator<(QuadInt const& l,QuadInt const& r)
    {
      if (l.v.llv[0] != r.v.llv[0])
	return (l.v.llv[0] < r.v.llv[0]);
      return l.v.llv[1] < r.v.llv[1];
    }    
    inline std::ostream& operator<<(std::ostream& os, QuadInt const& q){
      return os << "{" << q.v.iv[0] << "," << q.v.iv[1] << "," << q.v.iv[2] << "," << q.v.iv[3] << "}";
    }
    class QuadIntPair
    {
    public:
      std::array<QuadInt,2> v;
    public:
      QuadIntPair() {}
      const QuadInt& operator[](int i) const{
	return v[i];
      }
      const QuadInt& operator[](Player pl) const{
	return v[I(pl)];
      }
      QuadInt& operator[](int i){
	return v[i];
      }
      QuadInt& operator[](Player pl){
	return v[I(pl)];
      }
      QuadIntPair& operator+=(QuadIntPair const& a){
	v[0]+=a.v[0];
	v[1]+=a.v[1];
	return *this;
      }
      QuadIntPair& operator-=(QuadIntPair const& a){
	v[0]-=a.v[0];
	v[1]-=a.v[1];
	return *this;
      }
      void addFor(Player P, QuadIntPair const& si){
	if(P == BLACK) *this += si;
	else *this -= si;
      }
      void subFor(Player P, QuadIntPair const& si){
	if(P == BLACK) *this -= si;
	else *this += si;
      }
    };
    inline QuadIntPair operator+(QuadIntPair const& si0,QuadIntPair const& si1)
    {
      QuadIntPair ret(si0);
      ret+=si1;
      return ret;
    }
    inline QuadIntPair operator-(QuadIntPair const& si0,QuadIntPair const& si1)
    {
      QuadIntPair ret(si0);
      ret-=si1;
      return ret;
    }
    inline bool operator==(QuadIntPair const& l,QuadIntPair const& r)
    {
      return l[0] == r[0] && l[1] == r[1];
    }
  }
  
  using container::QuadInt;
  using container::QuadIntPair;
  inline std::ostream& operator<<(std::ostream& os, QuadIntPair const& q){
    return os << "{" << q.v[0] << "," << q.v[1] << "}";
  }
}

#if 0
namespace osl{
  constexpr bool isValid(Progress16 progress16) { 
    return (int(progress16) >= 0) && (int(progress16) < 16);
  }
}
#endif

#endif /* OSL_TYPES_H */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; coding:utf-8
// ;;; End:
