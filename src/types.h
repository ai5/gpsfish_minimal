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

#if !defined(TYPES_H_INCLUDED)
#define TYPES_H_INCLUDED

#include "osl_types.h"
//#include "ptypeEvalTraits.h"
#include <climits>
#include <cstdlib>

#if defined(_MSC_VER)

// Disable some silly and noisy warning from MSVC compiler
#pragma warning(disable: 4800) // Forcing value to bool 'true' or 'false'
#pragma warning(disable: 4127) // Conditional expression is constant
#pragma warning(disable: 4146) // Unary minus operator applied to unsigned type

// MSVC does not support <inttypes.h>
typedef   signed __int8    int8_t;
typedef unsigned __int8   uint8_t;
typedef   signed __int16  int16_t;
typedef unsigned __int16 uint16_t;
typedef   signed __int32  int32_t;
typedef unsigned __int32 uint32_t;
typedef   signed __int64  int64_t;
typedef unsigned __int64 uint64_t;

#else

#include <inttypes.h>

#endif

#define Min(x, y) (((x) < (y)) ? (x) : (y))
#define Max(x, y) (((x) < (y)) ? (y) : (x))

////
//// Configuration
////

//// For Linux and OSX configuration is done automatically using Makefile.
//// To get started type "make help".
////
//// For windows part of the configuration is detected automatically, but
//// some switches need to be set manually:
////
//// -DNDEBUG       | Disable debugging mode. Use always.
////
//// -DNO_PREFETCH  | Disable use of prefetch asm-instruction. A must if you want the
////                | executable to run on some very old machines.
////
//// -DUSE_POPCNT   | Add runtime support for use of popcnt asm-instruction.
////                | Works only in 64-bit mode. For compiling requires hardware
////                | with popcnt support. Around 4% speed-up.
////
//// -DOLD_LOCKS    | By default under Windows are used the fast Slim Reader/Writer (SRW)
////                | Locks and Condition Variables: these are not supported by Windows XP
////                | and older, to compile for those platforms you should enable OLD_LOCKS.

// Automatic detection for 64-bit under Windows
#if defined(_WIN64)
#define IS_64BIT
#endif

// Automatic detection for use of bsfq asm-instruction under Windows
#if defined(_WIN64)
#define USE_BSFQ
#endif

// Intel header for _mm_popcnt_u64() intrinsic
#if defined(USE_POPCNT) && defined(_MSC_VER) && defined(__INTEL_COMPILER)
#include <nmmintrin.h>
#endif

// Cache line alignment specification
#if defined(_MSC_VER) || defined(__INTEL_COMPILER)
#define CACHE_LINE_ALIGNMENT __declspec(align(64))
#else
#define CACHE_LINE_ALIGNMENT  __attribute__ ((aligned(64)))
#endif

// Define a __cpuid() function for gcc compilers, for Intel and MSVC
// is already available as an intrinsic.
#if !defined(__GNUC__) & defined(_MSC_VER)
#include <intrin.h>
#elif !defined(__ANDROID__) && defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
inline void __cpuid(int CPUInfo[4], int InfoType)
{
  int* eax = CPUInfo + 0;
  int* ebx = CPUInfo + 1;
  int* ecx = CPUInfo + 2;
  int* edx = CPUInfo + 3;

  *eax = InfoType;
  *ecx = 0;
  __asm__("cpuid" : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
                  : "0" (*eax), "2" (*ecx));
}
#else
inline void __cpuid(int CPUInfo[4], int)
{
   CPUInfo[0] = CPUInfo[1] = CPUInfo[2] = CPUInfo[3] = 0;
}
#endif

// Define FORCE_INLINE macro to force inlining overriding compiler choice
#if defined(_MSC_VER)
#define FORCE_INLINE  __forceinline
#elif defined(__GNUC__)
#define FORCE_INLINE  inline __attribute__((always_inline))
#else
#define FORCE_INLINE  inline
#endif

/// cpu_has_popcnt() detects support for popcnt instruction at runtime
inline bool cpu_has_popcnt() {

  int CPUInfo[4] = {-1};
  __cpuid(CPUInfo, 0x00000001);
  return (CPUInfo[2] >> 23) & 1;
}

/// CpuHasPOPCNT is a global constant initialized at startup that
/// is set to true if CPU on which application runs supports popcnt
/// hardware instruction. Unless USE_POPCNT is not defined.
#if defined(USE_POPCNT)
const bool CpuHasPOPCNT = cpu_has_popcnt();
#else
const bool CpuHasPOPCNT = false;
#endif


/// CpuIs64Bit is a global constant initialized at compile time that
/// is set to true if CPU on which application runs is a 64 bits.
#if defined(IS_64BIT)
const bool CpuIs64Bit = true;
#else
const bool CpuIs64Bit = false;
#endif

#include <string>

typedef uint64_t Key;

const int PLY_MAX = 100;
const int PLY_MAX_PLUS_2 = PLY_MAX + 2;

enum ValueType {
  VALUE_TYPE_NONE  = 0,
  VALUE_TYPE_UPPER = 1,
  VALUE_TYPE_LOWER = 2,
  VALUE_TYPE_EXACT = VALUE_TYPE_UPPER | VALUE_TYPE_LOWER
};

enum Value {
  VALUE_ZERO      = 0,
  VALUE_DRAW      = 0,
  VALUE_KNOWN_WIN = 15000,
  VALUE_MATE      = 30000,
  VALUE_INFINITE  = 30001,
  VALUE_NONE      = 30002,

  VALUE_MATE_IN_PLY_MAX  =  VALUE_MATE - PLY_MAX,
  VALUE_MATED_IN_PLY_MAX = -VALUE_MATE + PLY_MAX,

  VALUE_ENSURE_INTEGER_SIZE_P = INT_MAX,
  VALUE_ENSURE_INTEGER_SIZE_N = INT_MIN
};


typedef osl::Ptype PieceType;
// typedef osl::PtypeO Piece;
// typedef osl::Player Player;
using osl::Ptype;
using osl::PtypeO;
using osl::Player;
using osl::BLACK;
using osl::WHITE;
using osl::PTYPEO_SIZE;
const PieceType PIECE_TYPE_NONE=osl::Ptype::EMPTY;

enum Depth {

  ONE_PLY = 2,

  DEPTH_ZERO         =  0 * ONE_PLY,
  DEPTH_QS_CHECKS    = -1 * ONE_PLY,
  DEPTH_QS_NO_CHECKS = -2 * ONE_PLY,

  DEPTH_NONE = -127 * ONE_PLY
};

typedef osl::Square Square;

enum File {
  FILE_0, FILE_1, FILE_2, FILE_3, FILE_4, FILE_5, FILE_6, FILE_7, FILE_8, FILE_9
};

enum Rank {
  RANK_0, RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_9
};

enum SquarePlayer {
  DARK, LIGHT
};

enum ScaleFactor {
  SCALE_FACTOR_ZERO   = 0,
  SCALE_FACTOR_NORMAL = 64,
  SCALE_FACTOR_MAX    = 128,
  SCALE_FACTOR_NONE   = 255
};


/// Score enum keeps a midgame and an endgame value in a single
/// integer (enum), first LSB 16 bits are used to store endgame
/// value, while upper bits are used for midgame value. Compiler
/// is free to choose the enum type as long as can keep its data,
/// so ensure Score to be an integer type.
enum Score {
    SCORE_ZERO = 0,
    SCORE_ENSURE_INTEGER_SIZE_P = INT_MAX,
    SCORE_ENSURE_INTEGER_SIZE_N = INT_MIN
};

#define ENABLE_OPERATORS_ON(T) \
inline T operator+ (const T d1, const T d2) { return T(int(d1) + int(d2)); } \
inline T operator- (const T d1, const T d2) { return T(int(d1) - int(d2)); } \
inline T operator* (int i, const T d) {  return T(i * int(d)); } \
inline T operator* (const T d, int i) {  return T(int(d) * i); } \
inline T operator/ (const T d, int i) { return T(int(d) / i); } \
inline T operator- (const T d) { return T(-int(d)); } \
inline T operator++ (T& d, int) {d = T(int(d) + 1); return d; } \
inline T operator-- (T& d, int) { d = T(int(d) - 1); return d; } \
inline void operator+= (T& d1, const T d2) { d1 = d1 + d2; } \
inline void operator-= (T& d1, const T d2) { d1 = d1 - d2; } \
inline void operator*= (T& d, int i) { d = T(int(d) * i); } \
inline void operator/= (T& d, int i) { d = T(int(d) / i); }

ENABLE_OPERATORS_ON(Value)
// ENABLE_OPERATORS_ON(PieceType)
//ENABLE_OPERATORS_ON(Piece)
ENABLE_OPERATORS_ON(Player)
ENABLE_OPERATORS_ON(Depth)
ENABLE_OPERATORS_ON(File)
ENABLE_OPERATORS_ON(Rank)

#undef ENABLE_OPERATORS_ON

// Extra operators for adding integers to a Value
inline Value operator+ (Value v, int i) { return Value(int(v) + i); }
inline Value operator- (Value v, int i) { return Value(int(v) - i); }

// Extracting the _signed_ lower and upper 16 bits it not so trivial
// because according to the standard a simple cast to short is
// implementation defined and so is a right shift of a signed integer.
inline Value mg_value(Score s) { return Value(((int(s) + 32768) & ~0xffff) / 0x10000); }

// Unfortunatly on Intel 64 bit we have a small speed regression, so use a faster code in
// this case, although not 100% standard compliant it seems to work for Intel and MSVC.
#if defined(IS_64BIT) && (!defined(__GNUC__) || defined(__INTEL_COMPILER))
inline Value eg_value(Score s) { return Value(int16_t(s & 0xffff)); }
#else
inline Value eg_value(Score s) { return Value((int)(unsigned(s) & 0x7fffu) - (int)(unsigned(s) & 0x8000u)); }
#endif

inline Score make_score(int mg, int eg) { return Score((mg << 16) + eg); }

// Division must be handled separately for each term
inline Score operator/(Score s, int i) { return make_score(mg_value(s) / i, eg_value(s) / i); }

// Only declared but not defined. We don't want to multiply two scores due to
// a very high risk of overflow. So user should explicitly convert to integer.
inline Score operator*(Score s1, Score s2);

// Remaining Score operators are standard
inline Score operator+ (const Score d1, const Score d2) { return Score(int(d1) + int(d2)); }
inline Score operator- (const Score d1, const Score d2) { return Score(int(d1) - int(d2)); }
inline Score operator* (int i, const Score d) {  return Score(i * int(d)); }
inline Score operator* (const Score d, int i) {  return Score(int(d) * i); }
inline Score operator- (const Score d) { return Score(-int(d)); }
inline void operator+= (Score& d1, const Score d2) { d1 = d1 + d2; }
inline void operator-= (Score& d1, const Score d2) { d1 = d1 - d2; }
inline void operator*= (Score& d, int i) { d = Score(int(d) * i); }
inline void operator/= (Score& d, int i) { d = Score(int(d) / i); }

#if 0
const Value PawnValueMidgame   = (Value)osl::eval::PtypeEvalTraits<osl::PAWN>::val;
#else
const Value PawnValueMidgame   = (Value)osl::evVal[I(osl::PAWN)];
#endif

inline Value value_mate_in(int ply) {
  return VALUE_MATE - ply;
}

inline Value value_mated_in(int ply) {
  return -VALUE_MATE + ply;
}

inline PtypeO make_piece(Player c, PieceType pt) {
  return newPtypeO(c,pt);
}

inline PieceType type_of_piece(PtypeO p)  {
  return getPtype(p);
}

inline Player color_of_piece(PtypeO p) {
  return getOwner(p);
}

inline Player opposite_color(Player c) {
  return alt(c);
}

inline bool color_is_ok(Player c) {
  return isValid(c);
}

inline bool piece_type_is_ok(PieceType pt) {
  return isPiece(pt);
}

inline bool piece_is_ok(PtypeO p) {
  return piece_type_is_ok(type_of_piece(p)) && color_is_ok(color_of_piece(p));
}

inline char piece_type_to_char(PieceType pt) {
  static const char ch[] = "  plnsbrGKPLNSBR";
  return ch[static_cast<int>(pt)];
}

inline Square make_square(File f, Rank r) {
  return osl::newSquare(f,r);
}

inline File square_file(Square s) {
  return File(X(s));
}

inline Rank square_rank(Square s) {
  return Rank(Y(s));
}

inline Square flip_square(Square s) {
  // For shogi, do rotate180 instead of flipping
  return rotate180(s);
}

inline Square flop_square(Square s) {
  // flipHorizontal is expensive because it checks if s is pieceStand
  return flipHorizontal(s);
}

inline Square relative_square(Player c, Square s) {
  return forBlack(c,s);
}

inline Rank relative_rank(Player c, Rank r) {
  return (c==BLACK ? r : Rank(10-int(r)) );
}

inline Rank relative_rank(Player c, Square s) {
  return relative_rank(c, square_rank(s));
}

inline int file_distance(Square s1, Square s2) {
  return abs(int(square_file(s1))- int(square_file(s2)));
}

inline int rank_distance(Square s1, Square s2) {
  return abs(int(square_rank(s1)) - int(square_rank(s2)));
}

inline int square_distance(Square s1, Square s2) {
  return Max(file_distance(s1, s2), rank_distance(s1, s2));
}

inline File file_from_char(char c) {
  return FILE_9-File(c - 'a');
}

inline char file_to_char(File f) {
  return char(int('a')+FILE_9-f);
}

inline Rank rank_from_char(char c) {
  return Rank(c - '1') + RANK_1;
}

inline char rank_to_char(Rank r) {
  return char(r - RANK_1 + int('1'));
}

inline const std::string square_to_string(Square s) {
  char ch[] = { file_to_char(square_file(s)), rank_to_char(square_rank(s)), 0 };
  return std::string(ch);
}

inline bool file_is_ok(File f) {
  return f >= FILE_1 && f <= FILE_9;
}

inline bool rank_is_ok(Rank r) {
  return r >= RANK_1 && r <= RANK_9;
}

inline bool square_is_ok(Square s) {
  return isOnBoard(s);
}

#endif // !defined(TYPES_H_INCLUDED)
