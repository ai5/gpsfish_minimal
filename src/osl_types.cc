#include "config.h"
#include "osl_types.h"
#include "osl_eval.h"

#include "osl_position.h"
#include <iostream>
#include <string>
#include <stdexcept>

/* player.cc */

std::ostream& osl::operator<<(std::ostream& os,Player player)
{
  if(player==BLACK)
    return os << "+";
  else
    return os << "-";
}

/* ptype.cc */

std::istream& osl::operator>>(std::istream& is, osl::Ptype& ptype)
{
  std::string s;
  is >> s;
  if (s == "Ptype::EMPTY")
    ptype = Ptype::EMPTY;
  else if (s == "Ptype::EDGE")
    ptype = Ptype::EDGE;
  else if (s == "PPAWN")
    ptype = PPAWN;
  else if (s == "PLANCE")
    ptype = PLANCE;
  else if (s == "PKNIGHT")
    ptype = PKNIGHT;
  else if (s == "PSILVER")
    ptype = PSILVER;
  else if (s == "PBISHOP")
    ptype = PBISHOP;
  else if (s == "PROOK")
    ptype = PROOK;
  else if (s == "KING")
    ptype = KING;
  else if (s == "GOLD")
    ptype = GOLD;
  else if (s == "PAWN")
    ptype = PAWN;
  else if (s == "LANCE")
    ptype = LANCE;
  else if (s == "KNIGHT")
    ptype = KNIGHT;
  else if (s == "SILVER")
    ptype = SILVER;
  else if (s == "BISHOP")
    ptype = BISHOP;
  else if (s == "ROOK")
    ptype = ROOK;
  else{
    std::cerr << "Incorrect input : " << s << std::endl;
    ptype = Ptype::EMPTY;
  }
  return is;
}

std::ostream& osl::operator<<(std::ostream& os,const osl::Ptype ptype)
{
  return os << ptypeNames[I(ptype)];
}
  
std::ostream& osl::operator<<(std::ostream& os,const osl::PtypeO ptypeO)
{
  if (isPiece(ptypeO))
    return os << "PtypeO(" << getOwner(ptypeO) << "," 
	      << getPtype(ptypeO) << ")";
  return os << "PtypeO(" << (int)ptypeO << "," << getPtype(ptypeO) << ")";
}

/* direction.cc */
namespace osl
{
  std::ostream& operator<<(std::ostream& os,const Direction d){
    static const char* names[]={
      "UL","U","UR","L",
      "DR","D","DL","R",
      "UUL","UUR","*","*","DDR","DDL","*","*",
      "LONG_UL","LONG_U","LONG_UR","LONG_L",
      "LONG_DR","LONG_D","LONG_DL","LONG_R",
    };
    return os << names[I(d)];
  }

}
/* square.cc */
std::ostream& osl::operator<<(std::ostream& os, Square square)
{
  if (isPieceStand(square))
    return os << "OFF";
  return os << "Square(" << X(square) << Y(square) << ")";
}

/* piece.cc */
namespace osl
{
  static_assert(sizeof(Piece) == 4,"piece size is 4");
} // namespace osl

std::ostream& osl::operator<<(std::ostream& os,const Piece piece)
{
  if (isPiece(piece))
    os << "Piece(" << owner(piece) << "," << ptype(piece) 
       << ",num=" << number(piece) 
       << "," << square(piece) << ')';
  else if (piece == Piece_EMPTY)
    os << "PIECE_EMPTY";
  else if (piece == Piece_EDGE)
    os << "PIECE_Ptype::EDGE";
  else
    os << "unkown piece?!";
  return os;
}

/* oslmove.cc */
bool osl::isValid(osl::Move m)
{
  if (! osl::isNormal(m))
    return false;
  const Square from = osl::from(m);
  if (! osl::isValid(from))
    return false;
  const Square to = osl::to(m);
  if (! isOnBoard(to))
    return false;
  return osl::isValid(ptype(m))
    && osl::isValid(osl::capturePtype(m))
    && osl::capturePtype(m)!=KING
    && osl::isValid(player(m));
}

std::ostream& osl::operator<<(std::ostream& os,const Move move)
{
  if (move == Move_DeclareWin)
    return os << "MOVE_DECLARE_WIN";
  if (isInvalid(move))
    return os << "MOVE_INVALID";
  if (isPass(move))
    return os << "MOVE_PASS";
  const Player turn = player(move);
  if (isValid(move))
  {
    if (isPieceStand(from(move))) 
    {
      os << "Drop(" << turn << "," << ptype(move) << "," << to(move) << ")";
    }
    else
    {
      const Ptype capture_ptype=capturePtype(move);
      os << "Move(" << turn << "," << ptype(move) << "," 
	 << from(move) << "->" << to(move) ;
      if (isPromotion(move))
	os << ",promote";
      if (capture_ptype != Ptype::EMPTY)
	os << ",capture=" << capture_ptype;
      os << ")";
    }
  }
  else
  {
    os << "InvalidMove " << from(move) << " " << to(move) 
       << " " << osl::ptypeO(move) << " " << oldPtypeO(move)
       << " " << isPromotion(move)
       << " " << capturePtype(move) << "\n";
  }
  return os;
}

osl::Move16 osl::toMove16(osl::Move m)
{
  if (isInvalid(m) || V(m)==0)
    return Move16::NONE;
  if (isDrop(m))
    return Move16(0x80+(uint16_t)ptype(m)+((SquareCompressor::compress(to(m)))<<8));
  if (isPromotion(m))
    return Move16(SquareCompressor::compress(from(m))+(SquareCompressor::compress(to(m))<<8)+0x8000);
  return Move16(SquareCompressor::compress(from(m))+(SquareCompressor::compress(to(m))<<8));
}

/* boardTable.cc 
 */
constexpr std::array<osl::Direction,osl::Offset32_SIZE> osl::BoardTable::directions;
template <osl::Direction Dir>
void osl::BoardTable::setDirections(){
  const int blackDx=blackDxs[I(Dir)];
  const int blackDy=blackDys[I(Dir)];
  Offset offset=newOffset(blackDx,blackDy);
  for(int i=1;i<=8;i++){
    int dx=i*blackDx;
    int dy=i*blackDy;
    Offset32 offset32=newOffset32(dx,dy);
    short_offsets[I(offset32)]=offset;
    short_offsets_not_knight[I(offset32)]=offset;
    short8Dir[BI(newOffset(dx,dy))]=
      (unsigned char)(longToShort(Dir));
    short8Offset[BI(newOffset(dx,dy))]=int(offset);
  }
}
template <osl::Direction Dir>
void osl::BoardTable::setKnightDirections(){
  const int dx=blackDxs[I(Dir)];
  const int dy=blackDys[I(Dir)];
  Offset32 offset32=newOffset32(dx,dy);
  Offset offset=newOffset(dx,dy);
  short_offsets[I(offset32)]=offset;
  short_offsets[I(-offset32)]= -offset;
}

void osl::BoardTable::init(){
  short8Dir.fill((unsigned char)(Direction::INVALID_VALUE));
  short8Offset.fill(int8_t(0));
  short_offsets_not_knight.fill(Offset(0));
  setDirections<LONG_UL>();
  setDirections<LONG_U>();
  setDirections<LONG_UR>();
  setDirections<LONG_L>();
  setDirections<LONG_R>();
  setDirections<LONG_DL>();
  setDirections<LONG_D>();
  setDirections<LONG_DR>();
  setKnightDirections<UUL>();
  setKnightDirections<UUR>();
}

osl::BoardTable::BoardTable(){
  init();
}

osl::Centering5x3::
Table::Table()
{
  centers.fill(Square::STAND);
  for (int y=1; y<=9; ++y)
  {
    for (int x=1; x<=9; ++x)
    {
      const Square src = newSquare(x,y);
      centers[I(src)] = adjustCenterNaive(src);
    }
  }
}

namespace
{
  int adjustCenterX(int x)
  {
    if (x < 3)
      return 3;
    else if (x > 7)
      return 7;
    return x;
  }
  int adjustCenterY(int y)
  {
    if (y == 1)
      return y+1;
    else if (y == 9)
      return y-1;
    return y;
  }
} // anonymous namespace

osl::Square osl::
Centering5x3::adjustCenterNaive(Square src)
{
  const int x = adjustCenterX(X(src));
  const int y = adjustCenterY(Y(src));
  return newSquare(x, y);
}

/* ptypeTable.cc
 */
osl::PtypeTable::PtypeTable()
{
  init();
  
}
void osl::PtypeTable::init()
{
  for(auto& v: effectTable) v.fill(EffectContent(0));
  static_assert(sizeof(EffectContent) == 4,"EffectContent size is 4");
  assert(! hasEffect(getEffect(newPtypeO(BLACK,ROOK), newOffset32(-1,8))));

  for(Ptype ptype=Ptype::MIN;ptype<=Ptype::MAX;++ptype){
    for(Direction j=Direction::MIN;j<=Direction::MAX;j++){
      Direction dir=static_cast<Direction>(j);

      if((moveMasks[I(ptype)]&(1<<dir))!=0){
	int dx=blackDxs[I(dir)];
	int dy=blackDys[I(dir)];
	Offset32 offset32=newOffset32(dx,dy);
	Offset offset=newOffset(dx,dy);
	if(isLong(dir)){
	  effectTable[I(ptype)-int(PtypeO::MIN)][I(offset32)]=newEffectDirect(offset);
	  effectTable[I(ptype)-16-int(PtypeO::MIN)][I(-offset32)]=newEffectDirect(-offset);

	  for(int i=2;i<9;i++){
	    offset32=newOffset32(dx*i,dy*i);
	    effectTable[I(ptype)-int(PtypeO::MIN)][I(offset32)]=newEffect(offset);
	    effectTable[I(ptype)-16-int(PtypeO::MIN)][I(-offset32)]=newEffect(-offset);

	  }
	}
	else{
	  effectTable[I(ptype)-int(PtypeO::MIN)][I(offset32)]=EffectContent::DIRECT;
	  effectTable[I(ptype)-16-int(PtypeO::MIN)][I(-offset32)]=EffectContent::DIRECT;

	  assert(! hasEffect(getEffect(newPtypeO(BLACK,ROOK),newOffset32(-1,8))));
	}
      }
    }
  }
}

void osl::container::PieceVector::sort()
{
  std::sort(begin(),end(),[](Piece p0,Piece p1){
      int v0 = captureValue(ptype(p0));
      int v1 = captureValue(ptype(p1));
      if(v0 != v1) return v0 < v1;
      return uint32_t(I(square(p0))) < uint32_t(I(square(p1)));
    });
}
std::ostream& osl::operator<<(std::ostream& os,const osl::PieceMask& piece)
{
  return os << V(piece);
}

// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
