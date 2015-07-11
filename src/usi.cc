#include "usi.h"
#include "osl_position.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <cctype>

/* psn.cc
 */

const std::string osl::psn::
show(Square pos)
{
  const int x = X(pos);
  const int y = Y(pos);
  std::string result = "XX";
  result[0] = x + '0';
  result[1] = y + 'a' - 1;
  return result;
}

char osl::psn::
show(Ptype ptype)
{
  switch (ptype)
  {
  case PAWN:	return 'P';
  case LANCE:	return 'L';
  case KNIGHT:	return 'N';
  case SILVER:	return 'S';
  case GOLD:	return 'G';
  case BISHOP:	return 'B';
  case ROOK:	return 'R';
  case KING:	return 'K';
  default:
    assert("unsupported ptype" == 0);
    return '!';
  }
}

const std::string osl::psn::
show(Move m)
{
  const Square from = osl::from(m);
  const Square to = osl::to(m);
  if (isPieceStand(from))
  {
    std::string result = "X*";
    result[0] = show(osl::ptype(m));
    result += show(to);
    return result;
  }
  std::string result = show(from);
  result += show(to);
  if (isPromotion(m))
    result += '+';
  return result;
}

const std::string osl::psn::
showXP(Move m)
{
  if (isInvalid(m))
    return "resign";  
  if (isPass(m))
    return "pass";
  const Square from = osl::from(m);
  const Square to = osl::to(m);
  if (isPieceStand(from))
  {
    std::string result = "X*";
    result[0] = show(osl::ptype(m));
    result += show(to);
    return result;
  }
  std::string result = show(from);
  if (capturePtype(m) != Ptype::EMPTY)
    result += 'x';
  result += show(to);
  if (isPromotion(m))
    result += '+';
  else if (canPromote(osl::ptype(m))
	   && (canPromote(player(m),from) || canPromote(player(m),to)))
    result += '=';
  return result;
}


osl::Move osl::psn::
strToMove(const std::string& str, const osl::NumEffectState& s)
{
  if (str.size() < 4)
    throw ParseError("Invalid move string: " + str);

  const Square to = strToPos(str.substr(2,2));
  if (str[1] == '*')
  {
    const Ptype ptype = charToPtype(str[0]);
    return newMove(to, ptype, s.turn());
  }

  const Square from = strToPos(str.substr(0,2));
  const Ptype ptype = osl::ptype(s[from]);
  const Ptype captured = osl::ptype(s[to]);
  if (! isPiece(ptype))
    throw ParseError("No piece on square: " + str);    
  bool promotion = false;
  if (str.size() > 4)
  {
    assert(str[4] == '+');
    promotion = true;
  }
#if 0
  return newMove(from, to, (promotion ? promote(ptype) : ptype), 
	      captured, promotion, s.turn());
#else
  return newMove(from, to, ptype, captured, promotion, s.turn());
#endif
}

osl::Square osl::psn::
strToPos(const std::string& str)
{
  assert(str.size() == 2);
  const int x = str[0] - '0';
  const int y = str[1] - 'a' + 1;
  if (x <= 0 || x > 9 || y <= 0 || y > 9)
    throw ParseError("Invalid square character: " + str);
  return newSquare(x, y);
}

osl::Ptype osl::psn::
charToPtype(char c)
{
  switch (c) 
  {
  case 'P': return PAWN;
  case 'L': return LANCE;
  case 'N': return KNIGHT;
  case 'S': return SILVER;
  case 'G': return GOLD;
  case 'B': return BISHOP;
  case 'R': return ROOK;
  case 'K': return KING;
  default:
    return Ptype::EMPTY;
  }
}

/* usi.cc
 */

const std::string osl::usi::
show(Move m)
{
  if (isPass(m))
    return "pass";
  if (m == Move_DeclareWin)
    return "win";
  if (! isNormal(m))
    return "resign";
  return psn::show(m);
}

const std::string osl::usi::
show(PtypeO ptypeo)
{
  if (! isPiece(ptypeo))
    return "";

  char c = psn::show(unpromote(getPtype(ptypeo)));
  if (getOwner(ptypeo) == WHITE)
    c = tolower(c);
  std::string ret(1,c);
  if (isPromoted(getPtype(ptypeo)))
    ret = "+" + ret;
  return ret;
}

const std::string osl::usi::
show(Piece p)
{
  return show(osl::ptypeO(p));
}

const std::string osl::usi::
show(const NumEffectState& state)
{
  std::ostringstream ret;
  ret << "sfen ";
  for (int y=1; y<=9; ++y) {
    int empty_count = 0;
    for (int x=9; x>=1; --x) {
      const Piece p = state[newSquare(x,y)];
      if (isEmpty(p)) {
	++empty_count;
	continue;
      }
      if (empty_count) {
	ret << empty_count;
	empty_count = 0;
      }
      ret << show(p);
    }
    if (empty_count)
      ret << empty_count;
    if (y < 9) ret << "/";
  }
  ret << " " << "bw"[state.turn() == WHITE] << " ";
  bool has_any = false;
  for (Player pl : COLORS) {
    for(Ptype ptype : PieceStandOrder) {
      const int count = state.countPiecesOnStand(pl, ptype);
      if (count == 0)
	continue;
      if (count > 1)
	ret << count;
      ret << show(newPtypeO(pl, ptype));
      has_any = true;
    }
  }
  if (! has_any)
    ret << "-";
  ret << " 1";
  if(ret.str() == "sfen "+StartPositionFEN) return "startpos";
  return ret.str();
}

osl::Move osl::usi::
strToMove(const std::string& str, const NumEffectState& s)
{
  if (str == "win")
    return Move_DeclareWin;
  if (str == "pass" || str == "0000")
    return PASS(s.turn());
  if (str == "resign")
    return Move_INVALID;
  try {
    return psn::strToMove(str, s);
  }
  catch (std::exception& e) {
    throw ParseError("usi::strToMove failed for " + str + " by "+ e.what());
  }
  catch (...) {
    throw ParseError("usi::strToMove failed for " + str);
  }
}

osl::PtypeO osl::usi::
charToPtypeO(char c)
{
  const Ptype ptype = psn::charToPtype(toupper(c));
  if (ptype == Ptype::EMPTY)
    throw ParseError("Invalid piece character: " + std::string(1,c));
  const Player pl = isupper(c) ? BLACK : WHITE;
  return newPtypeO(pl, ptype);
}

void osl::usi::parseBoard(const std::string& word, NumEffectState& state)
{
  if (word.empty())
    throw ParseError(word);

  state.init();
  int x=9, y=1;
  for (size_t i=0; i<word.size(); ++i) {
    const char c = word[i];
    if (isalpha(c)) {
      const PtypeO ptypeo = charToPtypeO(c);
      state.setPiece(getOwner(ptypeo), newSquare(x,y), getPtype(ptypeo));
      --x;
    } else if (c == '+') {
      if ( (i+1) >= word.size() )
        throw ParseError(word);
      const char next = word[i+1];
      if (!isalpha(next))
        throw ParseError(word);
      const PtypeO ptypeo = charToPtypeO(next);
      if (!canPromote(ptypeo))
        throw ParseError(word);
      const PtypeO promoted = promote(ptypeo);
      state.setPiece(getOwner(promoted), newSquare(x,y), getPtype(promoted));
      --x;
      ++i;
    } else if (c == '/') {
      if (x != 0)
        throw ParseError(word);
      x = 9;
      ++y;
    } else if (isdigit(c)) {
      const int n = c - '0';
      if (n == 0)
        throw ParseError(word);
      x -= n;
    } else {
      throw ParseError("usi: unknown input " + std::string(1,c));
    }
    if (x < 0 || x > 9 || y < 0 || y > 9)
      throw ParseError(word);
  }
}

void osl::usi::parse(const std::string& line, NumEffectState& state)
{
  std::vector<Move> moves;
  parse(line, state, moves);
  state.initEffects();
  for(Move move : moves) {
    state.makeMove(move);
  }
}

osl::NumEffectState osl::usi::makeState(const std::string& line){
  osl::NumEffectState state;
  parse(line,state);
  return state;
}

void osl::usi::parse(std::string const& l, NumEffectState& state, std::vector<Move>& moves)
{
  moves.clear();
  std::string line(l);
  if(line == "startpos") line = "sfen " + StartPositionFEN; 
  std::istringstream is(line);
  std::string word;
  is >> word;
  if (word == "position")
    is >> word;
//  if (word == "startpos") 
//    word=StartPositionFEN;
  if (word != "sfen")
    throw ParseError("sfen not found "+word);
  is >> word;
  parseBoard(word, state);
  is >> word;
  if (word != "b" && word != "w")
    throw ParseError(" turn error "+word);
  state.setTurn((word == "b") ? BLACK : WHITE);
  is >> word;
  if (word != "-") {
    int prefix = 0;
    for(char c : word) {
      if (isalpha(c)) {
	PtypeO ptypeo = charToPtypeO(c);
	for (int j=0; j<std::max(1, prefix); ++j)
	  state.setPiece(getOwner(ptypeo), Square::STAND, getPtype(ptypeo));
	prefix = 0;
      }
      else {
	if (!isdigit(c))
	  throw ParseError(word);
	prefix = (c - '0') + prefix*10;
	if (prefix == 0)
	  throw ParseError(word);
      }
    }
  }
  state.initEffects();
  int move_number; // will not be used
  if (! (is >> move_number))
    return;
  assert(is);
  if (! (is >> word))
    return;
  if (word != "moves")
    throw ParseError("moves not found "+word);
  NumEffectState state_copy(state);
  while (is >> word) {
    Move m = strToMove(word, state_copy);
    moves.push_back(m);
    if (! isNormal(m) || ! state_copy.isValidMove(m))
      throw ParseError("invalid move "+word);
    state_copy.makeMove(m);
  }
} 

/* ------------------------------------------------------------------------- */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
