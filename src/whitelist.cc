#include "whitelist.h"
#include "osl_types.h"
#include "usi.h"
#include <fstream>
#include <iostream>
#include <sstream>

namespace {
  Ptype csaStrToPtype(const std::string& str)
  {
      if (str == "FU")
        return Ptype::PAWN;
      else if (str == "KY")
        return Ptype::LANCE;
      else if (str == "KE")
        return Ptype::KNIGHT;
      else if (str == "GI")
        return Ptype::SILVER;
      else if (str == "KI")
        return Ptype::GOLD;
      else if (str == "KA")
        return Ptype::BISHOP;
      else if (str == "HI")
        return Ptype::ROOK;
      else if (str == "OU")
        return Ptype::KING;
      else if (str == "TO")
        return Ptype::PPAWN;
      else if (str == "NY")
        return Ptype::PLANCE;
      else if (str == "NK")
        return Ptype::PKNIGHT;
      else if (str == "NG")
        return Ptype::PSILVER;
      else if (str == "UM")
        return Ptype::PBISHOP;
      else if (str == "RY")
        return Ptype::PROOK;
      else {
        std::cerr << "Invalid piece str: " << str << std::endl;
        exit(1);
      }

      return Ptype::EMPTY;
  }

  void trim(std::string& str) {
    if (str.empty())
      return;

    const static std::string whitespaces = " \t\f\v\n\r";
    const int start = str.find_first_not_of(whitespaces);
    str.erase(0,start);
    const int end   = str.find_last_not_of(whitespaces);
    str.erase(end);
  }
}

osl::Move 
WhiteList::strToMove(const std::string& str,
                     const osl::NumEffectState& state)
{
   if (str == "%KACHI")
     return Move::DECLARE_WIN;
   if (str == "%TORYO")
     return Move::INVALID_VALUE;
   if (str == "%PASS")             // FIXME: not in CSA protocol
     return PASS(state.turn());

    if (str.size() != 7) {
      std::cerr << "Invalid move str: " << str << std::endl;
      exit(1);
    }
    Player player = str[0] == '+' ? BLACK : WHITE;
    static const int offset = 48;
    const int from_x = str[1]-offset;
    const int from_y = str[2]-offset;
    const int to_x   = str[3]-offset;
    const int to_y   = str[4]-offset;
    const Square to = osl::newSquare(to_x, to_y);
    const std::string koma = str.substr(5);
    const Ptype pt = csaStrToPtype(koma);

    if (from_x == 0) {
      // drop
      return newMove(to, pt, player);
    }

    const Square from = osl::newSquare(from_x, from_y);
    const bool is_promote = pt != ptype(state.pieceAt(from));

    return newMove(from,
                   to,
                   ptype(state.pieceAt(from)),
                   ptype(state.pieceAt(to)),
                   is_promote,
                   player);
}

bool 
WhiteList::find(const osl::NumEffectState& state, std::set<osl::Move>& move) const
{
  const std::string key = osl::usi::show(state);
  return find(key, move);
}

bool 
WhiteList::find(const std::string& key, std::set<osl::Move>& move) const
{
  const keys_t::const_iterator ret = keys.find(key);
  if (ret == keys.end()) {
    return false;
  } else {
    move = ret->second;
    return true;
  }
}

bool 
WhiteList::add_state(const std::string& key, const osl::Move best_move)
{
  { /* check if the key is new */
    std::set<osl::Move> moves;
    if (find(key, moves)) {
      if (moves.count(best_move)) {
        std::ostringstream msg;
        msg << "WARNING: Duplidated state and move found ignored: " << key << " " << best_move << "\n";
        std::cerr << msg.str();
      } else {
        std::ostringstream msg;
        msg << "ERROR: Duplidated state found: " << key << " " << best_move << "\n";
        std::cerr << msg.str();
      }
    }
  }

std::cerr << key << " " << best_move << std::endl;

  keys[key].insert(best_move);
  return true;
}

bool 
WhiteList::add_both_states(const  osl::NumEffectState& state, const osl::Move best_move)
{
  const bool ret1 = add_state(osl::usi::show(state), best_move);
  //const bool ret2 = add_state(osl::usi::show(state.rotate180()), rotate(best_move));
  //assert(ret1 == ret2);

  //return ret1 && ret2;
  return ret1;
}

int WhiteList::load_index_file(const std::string& file_name)
{
  std::ifstream in(file_name.c_str());
  if (!in) {
    std::cerr << "File not found: " << file_name << "\n";
    return 0;
  }
  return load_index_file(in);
}

int 
WhiteList::load_index_file(std::istream& in)
{
  int count=0;
  std::string line;
  while (std::getline(in, line)) {
    trim(line);
    if (line.empty() || line[0] == '#')
      continue;

    std::vector<std::string> moves;
    std::istringstream is(line);
    std::string word;
    while (is >> word) {
      moves.push_back(word);
    }
    load_moves(moves);
    ++count;
  }

  std::cout << count << " states were loaded into the white list." << std::endl;
  return count;
}

bool 
WhiteList::load_moves(const std::vector<std::string>& strmoves)
{
  if (strmoves.empty()) {
    std::cerr << "WARNING: Skipped an empty sequece.\n";
    return false;
  }

  std::vector<osl::Move> moves;
  osl::NumEffectState state = osl::usi::makeState("startpos");
  osl::NumEffectState prev = state;

  for (const std::string& str : strmoves) {
    const osl::Move move = strToMove(str, state);
    if (!state.isValidMove(move)) {
      std::cerr << "Invalid move: " << str << std::endl;
      exit(1);
    }
    moves.push_back(move);
    prev = state;
    state.makeMove(move);
  }

  /** The last move is the best move */
  return add_both_states(prev, moves.back());
}

// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
