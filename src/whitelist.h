#ifndef _OPENINGBOOK_WHITE_LIST_H
#define _OPENINGBOOK_WHITE_LIST_H

#include "osl_position.h"
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>
#include <set>

class WhiteListError : public std::runtime_error
{
public:
  WhiteListError(const std::string& msg)
    : std::runtime_error(msg)
  {}
};


class WhiteList
{
public:
  static osl::Move strToMove(const std::string& str, const osl::NumEffectState& state);

private:
  typedef std::unordered_map<std::string, std::set<osl::Move>> keys_t;
  keys_t keys;

public:
  WhiteList()
  {}

  /**
   * Load a white list data from a file_name. 
   * Return number of states registerd.
   */
  int load_index_file(const std::string& file_name);
  /**
   * Load a white list data from a stream.
   * Return number of states registered.
   */
  int load_index_file(std::istream& in);
  /**
   * Regist a state using a sequence of moves.
   * Return new if an insertion took place; false otherwise.
   */
  bool load_moves(const std::vector<std::string>& moves);
  /**
   * Regist a state using a key and move. 
   * Return new if an insertion took place; false otherwise.
   */
  bool add_state(const std::string& key, const osl::Move best_move);
  /**
   * Regist both states (black and white) using a state and move. 
   * Return new if an insertion took place; false otherwise.
   */
  bool add_both_states(const osl::NumEffectState& state, const osl::Move best_move);
  /**
   * Retrurn true if a state is in the list; false otherwise.
   */
  bool find(const osl::NumEffectState& state, std::set<osl::Move>& move) const;
  /**
   * Retrurn true if a key of a state is in the list; false otherwise.
   */
  bool find(const std::string& key, std::set<osl::Move>& move) const;
};

#endif

// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
