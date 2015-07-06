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

#if !defined(BOOK_H_INCLUDED)
#define BOOK_H_INCLUDED

#include <string>

#include "move.h"
#include "position.h"
#include <unordered_map>
using namespace std;


struct BookEntry {
  static const int UNDEF = -10000001;
  vector<pair<string, int>> moves;
  vector<pair<string, int>> bestMoves;
  string otherMove;
  int otherScore, bestScore;
BookEntry(vector<string> const& moves_, string const& otherMove_, int otherScore_) : otherMove(otherMove_), otherScore(otherScore_), bestScore(UNDEF) {
    for(string s : moves_) moves.push_back(make_pair(s, -1));
  }
};
class Book {
  static unordered_map<string, int> sfens;
  static vector<BookEntry> contents;
public:
  static bool setUp();
#if 0
  Book();
  ~Book();
  void open(const std::string& fileName);
  void close();
#endif
  static Move get_move(const Position& pos, bool findBestMove);
  static void make_book(int argc, char *argv[]);
  static void make_weighted_book(string const& src_file, string const& dst_file);
};

#endif // !defined(BOOK_H_INCLUDED)
