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


/*
  The code in this file is based on the opening book code in PolyGlot
  by Fabien Letouzey. PolyGlot is available under the GNU General
  Public License, and can be downloaded from http://wbec-ridderkerk.nl
*/

#include "book.h"
#include "misc.h"
#include "usi.h"
#include "whitelist.h"
#include <fstream>
#include <map>
#include <vector>
#include <set>
#include <cassert>
#include <climits>
#include <cstdlib>
#include <ctime>

typedef vector<bool> vB;
typedef vector<int> vI;
unordered_map<string, int> Book::sfens;
vector<BookEntry> Book::contents;
int debugcount = 0;
void Book::make_book(int argc, char *argv[]){
#if !defined(__ANDROID__)
  assert(argc >= 4);
  string src_file(argv[2]), dst_file(argv[3]);
  int depth_limit = (argc >= 5 ? atoi(argv[4]) : -1);
  using namespace osl;
  sfens.clear();
  contents.clear(); 
  int l = 0;
  std::ifstream ifs(src_file);
  for(string line; getline(ifs, line);){
    vector<string> cols;
    auto e = line.end();
    for(auto it = line.begin(); it != e;){
      auto next = find(it, e, ':');
      cols.push_back(string(it, next));
      if(next == e) break;
      it = next + 1;
    }
    if (cols.size() != 4){
      std::cerr << "format error in the book file : " << line << std::endl;
      abort();
    }
    vector<string> moves;
    e = cols[1].end();
    for(auto it = cols[1].begin();it != e;){
      auto next = find(it, e, ' ');
      moves.push_back(string(it, next));
      if(next == e) break;
      it = next + 1;
    }
    contents.push_back(BookEntry(moves, cols[2], stoi(cols[3])));
    sfens[cols[0]] = l++;
  }
#if 1
  std::cerr << "end of reading file" << std::endl;
#endif  
  int start_i = -1;
  for(auto& e : sfens){
    int i = e.second;
    if(e.first == "startpos") start_i = i;
    contents[i].bestScore = 0; // avoid infinite loop
    NumEffectState st = osl::usi::makeState(e.first);
    for(auto& m : contents[i].moves){
      Move move = osl::psn::strToMove(m.first, st);
#if 1
      if(!st.isValidMove(move)){
	csaShow(std::cerr, st);
	std::cerr << m.first << "," << move << std::endl;
      }
#endif
      NumEffectState new_st(st);
      new_st.makeMove(move);
      string s = osl::usi::show(new_st);
      auto it = sfens.find(s);
      if(it == sfens.end()){
#if 1
	csaShow(std::cerr, new_st);
	std::cerr << s << std::endl;
#endif
	continue;
//	abort();
      }
      m.second = it->second;
    }
  }
#if 1
  std::cerr << "end of making links" << std::endl;
#endif  
  for(int i = 0;;i++){
    int changed = 0;
    vector<int> minVals;
    for(auto& e: contents){
      int minVal = -e.otherScore;
      for(auto& v : e.moves) {
	minVal = std::min(minVal, contents[v.second].bestScore);
      }
      minVals.push_back(minVal);
    }
    for (size_t k = 0; k < contents.size(); k++){
      if (-minVals[k] != contents[k].bestScore){
	changed++;
	contents[k].bestScore = -minVals[k];
      }
    }
#if 1
    std::cerr << "iterations = " << i << ", changed = " << changed << std::endl;
#endif  
    if(changed == 0 || i > 200) break;
  }
  assert(start_i >= 0);
  vB reachable_black(contents.size(),false);
  vB reachable_white(contents.size(),false);
  vI depth(contents.size(),INT_MAX);
  depth[start_i]=0;
  reachable_white[start_i]=reachable_black[start_i]=true;
  for(;;){
    int changed = 0;
    vI q(contents.size());
    vB visited(contents.size(), false);
    q[0]=start_i;
    for(int qs=0, qe=1;qs<qe;){
      int i=q[qs++], d=depth[i];
      auto& e = contents[i];
      int minVal = -e.otherScore;
      for(auto& v : e.moves) 
        minVal = std::min(minVal, contents[v.second].bestScore);
      for(auto& v : e.moves){
        int j=v.second;
        if(visited[j]) continue;
        visited[j]=true;
        q[qe++]=j;
        if(d + 1 < depth[j]) {
          depth[j] = d+1;
          changed++;
        }
        if(minVal == contents[j].bestScore){
          if((d % 2) == 0 && reachable_black[i] && !reachable_black[j]){
            reachable_black[j] = true; 
            changed++;
          }
          else if((d % 2) == 1 && reachable_white[i] && !reachable_white[j]){
            reachable_white[j] = true; 
            changed++;
          }
        }
        if((d % 2) == 1 && reachable_black[i] && !reachable_black[j]){
          reachable_black[j] = true; 
          changed++;
        }
        else if((d % 2) == 0 && reachable_white[i] && !reachable_white[j]){
          reachable_white[j] = true; 
          changed++;
        }
      }
    }
    cerr << "changed=" << changed << endl;
    if(changed==0) break;
  }
  std::ofstream ofs(dst_file);
  for(auto& sfen : sfens){
    int i = sfen.second;
    if(depth_limit >= 0 && depth[i] >= depth_limit) continue;
    if(!reachable_black[i] && !reachable_white[i]) continue;
    ofs << sfen.first << ":";
    auto& e = contents[i];
    int minVal = -e.otherScore;
    for(auto& v : e.moves) 
      minVal = std::min(minVal, contents[v.second].bestScore);
    if(-minVal == e.otherScore) 
      e.bestMoves.push_back(make_pair(e.otherMove, 1));
    for(auto& v : e.moves)
      if(minVal == contents[v.second].bestScore)
	e.bestMoves.push_back(make_pair(v.first, 1));
    if(e.bestMoves.size() == 0){
      std::cerr << "No best move is found" << std::endl;
    }
    for(size_t k = 0; k < e.bestMoves.size(); k++)
      ofs << (k == 0 ? "" : ",") << e.bestMoves[k].first << " " << e.bestMoves[k].second;
    ofs << "\n";
  }
#endif
}

#ifdef WEIGHTED_BOOK
void writeInt(std::ostream& os, int v){
  char cs[4];
  for(int i=0;i<4;i++){
    cs[i] = (v >> (8 * (4 - i - 1))) & 255;
  }
  os.write(&cs[0], 4);
}

int makeOMove(Move m){
  const Square from_m = from(m);
  const Square to_m = to(m);
  const int bitFrom = (isPieceStand(from_m) ? 0 : 
		       (X(from_m) << 4 | Y(from_m)));
  const int bitTo = (isPieceStand(to_m) ? 0 : 
		     (X(to_m) << 12 | Y(to_m) << 8));
  return  (bitFrom | bitTo |
		 static_cast<unsigned int>(isPromotion(m)) << 19 |
		 static_cast<unsigned int>(capturePtype(m)) << 20 |
		 static_cast<unsigned int>(ptype(m)) << 24 |
		 static_cast<int>(player(m)) << 28);
  
}

void Book::make_weighted_book(string const& src_file, string const& dst_file){
  using namespace osl;
  sfens.clear();
  contents.clear(); 

  int k = 0;
  std::ifstream ifs(src_file);
  for(string line; getline(ifs, line);){
    vector<string> cols;
    auto e = line.end();
    for(auto it = line.begin(); it != e;){
      auto next = find(it, e, ':');
      cols.push_back(string(it, next));
      if(next == e) break;
      it = next + 1;
    }
    if (cols.size() != 4){
      std::cerr << "format error in the book file : " << line << std::endl;
      abort();
    }
    vector<string> moves;
    e = cols[1].end();
    for(auto it = cols[1].begin();it != e;){
      auto next = find(it, e, ' ');
      moves.push_back(string(it, next));
      if(next == e) break;
      it = next + 1;
    }
    contents.push_back(BookEntry(moves, cols[2], stoi(cols[3])));
    sfens[cols[0]] = k++;
  }
#if 1
  std::cerr << "end of reading file" << std::endl;
#endif  

  // 0. Set up white and black lists
  WhiteList wl;
  wl.load_index_file("whitelist/00INDEX.txt");

  WhiteList bl; // same class for black list
  bl.load_index_file("blacklist/00INDEX.txt");
  
  // a position index ->  a white list move
  std::map<int, std::set<Move> > white_list_state_move;
  std::map<int, std::set<Move> > black_list_state_move;
  
  int n_states = k, start_state = -1;
  vI sizes(n_states,0);
  vI starts(n_states,0);
  vector<vector<Move> > allmoves(n_states);
  for(auto& e : sfens){
    int i = e.second;
    contents[i].bestScore = 0; // avoid infinite loop

    { // Convert white list positions to indexes
      std::set<osl::Move> moves;
      if (wl.find(e.first, moves)) {
        white_list_state_move[i] = moves;
      }
    }
    { // Convert black list positions to indexes
      std::set<osl::Move> moves;
      if (bl.find(e.first, moves)) {
        black_list_state_move[i] = moves;
      }
    }

    if(e.first == "startpos") start_state = i;
    NumEffectState st = osl::usi::makeState(e.first);
    for(auto& m : contents[i].moves){
      Move move = osl::psn::strToMove(m.first, st);
#if 1
      if(!st.isValidMove(move)){
	csaShow(std::cerr, st);
	std::cerr << m.first << "," << move << std::endl;
      }
#endif
      allmoves[i].push_back(move);
      NumEffectState new_st(st);
      new_st.makeMove(move);
      string s = osl::usi::show(new_st);
      auto it = sfens.find(s);
      if(it == sfens.end()){
#if 1
	csaShow(std::cerr, new_st);
	std::cerr << s << std::endl;
	abort();
#endif
	continue;
      }
      m.second = it->second;
    }
    if(contents[i].otherMove == "resign"){
      allmoves[i].push_back(osl::Move_INVALID);
    }
    else{
      Move move = osl::psn::strToMove(contents[i].otherMove, st);
      allmoves[i].push_back(move);
    }
    sizes[i] = contents[i].moves.size() + 1;
  }
  int n_moves = 0;
  for(int i=0;i<n_states;i++){
    starts[i]=n_moves;
    n_moves += sizes[i];
  }
#if 1
  std::cerr << "end of making links" << std::endl;
#endif  
  for(int i = 0;;i++){
    int changed = 0;
    vector<int> minVals;
    for(auto& e: contents){
      int minVal = -e.otherScore;
      for(auto& v : e.moves) {
	minVal = std::min(minVal, contents[v.second].bestScore);
      }
      minVals.push_back(minVal);
    }
    for (size_t l = 0; l < contents.size(); l++){
      if (-minVals[l] != contents[l].bestScore){
	changed++;
	contents[l].bestScore = -minVals[l];
      }
    }
#if 1
    std::cerr << "iterations = " << i << ", changed = " << changed << std::endl;
#endif  
    if(changed == 0 || i > 200) break;
  }
  std::ofstream ofs(dst_file, ios::out|ios::binary|ios::trunc);
  // 1. headers
  writeInt(ofs, 1);            // version
  writeInt(ofs, n_states);     // #states
  writeInt(ofs, n_moves);      // #moves
  writeInt(ofs, start_state);  // index of the start position


  // 2. states
  for(int i=0;i<n_states;i++){
    writeInt(ofs, starts[i]);                    // index of the first wmove
    writeInt(ofs, contents[i].moves.size() + 1); // #wmoves in this state
    writeInt(ofs, 0);                            // #wins of black
    writeInt(ofs, 0);                            // #wins of white
  }

  // 3. wmoves
  for(int i=0;i<n_states;i++){
    vI weights(contents[i].moves.size());
    auto& e = contents[i];
    int minVal = -e.otherScore;
    for(auto& v : e.moves) 
      minVal = std::min(minVal, contents[v.second].bestScore);
    if(-minVal == e.otherScore) 
      e.bestMoves.push_back(make_pair(e.otherMove, 1));
    for(auto& v : e.moves)
      if(minVal == contents[v.second].bestScore)
	e.bestMoves.push_back(make_pair(v.first, 1));

    std::set<osl::Move> white_list_move;
    std::map<int, std::set<osl::Move> >::iterator white_list_it = white_list_state_move.find(i);
    if (white_list_it != white_list_state_move.end()) {
      white_list_move = white_list_it->second;
      if (white_list_move.empty()) {
        exit(1);
      }
    }

    std::set<osl::Move> black_list_move;
    std::map<int, std::set<osl::Move> >::iterator black_list_it = black_list_state_move.find(i);
    if (black_list_it != black_list_state_move.end()) {
      black_list_move = black_list_it->second;
      if (black_list_move.empty()) {
        exit(1);
      }
    }

    for(size_t j=0;j<contents[i].moves.size();j++){
      const osl::Move move = allmoves[i][j];
      writeInt(ofs, makeOMove(move));   // move
      writeInt(ofs, contents[i].moves[j].second); // index of the target state
      if((white_list_move.empty() && minVal == contents[e.moves[j].second].bestScore) ||
         white_list_move.count(move)) {
        if (white_list_move.count(move))
          std::cout << "==== white_list: " << i << " " << j << " " << move << "\n";

        if (black_list_move.count(move)) {
          std::cout << "==== black_list: " << i << " " << j << " " << move << "\n";
          writeInt(ofs, 0);
        } else {
          writeInt(ofs, 1);                         // weight
        }
      } else {
        writeInt(ofs, 0);
      }
    }
    // append otherMove
    const osl::Move other_move = allmoves[i][contents[i].moves.size()];
    writeInt(ofs, makeOMove(other_move)); // move
    writeInt(ofs, -1);                                               // out of the book
    if(white_list_move.empty() && -minVal == e.otherScore) { 
        if (black_list_move.count(other_move)) {
          std::cout << "==== black_list other move: " << other_move << "\n";
          writeInt(ofs, 0);
        } else {
          writeInt(ofs, 1);                         // weight
        }
    } else {
      writeInt(ofs, 0);
    }
  }
}
#endif // WEIGHTED_BOOK

bool Book::setUp(){
  using namespace osl;
  srand(time(0));
  sfens.clear();
  contents.clear(); 
  const std::string book_path(gpsfish_home() + "book.txt");
  std::ifstream ifs(book_path);
  if(!ifs) {
    std::cerr << "Failed opening book file: " << book_path << std::endl;
    return false;
  }
  int i = 0;
  for(string line; getline(ifs, line);){
    vector<string> cols;
    auto e = line.end();
    for(auto it = line.begin(); it != e;){
      auto next = find(it, e, ':');
      cols.push_back(string(it, next));
      if(next == e) break;
      it = next + 1;
    }
    if (cols.size() != 2){
      std::cerr << "format error in the book file : " << line << std::endl;
      return false;
    }
    BookEntry be(vector<string>(), "", 0);
    e = cols[1].end();
    for(auto it = cols[1].begin(); it != e;){
      auto next = find(it, e, ' ');
      string move(it, next);
      it = next + 1;
      next = find(it, e, ',');
      int prob = std::atoi(string(it, next).c_str());
      be.bestMoves.push_back(make_pair(move, prob));
      if(next == e) break;
      it = next + 1;
    }
    contents.push_back(be);
    sfens[cols[0]] = i++;
  }
	
  return true;
}

Move Book::get_move(const Position& pos, bool /* findBestMove */){
  string s = osl::usi::show(*pos.osl_state);
  auto it = sfens.find(s);
  if(it == sfens.end()) return MOVE_NONE;
  int i = it->second;
  int sum = 0;
  for(auto& e : contents[i].bestMoves) sum += e.second;
  int j = rand() % sum;
  sum = 0;
  for(auto& e : contents[i].bestMoves){
    sum += e.second;
    if(j < sum) return osl::psn::strToMove(e.first, *pos.osl_state);
  }
  return MOVE_NONE;  
}
