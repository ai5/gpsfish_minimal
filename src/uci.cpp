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

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include "evaluate.h"
#include "misc.h"
#include "move.h"
#include "position.h"
#include "search.h"
#include "ucioption.h"

#include "movegen.h"

#include "osl_eval.h"
// #include "enterKing.h"
#include "usi.h"
#include "book.h"
#include <vector>

using namespace std;

namespace {

  // FEN string for the initial position

  // UCIParser is a class for parsing UCI input. The class
  // is actually a string stream built on a given input string.
  typedef istringstream UCIParser;

  void set_option(UCIParser& up);
  void set_position(Position& pos, UCIParser& up);
  bool go(Position& pos, UCIParser& up);
  void perft(Position& pos, UCIParser& up);
}

std::vector<Move> ignore_moves;



/// execute_uci_command() takes a string as input, uses a UCIParser
/// object to parse this text string as a UCI command, and calls
/// the appropriate functions. In addition to the UCI commands,
/// the function also supports a few debug commands.

bool execute_uci_command(const string& cmd) {


  static Position pos(osl::StartPositionFEN, 0); // The root position

  UCIParser up(cmd);
  string token;

  up >> token; // operator>>() skips any whitespace

  if (token == "quit")
    return false;

  if (token == "go")
    return go(pos, up);

#if 1
  if (token == "eval"){
    osl::OpenMidEndingEval eval(*(pos.osl_state));
    cout << "value=" << eval.value() << std::endl;
    cout << "progress=" << eval.progressValue() << std::endl;
    cout << "progressMax=" << eval.progressMax() << std::endl;
    eval.show(cout);
    return true;
  }
#endif

  if (token == "usinewgame")
    pos.from_fen(osl::StartPositionFEN);

  else if (token == "isready") {
#if 0
    bool ok = osl::OpenMidEndingEval::setUp();
    ok &= osl::NewProgress::setUp();
#else
    bool ok = osl::NewProgress::setUp();
    ok &= osl::OpenMidEndingEval::setUp();
#endif
#if 1
    if ( Options["OwnBook"].value<bool>() ) ok &= Book::setUp();
#endif
    if (! ok) {
      std::cerr << "set up failed\n";
      return false;
    }
    cout << "readyok" << endl;
  }

    else if (token == "position")
      set_position(pos, up);

    else if (token == "setoption")
      set_option(up);

    else if (token == "perft")
      perft(pos, up);

    else if (token == "d")
      pos.print();


    else if (token == "key")
      cout << "key: " << hex     << pos.get_key() << endl;


    else if ( token == "ignore_moves"){
      ignore_moves.clear();
      while(up >> token) ignore_moves.push_back(move_from_uci(pos, token));
    }


    else if (token == "usi")
      cout << "id name "     << engine_name()
           << "\nid author " << engine_authors()

  << Options.print_all()
           << "\nusiok"      << endl;

    else if (token == "stop"){
    }
    else if (token == "echo"){
      up >> token;
      cout << token << endl;
    }

    else if (token == "show_tree"){
      show_tree(pos);
    }
    else
      cout << "Unknown command: " << cmd << endl;

    return true;
  }


  namespace {

    // set_position() is called when engine receives the "position" UCI
    // command. The function sets up the position described in the given
    // fen string ("fen") or the starting position ("startpos") and then
    // makes the moves given in the following move list ("moves").

    void set_position(Position& pos, UCIParser& up) {

      string token, fen;


      ignore_moves.clear();

      up >> token; // operator>>() skips any whitespace

      if (token == "startpos")
      {

        pos.from_fen(osl::StartPositionFEN);
        up >> token; // Consume "moves" token if any
      }

      else if (token == "sfen")
      {
        while (up >> token && token != "moves")
	  fen += token + " ";


        pos.from_fen(fen);
      }
      else return;

      // Parse move list (if any)
      while (up >> token)
        pos.do_setup_move(move_from_uci(pos, token));
      assert(pos.eval_is_ok());
    }


    // set_option() is called when engine receives the "setoption" UCI
    // command. The function updates the corresponding UCI option ("name")
    // to the given value ("value").

    void set_option(UCIParser& up) {

      string token, name;
      string value = "true"; // UCI buttons don't have a "value" field

      up >> token; // Consume "name" token
      up >> name;  // Read option name

      // Handle names with included spaces
      while (up >> token && token != "value")
        name += " " + token;

      up >> value; // Read option value

      // Handle values with included spaces
      while (up >> token)
        value += " " + token;

      if (Options.find(name) != Options.end())
        Options[name].set_value(value);
      else
        cout << "No such option: " << name << endl;
    }


    // go() is called when engine receives the "go" UCI command. The
    // function sets the thinking time and other parameters from the input
    // string, and then calls think(). Returns false if a quit command
    // is received while thinking, true otherwise.

    bool go(Position& pos, UCIParser& up) {

      string token;
      SearchLimits limits;
      Move searchMoves[MAX_MOVES], *cur = searchMoves;

      std::array<int,2> time={{0,0}},inc={{0,0}};

      while (up >> token)
      {
        if (token == "infinite")
	  limits.infinite = true;
        else if (token == "ponder")
	  limits.ponder = true;
        else if (token == "wtime")
	  up >> time[I(WHITE)];
        else if (token == "btime")
	  up >> time[I(BLACK)];
        else if (token == "winc")
	  up >> inc[I(WHITE)];
        else if (token == "binc")
	  up >> inc[I(BLACK)];
        else if (token == "movestogo")
	  up >> limits.movesToGo;
        else if (token == "depth")
	  up >> limits.maxDepth;
        else if (token == "nodes")
	  up >> limits.maxNodes;

        else if (token == "movetime" || token=="byoyomi")
	  up >> limits.maxTime;
        else if (token == "mate"){
#if 0
	  int mateTime;
	  up >> mateTime;
	  do_checkmate(pos, mateTime);
#else
	  int depth;
	  up >> depth;
	  do_checkmate(pos, depth);
#endif
	  return true;
	}
        else if (token == "searchmoves")
	  while (up >> token)
	    *cur++ = move_from_uci(pos, token);
      }

      *cur = MOVE_NONE;
      limits.time = time[I(pos.side_to_move())];
      limits.increment = inc[I(pos.side_to_move())];

      assert(pos.is_ok());

      if(searchMoves == cur && !ignore_moves.empty()){
	MoveStack mlist[MAX_MOVES];
	MoveStack* last = generate<MV_PSEUDO_LEGAL>(pos, mlist);
	for(MoveStack* mp=mlist;mp<last;mp++){
	  if(find(ignore_moves.begin(),ignore_moves.end(),mp->move)==ignore_moves.end()){
	    *cur++= mp->move;
	  }
	}
	*cur = MOVE_NONE;
      }
      ignore_moves.clear();
      if(!using_tcp_connection
#if 0
	 && osl::EnterKing::canDeclareWin(*(pos.osl_state))){
#else
	 && pos.osl_state->canDeclareWin()){
#endif
	cout << "bestmove win" << endl;
	return true;
      }
      return think(pos, limits, searchMoves);
    }


    // perft() is called when engine receives the "perft" command.
    // The function calls perft() passing the required search depth
    // then prints counted leaf nodes and elapsed time.

    void perft(Position& pos, UCIParser& up) {

      int depth, time;
      int64_t n;

      if (!(up >> depth))
        return;

      time = get_system_time();

      n = perft(pos, depth * ONE_PLY);

      time = get_system_time() - time;

      std::cout << "\nNodes " << n
		<< "\nTime (ms) " << time
		<< "\nNodes/second " << int(n / (time / 1000.0)) << std::endl;
    }
  }
