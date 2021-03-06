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

// #include "milliSeconds.h"
#include <cassert>
#include <cfloat>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include "book.h"
#include "evaluate.h"
#include "history.h"
#include "misc.h"
#include "move.h"
#include "movegen.h"
#include "movepick.h"
#include "search.h"
#include "timeman.h"
#include "thread.h"
#include "tt.h"
#include "ucioption.h"
#include "rkiss.h"
#include "position.tcc"

using osl::Board_Table;

using osl::Ptype_Table;

using osl::Offset32;
#include "osl_checkmate.h"
using osl::ImmediateCheckmate;
using std::string;
// #include "enterKing.h"



# define GPSFISH_CHECKMATE3
# define GPSFISH_CHECKMATE3_QUIESCE
// # define GPSFISH_DFPN

using std::cout;
using std::endl;

namespace {

  // Set to true to force running with one thread. Used for debugging
  const bool FakeSplit = false;

  // Different node types, used as template parameter
  enum NodeType { NonPV, PV };

  // RootMove struct is used for moves at the root of the tree. For each root
  // move, we store two scores, a node count, and a PV (really a refutation
  // in the case of moves which fail low). Value pv_score is normally set at
  // -VALUE_INFINITE for all non-pv moves, while non_pv_score is computed
  // according to the order in which moves are returned by MovePicker.
  struct RootMove {

    RootMove();
    RootMove(const RootMove& rm) { *this = rm; }
    RootMove& operator=(const RootMove& rm);

    // RootMove::operator<() is the comparison function used when
    // sorting the moves. A move m1 is considered to be better
    // than a move m2 if it has an higher pv_score, or if it has
    // equal pv_score but m1 has the higher non_pv_score. In this way
    // we are guaranteed that PV moves are always sorted as first.
    bool operator<(const RootMove& m) const {
      return pv_score != m.pv_score ? pv_score < m.pv_score
	: non_pv_score < m.non_pv_score;
    }

    void extract_pv_from_tt_rec(Position& pos,int ply);
    void extract_pv_from_tt(Position& pos);
    void insert_pv_in_tt_rec(Position& pos,int ply);
    void insert_pv_in_tt(Position& pos);
    std::string pv_info_to_uci(Position& pos, int depth, int selDepth,
                               Value alpha, Value beta, int pvIdx);
    int64_t nodes;
    Value pv_score;
    Value non_pv_score;
    Move pv[PLY_MAX_PLUS_2];
  };

  // RootMoveList struct is just a vector of RootMove objects,
  // with an handful of methods above the standard ones.
  struct RootMoveList : public std::vector<RootMove> {

    typedef std::vector<RootMove> Base;

    void init(Position& pos, Move searchMoves[]);
    void sort() { insertion_sort<RootMove, Base::iterator>(begin(), end()); }
    void sort_multipv(int n) { insertion_sort<RootMove, Base::iterator>(begin(), begin() + n); }

    int bestMoveChanges;
  };

  // MovePickerExt template class extends MovePicker and allows to choose at compile
  // time the proper moves source according to the type of node. In the default case
  // we simply create and use a standard MovePicker object.
  template<bool SpNode, bool Root> struct MovePickerExt : public MovePicker {

    MovePickerExt(const Position& p, Move ttm, Depth d, const History& h, SearchStack* ss, Value b)
      : MovePicker(p, ttm, d, h, ss, b) {}

    RootMoveList::iterator rm; // Dummy, needed to compile
  };

  // In case of a SpNode we use split point's shared MovePicker object as moves source
  template<> struct MovePickerExt<true, false> : public MovePicker {

    MovePickerExt(const Position& p, Move ttm, Depth d, const History& h, SearchStack* ss, Value b)
      : MovePicker(p, ttm, d, h, ss, b), mp(ss->sp->mp) {}

    Move get_next_move() { return mp->get_next_move(); }

    RootMoveList::iterator rm; // Dummy, needed to compile
    MovePicker* mp;
  };

  // In case of a Root node we use RootMoveList as moves source
  template<> struct MovePickerExt<false, true> : public MovePicker {

    MovePickerExt(const Position&, Move, Depth, const History&, SearchStack*, Value);
    Move get_next_move();

    RootMoveList::iterator rm;
    bool firstCall;
  };


  /// Constants

  // Lookup table to check if a Piece is a slider and its access function

  // Step 6. Razoring

  // Maximum depth for razoring
  const Depth RazorDepth = 4 * ONE_PLY;

  // Dynamic razoring margin based on depth
  inline Value razor_margin(Depth d) { return Value(0x200 + 0x10 * int(d)); }

  // Maximum depth for use of dynamic threat detection when null move fails low
  const Depth ThreatDepth = 5 * ONE_PLY;

  // Step 9. Internal iterative deepening

  // Minimum depth for use of internal iterative deepening
  const Depth IIDDepth[] = { 8 * ONE_PLY, 5 * ONE_PLY };

  // At Non-PV nodes we do an internal iterative deepening search
  // when the static evaluation is bigger then beta - IIDMargin.
  const Value IIDMargin = Value(0x100);

  // Step 11. Decide the new search depth

  // Extensions. Array index 0 is used for non-PV nodes, index 1 for PV nodes
  const Depth CheckExtension[]         = { ONE_PLY / 2, ONE_PLY / 1 };

  // Step 12. Futility pruning

  // Futility margin for quiescence search
  const Value FutilityMarginQS = Value(0x80);

  // Futility lookup tables (initialized at startup) and their access functions
  Value FutilityMargins[16][64]; // [depth][moveNumber]
  int FutilityMoveCounts[2][32]; // [improving][depth]

  inline Value futility_margin(Depth d, int mn) {

    return d < 7 * ONE_PLY ? FutilityMargins[Max(d, 1)][Min(mn, 63)]
      : 2 * VALUE_INFINITE;
  }

  // Step 14. Reduced search

  // Reduction lookup tables (initialized at startup) and their access function
  int8_t Reductions[2][2][64][64]; // [pv][improving][depth][moveNumber]

  template <NodeType PV> inline Depth reduction(bool i, Depth d, int mn) {

    return (Depth) Reductions[PV][i][Min(d / ONE_PLY, 63)][Min(mn, 63)];
  }

  // Easy move margin. An easy move candidate must be at least this much
  // better than the second best move.
  const Value EasyMoveMargin = Value(0x200);


  /// Namespace variables

  // Root move list
  RootMoveList Rml;

  // MultiPV mode
  int MultiPV, UCIMultiPV;


  Value DrawValue;

  // Time management variables
  bool StopOnPonderhit, FirstRootMove, StopRequest, QuitRequest, AspirationFailLow;
  TimeManager TimeMgr;
  SearchLimits Limits;

  // Log file
  std::ofstream LogFile;

  // Skill level adjustment
  int SkillLevel;
  bool SkillLevelEnabled;

  // Node counters, used only by thread[0] but try to keep in different cache
  // lines (64 bytes each) from the heavy multi-thread read accessed variables.
  bool SendSearchedNodes;
  int NodesSincePoll;
  int NodesBetweenPolls = 30000;

  // History table
  History H;


  /// Local functions

  Move id_loop(Position& pos, Move searchMoves[], Move* ponderMove);

  template <NodeType PvNode, bool SpNode, bool Root>
  Value search(Position& pos, SearchStack* ss, Value alpha, Value beta, Depth depth);

  template <NodeType PvNode>
  Value qsearch(Position& pos, SearchStack* ss, Value alpha, Value beta, Depth depth);

  template <NodeType PvNode>
  inline Value search(Position& pos, SearchStack* ss, Value alpha, Value beta, Depth depth) {

    return depth < ONE_PLY ? qsearch<PvNode>(pos, ss, alpha, beta, DEPTH_ZERO)
      : search<PvNode, false, false>(pos, ss, alpha, beta, depth);
  }

  template <NodeType PvNode>
  Depth extension(const Position& pos, Move m, bool captureOrPromotion, bool moveIsCheck, bool* dangerous);

  bool check_is_dangerous(Position &pos, Move move, Value futilityBase, Value beta, Value *bValue);
  bool connected_moves(const Position& pos, Move m1, Move m2);
  Value value_to_tt(Value v, int ply);
  Value value_from_tt(Value v, int ply);
  bool ok_to_use_TT(const TTEntry* tte, Depth depth, Value beta, int ply);
  bool connected_threat(const Position& pos, Move m, Move threat);
  Value refine_eval(const TTEntry* tte, Value defaultEval, int ply);
  void update_history(const Position& pos, Move move, Depth depth, Move movesSearched[], int moveCount);
  void update_gains(const Position& pos, Move move, Value before, Value after);
  void do_skill_level(Move* best, Move* ponder);

  int current_search_time(int set = 0);
  std::string value_to_uci(Value v);
  std::string speed_to_uci(int64_t nodes);
  void poll(const Position& pos);
  void wait_for_stop_or_ponderhit();


  // When formatting a move for std::cout we must know if we are in Chess960
  // or not. To keep using the handy operator<<() on the move the trick is to
  // embed this flag in the stream itself. Function-like named enum set960 is
  // used as a custom manipulator and the stream internal general-purpose array,
  // accessed through ios_base::iword(), is used to pass the flag to the move's
  // operator<<() that will read it to properly format castling moves.
  void show_tree_rec(Position &pos){
    TTEntry* tte;
    StateInfo st;
    if ((tte = TT.probe(pos.get_key())) != NULL){
      std::cerr << "tte->value=" << tte->value() << std::endl;
      std::cerr << "tte->type=" << tte->type() << std::endl;
      std::cerr << "tte->generation=" << tte->generation() << std::endl;
      std::cerr << "tte->depth=" << tte->depth() << std::endl;
      std::cerr << "tte->static_value=" << tte->static_value() << std::endl;
      Move m=tte->move(pos);
      int dummy;
      if(m != MOVE_NONE
	 && pos.move_is_legal(m)
	 && !pos.is_draw(dummy)){
	std::cerr << "move=" << m << std::endl;
	pos.do_undo_move(m,st,
			 [&](osl::Square){ show_tree_rec(pos); }
	  );
      }
    }
  }


  Value value_draw(Position const& pos){
    if(pos.side_to_move()==osl::BLACK) return DrawValue;
    else return -DrawValue;
  }


  bool can_capture_king(Position const& pos){
    Player us=pos.side_to_move();
    Player them=opposite_color(us);
    const osl::Square king = pos.king_square(them);
    return pos.osl_state->hasEffect(us, king);
  }

} // namespace


/// init_search() is called during startup to initialize various lookup tables

void init_search() {

  int d;  // depth (ONE_PLY == 2)
  int hd; // half depth (ONE_PLY == 1)
  int mc; // moveCount

  // Init reductions array
  for (hd = 1; hd < 64; hd++) for (mc = 1; mc < 64; mc++)
  {
      double    pvRed = log(double(hd)) * log(double(mc)) / 3.0;
      double nonPVRed = 0.33 + log(double(hd)) * log(double(mc)) / 2.25;
      Reductions[1][1][hd][mc] = (int8_t) (   pvRed >= 1.0 ? floor(   pvRed * int(ONE_PLY)) : 0);
      Reductions[0][1][hd][mc] = (int8_t) (nonPVRed >= 1.0 ? floor(nonPVRed * int(ONE_PLY)) : 0);
      Reductions[1][0][hd][mc] = Reductions[1][1][hd][mc];
      Reductions[0][0][hd][mc] = Reductions[0][1][hd][mc];

      if (Reductions[0][0][hd][mc] > 2 * ONE_PLY)
	  Reductions[0][0][hd][mc] += ONE_PLY;
      else if (Reductions[0][0][hd][mc] > ONE_PLY)
	  Reductions[0][0][hd][mc] += ONE_PLY / 2;
  }

  // Init futility margins array
  for (d = 1; d < 16; d++) for (mc = 0; mc < 64; mc++)
      FutilityMargins[d][mc] = Value(112 * int(log(double(d * d) / 2) / log(2.0) + 1.001) - 8 * mc + 45);

  // Init futility move count array
  for (d = 0; d < 32; d++)
  {
      FutilityMoveCounts[1][d] = int(3.001 + 0.3 * pow(d, 1.8));
      FutilityMoveCounts[0][d] = (d < 5) ? FutilityMoveCounts[1][d]
	: 3 * FutilityMoveCounts[1][d] / 4;
  }
}


/// perft() is our utility to verify move generation. All the leaf nodes up to
/// the given depth are generated and counted and the sum returned.

int64_t perft(Position& pos, Depth depth) {

  MoveStack mlist[MAX_MOVES];
  StateInfo st;
  Move m;
  int64_t sum = 0;

  // Generate all legal moves
  MoveStack* last = generate<MV_LEGAL>(pos, mlist);

  // If we are at the last ply we don't need to do and undo
  // the moves, just to count them.
  if (depth <= ONE_PLY)
      return int(last - mlist);

  // Loop through all legal moves
  for (MoveStack* cur = mlist; cur != last; cur++)
  {
      m = cur->move;

      pos.do_undo_move(m,st,
		       [&](osl::Square){
			 assert(pos.is_ok());
			 sum += perft(pos, depth - ONE_PLY);}
	);
  }
  return sum;
}


/// think() is the external interface to Stockfish's search, and is called when
/// the program receives the UCI 'go' command. It initializes various global
/// variables, and calls id_loop(). It returns false when a "quit" command is
/// received during the search.

bool think(Position& pos, const SearchLimits& limits, Move searchMoves[]) {


  static Book book;

  // Initialize global search-related variables
  StopOnPonderhit = StopRequest = QuitRequest = AspirationFailLow = SendSearchedNodes = false;
  NodesSincePoll = 0;
  current_search_time(get_system_time());
  Limits = limits;
  TimeMgr.init(Limits, pos.startpos_ply_counter());

  // Set best NodesBetweenPolls interval to avoid lagging under time pressure
  if (Limits.maxNodes)
      NodesBetweenPolls = Min(Limits.maxNodes, 30000);
  else if (Limits.time && Limits.time < 1000)
      NodesBetweenPolls = 1000;
  else if (Limits.time && Limits.time < 10000)
      NodesBetweenPolls = 5000;
  else
      NodesBetweenPolls = 30000;

  NodesBetweenPolls = Min(NodesBetweenPolls, 1000);


  // Look for a book move
  if (Options["OwnBook"].value<bool>())
  {

      Move bookMove = book.get_move(pos, Options["Best Book Move"].value<bool>());
      if (bookMove != MOVE_NONE)
      {
          if (Limits.ponder)
              wait_for_stop_or_ponderhit();


          cout << "bestmove " << move_to_uci(bookMove) << endl;
          return !QuitRequest;
      }
  }

  // Read UCI options
  UCIMultiPV = Options["MultiPV"].value<int>();
  SkillLevel = Options["Skill Level"].value<int>();

  if(pos.side_to_move()==osl::BLACK)
    DrawValue = (Value)(Options["DrawValue"].value<int>()*2);
  else
    DrawValue = -(Value)(Options["DrawValue"].value<int>()*2);


  read_evaluation_uci_options(pos.side_to_move());
  Threads.read_uci_options();

  // If needed allocate pawn and material hash tables and adjust TT size
  Threads.init_hash_tables();
  TT.set_size(Options["Hash"].value<int>());

  if (Options["Clear Hash"].value<bool>())
  {
      Options["Clear Hash"].set_value("false");
      TT.clear();
  }

  // Do we have to play with skill handicap? In this case enable MultiPV that
  // we will use behind the scenes to retrieve a set of possible moves.
  SkillLevelEnabled = (SkillLevel < 20);
  MultiPV = (SkillLevelEnabled ? Max(UCIMultiPV, 4) : UCIMultiPV);

  // Wake up needed threads and reset maxPly counter
  for (int i = 0; i < Threads.size(); i++)
  {
      Threads[i].wake_up();
      Threads[i].maxPly = 0;
  }

  // Write to log file and keep it open to be accessed during the search
  if (Options["Use Search Log"].value<bool>())
  {
      std::string name = Options["Search Log Filename"].value<std::string>();
      LogFile.open(name.c_str(), std::ios::out | std::ios::app);

      if (LogFile.is_open())
          LogFile << "\nSearching: "  << pos.to_fen()
                  << "\ninfinite: "   << Limits.infinite
                  << " ponder: "      << Limits.ponder
                  << " time: "        << Limits.time
                  << " increment: "   << Limits.increment
                  << " moves to go: " << Limits.movesToGo
                  << endl;
  }

  // We're ready to start thinking. Call the iterative deepening loop function
  Move ponderMove = MOVE_NONE;
  Move bestMove = id_loop(pos, searchMoves, &ponderMove);

  cout << "info" << speed_to_uci(pos.nodes_searched()) << endl;

  // Write final search statistics and close log file
  if (LogFile.is_open())
  {
      int t = current_search_time();

      LogFile << "Nodes: "          << pos.nodes_searched()
              << "\nNodes/second: " << (t > 0 ? pos.nodes_searched() * 1000 / t : 0)
              << "\nBest move: "    << move_to_san(pos, bestMove);

      StateInfo st;

      if(isNormal(bestMove))
	pos.do_undo_move(bestMove,st,
			 [&](osl::Square){
			   assert(pos.is_ok());
			   LogFile << "\nPonder move: " << move_to_san(pos, ponderMove) << endl;}
	  );
      LogFile.close();
  }

  // This makes all the threads to go to sleep
  Threads.set_size(1);

  // If we are pondering or in infinite search, we shouldn't print the
  // best move before we are told to do so.
  if (!StopRequest && (Limits.ponder || Limits.infinite))
      wait_for_stop_or_ponderhit();

  // Could be MOVE_NONE when searching on a stalemate position

  cout << "bestmove " << move_to_uci(bestMove);

  // UCI protol is not clear on allowing sending an empty ponder move, instead
  // it is clear that ponder move is optional. So skip it if empty.

  if (ponderMove != MOVE_NONE && Options["Ponder"].value<bool>())
    cout << " ponder " << move_to_uci(ponderMove);

  cout << endl;

  return !QuitRequest;
}


namespace {

  // id_loop() is the main iterative deepening loop. It calls search() repeatedly
  // with increasing depth until the allocated thinking time has been consumed,
  // user stops the search, or the maximum search depth is reached.

  Move id_loop(Position& pos, Move searchMoves[], Move* ponderMove) {

    SearchStack ss[PLY_MAX_PLUS_2];
    Value bestValues[PLY_MAX_PLUS_2];
    int bestMoveChanges[PLY_MAX_PLUS_2];
#ifdef OSL_USE_SSE
    ALIGN(16)
    __m128i es_base[(PLY_MAX_PLUS_2*sizeof(eval_t)+sizeof(__m128i)-1)/sizeof(__m128i)]
#else
    uint64_t es_base[(PLY_MAX_PLUS_2*sizeof(eval_t)+sizeof(uint64_t)-1)/sizeof(uint64_t)]
#endif
	;
    eval_t *es=(eval_t *)&es_base[0];
    int depth, selDepth, aspirationDelta;
    Value value, alpha, beta;
    Move bestMove, easyMove, skillBest, skillPonder;
    bool triedEasyMove = false;

    // Initialize stuff before a new search
    memset(ss, 0, 5 * sizeof(SearchStack));
    TT.new_search();
    H.clear();
    *ponderMove = bestMove = easyMove = skillBest = skillPonder = MOVE_NONE;
    depth = aspirationDelta = 0;
    alpha = -VALUE_INFINITE, beta = VALUE_INFINITE;
    ss->currentMove = osl::PASS(pos.side_to_move()); // Hack to skip update_gains()
    pos.eval= &es[0];
#if 0
    *(pos.eval)=eval_t(*(pos.osl_state),false);
#else
    *(pos.eval)=eval_t(*(pos.osl_state));
#endif

    // Moves to search are verified and copied
    Rml.init(pos, searchMoves);

    // Handle special case of searching on a mate/stalemate position
    if (Rml.size() == 0)
    {
        cout << "info depth 0 score "
             << value_to_uci(pos.in_check() ? value_mated_in(1) : VALUE_DRAW)
             << endl;

        return MOVE_NONE;
    }
    // Iterative deepening loop until requested to stop or target depth reached
    while (!StopRequest && ++depth <= PLY_MAX && (!Limits.maxDepth || depth <= Limits.maxDepth))
    {
        Rml.bestMoveChanges = 0;
        cout << "info depth " << depth << endl;

        // Calculate dynamic aspiration window based on previous iterations
        if (MultiPV == 1 && depth >= 5 && abs(bestValues[depth - 1]) < VALUE_KNOWN_WIN)
        {
            int prevDelta1 = bestValues[depth - 1] - bestValues[depth - 2];
            int prevDelta2 = bestValues[depth - 2] - bestValues[depth - 3];

            aspirationDelta = Min(Max(abs(prevDelta1) + abs(prevDelta2) / 2, 16), 24);
            aspirationDelta = (aspirationDelta + 7) / 8 * 8; // Round to match grainSize

            alpha = Max(bestValues[depth - 1] - aspirationDelta, -VALUE_INFINITE);
            beta  = Min(bestValues[depth - 1] + aspirationDelta,  VALUE_INFINITE);
        }


        // Start with a small aspiration window and, in case of fail high/low,
        // research with bigger window until not failing high/low anymore.
        do {
            // Search starting from ss+1 to allow calling update_gains()
            value = search<PV, false, true>(pos, ss+2, alpha, beta, depth * ONE_PLY);

            // Write PV back to transposition table in case the relevant entries
            // have been overwritten during the search.
            for (int i = 0; i < Min(MultiPV, (int)Rml.size()); i++)
                Rml[i].insert_pv_in_tt(pos);

            // Value cannot be trusted. Break out immediately!
            if (StopRequest)
                break;

            assert(value >= alpha);

            // In case of failing high/low increase aspiration window and research,
            // otherwise exit the fail high/low loop.
            if (value >= beta)
            {
                beta = Min(beta + aspirationDelta, VALUE_INFINITE);
                aspirationDelta += aspirationDelta / 2;
            }
            else if (value <= alpha)
            {
                AspirationFailLow = true;
                StopOnPonderhit = false;

                alpha = Max(alpha - aspirationDelta, -VALUE_INFINITE);
                aspirationDelta += aspirationDelta / 2;
            }
            else
                break;

        } while (abs(value) < VALUE_KNOWN_WIN);

        // Collect info about search result
        bestMove = Rml[0].pv[0];
        *ponderMove = Rml[0].pv[1];
        bestValues[depth] = value;
        bestMoveChanges[depth] = Rml.bestMoveChanges;

        // Do we need to pick now the best and the ponder moves ?
        if (SkillLevelEnabled && depth == 1 + SkillLevel)
            do_skill_level(&skillBest, &skillPonder);

        // Retrieve max searched depth among threads
        selDepth = 0;
        for (int i = 0; i < Threads.size(); i++)
            if (Threads[i].maxPly > selDepth)
                selDepth = Threads[i].maxPly;

        // Send PV line to GUI and to log file
        for (int i = 0; i < Min(UCIMultiPV, (int)Rml.size()); i++)
            cout << Rml[i].pv_info_to_uci(pos, depth, selDepth, alpha, beta, i) << endl;

        if (LogFile.is_open())
            LogFile << pretty_pv(pos, depth, value, current_search_time(), Rml[0].pv) << endl;

        // Init easyMove after first iteration or drop if differs from the best move
        if (depth == 1 && (Rml.size() == 1 || Rml[0].pv_score > Rml[1].pv_score + EasyMoveMargin))
            easyMove = bestMove;
        else if (bestMove != easyMove)
            easyMove = MOVE_NONE;

	if (! Limits.ponder
	    && !StopRequest
	    && depth >= 5
	    && abs(bestValues[depth])     >= VALUE_MATE_IN_PLY_MAX
	    && abs(bestValues[depth - 1]) >= VALUE_MATE_IN_PLY_MAX)
	{
	    StopRequest = true;
	}
        // Check for some early stop condition
        if (!StopRequest && Limits.useTimeManagement())
        {
            // Stop search early if one move seems to be much better than the
            // others or if there is only a single legal move. Also in the latter
            // case we search up to some depth anyway to get a proper score.
            if (   depth >= 7
		&& Rml.bestMoveChanges <= DBL_EPSILON
                && easyMove == bestMove
		&& !triedEasyMove
                && (   Rml.size() == 1
                    ||(   Rml[0].nodes > (pos.nodes_searched() * 85) / 100
                       && current_search_time() > TimeMgr.available_time() / 16)
                    ||(   Rml[0].nodes > (pos.nodes_searched() * 98) / 100
                       && current_search_time() > TimeMgr.available_time() / 32)))
	      {
		triedEasyMove = true;
                StopRequest = true;
	      }

            // Take in account some extra time if the best move has changed
            if (depth > 4 && depth < 50)
                TimeMgr.pv_instability(bestMoveChanges[depth], bestMoveChanges[depth - 1]);

            // Stop search if most of available time is already consumed. We probably don't
            // have enough time to search the first move at the next iteration anyway.
            if (current_search_time() > (TimeMgr.available_time() * 62) / 100)
                StopRequest = true;

            // If we are allowed to ponder do not stop the search now but keep pondering
            if (StopRequest && Limits.ponder)
            {
                StopRequest = false;
                StopOnPonderhit = true;
            }
        }
    }

    // When using skills overwrite best and ponder moves with the sub-optimal ones
    if (SkillLevelEnabled)
    {
        if (skillBest == MOVE_NONE) // Still unassigned ?
            do_skill_level(&skillBest, &skillPonder);

        bestMove = skillBest;
        *ponderMove = skillPonder;
    }

    return bestMove;
  }


  // search<>() is the main search function for both PV and non-PV nodes and for
  // normal and SplitPoint nodes. When called just after a split point the search
  // is simpler because we have already probed the hash table, done a null move
  // search, and searched the first move before splitting, we don't have to repeat
  // all this work again. We also don't need to store anything to the hash table
  // here: This is taken care of after we return from the split point.

  template <NodeType PvNode, bool SpNode, bool Root>
  Value search(Position& pos, SearchStack* ss, Value alpha, Value beta, Depth depth) {

    assert(alpha >= -VALUE_INFINITE && alpha <= VALUE_INFINITE);
    assert(beta > alpha && beta <= VALUE_INFINITE);
    assert(PvNode || alpha == beta - 1);
    assert(pos.thread() >= 0 && pos.thread() < Threads.size());

    Move movesSearched[MAX_MOVES];
    int64_t nodes;
    StateInfo st;
    const TTEntry *tte;
    Key posKey;
    Move ttMove, move, excludedMove, threatMove;
    Depth ext, newDepth;
    ValueType vt;
    Value bestValue, value, oldAlpha;
    Value refinedValue, nullValue, futilityBase, futilityValueScaled; // Non-PV specific
    bool isPvMove, inCheck, singularExtensionNode, givesCheck, captureOrPromotion, dangerous, isBadCap, improving;
    int moveCount = 0, playedMoveCount = 0;
    int threadID = pos.thread();
    SplitPoint* sp = NULL;

    int repeat_check=0;



    if(can_capture_king(pos)){
      return value_mate_in(0);
    }

    refinedValue = bestValue = value = -VALUE_INFINITE;
    oldAlpha = alpha;
    inCheck = pos.in_check();
    ss->ply = (ss-1)->ply + 1;

    // Used to send selDepth info to GUI
    if (PvNode && Threads[threadID].maxPly < ss->ply)
        Threads[threadID].maxPly = ss->ply;

    if (SpNode)
    {
        sp = ss->sp;
        tte = NULL;
        ttMove = excludedMove = MOVE_NONE;
        threatMove = sp->threatMove;
        goto split_point_start;
    }
    else if (Root)
        bestValue = alpha;

    // Step 1. Initialize node and poll. Polling can abort search
    ss->currentMove = ss->bestMove = threatMove = (ss+1)->excludedMove = MOVE_NONE;
    (ss+1)->skipNullMove = false; (ss+1)->reduction = DEPTH_ZERO;
    (ss+2)->killers[0] = (ss+2)->killers[1] = (ss+2)->mateKiller = MOVE_NONE;

    if (threadID == 0 && ++NodesSincePoll > NodesBetweenPolls)
    {
        NodesSincePoll = 0;
        poll(pos);
    }

    // Step 2. Check for aborted search and immediate draw
    if ((   StopRequest
         || Threads[threadID].cutoff_occurred()

         || pos.is_draw(repeat_check)
         || ss->ply > PLY_MAX) && !Root)
        return value_draw(pos);

    if ( !Root ){
      if(repeat_check<0) 
        return value_mated_in(ss->ply);
      else if(repeat_check>0) 
        return value_mate_in(ss->ply);
#if 0
      else if(osl::EnterKing::canDeclareWin(*(pos.osl_state))) 
	return value_mate_in(ss->ply+1);
#else
      else if(pos.osl_state->canDeclareWin()) 
	return value_mate_in(ss->ply+1);
#endif
    }
    if (!ss->checkmateTested) {
      ss->checkmateTested = true;
      if(!pos.osl_state->inCheck()
	 && ImmediateCheckmate::hasCheckmateMove
	 (pos.side_to_move(),*(pos.osl_state),ss->bestMove)) {
	  return value_mate_in(ss->ply);
      }
#ifdef GPSFISH_CHECKMATE3
      if ((! isNormal((ss-1)->currentMove)
	   || ptype((ss-1)->currentMove) == osl::KING)) {
	  osl::King8Info king8= pos.osl_state->king8Info(alt(pos.side_to_move()));
	  assert(V(king8) == V(osl::newKing8Info(alt(pos.side_to_move()), *(pos.osl_state))));
	  bool in_danger = dropCandidate(king8) | moveCandidate2(king8);
	  if (in_danger) {
	       if (osl::FixedDepthSearcher::hasCheckmate(*(pos.osl_state), pos.side_to_move(), 2, ss->bestMove)) {
		  return value_mate_in(ss->ply+2);;
	      }
	  }
      }
#  endif
    }

    // Step 3. Mate distance pruning
    alpha = Max(value_mated_in(ss->ply), alpha);
    beta = Min(value_mate_in(ss->ply+1), beta);
    if (alpha >= beta)
        return alpha;

    // Step 4. Transposition table lookup
    // We don't want the score of a partial search to overwrite a previous full search
    // TT value, so we use a different position key in case of an excluded move.
    excludedMove = ss->excludedMove;

    posKey = excludedMove!=MOVE_NONE ? pos.get_exclusion_key() : pos.get_key();

    tte = TT.probe(posKey);

    ttMove = tte ? fromMove16(tte->move16Val(),pos) : MOVE_NONE;

    // At PV nodes we check for exact scores, while at non-PV nodes we check for
    // a fail high/low. Biggest advantage at probing at PV nodes is to have a
    // smooth experience in analysis mode.
    if (   !Root
        && tte
        && (PvNode ? tte->depth() >= depth && tte->type() == VALUE_TYPE_EXACT
                   : ok_to_use_TT(tte, depth, beta, ss->ply)))
    {
        TT.refresh(tte);
        ss->bestMove = ttMove; // Can be MOVE_NONE
        return value_from_tt(tte->value(), ss->ply);
    }

    // Step 5. Evaluate the position statically and update parent's gain statistics
    if (inCheck)
        ss->eval = ss->evalMargin = VALUE_NONE;
    else if (tte)
    {
        assert(tte->static_value() != VALUE_NONE);

        ss->eval = tte->static_value();
        ss->evalMargin = tte->static_value_margin();
        refinedValue = refine_eval(tte, ss->eval, ss->ply);
    }
    else
    {
        refinedValue = ss->eval = evaluate(pos, ss->evalMargin);
        TT.store(posKey, VALUE_NONE, VALUE_TYPE_NONE, DEPTH_NONE, MOVE_NONE, ss->eval, ss->evalMargin);
    }

    // Save gain for the parent non-capture move
    update_gains(pos, (ss-1)->currentMove, (ss-1)->eval, ss->eval);

    // Step 6. Razoring (is omitted in PV nodes)
    if (   !PvNode
        &&  depth < RazorDepth
        && !inCheck
        &&  refinedValue + razor_margin(depth) < beta
        &&  ttMove == MOVE_NONE
        &&  abs(beta) < VALUE_MATE_IN_PLY_MAX
      )
    {
        Value rbeta = beta - razor_margin(depth);
        Value v = qsearch<NonPV>(pos, ss, rbeta-1, rbeta, DEPTH_ZERO);
        if (v < rbeta)
            // Logically we should return (v + razor_margin(depth)), but
            // surprisingly this did slightly weaker in tests.
            return v;
    }

    // Step 7. Static null move pruning (is omitted in PV nodes)
    // We're betting that the opponent doesn't have a move that will reduce
    // the score by more than futility_margin(depth) if we do a null move.
    if (   !PvNode
        && !ss->skipNullMove
        &&  depth < RazorDepth
        && !inCheck
	   &&  refinedValue - futility_margin(depth, (ss-1)->futilityMoveCount) >= beta
        &&  abs(beta) < VALUE_MATE_IN_PLY_MAX
	   )
        return refinedValue - futility_margin(depth, (ss-1)->futilityMoveCount);

    // Step 8. Null move search with verification search (is omitted in PV nodes)
    if (   !PvNode
        && !ss->skipNullMove
        &&  depth > 2 * ONE_PLY
        && !inCheck
        &&  refinedValue >= beta
        &&  abs(beta) < VALUE_MATE_IN_PLY_MAX

      )
    {
      ss->currentMove = PASS(pos.side_to_move());

        // Null move dynamic reduction based on depth
        int R = 3 + (depth >= 5 * ONE_PLY ? depth / 8 : 0);

        // Null move dynamic reduction based on value
        if (refinedValue - PawnValueMidgame > beta)
            R++;


	pos.do_undo_null_move(st,
			      [&](osl::Square){
	    *(pos.eval+1)= *(pos.eval);
#if 1
            (pos.eval + 1)->previous = pos.eval;
#endif
	    pos.eval++;
	  pos.eval->update(*(pos.osl_state),ss->currentMove);
	    (ss+1)->skipNullMove = true;
	    nullValue = -search<NonPV>(pos, ss+1, -beta, -alpha, depth-R*ONE_PLY);
	    (ss+1)->skipNullMove = false;
	    --pos.eval;
	  }
	  );

        if (nullValue >= beta)
        {
            // Do not return unproven mate scores
            if (nullValue >= VALUE_MATE_IN_PLY_MAX)
                nullValue = beta;

            if (depth < 6 * ONE_PLY)
                return nullValue;

            // Do verification search at high depths
            ss->skipNullMove = true;
            Value v = search<NonPV>(pos, ss, alpha, beta, depth-R*ONE_PLY);
            ss->skipNullMove = false;

            if (v >= beta)
                return nullValue;
        }
        else
        {
            // The null move failed low, which means that we may be faced with
            // some kind of threat. If the previous move was reduced, check if
            // the move that refuted the null move was somehow connected to the
            // move which was reduced. If a connection is found, return a fail
            // low score (which will cause the reduced move to fail high in the
            // parent node, which will trigger a re-search with full depth).
            threatMove = (ss+1)->bestMove;

            if (   depth < ThreatDepth
                && (ss-1)->reduction
                && threatMove != MOVE_NONE
                && connected_moves(pos, (ss-1)->currentMove, threatMove))
                return beta - 1;
        }
    }

    // Step 9. Internal iterative deepening
    if (   depth >= IIDDepth[PvNode]
        && ttMove == MOVE_NONE
        && (PvNode || (!inCheck && ss->eval + IIDMargin >= beta)))
    {
	Depth d = depth - 2 * ONE_PLY - (PvNode ? DEPTH_ZERO : depth / 4);

        ss->skipNullMove = true;
        search<PvNode>(pos, ss, alpha, beta, d);
        ss->skipNullMove = false;

        ttMove = ss->bestMove;
        tte = TT.probe(posKey);
    }

split_point_start: // At split points actual search starts from here

    // Initialize a MovePicker object for the current position
    MovePickerExt<SpNode, Root> mp(pos, ttMove, depth, H, ss, (PvNode ? -VALUE_INFINITE : beta));
    ss->bestMove = MOVE_NONE;
    futilityBase = ss->eval + ss->evalMargin;
    improving = ss->eval >= (ss-2)->eval;
    singularExtensionNode =   !Root
                           && !SpNode
                           && depth >= 8 * ONE_PLY
                           && tte
      && tte->move16Val()!=Move16::NONE
                           && excludedMove==MOVE_NONE // Do not allow recursive singular extension search
                           && (tte->type() & VALUE_TYPE_LOWER)
                           && tte->depth() >= depth - 3 * ONE_PLY;
    if (SpNode)
    {
        lock_grab(&(sp->lock));
        bestValue = sp->bestValue;
    }

    // Step 10. Loop through moves
    // Loop through all legal moves until no moves remain or a beta cutoff occurs
    while (   bestValue < beta
           && (move = mp.get_next_move()) != MOVE_NONE
           && !Threads[threadID].cutoff_occurred())
    {
      assert(move_is_ok(move));

      if (SpNode)
      {
          moveCount = ++sp->moveCount;
          lock_release(&(sp->lock));
      }
      else if (move == excludedMove)
          continue;
      else
          moveCount++;
#ifdef MOVE_STACK_REJECTIONS
      if(!Root && move_stack_rejections_probe(move,pos,ss,alpha)) {
	if (SpNode)
	  lock_grab(&(sp->lock));
	continue;
      }
#endif      

      if (Root)
      {
          // This is used by time management
          FirstRootMove = (moveCount == 1);

          // Save the current node count before the move is searched
          nodes = pos.nodes_searched();

          // If it's time to send nodes info, do it here where we have the
          // correct accumulated node counts searched by each thread.
          if (SendSearchedNodes)
          {
              SendSearchedNodes = false;
              cout << "info" << speed_to_uci(pos.nodes_searched()) << endl;
          }

          if (current_search_time() > 2000)

//	    cout << "info currmove " << move_to_uci(move)
//                   << " currmovenumber " << moveCount << endl;
	  {}
      }

      // At Root and at first iteration do a PV search on all the moves to score root moves
      isPvMove = (PvNode && moveCount <= (Root ? depth <= ONE_PLY ? 1000 : MultiPV : 1));

      givesCheck = pos.move_gives_check(move);
      captureOrPromotion = pos.move_is_capture_or_promotion(move);

      // Step 11. Decide the new search depth
      ext = DEPTH_ZERO;
      dangerous = givesCheck;
      if (PvNode && dangerous)
	ext = ONE_PLY;
      else if (givesCheck && pos.see_sign(move) >= 0)
	ext = inCheck || ss->eval <= alpha ? ONE_PLY : ONE_PLY / 2;

      // Singular extension search. If all moves but one fail low on a search of
      // (alpha-s, beta-s), and just one fails high on (alpha, beta), then that move
      // is singular and should be extended. To verify this we do a reduced search
      // on all the other moves but the ttMove, if result is lower than ttValue minus
      // a margin then we extend ttMove.
      if (   singularExtensionNode

	     && move == fromMove16(tte->move16Val(),pos)
          && ext < ONE_PLY)
      {
          Value ttValue = value_from_tt(tte->value(), ss->ply);

          if (abs(ttValue) < VALUE_KNOWN_WIN)
          {
              Value rBeta = ttValue - int(depth);
              ss->excludedMove = move;
              ss->skipNullMove = true;
              Value v = search<NonPV>(pos, ss, rBeta - 1, rBeta, depth / 2);
              ss->skipNullMove = false;
              ss->excludedMove = MOVE_NONE;
              ss->bestMove = MOVE_NONE;
              if (v < rBeta)
                  ext = ONE_PLY;
          }
      }

      // Update current move (this must be done after singular extension search)
      ss->currentMove = move;
      ss->futilityMoveCount = 0;
      newDepth = depth - ONE_PLY + ext;

      // Step 12. Futility pruning (is omitted in PV nodes)
      if (   !PvNode
          && !captureOrPromotion
          && !inCheck
          && !dangerous
          &&  move != ttMove
          && !move_is_castle(move))
      {
          // Move count based pruning
          if (   depth < 16 * ONE_PLY
	      && moveCount >= FutilityMoveCounts[improving][depth]
	      && (threatMove==MOVE_NONE || !connected_threat(pos, move, threatMove)))
          {
              if (SpNode)
                  lock_grab(&(sp->lock));

              continue;
          }

          // Value based pruning
          // We illogically ignore reduction condition depth >= 3*ONE_PLY for predicted depth,
          // but fixing this made program slightly weaker.
	  ss->futilityMoveCount = moveCount;	  
	  Depth predictedDepth = newDepth - reduction<NonPV>(improving, depth, moveCount);
          futilityValueScaled =  futilityBase + futility_margin(predictedDepth, moveCount)
	    + H.gain(osl::ptypeO(move), move_to(move));

          if (futilityValueScaled < beta)
          {
	    bestValue = std::max(bestValue, futilityValueScaled);

	    if (SpNode)
              {
                  lock_grab(&(sp->lock));
                  if (futilityValueScaled > sp->bestValue)
                      sp->bestValue = bestValue = futilityValueScaled;
              }
              else if (futilityValueScaled > bestValue)
                  bestValue = futilityValueScaled;

              continue;
          }

          // Prune moves with negative SEE at low depths
          if (   predictedDepth < 4 * ONE_PLY
              && bestValue > VALUE_MATED_IN_PLY_MAX
              && pos.see_sign(move) < 0)
          {
              if (SpNode)
                  lock_grab(&(sp->lock));

              continue;
          }
      }

      // Bad capture detection. Will be used by prob-cut search
      isBadCap =   depth >= 3 * ONE_PLY
                && depth < 8 * ONE_PLY
                && captureOrPromotion
                && move != ttMove
                && !dangerous
                &&  abs(alpha) < VALUE_MATE_IN_PLY_MAX
                &&  pos.see_sign(move) < 0;


      // Step 13. Make the move
#if 0
      assert(pos.eval->value()==eval_t(*(pos.osl_state),false).value());
#else
      assert(pos.eval->value()==eval_t(*(pos.osl_state)).value());
#endif
      (ss+1)->checkmateTested = false;
      pos.do_undo_move(move,st,
		       [&](osl::Square){
	  *(pos.eval+1)= *(pos.eval);
#if 1
            (pos.eval + 1)->previous = pos.eval;
#endif
	  pos.eval++;
	  pos.eval->update(*(pos.osl_state),move);
	  assert(pos.eval->value()==eval_t(*(pos.osl_state)/*,false*/).value() ||
      ((pos.eval)->show(std::cerr), eval_t(*(pos.osl_state)/*,false*/).show(std::cerr), csaShow(std::cerr, *pos.osl_state), std::cerr << pos.eval->value() << "," << eval_t(*(pos.osl_state)/*,false*/).value() << "," << move << std::endl, 0));
      if (!SpNode && !captureOrPromotion)
          movesSearched[playedMoveCount++] = move;

      // Step extra. pv search (only in PV nodes)
      // The first move in list is the expected PV
      if (isPvMove)
      {
          // Aspiration window is disabled in multi-pv case
          if (Root && MultiPV > 1)
              alpha = -VALUE_INFINITE;

          value = -search<PV>(pos, ss+1, -beta, -alpha, newDepth);
      }
      else
      {
          // Step 14. Reduced depth search
          // If the move fails high will be re-searched at full depth.
          bool doFullDepthSearch = true;
          alpha = SpNode ? sp->alpha : alpha;

          if (    depth >= 3 * ONE_PLY
              && !captureOrPromotion
              && !move_is_castle(move)
              &&  ss->killers[0] != move
              &&  ss->killers[1] != move)
          {
	      ss->reduction = reduction<PvNode>(improving, depth, moveCount);
              if (ss->reduction)
              {
                  alpha = SpNode ? sp->alpha : alpha;
                  Depth d = newDepth - ss->reduction;
                  value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, d);

		  // Research at intermediate depth if reduction is very high
		  if (value > alpha && ss->reduction >= 4 *ONE_PLY)
		  {
		      Depth d2 = std::max(newDepth - 2 * ONE_PLY, ONE_PLY);
		      value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, d2);
		  }

                  doFullDepthSearch = (value > alpha);
              }
              ss->reduction = DEPTH_ZERO; // Restore original reduction
          }

          // Probcut search for bad captures. If a reduced search returns a value
          // very below beta then we can (almost) safely prune the bad capture.
          if (isBadCap)
          {
              ss->reduction = 3 * ONE_PLY;
              Value rAlpha = alpha - 300;
              Depth d = newDepth - ss->reduction;
              value = -search<NonPV>(pos, ss+1, -(rAlpha+1), -rAlpha, d);
              doFullDepthSearch = (value > rAlpha);
              ss->reduction = DEPTH_ZERO; // Restore original reduction
          }

          // Step 15. Full depth search
          if (doFullDepthSearch)
          {
              alpha = SpNode ? sp->alpha : alpha;
              value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, newDepth);

              // Step extra. pv search (only in PV nodes)
              // Search only for possible new PV nodes, if instead value >= beta then
              // parent node fails low with value <= alpha and tries another move.
              if (PvNode && value > alpha && (Root || value < beta))
                  value = -search<PV>(pos, ss+1, -beta, -alpha, newDepth);
          }
      }
      --pos.eval;
	}
	);


      assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

      // Step 17. Check for new best move
      if (SpNode)
      {
          lock_grab(&(sp->lock));
          bestValue = sp->bestValue;
          alpha = sp->alpha;
      }

      if (value > bestValue && !(SpNode && Threads[threadID].cutoff_occurred()))
      {
          bestValue = value;

          if (SpNode)
              sp->bestValue = value;

          if (!Root && value > alpha)
          {
              if (PvNode && value < beta) // We want always alpha < beta
              {
                  alpha = value;

                  if (SpNode)
                      sp->alpha = value;
              }
              else if (SpNode)
                  sp->is_betaCutoff = true;

              if (value == value_mate_in(ss->ply + 1))
                  ss->mateKiller = move;

              ss->bestMove = move;

              if (SpNode)
                  sp->ss->bestMove = move;
          }
      }

      if (Root)
      {
          // Finished searching the move. If StopRequest is true, the search
          // was aborted because the user interrupted the search or because we
          // ran out of time. In this case, the return value of the search cannot
          // be trusted, and we break out of the loop without updating the best
          // move and/or PV.
          if (StopRequest)
              break;

          // Remember searched nodes counts for this move
          mp.rm->nodes += pos.nodes_searched() - nodes;

          // PV move or new best move ?
          if (isPvMove || value > alpha)
          {
              // Update PV
              ss->bestMove = move;
              mp.rm->pv_score = value;
              mp.rm->extract_pv_from_tt(pos);

              // We record how often the best move has been changed in each
              // iteration. This information is used for time management: When
              // the best move changes frequently, we allocate some more time.
              if (!isPvMove && MultiPV == 1)
                  Rml.bestMoveChanges++;

              Rml.sort_multipv(moveCount);

              if (depth >= 5*ONE_PLY
		  && (!isPvMove || current_search_time() >= 5000))
		  cout << Rml[0].pv_info_to_uci(pos, depth/ONE_PLY,
						Threads[threadID].maxPly,
						alpha, beta, 0)
		       << endl;


              // Update alpha. In multi-pv we don't use aspiration window, so
              // set alpha equal to minimum score among the PV lines.
              if (MultiPV > 1)
                  alpha = Rml[Min(moveCount, MultiPV) - 1].pv_score; // FIXME why moveCount?
              else if (value > alpha)
                  alpha = value;
          }
          else
              mp.rm->pv_score = -VALUE_INFINITE;

      } // Root

      // Step 18. Check for split
      if (   !Root
          && !SpNode
          && depth >= Threads.min_split_depth()
          && bestValue < beta
          && Threads.available_slave_exists(threadID)
          && !StopRequest
          && !Threads[threadID].cutoff_occurred())
          Threads.split<FakeSplit>(pos, ss, &alpha, beta, &bestValue, depth,
                                   threatMove, moveCount, &mp, PvNode);
    }

    // Step 19. Check for mate and stalemate
    // All legal moves have been searched and if there are
    // no legal moves, it must be mate or stalemate.
    // If one move was excluded return fail low score.
    if (!SpNode && !moveCount)

      return excludedMove!=MOVE_NONE ? oldAlpha : (inCheck ? (move_is_pawn_drop((ss-1)->currentMove) ? value_mate_in(ss->ply) : value_mated_in(ss->ply) ): VALUE_DRAW);

    // Step 20. Update tables
    // If the search is not aborted, update the transposition table,
    // history counters, and killer moves.
    if (!SpNode && !StopRequest && !Threads[threadID].cutoff_occurred())
    {
        move = bestValue <= oldAlpha ? MOVE_NONE : ss->bestMove;
        vt   = bestValue <= oldAlpha ? VALUE_TYPE_UPPER
             : bestValue >= beta ? VALUE_TYPE_LOWER : VALUE_TYPE_EXACT;

        TT.store(posKey, value_to_tt(bestValue, ss->ply), vt, depth, move, ss->eval, ss->evalMargin);

        // Update killers and history only for non capture moves that fails high
        if (    bestValue >= beta
            && !pos.move_is_capture_or_promotion(move))
        {
            if (move != ss->killers[0])
            {
                ss->killers[1] = ss->killers[0];
                ss->killers[0] = move;
            }
            update_history(pos, move, depth, movesSearched, playedMoveCount);
        }
    }

    if (SpNode)
    {
        // Here we have the lock still grabbed
        sp->is_slave[threadID] = false;
        sp->nodes += pos.nodes_searched();
        lock_release(&(sp->lock));
    }

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }

  // qsearch() is the quiescence search function, which is called by the main
  // search function when the remaining depth is zero (or, to be more precise,
  // less than ONE_PLY).

template <NodeType PvNode>
Value qsearch(Position& pos, SearchStack* ss, Value alpha, Value beta, Depth depth) {

  assert(alpha >= -VALUE_INFINITE && alpha <= VALUE_INFINITE);
  assert(beta >= -VALUE_INFINITE && beta <= VALUE_INFINITE);
  assert(PvNode || alpha == beta - 1);
  assert(depth <= 0);
  assert(pos.thread() >= 0 && pos.thread() < Threads.size());

  StateInfo st;
  Move ttMove, move;
  Value bestValue, value, evalMargin, futilityValue, futilityBase;

  bool inCheck, givesCheck, evasionPrunable;
  const TTEntry* tte;
  Depth ttDepth;
  Value oldAlpha = alpha;

  ss->bestMove = ss->currentMove = MOVE_NONE;
  ss->ply = (ss-1)->ply + 1;


  if(can_capture_king(pos)){
    return value_mate_in(0);
  }
  if(!pos.osl_state->inCheck()
     && ImmediateCheckmate::hasCheckmateMove
     (pos.side_to_move(),*(pos.osl_state),ss->bestMove)) {
      return value_mate_in(ss->ply); 
  }


  // Check for an instant draw or maximum ply reached

  int threadID = pos.thread();
  if (threadID == 0 && ++NodesSincePoll > NodesBetweenPolls)
  {
    NodesSincePoll = 0;
    poll(pos);
  }
  int repeat_check=0;
  if (StopRequest || ss->ply > PLY_MAX || pos.is_draw(repeat_check))

        return value_draw(pos);
  if(repeat_check<0) 
    return value_mated_in(ss->ply+1);
  else if(repeat_check>0) 
    return value_mate_in(ss->ply);

  // Decide whether or not to include checks, this fixes also the type of
  // TT entry depth that we are going to use. Note that in qsearch we use
  // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
  inCheck = pos.in_check();
    
  ttDepth = (inCheck || depth >= DEPTH_QS_CHECKS ? DEPTH_QS_CHECKS : DEPTH_QS_NO_CHECKS);

  // Transposition table lookup. At PV nodes, we don't use the TT for
  // pruning, but only for move ordering.
  tte = TT.probe(pos.get_key());

  ttMove = tte ? fromMove16(tte->move16Val(),pos) : MOVE_NONE;

  if (!PvNode && tte && ok_to_use_TT(tte, ttDepth, beta, ss->ply))
  {
    ss->bestMove = ttMove; // Can be MOVE_NONE
    return value_from_tt(tte->value(), ss->ply);
  }

  // Evaluate the position statically
  if (inCheck)
  {
    bestValue = futilityBase = -VALUE_INFINITE;
    ss->eval = evalMargin = VALUE_NONE;
  }
  else
  {
    if (tte)
    {
      assert(tte->static_value() != VALUE_NONE);

      evalMargin = tte->static_value_margin();
      ss->eval = bestValue = tte->static_value();
    }
    else
      ss->eval = bestValue = evaluate(pos, evalMargin);

    update_gains(pos, (ss-1)->currentMove, (ss-1)->eval, ss->eval);

    // Stand pat. Return immediately if static value is at least beta
    if (bestValue >= beta)
    {
      if (!tte){
	TT.store(pos.get_key(), value_to_tt(bestValue, ss->ply), VALUE_TYPE_LOWER, DEPTH_NONE, MOVE_NONE, ss->eval, evalMargin);
      }
	  
      return bestValue;
    }

    if (PvNode && bestValue > alpha)
      alpha = bestValue;

    // Futility pruning parameters, not needed when in check
    futilityBase = ss->eval + evalMargin + FutilityMarginQS;
  }

  // Initialize a MovePicker object for the current position, and prepare
  // to search the moves. Because the depth is <= 0 here, only captures,
  // queen promotions and checks (only if depth >= DEPTH_QS_CHECKS) will
  // be generated.
  MovePicker mp(pos, ttMove, depth, H);

  // Loop through the moves until no moves remain or a beta cutoff occurs
  while (   alpha < beta
	    && (move = mp.get_next_move()) != MOVE_NONE)
  {
    assert(move_is_ok(move));

#ifdef MOVE_STACK_REJECTIONS
    if(move_stack_rejections_probe(move,pos,ss,alpha)) continue;
#endif      


    givesCheck = pos.move_gives_check(move);

    // Futility pruning
    if (   !PvNode
	   && !inCheck
	   && !givesCheck
	   &&  move != ttMove
	   )
    {

    futilityValue =  futilityBase
      + pos.endgame_value_of_piece_on(move_to(move))
      + (move_is_promotion(move) ? pos.promote_value_of_piece_on(move_from(move)) : VALUE_ZERO);

    if (futilityValue < alpha)
    {
      bestValue = std::max(bestValue, futilityValue);
      continue;
    }

    // Prune moves with negative or equal SEE
    if (   futilityBase < beta
	   && depth < DEPTH_ZERO
	   && pos.see(move) <= 0)
    {
      bestValue = std::max(bestValue, futilityBase);
      continue;
    }
  }

  // Detect non-capture evasions that are candidate to be pruned
  evasionPrunable =   inCheck
    && bestValue > VALUE_MATED_IN_PLY_MAX
    && !pos.move_is_capture(move)
      ;

  // Don't search moves with negative SEE values
  if (   !PvNode
	 && (!inCheck || evasionPrunable)
	 &&  move != ttMove
	 &&  pos.see_sign(move) < 0)
    continue;

#if 0
  if ( move != ttMove
      && !inCheck
      && depth < -1
      && !pos.move_is_capture(move) 
      && ( !pos.move_is_capture_or_promotion(move)
	   || ( !*(pos.osl_state).longEffectAt(move_from(move),pos.side_to_move()).any()
		&& *(pos.osl_state).countEffect(pos.side_to_move(),move_to(move))
		<= *(pos.osl_state).countEffect(osl::alt(pos.side_to_move()),move_to(move))))){
    continue;
  }
#endif
  // Don't search useless checks
  if (   !PvNode
	 && !inCheck
	 &&  givesCheck
	 &&  move != ttMove
	 && !pos.move_is_capture_or_promotion(move) 
	 &&  ss->eval + PawnValueMidgame / 4 < beta
	 && !check_is_dangerous(pos, move, futilityBase, beta, &bestValue))
  {
    if (ss->eval + PawnValueMidgame / 4 > bestValue)
      bestValue = ss->eval + PawnValueMidgame / 4;

    continue;
  }

  // Update current move
  ss->currentMove = move;

  // Make and search the move

  pos.do_undo_move(move,st,
		   [&](osl::Square){
      assert(pos.is_ok());
	  *(pos.eval+1)= *(pos.eval);
#if 1
            (pos.eval + 1)->previous = pos.eval;
#endif
	    pos.eval++;
      pos.eval->update(*(pos.osl_state),move);
      assert(pos.eval_is_ok() || (csaShow(std::cerr, *pos.osl_state), std::cerr << move << std::endl, 0));
      value = -qsearch<PvNode>(pos, ss+1, -beta, -alpha, depth-ONE_PLY);
      --pos.eval;
    }
    );

  assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

  // New best move?
  if (value > bestValue)
  {
    bestValue = value;
    if (value > alpha)
    {
      alpha = value;
      ss->bestMove = move;
    }
  }
  }

#ifdef GPSFISH_CHECKMATE3_QUIESCE
  if (bestValue < beta && depth >= DEPTH_QS_CHECKS
      && (!isNormal((ss-1)->currentMove)
	  || ptype((ss-1)->currentMove) == osl::KING)) {
      osl::King8Info king8= pos.osl_state->king8Info(alt(pos.side_to_move()));
      assert(V(king8) == V(osl::newKing8Info(alt(pos.side_to_move()), *(pos.osl_state))));
      bool in_danger = dropCandidate(king8) | moveCandidate2(king8);
      if (in_danger) {
	   if (osl::FixedDepthSearcher::hasCheckmate(*(pos.osl_state), pos.side_to_move(), 2,(ss)->bestMove)) {
	      return value_mate_in(ss->ply+2);;
	  }
      }
  }
#endif
    // All legal moves have been searched. A special case: If we're in check
    // and no legal moves were found, it is checkmate.
    if (inCheck && bestValue == -VALUE_INFINITE)

      return (move_is_pawn_drop((ss-1)->currentMove) ? value_mate_in(ss->ply) : value_mated_in(ss->ply));

    // Update transposition table
    ValueType vt = (bestValue <= oldAlpha ? VALUE_TYPE_UPPER : bestValue >= beta ? VALUE_TYPE_LOWER : VALUE_TYPE_EXACT);
	    
    TT.store(pos.get_key(), value_to_tt(bestValue, ss->ply), vt, ttDepth, ss->bestMove, ss->eval, evalMargin);

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // check_is_dangerous() tests if a checking move can be pruned in qsearch().
  // bestValue is updated only when returning false because in that case move
  // will be pruned.

  bool check_is_dangerous(Position &/* pos */, Move /* move */, Value /* futilityBase */, Value /* beta */, Value * /* bestValue */)
  {

    return false;
  }


  // connected_moves() tests whether two moves are 'connected' in the sense
  // that the first move somehow made the second move possible (for instance
  // if the moving piece is the same in both moves). The first move is assumed
  // to be the move that was made to reach the current position, while the
  // second move is assumed to be a move from the current position.

  bool connected_moves(const Position& pos, Move m1, Move m2) {

//    Square f1, t1, f2, t2;
    PtypeO p;

    assert(m1 != MOVE_NONE && move_is_ok(m1));
    assert(m2 != MOVE_NONE && move_is_ok(m2));

    // Case 1: The moving piece is the same in both moves
    Square f1 = move_from(m1);
    Square t1 = move_to(m1);
    Square f2 = move_from(m2);
    Square t2 = move_to(m2);
#if 1
    if (f2 == t1){
      if(t2 == f1){
        return move_is_promotion(m1) || move_is_promotion(m2);
      }
      if (isPieceStand(f1)){
        return osl::isCapture(m2) || osl::isPromotion(m2);
      }
      return 
          !osl::hasEffect(osl::Ptype_Table.getEffect(osl::oldPtypeO(m1), f1, t2)) ||
          osl::isCapture(m1) ||
        (move_is_promotion(m1) && !canPromote(osl::player(m1), f1));
    }
#else
    if (f2 == t1)
        return true;
#endif

    // Case 2: The destination square for m2 was vacated by m1
    if (t2 == f1)
        return true;

    // Case 3: Moving through the vacated square

    if(!isPieceStand(f2) && !isPieceStand(f1) &&
       Board_Table.getShortOffset(t2,f2) ==
       Board_Table.getShortOffset(f1,f2) &&
       abs(int(f2-t2))>abs(int(f2-f1))){
      return true;
    }

    // Case 4: The destination square for m2 is defended by the moving piece in m1
    p = pos.piece_on(t1);

    osl::Piece pc = pos.osl_state->pieceAt(t1);
    if(pos.osl_state->hasEffectByPiece(pc,t2)){
      return true;
    }
#if 1
    if (!isPieceStand(f2) && pos.osl_state->hasLongEffectByPiece(pc, f2) &&
       Board_Table.getShort8Unsafe(BLACK, t1, f2) ==
       Board_Table.getShort8Unsafe(BLACK, t1, t2)){
      return true;
    }
#endif

    // Case 5: Discovered check, checking piece is the piece moved in m1

    pc=pos.osl_state->pieceAt(t2);
    if(isPiece(pc) && pos.osl_state->hasEffectByPiece(pc,f2) &&
       hasBlockableEffect(Ptype_Table.getEffect(p,t1,pos.king_square(pos.side_to_move()))) &&
       Board_Table.isBetweenSafe(f2,t1,pos.king_square(pos.side_to_move())) &&
       !Board_Table.isBetweenSafe(t2,t1,pos.king_square(pos.side_to_move())) &&       pos.osl_state->isPinOrOpen(pos.side_to_move(), pos.osl_state->pieceAt(t1))){
      return true;
    }


    return false;
  }


  // value_to_tt() adjusts a mate score from "plies to mate from the root" to
  // "plies to mate from the current ply".  Non-mate scores are unchanged.
  // The function is called before storing a value to the transposition table.

  Value value_to_tt(Value v, int ply) {

    if (v >= VALUE_MATE_IN_PLY_MAX)
      return v + ply;

    if (v <= VALUE_MATED_IN_PLY_MAX)
      return v - ply;

    return v;
  }


  // value_from_tt() is the rotate180 of value_to_tt(): It adjusts a mate score from
  // the transposition table to a mate score corrected for the current ply.

  Value value_from_tt(Value v, int ply) {

    if (v >= VALUE_MATE_IN_PLY_MAX)
      return v - ply;

    if (v <= VALUE_MATED_IN_PLY_MAX)
      return v + ply;

    return v;
  }


  // extension() decides whether a move should be searched with normal depth,
  // or with extended depth. Certain classes of moves (checking moves, in
  // particular) are searched with bigger depth than ordinary moves and in
  // any case are marked as 'dangerous'. Note that also if a move is not
  // extended, as example because the corresponding UCI option is set to zero,
  // the move is marked as 'dangerous' so, at least, we avoid to prune it.
  template <NodeType PvNode>
  Depth extension(const Position& pos, Move m, bool /* captureOrPromotion */,
                  bool moveIsCheck, bool* dangerous) {

    assert(m != MOVE_NONE);

    Depth result = DEPTH_ZERO;
    *dangerous = moveIsCheck;

    if (moveIsCheck && pos.see_sign(m) >= 0)
        result += CheckExtension[PvNode];


    return Min(result, ONE_PLY);
  }


  // connected_threat() tests whether it is safe to forward prune a move or if
  // is somehow connected to the threat move returned by null search.

  bool connected_threat(const Position& pos, Move m, Move threat) {

    assert(move_is_ok(m));

    assert(threat!=MOVE_NONE && move_is_ok(threat));
    assert(!pos.move_gives_check(m));
    assert(!pos.move_is_capture_or_promotion(m));

//    Square mfrom, mto, tfrom, tto;

    Square mfrom = move_from(m);
    Square mto = move_to(m);
    Square tfrom = move_from(threat);
    Square tto = move_to(threat);

    // Case 1: Don't prune moves which move the threatened piece
    if (mfrom == tto)
        return true;

    // Case 2: If the threatened piece has value less than or equal to the
    // value of the threatening piece, don't prune moves which defend it.
#if 1
    if (pos.move_is_capture(threat)
        && ( pos.midgame_value_of_piece_on(tfrom) >= pos.midgame_value_of_piece_on(tto))){
      osl::EffectContent effect = osl::Ptype_Table.getEffect(osl::ptypeO(m), mto, tto);
      if (osl::hasEffect(effect)) {
	if (hasUnblockableEffect(effect)) {
          return true;
        }
	osl::Direction d1 = osl::BoardTable::getLongDirection(BLACK, tto, mto);
	osl::Direction d2 = osl::BoardTable::getLongDirection(BLACK, tto, tfrom);
	if (d1 == d2){
	  if (pos.osl_state->isEmptyBetween(tfrom, mto)){
            return true;
          }
	}
	else if (pos.osl_state->isEmptyBetween(tto, mto)){
          return true;
        }
      }
    }
#else
    if (   pos.move_is_capture(threat)
        && (   pos.midgame_value_of_piece_on(tfrom) >= pos.midgame_value_of_piece_on(tto)

	       || pos.type_of_piece_on(tfrom) == osl::KING)
	   && pos.osl_state->hasEffectIf(osl::ptypeO(m), to(m), tto))
        return true;
#endif

    // Case 3: If the moving piece in the threatened move is a slider, don't
    // prune safe moves which block its ray.

    if (!isPieceStand(tfrom) && Board_Table.isBetweenSafe(mto,tfrom,tto)  && pos.see_sign(m) >= 0)
        return true;

    return false;
  }


  // ok_to_use_TT() returns true if a transposition table score
  // can be used at a given point in search.

  bool ok_to_use_TT(const TTEntry* tte, Depth depth, Value beta, int ply) {

    Value v = value_from_tt(tte->value(), ply);

    return   (   tte->depth() >= depth
              || v >= Max(VALUE_MATE_IN_PLY_MAX, beta)
              || v < Min(VALUE_MATED_IN_PLY_MAX, beta))

          && (   ((tte->type() & VALUE_TYPE_LOWER) && v >= beta)
              || ((tte->type() & VALUE_TYPE_UPPER) && v < beta));
  }


  // refine_eval() returns the transposition table score if
  // possible otherwise falls back on static position evaluation.

  Value refine_eval(const TTEntry* tte, Value defaultEval, int ply) {

      assert(tte);

      Value v = value_from_tt(tte->value(), ply);

      if (   ((tte->type() & VALUE_TYPE_LOWER) && v >= defaultEval)
          || ((tte->type() & VALUE_TYPE_UPPER) && v < defaultEval))
          return v;

      return defaultEval;
  }


  // update_history() registers a good move that produced a beta-cutoff
  // in history and marks as failures all the other moves of that ply.

  void update_history(const Position& /* pos */, Move move, Depth depth,
                      Move movesSearched[], int moveCount) {
    Move m;
    Value bonus = Value(int(depth) * int(depth));


    H.update(osl::ptypeO(move), move_to(move), bonus);

    for (int i = 0; i < moveCount - 1; i++)
    {
        m = movesSearched[i];

        assert(m != move);


        H.update(osl::ptypeO(m), move_to(m), -bonus);
    }
  }


  // update_gains() updates the gains table of a non-capture move given
  // the static position evaluation before and after the move.

  void update_gains(const Position& pos, Move m, Value before, Value after) {


    if (   !isPass(m)
        && before != VALUE_NONE
        && after != VALUE_NONE
        && pos.captured_piece_type() == PIECE_TYPE_NONE
        && !move_is_special(m))

      H.update_gain(osl::ptypeO(m), move_to(m), -(before + after));
  }


  // current_search_time() returns the number of milliseconds which have passed
  // since the beginning of the current search.

  int current_search_time(int set) {

    static int searchStartTime;

    if (set)
        searchStartTime = set;

    return get_system_time() - searchStartTime;
  }


  // value_to_uci() converts a value to a string suitable for use with the UCI
  // protocol specifications:
  //
  // cp <x>     The score from the engine's point of view in centipawns.
  // mate <y>   Mate in y moves, not plies. If the engine is getting mated
  //            use negative values for y.

  std::string value_to_uci(Value v) {

    std::stringstream s;


    if (abs(v) < VALUE_MATE - PLY_MAX * ONE_PLY)
      s << "cp " << int(v) * 100 / 200;
    else
      s << "cp " << int(v);

    return s.str();
  }


  // speed_to_uci() returns a string with time stats of current search suitable
  // to be sent to UCI gui.

  std::string speed_to_uci(int64_t nodes) {

    std::stringstream s;
    int t = current_search_time();

    s << " nodes " << nodes
      << " nps "   << (t > 0 ? int(nodes * 1000 / t) : 0)

      << " time "  << (t > 0 ? t : 1);

    return s.str();
  }


  // poll() performs two different functions: It polls for user input, and it
  // looks at the time consumed so far and decides if it's time to abort the
  // search.

  void poll(const Position& pos) {

    static int lastInfoTime;
    int t = current_search_time();

    //  Poll for input
    if (input_available())
    {
        // We are line oriented, don't read single chars
        std::string command;

        if (!std::getline(std::cin, command) || command == "quit")
        {
            // Quit the program as soon as possible
            Limits.ponder = false;
            QuitRequest = StopRequest = true;
            return;
        }

	else if (command.size() >= 5 && string(command,0,5) == "echo "){
	  cout << string(command,5) << endl;
	}

        else if (command == "stop" || command.find("gameover")==0)
        {
            // Stop calculating as soon as possible, but still send the "bestmove"
            // and possibly the "ponder" token when finishing the search.
            Limits.ponder = false;
            StopRequest = true;
        }
        else if (command == "ponderhit")
        {
            // The opponent has played the expected move. GUI sends "ponderhit" if
            // we were told to ponder on the same move the opponent has played. We
            // should continue searching but switching from pondering to normal search.
            Limits.ponder = false;

            if (StopOnPonderhit)
                StopRequest = true;
        }
    }

    // Print search information
    if (t < 1000)
        lastInfoTime = 0;

    else if (lastInfoTime > t)
        // HACK: Must be a new search where we searched less than
        // NodesBetweenPolls nodes during the first second of search.
        lastInfoTime = 0;

    else if (t - lastInfoTime >= 1000)
    {
        lastInfoTime = t;

        dbg_print_mean();
        dbg_print_hit_rate();

        // Send info on searched nodes as soon as we return to root
        SendSearchedNodes = true;
    }

    // Should we stop the search?
    if (Limits.ponder)
        return;

    bool stillAtFirstMove =    FirstRootMove
                           && !AspirationFailLow
                           &&  t > TimeMgr.available_time();

    bool noMoreTime =   t > TimeMgr.maximum_time()
                     || stillAtFirstMove;

    if (   (Limits.useTimeManagement() && noMoreTime)
        || (Limits.maxTime && t >= Limits.maxTime)
        || (Limits.maxNodes && pos.nodes_searched() >= Limits.maxNodes)) // FIXME
        StopRequest = true;
  }


  // wait_for_stop_or_ponderhit() is called when the maximum depth is reached
  // while the program is pondering. The point is to work around a wrinkle in
  // the UCI protocol: When pondering, the engine is not allowed to give a
  // "bestmove" before the GUI sends it a "stop" or "ponderhit" command.
  // We simply wait here until one of these commands is sent, and return,
  // after which the bestmove and pondermove will be printed.

  void wait_for_stop_or_ponderhit() {

    std::string command;

    // Wait for a command from stdin
    while (   std::getline(std::cin, command)
	      && command.find("gameover") != 0
	      && command != "ponderhit" && command != "stop" && command != "quit")

    {
      if (command.size() >= 5 && string(command,0,5) == "echo ")
	cout << string(command,5) << endl;
    }

    if (command != "ponderhit" && command != "stop" && command.find("gameover")!=0)
        QuitRequest = true; // Must be "quit" or getline() returned false
  }


  // When playing with strength handicap choose best move among the MultiPV set
  // using a statistical rule dependent on SkillLevel. Idea by Heinz van Saanen.
  void do_skill_level(Move* best, Move* ponder) {

    assert(MultiPV > 1);

    static RKISS rk;

    // Rml list is already sorted by pv_score in descending order
    int s;
    int max_s = -VALUE_INFINITE;
    int size = Min(MultiPV, (int)Rml.size());
    int max = Rml[0].pv_score;
    int var = Min(max - Rml[size - 1].pv_score, PawnValueMidgame);
    int wk = 120 - 2 * SkillLevel;

    // PRNG sequence should be non deterministic
    for (int i = abs(get_system_time() % 50); i > 0; i--)
        rk.rand<unsigned>();

    // Choose best move. For each move's score we add two terms both dependent
    // on wk, one deterministic and bigger for weaker moves, and one random,
    // then we choose the move with the resulting highest score.
    for (int i = 0; i < size; i++)
    {
        s = Rml[i].pv_score;

        // Don't allow crazy blunders even at very low skills
        if (i > 0 && Rml[i-1].pv_score > s + EasyMoveMargin)
            break;

        // This is our magical formula
        s += ((max - s) * wk + var * (rk.rand<unsigned>() % wk)) / 128;

        if (s > max_s)
        {
            max_s = s;
            *best = Rml[i].pv[0];
            *ponder = Rml[i].pv[1];
        }
    }
  }


  /// RootMove and RootMoveList method's definitions

  RootMove::RootMove() {

    nodes = 0;
    pv_score = non_pv_score = -VALUE_INFINITE;
    pv[0] = MOVE_NONE;
  }

  RootMove& RootMove::operator=(const RootMove& rm) {

    const Move* src = rm.pv;
    Move* dst = pv;

    // Avoid a costly full rm.pv[] copy
    do *dst++ = *src; while (*src++ != MOVE_NONE);

    nodes = rm.nodes;
    pv_score = rm.pv_score;
    non_pv_score = rm.non_pv_score;
    return *this;
  }

  void RootMoveList::init(Position& pos, Move searchMoves[]) {

    MoveStack mlist[MAX_MOVES];
    Move* sm;

    clear();
    bestMoveChanges = 0;

    // Generate all legal moves and add them to RootMoveList
    MoveStack* last = generate<MV_LEGAL>(pos, mlist);
    for (MoveStack* cur = mlist; cur != last; cur++)
    {
        // If we have a searchMoves[] list then verify cur->move
        // is in the list before to add it.

        for (sm = searchMoves; *sm!=MOVE_NONE && *sm != cur->move; sm++) {}


        if (searchMoves[0]!=MOVE_NONE && *sm != cur->move)
            continue;

        RootMove rm;
        rm.pv[0] = cur->move;
        rm.pv[1] = MOVE_NONE;
        rm.pv_score = -VALUE_INFINITE;
        push_back(rm);
    }
  }


  void RootMove::extract_pv_from_tt_rec(Position& pos,int ply) {
    TTEntry* tte;

    int dummy=0;

    if (   (tte = TT.probe(pos.get_key())) != NULL
           && tte->move(pos) != MOVE_NONE
           && pos.move_is_legal(tte->move(pos))
           && ply < PLY_MAX

           && (!pos.is_draw(dummy) || ply < 2))
    {
        pv[ply] = tte->move(pos);
	StateInfo st;
	pos.do_undo_move(pv[ply],st,
			 [&](osl::Square){
	  assert(pos.is_ok());
	    extract_pv_from_tt_rec(pos,ply+1);
	  }
	  );
    }
    else
      pv[ply] = MOVE_NONE;
  }

  // extract_pv_from_tt() builds a PV by adding moves from the transposition table.
  // We consider also failing high nodes and not only VALUE_TYPE_EXACT nodes. This
  // allow to always have a ponder move even when we fail high at root and also a
  // long PV to print that is important for position analysis.

  void RootMove::extract_pv_from_tt(Position& pos) {


    assert(pv[0] != MOVE_NONE && pos.move_is_legal(pv[0]));


    StateInfo st;
    pos.do_undo_move(pv[0],st,
		     [&](osl::Square){
	  assert(pos.is_ok());
	extract_pv_from_tt_rec(pos,1);
      }
      );
  }


  void RootMove::insert_pv_in_tt_rec(Position& pos,int ply) {
    TTEntry* tte;
    Key k;
    Value v, m = VALUE_NONE;
    k = pos.get_key();
    tte = TT.probe(k);

    // Don't overwrite existing correct entries
    if (!tte || tte->move(pos) != pv[ply])
    {
      v = (pos.in_check() ? VALUE_NONE : evaluate(pos, m));
      TT.store(k, VALUE_NONE, VALUE_TYPE_NONE, DEPTH_NONE, pv[ply], v, m);
    }
    if(pv[ply+1]!=MOVE_NONE){
      StateInfo st;
      pos.do_undo_move(pv[ply],st,
		       [&](osl::Square){
	  assert(pos.is_ok());
	  *(pos.eval+1)= *(pos.eval);
#if 1
            (pos.eval + 1)->previous = pos.eval;
#endif
	  pos.eval++;
	  pos.eval->update(*(pos.osl_state),pv[ply]);
	  insert_pv_in_tt_rec(pos,ply+1);
	  --pos.eval;
	}
	);
    }
  }

  // insert_pv_in_tt() is called at the end of a search iteration, and inserts
  // the PV back into the TT. This makes sure the old PV moves are searched
  // first, even if the old TT entries have been overwritten.

  void RootMove::insert_pv_in_tt(Position& pos) {


    assert(pv[0] != MOVE_NONE && pos.move_is_legal(pv[0]));


    insert_pv_in_tt_rec(pos,0);
  }

  // pv_info_to_uci() returns a string with information on the current PV line
  // formatted according to UCI specification.

  std::string RootMove::pv_info_to_uci(Position& pos, int depth, int selDepth, Value /* alpha */,
                                       Value /* beta */, int /* pvIdx */) {
    std::stringstream s;

    s << "info depth " << depth
      << " seldepth " << selDepth
      << " score " << value_to_uci(pv_score)
      << speed_to_uci(pos.nodes_searched())
      << " pv ";

    for (Move* m = pv; *m != MOVE_NONE; m++)

      s << move_to_uci(*m) << " ";

    return s.str();
  }

  // Specializations for MovePickerExt in case of Root node
  MovePickerExt<false, true>::MovePickerExt(const Position& p, Move ttm, Depth d,
                                            const History& h, SearchStack* ss, Value b)
                            : MovePicker(p, ttm, d, h, ss, b), firstCall(true) {
    Move move;
    Value score = VALUE_ZERO;

    // Score root moves using standard ordering used in main search, the moves
    // are scored according to the order in which they are returned by MovePicker.
    // This is the second order score that is used to compare the moves when
    // the first orders pv_score of both moves are equal.
    while ((move = MovePicker::get_next_move()) != MOVE_NONE)
        for (rm = Rml.begin(); rm != Rml.end(); ++rm)
            if (rm->pv[0] == move)
            {
                rm->non_pv_score = score--;
                break;
            }

    Rml.sort();
    rm = Rml.begin();
  }

  Move MovePickerExt<false, true>::get_next_move() {

    if (!firstCall)
        ++rm;
    else
        firstCall = false;

    return rm != Rml.end() ? rm->pv[0] : MOVE_NONE;
  }

} // namespace


// ThreadsManager::idle_loop() is where the threads are parked when they have no work
// to do. The parameter 'sp', if non-NULL, is a pointer to an active SplitPoint
// object for which the current thread is the master.

void ThreadsManager::idle_loop(int threadID, SplitPoint* sp) {

  assert(threadID >= 0 && threadID < MAX_THREADS);

  int i;
  bool allFinished;

  while (true)
  {
      // Slave threads can exit as soon as AllThreadsShouldExit raises,
      // master should exit as last one.
      if (allThreadsShouldExit)
      {
          assert(!sp);
          threads[threadID].state = Thread::TERMINATED;
          return;
      }

      // If we are not thinking, wait for a condition to be signaled
      // instead of wasting CPU time polling for work.
      while (   threadID >= activeThreads
             || threads[threadID].state == Thread::INITIALIZING
             || (useSleepingThreads && threads[threadID].state == Thread::AVAILABLE))
      {
          assert(!sp || useSleepingThreads);
          assert(threadID != 0 || useSleepingThreads);

          if (threads[threadID].state == Thread::INITIALIZING)
              threads[threadID].state = Thread::AVAILABLE;

          // Grab the lock to avoid races with Thread::wake_up()
          lock_grab(&threads[threadID].sleepLock);

          // If we are master and all slaves have finished do not go to sleep
          for (i = 0; sp && i < activeThreads && !sp->is_slave[i]; i++) {}
          allFinished = (i == activeThreads);

          if (allFinished || allThreadsShouldExit)
          {
              lock_release(&threads[threadID].sleepLock);
              break;
          }

          // Do sleep here after retesting sleep conditions
          if (threadID >= activeThreads || threads[threadID].state == Thread::AVAILABLE)
              cond_wait(&threads[threadID].sleepCond, &threads[threadID].sleepLock);

          lock_release(&threads[threadID].sleepLock);
      }

      // If this thread has been assigned work, launch a search
      if (threads[threadID].state == Thread::WORKISWAITING)
      {
          assert(!allThreadsShouldExit);

          threads[threadID].state = Thread::SEARCHING;

          // Copy split point position and search stack and call search()
          // with SplitPoint template parameter set to true.
#ifdef MOVE_STACK_REJECTIONS
          SearchStack ss_base[PLY_MAX_PLUS_2];
	  SplitPoint* tsp = threads[threadID].splitPoint;
          Position pos(*tsp->pos, threadID);
	  int ply=tsp->ss->ply;
	  assert(0< ply && ply+3<PLY_MAX_PLUS_2);
	  for(int i=0;i<ply-1;i++)
	    ss_base[i].currentMove=(tsp->ss-ply+i)->currentMove;
	  SearchStack *ss= &ss_base[ply-1];
          memcpy(ss, tsp->ss - 1, 4 * sizeof(SearchStack));
          (ss+1)->sp = tsp;
#else
          SearchStack ss[PLY_MAX_PLUS_2];
          SplitPoint* tsp = threads[threadID].splitPoint;
          Position pos(*tsp->pos, threadID);

          memcpy(ss, tsp->ss - 1, 4 * sizeof(SearchStack));
          (ss+1)->sp = tsp;
#endif
	  uint64_t es_base[(PLY_MAX_PLUS_2*sizeof(eval_t)+sizeof(uint64_t)-1)/sizeof(uint64_t)];
	  eval_t *es=(eval_t *)&es_base[0];
	  assert(tsp->pos->eval);
	  es[0]= *(tsp->pos->eval);
	  pos.eval= &es[0];

          if (tsp->pvNode)
              search<PV, true, false>(pos, ss+1, tsp->alpha, tsp->beta, tsp->depth);
          else
              search<NonPV, true, false>(pos, ss+1, tsp->alpha, tsp->beta, tsp->depth);

          assert(threads[threadID].state == Thread::SEARCHING);

          threads[threadID].state = Thread::AVAILABLE;

          // Wake up master thread so to allow it to return from the idle loop in
          // case we are the last slave of the split point.
          if (   useSleepingThreads
              && threadID != tsp->master
              && threads[tsp->master].state == Thread::AVAILABLE)
              threads[tsp->master].wake_up();
      }

      // If this thread is the master of a split point and all slaves have
      // finished their work at this split point, return from the idle loop.
      for (i = 0; sp && i < activeThreads && !sp->is_slave[i]; i++) {}
      allFinished = (i == activeThreads);

      if (allFinished)
      {
          // Because sp->slaves[] is reset under lock protection,
          // be sure sp->lock has been released before to return.
          lock_grab(&(sp->lock));
          lock_release(&(sp->lock));

          // In helpful master concept a master can help only a sub-tree, and
          // because here is all finished is not possible master is booked.
          assert(threads[threadID].state == Thread::AVAILABLE);

          threads[threadID].state = Thread::SEARCHING;
          return;
      }
  }
}

#if 0
void do_checkmate(Position& pos, int mateTime){
  cout << "checkmate notimplemented";
  return;
}
#else
void do_checkmate(Position& pos, int depth){
  Move best_move;
  bool r = osl::FixedDepthSearcher::hasCheckmate(*(pos.osl_state), pos.osl_state->turn(), depth, best_move);
  cout << "checkmate " << (r ? move_to_uci(best_move) : "timeout") << endl;
  return;
}
#endif

void show_tree(Position &pos){
  show_tree_rec(pos);
}
