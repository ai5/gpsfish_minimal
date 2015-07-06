/* usi.h
 */
#ifndef OSL_USI_H
#define OSL_USI_H

#include "move.h"
#include <vector>
#include <string>
#include <stdexcept>

/* psn.h
 */

namespace osl
{
  class NumEffectState;
  /**
   * gnushogi で使われるフォーマット.
   * 何種類かある．
   */
  class psn
  {
  public:
    class ParseError : public std::invalid_argument
    {
    public:
      ParseError(const std::string& msg = "")
	: invalid_argument(msg)
      { }
    };
    static Move strToMove(const std::string&, const NumEffectState&);
    static Square strToPos(const std::string&);
    static Ptype charToPtype(char);

    static const std::string show(Move);
    static const std::string show(Square);
    static char show(Ptype);

    /** decorate capture by 'x', promote by '+', and unpromote by '=' */
    static const std::string showXP(Move);
  };
} // osl

namespace osl
{
  const std::string StartPositionFEN = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
  class NumEffectState;
  class usi
  {
  public:
    static Move strToMove(const std::string&, const NumEffectState&);
    static PtypeO charToPtypeO(char);

    static const std::string show(Move);
    static const std::string show(PtypeO);
    static const std::string show(Piece);
    static const std::string show(const NumEffectState&);

    class ParseError : public std::invalid_argument
    {
    public:
      ParseError(const std::string& msg = "")
	: invalid_argument(msg)
      { }
    };

    /** 
     * 盤面を取得する. 
     * board文字列が不正なときは、ParseErrorがthrowされる. 
     * @param board USIの文字列
     * @param state boardの解析結果が出力される
     */
    static void parseBoard(const std::string& board, NumEffectState&);
    /**  [sfen <sfenstring> | startpos ] moves <move1> ... <movei> */
    static void parse(const std::string& line, NumEffectState&);
    static void parse(const std::string& line, NumEffectState& initial, std::vector<Move>& moves);

    static NumEffectState makeState(const std::string& line);
  };
} // osl

#endif /* OSL_USI_H */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
