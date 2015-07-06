#include "osl_types.h"

#include "osl_eval.h"


#include "osl_checkmate.h"

namespace osl
{
  const BoardTable Board_Table;
  // PtypeTable depends on BoardTable
  const PtypeTable Ptype_Table;
#ifndef DFPNSTATONE
//   const eval::PtypeEvalTable eval::Ptype_Eval_Table;
//  eval::ml::OpenMidEndingPtypeTable eval::ml::OpenMidEndingEval::Piece_Value;
#endif
  const Centering5x3::Table Centering5x3::table;
#if 0
  const BoardMask Board_Mask_Table[Square_SIZE] = {
     {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0}, {0,0}, {0,0}, {0,0}, {0,0}, 
       {0,0}, {0,0},{1ull << 1, 0ull}, {1ull << 2, 0ull}, {1ull << 3, 0ull}, {1ull << 4, 0ull}, {1ull << 5, 0ull}, {1ull << 6, 0ull}, {1ull << 7, 0ull}, {1ull << 8, 0ull}, {1ull << 9, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 10, 0ull}, {1ull << 11, 0ull}, {1ull << 12, 0ull}, {1ull << 13, 0ull}, {1ull << 14, 0ull}, {1ull << 15, 0ull}, {1ull << 16, 0ull}, {1ull << 17, 0ull}, {1ull << 18, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 19, 0ull}, {1ull << 20, 0ull}, {1ull << 21, 0ull}, {1ull << 22, 0ull}, {1ull << 23, 0ull}, {1ull << 24, 0ull}, {1ull << 25, 0ull}, {1ull << 26, 0ull}, {1ull << 27, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 28, 0ull}, {1ull << 29, 0ull}, {1ull << 30, 0ull}, {1ull << 31, 0ull}, {1ull << 32, 0ull}, {1ull << 33, 0ull}, {1ull << 34, 0ull}, {1ull << 35, 0ull}, {1ull << 36, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 37, 0ull}, {1ull << 38, 0ull}, {1ull << 39, 0ull}, {1ull << 40, 0ull}, {1ull << 41, 0ull}, {1ull << 42, 0ull}, {1ull << 43, 0ull}, {1ull << 44, 0ull}, {1ull << 45, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 46, 0ull}, {1ull << 47, 0ull}, {1ull << 48, 0ull}, {1ull << 49, 0ull}, {1ull << 50, 0ull}, {1ull << 51, 0ull}, {1ull << 52, 0ull}, {1ull << 53, 0ull}, {1ull << 54, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{1ull << 55, 0ull}, {1ull << 56, 0ull}, {1ull << 57, 0ull}, {1ull << 58, 0ull}, {1ull << 59, 0ull}, {1ull << 60, 0ull}, {1ull << 61, 0ull}, {1ull << 62, 0ull}, {1ull << 63, 0ull}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{0ull, 1ull << 0}, {0ull, 1ull << 1}, {0ull, 1ull << 2}, {0ull, 1ull << 3}, {0ull, 1ull << 4}, {0ull, 1ull << 5}, {0ull, 1ull << 6}, {0ull, 1ull << 7}, {0ull, 1ull << 8}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
       {0,0}, {0,0},{0ull, 1ull << 9}, {0ull, 1ull << 10}, {0ull, 1ull << 11}, {0ull, 1ull << 12}, {0ull, 1ull << 13}, {0ull, 1ull << 14}, {0ull, 1ull << 15}, {0ull, 1ull << 16}, {0ull, 1ull << 17}, {0,0}, {0,0}, {0,0}, {0,0}, {0,0},
      {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0},  {0,0}, {0,0}
  };
#endif
  //  const BoardMaskTable Board_Mask_Table;
  const BoardMaskTable5x5 Board_Mask_Table5x5;
  const BoardMaskTable3x3 Board_Mask_Table3x3;
  const BoardMaskTable5x3Center Board_Mask_Table5x3_Center;
  const BoardMaskTableDir Board_Mask_Table_Dir;
  const Promotion37Mask Promotion37_Mask;
  const ImmediateCheckmateTable Immediate_Checkmate_Table;
}


// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
