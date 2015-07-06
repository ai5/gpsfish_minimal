#include "osl_position.h"
#include "usi.h"

#include <iostream>
#include <stdexcept>
#include <bitset>
#include <iostream>
#include <iostream>
#include <stdexcept>
#if defined __INTEL_COMPILER
/* __v2di */
#include <immintrin.h>
#endif
#include <emmintrin.h>
#include <iostream>


std::ostream& osl::operator<<(std::ostream& os,const osl::BoardMask& mask)
{
#if 1
  return os << "{" << mask.contents[0] << "," << mask.contents[1] << "}";
#else
  os << "{" << mask.contents[0] << "," << mask.contents[1] << "}";
  mask.each([&](Square sq){os << sq;});
  return os;
#endif
}
/* mobilityTable.cc
 */

bool osl::operator==(MobilityTable const& mt1, MobilityTable const& mt2)
{
  for(int num = 32; num <= 39; num++){
    for(Direction Dir : SHORT8_DIRECTIONS)
      if(mt1.get(Dir, num) != mt2.get(Dir, num)) return false;
  }
  return true;
}


template <bool show_error>
bool osl::NumEffectState::isAlmostValidDrop(Move move) const
{
  assert(isPieceStand(from(move)));
  const Square to=osl::to(move);
  const Piece to_piece=pieceAt(to);
  const Ptype ptype=osl::ptype(move);
  const Player pl = player(move);
  // check if the target square is empty
  if (! isEmpty(to_piece)) {
    if (show_error) std::cerr << "drop on to piece : " << move << std::endl;
    return false;
  }
  // player in turn has a ptype piece on one's piecestand
  if (! hasPieceOnStand(pl, ptype)) {
    if (show_error) std::cerr << pl << " don't have : " << ptype << std::endl;
    return false;
  }
  // check double pawn
  if (ptype==PAWN && isPawnMaskSet(pl, X(to))) {
    if (show_error) std::cerr << " Double Pawn : " << move << std::endl;
    return false;
  }
  return true;
}

template <bool show_error>
bool
osl::NumEffectState::testValidityOtherThanEffect(Move move) const
{
  const Square from=osl::from(move);
  const Piece from_piece = pieceAt(from);
  const Square to=osl::to(move);
  const Piece to_piece=pieceAt(to);
  // from square is occupied by a piece of the turn 
  if (isEmpty(from_piece) 
      || (owner(from_piece) != turn()))
  {
    if (show_error) 
      std::cerr << " No such piece0 : " << move << std::endl;
    return false;
  }
  if (osl::ptype(from_piece) != oldPtype(move)){
    if (show_error) 
      std::cerr << " No such piece1  : " << move << std::endl;
    return false;
  }
  // to square is occupied by the opponent's piece or empty
  if (!isEmpty(to_piece) && owner(to_piece)==turn()) {
    if (show_error) std::cerr << " No move on  : " << move << std::endl;
    return false;
  }
  // check the capturePtype
  if (ptype(to_piece)!=capturePtype(move)) {
    if (show_error) std::cerr << " Not such capture : " << move 
			      << std::endl << *this;
    return false;
  }
  return true;
}

bool osl::NumEffectState::isValidMove(Move move,bool show_error) const
{
  if (turn() != player(move)) {
    if (show_error) {
      std::cerr << "invalid player move : " << move << std::endl;
      std::cerr << *this;
    }
    return false;
  }
  if (! isValidMoveByRule(move, show_error) || ! isValid(move))
    return false;
  return isAlmostValidMove(move, show_error);
}


std::ostream& osl::operator<<(std::ostream& os,const osl::NumEffectState& state)
{
  return os << osl::usi::show(state);
}
/* numSimpleEffect.cc
 */

void osl::
NumSimpleEffectTable::init(const NumEffectState& state)
{
  std::fill(effects.begin(), effects.end(), NumBitmapEffect());
  effected_changed_mask.fill(PieceMask(0));
  effected_mask.fill(PieceMask(0));
  effectedNumTable.clear();
  for(int num=0; num < 40; num++){
    if(!test(state.usedMask(), num)) continue;
    if (isOnBoard(state.pieceOf(num))){
      Piece p = state.pieceOf(num);
      doEffect<NumBitmapEffect::Add, true>(state, p);
    }
  }
}

bool osl::operator==(const NumSimpleEffectTable& et1,const NumSimpleEffectTable& et2)
{
  for(int y=1;y<=9;y++)
    for(int x=9;x>0;x--){
      Square pos=newSquare(x,y);
      if (!(et1.effectSetAt(pos).pm == et2.effectSetAt(pos).pm)) return false;
    }
  if (! (et1.effected_mask == et2.effected_mask))
    return false;
  if(!(et1.mobilityTable==et2.mobilityTable)) return false;
  if(!(et1.effectedNumTable==et2.effectedNumTable)) return false;
  // intentionally ignore history dependent members: changed_effects, changed_effect_pieces, effected_changed_mask
  return true;
}

namespace osl
{
    template<NumBitmapEffect::Op OP,bool UC>
    void NumSimpleEffectTable::doEffectShort(Player P,Ptype T,Direction Dir, const NumEffectState& state,Square pos,int num)
    {
      if(hasMove(T, Dir)){
	Square target = pos + newOffset(P, Dir);
	int num1 = number(state[target]);
	if(isEdgeNum(num1)) return;
	effects[I(target)].opEqual(OP, NumBitmapEffect::makeEffect(P, num));
	if(UC){
	  changed_effects.set(target);
	  if(!isEmptyNum(num1)){
	    if(OP == NumBitmapEffect::Add){
	      set(effected_mask[I(P)], num1);
	    }
	    else{ // OP==Sub
	      if(effects[I(target)].countEffect(P) == 0){
		reset(effected_mask[I(P)], num1);
	      }
	    }
	    set(effected_changed_mask[I(P)], num1);
	  }
	}
      }
    }
    template<NumBitmapEffect::Op OP,bool UC>
    void NumSimpleEffectTable::doEffectLong(Player P,Ptype T,Direction Dir, const NumEffectState& state,Square pos,int num)
    {
      if (hasMove(T,(P==BLACK ? Dir : rotate180Long(Dir)))){
	const Offset offset=newOffset(BLACK,Dir);
	assert(offset != Offset::ZERO);
	NumBitmapEffect effect=NumBitmapEffect::makeLongEffect(P,num);
	const Direction SD=longToShort(Dir);
	if(OP==NumBitmapEffect::Sub){
	  Square ePos=mobilityTable.get(longToShort(Dir),num);
	  int count=((SD==D || SD==DL || SD==DR) ? Y(ePos)-Y(pos) :
		     ( (SD==U || SD==UL || SD==UR) ? Y(pos)-Y(ePos) :
		       ( SD==L ? X(ePos)-X(pos) : X(pos)-X(ePos))));
	  assert(0<=count && count<=9);
	  if(UC){
	    for(int i=1;i<count;i++){
	      pos+=offset;
	      effects[I(pos)].opEqual(OP, effect);
	      changed_effects.set(pos);
	    }
//		Piece p;
	    mobilityTable.set(longToShort(Dir),num,Square::STAND);
	    int num1=number(state.pieceAt(ePos));
	    if (!isEdgeNum(num1)){
	      effectedNumTable[num1][SD]=EMPTY_NUM;
	      effects[I(ePos)].opEqual(OP, effect);
	      set(effected_changed_mask[I(P)], num1);
	      changed_effects.set(ePos);
	      if(effects[I(ePos)].countEffect(P)==0){
		reset(effected_mask[I(P)], num1);
	      }
	    }
	  }
	  else{
	    for(int i=0;i<count;i++){
	      pos+=offset;
	      effects[I(pos)].opEqual(OP, effect);
	    }
	    int num1=number(state.pieceAt(ePos));
	    if (!isEdgeNum(num1))
	      effectedNumTable[num1][SD]=EMPTY_NUM;
	  }
	}
	else{ // OP==Add
	  for (;;)
	  {
	    pos=pos+offset;
	    int num1 = number(state[pos]);
	    if(isEdgeNum(num1)){
	      if(UC) mobilityTable.set(longToShort(Dir), num, pos);
	      break;
	    }
	    if(UC){
	      changed_effects.set(pos);
	    }
	    effects[I(pos)].opEqual(OP, effect);
	    // effect内にemptyを含むようにしたら短くなる
	    if (!isEmptyNum(num1)){
	      effectedNumTable[num1][SD]=num;
	      if(UC){
		mobilityTable.set(longToShort(Dir),num,pos);
		effectedNumTable[num1][SD]=num;
		changed_effects.set(pos);
		set(effected_mask[I(P)], num1);
		set(effected_changed_mask[I(P)], num1);
	      }
	      break;
	    }
	  }
	}
      }
    }
/**
 * posに駒を設置/削除して長い利きをブロック/延長する際の利きデータの更新.
 * xorなのでposに元々駒があって，取り除く時にも呼び出せる．
 * @param state - 局面の状態 posに駒を置く前でも後でもよい
 * @param pos - 変化する位置
 */
  template<osl::NumBitmapEffect::Op OP,bool UC>
  void osl::
  NumSimpleEffectTable::doBlockAt(const NumEffectState& state,Square pos,int piece_num)
  {
    if(UC) setChangedPieces(effects[I(pos)].pm);
    PieceMask mask1 = state.longEffect(pos);
    while(any(mask1)){
      int num=takeOneBit(mask1);
      assert(32<=num && num<=39);
      Piece p1=state.pieceOf(num);
      Player pl1=owner(p1);
      assert(ptype(p1)!=PPAWN);
      Square pos1=square(p1);
      Offset offset0;
      Direction d=Board_Table.getShort8(BLACK, pos1, pos, offset0);
      if(OP==NumBitmapEffect::Sub){
	Square endSquare=mobilityTable.get(d,num);
	NumBitmapEffect effect=NumBitmapEffect::makeLongEffect(pl1,num);
//      Piece p;
	Square pos2=pos+offset0;
	for(; pos2 != endSquare; pos2 += offset0){
	  if(UC) changed_effects.set(pos2);
	  effects[I(pos2)].opEqual(OP, effect);
	}
	effects[I(pos2)].opEqual(OP, effect);
	int num1 = number(state[endSquare]);
	if (!isEdgeNum(num1)){
	  effectedNumTable[num1][d] = EMPTY_NUM;
	  if(UC){
	    changed_effects.set(pos2);
	    if(effects[I(endSquare)].countEffect(pl1) == 0){
	      reset(effected_mask[I(pl1)], num1);
	    }
	    set(effected_changed_mask[I(pl1)], num1);
	    mobilityTable.set(d, num, pos);
	  }
	}
	else 
	  mobilityTable.set(d,num,pos);
	effectedNumTable[piece_num][d]=num;
      }
      else{
	NumBitmapEffect effect=NumBitmapEffect::makeLongEffect(pl1,num);
	Square pos2=pos+offset0;
	for(;;){
	  int num1=number(state[pos2]);
	  if(!isEmptyNum(num1)){
	    if(UC){
	      mobilityTable.set(d,num,pos2);
	      if(!isEdgeNum(num1)){
		effectedNumTable[num1][d]=num;
		effects[I(pos2)].opEqual(OP, effect);
		changed_effects.set(pos2);
		set(effected_mask[I(pl1)], num1);
		set(effected_changed_mask[I(pl1)], num1);
	      }
	    }
	    else if(!isEdgeNum(num1)){
	      effectedNumTable[num1][d]=num;
	      effects[I(pos2)].opEqual(OP, effect);
	    }
	    break;
	  }
	  if(UC) changed_effects.set(pos2);
	  effects[I(pos2)].opEqual(OP, effect);
	  pos2+=offset0;
	}
      }
    }
  }
}


/* effectedNumTable.cc
 */
void
osl::EffectedNumTable::clear()
{
  for(int i=0;i<40;i++) contents[i].clear();
}

std::ostream& osl::operator<<(std::ostream& os,EffectedNumTable const& et)
{
  os << "[\n";
  for(int num=0;num<=39;num++){
    os << " [";
    for(int d=0;d<7;d++) os << et[num][static_cast<Direction>(d)] << ",";
    os << et[num][static_cast<Direction>(7)] << "],\n";
  }
  return os <<  "]\n";
}
bool osl::operator==(EffectedNumTable const& e1, EffectedNumTable const& e2)
{
  for(int i=0;i<8;i++){
    for(int num=0;num<=39;num++){
      if(e1[num][static_cast<Direction>(i)]!=e2[num][static_cast<Direction>(i)]) return false;
    }
  }
  return true;
}

template<osl::NumBitmapEffect::Op OP,bool UC>
void  osl::
NumSimpleEffectTable::doEffect(const NumEffectState& state,PtypeO ptypeo,Square pos,int num)
{
  doEffectBy<OP,UC>(getOwner(ptypeo),getPtype(ptypeo),state,pos,num); 
}

template<osl::NumBitmapEffect::Op OP,bool UC>
void  osl::
NumSimpleEffectTable::doEffectBy(osl::Player P, osl::Ptype T, const NumEffectState& state,Square pos,int num)
{
  if(UC){
    if(T==LANCE || T==BISHOP || T==PBISHOP || T==ROOK || T==PROOK)
      setChangedPieces(NumBitmapEffect::makeLongEffect(P,num).pm);
    else
      setChangedPieces(NumBitmapEffect::makeEffect(P,num).pm);
  }
  for(Direction d : SHORT_DIRECTIONS){
    doEffectShort<OP,UC>(P,T,d, state, pos, num);
  }
  for(Direction d : LONG_DIRECTIONS){
    doEffectLong<OP,UC>(P,T,d, state,pos,num);
  }
}

/* boardMask.cc
 */

osl::
BoardMaskTable5x5::BoardMaskTable5x5()
{
  for (int cx=1; cx<=9; ++cx) {
    for (int cy=1; cy<=9; ++cy) {
      const int min_x = std::max(1, cx - 2);
      const int max_x = std::min(9, cx + 2);
      const int min_y = std::max(1, cy - 2);
      const int max_y = std::min(9, cy + 2);
      BoardMask m;
      m.clear();
      for (int x=min_x; x<=max_x; ++x) {
	for (int y=min_y; y<=max_y; ++y) {
	  m.set(BoardMask::index(x,y));
	}
      }
      data[I(newSquare(cx,cy))] = m;
    }
  }
}

osl::
BoardMaskTable3x3::BoardMaskTable3x3()
{
  for (int cx=1; cx<=9; ++cx) {
    for (int cy=1; cy<=9; ++cy) {
      const int min_x = std::max(1, cx - 1);
      const int max_x = std::min(9, cx + 1);
      const int min_y = std::max(1, cy - 1);
      const int max_y = std::min(9, cy + 1);
      BoardMask m;
      m.clear();
      for (int x=min_x; x<=max_x; ++x) {
	for (int y=min_y; y<=max_y; ++y) {
	  m.set(BoardMask::index(x,y));
	}
      }
      data[I(newSquare(cx,cy))] = m;
    }
  }
}

osl::
BoardMaskTable5x3Center::BoardMaskTable5x3Center()
{
  for (int cx=1; cx<=9; ++cx) {
    for (int cy=1; cy<=9; ++cy) {
      const Square center = Centering5x3::adjustCenter(newSquare(cx, cy));
      const int min_x = std::max(1, X(center) - 2);
      const int max_x = std::min(9, X(center) + 2);
      const int min_y = std::max(1, Y(center) - 1);
      const int max_y = std::min(9, Y(center) + 1);
      BoardMask m;
      m.clear();
      for (int x=min_x; x<=max_x; ++x) {
	for (int y=min_y; y<=max_y; ++y) {
	  m.set(BoardMask::index(x,y));
	}
      }
      data[I(newSquare(cx,cy))] = m;
    }
  }
}

osl::BoardMaskTableDir::BoardMaskTableDir()
{
  for (int x = 1; x <= 9; ++x) {
    for (int y = 1; y <= 9; ++y) {
      Square sq = newSquare(x, y);
      for(Direction d : SHORT8_DIRECTIONS){
	BoardMask m; m.clear();
	Offset o = newOffset(BLACK, d);
	for(Square sq1 = sq + o; isOnBoard(sq1); sq1 += o)
	  m.set(sq1);
	data[I(sq)][I(d)] = m;
      }
    }
  }
}

osl::Promotion37Mask::Promotion37Mask()
{
  for(Player P : COLORS){
    data[I(P)].clear();
    int rank = (P == BLACK ? 3 : 7);
    for (int x = 1; x <= 9; ++x) 
      data[I(P)].set(newSquare(x, rank));
  }
}

/* numEffectState.cc
 */


bool osl::operator==(const osl::NumEffectState& st1,
		     const osl::NumEffectState& st2)
{
  if (!(st1.effects == st2.effects)) 
    return false;
  if (!(st1.pieces_onboard == st2.pieces_onboard)) 
    return false;
  if (!(st1.piece_mask == st2.piece_mask)) return false;
  if (!(st1.promoted == st2.promoted)) 
    return false;
  if (!(st1.pin_or_open == st2.pin_or_open)) 
    return false;
  if (!(st1.king_mobility == st2.king_mobility)) 
    return false;
  if (!(st1.king8infos == st2.king8infos)) 
    return false;
  if (st1.turn()!=st2.turn()) 
    return false;
  if (st1.pawnPos!=st2.pawnPos) return false;
  for (int y=1;y<=9;y++)
    for (int x=9;x>0;x--) {
      Piece p1=st1.pieceAt(newSquare(x,y));
      Piece p2=st2.pieceAt(newSquare(x,y));
      if (osl::ptypeO(p1)!=osl::ptypeO(p2)) return false;
    }
  return true;
}

void osl::NumEffectState::makeKing8Info(osl::Player P)
{
#ifdef ALLOW_KING_ABSENCE
  if (isPieceStand(kingSquare(P)))
    return;
#endif
  king8infos[I(P)]=newKing8Info(P, *this);
}
void osl::NumEffectState::
updateKing8Info(std::array<PieceMask, 2> const& pin_or_open_backup){
  for(Player pl : COLORS)
    if(changedSquare().anyInRange(Board_Mask_Table3x3.mask(kingSquare(pl)))
    || pin_or_open[I(pl)] != pin_or_open_backup[I(pl)])
      makeKing8Info(pl);
}


void osl::
NumEffectState::initEffects() {
  initPawnPos();
  effects.init(*this);
  resetAll(promoted);
  for(Player P : COLORS){
    resetAll(pieces_onboard[I(P)]);
    resetAll(effects.effected_mask[I(P)]);
    resetAll(effects.effected_changed_mask[I(P)]);
  }
  piece_mask.clear();
  for(int num=0;num<40;num++){
    if(!test(used_mask, num)) continue;
    Piece p=pieceOf(num);
    if (osl::isOnBoard(p)){
      piece_mask.set(square(p));
      set(pieces_onboard[I(owner(p))], num);
      if (isPromoted(p))
	set(promoted, num);
      for(Player pl : COLORS){
	if(hasEffect(pl,square(p))){
	  set(effects.effected_mask[I(pl)], num);
	  set(effects.effected_changed_mask[I(pl)], num);
	}
      }
    }
  }
  for(Player P : COLORS){
    makePinOpen(P);
    if(osl::isOnBoard(kingSquare(P)))   
      king8infos[I(P)] = newKing8Info(P, *this);
  }
}
void osl::
NumEffectState::init() {
  player_to_move=BLACK;
  for (int ipos=0;ipos<Square_SIZE;ipos++) {
    setBoard(Square(ipos),Piece_EDGE);
  }
  for (int y=1;y<=9;y++)
    for (int x=9;x>0;x--) {
      setBoard(newSquare(x,y),Piece_EMPTY);
    }
  //  promoteMask.clearAll();
  for(Player P : COLORS){
    resetAll(stand_mask[I(P)]);
    stand_count[I(P)].fill(0);
  }
  resetAll(used_mask);
  for (int num=0;num<Piece_SIZE;num++){
#if 0
    pieces[num]=newPiece(WHITE, ptypes[num], num, Square::STAND);
#else
    pieces[num] = Piece_EDGE;
#endif
  }
#if 0
  initEffects();
#endif
}
osl::
NumEffectState::NumEffectState() 
{
  init();
}
osl::
NumEffectState::~NumEffectState() 
{
}

void osl::NumEffectState::initPawnPos(){
  for(Ptype ptype : PieceStandOrder)
    for(Player P : COLORS)
      stand_count[I(P)][ptype - Ptype::BASIC_MIN] = countPiecesOnStandBit(P, ptype);

  for(Player c : COLORS) pawnPos[I(c)].fill(0);
  for(int num=indexMin(PAWN);
      num< indexLimit(PAWN); num++){
    Piece p=pieceOf(num);
    Player player=owner(p);
    Square pos=square(p);
    if(!isPieceStand(pos) && !isPromoted(p)){
      if (isPawnMaskSet(player,X(pos)))
	throw "2FU!";
      setPawn(player,pos);
    }
  }
}

bool osl::NumEffectState::isValidMoveByRule(Move move,bool show_error) 
{
  assert(isNormal(move));
  const Square from=osl::from(move);
  const Square to=osl::to(move);
  const Ptype ptype=osl::ptype(move);
  const Player turn = player(move);
    
  if (isPieceStand(from)){ // 打つ手
    // 動けない場所ではないか?
    if (! canDropTo(turn,ptype,to)){
      if (show_error) std::cerr << " can't drop to : " << move << std::endl;
      return false;
    }
  }
  else{
    if (!isPromoted(osl::ptype(move)) && isPromotion(move)) {
      if (show_error) std::cerr << " inconsistent promote " << move << ",V(move)=" << V(move) << ",ptype(move)=" << osl::ptype(move) << std::endl;
      return false;
    }
    const PtypeO old_ptypeo = oldPtypeO(move);
    const EffectContent effect
      = Ptype_Table.getEffect(old_ptypeo, from, to);
    // その offsetの動きがptypeに関してvalidか?
    if (!hasUnblockableEffect(effect)){
      const Offset o = offset(effect);
      if (o==Offset::ZERO) {
	if (show_error) {
	  std::cerr << " No such move1 : " << move << std::endl;
	}
	return false;
      }
    }
    // promoteしている時にpromote可能か
    if (isPromotion(move)) {
      if (! (canPromote(unpromote(osl::ptype(move)))
	     && (canPromote(player(move),to) 
		 || canPromote(player(move),from)))){
	if (show_error) 
	  std::cerr << " illegal promote type or position : " << move << std::endl;
	return false;
      }
    }
    // promoteしていない時に強制promoteでないか?
    if ((!isPromoted(ptype)
	 && mustPromote(turn,getPtype(old_ptypeo),from)) 
	&& !isPromotion(move)) {
      if (show_error) 
	std::cerr << " must promote to this position : " << move << std::endl;
      return false;
    }
  }
  return true;
}

void osl::NumEffectState::setPiece(Player player,Square pos,Ptype ptype) {
  int num;
  for(num = 0; num < 40; num++) {
    if (!test(used_mask, num) && ptypes[num]==unpromote(ptype)
	&& (ptype != KING || num == indexMin(KING) + I(player))) {
      set(used_mask, num);
      Piece p = newPiece(player, ptype, num, pos);
      setPieceOf(num, p);
      if (isPieceStand(pos)) set(stand_mask[I(player)], num);
      else{
	setBoard(pos,p);
	if (ptype==PAWN) setPawn(player,pos);
      }
      return;
    }
  }
  std::cerr << "osl::SimpleState::setPiece! maybe too many pieces " 
	    << ptype << " " << pos << " " << player << "\n";
  abort();
}

osl::Piece osl::
NumEffectState::selectCheapPiece(PieceMask e) const
{
  Piece cheap=Piece_EMPTY;
  PieceMask effect1 = e;
  int v=16;
  for(Piece p : allPiece(effect1)){
    int v1=exVal[I(ptype(p))];
    if(v1<v) {v=v1; cheap=p; }
  }
  return cheap;
}

void osl::NumEffectState::makeNormalMove(Move move)
{
  assert(turn() == player(move));
  assert(!isPass(move));
  assert(isAlmostValidMove(move));
  const Square from=osl::from(move);
  const Square to=osl::to(move);
  if (isPieceStand(from)) makeDropMove(turn(), to, osl::ptype(move));
  else {
    const Piece captured = pieceAt(to);
    if (captured != Piece_EMPTY)
      makeCaptureMove(turn(), from, to, captured, isPromotion(move));
    else
      makeSimpleMove(turn(), from, to, isPromotion(move));
  }
  changeTurn();
}

void osl::NumEffectState::
makeSimpleMove(Player P,Square from, Square to, bool is_promote)
{
  std::array<PieceMask,2> pin_or_open_backup = pin_or_open;
  Piece oldPiece=pieceAt(from);
  Piece newPiece=checkPromote(oldPiece, is_promote);
  newPiece+=(to-from);
  int num=number(oldPiece);

  PtypeO oldPtypeO=osl::ptypeO(oldPiece);
  PtypeO new_ptypeo=osl::ptypeO(newPiece);
  // 自分自身の効きを外す
  setPieceOf(num,newPiece);
  effects.clearChangedEffects();
  effects.clearEffectedChanged();
  effects.doEffect<NumBitmapEffect::Sub,true>(*this,oldPtypeO,from,num);
  // 自分自身がブロックしていたpromote?の延長
  // あるいは自分自身のブロック
  effects.effectedNumTable[num].clear();
  setBoard(to,newPiece);
  piece_mask.reset(from);
  piece_mask.set(to);
  if(getPtype(new_ptypeo)==PAWN) setPawn(P,to);
  effects.doBlockAt<NumBitmapEffect::Sub,true>(*this,to,num);
  setBoard(from,Piece_EMPTY);
  effects.doBlockAt<NumBitmapEffect::Add,true>(*this,from,num);
  effects.doEffect<NumBitmapEffect::Add,true>(*this,new_ptypeo,to,num);

  if (oldPtypeO == newPtypeO(P,KING))
    makePinOpen(P);
  else {
    Direction lastD=UL;
    reset(pin_or_open[I(P)], num);
    recalcPinOpen(from,lastD,P);
    recalcPinOpen(to,lastD,P);
  }
  {
    Direction lastD=UL;
    reset(pin_or_open[I(alt(P))], num);
    recalcPinOpen(from,lastD,alt(P));
    recalcPinOpen(to,lastD,alt(P));
  }
  if (is_promote){
    set(promoted, num);
    if(num < indexLimit(PAWN)) clearPawn(P, to);
  }
  if(hasEffect(BLACK,to))
    set(effects.effected_mask[I(BLACK)], num);
  else
    reset(effects.effected_mask[I(BLACK)], num);
  if(hasEffect(WHITE,to))
    set(effects.effected_mask[I(WHITE)], num);
  else
    reset(effects.effected_mask[I(WHITE)], num);
  set(effects.effected_changed_mask[I(BLACK)], num);
  set(effects.effected_changed_mask[I(WHITE)], num);
  BoardMask changed=changedEffects();
  changed.set(from);
  changed.set(to);
  effects.changed_square = changed;
  updateKing8Info(pin_or_open_backup);
}


void osl::NumEffectState::
makeDropMove(osl::Player P, Square to, Ptype ptype)
{
  std::array<PieceMask,2> pin_or_open_backup = pin_or_open;
  if(ptype == PAWN) setPawn(P, to);
  int num=bsf(standMask(P),ptype);
  Piece oldPiece=pieceOf(num);
  Piece newPiece=oldPiece;
  newPiece+=to-Square::STAND;
  osl::PtypeO ptypeO = osl::ptypeO(newPiece);
  setPieceOf(num,newPiece);
  effects.clearChangedEffects();
  effects.clearEffectedChanged();
  effects.doBlockAt<NumBitmapEffect::Sub,true>(*this,to,num);
  effects.doEffect<NumBitmapEffect::Add,true>(*this,ptypeO,to,num);
  setBoard(to,newPiece);
  flip(stand_mask[I(P)],num);
  flip(pieces_onboard[I(P)],num);
  piece_mask.set(to);
  stand_count[I(P)][ptype-Ptype::BASIC_MIN]--;
  {
    Direction lastD=UL;
    recalcPinOpen(to,lastD,P);
  }
  {
    Direction lastD=UL;
    recalcPinOpen(to,lastD,alt(P));
  }
  if(hasEffect(BLACK,to))
    set(effects.effected_mask[I(BLACK)], num);
  else
    reset(effects.effected_mask[I(BLACK)], num);
  if (hasEffect(WHITE,to))
    set(effects.effected_mask[I(WHITE)], num);
  else
    reset(effects.effected_mask[I(WHITE)], num);
  set(effects.effected_changed_mask[I(BLACK)], num);
  set(effects.effected_changed_mask[I(WHITE)], num);
  BoardMask changed=changedEffects();
  changed.set(to);
  effects.changed_square = changed;
  updateKing8Info(pin_or_open_backup);
}

void osl::NumEffectState::
makeCaptureMove(Player P,Square from, Square to, Piece target, 
		bool is_promote)
{
  std::array<PieceMask,2> pin_or_open_backup = pin_or_open;
  int num1=number(target);
  flip(pieces_onboard[I(alt(P))],num1);
  piece_mask.reset(from);
  flip(stand_mask[I(P)], num1);
  Piece oldPiece=pieceAt(from);
  Piece newPiece=checkPromote(oldPiece, is_promote);
  newPiece+=(to-from);
  int num0=number(oldPiece);
  setPieceOf(num0,newPiece);
  setPieceOf(num1,captured(target));
      
  PtypeO oldPtypeO=osl::ptypeO(oldPiece);
  PtypeO new_ptypeo=osl::ptypeO(newPiece);
  if(getPtype(new_ptypeo)==PAWN) setPawn(P,to);
  PtypeO capturePtypeO=osl::ptypeO(target);
  if(getPtype(capturePtypeO) == PAWN) clearPawn(alt(P), to);
  stand_count[I(P)][unpromote(getPtype(capturePtypeO))-Ptype::BASIC_MIN]++;
  effects.clearChangedEffects();
  effects.clearEffectedChanged();
  effects.setChangedPieces(effect(to));
  effects.doEffect<NumBitmapEffect::Sub,true>(*this,capturePtypeO,to,num1);
  effects.doEffect<NumBitmapEffect::Sub,true>(*this,oldPtypeO,from,num0);
  setBoard(from,Piece_EMPTY);
  effects.doBlockAt<NumBitmapEffect::Add,true>(*this,from,num0);
  effects.effectedNumTable[num0]=effects.effectedNumTable[num1];
  effects.effectedNumTable[num1].clear();
  setBoard(to,newPiece);
  effects.doEffect<NumBitmapEffect::Add,true>(*this,new_ptypeo,to,num0);

  if (oldPtypeO == newPtypeO(P,KING))
    makePinOpen(P);
  else {
    Direction lastD=UL;
    reset(pin_or_open[I(P)], num0);
    reset(pin_or_open[I(P)], num1); // captured is not pin
    recalcPinOpen(from,lastD,P);
    recalcPinOpen(to,lastD,P);
  }
  {
    Direction lastD=UL;
    reset(pin_or_open[I(alt(P))], num0);
    reset(pin_or_open[I(alt(P))], num1); // captured is not pin
    recalcPinOpen(from,lastD,alt(P));
    recalcPinOpen(to,lastD,alt(P));
  }
  reset(promoted, num1);
  reset(effects.effected_mask[I(BLACK)], num1);
  reset(effects.effected_mask[I(WHITE)], num1);
  if (is_promote){
    set(promoted, num0);
    if(num0 < indexLimit(PAWN)) clearPawn(P, to);
  }
  if(hasEffect(BLACK,to))
    set(effects.effected_mask[I(BLACK)], num0);
  else
    reset(effects.effected_mask[I(BLACK)], num0);
  if(hasEffect(WHITE,to))
    set(effects.effected_mask[I(WHITE)], num0);
  else
    reset(effects.effected_mask[I(WHITE)], num0);
  set(effects.effected_changed_mask[I(BLACK)], num0);
  set(effects.effected_changed_mask[I(WHITE)], num0);
  BoardMask changed=changedEffects();
  changed.set(from);
  changed.set(to);
  effects.changed_square = changed;
  updateKing8Info(pin_or_open_backup);
}

template <bool show_error>
bool
#if (defined __GNUC__) && (! defined GPSONE) && (! defined GPSUSIONE)
__attribute__ ((used,noinline))
#endif
osl::NumEffectState::isAlmostValidMove(Move move) const{
  if(!isValid(move)){
    std::cerr << *this << std::endl;
    std::cerr << move << std::endl;
  }
  assert(isValid(move));
  assert(isNormal(move));
  assert(this->turn() == player(move));
  assert(isValidMoveByRule(move, true));

  const Square from=osl::from(move);
  if (isPieceStand(from)) // 打つ手
    return isAlmostValidDrop<show_error>(move);
  const Square to=osl::to(move);
  const Piece from_piece = this->pieceAt(from);
    
  if (! testValidityOtherThanEffect<show_error>(move))
    return false;
  if(!hasEffectByPiece(from_piece,to)){
    if (show_error) {
      std::cerr << " No such move2 : " << move << std::endl;
    }
    return false;
  }
  return true;
}

bool osl::NumEffectState::
isAlmostValidMove(Move move,bool show_error) const{
  if(show_error)
    return isAlmostValidMove<true>(move);
  else
    return isAlmostValidMove<false>(move);
}


void osl::NumEffectState::
makePinOpen(osl::Player defense)
{
  PieceMask pins = PieceMask(0);
#ifdef ALLOW_KING_ABSENCE
 if(isPieceStand(kingSqure(defense))) return pins;
#endif
  PieceMask mask = piecesOnBoard(alt(defense));
  for(Direction Dir : SHORT8_DIRECTIONS)
    makePinOpenDir(Dir, kingSquare(defense), pins, mask, defense);
  pin_or_open[I(defense)] = pins;
}

namespace{
  using namespace osl;
  using osl::Piece;
//  using namespace osl::eval;

  void findAdditionalPieces(const NumEffectState& state, Player attack, 
			    Square target,
			    Piece from_piece,
			    PieceVector& out)
  {
    Direction d = Board_Table.getShort8Unsafe(BLACK, square(from_piece), target);
    if(d == Direction::INVALID_VALUE) return;
    assert(isShort8(d) || (std::cerr << "V(d)=" << V(d) << ",from_piece=" << from_piece << ",target=" << target << ",state=" << state << std::endl,0));
    int num = number(from_piece);
    int num1 = state.longEffectNumTable()[num][Direction(d)];
    if(isEmptyNum(num1)) return;
    osl::Piece p=state.pieceOf(num1);
    if(owner(p) != attack) return;
    out.push_back(p);
  }

  template <Player P>
    void findEffectPieces(const NumEffectState& state, Square effect_to,
			  PieceVector& my_pieces, 
			  PieceVector& op_pieces)
{
  for(osl::Piece p : state.allPiece(state.effect(alt(P), effect_to)))
    op_pieces.push_back(p);
  if (op_pieces.empty())
    return;
  op_pieces.sort();
  if ((int)op_pieces.size() <= state.countEffect(P, effect_to))
  {
    for(osl::Piece p : state.allPiece(state.effect(P, effect_to)))
    my_pieces.push_back(p);
    my_pieces.sort();
    return;
  }
  PieceVector my_pieces_more;
  for(osl::Piece p : state.allPiece(state.effect(P, effect_to))){
    my_pieces.push_back(p);
    findAdditionalPieces(state, P, effect_to, p, my_pieces_more);
  }
  my_pieces.sort();
  // sort my_pieces_more ?
  my_pieces.push_back(my_pieces_more.begin(), my_pieces_more.end());
  
  if (op_pieces.size() <= my_pieces.size())
    return;
  my_pieces_more.clear();
  // gather shadow efect
  for (size_t i=0; i<op_pieces.size(); ++i) {
    findAdditionalPieces(state, P, effect_to, op_pieces[i], my_pieces_more);
  }
  my_pieces.push_back(my_pieces_more.begin(), my_pieces_more.end());
}

template <osl::Player P>
void findEffectPiecesAfterMove(const NumEffectState& state, Move move,
			       PieceVector& my_pieces, 
			       PieceVector& op_pieces)
{
  const Square from=osl::from(move);
  const Square to=osl::to(move);

  for(osl::Piece p : state.allPiece(state.effect(alt(P), to)))
  op_pieces.push_back(p);
  if (op_pieces.empty())
    return;
  op_pieces.sort();

  osl::Piece moved = state[from];
  PieceMask ignore=PieceMask(0);		// here do not use my_pin to get optimistic result
  set(ignore, number(moved));
  if ((int)op_pieces.size() < state.countEffect(P, to))
  {
    for(osl::Piece p : state.allPiece(state.effect(P, to) & ~ignore))
    my_pieces.push_back(p);
    my_pieces.sort();
    return;
  }

  PieceVector my_pieces_more;
  findAdditionalPieces(state, player(move), to, moved, my_pieces_more);

  for(osl::Piece p : state.allPiece(state.effect(P, to) & ~ignore)){
    my_pieces.push_back(p);
    findAdditionalPieces(state, P, to, p, my_pieces_more);
  }
  my_pieces.sort();
  // sort my_pieces_more ?
  my_pieces.push_back(my_pieces_more.begin(), my_pieces_more.end());

  if (op_pieces.size() < my_pieces.size())
    return;
  my_pieces_more.clear();
  // gather shadow efect
  for (size_t i=0; i<op_pieces.size(); ++i) {
    findAdditionalPieces(state, P, to, op_pieces[i], my_pieces_more);
  }
  my_pieces.push_back(my_pieces_more.begin(), my_pieces_more.end());
}

template <Player P>
int computeValue(const NumEffectState& state, Move move,
		  PieceVector& my_pieces, 
		  PieceVector& op_pieces,
		 const PieceMask& my_pin, const PieceMask& op_pin)
{
  Square target=to(move), move_from=from(move);
  PtypeO ptypeO=osl::ptypeO(move);

  int val = 0;
  std::array<int,Piece_SIZE> vals;
  constexpr Player Opponent = alt(P);
  size_t i;
  int c=0;
  bool op_deleted=false, my_deleted=false;
  for (i=0;i<op_pieces.size();i++,c++)
  {
    if(c>10) break; // avoid infinite loop
      {
	osl::Piece p=op_pieces[i];
	int num=number(p);
	if(num==kingIndex(Opponent) && my_deleted) break;
	assert(int(owner(p))==int(Opponent));
	if(test(op_pin, num) && !state.pinnedCanMoveTo(Opponent, p, target) &&
	   ptypeO!=newPtypeO(P,KING)){
	  osl::Piece attacker=state.pinAttacker<Opponent>(p);
	  assert(owner(attacker)==P || (csaShow(std::cerr, state), std::cerr << attacker << "," << p << ",move=" << move << ",op_pin=" << op_pin << ",state.pin_or_open=" << state.pin_or_open << std::endl, 0));
	  Square attacker_sq=square(attacker);
	  if(attacker_sq != move_from){
	    size_t j=0;
	    for(;j<my_pieces.size();j++) if(my_pieces[j]==attacker) break;
	    if(i<=j){
	      if(j==my_pieces.size() || op_pieces.size()<=j+1 ){
		for(size_t k=i;k<op_pieces.size()-1;k++)
		  op_pieces[k]=op_pieces[k+1];
		op_pieces.pop_back();
		op_deleted=true;
	      }
	      else{
		Piece v=op_pieces[i];
		for(size_t k=i;k<=j;k++)
		  op_pieces[k]=op_pieces[k+1];
		op_pieces[j+1]=v;
	      }
	      i--;
	      continue;
	    }
	  }
	  // pin move?
	}
      }
    vals[i*2]=val;
    // opponent moves
    val += captureValue(ptypeO);
    {
      ptypeO = osl::ptypeO(op_pieces[i]);
      const bool promotable = canPromote(ptypeO) 
	&& (canPromote(Opponent,target) 
	    || canPromote(Opponent,square(op_pieces[i])));
      if (promotable)
      {
	ptypeO=promote(ptypeO);
	val += promoteValue(ptypeO);
      }
    }
    vals[i*2+1]=val;
    // my moves
  retry:
    if (i>=my_pieces.size()){
      break;
    }
      {
	Piece p=my_pieces[i];
	int num=number(p);
	assert(owner(p)==P);
	if(num==kingIndex(P) && op_deleted) break;
	if(test(my_pin, num) && !state.pinnedCanMoveTo(P, p, target) &&
	   ptypeO!=newPtypeO(Opponent,KING)){
	  osl::Piece attacker=state.pinAttacker<P>(p);
	  assert(int(owner(attacker))==int(Opponent));
	  size_t j=0;
	  for(;j<op_pieces.size();j++) if(op_pieces[j]==attacker) break;
	  if(i<j){
	    if(j==op_pieces.size() || my_pieces.size()<=j ){
	      for(size_t k=i;k<my_pieces.size()-1;k++)
		my_pieces[k]=my_pieces[k+1];
	      my_pieces.pop_back();
	      my_deleted=true;
	    }
	    else{
	      Piece  v=my_pieces[i];
	      for(size_t k=i;k<j;k++)
		my_pieces[k]=my_pieces[k+1];
	      my_pieces[j]=v;
	    }
	    goto retry;
	  }
	  // pin move?
	}
      }
      val += captureValue(ptypeO);
    {
      ptypeO=osl::ptypeO(my_pieces[i]);
      const bool promotable = canPromote(ptypeO) 
	&& (canPromote(P,target) 
		|| canPromote(P,square(my_pieces[i])));
      if (promotable)
      {
	ptypeO=promote(ptypeO);
	val += promoteValue(ptypeO);
      }
    }
  }
  for (int j=i-1;j>=0;j--)
  {
    if(P==BLACK){
      val = std::max(val, vals[j*2+1]);
      val = std::min(val, vals[j*2]);
    }
    else{
      val = std::min(val, vals[j*2+1]);
      val = std::max(val, vals[j*2]);
    }
  }
  return val;
}

template <osl::Player P>
int seeInternal(const NumEffectState& state, Move move,
		const PieceMask& my_pin, const PieceMask& op_pin)
{
  assert(state.isAlmostValidMove(move));
  const Square from=osl::from(move);
  const Square to=osl::to(move);
  PieceVector my_pieces, op_pieces;
  int val=0; 
  if (isPieceStand(from))
  {
    findEffectPieces<P>(state, to, my_pieces, op_pieces);
  }
  else
  {
    val = diffWithMove(state,move);
    findEffectPiecesAfterMove<P>(state, move, my_pieces, op_pieces);
  }
  if (op_pieces.empty())
    return val;
  return val + computeValue<P>(state, move, my_pieces, op_pieces, my_pin, op_pin);
}
}

int osl::NumEffectState::see(osl::Move move) const
{
  if (player(move) == BLACK)
    return seeInternal<BLACK>(*this, move, pin(BLACK), pin(WHITE));
  else
    return -seeInternal<WHITE>(*this, move, pin(WHITE), pin(BLACK));
}


void osl::csaShow(std::ostream& os, NumEffectState const& st){
  constexpr char const * csastr[14] = {"TO", "NY", "NK", "NG", "UM", "RY", "OU", "KI", 
		      "FU", "KY", "KE", "GI", "KA", "HI"};
  for(int y = 1; y < 10; y++){
    os << "P" << std::to_string(y);
    for(int x = 9; x >= 1; x--){
      Piece p = st[newSquare(x, y)];
      if (isEmpty(p)) os << " . ";
      else os << (owner(p) == BLACK ? "+" : "-") << csastr[PI(ptype(p))];
    }
    os << std::endl;
  }
  for(Player P : COLORS){
    os << "P" << (P == BLACK ? "+" : "-");
    for(Ptype pt = Ptype::BASIC_MIN; pt <= Ptype::MAX; pt++){
      for(int i = 0; i < st.countPiecesOnStand(P, pt); i++)
	os << "00" << csastr[PI(pt)];
    }
    os << std::endl;
  }
  os << (st.turn() == BLACK ? "+" : "-") << std::endl;
}



// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
