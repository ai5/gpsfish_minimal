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

#if !defined(TT_H_INCLUDED)
#define TT_H_INCLUDED

#include <iostream>

#include "move.h"
using osl::SquareCompressor;
#include "position.h"
#include "types.h"


/// The TTEntry is the class of transposition table entries
///
/// A TTEntry needs 128 bits to be stored
///
/// bit  0-31: key
/// bit 32-63: data
/// bit 64-79: value
/// bit 80-95: depth
/// bit 96-111: static value
/// bit 112-127: margin of static value
///
/// the 32 bits of the data field are so defined
///
/// bit  0-15: move
/// bit 16-20: not used
/// bit 21-22: value type
/// bit 23-31: generation

// enum Move16 {MOVE16_NONE = 0};
using osl::Move16;
#if 0
inline Move16 toMove16(Move m){
  Move16 move16;
  if(m==MOVE_NONE) return Move16::NONE;
  if(isDrop(m))
    move16=Move16(0x80+(uint16_t)ptype(m)+((SquareCompressor::compress(to(m)))<<8));
  else if(isPromotion(m)){
    move16=Move16(SquareCompressor::compress(from(m))+(SquareCompressor::compress(to(m))<<8)+0x8000);
  }
  else{
    move16=Move16(SquareCompressor::compress(from(m))+(SquareCompressor::compress(to(m))<<8));
  }
  return move16;
}
#endif
inline Move fromMove16(Move16 move16,Position const& p) {
  if(move16==Move16::NONE) return MOVE_NONE;
  Player turn=p.side_to_move();
  Square to=SquareCompressor::melt((int16_t(move16)>>8)&0x7f);
  if((int16_t(move16)&0x80)!=0){
    Ptype ptype=(Ptype)(int16_t(move16)-0x80);
    return osl::newMove(to,ptype,turn);
  }
  Square from=SquareCompressor::melt(int16_t(move16)&0x7f);
  Ptype ptype=p.type_of_piece_on(from);
  Ptype capture_ptype=p.type_of_piece_on(to);
  bool is_promote=(int16_t(move16)&0x8000)!=0;
  if(is_promote)
    return osl::newMove(from,to,ptype,capture_ptype,true,turn);
  else
    return osl::newMove(from,to,ptype,capture_ptype,false,turn);
}
class TTEntry {

public:
  void save(uint32_t k, Value v, ValueType t, Depth d, Move16 m, int g, Value statV, Value statM) {

    key32        = (uint32_t)k;
    move16       = (uint16_t)m;
    valueType    = (uint8_t)t;
    generation8  = (uint8_t)g;
    value16      = (int16_t)v;
    depth16      = (int16_t)d;
    staticValue  = (int16_t)statV;
    staticMargin = (int16_t)statM;
  }
  void save(uint32_t k, Value v, ValueType t, Depth d, Move m, int g, Value statV, Value statM) {
    return save(k,v,t,d,toMove16(m),g,statV,statM);
  }
  void set_generation(int g) { generation8 = (uint8_t)g; }

  uint32_t key() const              { return key32; }
  Depth depth() const               { return (Depth)depth16; }
  Move16 move16Val() const { 
    return (Move16)move16; 
  }
  Move move(Position const& pos) const
  { return fromMove16((Move16)move16,pos); }
  Value value() const               { return (Value)value16; }
  ValueType type() const            { return (ValueType)valueType; }
  int generation() const            { return (int)generation8; }
  Value static_value() const        { return (Value)staticValue; }
  Value static_value_margin() const { return (Value)staticMargin; }

private:
  uint32_t key32;
  uint16_t move16;
  uint8_t valueType, generation8;
  int16_t value16, depth16, staticValue, staticMargin;
};


/// This is the number of TTEntry slots for each cluster
const int ClusterSize = 4;


/// TTCluster consists of ClusterSize number of TTEntries. Size of TTCluster
/// must not be bigger than a cache line size. In case it is less, it should
/// be padded to guarantee always aligned accesses.

struct TTCluster {
  TTEntry data[ClusterSize];
};


/// The transposition table class. This is basically just a huge array containing
/// TTCluster objects, and a few methods for writing and reading entries.

class TranspositionTable {

  TranspositionTable(const TranspositionTable&);
  TranspositionTable& operator=(const TranspositionTable&);

public:
  TranspositionTable();
  ~TranspositionTable();
  void set_size(size_t mbSize);
  void clear();
  void store(const Key posKey, Value v, ValueType type, Depth d, Move16 m, Value statV, Value kingD);
  void store(const Key posKey, Value v, ValueType type, Depth d, Move m, Value statV, Value kingD);
  TTEntry* probe(const Key posKey) const;
  void new_search();
  TTEntry* first_entry(const Key posKey) const;
  void refresh(const TTEntry* tte) const;

private:
  size_t size;
  TTCluster* entries;
  uint8_t generation; // Size must be not bigger then TTEntry::generation8
};

extern TranspositionTable TT;


/// TranspositionTable::first_entry() returns a pointer to the first entry of
/// a cluster given a position. The lowest order bits of the key are used to
/// get the index of the cluster.

inline TTEntry* TranspositionTable::first_entry(const Key posKey) const {

  return entries[((uint32_t)posKey) & (size - 1)].data;
}


/// TranspositionTable::refresh() updates the 'generation' value of the TTEntry
/// to avoid aging. Normally called after a TT hit.

inline void TranspositionTable::refresh(const TTEntry* tte) const {

  const_cast<TTEntry*>(tte)->set_generation(generation);
}


/// A simple fixed size hash table used to store pawns and material
/// configurations. It is basically just an array of Entry objects.
/// Without cluster concept or overwrite policy.

template<class Entry, int HashSize>
struct SimpleHash {

  typedef SimpleHash<Entry, HashSize> Base;

  void init() {

    if (entries)
        return;

    entries = new (std::nothrow) Entry[HashSize];
    if (!entries)
    {
        std::cerr << "Failed to allocate " << HashSize * sizeof(Entry)
                  << " bytes for hash table." << std::endl;
        exit(EXIT_FAILURE);
    }
    memset(entries, 0, HashSize * sizeof(Entry));
  }

  virtual ~SimpleHash() { delete [] entries; }

  Entry* probe(Key key) const { return entries + ((uint32_t)key & (HashSize - 1)); }
  void prefetch(Key key) const { ::prefetch((char*)probe(key)); }

protected:
  Entry* entries;
};

#endif // !defined(TT_H_INCLUDED)
