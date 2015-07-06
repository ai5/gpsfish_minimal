ifdef PROFILE
RELEASE = true
endif

GTAGS=/opt/local/bin/gtags 
# INCLUDES += -I../lib
# INCLUDES += -I/opt/local/include
CXX=g++
GXX=g++
# CXX=g++-mp-4.8
# GXX=g++-mp-4.8

# BOOST_POSTFIX=-mt
# BOOST_POSTFIX=
# LOADLIBES += -L/opt/local/lib -lboost_filesystem$(BOOST_POSTFIX) -lboost_serialization$(BOOST_POSTFIX) -lboost_iostreams$(BOOST_POSTFIX) -lboost_system$(BOOST_POSTFIX) -lboost_date_time$(BOOST_POSTFIX) -lboost_exception$(BOOST_POSTFIX) -lpthread
# LOADLIBES += -L/opt/local/lib -lboost_filesystem$(BOOST_POSTFIX) -lboost_serialization$(BOOST_POSTFIX) -lboost_iostreams$(BOOST_POSTFIX) -lboost_system$(BOOST_POSTFIX) -lboost_date_time$(BOOST_POSTFIX) -lpthread

# LOADLIBES += -L/opt/local/lib -lpthread
LOADLIBES += -Wl,-rpath,/usr/local/lib64 -L/usr/local/lib64 -L/opt/local/lib -lpthread -lws2_32
LDFLAGS += -static

# -lboost_thread$(BOOST_POSTFIX_MT)  -lboost_program_options$(BOOST_POSTFIX) 

## #gcc
OTHERFLAGS = -pipe $(CPUOPTION)
OTHERFLAGS += # -DGENERATE_PAWNDROP_CHECKMATE
# RELEASE_CXX_OPTFLAGS= -O3 
# RELEASE_CXX_OPTFLAGS= -march=corei7 -O3 -g -DNDEBUG
RELEASE_CXX_OPTFLAGS= -march=native -O3 -g -DNDEBUG
# RELEASE_CXX_OPTFLAGS= -march=native -O1 -g
# RELEASE_CXX_OPTFLAGS= -march=native -O3 -g 
ifdef DEBUG
CXXOPTFLAGS = -O
OTHERFLAGS += -g -DDEBUG 
else
CXXOPTFLAGS = $(RELEASE_CXX_OPTFLAGS)
WARNING_FLAGS += $(WARN_INLINE)
ifdef RELEASE
# OTHERFLAGS += -DNDEBUG
endif
ifndef PROF
CXXOPTFLAGS += -fomit-frame-pointer
endif
endif

OTHERFLAGS += -DGPSFISH_HOME=\"`pwd`\"

#CXXOPTFLAGS+=-std=c++11 -Wall -Wno-missing-braces
CXXOPTFLAGS+=-std=c++0x -Wall -Wno-missing-braces -flax-vector-conversions -Wcast-qual -fno-rtti -pedantic -Wno-long-long -Wextra -Wshadow 


# OTHERFLAGS += -msse4.2 -DOSL_USE_SSE41
# OTHERFLAGS += -DNDEBUG
# no prefetch
# OTHERFLAGS += -DNO_PREFETCH

# CXXOPTFLAGS += -std=c++0x
# INCLUDES += -I../lib
CXXFLAGS = $(PROF) $(OTHERFLAGS) $(CXXOPTFLAGS) $(WARNING_FLAGS) $(INCLUDES)

LIBSRC=osl_checkmate.cc usi.cc tables.cc osl_eval.cc osl_position.cc osl_types.cc whitelist.cc

# king8Info.cc mobilityTable.cc numEffectState.cc numSimpleEffect.cc  simpleState.cc effectedNumTable.cc oslmove.cc squareCompressor.cc bookInMemory.cc openingBook.cc compactBoard.cc hashKey.cc neighboring8Direct.cc weights.cc see.cc 

# king8.cc kingTable.cc majorPiece.cc minorPiece.cc ml_pieceEval.cc ml_pieceStand.cc piecePair.cc piecePairKing.cc pieceEval.cc openMidEndingEval.cc pin.cc   pool_allocator.cc newProgress.cc pathEncoding.cc 
# pieceCost.cc 
#  dfpn.cc fixedDepthSearcher.cc  pieceOnBoard.cc   csa.cc
LIBOBJS=$(patsubst %.cc,%.o,$(LIBSRC))

SRCS = benchmark.cpp misc.cpp timeman.cpp evaluate.cpp move.cpp position.cpp tt.cpp main.cpp movegen.cpp search.cpp uci.cpp book.cpp movepick.cpp thread.cpp ucioption.cpp
OBJS = $(patsubst %.cpp,%.o,$(SRCS))
CC = $(CXX)
PROFILE_USE = -fprofile-use -fprofile-correction
PROFILE_GENERATE = -fprofile-generate 

all: 
	$(MAKE) programs RELEASE=t

programs : gpsfish

.deps/%.cpp.d: %.cpp
	if [ ! -d `dirname $@` ]; then mkdir -p `dirname $@`; fi;
	$(GXX) -MM $(CXXFLAGS) $< \
		| sed 's/\(.*\.o\)[ :]*/$(subst /,\/,$(patsubst .deps/%.cpp.d,%.o,$@)) $(subst /,\/,$@) : /g' > $@
	grep $< $@ > /dev/null 2>&1 || (rm -f $@; false)

gpsfish: $(OBJS) $(LIBOBJS) $(FILE_OSL_ALL)
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $(OBJS) $(LIBOBJS) $(FILE_OSL_ALL) $(LDLIBS) $(LOADLIBES)
	if [ -x $(GTAGS) ] ; then $(GTAGS) -v; fi

gpsfish1: mainone.cc
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $< $(LDLIBS) $(LOADLIBES)

gpsfishoneprof: mainone.cc
	$(CXX) -o $@ $(CXXFLAGS) $(PROFILE_GENERATE) $(LDFLAGS) $< $(LDLIBS) $(LOADLIBES)

gpsfishone: mainone.cc gpsfishoneprof
	$(CXX) -o $@ $(CXXFLAGS) $(PROFILE_USE) $(LDFLAGS) $< $(LDLIBS) $(LOADLIBES)

update-gpsfishone:
	-rm gpsfishone gpsfishoneprof
	$(MAKE) gpsfishoneprof RELEASE=t
	$(MAKE) run-profile
	$(MAKE) gpsfishone RELEASE=t

run-profile: gpsfishoneprof
	./gpsfishoneprof bench 32 1 12 default depth

joseki.dat: gpsfish FORCE
	-mv negaMaxBook.txt negaMaxBook.txt.back
	scp -p gopteron14:~ktanaka/work/gpsshogi/data/negaMaxBook.txt .
	./gpsfish makewbook negaMaxBook.txt $@

FORCE:

clean:
	-rm *.o $(PROGRAM)
	-rm -rf .deps

light-clean:
	-rm -rf .deps .objs .gch 

-include $(patsubst %.cpp,.deps/%.cpp.d,$(SRCS))

.PHONY: all clean light-clean run-profile update-gpsusione
