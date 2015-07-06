CXX=icl

LOADLIBES += 
CXXFLAGS = -Qstd=c++11

# CXXFLAGS += -fast -DNDEBUG
CXXFLAGS += -Od -debug

# OTHERFLAGS += -DNDEBUG

OTHERFLAGS += # -DOSL_NO_SSE=1

PROFILE_GENERATE = -Qprof_gen
PROFILE_USE = -Qprof_use

CXXFLAGS += $(OTHERFLAGS)

%.obj: %.cpp
	$(CXX) -c $(CXXFLAGS) -o $@ $<

%.obj: %.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

LIBSRC=osl_checkmate.cc usi.cc tables.cc osl_eval.cc osl_position.cc osl_types.cc

LIBOBJS=$(patsubst %.cc,%.obj,$(LIBSRC))

SRCS = benchmark.cpp misc.cpp timeman.cpp evaluate.cpp move.cpp position.cpp tt.cpp main.cpp movegen.cpp search.cpp uci.cpp book.cpp movepick.cpp thread.cpp ucioption.cpp
OBJS = $(patsubst %.cpp,%.obj,$(SRCS))
# OSL_HOME_FLAGS = -DOSL_HOME=\"$(shell dirname `dirname \`pwd\``)/osl\"
OSL_HOME_FLAGS =
CC = $(CXX)

LDFLAGS += -F10000000

all: programs

programs : gpsfish.exe

gpsfish.exe: $(OBJS) $(LIBOBJS) $(FILE_OSL_ALL)
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $(OBJS) $(LIBOBJS) $(FILE_OSL_ALL) $(LDLIBS) $(LOADLIBES)

gpsfishprof.exe: 
	$(CXX) -o $@ $(SRCS) $(LIBSRC) $(CXXFLAGS) $(OSL_HOME_FLAGS) $(PROFILE_GENERATE) $(LDFLAGS) $< $(LDLIBS) $(LOADLIBES)

gpsfishone.exe: 
	$(CXX) -o $@ $(SRCS) $(LIBSRC) $(CXXFLAGS) $(OSL_HOME_FLAGS) $(PROFILE_USE) $(LDFLAGS) $< $(LDLIBS) $(LOADLIBES)

update-gpsfishone:
	-rm gpsfishone gpsfishoneprof
	$(MAKE) gpsfishoneprof RELEASE=t
	$(MAKE) run-profile
	$(MAKE) gpsfishone RELEASE=t

run-profile: gpsfishoneprof
	./gpsfishoneprof bench 32 1 12 default depth

clean:
	-rm *.o $(PROGRAM)
	-rm -rf .deps

light-clean:
	-rm -rf .deps .objs .gch 

-include $(patsubst %.cpp,.deps/%.cpp.d,$(SRCS))

.PHONY: all clean light-clean run-profile update-gpsusione
 