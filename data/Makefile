INCLUDES=-I/opt/local/include 
BOOST_POSTFIX=-mt
LOADLIBES += -L/opt/local/lib -lboost_filesystem$(BOOST_POSTFIX) -lboost_serialization$(BOOST_POSTFIX) -lboost_iostreams$(BOOST_POSTFIX) -lboost_system$(BOOST_POSTFIX) -lboost_date_time$(BOOST_POSTFIX) -lpthread

all : eval.txt

eval.txt : eval.bin bin2txt
	./bin2txt eval.bin > eval.txt

bin2txt : bin2txt.cc binaryIO.cc
	g++ $(INCLUDES) -o bin2txt bin2txt.cc binaryIO.cc $(LOADLIBES)
