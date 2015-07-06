#include "binaryIO.h"
#include <fstream>
#include <iostream>
using namespace std;

int main(int ac,char **ag){
  const char *fileName="eval.bin";
  if(ac>1) fileName=ag[1];
  ifstream is(fileName,std::ios_base::binary);
  osl::misc::BinaryElementReader<int> reader(is);
  while(reader.hasNext()){
    cout << reader.read() << endl;
  }
}
