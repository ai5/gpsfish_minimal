/* binaryIO.cc
 */
#include "binaryIO.h"
#include <boost/serialization/serialization.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/filter/bzip2.hpp>
#include <iostream>

namespace osl
{
  namespace
  {
    const size_t split_limit = 8*1024;
    template <class T>
    void write_vector(std::ostream& os, const std::vector<T>& data)
    {
      boost::iostreams::filtering_streambuf<boost::iostreams::output> filter;
      filter.push(boost::iostreams::bzip2_compressor());
      filter.push(os);
      std::ostream out(&filter);
    
      boost::archive::text_oarchive oa(out);
      if (data.size() <= split_limit) {
	oa << data;
      }
      else {
	for (size_t p=0; p<data.size(); p+=split_limit) {
	  std::vector<T> tmp(data.begin()+p,
			     data.begin()+std::min(p+split_limit,
						   data.size()));
	  oa << tmp;
	}
      }
    }
  }
}

void osl::misc::BinaryWriter::
write(std::ostream& os, const std::vector<int>& data)
{
  write_vector(os, data);
}
void osl::misc::BinaryWriter::
write(std::ostream& os, const std::vector<double>& data)
{
  write_vector(os, data);
}


template <class T>
osl::misc::BinaryReader<T>::BinaryReader(std::istream& is)
  : state(new State(is))
{
}
template <class T>
osl::misc::BinaryReader<T>::~BinaryReader()
{
}

template <class T>
struct osl::misc::BinaryReader<T>::State
{
  boost::iostreams::filtering_streambuf<boost::iostreams::input> filter;
  boost::scoped_ptr<std::istream> in;
  boost::scoped_ptr<boost::archive::text_iarchive> ia;
  explicit State(std::istream &is)
  {
    if (!is)
      return;
    filter.push(boost::iostreams::bzip2_decompressor());
    filter.push(is);
    in.reset(new std::istream(&filter));
    ia.reset(new boost::archive::text_iarchive(*in));
  }
  bool read_vector(std::vector<T>& data)
  {
    if (! in || ! *in)
      return false;
    return (*ia) >> data, (bool)*in;
  }
};

template <class T>
bool osl::misc::BinaryReader<T>::
read(std::vector<T>& data)
{
  return state->read_vector(data);
}

template <class T>
size_t osl::misc::BinaryReader<T>::blockSize()
{
  return split_limit;
}


template <class T>
struct osl::misc::BinaryElementReader<T>::State
{
  BinaryReader<T> reader;
  std::vector<T> data;
  size_t cur;
  bool failed;
  explicit State(std::istream& is) : reader(is), cur(0), failed(!is)
  {
  }
  bool hasNext() 
  {
    tryRead();
    return cur < data.size();
  }
  T read()
  {
    if (! hasNext())
      throw std::logic_error("no data in BinaryReader::read");
    return data[cur++];
  }
  void tryRead()
  {
    if (cur < data.size())
      return;
    data.clear();
    cur = 0;
    try {
      failed = ! reader.read(data);
    } catch (boost::archive::archive_exception& e) {
      cur = data.size();
      failed = true;
    }
  }
};
  
template <class T>
osl::misc::BinaryElementReader<T>::BinaryElementReader(std::istream& is)
  : state(new State(is))
{
}
template <class T>
osl::misc::BinaryElementReader<T>::~BinaryElementReader()
{
}
template <class T>
bool osl::misc::BinaryElementReader<T>::
hasNext() const
{
  return state->hasNext();
}
template <class T>
bool osl::misc::BinaryElementReader<T>::
failed() const
{
  return state->failed;
}
template <class T>
T osl::misc::BinaryElementReader<T>::read()
{
  return state->read();
}


template class osl::misc::BinaryReader<int>;
template class osl::misc::BinaryReader<double>;
template class osl::misc::BinaryElementReader<int>;
template class osl::misc::BinaryElementReader<double>;
#ifdef MAIN
int main(){
  BinaryReader<int> reader(std::cin);
  return 0;
}
#endif

// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
