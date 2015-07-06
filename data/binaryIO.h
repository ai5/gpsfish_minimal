/* binaryIO.h
 */
#ifndef OSL_BINARYIO_H
#define OSL_BINARYIO_H
#include <boost/scoped_ptr.hpp>
#include <vector>
#include <iosfwd>

namespace osl
{
  namespace misc
  {
    struct BinaryWriter
    {
      static void write(std::ostream&, const std::vector<int>& data);
      static void write(std::ostream&, const std::vector<double>& data);
    };
    template <class T>
    class BinaryReader
    {
    public:
      explicit BinaryReader(std::istream& is);
      ~BinaryReader();
      
      bool read(std::vector<T>& data);
      static size_t blockSize();
    private:
      struct State;
      boost::scoped_ptr<State> state;
    };

    template <class T>
    class BinaryElementReader
    {
    public:
      explicit BinaryElementReader(std::istream& is);
      ~BinaryElementReader();

      T read();
      bool hasNext() const;
      bool failed() const;
    private:
      struct State;
      boost::scoped_ptr<State> state;
    };
  }    
}

#endif /* OSL_BINARYIO_H */
// ;;; Local Variables:
// ;;; mode:c++
// ;;; c-basic-offset:2
// ;;; End:
