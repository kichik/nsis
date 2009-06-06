#include <cppunit/extensions/HelperMacros.h>
#include "../mmap.h"

#include <time.h>
#include <stdlib.h>
#include <stdio.h>

using namespace std; // for std::min

int g_display_errors = 1;
FILE *g_output = stderr;

void quit() {
  fprintf(g_output, "MMap quit\n");
}

class MMapTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( MMapTest );
  CPPUNIT_TEST( testMMapFile );
  CPPUNIT_TEST_SUITE_END();

public:
  void testMMapFile() {
    size_t i;
    const int BUF_SIZE = 50000; // 50MB

    // resize

    MMapFile mmap;
    mmap.resize(BUF_SIZE);
    CPPUNIT_ASSERT_EQUAL( BUF_SIZE, mmap.getsize() );

    // set content

    char *buf = (char *) mmap.get(0, BUF_SIZE);

    for (i = 0; i < BUF_SIZE; i++) {
      buf[i] = i % 256;
    }

    mmap.release();

    // test content and get(), getmore()

    srand(time(NULL));

    for (i = 0; i < 100; i++) {
      int offset1 = rand() % BUF_SIZE;
      int size1 = rand() % (BUF_SIZE - offset1);
      char *p1 = (char *) mmap.get(offset1, size1);

      int offset2 = rand() % BUF_SIZE;
      int size2 = rand() % (BUF_SIZE - offset2);
      char *p2 = (char *) mmap.getmore(offset2, size2);

      int j;

      for (j = 0; j < size1; j++) {
        CPPUNIT_ASSERT_EQUAL( p1[j], char((offset1 + j) % 256) );
      }

      for (j = 0; j < size2; j++) {
        CPPUNIT_ASSERT_EQUAL( p2[j], char((offset2 + j) % 256) );
      }

      mmap.release();
      mmap.release(p2, size2);
    }
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( MMapTest );
