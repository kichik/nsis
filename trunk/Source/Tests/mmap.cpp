#include <cppunit/extensions/HelperMacros.h>
#include "../strlist.h"

#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int MMapFile::m_iAllocationGranularity = 0;
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
    const int BUF_SIZE = 50000; // 50MB

    MMapFile mmap;
    mmap.resize(BUF_SIZE);
    CPPUNIT_ASSERT_EQUAL( BUF_SIZE, mmap.getsize() );

    void *buf = mmap.get(0, BUF_SIZE);
    memset(buf, 0x85, BUF_SIZE);
    mmap.release();

    srand(time(NULL));

    for (int i = 0; i < 100; i++) {
      int offset1 = rand() % BUF_SIZE;
      int size1 = rand() % (BUF_SIZE - offset1);
      char *p1 = (char *) mmap.get(offset1, size1);

      int offset2 = rand() % BUF_SIZE;
      int size2 = rand() % (BUF_SIZE - offset2);
      char *p2 = (char *) mmap.getmore(offset2, &size2);

      int minsize = min(size1, size2);
      for (int j = 0; j < minsize; j++) {
        CPPUNIT_ASSERT_EQUAL( p1[j], p2[j] );
      }

      mmap.release();
      mmap.release(p2, size2);
    }
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( MMapTest );