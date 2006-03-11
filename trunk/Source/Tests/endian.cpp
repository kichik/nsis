#include <cppunit/extensions/HelperMacros.h>
#include "../util.h"

class EndianTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( EndianTest );
  CPPUNIT_TEST( testSwapEndian );
  CPPUNIT_TEST( testFixEndian16 );
  CPPUNIT_TEST( testFixEndian32 );
  CPPUNIT_TEST_SUITE_END();

public:
  void testSwapEndian() {
    CPPUNIT_ASSERT_EQUAL( (int)0x78563412, (int)SWAP_ENDIAN_INT32(0x12345678) );
    CPPUNIT_ASSERT_EQUAL( (int)0xFFFFFFFF, (int)SWAP_ENDIAN_INT32(0xFFFFFFFF) );
    CPPUNIT_ASSERT_EQUAL( (int)0, (int)SWAP_ENDIAN_INT32(0) );
    CPPUNIT_ASSERT_EQUAL( (int)0x3412, (int)SWAP_ENDIAN_INT16(0x1234) );
    CPPUNIT_ASSERT_EQUAL( (int)0xFFFF, (int)SWAP_ENDIAN_INT16(0xFFFF) );
    CPPUNIT_ASSERT_EQUAL( (int)0, (int)SWAP_ENDIAN_INT16(0) );
  }

  void testFixEndian32() {
    int i=1;
    int actual = 0x12345678;
    FIX_ENDIAN_INT32_INPLACE(actual);
    int expected;
    if (((char*)&i)[0] == 1) {
      // little endian
      expected = 0x12345678;
    }
    else {
      // big endian
      expected = 0x78563412;
    }
    CPPUNIT_ASSERT_EQUAL(expected, actual);
  }

  void testFixEndian16() {
    int i=1;
    int actual = 0x1234;
    FIX_ENDIAN_INT16_INPLACE(actual);
    int expected;
    if (((char*)&i)[0] == 1) {
      // little endian
      expected = 0x1234;
    }
    else {
      // big endian
      expected = 0x3412;
    }
    CPPUNIT_ASSERT_EQUAL(expected, actual);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( EndianTest );