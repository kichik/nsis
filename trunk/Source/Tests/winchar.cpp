// Reviewed for Unicode support by Jim Park -- 08/13/2007
#include <cppunit/extensions/HelperMacros.h>
#include "../winchar.h"

#include <time.h>
#include <stdlib.h>

// macro for fixing endianity
#define _x(x) FIX_ENDIAN_INT16(WCHAR(x))

class WinCharTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( WinCharTest );
  CPPUNIT_TEST( testFromTchar );
  CPPUNIT_TEST( testStrCpy );
  CPPUNIT_TEST( testStrNCpy );
  CPPUNIT_TEST( testStrLen );
  CPPUNIT_TEST( testStrCmp );
  CPPUNIT_TEST( testStrDup );
  CPPUNIT_TEST_SUITE_END();

public:
  void testFromTchar() {
    WCHAR test[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };
    WCHAR *dyn = wcsdup_fromansi("test");

    CPPUNIT_ASSERT_EQUAL( 0, memcmp(test, dyn, 5) );

    free(dyn);
  }

  void testStrCpy() {
    WCHAR a[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };
    WCHAR b[5];

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) wcscpy(b, a) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a, b, 5) );
  }

  void testStrNCpy() {
    WCHAR a1[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };
    WCHAR b[5];

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) wcsncpy(b, a1, 5) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a1, b, 5 * sizeof(WCHAR)) );

    WCHAR a2[] = { _x('t'), _x('e'), 0, 0, 0 };

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) wcsncpy(b, a2, 5) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WCHAR)) );

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) wcsncpy(b, a1, 2) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WCHAR)) );
  }

  void testStrLen() {
    WCHAR test[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };

    CPPUNIT_ASSERT_EQUAL( (size_t) 4, wcslen(test) );
  }

  static int simplifyNumber(int n) {
    if (n < 0)
      return -1;
    if (n > 0)
      return 1;
    return 0;
  }

  void testStrCmp() {
    char a[] = "a";
    WCHAR wa[] = { _x('a'), 0 };
    char b[] = "b";
    WCHAR wb[] = { _x('b'), 0 };
    char empty[] = "";
    WCHAR wempty[] = { 0 };

    #define TEST_STR_CMP(x, y) \
      CPPUNIT_ASSERT_EQUAL(\
        simplifyNumber(strcmp(x, y)), \
        simplifyNumber(wcscmp(w##x, w##y)) \
      )

    TEST_STR_CMP(a, b);
    TEST_STR_CMP(b, a);
    TEST_STR_CMP(a, a);
    TEST_STR_CMP(b, b);
    TEST_STR_CMP(a, empty);
    TEST_STR_CMP(empty, b);
    TEST_STR_CMP(empty, empty);
  }

  void testStrDup() {
    WCHAR a[] = { _x('a'), _x('b'), _x('c'), 0 };

    WCHAR *b = _wcsdup(a);

    CPPUNIT_ASSERT_EQUAL( 0, wcscmp(a, b) );

    delete [] b;
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( WinCharTest );
