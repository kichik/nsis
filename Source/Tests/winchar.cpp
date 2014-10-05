// Reviewed for Unicode support by Jim Park -- 08/13/2007
#include <cppunit/extensions/HelperMacros.h>
#include "../util.h"
#include "../winchar.h"

#include <time.h>
#include <stdlib.h>

// macro for fixing endianity
#define _x(x) FIX_ENDIAN_INT16(WCHAR(x))

// BUGBUG: These tests currently run as Ansi, it would be better if it respected defenv['UNICODE']

// BUGBUG: WinWStrDupFromWC is unable to test WCToUTF16LEHlpr because it is behind #ifdef MAKENSIS

// TODO write equal() for WINWCHAR -- http://subcommanderblog.wordpress.com/2009/01/10/cppunit_assert_equal-and-custom-data-types/

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
  void setUp() {
    NSISRT_Initialize();
  }

  void testFromTchar() {
    WINWCHAR test[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };

    WINWCHAR *dyn = WinWStrDupFromTChar(_T("test"));
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(test, dyn, 5) );
    free(dyn);

    dyn = WinWStrDupFromChar("test");
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(test, dyn, 5) );
    free(dyn);

    dyn = WinWStrDupFromWC(L"test");
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(test, dyn, 5) );
    free(dyn);
  }

  void testStrCpy() {
    WINWCHAR a[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };
    WINWCHAR b[5];

    CPPUNIT_ASSERT( !WinWStrCmp(b, WinWStrCpy(b, a)) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a, b, 5 * sizeof(WINWCHAR)) );
  }

  void testStrNCpy() {
    WINWCHAR a1[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };
    WINWCHAR b[5];

    CPPUNIT_ASSERT( !WinWStrCmp(b, WinWStrNCpy(b, a1, 5)) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a1, b, 5 * sizeof(WINWCHAR)) );

    WINWCHAR a2[] = { _x('t'), _x('e'), 0, 0, 0 };

    CPPUNIT_ASSERT( !WinWStrCmp(b, WinWStrNCpy(b, a2, 5)) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WINWCHAR)) );

    CPPUNIT_ASSERT( !WinWStrCmp(b, WinWStrNCpy(b, a1, 2)) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WINWCHAR)) );
  }

  void testStrLen() {
    WINWCHAR test[] = { _x('t'), _x('e'), _x('s'), _x('t'), 0 };

    CPPUNIT_ASSERT_EQUAL( (size_t) 4, WinWStrLen(test) );
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
    WINWCHAR wa[] = { _x('a'), 0 };
    char b[] = "b";
    WINWCHAR wb[] = { _x('b'), 0 };
    char empty[] = "";
    WINWCHAR wempty[] = { 0 };

    #define TEST_STR_CMP(x, y) \
      CPPUNIT_ASSERT_EQUAL(\
        simplifyNumber(strcmp(x, y)), \
        simplifyNumber(WinWStrCmp(w##x, w##y)) \
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
    WINWCHAR a[] = { _x('a'), _x('b'), _x('c'), 0 };

    WINWCHAR *b = WinWStrDupFromWinWStr(a);

    CPPUNIT_ASSERT_EQUAL( 0, WinWStrCmp(a, b) );

    free(b);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( WinCharTest );
