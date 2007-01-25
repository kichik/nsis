#include <cppunit/extensions/HelperMacros.h>
#include "../winchar.h"

#include <time.h>
#include <stdlib.h>

class WinCharTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( WinCharTest );
  CPPUNIT_TEST( testFromAnsi );
  CPPUNIT_TEST( testToAnsi );
  CPPUNIT_TEST( testStrCpy );
  CPPUNIT_TEST( testStrNCpy );
  CPPUNIT_TEST( testStrLen );
  CPPUNIT_TEST( testStrCmp );
  CPPUNIT_TEST( testStoi );
  CPPUNIT_TEST_SUITE_END();

public:
  void testFromAnsi() {
    WCHAR test[] = { 't', 'e', 's', 't', 0 };
    WCHAR *dyn = winchar_fromansi("test");

    CPPUNIT_ASSERT_EQUAL( 0, memcmp(test, dyn, 5) );

    delete [] dyn;
  }

  void testToAnsi() {
    WCHAR test[] = { 't', 'e', 's', 't', 0 };
    char *dyn = winchar_toansi(test);

    CPPUNIT_ASSERT_EQUAL( 0, strcmp("test", dyn) );

    delete [] dyn;
  }

  void testStrCpy() {
    WCHAR a[] = { 't', 'e', 's', 't', 0 };
    WCHAR b[5];

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) winchar_strcpy(b, a) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a, b, 5) );
  }

  void testStrNCpy() {
    WCHAR a1[] = { 't', 'e', 's', 't', 0 };
    WCHAR b[5];

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) winchar_strncpy(b, a1, 5) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a1, b, 5 * sizeof(WCHAR)) );

    WCHAR a2[] = { 't', 'e', 0, 0, 0 };

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) winchar_strncpy(b, a2, 5) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WCHAR)) );

    CPPUNIT_ASSERT_EQUAL( (WCHAR*) b, (WCHAR*) winchar_strncpy(b, a1, 2) );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(a2, b, 5 * sizeof(WCHAR)) );
  }

  void testStrLen() {
    WCHAR test[] = { 't', 'e', 's', 't', 0 };

    CPPUNIT_ASSERT_EQUAL( (size_t) 4, winchar_strlen(test) );
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
    WCHAR wa[] = { 'a', 0 };
    char b[] = "b";
    WCHAR wb[] = { 'b', 0 };
    char empty[] = "";
    WCHAR wempty[] = { 0 };

    #define TEST_STR_CMP(x, y) \
      CPPUNIT_ASSERT_EQUAL(\
        simplifyNumber(strcmp(x, y)), \
        simplifyNumber(winchar_strcmp(w##x, w##y)) \
      )

    TEST_STR_CMP(a, b);
    TEST_STR_CMP(b, a);
    TEST_STR_CMP(a, a);
    TEST_STR_CMP(b, b);
    TEST_STR_CMP(a, empty);
    TEST_STR_CMP(empty, b);
    TEST_STR_CMP(empty, empty);
  }

  void testStoi() {
    srand(time(0));

    for (int i = 0; i < 1000; i++)
    {
      int r = rand();
      char s[128];
      sprintf(s, "%d", r);
      WCHAR *ws = winchar_fromansi(s);
      CPPUNIT_ASSERT_EQUAL( r, winchar_stoi(ws) );
      delete [] ws;
    }
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( WinCharTest );
