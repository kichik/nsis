#include <cppunit/extensions/HelperMacros.h>
#include "../Platform.h"
#include "../tchar.h"
#include "../Util.h"

class PlatformTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( PlatformTest );
  CPPUNIT_TEST( testCore );
  CPPUNIT_TEST( testCoreString );
  CPPUNIT_TEST( testCoreMath );
  CPPUNIT_TEST_SUITE_END();

public:
  void testCore() {
    CPPUNIT_ASSERT_EQUAL(sizeof(WINWCHAR), 2);
    CPPUNIT_ASSERT(sizeof(wchar_t) >= sizeof(WINWCHAR));
  }

  void testCoreString() {
    TCHAR tbuf[42];

    CPPUNIT_ASSERT(3 == my_strncpy(tbuf, _T("abc"), 4) && tbuf[2] == _T('c') && tbuf[3] == _T('\0'));
    CPPUNIT_ASSERT(2 == my_strncpy(tbuf, _T("abc"), 3) && tbuf[1] == _T('b') && tbuf[2] == _T('\0'));
  }

  void testCoreMath() {

    unsigned int ut;
    CPPUNIT_ASSERT(ui_add(ut, 0, 0) != false);
    CPPUNIT_ASSERT(ui_add(ut, UINT_MAX, 0) != false && ut == UINT_MAX);
    CPPUNIT_ASSERT(ui_add(ut, UINT_MAX, 1) == false);

    int st;
    CPPUNIT_ASSERT(si_add(st, 0, 0) != false);
    CPPUNIT_ASSERT(si_add(st, INT_MAX,  0) != false && st == INT_MAX);
    CPPUNIT_ASSERT(si_add(st, INT_MAX,  1) == false);
    CPPUNIT_ASSERT(si_add(st, INT_MAX, -1) != false);
    CPPUNIT_ASSERT(si_add(st, INT_MIN,  0) != false);
    CPPUNIT_ASSERT(si_add(st, INT_MIN,  1) != false);
    CPPUNIT_ASSERT(si_add(st, INT_MIN, -1) == false);
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( PlatformTest );
