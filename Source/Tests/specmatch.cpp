#include <cppunit/extensions/HelperMacros.h>
#include "../dirreader.h"
#include "../tstring.h"

#ifndef TEXT
#define TEXT _T
#endif

using namespace std;

class SpecTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( SpecTest );
  CPPUNIT_TEST( testMatches );
  CPPUNIT_TEST_SUITE_END();

public:
  void testMatches() {
    testMatch(TEXT("test.exe"), TEXT("test.exe"), true);
    testMatch(TEXT("test"), TEXT("test"), true);
    testMatch(TEXT("test.exe"), TEXT("test.*"), true);
    testMatch(TEXT("test"), TEXT("test.*"), true);
    testMatch(TEXT("test"), TEXT("????"), true);
    testMatch(TEXT("test"), TEXT("???"), false);
    testMatch(TEXT("test"), TEXT("*.exe"), false);
    testMatch(TEXT("test.exe.bat"), TEXT("*.exe"), false);
    testMatch(TEXT("test.exe.bat"), TEXT("*.bat"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("*t"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("*"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("*x*"), true);
    testMatch(TEXT("test.exe.exe"), TEXT("*.*"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("*.b*"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("tes?.*.bat"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("tes?.*bat"), true);
    testMatch(TEXT("test.exe.bat"), TEXT("tes?.*bat***."), true);
    testMatch(TEXT("test.exe"), TEXT("????.*"), true);
    testMatch(TEXT("testing.exe"), TEXT("????.*"), false);
  }

private:

  void testMatch(tstring name, tstring spec, bool result) {
    CPPUNIT_ASSERT_EQUAL( dir_reader::matches(name, spec), result );
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( SpecTest );
