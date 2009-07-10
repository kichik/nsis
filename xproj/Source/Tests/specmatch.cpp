#include <cppunit/extensions/HelperMacros.h>
#include "../dirreader.h"

#include <string>

using namespace std;

class SpecTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( SpecTest );
  CPPUNIT_TEST( testMatches );
  CPPUNIT_TEST_SUITE_END();

public:
  void testMatches() {
    testMatch("test.exe", "test.exe", true);
    testMatch("test", "test", true);
    testMatch("test.exe", "test.*", true);
    testMatch("test", "test.*", true);
    testMatch("test", "????", true);
    testMatch("test", "???", false);
    testMatch("test", "*.exe", false);
    testMatch("test.exe.bat", "*.exe", false);
    testMatch("test.exe.bat", "*.bat", true);
    testMatch("test.exe.bat", "*t", true);
    testMatch("test.exe.bat", "*", true);
    testMatch("test.exe.bat", "*x*", true);
    testMatch("test.exe.exe", "*.*", true);
    testMatch("test.exe.bat", "*.b*", true);
    testMatch("test.exe.bat", "tes?.*.bat", true);
    testMatch("test.exe.bat", "tes?.*bat", true);
    testMatch("test.exe.bat", "tes?.*bat***.", true);
    testMatch("test.exe", "????.*", true);
    testMatch("testing.exe", "????.*", false);
  }

private:

  void testMatch(string name, string spec, bool result) {
    CPPUNIT_ASSERT_EQUAL( dir_reader::matches(name, spec), result );
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( SpecTest );
