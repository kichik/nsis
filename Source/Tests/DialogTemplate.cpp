#include <cppunit/extensions/HelperMacros.h>
#include "../DialogTemplate.h"

#include <stdlib.h>

class CDialogTemplateTest : public CppUnit::TestFixture {

  CPPUNIT_TEST_SUITE( CDialogTemplateTest );
  CPPUNIT_TEST( testCorrectness );
  CPPUNIT_TEST_SUITE_END();

public:
  void testCorrectness() {
    unsigned char original_dialog[184] = {
      1, 0, 255, 255, 0, 0, 0, 0, 0, 0,
      0, 0, 72, 4, 0, 64, 3, 0, 0, 0, 0,
      0, 10, 1, 130, 0, 0, 0, 0, 0, 0, 0,
      8, 0, 0, 0, 0, 1, 77, 0, 83, 0, 32,
      0, 83, 0, 104, 0, 101, 0, 108, 0, 108, 0,
      32, 0, 68, 0, 108, 0, 103, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 80,
      0, 0, 0, 0, 22, 0, 20, 0, 7, 4, 0,
      0, 255, 255, 130, 0, 255, 255, 103, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 2, 80, 25, 0, 0, 0, 241, 0, 23, 0,
      238, 3, 0, 0, 255, 255, 130, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8,
      161, 80, 0, 0, 24, 0, 10, 1, 105, 0, 232,
      3, 0, 0, 82, 0, 105, 0, 99, 0, 104, 0,
      69, 0, 100, 0, 105, 0, 116, 0, 50, 0, 48,
      0, 65, 0, 0, 0, 0, 0, 0, 0
    };
    
    CDialogTemplate dt(original_dialog, 1252);

    DWORD dwSize;
    unsigned char *saved_dialog = dt.Save(dwSize);

    FILE *f = fopen("F:\\NSIS\\Source\\test2.dlg", "wb");
    fwrite(saved_dialog, dwSize, 1, f);
    fclose(f);

    CPPUNIT_ASSERT_EQUAL( (DWORD) sizeof(original_dialog), dwSize );
    CPPUNIT_ASSERT_EQUAL( 0, memcmp(saved_dialog, original_dialog, dwSize) );

    delete [] saved_dialog;
  }

};

CPPUNIT_TEST_SUITE_REGISTRATION( CDialogTemplateTest );
