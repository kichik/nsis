#ifndef ___NLF___H_____
#define ___NLF___H_____

#include <Windows.h>

#define NLF_VERSION 1
#define NLF_STRINGS 57

#define NLF_BRANDING 0
#define NLF_CAPTION 1
#define NLF_UPCATION 2
#define NLF_SUBCAPTION_LICENSE 3
#define NLF_SUBCAPTION_OPTIONS 4
#define NLF_SUBCAPTION_DIR 5
#define NLF_SUBCAPTION_INSTFILES 6
#define NLF_SUBCAPTION_COMPLETED 7
#define NLF_USUBCAPTION_CONFIRM 8
#define NLF_USUBCAPTION_INSTFILES 9
#define NLF_USUBCAPTION_COMPLETED 10
#define NLF_BTN_NEXT 11
#define NLF_BTN_BACK 12
#define NLF_BTN_AGREE 13
#define NLF_BTN_INSTALL 14
#define NLF_BTN_UNINSTALL 15
#define NLF_BTN_CANCEL 16
#define NLF_BTN_CLOSE 17
#define NLF_BTN_BROWSE 18
#define NLF_BTN_DETAILS 19
#define NLF_DEF_NAME 20
#define NLF_COMPLETED 21
#define NLF_COMP_CUSTOM 22
#define NLF_COMP_SUBTEXT1_NO_INST_TYPES 23
#define NLF_COMP_SUBTEXT1 24
#define NLF_COMP_SUBTEXT2 25
#define NLF_DIR_SUBTEXT 26
#define NLF_SPACE_AVAIL 27
#define NLF_SPACE_REQ 28
#define NLF_UNINST_SUBTEXT 29
#define NLF_FILE_ERROR 30
#define NLF_VERIFY 31
#define NLF_CANT_WRITE 32
#define NLF_COPY_FAILED 33
#define NLF_COPY_TO 34
#define NLF_SYMBOL_NOT_FOUND 35
#define NLF_COULD_NOT_LOAD 36
#define NLF_CREATE_DIR 37
#define NLF_CREATE_SHORTCUT 38
#define NLF_CREATED_UNINST 39
#define NLF_DEL_FILE 40
#define NLF_DEL_ON_REBOOT 41
#define NLF_ERR_CREATING_SHORTCUT 42
#define NLF_ERR_CREATING 43
#define NLF_ERR_DECOMPRESSING 44
#define NLF_ERR_REG_DLL 45
#define NLF_EXEC_SHELL 46
#define NLF_EXEC 47
#define NLF_EXTRACT 48
#define NLF_ERR_WRITING 49
#define NLF_INST_CORRUPTED 50
#define NLF_NO_OLE 51
#define NLF_OUTPUT_DIR 52
#define NLF_REMOVE_DIR 53
#define NLF_RENAME_ON_REBOOT 54
#define NLF_RENAME 55
#define NLF_SKIPPED 56

char SkipComments(FILE *f) {
  char c;
  while (c = fgetc(f))
    if (c == '#' || c == ';') {
      while (c = fgetc(f))
       if (c == '\n') break;
      }
    else break;
  return c;
}

extern FILE *g_output;
#define s(x) fprintf(g_output, x"\n"); fflush(g_output);
#define s2(x,y) fprintf(g_output, x"\n", y); fflush(g_output);

// NSIS Language File parser
class NLF {
  public:
    NLF(char *filename) {
      FILE *f = fopen(filename, "r");
      if (!f) throw runtime_error("Can't open language file!");

      char buf[1024];
      buf[0] = SkipComments(f);
      fgets(buf+1, 1024, f);

      // Check header
      if (strncmp(buf, "NLF v", 5)) throw runtime_error("Invalid language file!");
      if (atoi(buf+5) != NLF_VERSION) throw runtime_error("Language file version doesn't match NSIS version!");

      // Get language ID
      buf[0] = SkipComments(f);
      fgets(buf+1, 1024, f);
      m_wLangId = atoi(buf);

      // Read entries
      int i = 0;
      while (i < NLF_STRINGS) {
        buf[0] = SkipComments(f);
        fgets(buf+1, 1024, f);
        if (lstrlen(buf) == 1023) {
          wsprintf(buf, "String too long (string #%d)!", i);
          throw runtime_error(buf);
        }
        //m_szStrings[i] = new char[lstrlen(buf)];
        m_szStrings[i] = (char*)GlobalAlloc(GPTR, lstrlen(buf));
        lstrcpy(m_szStrings[i], buf);
        i++;
      }

      for (i = 0; i < NLF_STRINGS; i++) {
        GlobalFree(m_szStrings[i]);
      }

      fclose(f);
    }

  private:
    WORD m_wLangId;
    char *m_szStrings[NLF_STRINGS];
};

#endif