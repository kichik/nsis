#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "nlf.h"

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

// NSIS Language File parser
NLF::NLF(char *filename) {
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

  // Read strings
  for (int i = 0; i < NLF_STRINGS; i++) {
    buf[0] = SkipComments(f);
    fgets(buf+1, 1024, f);
    if (lstrlen(buf) == 1023) {
      wsprintf(buf, "String too long (string #%d)!", i);
      throw runtime_error(buf);
    }
    StrTrim(buf, "\r\n\0");
    m_szStrings[i] = new char[lstrlen(buf)+1];
    lstrcpy(m_szStrings[i], buf);
  }

  fclose(f);
}

NLF::~NLF() {
  MessageBox(0, "destructor", "info", MB_OK);
  for (int i = 0; i < NLF_STRINGS; i++) {
    delete [] m_szStrings[i];
  }
}

WORD NLF::GetLang() {
  return m_wLangId;
}

char* NLF::GetString(int idx) {
  if (idx < 0 || idx >= NLF_STRINGS) return 0;
  return m_szStrings[idx];
}