/*
 * version.c: version string
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#ifndef VERSION
#define VER "anonymous build (" __DATE__ " " __TIME__ ")"
#else
#define VER "version " VERSION
#endif

#define VERSTRFMT "v1.0 (NSIS Custom Build, %s)"
#define VERSTRSCMREVMAX 20

static char versionbuf[sizeof(VERSTRFMT)-2+VERSTRSCMREVMAX];
const char *const version = versionbuf;

void initversionstring(void)
{
  char scmverbuf[VERSTRSCMREVMAX+1];
  int cchsvnrev = 0;
  /* SCM trigger 20210912 */
  const char*svnproprev = "$Revision$";
  if ('$' == *svnproprev++)
  {
    const char*p;
    while('$' != *svnproprev && !isdigit(*svnproprev)) svnproprev++;
    for (p = svnproprev; isdigit(*p); ++p) cchsvnrev++;
  }
  if (!cchsvnrev)
  {
    cchsvnrev = 1;
    svnproprev = "?";
  }

  strcpy(scmverbuf, "SVN:r");
  strncat(scmverbuf, svnproprev, cchsvnrev);
  sprintf(versionbuf,VERSTRFMT,scmverbuf);
}

