#ifndef _UTIL_H_
#define _UTIL_H_

#ifndef _WIN32
#  include <iconv.h>
#endif
#include "ResourceEditor.h"

// these are the standard pause-before-quit shit.
extern int g_dopause;
extern void dopause(void);

// Adds the bitmap in filename using resource editor re as id id.
// If width or height are specified it will also make sure the bitmap is in that size
int update_bitmap(CResourceEditor* re, WORD id, char* filename, int width=0, int height=0, int maxbpp=0);

// reads icon file filename and places its icons in the resource wIconId using resource editor re. Also updates icondata_size.
int replace_icon(CResourceEditor* re, WORD wIconId, char* filename);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon (inside filename) that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(char* filename);
// Fill the array of icons for uninstall with their offsets
int generate_unicons_offsets(unsigned char* exeHeader, unsigned char* uninstIconData);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

// returns the number of WCHARs in str including null charcter
int WCStrLen(const WCHAR* szwStr);

size_t my_strftime(char *s, size_t max, const char  *fmt, const struct tm *tm);

#ifndef _WIN32
char *CharPrev(const char *s, const char *p);
char *CharNext(const char *s);
int wsprintf(char *s, const char *format, ...);
char *my_realpath(char *path);
void my_free_realpath(char *path, char *buffer);
// iconv const inconsistency workaround by Alexandre Oliva
template <typename T>
inline size_t __iconv_adaptor
  (size_t (*iconv_func)(iconv_t, T, size_t *, char**,size_t*),
  iconv_t cd, char **inbuf, size_t *inbytesleft,
  char **outbuf, size_t *outbytesleft)
{
  return iconv_func (cd, (T)inbuf, inbytesleft, outbuf, outbytesleft);
}
#endif

#endif //_UTIL_H_
