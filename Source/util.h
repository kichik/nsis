#ifndef _UTIL_H_
#define _UTIL_H_

#ifndef _WIN32
#  include <iconv.h>
#  include <stdio.h>
#  include <glob.h>
#endif
#include "ResourceEditor.h"

#include <string>

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

#ifndef __BIG_ENDIAN__
# define FIX_ENDIAN_INT32_INPLACE(x) (x)
#else
# define FIX_ENDIAN_INT32_INPLACE(x) ((x) = SWAP_ENDIAN_INT32(x))
#endif
#define SWAP_ENDIAN_INT32(x) ( \
  (((x)&0xFF000000) >> 24) | \
  (((x)&0x00FF0000) >>  8) | \
  (((x)&0x0000FF00) <<  8) | \
  (((x)&0x000000FF) << 24) )

std::string get_full_path(const std::string &path);
std::string get_dir_name(const std::string& path);

#ifndef _WIN32
char *CharPrev(const char *s, const char *p);
char *CharNext(const char *s);
int wsprintf(char *s, const char *format, ...);
// iconv const inconsistency workaround by Alexandre Oliva
template <typename T>
inline size_t __iconv_adaptor
  (size_t (*iconv_func)(iconv_t, T, size_t *, char**,size_t*),
  iconv_t cd, char **inbuf, size_t *inbytesleft,
  char **outbuf, size_t *outbytesleft)
{
  return iconv_func (cd, (T)inbuf, inbytesleft, outbuf, outbytesleft);
}

char *my_convert(const char *path);
void my_convert_free(char *converted_path);
int my_open(const char *pathname, int flags);
FILE *my_fopen(const char *path, const char *mode);
int my_glob(const char *pattern, int flags,
            int errfunc(const char * epath, int eerrno), glob_t *pglob);

#define FOPEN(a, b) my_fopen(a, b)
#define GLOB(a, b, c, d) my_glob(a, b, c, d)
#define OPEN(a, b) my_open(a, b)

#else

#define FOPEN(a, b) fopen(a, b)
#define GLOB(a, b, c, d) glob(a, b, c, d)
#define OPEN(a, b) open(a, b)
#endif

// round a value up to be a multiple of 512
// assumption: T is an int type
template <class T>
inline T align_to_512(const T x) {
  return (x+511) & ~511;
}

// When a ResourceManager instance goes out of scope, it will run
// _FREE_RESOURCE on the resource.
// Example use:
// int fd = open(..);
// assert(fd != -1);
// ResourceManager<int, close> fdManager(fd);
template <typename _RESOURCE, typename _FREE_RESOURCE>
class ResourceManager {
public:
	ResourceManager(_RESOURCE& resource) : m_resource(resource) {}
	~ResourceManager() { m_free_resource(m_resource); };
private: // members
	_RESOURCE& m_resource;
  _FREE_RESOURCE m_free_resource;
private: // don't copy instances
	ResourceManager(const ResourceManager&);
	void operator=(const ResourceManager&);
};

#define DEFINE_FREEFUNC(freefunc) \
struct __free_with_##freefunc { \
  template <typename T> void operator()(T& x) { freefunc(x); } \
}

DEFINE_FREEFUNC(close);
DEFINE_FREEFUNC(CloseHandle);
DEFINE_FREEFUNC(fclose);
DEFINE_FREEFUNC(free);

// TODO: (orip)
// Can the ResourceManager be made generic? Specifically use the same class to:
// (a) close(fd) on POSIX
// (b) CloseHandle(handle) on WIN32

#endif //_UTIL_H_
