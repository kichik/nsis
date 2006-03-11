#ifndef _UTIL_H_
#define _UTIL_H_

#include <string> // for std::string

#include "boost/scoped_ptr.hpp" // for boost::scoped_ptr
#include "ResourceEditor.h"

#ifndef _WIN32
#  include <iconv.h>
#  include <stdio.h>
#  include <glob.h>
#endif


// these are the standard pause-before-quit shit.
extern int g_dopause;
extern void dopause(void);

// Adds the bitmap in filename using resource editor re as id id.
// If width or height are specified it will also make sure the bitmap is in that size
int update_bitmap(CResourceEditor* re, WORD id, const char* filename, int width=0, int height=0, int maxbpp=0);

// reads icon file filename and places its icons in the resource wIconId using resource editor re
void replace_icon(CResourceEditor* re, WORD wIconId, const char* filename);

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon (inside filename) that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(const char* filename, size_t &size);
// Fill the array of icons for uninstall with their offsets
int generate_unicons_offsets(unsigned char* exeHeader, unsigned char* uninstIconData);
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT

// returns the number of WCHARs in str including null charcter
size_t WCStrLen(const WCHAR* szwStr);

size_t my_strftime(char *s, size_t max, const char  *fmt, const struct tm *tm);

std::string get_full_path(const std::string& path);
std::string get_dir_name(const std::string& path);
std::string get_file_name(const std::string& path);
std::string get_executable_dir(const char *argv0);
std::string remove_file_extension(const std::string& path);
std::string lowercase(const std::string&);

std::string get_string_prefix(const std::string& str, const std::string& separator);
std::string get_string_suffix(const std::string& str, const std::string& separator);

#ifndef _WIN32
char *CharPrev(const char *s, const char *p);
char *CharNext(const char *s);
int wsprintf(char *s, const char *format, ...);
int WideCharToMultiByte(UINT CodePage, DWORD dwFlags, LPCWSTR lpWideCharStr,
    int cchWideChar, LPSTR lpMultiByteStr, int cbMultiByte, LPCSTR lpDefaultChar,
    LPBOOL lpUsedDefaultChar);
int MultiByteToWideChar(UINT CodePage, DWORD dwFlags, LPCSTR lpMultiByteStr,
    int cbMultiByte, LPWSTR lpWideCharStr, int cchWideChar);

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

// ================
// ResourceManagers
// ================

// When a ResourceManager instance goes out of scope, it will run
// _FREE_RESOURCE on the resource.
// Example use:
// int fd = open(..);
// assert(fd != -1);
// MANAGE_WITH(fd, close);

class BaseResourceManager {
protected:
	BaseResourceManager() {}
public:
	virtual ~BaseResourceManager() {}
};

template <typename _RESOURCE, typename _FREE_RESOURCE>
class ResourceManager : public BaseResourceManager {
public:
  ResourceManager(_RESOURCE& resource) : m_resource(resource) {}
  virtual ~ResourceManager() { m_free_resource(m_resource); };
private: // members
  _RESOURCE& m_resource;
  _FREE_RESOURCE m_free_resource;
private: // don't copy instances
  ResourceManager(const ResourceManager&);
  void operator=(const ResourceManager&);
};

#define RM_MANGLE_FREEFUNC(freefunc) \
	__free_with_##freefunc

#define RM_DEFINE_FREEFUNC(freefunc) \
struct RM_MANGLE_FREEFUNC(freefunc) { \
  template <typename T> void operator()(T& x) { freefunc(x); } \
}

typedef boost::scoped_ptr<BaseResourceManager> ResourceManagerPtr;

template<typename _FREE_RESOURCE, typename _RESOURCE>
void createResourceManager(_RESOURCE& resource, ResourceManagerPtr& ptr) {
	ptr.reset(new ResourceManager<_RESOURCE, _FREE_RESOURCE>(resource));
}

#define RM_MANGLE_RESOURCE(resource) resource##_autoManager
#define MANAGE_WITH(resource, freefunc) \
	ResourceManagerPtr RM_MANGLE_RESOURCE(resource); \
		createResourceManager<RM_MANGLE_FREEFUNC(freefunc)>( \
      resource, RM_MANGLE_RESOURCE(resource))

// Add more resource-freeing functions here when you need them
RM_DEFINE_FREEFUNC(close);
RM_DEFINE_FREEFUNC(CloseHandle);
RM_DEFINE_FREEFUNC(fclose);
RM_DEFINE_FREEFUNC(free);

#endif //_UTIL_H_
