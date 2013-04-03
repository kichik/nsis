/*
 * util.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2009 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

/* Unicode support by Jim Park -- 07/23/2007 */

#include "Platform.h"
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>
#include "tchar.h"
#include "exehead/fileform.h"
#include "util.h"
#include "strlist.h"
#include "winchar.h"

#ifndef _WIN32
#  include <ctype.h>
#  include <unistd.h> // for close(2)
#  include <fcntl.h> // for open(2)
#  include <iconv.h>
#  include <locale.h>
#endif

#ifdef __APPLE__
namespace Apple { // defines struct section
#  include <mach-o/dyld.h> // for _NSGetExecutablePath
};
#  define FALSE 0 // bugs #1851365, #2497290
#  define TRUE 1
#  include <sys/param.h> // for MAXPATHLEN
#endif

#include <cassert> // for assert
#include <algorithm>
#include <stdexcept>

using namespace std;

int g_dopause=0;
extern int g_display_errors;
extern FILE *g_output;

void dopause(void)
{
  if (g_dopause)
  {
    if (g_display_errors) _ftprintf(g_output,_T("MakeNSIS done - hit enter to close..."));
    fflush(stdout);
    int a;
    while ((a=_gettchar()) != _T('\r') && a != _T('\n') && a != 27/*esc*/);
  }
}

double my_wtof(const wchar_t *str) 
{
  char buf[100];
  WideCharToMultiByte(0,0,str,-1,buf,100,0,0);
  return atof(buf);
}

unsigned int my_strncpy(TCHAR*Dest, const TCHAR*Src, unsigned int cchMax)
{
  // Dest and Src must be valid, Dest is always \0 terminated.
  // Returns number of TCHARs copied to Dest; min(strlen(Src),cchMax-1).
  unsigned int cch = 0;
  if (cchMax)
  {
    for(;--cchMax;)
    {
      TCHAR ch = Src[cch];
      if (!ch) break;
      Dest[cch++] = ch;
    }
    Dest[cch] = _T('\0');
  }
  return cch;
}

// Returns 0 if everything is OK
// Returns -1 if can't find the file
// Returns -2 if the file is an invalid bitmap
// Returns -3 if the size doesn't match
// Returns -4 if the bpp doesn't match
int update_bitmap(CResourceEditor* re, WORD id, const TCHAR* filename, int width/*=0*/, int height/*=0*/, int maxbpp/*=0*/) {
  FILE *f = FOPEN(filename, ("rb"));
  if (!f) return -1;

  if (fgetc(f) != 'B' || fgetc(f) != 'M') {
    fclose(f);
    return -2;
  }

  if (width != 0) {
    INT32 biWidth;
    fseek(f, 18, SEEK_SET); // Seek to the width member of the header
    fread(&biWidth, sizeof(INT32), 1, f);
    FIX_ENDIAN_INT32_INPLACE(biWidth);
    if (width != biWidth) {
      fclose(f);
      return -3;
    }
  }

  if (height != 0) {
    INT32 biHeight;
    fseek(f, 22, SEEK_SET); // Seek to the height member of the header
    fread(&biHeight, sizeof(INT32), 1, f);
    FIX_ENDIAN_INT32_INPLACE(biHeight);
    // Bitmap height can be negative too...
    if (height != abs(biHeight)) {
      fclose(f);
      return -3;
    }
  }

  if (maxbpp != 0) {
    WORD biBitCount;
    fseek(f, 28, SEEK_SET); // Seek to the height member of the header
    fread(&biBitCount, sizeof(WORD), 1, f);
    FIX_ENDIAN_INT16_INPLACE(biBitCount);
    if (biBitCount > maxbpp) {
      fclose(f);
      return -4;
    }
  }

  DWORD dwSize;
  fseek(f, 2, SEEK_SET);
  fread(&dwSize, sizeof(DWORD), 1, f);
  FIX_ENDIAN_INT32_INPLACE(dwSize);
  dwSize -= 14;

  unsigned char* bitmap = (unsigned char*)malloc(dwSize);
  if (!bitmap) {
    fclose(f);
    throw bad_alloc();
  }

  bool gotbmdata = !fseek(f, 14, SEEK_SET) && dwSize == fread(bitmap, 1, dwSize, f);
  int retval = gotbmdata ? 0 : -2;
  fclose(f);

  if (gotbmdata)
    re->UpdateResource(RT_BITMAP, id, NSIS_DEFAULT_LANG, bitmap, dwSize);

  free(bitmap);
  return retval;
}

#ifndef _WIN32
TCHAR *CharPrev(const TCHAR *s, const TCHAR *p) {
  if (!s || !p || p < s)
    return NULL;
  while (*s) {
    TCHAR *n = CharNext(s);
    if (n >= p)
      break;
    s = n;
  }
  return (TCHAR *) s;
}

char *CharNextA(const char *s) {
  int l = 0;
  if (s && *s)
    l = max(1, mblen(s, MB_CUR_MAX));
  return (char *) s + l;
}

WCHAR *CharNextW(const WCHAR *s) {
  // BUGBUG: Is this the best we can do?
  return s + 1;
}

char *CharNextExA(WORD codepage, const char *s, int flags) {
  char buf[30];
  snprintf(buf, 30, "CP%d", codepage);
  const char* orglocct = setlocale(LC_CTYPE, buf);

  const char* np;
  int len = mblen(s, strlen(s));
  if (len > 0)
    np = s + len;
  else
    np = s + 1;

  setlocale(LC_CTYPE, orglocct);

  return (char *) np;
}

int wsprintf(TCHAR *s, const TCHAR *format, ...) {
  va_list val;
  va_start(val, format);
  int res = _vsntprintf(s, 1024, format, val);
  va_end(val);
  return res;
}

bool nsis_iconv_reallociconv(iconv_t CD, char**In, size_t*cbInLeft, char**Mem, size_t&cbConverted)
{
  char *in, *heap = *Mem;
  UINT cbMem = 512;
  size_t inleft, outleft, icvret = (size_t) -1;
  for(;;)
  {
    in = *In, inleft = *cbInLeft, outleft = cbMem - sizeof(UINT32); // Leave room for \0
    char *p = (char*) realloc(heap, cbMem), *out = p;
    if (!p) break;
    heap = p, icvret = nsis_iconv_adaptor(iconv, CD, &in, &inleft, &out, &outleft);
    if ((size_t) -1 != icvret || E2BIG != errno) break;
    cbMem *= 4;
  }
  *In = in, *Mem = heap;
  cbConverted = cbMem - (outleft + sizeof(UINT32)), *cbInLeft = inleft;
  if ((size_t) -1 != icvret)
  {
    *((UINT32*)(&heap[cbConverted])) = 0;
    return true;
  }
  return false;
}

void static create_code_page_string(TCHAR *buf, size_t len, UINT code_page) {
  switch(code_page)
  {
  case CP_ACP:
  case 1: // OEMCP
    code_page = 1252;
    break;
  case CP_UTF8:
    _sntprintf(buf, len, _T("UTF-8"));
    return;
  case 1200: // UTF16LE
  case 1201: // UTF16BE
    _sntprintf(buf, len, _T("UTF-16%cE"), 1200==code_page ? _T('L') : _T('B'));
    return;
  }
  _sntprintf(buf, len, _T("CP%d"), code_page);
}

int WideCharToMultiByte(UINT CodePage, DWORD dwFlags, LPCWSTR lpWideCharStr,
    int cchWideChar, LPSTR lpMultiByteStr, int cbMultiByte, LPCSTR lpDefaultChar,
    LPBOOL lpUsedDefaultChar) {
  static char buffer[4096]; // BUGBUG: Should this be 4*NSIS_MAX_STRLEN for large string build?

  char cp[128];
  create_code_page_string(cp, sizeof(cp), CodePage);

  iconv_t cd = iconv_open(cp, "wchar_t");
  if (cd == (iconv_t) -1) {
    return 0;
  }

  if (cchWideChar < 0) {
    cchWideChar = (int) _wcslen(lpWideCharStr) + 1;
  }

  if (cbMultiByte == 0) {
    cbMultiByte = sizeof(buffer);
    lpMultiByteStr = buffer;
  }

  char *in = (char *) lpWideCharStr;
  char *out = lpMultiByteStr;
  size_t inbytes = cchWideChar * sizeof(WCHAR);
  size_t outbytes = cbMultiByte;

  if (nsis_iconv_adaptor(iconv, cd, &in, &inbytes, &out, &outbytes) == (size_t) -1) {
    iconv_close(cd);
    return 0;
  }

  iconv_close(cd);

  return cbMultiByte - outbytes;
}

int MultiByteToWideChar(UINT CodePage, DWORD dwFlags, LPCSTR lpMultiByteStr,
    int cbMultiByte, LPWSTR lpWideCharStr, int cchWideChar) {
  static WCHAR buffer[4096]; // BUGBUG: Should this be 4*NSIS_MAX_STRLEN for large string build?

  char cp[128];
  create_code_page_string(cp, sizeof(cp), CodePage);

  iconv_t cd = iconv_open("wchar_t", cp);
  if (cd == (iconv_t) -1) {
    return 0;
  }

  if (cbMultiByte < 0) {
    cbMultiByte = strlen(lpMultiByteStr) + 1;
  }

  if (cchWideChar == 0) {
    cchWideChar = sizeof(buffer);
    lpWideCharStr = buffer;
  }

  char *in = (char *) lpMultiByteStr;
  char *out = (char *) lpWideCharStr;
  size_t inbytes = cbMultiByte;
  size_t outbytes = cchWideChar * sizeof(WCHAR);

  if (nsis_iconv_adaptor(iconv, cd, &in, &inbytes, &out, &outbytes) == (size_t) -1) {
    iconv_close(cd);
    return 0;
  }

  iconv_close(cd);

  return cchWideChar - (outbytes / sizeof (WCHAR));
}

BOOL IsValidCodePage(UINT CodePage)
{
  TCHAR cp[128];
  create_code_page_string(cp, sizeof(cp), CodePage);

  iconv_t cd = iconv_open(_T("wchar_t"), cp);
  if (cd == (iconv_t) -1)
    return FALSE;

  iconv_close(cd);

  return TRUE;
}

#ifdef _UNICODE
void PathConvertWinToPosix(char*p)
{
  if ('\"' == *p) ++p; // Skip opening quote if any (For !system)
  size_t len = strlen(p);

  /* Replace drive letter X: by /X */
  if (len >= 2 && ':' == p[1])
  {
    p[1] = (char) tolower((int) p[0]);
    p[0] = '/';
  }

  do
  {
    if ('\\' == *p) *p = '/';
    p = CharNextA(p);
  }
  while (*p);
}
#endif
void PathConvertWinToPosix(TCHAR*p)
{
  if (_T('\"') == *p) ++p; // Skip opening quote if any (For !system)
  size_t len = _tcsclen(p);

  /* Replace drive letter X: by /X */
  if (len >= 2 && _T(':') == p[1])
  {
    p[1] = (TCHAR) tolower((int) p[0]);
    p[0] = _T('/');
  }

  do
  {
    if (_T('\\') == *p) *p = _T('/');
    p = CharNext(p);
  }
  while (*p);
}

#define MY_ERROR_MSG(x) {if (g_display_errors) {PrintColorFmtMsg_ERR(_T("%s"), x);}}

TCHAR *my_convert(const TCHAR *path)
{
  TCHAR *converted_path = _tcsdup(path);
  if(!converted_path)
  {
    MY_ERROR_MSG(_T("Error: could not allocate memory in my_convert()\n"));
    return 0;
  }
  PathConvertWinToPosix(converted_path);
  return converted_path;
}

void my_convert_free(TCHAR *converted_path)
{
  free(converted_path);
}

int my_open(const TCHAR *pathname, int flags)
{
  TCHAR *converted_pathname = my_convert(pathname);

  int result = open(converted_pathname, flags);
  my_convert_free(converted_pathname);
  return result;
}
#endif//!_WIN32

FILE* my_fopen(const TCHAR *path, const char *mode)
{
  FILE*f = 0;
#ifndef _UNICODE
  f = fopen(path, mode);
#else
#ifdef _WIN32
  TCHAR tmode[20];
  for (int i=0; ; ++i) if (0 == (tmode[i] = mode[i])) break;
  f = _wfopen(path, tmode);
#else
  const char* orglocct = setlocale(LC_CTYPE, "");
  const wchar_t* srcW = path;
  size_t cb = wcsrtombs(0,&srcW,0,0);
  if (-1 != cb)
  {
    char* nativepath = (char*) malloc(++cb);
    if (nativepath)
    {
      cb = wcsrtombs(nativepath,&path,cb,0);
      if (-1 != cb)
      {
        PathConvertWinToPosix(nativepath);
        f = fopen(nativepath, mode);
      }
      free(nativepath);
    }
  }
  setlocale(LC_CTYPE, orglocct);
#endif
#endif
  return f;
}


void *operator new(size_t size) throw(bad_alloc) {
  void *p = malloc(size);
  if (!p)
    throw bad_alloc();
  return p;
}

void operator delete(void *p) throw() {
  if (p) free(p);
}

void operator delete [](void *p) throw() {
  if (p) free(p);
}

size_t my_strftime(TCHAR *s, size_t max, const TCHAR  *fmt, const struct tm *tm) {
  return _tcsftime(s, max, fmt, tm);
}

tstring get_full_path(const tstring &path) {
#ifdef _WIN32
  TCHAR *throwaway;
  TCHAR real_path[1024];
  int rc = GetFullPathName(path.c_str(),1024,real_path,&throwaway);
  assert(rc <= 1024); // path size is limited by MAX_PATH (260)
  assert(rc != 0); // rc==0 in case of error
  return tstring(real_path);
#else//_WIN32
#ifdef PATH_MAX
  static TCHAR buffer[PATH_MAX];
#else//PATH_MAX
  int path_max = pathconf(path.c_str(), _PC_PATH_MAX);
  if (path_max <= 0)
    path_max = 4096;
  TCHAR *buffer = (TCHAR *) malloc(path_max*sizeof(TCHAR));
  if (!buffer)
    return tstring(path);
#endif//PATH_MAX
  if (!realpath(path.c_str(), buffer))
    _tcscpy(buffer, path.c_str());
  tstring result(buffer);
#ifndef PATH_MAX
  free(buffer);
#endif//!PATH_MAX
  return result;
#endif//_WIN32
}

tstring get_string_prefix(const tstring& str, const tstring& separator) {
  const tstring::size_type last_separator_pos = str.rfind(separator);
  if (last_separator_pos == string::npos)
    return str;
  return str.substr(0, last_separator_pos);
}

tstring get_string_suffix(const tstring& str, const tstring& separator) {
  const tstring::size_type last_separator_pos = str.rfind(separator);
  if (last_separator_pos == tstring::npos)
    return str;
  return str.substr(last_separator_pos + separator.size(), tstring::npos);
}

tstring get_dir_name(const tstring& path) {
  return get_string_prefix(path, PLATFORM_PATH_SEPARATOR_STR);
}

tstring get_file_name(const tstring& path) {
  return get_string_suffix(path, PLATFORM_PATH_SEPARATOR_STR);
}

tstring get_executable_path(const TCHAR* argv0) {
#ifdef _WIN32
  TCHAR temp_buf[MAX_PATH+1];
  temp_buf[0] = _T('\0');
  int rc = GetModuleFileName(NULL,temp_buf,MAX_PATH);
  assert(rc != 0);
  return tstring(temp_buf);
#elif __APPLE__
  TCHAR temp_buf[MAXPATHLEN+1];
  unsigned int buf_len = MAXPATHLEN;
  int rc = Apple::_NSGetExecutablePath(temp_buf, &buf_len);
  assert(rc == 0);
  return tstring(temp_buf);
#else /* Linux/BSD/POSIX/etc */
  const TCHAR *envpath = _tgetenv(_T("_"));
  if( envpath != NULL ) return get_full_path( envpath );
  else {
    TCHAR* pathtmp;
    TCHAR* path = NULL;
    size_t len = 100;
    int nchars;
    while(1){
      pathtmp = (TCHAR*)realloc(path,len+1);
      if( pathtmp == NULL ){
        free(path);
        return get_full_path(argv0);
      }
      path = pathtmp;
      nchars = readlink(_T("/proc/self/exe"), path, len);
      if( nchars == -1 ){
        free(path);
        return get_full_path(argv0);
      }
      if( nchars < (int) len ){
        path[nchars] = _T('\0');
        string result(path);
        free(path);
        return result;
      }
      len *= 2;
    }
  }
#endif
}

tstring get_executable_dir(const TCHAR *argv0) {
  return get_dir_name(get_executable_path(argv0));
}

tstring remove_file_extension(const tstring& path) {
  return get_string_prefix(path, _T("."));
}

struct ToLower
{
   TCHAR operator() (TCHAR c) const { return _totlower(c); }
};

tstring lowercase(const tstring &str) {
  tstring result = str;
  transform(str.begin(), str.end(), result.begin(), ToLower());
  return result;
}

/*
 * ExpandoStrFmtVaList returns the number of characters written excluding
 * the \0 terminator or 0 on error.
 * realloc() is used on *ppMalloc if cchStack is not 
 * large enough to hold the formated string.
*/
size_t ExpandoStrFmtVaList(wchar_t*Stack, size_t cchStack, wchar_t**ppMalloc, const wchar_t*FmtStr, va_list Args)
{
#ifdef _WIN32
// For _vsnwprintf, the \0 terminator is not part of the size
#define ExpandoStrFmtVaList_vsnwprintf(d,c,f,v) _vsnwprintf((d),(c)?(c)-1:0,(f),(v))
#else
#define ExpandoStrFmtVaList_vsnwprintf vswprintf
#endif
#if defined(_ISOC99_SOURCE) || _POSIX_C_SOURCE >= 200112L
  const bool cansizecalc = true, msvcbackdoor = false;
#else
  static char testedsizecalc = 0;
  if (!testedsizecalc)
  {
#ifdef _WIN32
    size_t cch = ExpandoStrFmtVaList_vsnwprintf(0, INT_MAX, L"333", Args);
#else
    wchar_t testbuf[1+1];
    size_t cch = ExpandoStrFmtVaList_vsnwprintf(testbuf, COUNTOF(testbuf), L"333", Args);
#endif
    testedsizecalc = (3 == cch) + 1;
  }
#ifdef _WIN32
  const bool msvcbackdoor = !!(testedsizecalc - 1), cansizecalc = false;
#else
  const bool cansizecalc = !!(testedsizecalc - 1), msvcbackdoor = false;
#endif
#endif
  size_t &cchAvail = cchStack, cch;
  wchar_t *&dest = Stack, *mem = *ppMalloc;
  for(;;)
  {
    cch = ExpandoStrFmtVaList_vsnwprintf(dest, cchAvail, FmtStr, Args);
    if ((unsigned)-1 == cch)
    {
      cch = 0;
      if (cansizecalc) break; // vswprintf error, abort!
      if (msvcbackdoor)
        cchAvail = ExpandoStrFmtVaList_vsnwprintf(0, INT_MAX, FmtStr, Args) + 1;
      else
        cchAvail = 4 * STD_MAX(cchAvail, (size_t)500);
    }
    else
    {
      if (cch < cchAvail) break; // We are done.
      cchAvail = ++cch; // cch from vswprintf did not include the \0 terminator
    }
    dest = mem = (wchar_t*) realloc(mem, cchAvail * sizeof(wchar_t));
    if (!mem) return 0;
  }
  *ppMalloc = mem;
  return cch;
}


int sane_system(const TCHAR *command) {
#ifdef _WIN32

  // workaround for bug #1509909
  // http://sf.net/tracker/?func=detail&atid=373085&aid=1509909&group_id=22049
  //
  // cmd.exe /C has some weird handling for quotes. it strips
  // the surrounding quotes, if they exist. if there are quotes
  // around the program path and its arguments, it will strip
  // the outer quotes. this may result in something like:
  //   `program files\nsis\makensis.exe" "args`
  // which obviously fails...
  //
  // to avoid the stripping, a harmless string is prefixed
  // to the command line.
  tstring command_s = _T("IF 1==1 ");
  command_s += command;
  return _tsystem(command_s.c_str());

#else
  return _tsystem(command);
#endif
}


void PrintColorFmtMsg(unsigned int type, const TCHAR *fmtstr, va_list args)
{
#ifdef _WIN32
  const HANDLE hWin32Con = GetStdHandle(STD_OUTPUT_HANDLE);
  static INT32 contxtattrbak = -1;
  WORD txtattr = 0;
  if (contxtattrbak < 0)
  {
    if (-1 == contxtattrbak)
    {
      CONSOLE_SCREEN_BUFFER_INFO csbi;
      contxtattrbak = -2;
      if ( GetConsoleScreenBufferInfo(hWin32Con, &csbi) )
      {
        contxtattrbak = csbi.wAttributes;
        goto gottxtattrbak;
      }
    }
  }
  else
  {
gottxtattrbak:
    switch(type & 0xF)
    {
    case 0: goto resettxtattr;
    case 1: txtattr = FOREGROUND_INTENSITY|FOREGROUND_GREEN|FOREGROUND_RED; break;
    case 2: txtattr = FOREGROUND_INTENSITY|FOREGROUND_RED; break;
    }
    SetConsoleTextAttribute(hWin32Con, txtattr);
  }
#endif

  if (fmtstr) _vftprintf(g_output, fmtstr, args);
  fflush(g_output);

#ifdef _WIN32
  if (contxtattrbak >= 0 && !(0x10 & type))
  {
resettxtattr:
    SetConsoleTextAttribute(hWin32Con, contxtattrbak);
  }
#endif
}

void FlushOutputAndResetPrintColor()
{
  fflush(g_output);
#ifdef _WIN32
  PrintColorFmtMsg(0, NULL, (va_list)NULL); //va_list is just a pointer on windows so this is ok
#endif
}


static bool GetDLLVersionUsingRE(const tstring& filepath, DWORD& high, DWORD & low)
{
  bool found = false;

  FILE *fdll = FOPEN(filepath.c_str(), ("rb"));
  if (!fdll)
    return 0;

  fseek(fdll, 0, SEEK_END);
  unsigned int len = ftell(fdll);
  fseek(fdll, 0, SEEK_SET);

  LPBYTE dll = (LPBYTE) malloc(len);

  if (!dll)
  {
    fclose(fdll);
    return 0;
  }

  if (fread(dll, 1, len, fdll) != len)
  {
    fclose(fdll);
    free(dll);
    return 0;
  }
  fclose(fdll);

  try
  {
    CResourceEditor *dllre = new CResourceEditor(dll, len);
    LPBYTE ver = dllre->GetResource(VS_FILE_INFO, VS_VERSION_INFO, 0);
    int versize = dllre->GetResourceSize(VS_FILE_INFO, VS_VERSION_INFO, 0);

    if (ver)
    {
      if ((size_t) versize > sizeof(WORD) * 3)
      {
        // get VS_FIXEDFILEINFO from VS_VERSIONINFO
        WCHAR *szKey = (WCHAR *)(ver + sizeof(WORD) * 3);
        int len = (wcslen(szKey) + 1) * sizeof(WCHAR) + sizeof(WORD) * 3;
        len = (len + 3) & ~3; // align on DWORD boundry
        VS_FIXEDFILEINFO *verinfo = (VS_FIXEDFILEINFO *)(ver + len);
        if (versize > len && verinfo->dwSignature == VS_FFI_SIGNATURE)
        {
          low = verinfo->dwFileVersionLS;
          high = verinfo->dwFileVersionMS;
          found = true;
        }
      }
      dllre->FreeResource(ver);
    }

    delete dllre;
  }
  catch (exception&)
  {
  }

  return found;
}

static bool GetDLLVersionUsingAPI(const tstring& filepath, DWORD& high, DWORD& low)
{
  bool found = false;

#ifdef _WIN32
  TCHAR path[1024];
  TCHAR *name;
  path[0] = 0;

  GetFullPathName(filepath.c_str(), 1024, path, &name);

  DWORD d;
  DWORD verSize = GetFileVersionInfoSize(path, &d);
  if (verSize)
  {
    void *buf = (void *) GlobalAlloc(GPTR, verSize);
    if (buf)
    {
      UINT uLen;
      VS_FIXEDFILEINFO *pvsf;
      if (GetFileVersionInfo(path, 0, verSize, buf) && VerQueryValue(buf, _T("\\"), (void**) &pvsf, &uLen))
      {
        low = pvsf->dwFileVersionLS;
        high = pvsf->dwFileVersionMS;
        found = true;
      }
      GlobalFree(buf);
    }
  }
#endif

  return found;
}

#ifdef _WIN32

// the following structure must be byte-aligned.
#pragma pack( push, pre_vxd_ver, 1 )
typedef struct _VXD_VERSION_RESOURCE {
  char  cType;				// Should not be converted to TCHAR (JP)
  WORD  wID;
  char  cName;				// Should not be converted to TCHAR (JP)
  WORD  wOrdinal;
  WORD  wFlags;
  DWORD dwResSize;
  BYTE  bVerData;
} VXD_VERSION_RESOURCE, *PVXD_VERSION_RESOURCE;
#pragma pack( pop, pre_vxd_ver )

static BOOL GetVxdVersion( LPCTSTR szFile, LPDWORD lpdwLen, LPVOID lpData ) 
{

  HANDLE hFile        = NULL;
  HANDLE hFileMapping = NULL;
  void * pView        = NULL;
  DWORD  dwSize       = 0;
  DWORD  dwError      = 0;

  PIMAGE_DOS_HEADER       pDosExeHdr = NULL;
  PIMAGE_NT_HEADERS       pNtExeHdr  = NULL;
  PIMAGE_VXD_HEADER       pLEHdr     = NULL;
  PVXD_VERSION_RESOURCE   pVerRes    = NULL;
  LPVOID                  pRawRes    = NULL;

  // Open the file for shared read access.
  hFile = CreateFile( szFile, GENERIC_READ, FILE_SHARE_READ,
       NULL, OPEN_EXISTING, 0, NULL );
  if ( hFile == INVALID_HANDLE_VALUE )
  {
    return FALSE;
  }

  // Create a read-only file mapping object for the file.
  hFileMapping = CreateFileMapping( hFile, NULL,
       PAGE_READONLY, 0, 0, NULL);
  if ( !hFileMapping )
  {
    dwError = GetLastError();

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( dwError );
    return FALSE;
  }

  // Map a view of the the file.
  pView = MapViewOfFile( hFileMapping, FILE_MAP_READ, 0, 0, 0 );
  if ( !pView )
  {
    dwError = GetLastError();

    if ( hFileMapping )
      CloseHandle( hFileMapping );

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( dwError );
    return FALSE;
  }

  // The DOS header begins at byte 0.
  pDosExeHdr = (PIMAGE_DOS_HEADER) pView;

  // Check to make sure the file has a DOS EXE header.
  if ( pDosExeHdr->e_magic != IMAGE_DOS_SIGNATURE ) 
  {
    if ( pView )
      UnmapViewOfFile( pView );

    if ( hFileMapping )
      CloseHandle( hFileMapping );

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( ERROR_BAD_FORMAT );
    return FALSE;
  }

  // Find the beginning of the NT header at offset e_lfanew.
  pNtExeHdr = (PIMAGE_NT_HEADERS) ( (ULONG_PTR) pView
       + pDosExeHdr->e_lfanew );

  // Check to make sure the file is a VxD.
  if ( (DWORD) pNtExeHdr->Signature != IMAGE_VXD_SIGNATURE ) 
  {
    if ( pView )
      UnmapViewOfFile( pView );

    if ( hFileMapping )
      CloseHandle( hFileMapping );

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( ERROR_BAD_FORMAT );
    return FALSE;
  }

  // The LE header begins at the same place as the NT header.
  pLEHdr = (PIMAGE_VXD_HEADER) pNtExeHdr;

  // e32_winreslen contains the size of the VxD's version resource.
  if ( pLEHdr->e32_winreslen == 0 ) {
    *lpdwLen = 0;
    if ( pView )
      UnmapViewOfFile( pView );

    if ( hFileMapping )
      CloseHandle( hFileMapping );

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( ERROR_RESOURCE_DATA_NOT_FOUND );
    return FALSE;
  }

  // e32_winresoff contains the offset of the resource in the VxD.
  pVerRes = (VXD_VERSION_RESOURCE *) ( (ULONG_PTR) pView
       + pLEHdr->e32_winresoff );
  dwSize = pVerRes->dwResSize;
  pRawRes = &(pVerRes->bVerData);

  // Make sure the supplied buffer is large enough for the resource.
  if ( ( lpData == NULL ) || ( *lpdwLen < dwSize ) ) {
    *lpdwLen = dwSize;

    if ( pView )
      UnmapViewOfFile( pView );

    if ( hFileMapping )
      CloseHandle( hFileMapping );

    if ( hFile != INVALID_HANDLE_VALUE )
      CloseHandle( hFile );

    SetLastError( ERROR_INSUFFICIENT_BUFFER );
    return FALSE;
  }

  // Zero the passed buffer and copy the resource into it.
  ZeroMemory( lpData, *lpdwLen );
  CopyMemory( lpData, pRawRes, dwSize );
  *lpdwLen = dwSize;

  // Clean up resources.
  if ( pView )
    UnmapViewOfFile( pView );

  if ( hFileMapping )
    CloseHandle( hFileMapping );

  if ( hFile != INVALID_HANDLE_VALUE )
    CloseHandle( hFile );

  SetLastError(0);
  return TRUE;
}

static DWORD GetVxdVersionInfoSize( LPCTSTR szFile ) 
{
  DWORD dwResult = 0;

  // Call GetVxdVersion() with NULL for the pointer to the buffer.
  if ( !GetVxdVersion( szFile, &dwResult, NULL ) ) 
  {
    DWORD dwError = GetLastError();

    // GetVxdVersion() will fail with ERROR_INSUFFICIENT_BUFFER and
    // the required buffer size will be returned in dwResult.
    if ( dwError == ERROR_INSUFFICIENT_BUFFER ) 
    {
      SetLastError( 0 );
      return dwResult;
    }
  }

  // The following line is never executed.
  return 0;
}

static BOOL GetVxdVersionInfo( LPCTSTR szFile, DWORD dwLen, LPVOID lpData ) 
{
  return GetVxdVersion( szFile, &dwLen, lpData );
}

#endif //_WIN32

static bool GetDLLVersionFromVXD(const tstring& filepath, DWORD& high, DWORD& low)
{
  bool found = false;

#ifdef _WIN32
  DWORD verSize = GetVxdVersionInfoSize(filepath.c_str());
  if (verSize)
  {
    void *buf = (void *) GlobalAlloc(GPTR, verSize);
    if (buf)
    {
      UINT uLen;
      VS_FIXEDFILEINFO *pvsf;
      if (GetVxdVersionInfo(filepath.c_str(), verSize, buf) && VerQueryValue(buf, _T("\\"), (void**) &pvsf, &uLen))
      {
        low = pvsf->dwFileVersionLS;
        high = pvsf->dwFileVersionMS;
        found = true;
      }
      GlobalFree(buf);
    }
  }
#endif

  return found;
}

bool GetDLLVersion(const tstring& filepath, DWORD& high, DWORD& low)
{
  if (GetDLLVersionUsingAPI(filepath, high, low))
    return true;

  if (GetDLLVersionUsingRE(filepath, high, low))
    return true;

  if (GetDLLVersionFromVXD(filepath, high, low))
    return true;

  return false;
}

bool Platform_SupportsUTF8Conversion()
{
  static unsigned char cached = -1;
  if (-1 == cached) cached = !!IsValidCodePage(CP_UTF8);
  return cached != 0;
}