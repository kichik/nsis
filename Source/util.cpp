#include "Platform.h"
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>
#include "exehead/fileform.h"
#include "util.h"
#include "strlist.h"

#ifndef _WIN32
#  include <ctype.h>
#  include <unistd.h> // for close(2)
#  include <fcntl.h> // for open(2)
#  include <iconv.h>
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
    if (g_display_errors) fprintf(g_output,"MakeNSIS done - hit enter to close...");
    fflush(stdout);
    int a;
    while ((a=getchar()) != '\r' && a != '\n' && a != 27/*esc*/);
  }
}

// Returns 0 if everything is OK
// Returns -1 if can't find the file
// Returns -2 if the file is an invalid bitmap
// Returns -3 if the size doesn't match
// Returns -4 if the bpp doesn't match
int update_bitmap(CResourceEditor* re, WORD id, const char* filename, int width/*=0*/, int height/*=0*/, int maxbpp/*=0*/) {
  FILE *f = FOPEN(filename, "rb");
  if (!f) return -1;

  if (fgetc(f) != 'B' || fgetc(f) != 'M') {
    fclose(f);
    return -2;
  }

  if (width != 0) {
    LONG biWidth;
    fseek(f, 18, SEEK_SET); // Seek to the width member of the header
    fread(&biWidth, sizeof(LONG), 1, f);
    FIX_ENDIAN_INT32_INPLACE(biWidth);
    if (width != biWidth) {
      fclose(f);
      return -3;
    }
  }

  if (height != 0) {
    LONG biHeight;
    fseek(f, 22, SEEK_SET); // Seek to the height member of the header
    fread(&biHeight, sizeof(LONG), 1, f);
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
  if (!bitmap) throw bad_alloc();

  fseek(f, 14, SEEK_SET);
  if (fread(bitmap, 1, dwSize, f) != dwSize) {
    fclose(f);
    return -2;
  }
  fclose(f);

  re->UpdateResource(RT_BITMAP, MAKEINTRESOURCE(id), NSIS_DEFAULT_LANG, bitmap, dwSize);

  free(bitmap);

  return 0;
}

// Added by Amir Szekely 8th July 2002
// Icon editing structures
typedef struct {
  WORD wReserved;
  WORD wIsIcon;
  WORD wCount;
} IconGroupHeader;

typedef struct {
  BYTE bWidth;
  BYTE bHeight;
  BYTE bPaletteEntries;
  BYTE bReserved;
  WORD wPlanes;
  WORD wBitsPerPixel;
  DWORD dwRawSize;
  DWORD dwImageOffset;
} FileIconGroupEntry;

typedef struct {
  BYTE bWidth;
  BYTE bHeight;
  BYTE bPaletteEntries;
  BYTE bReserved;
  WORD wPlanes;
  WORD wBitsPerPixel;
  DWORD dwRawSize;
  WORD wRsrcId;
} RsrcIconGroupEntry;

#define SIZEOF_RSRC_ICON_GROUP_ENTRY 14

static FILE * open_icon(const char* filename, IconGroupHeader *igh)
{
  FILE* f = FOPEN(filename, "rb");
  if (!f)
    throw runtime_error("can't open file");

  if (!fread(igh, sizeof(IconGroupHeader), 1, f))
    throw runtime_error("unable to read file");

  FIX_ENDIAN_INT16_INPLACE(igh->wIsIcon);
  FIX_ENDIAN_INT16_INPLACE(igh->wReserved);
  FIX_ENDIAN_INT16_INPLACE(igh->wCount);

  if (igh->wIsIcon != 1 || igh->wReserved != 0)
    throw runtime_error("invalid icon file");

  return f;
}

// replace_icon, must get an initialized resource editor
void replace_icon(CResourceEditor* re, WORD wIconId, const char* filename)
{
  IconGroupHeader igh;
  FILE *f = open_icon(filename, &igh);

  BYTE* rsrcIconGroup = (BYTE*)malloc(sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);
  if (!rsrcIconGroup) throw bad_alloc();

  CopyMemory(rsrcIconGroup, &igh, sizeof(IconGroupHeader));

  RsrcIconGroupEntry* ige = (RsrcIconGroupEntry*)(rsrcIconGroup + sizeof(IconGroupHeader));

  int i = 1;

  // Delete old icons
  while (re->UpdateResource(RT_ICON, MAKEINTRESOURCE(i++), NSIS_DEFAULT_LANG, 0, 0));

  for (i = 0; i < igh.wCount; i++) {
    fread(ige, sizeof(FileIconGroupEntry)-sizeof(DWORD), 1, f);

    FIX_ENDIAN_INT16_INPLACE(ige->wRsrcId);
    FIX_ENDIAN_INT32_INPLACE(ige->dwRawSize);

    ige->wRsrcId = i+1;

    DWORD dwOffset;
    fread(&dwOffset, sizeof(DWORD), 1, f);

    FIX_ENDIAN_INT32_INPLACE(dwOffset);

    fpos_t pos;
    fgetpos(f, &pos);

    if (fseek(f, dwOffset, SEEK_SET)) {
      free(rsrcIconGroup);
      throw runtime_error("corrupted icon file, too small");
    }
    BYTE* iconData = (BYTE*)malloc(ige->dwRawSize);
    if (!iconData) {
      free(rsrcIconGroup);
      throw bad_alloc();
    }
    fread(iconData, sizeof(BYTE), ige->dwRawSize, f);
    re->UpdateResource(RT_ICON, MAKEINTRESOURCE(i+1), NSIS_DEFAULT_LANG, iconData, ige->dwRawSize);
    free(iconData);

    fsetpos(f, &pos);

    // Seems like the compiler refuses to increase the pointer by just 14.
    // If you'll replace this line by ige++ you will get unwanted results.
    ige = (RsrcIconGroupEntry*)((BYTE*)ige + SIZEOF_RSRC_ICON_GROUP_ENTRY);
  }

  fclose(f);

  re->UpdateResource(RT_GROUP_ICON, MAKEINTRESOURCE(wIconId), NSIS_DEFAULT_LANG, rsrcIconGroup, sizeof(IconGroupHeader) + igh.wCount*SIZEOF_RSRC_ICON_GROUP_ENTRY);

  free(rsrcIconGroup);
}

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
// returns the data of the uninstaller icon that should replace the installer icon data
unsigned char* generate_uninstall_icon_data(const char* filename, size_t &size)
{
  int i;

  IconGroupHeader igh;
  FILE *f = open_icon(filename, &igh);

  int iNewIconSize = 0;
  FileIconGroupEntry ige;

  DWORD* offsets = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  DWORD* rawSizes = (DWORD*)malloc(sizeof(DWORD)*igh.wCount);
  if (!offsets || !rawSizes) throw bad_alloc();

  for (i = 0; i < igh.wCount; i++) {
    if (!fread(&ige, sizeof(FileIconGroupEntry), 1, f)) throw runtime_error("unable to read file");
    offsets[i] = ige.dwImageOffset;
    rawSizes[i] = ige.dwRawSize;
    iNewIconSize += FIX_ENDIAN_INT32(ige.dwRawSize);
  }

  // Before each icon come two DWORDs, one for size and the other for offset (set later)
  // The last size is 0, no offset
  iNewIconSize += sizeof(DWORD)*(1 + igh.wCount*2);

  BYTE* pbUninstIcon = (BYTE*)malloc(iNewIconSize);
  if (!pbUninstIcon) throw bad_alloc();

  BYTE* seeker = pbUninstIcon;

  for (i = 0; i < igh.wCount; i++) {
    *(DWORD*)seeker = rawSizes[i];
    seeker += sizeof(DWORD);
    *(DWORD*)seeker = 0;
    seeker += sizeof(DWORD);
    fseek(f, FIX_ENDIAN_INT32(offsets[i]), SEEK_SET);
    fread(seeker, 1, FIX_ENDIAN_INT32(rawSizes[i]), f);
    seeker += FIX_ENDIAN_INT32(rawSizes[i]);
  }

  // This is how we know there are no more icons (size = 0)
  *(DWORD*)seeker = 0;

  free(offsets);
  free(rawSizes);

  size = iNewIconSize;

  return pbUninstIcon;
}

// Added by Amir Szekely 11th July 2002
#define MY_ASSERT(x, y) if (x) {if (g_display_errors) fprintf(g_output,"\nError finding icon resources: %s -- failing!\n", y);return 0;}

int find_in_dir(PRESOURCE_DIRECTORY rd, WORD id) {
  WORD i = FIX_ENDIAN_INT16(rd->Header.NumberOfNamedEntries);
  WORD l = FIX_ENDIAN_INT16(rd->Header.NumberOfNamedEntries + rd->Header.NumberOfIdEntries);

  for (; i < l; i++) {
    if (FIX_ENDIAN_INT16(rd->Entries[i].Id) == id) {
      return i;
    }
  }
  return -1;
}

// Fill the array of icons for uninstall with their offsets
// Returns 0 if failed, anything else is the icon offset in the PE.
int generate_unicons_offsets(unsigned char* exeHeader, unsigned char* uninstIconData) {
  int i;

  MY_ASSERT(PIMAGE_DOS_HEADER(exeHeader)->e_magic != IMAGE_DOS_SIGNATURE, "invalid dos header");

  PIMAGE_NT_HEADERS ntHeaders = PIMAGE_NT_HEADERS(exeHeader + PIMAGE_DOS_HEADER(exeHeader)->e_lfanew);

  MY_ASSERT(ntHeaders->Signature != IMAGE_NT_SIGNATURE, "invalid nt headers");

  DWORD dwResourceSectionVA = ntHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
  PIMAGE_SECTION_HEADER sectionHeadersArray = IMAGE_FIRST_SECTION(ntHeaders);

  for (i = 0; i < ntHeaders->FileHeader.NumberOfSections; i++)
    if (dwResourceSectionVA == sectionHeadersArray[i].VirtualAddress)
      break;

  MY_ASSERT(i == ntHeaders->FileHeader.NumberOfSections, "can't find resource section");

  PRESOURCE_DIRECTORY rdRoot = PRESOURCE_DIRECTORY(exeHeader + sectionHeadersArray[i].PointerToRawData);

  int iNextSection;
  if (i == ntHeaders->FileHeader.NumberOfSections - 1)
    iNextSection = (int)ntHeaders->OptionalHeader.SizeOfImage;
  else
    iNextSection = (int)sectionHeadersArray[i+1].PointerToRawData;

  MY_ASSERT((long)rdRoot - (long)exeHeader > iNextSection, "corrupted EXE - invalid pointer");

  int idx = find_in_dir(rdRoot, (WORD) (long) RT_ICON);
  MY_ASSERT(idx == -1, "no icons?!");
  MY_ASSERT(!rdRoot->Entries[idx].DirectoryOffset.DataIsDirectory, "bad resource directory");

  PRESOURCE_DIRECTORY rdIcons = PRESOURCE_DIRECTORY(rdRoot->Entries[idx].DirectoryOffset.OffsetToDirectory + DWORD(rdRoot));

  MY_ASSERT((long)rdIcons - (long)exeHeader > iNextSection, "corrupted EXE - invalid pointer");

  MY_ASSERT(rdIcons->Header.NumberOfIdEntries == 0, "no icons found");

  for (i = 0; i < rdIcons->Header.NumberOfIdEntries; i++) { // Icons dir can't have named entries
    MY_ASSERT(!rdIcons->Entries[i].DirectoryOffset.DataIsDirectory, "bad resource directory");
    PRESOURCE_DIRECTORY rd = PRESOURCE_DIRECTORY(rdIcons->Entries[i].DirectoryOffset.OffsetToDirectory + DWORD(rdRoot));
    
    MY_ASSERT((long)rd - (long)exeHeader > iNextSection, "corrupted EXE - invalid pointer");
    MY_ASSERT(rd->Entries[0].DirectoryOffset.DataIsDirectory, "bad resource directory");
    
    PIMAGE_RESOURCE_DATA_ENTRY rde = PIMAGE_RESOURCE_DATA_ENTRY(rd->Entries[0].OffsetToData + DWORD(rdRoot));

    MY_ASSERT((long)rde - (long)exeHeader > iNextSection, "corrupted EXE - invalid pointer");

    // find icon to replace
    LPBYTE seeker = uninstIconData;
    while (*seeker) {
      DWORD dwSize = *(DWORD*)seeker;
      seeker += sizeof(DWORD);
      DWORD dwOffset = *(DWORD*)seeker;
      // if we haven't set the offset yet and the size is the same, it's a match
      if (!dwOffset && dwSize == rde->Size)
        break;

      seeker += dwSize + sizeof(DWORD);

      // reached the end of the list and no match
      MY_ASSERT(!*seeker, "installer, uninstaller icon size mismatch - see the Icon instruction's documentation for more information");
    }

    // Set offset
    *(LPDWORD) seeker = rde->OffsetToData + DWORD(rdRoot) - dwResourceSectionVA - DWORD(exeHeader);

    MY_ASSERT(*(int*)seeker > iNextSection || *(int*)seeker < (long)rdRoot - (long)exeHeader, "invalid data offset - icon resource probably compressed");
  }

  LPBYTE seeker = uninstIconData;
  while (*seeker) {
    DWORD dwSize = *(DWORD*)seeker;
    seeker += sizeof(DWORD);
    DWORD dwOffset = *(DWORD*)seeker;
    seeker += sizeof(DWORD);
    // offset isn't set which means we found no match for this one
    MY_ASSERT(!dwOffset, "installer, uninstaller number of icons doesn't match - see the Icon instruction's documentation for more information");
    seeker += dwSize;
  }

  return PIMAGE_RESOURCE_DATA_ENTRY(PRESOURCE_DIRECTORY(rdIcons->Entries[0].DirectoryOffset.OffsetToDirectory + DWORD(rdRoot))->Entries[0].OffsetToData + DWORD(rdRoot))->OffsetToData + DWORD(rdRoot) - dwResourceSectionVA - DWORD(exeHeader);
}
#endif // NSIS_CONFIG_UNINSTALL_SUPPORT

// returns the number of WCHARs in str including null charcter
size_t WCStrLen(const WCHAR* szwStr) {
  size_t i;
  for (i = 0; szwStr[i]; i++);
  return i+1;
}

#ifndef _WIN32
char *CharPrev(const char *s, const char *p) {
  if (!s || !p || p < s)
    return NULL;
  while (*s) {
    char *n = CharNext(s);
    if (n >= p)
      break;
    s = n;
  }
  return (char *) s;
}

char *CharNext(const char *s) {
  int l = 0;
  if (s && *s)
    l = max(1, mblen(s, MB_CUR_MAX));
  return (char *) s + l;
}

int wsprintf(char *s, const char *format, ...) {
  va_list val;
  va_start(val, format);
  int res = vsnprintf(s, 1024, format, val);
  va_end(val);
  return res;
}

// iconv const inconsistency workaround by Alexandre Oliva
template <typename T>
inline size_t __iconv_adaptor
  (size_t (*iconv_func)(iconv_t, T, size_t *, char**,size_t*),
  iconv_t cd, char **inbuf, size_t *inbytesleft,
  char **outbuf, size_t *outbytesleft)
{
  return iconv_func (cd, (T)inbuf, inbytesleft, outbuf, outbytesleft);
}

void static create_code_page_string(char *buf, size_t len, UINT code_page) {
  if (code_page == CP_ACP)
    code_page = 1252;

  snprintf(buf, len, "CP%d", code_page);
}

int WideCharToMultiByte(UINT CodePage, DWORD dwFlags, LPCWSTR lpWideCharStr,
    int cchWideChar, LPSTR lpMultiByteStr, int cbMultiByte, LPCSTR lpDefaultChar,
    LPBOOL lpUsedDefaultChar) {
  static char buffer[4096];

  char cp[128];
  create_code_page_string(cp, sizeof(cp), CodePage);

  iconv_t cd = iconv_open(cp, "UCS-2LE");
  if (cd == (iconv_t) -1) {
    return 0;
  }

  if (cchWideChar < 0) {
    cchWideChar = (int) WCStrLen(lpWideCharStr); // including null char
  }

  if (cbMultiByte == 0) {
    cbMultiByte = sizeof(buffer);
    lpMultiByteStr = buffer;
  }

  char *in = (char *) lpWideCharStr;
  char *out = lpMultiByteStr;
  size_t inbytes = cchWideChar * sizeof(WCHAR);
  size_t outbytes = cbMultiByte;

  if (__iconv_adaptor(iconv, cd, &in, &inbytes, &out, &outbytes) == (size_t) -1) {
    iconv_close(cd);
    return 0;
  }

  iconv_close(cd);

  return cbMultiByte - outbytes;
}

int MultiByteToWideChar(UINT CodePage, DWORD dwFlags, LPCSTR lpMultiByteStr,
    int cbMultiByte, LPWSTR lpWideCharStr, int cchWideChar) {
  static WCHAR buffer[4096];

  char cp[128];
  create_code_page_string(cp, sizeof(cp), CodePage);

  iconv_t cd = iconv_open("UCS-2LE", cp);
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

  if (__iconv_adaptor(iconv, cd, &in, &inbytes, &out, &outbytes) == (size_t) -1) {
    iconv_close(cd);
    return 0;
  }

  iconv_close(cd);

  return cchWideChar - (outbytes / sizeof (WCHAR));
}

#define MY_ERROR_MSG(x) {if (g_display_errors) {fprintf(g_output,"%s", x);}}

char *my_convert(const char *path)
{
  // TODO: (orip) ref. this func. to use std::string?
  char *converted_path = strdup(path);
  size_t len = strlen(path);

  if(!converted_path)
  {
    MY_ERROR_MSG("Error: could not allocate memory in my_convert()\n");
    return (char*) path; /* dirty */
  }

  /* Replace drive letter X: by /X */
  if(len >= 2)
  {
    if (path[1] == ':')
    {
      converted_path[0] = '/';
      converted_path[1] = (char) tolower((int) path[0]);
    }
  }

  char *p = converted_path;

  do
  {
    if (*p == '\\')
    {
      *p = '/';
    }
    p = CharNext(p);
  }
  while (*p);

  return converted_path;
}

void my_convert_free(char *converted_path)
{
  free(converted_path);
}

int my_open(const char *pathname, int flags)
{
  char *converted_pathname = my_convert(pathname);

  int result = open(converted_pathname, flags);
  my_convert_free(converted_pathname);
  return result;
}

FILE *my_fopen(const char *path, const char *mode)
{
  char *converted_path = my_convert(path);

  FILE *result = fopen(converted_path, mode);
  my_convert_free(converted_path);
  return result;
}

int my_glob(const char *pattern, int flags,
         int errfunc(const char * epath, int eerrno), glob_t *pglob)
{
  char *converted_pattern = my_convert(pattern);

  int result = glob(converted_pattern, flags, errfunc, pglob);
  my_convert_free(converted_pattern);
  return result;
}
#endif//!_WIN32

void *operator new(size_t size) {
  void *p = malloc(size);
  if (!p)
    throw bad_alloc();
  return p;
}

void operator delete(void *p) {
  if (p) free(p);
}

void operator delete [](void *p) {
  if (p) free(p);
}

size_t my_strftime(char *s, size_t max, const char  *fmt, const struct tm *tm) {
  return strftime(s, max, fmt, tm);
}

string get_full_path(const string &path) {
#ifdef _WIN32
  char *throwaway;
  char real_path[1024];
  int rc = GetFullPathName(path.c_str(),1024,real_path,&throwaway);
  assert(rc <= 1024); // path size is limited by MAX_PATH (260)
  assert(rc != 0); // rc==0 in case of error
  return string(real_path);
#else//_WIN32
#ifdef PATH_MAX
  static char buffer[PATH_MAX];
#else//PATH_MAX
  int path_max = pathconf(path, _PC_PATH_MAX);
  if (path_max <= 0)
    path_max = 4096;
  char *buffer = (char *) malloc(path_max);
  if (!buffer)
    return string(path);
#endif//PATH_MAX
  if (!realpath(path.c_str(), buffer))
    strcpy(buffer, path.c_str());
  string result(buffer);
#ifndef PATH_MAX
  free(buffer);
#endif//!PATH_MAX
  return result;
#endif//_WIN32
}

string get_string_prefix(const string& str, const string& separator) {
  const string::size_type last_separator_pos = str.rfind(separator);
  if (last_separator_pos == string::npos)
    return str;
  return str.substr(0, last_separator_pos);
}

string get_string_suffix(const string& str, const string& separator) {
  const string::size_type last_separator_pos = str.rfind(separator);
  if (last_separator_pos == string::npos)
    return str;
  return str.substr(last_separator_pos + separator.size(), string::npos);
}

string get_dir_name(const string& path) {
  return get_string_prefix(path, PLATFORM_PATH_SEPARATOR_STR);
}

string get_file_name(const string& path) {
  return get_string_suffix(path, PLATFORM_PATH_SEPARATOR_STR);
}

string get_executable_path(const char* argv0) {
#ifdef _WIN32
  char temp_buf[MAX_PATH+1];
  temp_buf[0] = '\0';
  int rc = GetModuleFileName(NULL,temp_buf,MAX_PATH);
  assert(rc != 0);
  return string(temp_buf);
#elif __APPLE__
  char temp_buf[MAXPATHLEN+1];
  unsigned long buf_len = MAXPATHLEN;
  int rc = _NSGetExecutablePath(temp_buf, &buf_len);
  assert(rc == 0);
  return string(temp_buf);
#else /* Linux/BSD/POSIX/etc */
  const char *envpath = getenv("_");
  if( envpath != NULL ) return get_full_path( envpath );
  else {
    char* pathtmp;
    char* path;
    size_t len = 100;
    size_t nchars;
    while(1){
      pathtmp = (char*)realloc(path,len+1);
      if( pathtmp == NULL ){
        free(path);
        return get_full_path(argv0);
      }
      path = pathtmp;
      nchars = readlink("/proc/self/exe", path, len);
      if( nchars < 0 ){
        free(path);
        return get_full_path(argv0);
      }
      if( nchars < len ){
        path[nchars] = '\0';
        string result(path);
        free(path);
        return result;
      }
      len *= 2;
    }
  }
#endif
}

string get_executable_dir(const char *argv0) {
  return get_dir_name(get_executable_path(argv0));
}

string remove_file_extension(const string& path) {
  return get_string_prefix(path, ".");
}

string lowercase(const string &str) {
  string result = str;
  transform(str.begin(), str.end(), result.begin(), tolower);
  return result;
}
