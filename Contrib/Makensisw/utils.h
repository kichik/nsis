/* 
  Copyright (c) 2002 Robert Rainwater
  Copyright (c) 2002 Robert Rainwater
  Contributors: Justin Frankel, Fritz Elfert, and Sunil Kamath

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Unicode support by Jim Park -- 08/20/2007

*/
#ifndef UTILS_H
#define UTILS_H
#include "resource.h"
#include "toolbar.h"

#ifdef COUNTOF
#undef COUNTOF
#endif
#define COUNTOF(a) (sizeof(a)/sizeof(a[0]))

#if defined(_MSC_VER) && _MSC_VER >= 1200
EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_THISCOMPONENT ( (HINSTANCE) &__ImageBase )
#define HINST_APPLICATION HINST_THISCOMPONENT
#else
#define HINST_APPLICATION ( (HINSTANCE) GetModuleHandle(NULL) )
#endif

#define MRU_LIST_SIZE 5
#define MRU_DISPLAY_LENGTH 40
#define SYMSETNAME_MAXLEN 40

template<class T> inline T API_cast(INT_PTR(CALLBACK*p1)(HWND,UINT,WPARAM,LPARAM)) { return (DLGPROC) p1; } // DLGPROC in really old SDKs return BOOL

void* MemAllocZI(SIZE_T cb);
void MemSafeFree(void*mem);
#define MemAlloc MemAllocZI
#define MemFree MemSafeFree
HMODULE LoadSysLibrary(LPCSTR Mod);
FARPROC GetSysProcAddr(LPCSTR Mod, LPCSTR FuncName);
bool WriteUTF16LEBOM(HANDLE hFile);

void FreeSpawn(PROCESS_INFORMATION *pPI, HANDLE hRd, HANDLE hWr);
BOOL InitSpawn(STARTUPINFO &si, HANDLE &hRd, HANDLE &hWr);

typedef BYTE PACKEDCMDID_T;
#define PACKCMDID(id) ( PACKEDCMDID_T((id) - IDM_CMDBASE) )
#define UNPACKCMDID(id) ( IDM_CMDBASE + (id) )

int SetArgv(const TCHAR *cmdLine, TCHAR ***argv);
void SetTitle(HWND hwnd,const TCHAR *substr);
void PlayAppSoundAsync(LPCSTR SoundName, int MBFallback = -1);
void CopyToClipboard(HWND hwnd);
enum LOGCOLOR { LC_SUCCESS, LC_WARNING, LC_ERROR, LC_SYSCOLOR };
void SetLogColor(enum LOGCOLOR lc);
void ClearLog(HWND hwnd);
void LogMessage(HWND hwnd,const TCHAR *str);
void ErrorMessage(HWND hwnd,const TCHAR *str);
void CenterOnParent(HWND hwnd);
void SetDialogFocus(HWND hDlg, HWND hCtl); // Use this and not SetFocus()!
#define DlgRet(hDlg, val) ( SetWindowLongPtr((hDlg), DWLP_MSGRESULT, (val)) | TRUE )
HWND GetComboEdit(HWND hCB);
#define DisableItems(hwnd) EnableDisableItems(hwnd, 0)
#define EnableItems(hwnd) EnableDisableItems(hwnd, 1)
void EnableDisableItems(HWND hwnd, int on);
bool OpenRegSettingsKey(HKEY &hKey, bool create = false);
#define CreateRegSettingsKey(refhkey) OpenRegSettingsKey((refhkey), true)
DWORD ReadRegSettingDW(LPCTSTR name, const DWORD defval);
void RestoreWindowPos(HWND hwnd);
void SaveWindowPos(HWND hwnd);
void ResetObjects();
void ResetSymbols();
int InitBranding();
void InitTooltips(HWND h);
void DestroyTooltips();
void AddTip(HWND hWnd,LPCTSTR lpszToolTip);
void ShowDocs();
void RestoreCompressor();
void SaveCompressor();
void SetCompressorStats();

BOOL PopMRUFile(TCHAR* fname);
void PushMRUFile(TCHAR* fname);
void BuildMRUMenus();
void LoadMRUFile(int position);
void ClearMRUList();

struct FSPath {
  template<class T> static inline bool IsAgnosticSeparator(const T c) { return '\\' == c || '/' == c; }
  template<class T> static T* FindLastComponent(T*p) // Note: Returns "" for "dir\"
  {
    for (T *sep = 0, *start = p;; ++p)
      if (!*p)
        return sep ? ++sep : start;
      else if (IsAgnosticSeparator(*p))
        sep = p;
  }
};

template<class T> UINT StrLenT(const T*s) { return sizeof(T) > 1 ? lstrlenW((WCHAR*) s) : lstrlenA((CHAR*) s) ; }
template<class T> T AsciiLoCh(T ch) { return ch >= 'A' && ch <= 'Z' ? (ch | 32) : ch; }

template<class A, class B> int AsciiCmpNI(const A*a, const B*b, SIZE_T len)
{
	int cmp = 0;
	for (SIZE_T i = 0;; ++i)
		if (i == len || (cmp = AsciiLoCh(a[i]) - AsciiLoCh(b[i])) || !a[i])
			return cmp;
}
template<class A, class B> inline bool AsciiMatchNI(const A*a, const B*b, SIZE_T c) { return !AsciiCmpNI(a, b, c); }
template<class A, class B> bool AsciiMatchPrefixI(const A*s, const B*p) { return AsciiMatchNI(s, p, StrLenT(p)); }

template<class T> HKEY GetRegRootKey(const T*Str)
{
	if (AsciiMatchPrefixI(Str, "HKEY_CLASSES_ROOT") || AsciiMatchPrefixI(Str, "HKCR")) return HKEY_CLASSES_ROOT;
	if (AsciiMatchPrefixI(Str, "HKEY_CURRENT_USER") || AsciiMatchPrefixI(Str, "HKCU")) return HKEY_CURRENT_USER;
	if (AsciiMatchPrefixI(Str, "HKEY_LOCAL_MACHINE") || AsciiMatchPrefixI(Str, "HKLM")) return HKEY_LOCAL_MACHINE;
	if (AsciiMatchPrefixI(Str, "HKEY_USERS") || AsciiMatchPrefixI(Str, "HKU")) return HKEY_USERS;
	return (HKEY) NULL;
}
DWORD RegOpenKeyForReading(HKEY hRoot, LPCTSTR SubKey, HKEY*pKey);

bool FileExists(const TCHAR *fname);
bool OpenUrlInDefaultBrowser(HWND hwnd, LPCSTR Url);

HMENU FindSubMenu(HMENU hMenu, UINT uId);

typedef enum { CFF_RAWSIZE = 0x00, CFF_DPIPT = 0x01, CFF_DPIFROMHWND = 0x02 } CREATEFONTFLAGS;
HFONT CreateFontHelper(INT_PTR Data, int Height, DWORD p1, LPCTSTR Face);
inline HFONT CreateFont(INT_PTR Data, WORD Flags, int Height, WORD Weight, BYTE PitchAndFamily, BYTE CharSet, LPCTSTR Face)
{
  DWORD packed = MAKELONG(MAKEWORD(Weight>>2, Flags), MAKEWORD(CharSet, PitchAndFamily));
  return CreateFontHelper(Data, Height, packed, Face);
}
inline HFONT CreateFontPt(HWND hWndDPI, int Height, WORD Weight, BYTE PitchAndFamily, BYTE CharSet, LPCTSTR Face)
{
  return CreateFont((INT_PTR) hWndDPI, CFF_DPIFROMHWND|CFF_DPIPT, Height, Weight, PitchAndFamily, CharSet, Face);
}
BOOL FontExists(LPCTSTR Face);
BOOL FillRectColor(HDC hDC, const RECT &Rect, COLORREF Color);
BOOL DrawHorzGradient(HDC hDC, LONG l, LONG t, LONG r, LONG b, COLORREF c1, COLORREF c2);
inline long RectW(const RECT&r) { return r.right - r.left; }
inline long RectH(const RECT&r) { return r.bottom - r.top; }
long DlgUnitToPixelX(HWND hDlg, long x);
long DlgUnitToPixelY(HWND hDlg, long y);
UINT DpiGetForMonitor(HWND hWnd);
UINT DpiGetForWindow(HWND hWnd);
int DpiScaleY(HWND hWnd, int Val);

void DrawGripper(HWND hWnd, HDC hDC, const RECT&r);
static inline void GetGripperPos(HWND hwnd, RECT&r)
{
  GetClientRect(hwnd, &r);
  r.left = r.right - GetSystemMetrics(SM_CXVSCROLL);
  r.top = r.bottom - GetSystemMetrics(SM_CYVSCROLL);
}

bool RicheditHasSelection(HWND hRE);

void EnableUICommand(UINT Id, INT_PTR Enabled);

#endif
