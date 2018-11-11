// Copyright (C) 2018 Anders Kjersem
//
// This file is a part of NSIS.
//
// Licensed under the zlib/libpng license (the "License");
// you may not use this file except in compliance with the License.
//
// Licence details can be found in the file COPYING.
//
// This software is provided 'as-is', without any express or implied
// warranty.

#include "makensisw.h"
#include <windows.h>
#include <windowsx.h>
#include "utils.h"
#include "resource.h"

#define InitializeApiFuncWithFallback(mn, fn) { FARPROC f =  GetSysProcAddr((mn), (#fn)); g_##fn = Compat_##fn; if (f) (FARPROC&) g_##fn = f; }
#define InitializeApiFunc(mn, fn) ( (FARPROC&)(g_##fn) = GetSysProcAddr((mn), (#fn)) )
#define CallApiFunc(fn) ( g_##fn )
#define HasApiFunc(fn) ( !!(g_##fn) )

INT_PTR(WINAPI*g_SetThreadDpiAwarenessContext)(INT_PTR);
UINT(WINAPI*g_GetDpiForWindow)(HWND);

typedef struct _DPI {
  enum { a_invalid = -1, a_unaware = 0, a_system = 1, a_pm = 2 };
  enum { ac_invalid = 0, ac_unaware = -1, ac_system = -2, ac_pm1 = -3, ac_pm2 = -4 };
  static inline INT_PTR SetThreadDpiAwarenessContext(INT_PTR a1) { return g_SetThreadDpiAwarenessContext(a1); }
  static inline UINT GetDpiForWindow(HWND a1) { return HasApiFunc(GetDpiForWindow) ? CallApiFunc(GetDpiForWindow)(a1) : 0; }
} DPI;

static INT_PTR WINAPI Compat_SetThreadDpiAwarenessContext(INT_PTR AC)
{
  return 0;
}

static void InitializeDpiApi()
{
  InitializeApiFuncWithFallback("USER32", SetThreadDpiAwarenessContext);
  InitializeApiFunc("USER32", GetDpiForWindow);
}

static HWND GetParentWindow(HWND hWnd)
{
  HWND hParent = GetAncestor(hWnd, GA_PARENT); // Parent but NOT owner.
  return hParent == GetParent(hWnd) ? hParent : NULL; // Filter away GetDesktopWindow().
}

typedef struct _DIALOGDATA {
  BOOL Dragging;
  HWND hWndTarget;
  int DialogAwarenessContext;
  static struct _DIALOGDATA* Get(HWND hDlg) { return (struct _DIALOGDATA*) GetWindowLongPtr(hDlg, DWLP_USER); }
} DIALOGDATA;

typedef struct {
  HWND hWnd;
  POINT pt;
  ULONG Area;
  bool IncludeHidden;
  void Init(POINT pt, bool IncludeHidden) { Area = 0, Area = ~Area, this->pt = pt, this->IncludeHidden = IncludeHidden; }
} FINDCHILDDATA;

static BOOL CALLBACK FindChildWindowFromPointProc(HWND hWnd, LPARAM LParam)
{
  RECT r;
  FINDCHILDDATA*pFCD = (FINDCHILDDATA*) LParam;
  if (GetWindowRect(hWnd, &r) && PtInRect(&r, pFCD->pt)) // TODO: Region?
  {
    ULONG area = (r.right - r.left) * (r.bottom - r.top);
    if (area < pFCD->Area && (pFCD->IncludeHidden || IsWindowVisible(hWnd)))
      pFCD->Area = area, pFCD->hWnd = hWnd;
  }
  return TRUE;
}

static HWND FindChildWindowFromPoint(HWND hWnd, FINDCHILDDATA*pFCD)
{
  HWND hParent = GetParentWindow(hWnd), hOrg;
  if (!hParent)
    hParent = hWnd;
recurse:
  hOrg = pFCD->hWnd = hWnd;
  EnumChildWindows(hParent, FindChildWindowFromPointProc, (LPARAM) pFCD);
  if (hOrg && hOrg != pFCD->hWnd)
  {
    hWnd = hParent = pFCD->hWnd;
    goto recurse;
  }
  return pFCD->hWnd;
}

static HWND GetChildWindowFromPointHelper(POINT pt)
{
  const bool includeHidden = false;
  HWND hWnd = WindowFromPoint(pt), hWnd2;
  if (!hWnd)
    return hWnd;
  while(!includeHidden && hWnd && !IsWindowVisible(hWnd))
    if ((hWnd2 = GetParentWindow(hWnd)))
      hWnd = hWnd2;

  FINDCHILDDATA fcd;
  fcd.Init(pt, includeHidden);
  hWnd = FindChildWindowFromPoint(hWnd, &fcd);
  return hWnd;
}

#if defined(__MINGW32_MAJOR_VERSION) && !defined(__MINGW64_VERSION_MAJOR) && (!defined(_WIN32_WINNT) || (_WIN32_WINNT < 0x0500))
WINUSERAPI BOOL WINAPI IsHungAppWindow(HWND); // MinGW is wrong, IsHungAppWindow was added in WinNT4. MinGW < 3.20? does not even have it in their .lib!
#endif
static BOOL IsHung(HWND hWnd)
{
#if !(defined(__MINGW32_MAJOR_VERSION) && !defined(__MINGW64_VERSION_MAJOR) && (__MINGW32_MINOR_VERSION <= 15))
  if (sizeof(void*) > 4 || sizeof(TCHAR) > 1)
  {
    return IsHungAppWindow(hWnd);
  }
  else
#endif
  {
    static FARPROC g_func = GetSysProcAddr("USER32", "IsHungAppWindow");
    if (g_func) return ((BOOL(WINAPI*)(HWND))g_func)(hWnd);
    DWORD_PTR mr;
    LRESULT rv = SendMessageTimeout(hWnd, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, 500, &mr);
    return rv == 0;
  }
}

static void SetDragSourceImage(HWND hDlg, INT_PTR Dragging = false)
{
  HCURSOR hCur = Dragging ? NULL : LoadCursor(NULL, IDC_CROSS);
  SendDlgItemMessage(hDlg, IDC_SPYDRAG, STM_SETIMAGE, IMAGE_CURSOR, (LPARAM) hCur);
}

static void ShowWindowInfo(HWND hDlg, HWND hWnd)
{
  const TCHAR *strFmtPtr = sizeof(void*) > 4 ? _T("%#.16I64x") : _T("%#.8x");
  const TCHAR *strFmtPtr2 = sizeof(void*) > 4 ? _T("%#.16I64x & %#.16I64x") : _T("%#.8x & %#.8x");
  const TCHAR *strIntPtr = sizeof(void*) > 4 ? _T("%I64d") : _T("%d");
  TCHAR buf[300];
  LONG_PTR style = GetWindowLongPtr(hWnd, GWL_STYLE);

  wsprintf(buf, strIntPtr, GetWindowLongPtr(hWnd, GWLP_ID));
  SetDlgItemText(hDlg, IDC_WNDID, buf);

  wsprintf(buf, strFmtPtr, hWnd);
  SetDlgItemText(hDlg, IDC_HWND, buf);

  if (!GetClassName(hWnd, buf, COUNTOF(buf))) *buf = _T('\0');
  SetDlgItemText(hDlg, IDC_WNDCLASS, buf);

  wsprintf(buf, strFmtPtr2, GetWindowLongPtr(hWnd, GWLP_USERDATA), GetWindowLongPtr(hWnd, DWLP_USER));
  SetDlgItemText(hDlg, IDC_WNDUSERDATA, buf);

  wsprintf(buf, strFmtPtr2, (LONG_PTR) style, GetWindowLongPtr(hWnd, GWL_EXSTYLE));
  SetDlgItemText(hDlg, IDC_WNDSTYLE, buf);

  RECT rw, rc;
  GetWindowRect(hWnd, &rw), GetClientRect(hWnd, &rc);
  wsprintf(buf, _T("%dx%d..%dx%d \x2248 %dx%d (%dx%d)"), rw.left, rw.top, rw.right, rw.bottom, rw.right - rw.left, rw.bottom - rw.top, rc.right - rc.left, rc.bottom - rc.top); // '\x2245' is not present on XP
  SetDlgItemText(hDlg, IDC_WNDSIZE, hWnd ? buf : NULL);

  *buf = _T('\0');
  if (IsWindowUnicode(hWnd)) lstrcat(buf, _T("Unicode"));
  if (IsWindowVisible(hWnd)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Visible")); // IsWindowVisible is not exactly the same as WS_VISIBLE
  if (!(style & WS_DISABLED)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Enabled"));
  if (IsHung(hWnd)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Hung"));
  SetDlgItemText(hDlg, IDC_WNDINFO, hWnd ? buf : NULL);

  UINT dpi = DPI::GetDpiForWindow(hWnd);
  wsprintf(buf, dpi ? _T("%u") : _T("?"), dpi);
  SetDlgItemText(hDlg, IDC_WNDDPI, hWnd ? buf : NULL);
}

static INT_PTR CALLBACK SpyDlgProc(HWND hDlg, UINT Msg, WPARAM WParam, LPARAM LParam)
{
  DIALOGDATA*pDD = DIALOGDATA::Get(hDlg);
  switch(Msg)
  {
  case WM_INITDIALOG:
    SetWindowLongPtr(hDlg, DWLP_USER, (LONG_PTR) (pDD = (DIALOGDATA*) LParam));
    CenterOnParent(hDlg);
    // On >= 10.1703 we are PMv2 and Windows scales our dialog and child controls.
    // On >= 10.1607 && < 10.1703 we are System aware but try to upgrade this thread to 
    // PMv1 to reduce compatibility handling in other USER32 functions. We don't want 
    // the dialog HWND to be PMv1 because we are not going to reposition our controls.
    // On < 10.1607 we only have the process awareness (not ideal).
    if (pDD->DialogAwarenessContext > DPI::ac_pm2) // Is the dialog AC < PMv2? Note: The canonical AC numbers are negative.
      DPI::SetThreadDpiAwarenessContext(DPI::ac_pm1);
    SendMessage(hDlg, WM_CAPTURECHANGED, 0, 0);
    break;
  case WM_CLOSE: close:
    return EndDialog(hDlg, 0);
  case WM_COMMAND:
    switch(WParam)
    {
    case IDCANCEL:
      goto close;
    case MAKELONG(IDC_SPYDRAG, STN_CLICKED):
      SetCapture(hDlg);
      pDD->hWndTarget = 0;
      pDD->Dragging++;
      SetDragSourceImage(hDlg, (INT_PTR) hDlg);
      SetCursor(LoadCursor(NULL, IDC_CROSS));
      break;
    }
    break;
  case WM_MOUSEMOVE:
    if (pDD->Dragging)
    {
      POINT pt;
      GetCursorPos(&pt);
      HWND hWnd = GetChildWindowFromPointHelper(pt);
      if (hWnd == pDD->hWndTarget)
        break;

      pDD->hWndTarget = hWnd;
      if (GetAncestor(hWnd, GA_ROOT) == hDlg)
        hWnd = 0;

      ShowWindowInfo(hDlg, hWnd);
    }
    break;
  case WM_LBUTTONUP:
    ReleaseCapture();
    break;
  case WM_CAPTURECHANGED:
    SetFocus(GetDlgItem(hDlg, IDC_SPYDRAG));
    pDD->Dragging = 0;
    SetDragSourceImage(hDlg, FALSE);
    break;
  }
  return FALSE;
}

struct ScopedThreadDpiAwarenessContext { // Note: Assumes InitializeDpiApi() has been called!
  struct List {
    List() : m_l(0) {}
    List operator <<(INT_PTR in) { List r = *this; r.m_l |= (1 << (in < 0 ? -in : in)); return r; }
    UINT GetBits() const { return m_l; }
    UINT m_l;
  };
  ScopedThreadDpiAwarenessContext(List ACList) : m_OrgAC(0)
  {
    for (UINT s = 4, list = ACList.GetBits(); (m_AC = -(int)s); --s)
      if ((1 << s) & list)
        if ((m_OrgAC = DPI::SetThreadDpiAwarenessContext((INT_PTR) m_AC)))
          break;
  }
  ~ScopedThreadDpiAwarenessContext()
  {
    DPI::SetThreadDpiAwarenessContext(m_OrgAC);
  }
  int GetCanonicalActiveAwarenessContext() const { return m_AC; }
  INT_PTR m_OrgAC;
  int m_AC; // Canonical "active" DPI_AWARENESS_CONTEXT
};

int ShowWndSpy(HWND hOwner)
{
  InitializeDpiApi();
  ScopedThreadDpiAwarenessContext::List aclist;
  ScopedThreadDpiAwarenessContext stdac(aclist << DPI::ac_pm2 << DPI::ac_system);
  DIALOGDATA dd;
  dd.DialogAwarenessContext = stdac.GetCanonicalActiveAwarenessContext();
  return DialogBoxParam(GetModuleHandle(NULL), MAKEINTRESOURCE(DLG_WNDSPY), hOwner, SpyDlgProc, (LPARAM) &dd);
}
