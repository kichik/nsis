// Copyright (C) 2018-2021 Anders Kjersem
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
#define InitializeApiFuncEx(mn, fn, ptr) ( (FARPROC&)(g_##ptr) = GetSysProcAddr((mn), (#fn)) )
#define InitializeApiFunc(mn, fn) ( (FARPROC&)(g_##fn) = GetSysProcAddr((mn), (#fn)) )
#define CallApiFunc(fn) ( g_##fn )
#define HasApiFunc(fn) ( !!(g_##fn) )

INT_PTR(WINAPI*g_SetThreadDpiAwarenessContext)(INT_PTR);
UINT(WINAPI*g_GetDpiForWindow)(HWND);
BOOL(WINAPI*g_LogicalToPhysicalPoint)(HWND hWnd, LPPOINT lpPoint);
BOOL(WINAPI*g_PhysicalToLogicalPoint)(HWND hWnd, LPPOINT lpPoint);

typedef struct _DPI {
  enum { a_invalid = -1, a_unaware = 0, a_system = 1, a_pm = 2 };
  enum { ac_invalid = 0, ac_unaware = -1, ac_system = -2, ac_pm1 = -3, ac_pm2 = -4, ac_count = ac_pm2 * -1 };
  static inline INT_PTR SetThreadDpiAwarenessContext(INT_PTR a1) { return g_SetThreadDpiAwarenessContext(a1); }
  static inline UINT GetDpiForWindow(HWND a1) { return HasApiFunc(GetDpiForWindow) ? CallApiFunc(GetDpiForWindow)(a1) : 0; }
  static inline UINT GetDpiForMonitor(HWND hWnd) { return DpiGetForMonitor(hWnd); }
} DPI;

static INT_PTR WINAPI Compat_SetThreadDpiAwarenessContext(INT_PTR AC) { return 0; }
static BOOL WINAPI Compat_LogicalToPhysicalPoint(HWND hWnd, LPPOINT lpPoint) { return TRUE; }
static BOOL WINAPI Compat_PhysicalToLogicalPoint(HWND hWnd, LPPOINT lpPoint) { return TRUE; }

static void InitializeDpiApi()
{
  InitializeApiFuncWithFallback("USER32", SetThreadDpiAwarenessContext);
  InitializeApiFunc("USER32", GetDpiForWindow);
  InitializeApiFuncEx("USER32", LogicalToPhysicalPointForPerMonitorDPI, LogicalToPhysicalPoint);
  if (!HasApiFunc(LogicalToPhysicalPoint)) InitializeApiFuncWithFallback("USER32", LogicalToPhysicalPoint);
  InitializeApiFuncEx("USER32", PhysicalToLogicalPointForPerMonitorDPI, PhysicalToLogicalPoint);
  if (!HasApiFunc(PhysicalToLogicalPoint)) InitializeApiFuncWithFallback("USER32", PhysicalToLogicalPoint);
}

static BOOL LogicalToPhysical(HWND hWnd, POINT&Pt) { return CallApiFunc(LogicalToPhysicalPoint)(hWnd, &Pt); }
static BOOL PhysicalToLogical(HWND hWnd, POINT&Pt) { return CallApiFunc(PhysicalToLogicalPoint)(hWnd, &Pt); }

static void LogicalToPhysical(HWND hWnd, RECT&Rect)
{
  POINT *p = (POINT*) &Rect;
  LogicalToPhysical(hWnd, p[0]), LogicalToPhysical(hWnd, p[1]);
}
static void PhysicalToLogical(HWND hWnd, RECT&Rect)
{
  POINT *p = (POINT*) &Rect;
  PhysicalToLogical(hWnd, p[0]), PhysicalToLogical(hWnd, p[1]);
}

static BOOL GetPhysicalWindowRect(HWND hWnd, RECT&Rect, HWND hWndCaller)
{
  BOOL succ = GetWindowRect(hWnd, &Rect);
  LogicalToPhysical(hWndCaller, Rect);
  return succ;
}

static BOOL GetPhysicalClientRect(HWND hWnd, RECT&Rect, HWND hWndCaller)
{
  BOOL succ = GetClientRect(hWnd, &Rect);
  LogicalToPhysical(hWndCaller, Rect);
  return succ;
}

static inline void PhysicalToLogical(HWND hWnd, RECT&Rect, HWND hWndCaller)
{
  PhysicalToLogical(hWndCaller, Rect);
}

static void ConvertLogicalToLogical(RECT&Rect, HWND hSrc, HWND hDst)
{
  LogicalToPhysical(hSrc, Rect), PhysicalToLogical(hDst, Rect);
}

static BOOL GetLogicalWindowRect(HWND hWnd, RECT&Rect, HWND hWndCaller)
{
  BOOL succ = GetWindowRect(hWnd, &Rect);
  if (succ) ConvertLogicalToLogical(Rect, hWndCaller, hWnd);
  return succ;
}

static HWND GetParentWindow(HWND hWnd)
{ 
  HWND r = GetParent(hWnd); // Parent or owner
  return r != GetWindow(hWnd, GW_OWNER) ? r : NULL;
}

static HWND GetAncestorRoot(HWND hWnd)
{
  if (!SupportsWNT4() && !SupportsW95()) return GetAncestor(hWnd, GA_ROOT);
  for (HWND hTmp; (hTmp = GetParentWindow(hWnd)); ) hWnd = hTmp;
  return hWnd;
}

typedef struct _DIALOGDATA {
  BOOL Dragging;
  HWND hWndTarget, hWndOutline;
  int DialogAwarenessContext; // Canonical DPI awareness context
  _DIALOGDATA() : hWndOutline(0) {}
  static struct _DIALOGDATA* Get(HWND hDlg) { return (struct _DIALOGDATA*) GetWindowLongPtr(hDlg, DWLP_USER); }
  static void Set(HWND hDlg, void*pDD) { SetWindowLongPtr(hDlg, DWLP_USER, (LONG_PTR) pDD); }
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

static LRESULT CALLBACK OutlineWindowProc(HWND hWnd, UINT Msg, WPARAM WParam, LPARAM LParam)
{
  WNDPROC orgProc = (WNDPROC) GetWindowLongPtr(hWnd, GWLP_USERDATA);
  switch(Msg)
  {
  case WM_WINDOWPOSCHANGED:
    {
      WINDOWPOS wp = *(WINDOWPOS*) LParam;
      int size = GetSystemMetrics(SM_CXBORDER) * 2, sizeX = size, sizeY = size;
      if (sizeX >= wp.cx) sizeX = 1;
      if (sizeY >= wp.cy) sizeY = 1;
      HRGN hRgn = CreateRectRgn(0, 0, wp.cx, wp.cy);
      HRGN hRgnInner = CreateRectRgn(sizeX, sizeY, wp.cx - sizeX, wp.cy - sizeY);
      if (sizeX * 2 < wp.cx && sizeY * 2 < wp.cy) CombineRgn(hRgn, hRgn, hRgnInner, RGN_XOR);
      DeleteObject(hRgnInner);
      SetWindowRgn(hWnd, hRgn, TRUE);
    }
    return 0;
  case WM_PAINT:
    {
      PAINTSTRUCT ps;
      HDC hDC = BeginPaint(hWnd, &ps);
      FillRectColor(hDC, ps.rcPaint, RGB(255, 0, 255));
      EndPaint(hWnd, &ps);
    }
    return 0;
  }
  return orgProc ? CallWindowProc(orgProc, hWnd, Msg, WParam, LParam) : DefWindowProc(hWnd, Msg, WParam, LParam);
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
  GetLogicalWindowRect(hWnd, rw, hDlg);
  wsprintf(buf, _T("%dx%d..%dx%d \x2248 %dx%d"), rw.left, rw.top, rw.right, rw.bottom, rw.right - rw.left, rw.bottom - rw.top); // '\x2245' is not present on XP
  SetDlgItemText(hDlg, IDC_WNDLOGISIZE, hWnd ? buf : NULL);

  GetPhysicalWindowRect(hWnd, rw, hDlg);
  GetPhysicalClientRect(hWnd, rc, hDlg);
  wsprintf(buf, _T("%dx%d..%dx%d \x2248 %dx%d (%dx%d)"), rw.left, rw.top, rw.right, rw.bottom, rw.right - rw.left, rw.bottom - rw.top, rc.right - rc.left, rc.bottom - rc.top);
  SetDlgItemText(hDlg, IDC_WNDPHYSSIZE, hWnd ? buf : NULL);

  *buf = _T('\0');
  if (IsWindowUnicode(hWnd)) lstrcat(buf, _T("Unicode"));
  if (IsWindowVisible(hWnd)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Visible")); // IsWindowVisible is not exactly the same as WS_VISIBLE
  if (!(style & WS_DISABLED)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Enabled"));
  if (GetWindow(hWnd, GW_OWNER)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Owned"));
  if (IsHung(hWnd)) lstrcat(*buf ? lstrcat(buf, _T(", ")) : buf, _T("Hung"));
  SetDlgItemText(hDlg, IDC_WNDINFO, hWnd ? buf : NULL);

  UINT dpi = DPI::GetDpiForWindow(hWnd), mondpi = DPI::GetDpiForMonitor(hWnd);
#ifndef _M_ARM64 // Not i386, AMD64 nor ARM(32-bit)
  if (!dpi)
  {
    OSVERSIONINFO osv;
    GetVersionEx((osv.dwOSVersionInfoSize = sizeof(osv), &osv));
    if (MAKELONG(osv.dwMinorVersion, osv.dwMajorVersion) < MAKELONG(3, 6))
      dpi = mondpi; // <= 8.0 has the same DPI on all monitors (blogs.windows.com/windowsexperience/2013/07/15/windows-8-1-dpi-scaling-enhancements/)
  }
#endif
  UINT cch = wsprintf(buf, dpi ? _T("%u") : _T("?"), dpi);
  if ((DpiAwarePerMonitor() || dpi) && dpi != mondpi) wsprintf(buf + cch, _T(" (on %u)"), mondpi); // Don't display on >= 8.1 && < 10FU1607 (because we are not PMv1). GetDpiForWindow will also fail on those systems.
  SetDlgItemText(hDlg, IDC_WNDDPI, hWnd ? buf : NULL);
}

static INT_PTR CALLBACK SpyDlgProc(HWND hDlg, UINT Msg, WPARAM WParam, LPARAM LParam)
{
  enum { TID_OUTLINE = 1 };
  DIALOGDATA*pDD = DIALOGDATA::Get(hDlg);
  switch(Msg)
  {
  case WM_INITDIALOG:
    DIALOGDATA::Set(hDlg, (pDD = (DIALOGDATA*) LParam));
    CenterOnParent(hDlg);
    // On >= 10FU1703 we are PMv2 and Windows scales our dialog and child controls.
    // On >= 10FU1607 && < 10FU1703 we are System aware but try to upgrade this thread to 
    // PMv1 to reduce compatibility handling in other USER32 functions. We don't want 
    // the dialog HWND to be PMv1 because we are not going to manually reposition our controls.
    // On < 10FU1607 we only have the process awareness (not ideal).
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
      if (GetAncestorRoot(hWnd) == hDlg)
        hWnd = 0;

      ShowWindowInfo(hDlg, hWnd);
    }
    break;
  case WM_LBUTTONUP:
    if (pDD->Dragging && pDD->hWndTarget)
    {
      if (!pDD->hWndOutline)
      {
        pDD->hWndOutline = CreateWindowEx(WS_EX_TOOLWINDOW|WS_EX_TOPMOST, WC_STATIC, NULL, WS_POPUP|WS_DISABLED|SS_BLACKRECT, 0, 0, 0, 0, hDlg, 0, 0, 0);
        SetWindowLongPtr(pDD->hWndOutline, GWLP_USERDATA, SetWindowLongPtr(pDD->hWndOutline, GWLP_WNDPROC, (LONG_PTR) OutlineWindowProc));
      }
      SetWindowPos(pDD->hWndOutline, HWND_BOTTOM, -32767, -32767, 1, 1, SWP_SHOWWINDOW|SWP_NOCOPYBITS|SWP_NOACTIVATE|SWP_NOOWNERZORDER); // MSDN says LogicalToPhysicalPoint requires a visible window
      RECT r;
      GetPhysicalWindowRect(pDD->hWndTarget, r, hDlg), PhysicalToLogical(pDD->hWndOutline, r, hDlg);
      if (GetAncestorRoot(pDD->hWndTarget) != hDlg)
      {
        SetWindowPos(pDD->hWndOutline, HWND_TOPMOST, r.left, r.top, r.right - r.left, r.bottom - r.top, SWP_HIDEWINDOW|SWP_NOCOPYBITS|SWP_NOACTIVATE|SWP_NOOWNERZORDER);
        ShowWindow(pDD->hWndOutline, SW_SHOW); // To avoid a small Windows redraw bug, don't show the window until after it has the correct size
      }
      SetTimer(hDlg, TID_OUTLINE, 2 * 1000, NULL);
    }
    ReleaseCapture();
    break;
  case WM_CAPTURECHANGED:
    SetFocus(GetDlgItem(hDlg, IDC_SPYDRAG));
    pDD->Dragging = 0;
    SetDragSourceImage(hDlg, FALSE);
    break;
  case WM_TIMER:
    if (WParam == TID_OUTLINE)
    {
      KillTimer(hDlg, WParam);
      ShowWindow(pDD->hWndOutline, SW_HIDE);
    }
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
    for (UINT s = DPI::ac_count, list = ACList.GetBits(); (m_AC = -(int)s); --s)
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

INT_PTR ShowWndSpy(HWND hOwner)
{
  InitializeDpiApi();
  ScopedThreadDpiAwarenessContext::List aclist;
  ScopedThreadDpiAwarenessContext stdac(aclist << DPI::ac_pm2 << DPI::ac_system);
  DIALOGDATA dd;
  dd.DialogAwarenessContext = stdac.GetCanonicalActiveAwarenessContext();
  return DialogBoxParam(HINST_APPLICATION, MAKEINTRESOURCE(DLG_WNDSPY), hOwner, SpyDlgProc, (LPARAM) &dd);
}
