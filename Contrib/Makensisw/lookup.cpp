// Copyright (C) 2020-2021 Anders Kjersem
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
#include <shlobj.h>
#include <shlwapi.h>
#include "utils.h"
#include "resource.h"

enum { reg_mui_string_truncate = 0x01 };

static void StrTToW(LPCTSTR Src, LPWSTR Dst, UINT cch)
{
  if (sizeof(*Src) == sizeof(*Dst))
    lstrcpynW(Dst, (LPCWSTR) Src, cch);
  else
    MultiByteToWideChar(CP_ACP, 0, (LPCSTR) Src, -1, Dst, cch);
}

template<class T> static LPWSTR SmartStrTToW(T*Src, LPWSTR Dst, UINT cch)
{
  if (sizeof(T) == sizeof(*Dst)) return (LPWSTR) Src;
  return (StrTToW(Src, Dst, cch), Dst);
}

#define StrBase10ToSInt(s) StrToSInt((s), 10)
template<class T> static int WINAPI StrToSInt(const T*Str, UINT Base = 0)
{
  if (Base && !(*Str >= '0' && *Str <= '9') && *Str != '-') return 0; // Don't allow leading space
  int v, succ;
  if (sizeof(*Str) > 1)
    succ = StrToIntExW((WCHAR*) Str, Base != 10 ? STIF_SUPPORT_HEX : STIF_DEFAULT, &v);
  else
    succ = StrToIntExA((CHAR *) Str, Base != 10 ? STIF_SUPPORT_HEX : STIF_DEFAULT, &v);
  return succ ? v : 0; // Not full base support, we only need 10 and 16
}

template<class T> static ULARGE_INTEGER PathParseIconLocationEx(T*Path)
{
  ULARGE_INTEGER li;
  int idx = 0;
  SIZE_T i, comma = 0;
  for (i = 0; Path[i]; ++i)
  {
    if (Path[i] == ',') comma = i;
    if (FSPath::IsAgnosticSeparator(Path[i])) comma = 0;
  }
  if (comma)
  {
    Path[comma] = T('\0');
    idx = StrBase10ToSInt(Path + comma + 1);
  }
  return (li.HighPart = (UINT) comma, li.LowPart = idx, li);
}

static int WINAPI PathParseIconLocationFallback(LPSTR Path)
{
  ULARGE_INTEGER li = PathParseIconLocationEx(Path);
  return li.LowPart;
}

static HRESULT GetSpecialFolderPath(HWND hWnd, LPTSTR Buf, UINT csidl)
{
  UINT create = csidl & CSIDL_FLAG_CREATE, succ, ec;
  csidl &= ~CSIDL_FLAG_CREATE;
  if (SupportsWNT4() || SupportsW95())
  {
    LPITEMIDLIST pidl;
    HRESULT hr = SHGetSpecialFolderLocation(hWnd, csidl, &pidl);
    if (SUCCEEDED(hr))
    {
      hr = SHGetPathFromIDList(pidl, Buf) ? S_OK : E_FAIL, CoTaskMemFree(pidl);
      if (SUCCEEDED(hr) && create)
      {
        succ = CreateDirectory(Buf, NULL);
        hr = (succ || (ec = GetLastError()) == ERROR_ALREADY_EXISTS) ? S_OK : HRESULT_FROM_WIN32(ec);
      }
    }
    return hr;
  }
  succ = SHGetSpecialFolderPath(hWnd, Buf, csidl, false);
  if (!succ && create)  succ = SHGetSpecialFolderPath(hWnd, Buf, csidl, create);
  return succ ? S_OK : E_FAIL;
}

static HICON LoadIconFromLocation(LPCTSTR Path, int Idx)
{
  // web.archive.org/web/201901/https://blogs.msdn.microsoft.com/oldnewthing/20100505-00/?p=14153# -1 is special
  HICON hIco = ExtractIcon(NULL, Path, Idx == -1 ? 0 : Idx);
  if (!hIco && !GetLastError()) SetLastError(ERROR_NOT_FOUND);
  return hIco;
}

template<class T> static void AppendText(HWND hEdit, const T*Str)
{
  SNDMSG(hEdit, EM_SETSEL, 0, -1), SNDMSG(hEdit, EM_SETSEL, -1, -1);
  if (sizeof(T) > 1)
    SendMessageW(hEdit, EM_REPLACESEL, 0, (SIZE_T) Str);
  else
    SendMessageA(hEdit, EM_REPLACESEL, 0, (SIZE_T) Str);
}

static int AddComboStringWithData(HWND hWnd, LPCSTR Str, SIZE_T Data)
{
  int idx = (int) SendMessageA(hWnd, CB_INSERTSTRING, (INT_PTR) -1, (SIZE_T) const_cast<LPCSTR>(Str));
  if (idx >= 0) SendMessage(hWnd, CB_SETITEMDATA, idx, Data);
  return idx;
}

template<class I, class O> static HKEY ParseRegPath(I*In, O*&Path, O*&Name)
{
  HKEY hKey = GetRegRootKey(In);
  I*pp = 0, *pn = 0;
  if (hKey)
  {
    SIZE_T i;
    for (i = 0; In[i]; ++i)
      if (In[i] == I('\\'))
        (!pp ? pp : pn) = &In[i + 1]; // First separator after HK* or last separator
    if (pn) pn[-1] = I('\0'); // Split path and name
  }
  return (Path = const_cast<O*>(pp), Name = const_cast<O*>(pn), hKey);
}

HRESULT (WINAPI*g_RLMSOld)(HKEY hKey, LPCWSTR pszValue, LPWSTR pszOutBuf, DWORD cbOutBuf);

static LONG WINAPI RegLoadMUIStringFallbackW(HKEY hKey, LPCWSTR Name, LPWSTR Out, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCSTR pszDirectory)
{
  if ((Flags & reg_mui_string_truncate) && pcbData) return ERROR_INVALID_PARAMETER;
  if (g_RLMSOld)
  {
    HRESULT hr = g_RLMSOld(hKey, Name, Out, cbOutBuf);
    if (SUCCEEDED(hr))
    {
      if (pcbData) *pcbData = (StrLenT(Out) + 1) * sizeof(*Out);
      return ERROR_SUCCESS;
    }
    if (pcbData) *pcbData = (cbOutBuf|1) * 2; // We have no real value to set, caller just has to loop until success
    return hr; // Note: Not converting E_* here, FormatMessage knows what to do.
  }
  return ERROR_NOT_SUPPORTED;
}

enum { LM_SHGLN, LM_SHLIS, LM_RLMS, LM_PPIL, LM_FMTMSG };
LPCSTR g_ModeLbl[] = { "Path:", "Path:", "Registry:", "Path:", "[Path,]Number:" };

struct DIALOGDATA {
  HRESULT (WINAPI*SHLIS)(LPCWSTR s, LPWSTR o, UINT cch, PVOID*ppvReserved);
  HRESULT (WINAPI*SHGLN)(PCWSTR p, PWSTR m, UINT cch, int*rid);
  LONG (WINAPI*RLMS)(HKEY hKey, LPCWSTR pszValue, LPWSTR pszOutBuf, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCSTR pszDirectory);
  int (WINAPI*PPIL)(LPTSTR p);
  void*OrgParentDlgData;
  HWND hMode, hExtra, hOutTxt, hOutIco;

  DIALOGDATA() { extern void* g_ModalDlgData; OrgParentDlgData = g_ModalDlgData, g_ModalDlgData = this; }
  ~DIALOGDATA() { extern void* g_ModalDlgData; g_ModalDlgData = OrgParentDlgData; }
  static DIALOGDATA* Get(HWND hDlg) { return (DIALOGDATA*) g_ModalDlgData; }
  static inline void Set(HWND hDlg, DIALOGDATA*p) { }
};

static INT_PTR CALLBACK LookupDlgProc(HWND hDlg, UINT Msg, WPARAM WParam, LPARAM LParam)
{
  DIALOGDATA*pDD = DIALOGDATA::Get(hDlg);
  int idx = pDD ? (int) SendMessage(pDD->hMode, CB_GETCURSEL, 0, 0) : 0;
  UINT mode = pDD ? (UINT) SendMessage(pDD->hMode, CB_GETITEMDATA, idx, 0) : 0;
  HRESULT hr;
  TCHAR buf[MAX_PATH + 1 + 6]; // Path + ',' + resid

  switch(Msg)
  {
  case WM_INITDIALOG:
    DIALOGDATA::Set(hDlg, (pDD = (DIALOGDATA*) LParam));
    CenterOnParent(hDlg);
    pDD->hMode = GetDlgItem(hDlg, IDC_LUMODE);
    pDD->hExtra = GetDlgItem(hDlg, IDC_LUEXTRATEXT);
    pDD->hOutTxt = GetDlgItem(hDlg, IDC_LUOUTPUTTEXT);
    pDD->hOutIco = CreateWindowEx(WS_EX_TRANSPARENT, _T("STATIC"), 0, WS_CHILD|WS_VISIBLE|SS_ICON|SS_CENTERIMAGE|SS_REALSIZECONTROL, 0, 0, 0, 0, pDD->hOutTxt, 0, 0, 0);
    if (pDD->SHLIS) AddComboStringWithData(pDD->hMode, "SHLoadIndirectString", LM_SHLIS);
    if (pDD->RLMS) AddComboStringWithData(pDD->hMode, "RegLoadMUIString", LM_RLMS);
    if (pDD->SHGLN) AddComboStringWithData(pDD->hMode, "SHGetLocalizedName", LM_SHGLN);
    if (pDD->PPIL) AddComboStringWithData(pDD->hMode, "PathParseIconLocation", LM_PPIL);
    AddComboStringWithData(pDD->hMode, "FormatMessage", LM_FMTMSG);
    SNDMSG(pDD->hExtra, EM_LIMITTEXT, COUNTOF(buf), 0);
    SNDMSG(pDD->hMode, CB_SETCURSEL, (SIZE_T) 0, 0), SNDMSG(hDlg, WM_COMMAND, MAKELONG(IDC_LUMODE, CBN_SELENDOK), (SIZE_T) pDD->hMode);
    return TRUE;
  case WM_CLOSE: close:
    return EndDialog(hDlg, 0);
  case WM_DESTROY:
    {
      HICON hOld = (HICON) SNDMSG(pDD->hOutIco, STM_SETICON, (SIZE_T) NULL, 0);
      if (hOld) DestroyIcon(hOld);
    }
    break;
  case WM_COMMAND:
    switch((UINT) WParam)
    {
    case IDCANCEL:
      goto close;
    case MAKELONG(IDC_LUMODE, CBN_SELENDOK):
      {
        SendMessageA(GetDlgItem(hDlg, IDC_LUEXTRALABEL), WM_SETTEXT, 0, (SIZE_T) const_cast<LPCSTR>(g_ModeLbl[mode]));
        LPCTSTR extra = buf;
        *buf = '\0';
        switch(mode)
        {
        case LM_SHGLN:
          GetSpecialFolderPath(NULL, buf, CSIDL_COMMON_DESKTOPDIRECTORY);
          break;
        case LM_SHLIS:
          extra = _T("@shell32,-4097");
          break;
        case LM_RLMS:
          extra = _T("HKCR\\AudioCD\\shell\\play\\MUIVerb");
          break;
        case LM_PPIL:
          extra = _T("%WINDIR%\\Explorer.exe,-101");
          break;
        case LM_FMTMSG:
          extra = _T("wininet.dll,12005");
          break;
        }
        ShowWindow(pDD->hOutIco, SW_HIDE);
        SetWindowText(pDD->hExtra, extra);
      }
      break;
    case MAKELONG(IDC_LUEXTRATEXT, EN_UPDATE):
      SendMessage(pDD->hExtra, WM_GETTEXT, COUNTOF(buf), (SIZE_T) buf);
      SetWindowText(pDD->hOutTxt, _T(""));
      switch(mode)
      {
      case LM_SHGLN:
        {
          int resid;
          WCHAR path[COUNTOF(buf)], mod[MAX_PATH], *pis;
          pis = SmartStrTToW(buf, path, COUNTOF(path));
          hr = pDD->SHGLN(pis, mod, COUNTOF(mod), &resid);
          if (SUCCEEDED(hr))
          {
            AppendText(pDD->hOutTxt, mod);
            AppendText(pDD->hOutTxt, (wsprintfW(path, L",%d\r\n\r\n", resid), path));
            if (ExpandEnvironmentStringsW(mod, path, COUNTOF(path)))
            {
              if (HMODULE hMod = LoadLibraryExW(path, NULL, LOAD_LIBRARY_AS_DATAFILE))
              {
                if (LoadStringW(hMod, resid, mod, COUNTOF(mod)))
                {
                  AppendText(pDD->hOutTxt, mod);
                }
                FreeLibrary(hMod);
              }
            }
          }
          else die_hr:
          {
            FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS, NULL, hr, 0, buf, COUNTOF(buf), NULL);
            AppendText(pDD->hOutTxt, (wsprintfW(path, L"%#x: ", hr), path));
            AppendText(pDD->hOutTxt, buf);
          }
        }
        break;
      case LM_SHLIS:
        {
          WCHAR is[COUNTOF(buf)], os[MAX_PATH], *pis;
          pis = SmartStrTToW(buf, is, COUNTOF(is));
          hr = pDD->SHLIS(pis, os, COUNTOF(os), NULL);
          if (FAILED(hr)) goto die_hr;
          AppendText(pDD->hOutTxt, os);
        }
        break;
      case LM_RLMS:
        {
          UINT ec;
          TCHAR *pkey, *pname;
          if (HKEY hKey = ParseRegPath(buf, pkey, pname))
          {
            if (!(ec = RegOpenKeyForReading(hKey, pkey, &hKey)))
            {
              WCHAR namebuf[255+!0], outbuf[MAX_PATH], *pwn = SmartStrTToW(pname, namebuf, COUNTOF(namebuf));
              ec = pDD->RLMS(hKey, pwn, outbuf, sizeof(outbuf), NULL, reg_mui_string_truncate, NULL);
              if (!ec) AppendText(pDD->hOutTxt, outbuf);
              RegCloseKey(hKey);
            }
            if (FAILED(hr = HRESULT_FROM_WIN32(ec))) goto die_hr;
          }
        }
        break;
      case LM_PPIL:
        {
          RECT r;
          int idx = pDD->PPIL(buf);
          HICON hIco = LoadIconFromLocation(buf, idx), hOld;
          hr = GetLastError();
          GetClientRect(pDD->hOutTxt, &r);
          SetWindowPos(pDD->hOutIco, HWND_TOP, 0, 0, r.right, r.bottom, SWP_NOACTIVATE|(hIco ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
          hOld = (HICON) SNDMSG(pDD->hOutIco, STM_SETICON, (SIZE_T) hIco, 0);
          if (hOld) DestroyIcon(hOld);
          if (!hIco) goto die_hr;
        }
        break;
      case LM_FMTMSG:
        {
          UINT flags = FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS;
          ULARGE_INTEGER li = PathParseIconLocationEx(buf);
          LPCTSTR numstr = buf;
          HMODULE hMod = 0;
          if (li.HighPart)
          {
            numstr = buf + ++li.HighPart;
            hMod = LoadLibraryEx(buf, 0, LOAD_LIBRARY_AS_DATAFILE), flags |= FORMAT_MESSAGE_FROM_HMODULE;
            if (!hMod) goto badmsgmod;
          }
          hr = StrToSInt(numstr);
          if (!FormatMessage(flags, hMod, hr, 0, buf, COUNTOF(buf), NULL)) badmsgmod:
          {
            hr = GetLastError();
            goto die_hr;
          }
          if (hMod) FreeLibrary(hMod);
          SetWindowText(pDD->hOutTxt, buf);
        }
        break;
      }
      break;
    }
    break;
  }
  return FALSE;
}

INT_PTR ShowLookupDialog(HWND hOwner)
{
  DIALOGDATA dd;
  (FARPROC&) dd.SHLIS = GetSysProcAddr("SHLWAPI", "SHLoadIndirectString"); // WXP+
  (FARPROC&) dd.SHGLN = GetSysProcAddr("SHELL32", "SHGetLocalizedName"); // WVista+
  (FARPROC&) dd.RLMS = GetSysProcAddr("ADVAPI32", "RegLoadMUIStringW"); // WVista+ Note: RegLoadMUIStringA always returns ERROR_CALL_NOT_IMPLEMENTED
  if (!dd.RLMS && ((FARPROC&) g_RLMSOld = GetSysProcAddr("SHLWAPI", (LPCSTR) 439))) dd.RLMS = RegLoadMUIStringFallbackW; // W98SE+,IE5+
  (FARPROC&) dd.PPIL = 
  #ifdef _WIN64
    (FARPROC) PathParseIconLocation;
  #else
    GetSysProcAddr("SHLWAPI", sizeof(TCHAR) == 1 ? "PathParseIconLocationA" : "PathParseIconLocationW"); // W95OSR2+,IE3.1+
  if (!dd.PPIL || (SupportsWNT4() || IsWin9598ME()))
  {
    (FARPROC&) dd.PPIL = GetSysProcAddr("SHELL32", (LPCSTR) 249); // WNT4+ PathParseIconLocationT
    if (sizeof(TCHAR) == 1) (FARPROC&) dd.PPIL = (FARPROC) PathParseIconLocationFallback;
  }
  #endif

  return DialogBoxParam(HINST_APPLICATION, MAKEINTRESOURCE(DLG_LOOKUP), hOwner, LookupDlgProc, (LPARAM) &dd);
}