/*
  Copyright (c) 2003 Sunil Kamath
  Modified by Joost Verburg

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
#define TOOLBAR_CPP

#include "makensisw.h"
#include "resource.h"
#include "toolbar.h"

NTOOLBAR g_toolbar;
extern NSCRIPTDATA g_sdata;
extern NTOOLTIP g_tip;

typedef struct {
  BYTE Style, State;
  BYTE ImgIdx;
  BYTE TTip;
  WORD CmdId;
} TBBTNDESC;
#define MKNAMEDTBBTNDESC(id, sta, sty) sty, sta, IDB_##id, IDS_##id, IDM_##id

static const TBBTNDESC g_TBBtnsDesc[BUTTONCOUNT] = {
/*TBB_LOADSCRIPT*/ { MKNAMEDTBBTNDESC(LOADSCRIPT, TBSTATE_ENABLED,       TBSTYLE_BUTTON  ) },
/*TBB_SAVE      */ { MKNAMEDTBBTNDESC(SAVE,       TBSTATE_INDETERMINATE, TBSTYLE_BUTTON  ) },
/*TBB_SEP1      */ { TBSTYLE_SEP },
/*TBB_COPY      */ { MKNAMEDTBBTNDESC(COPY,       TBSTATE_ENABLED,       TBSTYLE_BUTTON  ) },
/*TBB_FIND      */ { MKNAMEDTBBTNDESC(FIND,       TBSTATE_ENABLED,       TBSTYLE_BUTTON  ) },
/*TBB_CLEARLOG  */ { MKNAMEDTBBTNDESC(CLEARLOG,   TBSTATE_ENABLED,       TBSTYLE_BUTTON  ) },
/*TBB_SEP2      */ { TBSTYLE_SEP },
/*TBB_RECOMPILE */ { MKNAMEDTBBTNDESC(RECOMPILE,  TBSTATE_INDETERMINATE, TBSTYLE_BUTTON  ) },
/*TBB_TEST      */ { MKNAMEDTBBTNDESC(TEST,       TBSTATE_INDETERMINATE, TBSTYLE_BUTTON  ) },
/*TBB_COMPRESSOR*/ { MKNAMEDTBBTNDESC(COMPRESSOR, TBSTATE_ENABLED,       TBSTYLE_DROPDOWN) },
/*TBB_EDITSCRIPT*/ { MKNAMEDTBBTNDESC(EDITSCRIPT, TBSTATE_INDETERMINATE, TBSTYLE_BUTTON  ) },
/*TBB_BROWSESCR */ { MKNAMEDTBBTNDESC(BROWSESCR,  TBSTATE_INDETERMINATE, TBSTYLE_BUTTON  ) }
};

static const BYTE g_TBIL[] = {
/* 16 */ IDB_TOOLBAR16N24, IDB_TOOLBAR16D24, IDB_TOOLBAR16H24,
/* 24 */ IDB_TOOLBAR24N24, IDB_TOOLBAR24D24, IDB_TOOLBAR24H24,
/* 32 */ IDB_TOOLBAR32N24, IDB_TOOLBAR32D24, IDB_TOOLBAR32H24
};

static void LoadToolBarImages();

void CreateToolBar()
{
  g_toolbar.hwnd = CreateWindowEx(0, TOOLBARCLASSNAME, NULL,
    WS_CHILD | WS_VISIBLE | TBSTYLE_TRANSPARENT | TBSTYLE_FLAT,
    0, 0, 0, 0, g_sdata.hwnd, (HMENU) IDC_TOOLBAR, g_sdata.hInstance, NULL);
  
  TBBUTTON tbbs[BUTTONCOUNT];
  SendMessage(g_toolbar.hwnd, TB_BUTTONSTRUCTSIZE, sizeof(tbbs[0]), 0);
  for (UINT i = 0; i < BUTTONCOUNT; ++i)
  {
    tbbs[i].iBitmap = g_TBBtnsDesc[i].ImgIdx;
    tbbs[i].idCommand = g_TBBtnsDesc[i].CmdId;
    tbbs[i].fsState = g_TBBtnsDesc[i].State;
    tbbs[i].fsStyle = g_TBBtnsDesc[i].Style;
    tbbs[i].dwData = 0, tbbs[i].iString = 0;
  }
  SendMessage(g_toolbar.hwnd, TB_ADDBUTTONS, BUTTONCOUNT, (LPARAM) &tbbs);
  LoadToolBarImages();
}

static void LoadToolBarImages()
{
  HWND hTB = g_toolbar.hwnd;
  // Comctl32.dll version detection
#ifndef _WIN64
  HMODULE hMod = GetModuleHandle(_T("comctl32.dll"));
  const FARPROC hasCC4_70 = (SupportsW95()) ? GetProcAddress(hMod, "InitCommonControlsEx") : (FARPROC) TRUE; // NT4 shipped with v4.70
  const FARPROC hasCC4_71 = (SupportsWNT4() || SupportsW95()) ? GetProcAddress(hMod, "DllGetVersion") : (FARPROC) TRUE; // IE4 shipped with v4.71
#else
  const bool hasCC4_70 = true, hasCC4_71 = true;
#endif

  UINT iltypecount = 3, s16 = DpiScaleY(hTB, 16), imgsize, iloffs; // 144dpi(150%)=24 120dpi(125%)=20
  if (s16 > 24)
    imgsize = 32, iloffs = 2 * iltypecount;
  else if (s16 > 16)
    imgsize = 24, iloffs = 1 * iltypecount;
  else
    imgsize = 16, iloffs = 0 * iltypecount;

  if (hasCC4_70)
  {
    // Version 4.70 => Modern toolbar, 24-bit bitmaps
    g_toolbar.imagelist = ImageList_LoadImage(g_sdata.hInstance, MAKEINTRESOURCE(g_TBIL[iloffs+0]), imgsize, 0, RGB(255, 0, 255), IMAGE_BITMAP, LR_CREATEDIBSECTION);
    g_toolbar.imagelistd = ImageList_LoadImage(g_sdata.hInstance, MAKEINTRESOURCE(g_TBIL[iloffs+1]), imgsize, 0, RGB(255, 0, 255), IMAGE_BITMAP, LR_CREATEDIBSECTION);
    g_toolbar.imagelisth = ImageList_LoadImage(g_sdata.hInstance, MAKEINTRESOURCE(g_TBIL[iloffs+2]), imgsize, 0, RGB(255, 0, 255), IMAGE_BITMAP, LR_CREATEDIBSECTION);
    SendMessage(hTB, TB_SETIMAGELIST, 0, (LPARAM) g_toolbar.imagelist);
    SendMessage(hTB, TB_SETDISABLEDIMAGELIST, 0, (LPARAM) g_toolbar.imagelistd);
    SendMessage(hTB, TB_SETHOTIMAGELIST, 0, (LPARAM) g_toolbar.imagelisth);

    if (hasCC4_71)
      SendMessage(hTB, TB_SETEXTENDEDSTYLE, 0, TBSTYLE_EX_DRAWDDARROWS);
  }
  else 
  {
    // Version 4.00 => Old Windows 95 toolbar, 256 color bitmap with system palette
    TBADDBITMAP tbBitmap;
    tbBitmap.hInst = g_sdata.hInstance;
    tbBitmap.nID = IDB_TOOLBAR;
    SendMessage(hTB, TB_ADDBITMAP, IMAGECOUNT, (LPARAM) &tbBitmap);
  }
}

void UpdateToolBarCompressorButton()
{
  int iBitmap, iString;
  TCHAR szBuffer[124]; // increased to 124 for good measure, also.
  TCHAR temp[64]; // increased to 64.  Hit limit 08/20/2007 -- Jim Park.

  if(g_sdata.compressor >= COMPRESSOR_SCRIPT && g_sdata.compressor <= COMPRESSOR_BEST) {
    iBitmap = compressor_bitmaps[(int)g_sdata.compressor];
    iString = compressor_strings[(int)g_sdata.compressor];
  }
  else {
    return;
  }

  LoadString(g_sdata.hInstance, IDS_COMPRESSOR, temp, COUNTOF(temp));
  szBuffer[0] = _T('\0');
  lstrcat(szBuffer,temp);
  lstrcat(szBuffer,_T(" ["));
  LoadString(g_sdata.hInstance, iString, temp, COUNTOF(temp));
  lstrcat(szBuffer,temp);
  lstrcat(szBuffer,_T("]"));

  SendMessage(g_toolbar.hwnd, TB_CHANGEBITMAP, (WPARAM) IDM_COMPRESSOR, (LPARAM) MAKELPARAM(iBitmap, 0));

  TOOLINFO ti = { sizeof(TOOLINFO), 0 };
  ti.hwnd = g_toolbar.hwnd;
  ti.uId = (UINT)TBB_COMPRESSOR;
  ti.hinst = g_sdata.hInstance;
  SendMessage(g_tip.tip, TTM_GETTOOLINFO, 0, (LPARAM) (LPTOOLINFO) &ti);
  ti.lpszText = (LPTSTR)szBuffer;
  SendMessage(g_tip.tip, TTM_SETTOOLINFO, 0, (LPARAM) (LPTOOLINFO) &ti);
}

void AddToolBarButtonTooltip(UINT idx, int iString)
{
  TOOLINFO ti;
  TCHAR   szBuffer[64];
  RECT rect;

  memset(&ti, 0, sizeof(TOOLINFO));
  ti.cbSize = sizeof(TOOLINFO);
  ti.uFlags = 0;
  ti.hwnd = g_toolbar.hwnd;
  ti.hinst = g_sdata.hInstance;
  ti.uId = idx;
  LoadString(g_sdata.hInstance, iString, szBuffer, COUNTOF(szBuffer));
  ti.lpszText = (LPTSTR) szBuffer;
  SendMessage(g_toolbar.hwnd, TB_GETITEMRECT, idx, (LPARAM) &rect);
  ti.rect.left =rect.left;
  ti.rect.top = rect.top;
  ti.rect.right = rect.right;
  ti.rect.bottom = rect.bottom;

  SendMessage(g_tip.tip, TTM_ADDTOOL, 0, (LPARAM) &ti);
}

void AddToolBarTooltips()
{
  for (UINT i = 0; i < BUTTONCOUNT; ++i) {
    int ids = g_TBBtnsDesc[i].TTip;
    if (ids) AddToolBarButtonTooltip(i, ids);
  }
}

void EnableToolBarButton(int cmdid, BOOL enabled)
{
  UINT state = enabled ? TBSTATE_ENABLED : TBSTATE_INDETERMINATE;
  SendMessage(g_toolbar.hwnd, TB_SETSTATE, cmdid, MAKELPARAM(state, 0));
}

static UINT IsRTL(HWND hWnd) { return ((UINT) GetWindowLongPtr(hWnd, GWL_EXSTYLE)) & (WS_EX_LAYOUTRTL); } // WS_EX_RIGHT? WS_EX_RTLREADING?

static UINT GetToolbarDropdownMenuPos(HWND hTB, UINT Id, POINT&pt)
{
  RECT r;
  INT_PTR idx = SendMessage(hTB, TB_COMMANDTOINDEX, Id, 0);
  SendMessage(hTB, TB_GETITEMRECT, idx, (LPARAM) &r);
  MapWindowPoints(hTB, NULL, (POINT*) &r, 2);
  pt.x = IsRTL(hTB) ? r.right : r.left, pt.y = r.bottom;
  return GetSystemMetrics(SM_MENUDROPALIGNMENT) ? TPM_RIGHTALIGN : TPM_LEFTALIGN;
}

static void ShowToolbarDropdownMenu(const NMTOOLBAR&nmtb, HWND hNotifyWnd, HMENU hParentMenu, UINT SubMenuId = -1)
{
  POINT pt;
  HMENU hMenu = SubMenuId == static_cast<UINT>(-1) ? hParentMenu : FindSubMenu(hParentMenu, SubMenuId);
  UINT tpmf = GetToolbarDropdownMenuPos(nmtb.hdr.hwndFrom, nmtb.iItem, pt);
  TrackPopupMenu(hMenu, tpmf, pt.x, pt.y, 0, hNotifyWnd, NULL);
}

void ShowCompressorToolbarDropdownMenu(const NMTOOLBAR&nmtb)
{
  HMENU hParentMenu = FindSubMenu(g_sdata.menu, IDM_SCRIPT);
  ShowToolbarDropdownMenu(nmtb, g_sdata.hwnd, hParentMenu, IDM_COMPRESSOR_SUBMENU);
}
