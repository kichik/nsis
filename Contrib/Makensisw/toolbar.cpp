/*
  Copyright (c) 2003 Sunil Kamath

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

*/
#define TOOLBAR_CPP

#include <windows.h>
#include "resource.h"
#include "makensisw.h"
#include "noclib.h"
#include "toolbar.h"

NTOOLBAR g_toolbar;
extern NSCRIPTDATA g_sdata;
extern NTOOLTIP g_tip;

TBBUTTON CreateToolBarButton(int iBitmap, int idCommand, BYTE fsState, BYTE fsStyle, DWORD dwData, int iString)
{
  TBBUTTON tbButton;
  tbButton.iBitmap = iBitmap;
  tbButton.idCommand = idCommand;
  tbButton.fsState = fsState;
  tbButton.fsStyle = fsStyle;
  tbButton.dwData = dwData;
  tbButton.iString = iString;

  return tbButton;
}

void CreateToolBar()
{
  static TBBUTTON tbButton[BUTTONCOUNT];
  tbButton[TBB_LOADSCRIPT]  = CreateToolBarButton(IDB_LOADSCRIPT,    IDM_LOADSCRIPT, TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_SAVE]        = CreateToolBarButton(IDB_SAVE,          IDM_SAVE,       TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_EXIT]        = CreateToolBarButton(IDB_EXIT,          IDM_EXIT,       TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_SEP1]        = CreateToolBarButton(0,                 0,              TBSTATE_ENABLED,        TBSTYLE_SEP,      0, 0);
  tbButton[TBB_COPY]        = CreateToolBarButton(IDB_COPY,          IDM_COPY,       TBSTATE_INDETERMINATE,  TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_FIND]        = CreateToolBarButton(IDB_FIND,          IDM_FIND,       TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_SEP2]        = CreateToolBarButton(0,                 0,              TBSTATE_ENABLED,        TBSTYLE_SEP,      0, 0);
  tbButton[TBB_SETTINGS]    = CreateToolBarButton(IDB_SETTINGS,      IDM_SETTINGS,   TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_RECOMPILE]   = CreateToolBarButton(IDB_RECOMPILE,     IDM_RECOMPILE,  TBSTATE_INDETERMINATE,  TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_COMPRESSOR]  = CreateToolBarButton(IDB_COMPRESSOR,    IDM_COMPRESSOR, TBSTATE_ENABLED,        TBSTYLE_DROPDOWN, 0, 0);
  tbButton[TBB_TEST]        = CreateToolBarButton(IDB_TEST,          IDM_TEST,       TBSTATE_INDETERMINATE,  TBSTYLE_BUTTON,   0, 0);
  // Added by Darren Owen (DrO) on 1/10/2003
  tbButton[TBB_RECOMPILE_TEST] = CreateToolBarButton(IDB_RECOMPILE_TEST, IDM_RECOMPILE_TEST, TBSTATE_INDETERMINATE, TBSTYLE_BUTTON, 0, 0);
  tbButton[TBB_EDITSCRIPT]  = CreateToolBarButton(IDB_EDITSCRIPT,    IDM_EDITSCRIPT, TBSTATE_INDETERMINATE,  TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_BROWSESCR]   = CreateToolBarButton(IDB_BROWSESCR,     IDM_BROWSESCR,  TBSTATE_INDETERMINATE,  TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_CLEARLOG]    = CreateToolBarButton(IDB_CLEARLOG,      IDM_CLEARLOG,   TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_SEP3]        = CreateToolBarButton(0,                 0,              TBSTATE_ENABLED,        TBSTYLE_SEP,      0, 0);
  tbButton[TBB_NSISHOME]    = CreateToolBarButton(IDB_NSISHOME,      IDM_NSISHOME,   TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_FORUM]       = CreateToolBarButton(IDB_FORUM,         IDM_FORUM,      TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_NSISUPDATE]  = CreateToolBarButton(IDB_NSISUPDATE,    IDM_NSISUPDATE, TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);
  tbButton[TBB_SEP4]        = CreateToolBarButton(0,                 0,              TBSTATE_ENABLED,        TBSTYLE_SEP,      0, 0);
  tbButton[TBB_DOCS]        = CreateToolBarButton(IDB_DOCS,          IDM_DOCS,       TBSTATE_ENABLED,        TBSTYLE_BUTTON,   0, 0);

  g_toolbar.hwnd = CreateToolbarEx(g_sdata.hwnd,
                                   WS_CHILD | WS_VISIBLE | TBSTYLE_TRANSPARENT | TBSTYLE_FLAT,
                                   TOOLBAR_ID,
                                   NUMIMAGES,
                                   g_sdata.hInstance,
                                   IDB_TOOLBAR,
                                   tbButton,
                                   BUTTONCOUNT,
                                   BUTTONWIDTH,
                                   BUTTONHEIGHT,
                                   IMAGEWIDTH,
                                   IMAGEHEIGHT,
                                    sizeof(TBBUTTON));
  SendMessage(g_toolbar.hwnd, TB_SETEXTENDEDSTYLE, 0, (LPARAM) (DWORD) TBSTYLE_EX_DRAWDDARROWS);
  HMENU toolmenu = GetSubMenu(g_sdata.menu, TOOLS_MENU_INDEX);
  g_toolbar.dropdownmenu = GetSubMenu(toolmenu, COMPRESSOR_MENU_INDEX);
  RECT rect;
  SendMessage(g_toolbar.hwnd, TB_GETITEMRECT, TBB_COMPRESSOR, (LPARAM) (LPRECT) &rect);
  g_toolbar.dropdownpoint.x = rect.left;
  g_toolbar.dropdownpoint.y = rect.bottom+1;
}

void UpdateToolBarCompressorButton()
{
  int iBitmap;
  int iString;
  char   szBuffer[64];
  char   temp[32];
  TOOLINFO ti;

  my_memset(&ti, 0, sizeof(TOOLINFO));

  if(g_sdata.compressor >= COMPRESSOR_SCRIPT && g_sdata.compressor <= COMPRESSOR_BEST) {
    iBitmap = compressor_bitmaps[(int)g_sdata.compressor];
    iString = compressor_strings[(int)g_sdata.compressor];
  }
  else {
    return;
  }
  LoadString(g_sdata.hInstance,
             IDS_COMPRESSOR,
             temp,
             sizeof(temp));
  my_memset(szBuffer, 0, sizeof(szBuffer));
  lstrcat(szBuffer,temp);
  lstrcat(szBuffer," (");
  LoadString(g_sdata.hInstance,
             iString,
             temp,
             sizeof(temp));
  lstrcat(szBuffer,temp);
  lstrcat(szBuffer,")");

  SendMessage(g_toolbar.hwnd, TB_CHANGEBITMAP, (WPARAM) IDM_COMPRESSOR, (LPARAM) MAKELPARAM(iBitmap, 0));

  ti.cbSize = sizeof(TOOLINFO);
  ti.uFlags = 0;
  ti.hinst = g_sdata.hInstance;
  ti.hwnd = g_toolbar.hwnd;
  ti.uId = (UINT)TBB_COMPRESSOR;
  SendMessage(g_tip.tip, TTM_GETTOOLINFO, 0, (LPARAM) (LPTOOLINFO) &ti);
  ti.lpszText = (LPSTR)szBuffer;
  SendMessage(g_tip.tip, TTM_SETTOOLINFO, 0, (LPARAM) (LPTOOLINFO) &ti);
}

void AddToolBarButtonTooltip(int id, int iString)
{
  TOOLINFO ti;
  char   szBuffer[64];
  RECT rect;

  my_memset(&ti, 0, sizeof(TOOLINFO));

  SendMessage(g_toolbar.hwnd, TB_GETITEMRECT, id, (LPARAM) (LPRECT) &rect);

  ti.cbSize = sizeof(TOOLINFO);
  ti.uFlags = 0;
  ti.hwnd = g_toolbar.hwnd;
  ti.hinst = g_sdata.hInstance;
  ti.uId = (UINT)id;

  LoadString(g_sdata.hInstance,
             iString,
             szBuffer,
             sizeof(szBuffer));
  ti.lpszText = (LPSTR) szBuffer;
  ti.rect.left =rect.left;
  ti.rect.top = rect.top;
  ti.rect.right = rect.right;
  ti.rect.bottom = rect.bottom;

  SendMessage(g_tip.tip, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO) &ti);
}

void AddToolBarTooltips()
{
  AddToolBarButtonTooltip(TBB_LOADSCRIPT, IDS_LOADSCRIPT);
  AddToolBarButtonTooltip(TBB_SAVE, IDS_SAVE);
  AddToolBarButtonTooltip(TBB_EXIT, IDS_EXIT);
  AddToolBarButtonTooltip(TBB_COPY, IDS_COPY);
  AddToolBarButtonTooltip(TBB_FIND, IDS_FIND);
  AddToolBarButtonTooltip(TBB_SETTINGS, IDS_SETTINGS);
  AddToolBarButtonTooltip(TBB_RECOMPILE, IDS_RECOMPILE);
  AddToolBarButtonTooltip(TBB_COMPRESSOR, IDS_COMPRESSOR);
  AddToolBarButtonTooltip(TBB_TEST, IDS_TEST);
  // Added by Darren Owen (DrO) on 1/10/2003
  AddToolBarButtonTooltip(TBB_RECOMPILE_TEST, IDS_RECOMPILE_TEST);
  AddToolBarButtonTooltip(TBB_EDITSCRIPT, IDS_EDITSCRIPT);
  AddToolBarButtonTooltip(TBB_BROWSESCR, IDS_BROWSESCR);
  AddToolBarButtonTooltip(TBB_CLEARLOG, IDS_CLEARLOG);
  AddToolBarButtonTooltip(TBB_NSISHOME, IDS_NSISHOME);
  AddToolBarButtonTooltip(TBB_FORUM, IDS_FORUM);
  AddToolBarButtonTooltip(TBB_NSISUPDATE, IDS_NSISUPDATE);
  AddToolBarButtonTooltip(TBB_DOCS, IDS_DOCS);
}

void EnableToolBarButton(int id, BOOL enabled)
{
  UINT state = (enabled?TBSTATE_ENABLED:TBSTATE_INDETERMINATE);

  SendMessage(g_toolbar.hwnd, TB_SETSTATE, id, MAKELPARAM(state, 0));
}

void ShowToolbarDropdownMenu()
{
  RECT rect;
  GetWindowRect(g_toolbar.hwnd, (LPRECT) &rect);
  TrackPopupMenu(g_toolbar.dropdownmenu,
                NULL,
                rect.left + (int)(short)g_toolbar.dropdownpoint.x,
                rect.top + (int)(short)g_toolbar.dropdownpoint.y,
                0,
                g_sdata.hwnd,
                0);
}
