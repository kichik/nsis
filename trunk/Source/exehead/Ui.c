/*
*  Copyright (C) 1999-2002 Nullsoft, Inc.
*  Portions Copyright (C) 2002 Jeff Doozan
*
*  This software is provided 'as-is', without any express or implied warranty.
*  In no event will the authors be held liable for any damages arising from the
*  use of this software.
*
*  Permission is granted to anyone to use this software for any purpose, including
*  commercial applications, and to alter it and redistribute it freely, subject to
*  the following restrictions:
*
*  1. The origin of this software must not be misrepresented; you must not claim that
*  you wrote the original software. If you use this software in a product, an
*  acknowledgment in the product documentation would be appreciated but is not required.
*
*  2. Altered source versions must be plainly marked as such, and must not be 
*  misrepresented as being the original software.
*
*  3. This notice may not be removed or altered from any source distribution.
*/

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <stdlib.h>
#include <shellapi.h>

#include "resource.h"

#include "fileform.h"
#include "state.h"
#include "util.h"
#include "ui.h"
#include "exec.h"
#include "lang.h"

#define LB_ICONWIDTH 20
#define LB_ICONHEIGHT 20

// Added by Amir Szekely 3rd August 2002
installer_strings *install_strings_tables;
installer_strings *cur_install_strings_table;
common_strings *common_strings_tables;
common_strings *cur_common_strings_table;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
uninstall_strings *uninstall_strings_tables;
uninstall_strings *cur_uninstall_strings_table;
#endif

int g_quit_flag; // set when Quit has been called (meaning bail out ASAP)

#if NSIS_MAX_INST_TYPES >= 31 || NSIS_MAX_INST_TYPES < 1
#error invalid value for NSIS_MAX_INST_TYPES
#endif

char g_autoclose;
char g_noicon;
int progress_bar_pos, progress_bar_len;

HWND g_progresswnd;

static char g_tmp[4096];

// sent to the last child window to tell it that the install thread is done
#define WM_NOTIFY_INSTPROC_DONE (WM_USER+0x4)

// sent to the outer window to tell it to go to the next inner window
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)

// update message used by DirProc and SelProc for space display
#define WM_IN_UPDATEMSG (WM_USER+0xf)

#define WM_TREEVIEW_KEYHACK (WM_USER+0x13)

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
static BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static int CALLBACK WINAPI BrowseCallbackProc( HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData);
#ifdef NSIS_CONFIG_LICENSEPAGE
static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif
static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK InstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

static DWORD WINAPI install_thread(LPVOID p);

HWND bgWnd_Init(HINSTANCE hInstance, char *title, int color1, int color2, int);

HWND insthwnd, insthwnd2,insthwndbutton;

void *g_inst_combinedheader;
section *g_inst_section;
entry *g_inst_entry;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
int g_is_uninstaller;
static int g_max_page=1;
static int g_page_offs=4;
#else
#define g_max_page 3
#define g_page_offs 0
#endif

static int m_page=-1,m_abort;
HWND m_curwnd;
static int m_whichcfg;

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
BOOL my_SetDlgItemText(HWND dlg, WORD id, int strtab) {
  return SetDlgItemText(dlg,id,STR(strtab));
}

BOOL SetUIText(HWND defhw, WORD def, WORD custom, int strtab) {
  return my_SetDlgItemText(custom?g_hwnd:defhw,custom?custom:def,strtab);
}

// no tab
BOOL SetUITextNT(HWND defhw, WORD def, WORD custom, const char *text) {
  return SetDlgItemText(custom?g_hwnd:defhw,custom?custom:def,text);
}

UINT GetUIText(WORD def, WORD custom, char *str, int max_size) {
  return GetDlgItemText(custom?g_hwnd:m_curwnd,custom?custom:def,str,max_size);
}

HWND GetUIItem(HWND defhw, WORD def, WORD custom) {
  return GetDlgItem(custom?g_hwnd:defhw,custom?custom:def);
}
#endif

#ifdef NSIS_CONFIG_LOG
static void build_g_logfile()
{
  lstrcpy(g_log_file,state_install_directory);
  addtrailingslash(g_log_file);
  lstrcat(g_log_file,"install.log");
}
#endif

#ifdef NSIS_CONFIG_COMPONENTPAGE
static void SetChildrenStates(HWND hWnd, TV_ITEM *pItem, int iState) {
  HTREEITEM hItem;
  int l=0;

  pItem->mask|=TVIF_PARAM;

  TreeView_GetItem(hWnd, pItem);
  if (pItem->state >> 12 == 0) 
    return;

  if (iState < 3 && (g_inst_section[pItem->lParam].default_state & DFS_RO)) l=3;

  pItem->state = INDEXTOSTATEIMAGEMASK(iState+l);
  pItem->stateMask = TVIS_STATEIMAGEMASK;

  if (!(g_inst_section[pItem->lParam].default_state & DFS_RO))
  {
    if (iState == 2) g_inst_section[pItem->lParam].default_state |= DFS_SET;
    else g_inst_section[pItem->lParam].default_state &= ~DFS_SET;
    TreeView_SetItem(hWnd, pItem);
  }

  hItem = TreeView_GetChild(hWnd, pItem->hItem);
  while (hItem) {
    pItem->hItem = hItem;
    SetChildrenStates(hWnd, pItem, iState);
    hItem = TreeView_GetNextSibling(hWnd, hItem);
  }
}

static void SetParentState(HWND hWnd, TV_ITEM *pItem) {

  HTREEITEM hItem;
  int    iState = 0, iStatePrev = 0;

  HTREEITEM hParent = TreeView_GetParent(hWnd, pItem->hItem);
  if (!hParent)
    return;

  hItem = TreeView_GetChild(hWnd, hParent);

  while (hItem) {
    pItem->hItem = hItem;
    pItem->mask|=TVIF_PARAM;
    TreeView_GetItem(hWnd, pItem);
    iState = pItem->state >> 12;      
    if (iState && !(g_inst_section[pItem->lParam].default_state & DFS_RO)) 
    {
      if (iState==5) iState=2;
      else if (iState==4) iState=1;
      if (iStatePrev && (iStatePrev != iState)) {
        iState = 3;
        break;
      }
      iStatePrev = iState;
    }
    hItem = TreeView_GetNextSibling(hWnd, hItem);
  }

  pItem->hItem = hParent;
  if (iState) {
    pItem->state = INDEXTOSTATEIMAGEMASK(iState);
    TreeView_SetItem(hWnd, pItem);
  }

  SetParentState(hWnd, pItem);
}


static void CheckTreeItem(HWND hWnd, TV_ITEM *pItem, int checked) {
  HTREEITEM hItem = pItem->hItem;
  int l=0;

  pItem->mask = TVIF_STATE|TVIF_PARAM;
  TreeView_GetItem(hWnd, pItem);
  if (pItem->state >> 12 == 0) 
    return;

  if (g_inst_section[pItem->lParam].default_state & DFS_RO) l=3;

  pItem->state = INDEXTOSTATEIMAGEMASK(checked?2:1+l);
  pItem->stateMask = TVIS_STATEIMAGEMASK;

  TreeView_SetItem(hWnd, pItem);

  SetChildrenStates(hWnd, pItem, checked?2:1);
  pItem->hItem = hItem;
  SetParentState(hWnd, pItem);
}

#endif//NSIS_CONFIG_COMPONENTPAGE

int ui_doinstall(void)
{
  g_autoclose=g_inst_cmnheader->misc_flags&1;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (!g_is_uninstaller)
  {
    g_max_page=3;
    g_page_offs=0;
#else
  {
#endif
    if (!is_valid_instpath(state_install_directory))
    {
      if (g_inst_header->install_reg_key_ptr>=0)
      {
        myRegGetStr((HKEY)g_inst_header->install_reg_rootkey,
          GetStringFromStringTab(g_inst_header->install_reg_key_ptr),
            GetStringFromStringTab(g_inst_header->install_reg_value_ptr),ps_tmpbuf);
        if (ps_tmpbuf[0])
        {
          char *p=ps_tmpbuf;
          char *e;
          if (p[0]=='\"')
          {
            char *p2=CharNext(p);
            p=p2;
            while (*p2 && *p2 != '\"') p2=CharNext(p2);
            *p2=0;
          }
          // p is the path now, check for .exe extension

          e=p+lstrlen(p)-4;
          if (e > p)
          {
            // if filename ends in .exe, and is not a directory, remove the filename
            if (!lstrcmpi(e,".exe"))        // check extension
            {
              DWORD d;
              d=GetFileAttributes(p);
              if (d == (DWORD)-1 || !(d&FILE_ATTRIBUTE_DIRECTORY))
              {
                e=scanendslash(p);
                if (e>=p) *e=0;
              }
            }
          }

          lstrcpy(state_install_directory,p);
        }
      }
    }
    if (!is_valid_instpath(state_install_directory))
    {
      process_string_fromtab(state_install_directory,g_inst_header->install_directory_ptr);
    }

#ifdef NSIS_CONFIG_LOG
    if (g_inst_cmnheader->silent_install==2)
    {
      build_g_logfile();
      log_dolog=1;
    }
#endif
  }

  {
    // Added by Amir Szekely 3rd August 2002
    // Multilingual support
    char pa=1;
    int num=g_inst_header->str_tables_num;
    LANGID user_lang=GetUserDefaultLangID();
    int size=num*sizeof(common_strings);
    cur_common_strings_table=common_strings_tables=(common_strings*)GlobalAlloc(GPTR,size);
    GetCompressedDataFromDataBlockToMemory(g_inst_header->common.str_tables,(char*)common_strings_tables,size);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (g_is_uninstaller) {
      size=num*sizeof(uninstall_strings);
      cur_uninstall_strings_table=uninstall_strings_tables=(uninstall_strings*)GlobalAlloc(GPTR,size);
      GetCompressedDataFromDataBlockToMemory(g_inst_uninstheader->str_tables,(char*)uninstall_strings_tables,size);
    }
    else
#endif
    {
      size=num*sizeof(installer_strings);
      cur_install_strings_table=install_strings_tables=(installer_strings*)GlobalAlloc(GPTR,size);
      GetCompressedDataFromDataBlockToMemory(g_inst_header->str_tables,(char*)install_strings_tables,size);
    }
lang_again:
    for (size=0; size < num; size++) {
      if (user_lang == common_strings_tables[size].lang_id) {
        cur_install_strings_table+=size;
        cur_common_strings_table+=size;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        cur_uninstall_strings_table+=size;
#endif
        pa--;
        break;
      }
      common_strings_tables[size].lang_id&=0x3ff; // primary lang
    }
    if (pa) {
      user_lang&=0x3ff; // primary lang
      pa--;
      goto lang_again;
    }
  }

  process_string_fromtab(g_caption,COMMON_STR(caption));

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (!g_inst_cmnheader->silent_install)
#endif//NSIS_CONFIG_SILENT_SUPPORT
  {
    HWND h=GetDesktopWindow();
#ifdef NSIS_SUPPORT_BGBG
    if (g_inst_cmnheader->bg_color1 != -1)
    {
      h=bgWnd_Init(g_hInstance,g_caption,g_inst_cmnheader->bg_color1,g_inst_cmnheader->bg_color2,g_inst_cmnheader->bg_textcolor);
    }
#endif//NSIS_SUPPORT_BGBG
#ifdef NSIS_SUPPORT_CODECALLBACKS
    g_hwnd=h;
    if (ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInit,NULL)) return 1;
    g_hwnd=NULL;
#endif//NSIS_SUPPORT_CODECALLBACKS
    return DialogBox(g_hInstance,MAKEINTRESOURCE(IDD_INST),h,DialogProc);
  }
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  else
#endif
  {
#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInit,NULL)) return 1;
#endif//NSIS_SUPPORT_CODECALLBACKS
    if (install_thread(NULL))
    {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (!g_quit_flag) ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
      return 1;
    }
#ifdef NSIS_SUPPORT_CODECALLBACKS
    ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    return 0;
  }
#endif//NSIS_CONFIG_SILENT_SUPPORT
}


#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
static int CALLBACK WINAPI BrowseCallbackProc( HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
  if (uMsg==BFFM_INITIALIZED)
  {
    GetDlgItemText((HWND)lpData,IDC_DIR,g_tmp,sizeof(g_tmp));
    SendMessage(hwnd,BFFM_SETSELECTION,(WPARAM)1,(LPARAM)g_tmp);
  }
  return 0;
}


static BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HICON hIcon;
  if (uMsg == WM_DESTROY && hIcon) { DeleteObject(hIcon); hIcon=0; }
  if (uMsg == WM_INITDIALOG || uMsg == WM_NOTIFY_OUTER_NEXT)
  {
    int iscp=0,islp=0,isdp=0,ispotentiallydp=0;
    int delta=(uMsg == WM_NOTIFY_OUTER_NEXT)?wParam:0;
    int prev_page=m_page;
    static struct
    {
      char *id;
      DLGPROC proc;
    }
    windows[]=
    {
#ifdef NSIS_CONFIG_LICENSEPAGE
      {MAKEINTRESOURCE(IDD_LICENSE),LicenseProc},
#else
      {NULL,NULL},
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
      {MAKEINTRESOURCE(IDD_SELCOM),SelProc},
#else
      {NULL,NULL},
#endif
      {MAKEINTRESOURCE(IDD_DIR),DirProc},
      {MAKEINTRESOURCE(IDD_INSTFILES),InstProc},
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      {MAKEINTRESOURCE(IDD_UNINST),UninstProc},
      {MAKEINTRESOURCE(IDD_INSTFILES),InstProc},
#endif
    };

    if (uMsg == WM_INITDIALOG)
    {
      g_hwnd=hwndDlg;
      my_SetDlgItemText(hwndDlg,IDC_VERSTR,LANG_BRANDING);
      hIcon=LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON2));
      SetClassLong(hwndDlg,GCL_HICON,(long)hIcon);
      my_SetDlgItemText(hwndDlg,IDCANCEL,LANG_BTN_CANCEL);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!g_is_uninstaller)
#endif
        my_SetDlgItemText(hwndDlg,IDC_BACK,LANG_BTN_BACK);
      ShowWindow(hwndDlg,SW_SHOW);
    }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (g_is_uninstaller)
    {
      islp=UNINSTALL_STR(uninstalltext)>=0;
      iscp++;
    }
    else
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
    {
#ifdef NSIS_CONFIG_LICENSEPAGE
      if (INSTALL_STR(licensedata)>=0) islp++;
#endif//NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_COMPONENTPAGE
      if (INSTALL_STR(componenttext)>=0) iscp++;
#endif//NSIS_CONFIG_COMPONENTPAGE
      if (INSTALL_STR(text)>=0) ispotentiallydp++;
      if (ispotentiallydp &&
          !((g_inst_cmnheader->misc_flags&2) &&
            is_valid_instpath(state_install_directory)
#ifdef NSIS_SUPPORT_CODECALLBACKS
            && !ExecuteCodeSegment(g_inst_entry,g_inst_header->code_onVerifyInstDir,NULL)
#endif//NSIS_SUPPORT_CODECALLBACKS
            )) isdp++;
    }

    if (m_page<=0) delta=1;
    do
    {
      int count=1;  // Number of pages to move by
#ifdef NSIS_SUPPORT_CODECALLBACKS
      // Call onNext|PrevPage for every not-definitely-disabled page
      if (ExecuteCodeSegment(g_inst_entry,delta>0?g_inst_cmnheader->code_onNextPage:g_inst_header->code_onPrevPage,NULL))
      {
        if (g_quit_flag)  // Quit instruction used?
          m_page=count=-1;
        // Mmm - relies on ps_tmpbuf still being set from the Abort command - safe?
        else if ((count = myatoi(ps_tmpbuf)) != 0)
          count /= (delta = (count>0?1:-1));
      }
#endif//NSIS_SUPPORT_CODECALLBACKS
      // Skip any definitely-disabled pages, then the required number of pages
      while ((m_page==0 && !islp) || (m_page==1 && !iscp) || (m_page==2 && !ispotentiallydp) || (--count>=0))
        m_page+=delta;
      // Skip any possibly-disabled pages
    } while ((m_page >= 0) && (m_page <= g_max_page) && (m_page==2 && !isdp));

#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (m_page>g_max_page) ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    if (m_curwnd && (m_page!=prev_page))
    {
      DestroyWindow(m_curwnd);
      m_curwnd=0;
    }

    if (m_page < 0 || m_page > g_max_page)
    {
      EndDialog(hwndDlg,0);
    }
    else if (!m_curwnd)
    {
      HWND hwndtmp;
      my_SetDlgItemText(hwndDlg,IDOK,
       (m_page == g_max_page) ? LANG_BTN_CLOSE :
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        g_is_uninstaller ? LANG_BTN_UNINST :
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
        (m_page == 0) ? LANG_BTN_LICENSE :
#endif
        (m_page == 2 || (m_page == 1 && !isdp)) ? LANG_BTN_INSTALL :
        LANG_BTN_NEXT
      );
      lstrcpy(g_tmp,g_caption);
      process_string_fromtab(g_tmp+lstrlen(g_tmp),LANG_SUBCAPTION(m_page));

      SetWindowText(hwndDlg,g_tmp);

      m_curwnd=CreateDialog(g_hInstance,windows[g_page_offs+m_page].id,hwndDlg,windows[g_page_offs+m_page].proc);
      if (m_curwnd)
      {
        RECT r;
        GetWindowRect(GetDlgItem(hwndDlg,IDC_CHILDRECT),&r);
        ScreenToClient(hwndDlg,(LPPOINT)&r);
        SetWindowPos(m_curwnd,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
        ShowWindow(m_curwnd,SW_SHOWNA);
      }

      hwndtmp=GetDlgItem(hwndDlg,IDC_BACK);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      ShowWindow(hwndtmp,(m_page&&!g_is_uninstaller)?SW_SHOWNA:SW_HIDE);
      if (!g_is_uninstaller)
#else
      ShowWindow(hwndtmp,m_page?SW_SHOWNA:SW_HIDE);
#endif
        EnableWindow(hwndtmp, (m_page==1&&islp) || (m_page==2&&(islp||iscp)));
      SetFocus(GetDlgItem(hwndDlg,IDOK));
    }
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam);

    if (id == IDOK && m_curwnd)
    {
      SendMessage(hwndDlg,WM_NOTIFY_OUTER_NEXT,1,0);
    }
    if (
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      !g_is_uninstaller &&
#endif
      (id == IDC_BACK && m_curwnd && m_page>0))
    {
      EnableWindow(GetDlgItem(hwndDlg, IDOK), TRUE);
      SendMessage(hwndDlg,WM_NOTIFY_OUTER_NEXT,-1,0);
    }
    if (id == IDCANCEL)
    {
      if (m_abort)
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
        EndDialog(hwndDlg,2);
      }
      else
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (!ExecuteCodeSegment(g_inst_entry,g_inst_cmnheader->code_onUserAbort,NULL))
#endif//NSIS_SUPPORT_CODECALLBACKS
        {
          EndDialog(hwndDlg,1);
        }
      }
    }
  }
  if (uMsg == WM_CLOSE)
  {
    if (!IsWindowEnabled(GetDlgItem(hwndDlg,IDCANCEL)) && IsWindowEnabled(GetDlgItem(hwndDlg,IDOK)))
      SendMessage(hwndDlg,WM_COMMAND,IDOK,0);
  }
  return 0;
}

#ifdef NSIS_CONFIG_LICENSEPAGE
// Changed by Amir Szekely 27th July 2002
#define _RICHEDIT_VER 0x0200
#include <RichEdit.h>
#undef _RICHEDIT_VER
static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HWND hwLicense;
  static HINSTANCE hRichEditDLL;
  if (!hRichEditDLL) hRichEditDLL=LoadLibrary("RichEd32.dll");
  if (uMsg == WM_INITDIALOG)
  {
    hwLicense=GetDlgItem(hwndDlg,IDC_EDIT1);
    SendMessage(hwLicense,EM_AUTOURLDETECT,TRUE,0);
    SendMessage(hwLicense,EM_SETBKGNDCOLOR,0,g_inst_header->license_bg);
    SendMessage(hwLicense,EM_SETEVENTMASK,0,ENM_LINK);
    SetWindowText(hwLicense,STR(LANG_LICENSE_DATA));
    SetUIText(hwndDlg,IDC_INTROTEXT,g_inst_header->common.intro_text_id,LANG_LICENSE_TEXT);
  }
  else if (uMsg == WM_NOTIFY) {
    ENLINK *enlink=(ENLINK *)lParam;
    if (enlink->nmhdr.code==EN_LINK) {
      if (enlink->msg==WM_LBUTTONDOWN) {
        char *szUrl;
        long min=enlink->chrg.cpMin, max=enlink->chrg.cpMax;
        SendMessage(hwLicense,EM_SETSEL,min,max);
        szUrl=(char *)GlobalAlloc(GPTR,max-min+1);
        SendMessage(hwLicense,EM_GETSELTEXT,0,(LPARAM)szUrl);
        SetCursor(LoadCursor(0,IDC_WAIT));
        ShellExecute(hwndDlg,"open",szUrl,NULL,NULL,SW_SHOWNORMAL);
        SetCursor(LoadCursor(0,IDC_ARROW));
        GlobalFree(szUrl);
      }
      else if (enlink->msg==WM_SETCURSOR) {
#ifndef IDC_HAND
#define IDC_HAND MAKEINTRESOURCE(32649)
#endif
        SetCursor(LoadCursor(0,IDC_HAND));
      }
    }
  }
  else if (uMsg == WM_CLOSE) {
    SendMessage(g_hwnd,WM_CLOSE,0,0);
  }
  return 0;
}
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    SetUIText(hwndDlg,IDC_INTROTEXT,g_inst_header->common.intro_text_id,LANG_UNINST_TEXT);
    SetUIText(hwndDlg,IDC_UNINSTFROM,g_inst_uninstheader->uninst_subtext_id,LANG_UNINST_SUBTEXT);
    SetDlgItemText(hwndDlg,IDC_EDIT1,state_install_directory);
  }
  return 0;
}
#endif


static void inttosizestr(int kb, char *str)
{
  while (*str) str++;
  if (kb < 1024) wsprintf(str,"%dKB",kb);
  else if (kb < 1024*1024) wsprintf(str,"%d.%dMB",kb>>10,((kb*10)>>10)%10);
  else wsprintf(str,"%d.%dGB%s",kb>>20,((kb*10)>>20)%10,(GetVersion()&0x80000000) ? "+":"");
}

static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_DESTROY)
  {
    GetDlgItemText(hwndDlg,IDC_DIR,state_install_directory,NSIS_MAX_STRLEN);
#ifdef NSIS_CONFIG_LOG
    build_g_logfile();
    log_dolog = !!IsDlgButtonChecked(hwndDlg,IDC_CHECK1);
#endif
  }
  if (uMsg == WM_INITDIALOG)
  {
#ifdef NSIS_CONFIG_LOG
    if (GetAsyncKeyState(VK_SHIFT)&0x8000)
    {
      HWND h=GetDlgItem(hwndDlg,IDC_CHECK1);
      SetWindowText(h,"Log install process");
      ShowWindow(h,SW_SHOWNA);
    }
#endif
    SetDlgItemText(hwndDlg,IDC_DIR,state_install_directory);
    SetUIText(hwndDlg,IDC_INTROTEXT,g_inst_header->common.intro_text_id,LANG_DIR_TEXT);
    my_SetDlgItemText(hwndDlg,IDC_BROWSE,LANG_BTN_BROWSE);
    SetUIText(hwndDlg,IDC_SELDIRTEXT,g_inst_header->dir_subtext_id,LANG_DIR_SUBTEXT);
    SendMessage(hwndDlg,WM_IN_UPDATEMSG,0,0);
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam);
    if (id == IDC_DIR && HIWORD(wParam) == EN_CHANGE)
    {
      SendMessage(hwndDlg,WM_IN_UPDATEMSG,0,0);
    }
    if (id == IDC_BROWSE)
    {
      char name[256];
      char str[256];
      BROWSEINFO bi={0,};
      ITEMIDLIST *idlist;
      GetDlgItemText(hwndDlg,IDC_DIR,name,256);
      GetUIText(IDC_SELDIRTEXT,g_inst_header->dir_subtext_id,str,256);
      bi.hwndOwner = hwndDlg;
      bi.pszDisplayName = name;
      bi.lpfn=BrowseCallbackProc;
      bi.lParam=(LPARAM)hwndDlg;
      bi.lpszTitle = str;
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x0040
#endif
      bi.ulFlags = BIF_RETURNONLYFSDIRS | BIF_NEWDIALOGSTYLE;
      idlist = SHBrowseForFolder( &bi );
      if (idlist)
      {
        const char *post_str;
        const char *p;
        IMalloc *m;
        SHGetPathFromIDList( idlist, name );
        SHGetMalloc(&m);
        if (m)
        {
          m->lpVtbl->Free(m,idlist);
          m->lpVtbl->Release(m);
        }
        post_str=GetStringFromStringTab(g_inst_header->install_directory_ptr);

        p=scanendslash(post_str);
        if (p >= post_str && *++p)
        {
          post_str=p;
          p=name+lstrlen(name)-lstrlen(post_str);
          if (p <= name || *CharPrev(name,p)!='\\' || lstrcmpi(p,post_str))
          {
            addtrailingslash(name);
            lstrcat(name,post_str);
          }
        }

        SetDlgItemText(hwndDlg,IDC_DIR,name);
      }
    }
  }
  if (uMsg == WM_IN_UPDATEMSG)
  {
    static char s[NSIS_MAX_STRLEN];
    int is_valid_path;
    int x;
    int total=0, available=-1;
    DWORD spc,bps,fc,tc;

    GetDlgItemText(hwndDlg,IDC_DIR,state_install_directory,NSIS_MAX_STRLEN);
    is_valid_path=is_valid_instpath(state_install_directory);

    mini_memcpy(s,state_install_directory,NSIS_MAX_STRLEN);
    s[sizeof(s)-1]=0;
    if (s[1] == ':') s[3]=0;
    else if (s[0] == '\\' && s[1] == '\\') // \\ path
    {
      addtrailingslash(s);
    }

    if (GetDiskFreeSpace(s,&spc,&bps,&fc,&tc))
    {
      DWORD r;
      DWORD v=0x7fffffff;
      r=bps*spc*(fc>>10);
      if (!r) r=(bps*spc*fc)>>10;
      if (r > v) r=v;
      available=(int)r;
    }
    for (x = 0; x < g_inst_header->num_sections; x ++)
    {
#ifdef NSIS_CONFIG_COMPONENTPAGE
      if (g_inst_section[x].default_state&DFS_SET)
#endif
       total+=g_inst_section[x].size_kb;
    }

    // Added by Amir Szekely 24th July 2002
    // Allows 'SpaceTexts none'
    if (INSTALL_STR(spacerequired) >= 0) {
      lstrcpy(s,STR(LANG_SPACE_REQ));
      inttosizestr(total,s);
      SetUITextNT(hwndDlg,IDC_SPACEREQUIRED,g_inst_header->space_req_id,s);
      if (available != -1)
      {
        lstrcpy(s,STR(LANG_SPACE_AVAIL));
        inttosizestr(available,s);
        SetUITextNT(hwndDlg,IDC_SPACEAVAILABLE,g_inst_header->space_avail_id,s);
      }
      else
        SetUITextNT(hwndDlg,IDC_SPACEAVAILABLE,g_inst_header->space_avail_id,"");
    }

    EnableWindow(GetDlgItem(g_hwnd,IDOK),
      is_valid_path && (available >= total || available == -1)
#ifdef NSIS_SUPPORT_CODECALLBACKS
      && !ExecuteCodeSegment(g_inst_entry,g_inst_header->code_onVerifyInstDir,NULL)
#endif
      );
  }
  return 0;
}

#ifdef NSIS_CONFIG_COMPONENTPAGE

static LONG oldTreeWndProc;
static DWORD WINAPI newTreeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_KEYDOWN && wParam == VK_SPACE)
  {
    SendMessage(GetParent(hwnd),WM_TREEVIEW_KEYHACK,0,0);
    return 0;
  }
  return CallWindowProc((WNDPROC)oldTreeWndProc,hwnd,uMsg,wParam,lParam);
}

int m_num_insttypes;

static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HTREEITEM *hTreeItems;
  static HIMAGELIST hImageList;
  HWND hwndCombo1 = GetDlgItem(hwndDlg,IDC_COMBO1);
  HWND hwndTree1 = GetDlgItem(hwndDlg,IDC_TREE1);
  if (uMsg == WM_INITDIALOG)
  {
    int doLines=0;
    HTREEITEM Par;
    HBITMAP hBMcheck1;
    int x;
    if (hTreeItems) GlobalFree(hTreeItems);
    hTreeItems=(HTREEITEM*)GlobalAlloc(GPTR,sizeof(HTREEITEM)*g_inst_header->num_sections);

    hBMcheck1=LoadBitmap(g_hInstance, MAKEINTRESOURCE(IDB_BITMAP1));
    SetUIText(hwndDlg,IDC_INTROTEXT,g_inst_header->common.intro_text_id,LANG_COMP_TEXT);
    SetUIText(hwndDlg,IDC_TEXT1,g_inst_header->com_subtext1_id,LANG_COMP_SUBTEXT(0));
    SetUIText(hwndDlg,IDC_TEXT2,g_inst_header->com_subtext2_id,LANG_COMP_SUBTEXT(1));

    oldTreeWndProc=GetWindowLong(hwndTree1,GWL_WNDPROC);
    SetWindowLong(hwndTree1,GWL_WNDPROC,(DWORD)newTreeWndProc);

    if (hImageList) ImageList_Destroy(hImageList);
    hImageList = ImageList_Create(16,16, ILC_COLOR32, 4, 4);
    ImageList_SetBkColor(hImageList, GetSysColor(COLOR_WINDOW));
    
    ImageList_Add(hImageList,hBMcheck1,NULL);

    TreeView_SetImageList(hwndTree1, hImageList, TVSIL_STATE);

    DeleteObject(hBMcheck1);

    if (g_inst_header->install_types_ptr[0]<0)
    {
      ShowWindow(hwndCombo1,SW_HIDE);
    }
    else
    {
      for (m_num_insttypes = 0; m_num_insttypes < NSIS_MAX_INST_TYPES && 
           g_inst_header->install_types_ptr[m_num_insttypes]>=0; m_num_insttypes ++)
      {
        SendMessage(hwndCombo1,CB_ADDSTRING,0,(LPARAM)GetStringFromStringTab(g_inst_header->install_types_ptr[m_num_insttypes]));
      }
      if (g_inst_header->no_custom_instmode_flag!=1) SendMessage(hwndCombo1,CB_ADDSTRING,0,(LPARAM)STR(LANG_COMP_CUSTOM));
      SendMessage(hwndCombo1,CB_SETCURSEL,m_whichcfg,0);
    }

    Par=NULL;

    for (x = 0; x < g_inst_header->num_sections; x ++)
    {
      if (g_inst_section[x].name_ptr>=0)
      {
        TVINSERTSTRUCT tv;
        process_string_fromtab(ps_tmpbuf,g_inst_section[x].name_ptr);
        tv.hParent=Par;
        tv.hInsertAfter=TVI_LAST;
        tv.item.mask=TVIF_PARAM|TVIF_TEXT|TVIF_STATE;
        tv.item.lParam=x;
        tv.item.pszText=ps_tmpbuf;
        tv.item.stateMask=TVIS_STATEIMAGEMASK;

        if (m_num_insttypes && m_whichcfg != m_num_insttypes && !(g_inst_section[x].default_state&DFS_RO))
        {
          if ((g_inst_section[x].default_state>>m_whichcfg) & 1)
            g_inst_section[x].default_state|=DFS_SET;
          else
            g_inst_section[x].default_state&=~DFS_SET;
        }

        {
          int l=1;
          if (g_inst_section[x].default_state & DFS_SET) l++;
          if (g_inst_section[x].default_state & DFS_RO) l+=3;

          tv.item.state=INDEXTOSTATEIMAGEMASK(l);
        }

        if (ps_tmpbuf[0] == '!' || (ps_tmpbuf[0] == '-' && ps_tmpbuf[1] == '!'))
        {
          tv.item.pszText++;
          tv.item.stateMask|=TVIS_BOLD;
          tv.item.state|=TVIS_BOLD;
        }

        if (ps_tmpbuf[0] == '-')
        {
          if (ps_tmpbuf[1])
          {
            tv.item.pszText++;
            tv.item.mask|=TVIF_CHILDREN;
            tv.item.cChildren=1;
            Par = hTreeItems[x] = TreeView_InsertItem(hwndTree1,&tv);
            doLines=1;
          }
          else if (Par && x)
          {
            TV_ITEM it;
            it.hItem = hTreeItems[x-1];
            it.mask = TVIF_STATE;
            it.stateMask=TVIS_STATEIMAGEMASK;

            SetParentState(hwndTree1,&it);

            Par=TreeView_GetParent(hwndTree1,Par);
          }
        }
        else
        {
      hTreeItems[x] = TreeView_InsertItem(hwndTree1,&tv);
        }
      }
    }
    for (x = 0; x < g_inst_header->num_sections; x ++)
    {
      if (g_inst_section[x].name_ptr>=0 && g_inst_section[x].expand==1)
      {
    SendMessage(hwndTree1,TVM_EXPAND,(WPARAM) TVE_TOGGLE,(LPARAM) hTreeItems[x]);
    }
  }
    if (!doLines)
    {
      SetWindowLong(hwndTree1,GWL_STYLE,GetWindowLong(hwndTree1,GWL_STYLE)&~(TVS_LINESATROOT));
    }
    SendMessage(hwndTree1,WM_VSCROLL,SB_TOP,0);

    SendMessage(hwndDlg,WM_IN_UPDATEMSG,0,0);
  }
  if (uMsg == WM_USER+0x17)
  {
    int x=wParam;
    int ns=lParam;

    if (g_inst_section[x].name_ptr>=0 && ns >= 0)
    {
      TVITEM tv;
      process_string_fromtab(ps_tmpbuf,ns);
      tv.hItem=hTreeItems[x];
      tv.mask=TVIF_TEXT;
      tv.pszText=ps_tmpbuf;
      TreeView_SetItem(hwndTree1,&tv);
    }
    SendMessage(hwndDlg,WM_USER+0x18,x,(LPARAM)!!(g_inst_section[x].default_state&DFS_SET));
  }
  if (uMsg == WM_USER+0x18) // select
  {
    TVITEM hItem;
    hItem.mask = TVIF_STATE;
    hItem.hItem=hTreeItems[wParam];
    if (hItem.hItem) CheckTreeItem(hwndTree1, &hItem,lParam);
  }
  if (uMsg == WM_NOTIFY || uMsg == WM_TREEVIEW_KEYHACK)
  {
    LPNMHDR lpnmh = (LPNMHDR) lParam;   
    if (uMsg == WM_TREEVIEW_KEYHACK || lpnmh->idFrom == IDC_TREE1)
    {
      if (uMsg == WM_TREEVIEW_KEYHACK || lpnmh->code == NM_CLICK) 
      {
        TVHITTESTINFO ht = {0};
        if (uMsg != WM_TREEVIEW_KEYHACK)
        {
           DWORD dwpos = GetMessagePos();
       
           ht.pt.x = GET_X_LPARAM(dwpos);
           ht.pt.y = GET_Y_LPARAM(dwpos);
           MapWindowPoints(HWND_DESKTOP, hwndTree1, &ht.pt, 1);
       
           TreeView_HitTest(hwndTree1, &ht);
        }
        else
        {
          ht.hItem=TreeView_GetSelection(hwndTree1);
          if (ht.hItem) ht.flags=TVHT_ONITEMSTATEICON;
        }
       
        if ((TVHT_ONITEMSTATEICON|TVHT_ONITEMLABEL|TVHT_ONITEMRIGHT|TVHT_ONITEM) & ht.flags) 
        {
            TVITEM hItem;
            int image,wh;
            hItem.hItem = ht.hItem;
          
            hItem.mask = TVIF_STATE|TVIF_PARAM;
            TreeView_GetItem(hwndTree1, &hItem);

            image = hItem.state >> 12;
            wh=hItem.lParam;

            if (!(g_inst_section[wh].default_state&DFS_RO))
            {
              if (image == 2) // already checked
              {
                g_inst_section[wh].default_state&=~DFS_SET;
                CheckTreeItem(hwndTree1, &hItem,0);
              }
              else
              {
                g_inst_section[wh].default_state|=DFS_SET;
                CheckTreeItem(hwndTree1, &hItem,1);
              }
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_COMPONENTPAGE)
              {
                extern HWND g_SectionHack;
                g_SectionHack=hwndDlg;
                ExecuteCodeSegment(g_inst_entry,g_inst_header->code_onSelChange,NULL);
                g_SectionHack=0;
              }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_COMPONENTPAGE
              {
                int r,x;
                // check to see which install type we are
                for (r = 0; r < m_num_insttypes; r ++)
                {
                  for (x = 0; x < g_inst_header->num_sections; x ++)
                  {
                    char c=GetStringFromStringTab(g_inst_section[x].name_ptr)[0];
                    if (c && c!='-')
                    {
                      TV_ITEM hItem;
                      hItem.hItem=hTreeItems[x];
                      if (g_inst_header->no_custom_instmode_flag==1)
                      {
                        int c=(g_inst_section[x].default_state>>m_whichcfg)&1;
                        CheckTreeItem(hwndTree1, &hItem,c);
                      }
                      else if (!(g_inst_section[x].default_state&DFS_RO))
                      {
                        hItem.mask=TVIF_STATE;
                        TreeView_GetItem(hwndTree1,&hItem);
                        if (!(g_inst_section[x].default_state&(1<<r)) != !((hItem.state>>12)>1 )) break;
                      }
                    }
                  }
                  if (x == g_inst_header->num_sections) break;
                }

                if (!g_inst_header->no_custom_instmode_flag)
                {
                  SendMessage(hwndCombo1,CB_SETCURSEL,r,0);
                  m_whichcfg=r;
                }
              } // end of typecheckshit
              SendMessage(hwndDlg,WM_IN_UPDATEMSG,0,0);
            } // not ro
        } // was valid click
      } // was click or hack
    }
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam),code=HIWORD(wParam);
    if (id == IDC_COMBO1 && code==CBN_SELCHANGE)
    {
      int t=SendMessage(hwndCombo1,CB_GETCURSEL,0,0);
      if (t != CB_ERR)
      {
        m_whichcfg=t;
        if (m_whichcfg != m_num_insttypes)
        {
          int x;
          for (x = 0; x < g_inst_header->num_sections; x ++)
          {
            if (g_inst_section[x].name_ptr>=0)
            {
              if (!(g_inst_section[x].default_state & DFS_RO))
              {
                TVITEM tv;
                int n=(g_inst_section[x].default_state & (1<<m_whichcfg));
                int l=n?2:1;
                if (n) g_inst_section[x].default_state|=DFS_SET;
                else g_inst_section[x].default_state&=~DFS_SET;

                tv.hItem=hTreeItems[x];
                tv.mask=TVIF_STATE;
                if (g_inst_section[x].default_state & DFS_RO) l+=3;

                tv.state=INDEXTOSTATEIMAGEMASK(l);
                tv.stateMask = TVIS_STATEIMAGEMASK;

                TreeView_SetItem(hwndTree1,&tv);
                SetParentState(hwndTree1,&tv);
              }
            }
          }
          SendMessage(hwndTree1,WM_VSCROLL,SB_TOP,0);
        }
        SendMessage(hwndDlg,WM_IN_UPDATEMSG,0,0);
      }
    }
  }
  if (uMsg == WM_DESTROY)
  {
    if (hImageList) ImageList_Destroy(hImageList);
    if (hTreeItems) GlobalFree(hTreeItems);
    hTreeItems=0;
  }
  if (uMsg == WM_IN_UPDATEMSG)
  {
    if (g_inst_header->no_custom_instmode_flag==2)
    {
      int c=(m_whichcfg == m_num_insttypes && m_num_insttypes)<<3;// SW_SHOWNA=8, SW_HIDE=0
      ShowWindow(hwndTree1,c);
      ShowWindow(GetUIItem(hwndDlg,IDC_TEXT2,g_inst_header->com_subtext2_id),c);
    }

    if (INSTALL_STR(spacerequired) >= 0) {
      int x,total;
      char s[128];
      for (total=x=0; x < g_inst_header->num_sections; x ++)
      {
        if (g_inst_section[x].default_state&DFS_SET)
          total+=g_inst_section[x].size_kb;
      }
      lstrcpy(s,STR(LANG_SPACE_REQ));
      inttosizestr(total,s);
      SetUITextNT(hwndDlg,IDC_SPACEREQUIRED,g_inst_header->space_req_id,s);
    }
  }
  return 0;
}
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//NSIS_CONFIG_VISIBLE_SUPPORT

int ui_st_updateflag=0x3;

void update_status_text_from_tab(int texttab, const char *text2)
{
  update_status_text(STR(texttab), text2);
}

void update_status_text(const char *text1, const char *text2)
{
  static LVITEM new_item = {LVIF_TEXT,0,0,0,0,ps_tmpbuf};
  RECT r;
  if (insthwnd)
  {
    if (lstrlen(text1)+lstrlen(text2) >= sizeof(ps_tmpbuf)) return;
    wsprintf(ps_tmpbuf,"%s%s",text1,text2);
    if ((ui_st_updateflag&1))
    {
      // Changed by Amir Szekely 26th July 2002
      new_item.iItem=ListView_GetItemCount(insthwnd);
      ListView_InsertItem(insthwnd, &new_item);
      ListView_EnsureVisible(insthwnd, new_item.iItem, 0);
      GetClientRect(insthwnd,&r);
      ListView_SetColumnWidth(insthwnd, 0, r.right-r.left);
    }
    if ((ui_st_updateflag&2)) SetWindowText(insthwnd2,ps_tmpbuf);
  }
}


static DWORD WINAPI install_thread(LPVOID p)
{
  HWND hwndDlg=(HWND)p;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    if (ExecuteCodeSegment(g_inst_entry,g_inst_uninstheader->code,g_progresswnd)) m_abort++;
  }
  else
  {
#endif
    int m_inst_sec=0;
    while (m_inst_sec<g_inst_header->num_sections && !m_abort)
    {
#ifdef NSIS_CONFIG_COMPONENTPAGE
      if (g_inst_section[m_inst_sec].default_state&DFS_SET
#ifdef NSIS_CONFIG_SILENT_SUPPORT
        || g_inst_cmnheader->silent_install
#endif//NSIS_CONFIG_SILENT_SUPPORT
        )
#endif
      {
        log_printf2("Section: \"%s\"",GetStringFromStringTab(g_inst_section[m_inst_sec].name_ptr));
        if (ExecuteCodeSegment(g_inst_entry,g_inst_section[m_inst_sec].code,g_progresswnd)) m_abort++;
      }
#ifdef NSIS_CONFIG_COMPONENTPAGE
      else
      {
        log_printf2("Skipping section: \"%s\"",GetStringFromStringTab(g_inst_section[m_inst_sec].name_ptr));
      }
#endif
      m_inst_sec++;
    }
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  }
#endif
  if (hwndDlg) SendMessage(hwndDlg,WM_NOTIFY_INSTPROC_DONE,m_abort,0);
  return m_abort;
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
static BOOL CALLBACK InstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    DWORD id;
    int num=0;
    int x=0;
    LVCOLUMN lvc = {0, 0, -1, 0, 0, -1};
    int lb_bg=g_inst_cmnheader->lb_bg,lb_fg=g_inst_cmnheader->lb_fg;

    insthwndbutton=GetDlgItem(hwndDlg,IDC_SHOWDETAILS);
    insthwnd2=GetUIItem(hwndDlg,IDC_INTROTEXT,g_inst_header->common.intro_text_id);
    insthwnd=GetDlgItem(hwndDlg,IDC_LIST1);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (g_is_uninstaller)
    {
      num=g_inst_uninstheader->code_size;
    }
    else
#endif
    {
      log_printf3("New install of \"%s\" to \"%s\"",STR(LANG_NAME),state_install_directory);
      for (; x < g_inst_header->num_sections; x ++)
      {
#ifdef NSIS_CONFIG_COMPONENTPAGE
        if (g_inst_section[x].default_state&DFS_SET)
#endif
          num+=g_inst_section[x].code_size;
      }
    }
    // Changed by Amir Szekely 26th July 2002
    ListView_InsertColumn(insthwnd, 0, &lvc);
#define LVS_EX_LABELTIP         0x00004000 // listview unfolds partly hidden labels if it does not have infotip text
    ListView_SetExtendedListViewStyleEx(insthwnd, LVS_EX_LABELTIP, LVS_EX_LABELTIP);
    ListView_SetBkColor(insthwnd, lb_bg);
    ListView_SetTextBkColor(insthwnd, lb_bg);
    ListView_SetTextColor(insthwnd, lb_fg);
    SetWindowText(insthwndbutton,STR(LANG_BTN_DETAILS));
    if (g_inst_cmnheader->show_details)
    {
      ShowWindow(insthwndbutton,SW_HIDE);
      if (g_inst_cmnheader->show_details != 2) ShowWindow(insthwnd,SW_SHOWNA);
      else insthwndbutton=NULL;
    }
    progress_bar_len=num;

    g_progresswnd=GetDlgItem(hwndDlg,IDC_PROGRESS1+(g_inst_cmnheader->progress_flags&1));
    ShowWindow(g_progresswnd,SW_SHOWNA);
    SendMessage(g_progresswnd,PBM_SETRANGE,0,MAKELPARAM(0,30000));
    if (g_inst_cmnheader->progress_flags&2)
    {
      SendMessage(g_progresswnd,PBM_SETBARCOLOR,0,lb_fg);
      SendMessage(g_progresswnd,PBM_SETBKCOLOR,0,lb_bg);
    }

    EnableWindow(GetDlgItem(g_hwnd,IDOK),0);
    EnableWindow(GetDlgItem(g_hwnd,IDCANCEL),0);

    CloseHandle(CreateThread(NULL,0,install_thread,(LPVOID)hwndDlg,0,&id));
  }
  if (uMsg == WM_COMMAND && LOWORD(wParam) == IDC_SHOWDETAILS)
  {
    ShowWindow(GetDlgItem(hwndDlg,IDC_SHOWDETAILS),SW_HIDE);
    SendMessage(insthwnd,WM_VSCROLL,SB_BOTTOM,0);
    ShowWindow(insthwnd,SW_SHOWNA);
  }
  if (uMsg == WM_NOTIFY_INSTPROC_DONE)
  {
    if (g_quit_flag) EndDialog(g_hwnd,1);
    else if (!wParam)
    {
      HWND h=GetDlgItem(g_hwnd,IDOK);
      EnableWindow(h,1);
      if (!g_autoclose)
      {
        ShowWindow(g_hwnd,SW_SHOWNA);
        lstrcpy(g_tmp,g_caption);
        process_string_fromtab(g_tmp+lstrlen(g_caption),COMMON_STR(subcaptions[g_max_page+1]));
        update_status_text_from_tab(LANG_COMPLETED,"");
        SetWindowText(g_hwnd,g_tmp);
        SetFocus(h);
      }
      else
      {
        SendMessage(g_hwnd,WM_NOTIFY_OUTER_NEXT,1,0);
      }
    }
    else
    {
      HWND h=GetDlgItem(g_hwnd,IDCANCEL);
      EnableWindow(h,1);
      SetFocus(h);
    }
  }
  return 0;
}
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
