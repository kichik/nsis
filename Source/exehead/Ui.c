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

HICON g_hIcon;
static char gDontFookWithFocus = 0;

// Added by Amir Szekely 3rd August 2002
common_strings *common_strings_tables;
common_strings *cur_common_strings_table;
char *install_strings_tables; // installer_strings/uninstall_strings depending on installer type
char *cur_install_strings_table;

int g_quit_flag; // set when Quit has been called (meaning bail out ASAP)

#if NSIS_MAX_INST_TYPES >= 31 || NSIS_MAX_INST_TYPES < 1
#error invalid value for NSIS_MAX_INST_TYPES
#endif

char g_autoclose;
char g_noicon;
int progress_bar_pos, progress_bar_len;

HWND g_progresswnd;

static char g_tmp[4096];

static int num_sections;

// sent to the last child window to tell it that the install thread is done
#define WM_NOTIFY_INSTPROC_DONE (WM_USER+0x4)

// sent to every child window to tell it it can start executing NSIS code
#define WM_NOTIFY_START (WM_USER+0x5)

// sent to the outer window to tell it to go to the next inner window
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)

// update message used by DirProc and SelProc for space display
#define WM_IN_UPDATEMSG (WM_USER+0xf)

#define WM_TREEVIEW_KEYHACK (WM_USER+0x13)

static void NSISCALL outernotify(char num) {
  SendMessage(g_hwnd,WM_NOTIFY_OUTER_NEXT,(WPARAM)num,0);
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
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

HWND NSISCALL bgWnd_Init();

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
static HWND m_curwnd, m_bgwnd, m_hwndOK, m_hwndCancel;
static int m_whichcfg;

static BOOL NSISCALL SetDlgItemTextFromLang_(HWND dlg, int id, langid_t lid) {
  return my_SetDialogItemText(dlg,id+1000,STR(GetLangString(lid)));
}

#define SetDlgItemTextFromLang(dlg,id,lid) SetDlgItemTextFromLang_(dlg,(id)-1000,lid)

#define SetUITextFromLang(it,la) SetDlgItemTextFromLang_(hwndDlg,(it)-1000,la)
#define SetUITextNT(it,text) my_SetDialogItemText(hwndDlg,it,text)
#define GetUIText(it,s,ss) GetDlgItemText(hwndDlg,it,s,ss)
#define GetUIItem(it) GetDlgItem(hwndDlg,it)

#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
#define HandleStaticBkColor() _HandleStaticBkColor(uMsg, wParam, lParam)
static BOOL NSISCALL _HandleStaticBkColor(UINT uMsg, WPARAM wParam, LPARAM lParam) {
  if (uMsg == WM_CTLCOLORSTATIC) {
    COLORREF color = GetWindowLong((HWND)lParam, GWL_USERDATA);
    if (color) {
      LOGBRUSH b={BS_SOLID, color-1, 0};
      SetBkColor((HDC)wParam, b.lbColor);
      return (BOOL)CreateBrushIndirect(&b);
    }
  }
  return 0;
}
#else//NSIS_CONFIG_ENHANCEDUI_SUPPORT
#define HandleStaticBkColor() 0
#endif//!NSIS_CONFIG_ENHANCEDUI_SUPPORT

#ifdef NSIS_CONFIG_LOG
void NSISCALL build_g_logfile()
{
  lstrcat(addtrailingslash(mystrcpy(g_log_file,state_install_directory)),"install.log");
}
#endif

#ifdef NSIS_CONFIG_COMPONENTPAGE
static void NSISCALL SetChildrenStates(HWND hWnd, TV_ITEM *pItem, int iState) {
  HTREEITEM hItem;
  int l=0;
  int *def_state;

  pItem->mask|=TVIF_PARAM;

  TreeView_GetItem(hWnd, pItem);
  if (pItem->state >> 12 == 0)
    return;

  def_state=&g_inst_section[pItem->lParam].default_state;

  if (iState < 3 && (*def_state & DFS_RO)) l=3;

  pItem->state = INDEXTOSTATEIMAGEMASK(iState+l);
  pItem->stateMask = TVIS_STATEIMAGEMASK;

  if (!(*def_state & DFS_RO))
  {
    if (iState == 2) *def_state |= DFS_SET;
    else *def_state &= ~DFS_SET;
    TreeView_SetItem(hWnd, pItem);
  }

  hItem = TreeView_GetChild(hWnd, pItem->hItem);
  while (hItem) {
    pItem->hItem = hItem;
    SetChildrenStates(hWnd, pItem, iState);
    hItem = TreeView_GetNextSibling(hWnd, hItem);
  }
}

static void NSISCALL SetParentState(HWND hWnd, TV_ITEM *pItem) {
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
    pItem->mask&=~TVIF_PARAM;
    pItem->state = INDEXTOSTATEIMAGEMASK(iState);
    TreeView_SetItem(hWnd, pItem);
  }

  SetParentState(hWnd, pItem);
}

static void NSISCALL CheckTreeItem(HWND hWnd, TV_ITEM *pItem, int checked) {
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

static int lang_num;

static void NSISCALL set_language()
{
  int i;
  LANGID lang_mask=~(LANGID)0;
  LANGID lang=myatoi(state_language);

lang_again:
  for (i = 0; i < lang_num; i++) {
    if (!((lang ^ common_strings_tables[i].lang_id) & lang_mask)) {
      cur_common_strings_table=common_strings_tables+i;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (g_is_uninstaller)
        (uninstall_strings *)cur_install_strings_table=(uninstall_strings *)install_strings_tables+i;
      else
#endif
        (installer_strings *)cur_install_strings_table=(installer_strings *)install_strings_tables+i;
      break;
    }
  }
  if ((i == lang_num) && (lang_mask == ~(LANGID)0)) {
    lang_mask=0x3ff; // primary lang
    goto lang_again;
  }

  myitoa(state_language, cur_common_strings_table->lang_id);

  SendMessage(m_bgwnd, WM_SETTEXT, 0, (LPARAM)process_string_from_lang(g_caption,LANGID_CAPTION));
}

int NSISCALL ui_doinstall(void)
{
  num_sections=g_inst_header->num_sections;
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

          e=p+mystrlen(p)-4;
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

          mystrcpy(state_install_directory,p);
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

  // Added by Amir Szekely 3rd August 2002
  // Multilingual support
  {
    extern char *g_db_strtab;
    lang_num=g_inst_cmnheader->str_tables_num;
    cur_common_strings_table=common_strings_tables=
      (common_strings*)(g_db_strtab+g_inst_cmnheader->num_string_bytes);
    cur_install_strings_table=install_strings_tables=
      (char*)((unsigned long)common_strings_tables+lang_num*sizeof(common_strings));

    myitoa(state_language, GetUserDefaultLangID());
    set_language();
  }

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (!g_inst_cmnheader->silent_install)
#endif//NSIS_CONFIG_SILENT_SUPPORT
  {
    g_hIcon=LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON2));
    m_bgwnd=GetDesktopWindow();
#ifdef NSIS_SUPPORT_BGBG
    if (g_inst_cmnheader->bg_color1 != -1)
    {
      m_bgwnd=bgWnd_Init();
    }
#endif//NSIS_SUPPORT_BGBG
#ifdef NSIS_SUPPORT_CODECALLBACKS
    g_hwnd=m_bgwnd;
    // Select language
    if (ExecuteCodeSegment(g_inst_cmnheader->code_onInit,NULL)) return 1;
    set_language();
    g_hwnd=NULL;
    ShowWindow(m_bgwnd, SW_SHOW);
#endif//NSIS_SUPPORT_CODECALLBACKS

#ifdef NSIS_CONFIG_LICENSEPAGE
    { // load richedit DLL
      static WNDCLASS wc;
      static char str1[]="RichEd20.dll";
      static char str2[]="RichEdit20A";
      if (!LoadLibrary(str1))
      {
        *(WORD*)(str1+6) = CHAR2_TO_WORD('3','2');
        LoadLibrary(str1);
      }

      // make richedit20a point to RICHEDIT
      if (!GetClassInfo(NULL,str2,&wc))
      {
        str2[8]=0;
        GetClassInfo(NULL,str2,&wc);
        wc.lpszClassName = str2;
        str2[8]='2';
        RegisterClass(&wc);
      }
    }
#endif

    return DialogBox(g_hInstance,MAKEINTRESOURCE(IDD_INST),m_bgwnd,DialogProc);
  }
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  else
#endif
  {
#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (ExecuteCodeSegment(g_inst_cmnheader->code_onInit,NULL)) return 1;
    set_language();
#endif//NSIS_SUPPORT_CODECALLBACKS
    if (install_thread(NULL))
    {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (!g_quit_flag) ExecuteCodeSegment(g_inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
      return 1;
    }
#ifdef NSIS_SUPPORT_CODECALLBACKS
    ExecuteCodeSegment(g_inst_cmnheader->code_onInstSuccess,NULL);
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


BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
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
      m_hwndOK=GetDlgItem(hwndDlg,IDOK);
      m_hwndCancel=GetDlgItem(hwndDlg,IDCANCEL);
      SetDlgItemTextFromLang(hwndDlg,IDC_VERSTR,LANGID_BRANDING);
      SetClassLong(hwndDlg,GCL_HICON,(long)g_hIcon);
      SetDlgItemTextFromLang(hwndDlg,IDCANCEL,LANGID_BTN_CANCEL);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      if (!g_is_uninstaller)
#endif
        SetDlgItemTextFromLang(hwndDlg,IDC_BACK,LANGID_BTN_BACK);
      ShowWindow(hwndDlg,SW_SHOW);
    }

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (g_is_uninstaller)
    {
      islp = (LANG_UNINST_TEXT>=0);
      iscp++;
    }
    else
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
    {
#ifdef NSIS_CONFIG_LICENSEPAGE
      islp = (LANG_LICENSE_DATA>=0);
#endif//NSIS_CONFIG_LICENSEPAGE
#ifdef NSIS_CONFIG_COMPONENTPAGE
      iscp = (LANG_COMP_TEXT>=0);
#endif//NSIS_CONFIG_COMPONENTPAGE
      ispotentiallydp = (LANG_DIR_TEXT>=0);
      if (ispotentiallydp &&
          !((g_inst_cmnheader->misc_flags&2) &&
            is_valid_instpath(state_install_directory)
#ifdef NSIS_SUPPORT_CODECALLBACKS
            && !ExecuteCodeSegment(g_inst_header->code_onVerifyInstDir,NULL)
#endif//NSIS_SUPPORT_CODECALLBACKS
            )) isdp++;
    }

    if (m_page<=0) delta=1;
    do
    {
      int count=1;  // Number of pages to move by
#ifdef NSIS_SUPPORT_CODECALLBACKS
      // Call onNext|PrevPage for every not-definitely-disabled page
      if (ExecuteCodeSegment(delta>0?g_inst_cmnheader->code_onNextPage:g_inst_header->code_onPrevPage,NULL))
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
    if (m_page>g_max_page) ExecuteCodeSegment(g_inst_cmnheader->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    if (m_curwnd && (m_page!=prev_page))
    {
      DestroyWindow(m_curwnd);
      m_curwnd=0;
    }

    if (m_page < 0 || m_page > g_max_page)
      EndDialog(hwndDlg,0);
    else if (!m_curwnd)
    {
      HWND hwndtmp;
      langid_t langid =
        (m_page == g_max_page) ? LANGID_BTN_CLOSE :
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
        g_is_uninstaller ? LANGID_BTN_UNINST :
#endif
#ifdef NSIS_CONFIG_LICENSEPAGE
        (m_page == 0) ? LANGID_BTN_LICENSE :
#endif
        (m_page == 2 || (m_page == 1 && !isdp)) ? LANGID_BTN_INSTALL :
        LANGID_BTN_NEXT;
      SetDlgItemTextFromLang(hwndDlg,IDOK,langid);
      mystrcpy(g_tmp,g_caption);
      process_string_from_lang(g_tmp+mystrlen(g_tmp),LANGID_SUBCAPTION(m_page));

      SetWindowText(hwndDlg,g_tmp);

      gDontFookWithFocus = 0;
      m_curwnd=CreateDialog(g_hInstance,windows[g_page_offs+m_page].id,hwndDlg,windows[g_page_offs+m_page].proc);
      if (m_curwnd)
      {
        RECT r;
        GetWindowRect(GetDlgItem(hwndDlg,IDC_CHILDRECT),&r);
        ScreenToClient(hwndDlg,(LPPOINT)&r);
        SetWindowPos(m_curwnd,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
#ifdef NSIS_SUPPORT_CODECALLBACKS
        ExecuteCodeSegment(g_inst_cmnheader->code_onInitDialog,NULL);
#endif //NSIS_SUPPORT_CODECALLBACKS
        SendMessage(m_curwnd, WM_NOTIFY_START, 0, 0);
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
//XGE 5th September 2002 - Do *not* move the focus to the OK button if we are
//on the license page, instead we want the focus left alone because in
//WM_INITDIALOG it is given to the richedit control.
      if (!gDontFookWithFocus)
          SetFocus(m_hwndOK);
//XGE End
    }
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam);

    if (id == IDOK && m_curwnd)
    {
      outernotify(1);
    }
    if (
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      !g_is_uninstaller &&
#endif
      (id == IDC_BACK && m_curwnd && m_page>0))
    {
      EnableWindow(m_hwndOK, TRUE);
      outernotify(-1);
    }
    if (id == IDCANCEL)
    {
      if (m_abort)
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        ExecuteCodeSegment(g_inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
        EndDialog(hwndDlg,2);
      }
      else
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (!ExecuteCodeSegment(g_inst_cmnheader->code_onUserAbort,NULL))
#endif//NSIS_SUPPORT_CODECALLBACKS
        {
          EndDialog(hwndDlg,1);
        }
      }
    }
  }
  if (uMsg == WM_CLOSE)
  {
    if (!IsWindowEnabled(m_hwndCancel) && IsWindowEnabled(m_hwndOK))
      SendMessage(hwndDlg,WM_COMMAND,IDOK,0);
  }
  return HandleStaticBkColor();
}

#ifdef NSIS_CONFIG_LICENSEPAGE
// Changed by Amir Szekely 27th July 2002
#define _RICHEDIT_VER 0x0200
#include <RichEdit.h>
#undef _RICHEDIT_VER
static DWORD dwRead;
DWORD CALLBACK StreamLicense(DWORD dwCookie, LPBYTE pbBuff, LONG cb, LONG *pcb)
{
  lstrcpyn(pbBuff,(char*)dwCookie+dwRead,cb);
  dwRead+=mystrlen(pbBuff);
  *pcb=mystrlen(pbBuff);
  return 0;
}

static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HWND hwLicense;
  if (uMsg == WM_INITDIALOG)
  {
    EDITSTREAM es={(DWORD)STR(LANG_LICENSE_DATA),0,StreamLicense};
    hwLicense=GetUIItem(IDC_EDIT1);
    SendMessage(hwLicense,EM_AUTOURLDETECT,TRUE,0);
    SendMessage(hwLicense,EM_SETBKGNDCOLOR,0,g_inst_header->license_bg>=0?g_inst_header->license_bg:GetSysColor(COLOR_BTNFACE));
    SendMessage(hwLicense,EM_SETEVENTMASK,0,ENM_LINK|ENM_KEYEVENTS); //XGE 8th September 2002 Or'd in ENM_KEYEVENTS
    dwRead=0;
    SendMessage(hwLicense,EM_STREAMIN,(((char*)es.dwCookie)[0]=='{')?SF_RTF:SF_TEXT,(LPARAM)&es);
    SetUITextFromLang(IDC_INTROTEXT,LANGID_LICENSE_TEXT);
    //XGE 5th September 2002 - place the initial focus in the richedit control
    gDontFookWithFocus++;
    SetFocus(hwLicense);
    return FALSE;
    //End Xge
  }
  else if (uMsg == WM_NOTIFY) {
    #define nmhdr ((NMHDR *)lParam)
    #define enlink ((ENLINK *)lParam)
    #define msgfilter ((MSGFILTER *)lParam)
    if (nmhdr->code==EN_LINK) {
      if (enlink->msg==WM_LBUTTONDOWN) {
        char *szUrl;
        long min=enlink->chrg.cpMin, max=enlink->chrg.cpMax;
        SendMessage(hwLicense,EM_SETSEL,min,max);
        szUrl=(char *)my_GlobalAlloc(max-min+1);
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
    //Ximon Eighteen 8th September 2002 Capture return key presses in the rich
    //edit control now that the control gets the focus rather than the default
    //push button. When the user presses return ask the outer dialog to move
    //the installer onto the next page. MSDN docs say return non-zero if the
    //rich edit control should NOT process this message, hence the return 1.
    else if (nmhdr->code==EN_MSGFILTER)
    {
      if (msgfilter->msg==WM_KEYDOWN &&
          msgfilter->wParam==VK_RETURN)
      {
        outernotify(1);
        return 1;
      }
    }
  }
  else if (uMsg == WM_CLOSE) {
    SendMessage(g_hwnd,WM_CLOSE,0,0);
  }
  return HandleStaticBkColor();
}
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    SetUITextFromLang(IDC_INTROTEXT,LANGID_UNINST_TEXT);
    SetUITextFromLang(IDC_UNINSTFROM,LANGID_UNINST_SUBTEXT);
    SetUITextNT(IDC_EDIT1,state_install_directory);
  }
  return HandleStaticBkColor();
}
#endif


static void NSISCALL inttosizestr(int kb, char *str)
{
  char sh=20;
  char c='G';
  char *s="";
  if (kb < 1024) { sh=0; c='K'; }
  else if (kb < 1024*1024) { sh=10; c='M'; }
  else if (GetVersion()&0x80000000) s="+";
  wsprintf(str+mystrlen(str),"%d.%d%cB%s",kb>>sh,((kb*10)>>sh)%10,c,s);
}

static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_DESTROY)
  {
    GetUIText(IDC_DIR,state_install_directory,NSIS_MAX_STRLEN);
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
      HWND h=GetUIItem(IDC_CHECK1);
      SetWindowText(h,"Log install process");
      ShowWindow(h,SW_SHOWNA);
    }
#endif
    SetUITextNT(IDC_DIR,state_install_directory);
    SetUITextFromLang(IDC_INTROTEXT,LANGID_DIR_TEXT);
    SetUITextFromLang(IDC_BROWSE,LANGID_BTN_BROWSE);
    SetUITextFromLang(IDC_SELDIRTEXT,LANGID_DIR_SUBTEXT);
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
      GetUIText(IDC_DIR,name,256);
      GetUIText(IDC_SELDIRTEXT,str,256);
      bi.hwndOwner = hwndDlg;
      bi.pszDisplayName = name;
      bi.lpfn=BrowseCallbackProc;
      bi.lParam=(LPARAM)hwndDlg;
      bi.lpszTitle=str;
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
          p=name+mystrlen(name)-mystrlen(post_str);
          if (p <= name || *CharPrev(name,p)!='\\' || lstrcmpi(p,post_str))
          {
            lstrcat(addtrailingslash(name),post_str);
          }
        }

        SetUITextNT(IDC_DIR,name);
      }
    }
  }
  if (uMsg == WM_IN_UPDATEMSG || uMsg == WM_NOTIFY_START)
  {
    static char s[NSIS_MAX_STRLEN];
    int is_valid_path;
    int x;
    int total=0, available=-1;
    DWORD spc,bps,fc,tc;

    GetUIText(IDC_DIR,state_install_directory,NSIS_MAX_STRLEN);
    is_valid_path=is_valid_instpath(state_install_directory);

    mini_memcpy(s,state_install_directory,NSIS_MAX_STRLEN);
    s[sizeof(s)-1]=0;
    if (s[1] == ':') s[3]=0;
    else if (*(WORD*)s == CHAR2_TO_WORD('\\','\\')) // \\ path
    {
      addtrailingslash(s);
    }

    if (GetDiskFreeSpace(s,&spc,&bps,&fc,&tc))
    {
      DWORD r=MulDiv(bps*spc,fc,1<<10);
      if (r > 0x7fffffff) r=0x7fffffff;
      available=(int)r;
    }
    for (x = 0; x < num_sections; x ++)
    {
#ifdef NSIS_CONFIG_COMPONENTPAGE
      if (g_inst_section[x].default_state&DFS_SET)
#endif
       total+=g_inst_section[x].size_kb;
    }

    // Added by Amir Szekely 24th July 2002
    // Allows 'SpaceTexts none'
    if (LANG_SPACE_REQ >= 0) {
      inttosizestr(total,mystrcpy(s,STR(LANG_SPACE_REQ)));
      SetUITextNT(IDC_SPACEREQUIRED,s);
      if (available != -1)
      {
        inttosizestr(available,mystrcpy(s,STR(LANG_SPACE_AVAIL)));
        SetUITextNT(IDC_SPACEAVAILABLE,s);
      }
      else
        SetUITextNT(IDC_SPACEAVAILABLE,"");
    }

    EnableWindow(m_hwndOK,
      is_valid_path && (available >= total || available == -1)
#ifdef NSIS_SUPPORT_CODECALLBACKS
      && !ExecuteCodeSegment(g_inst_header->code_onVerifyInstDir,NULL)
#endif
      );
  }
  return HandleStaticBkColor();
}

#ifdef NSIS_CONFIG_COMPONENTPAGE

TVHITTESTINFO NSISCALL hit_test(HWND tree)
{
  static TVHITTESTINFO ht;
  DWORD dwpos = GetMessagePos();

  ht.pt.x = GET_X_LPARAM(dwpos);
  ht.pt.y = GET_Y_LPARAM(dwpos);
  MapWindowPoints(HWND_DESKTOP, tree, &ht.pt, 1);

  TreeView_HitTest(tree, &ht);

  return ht;
}

static LONG oldTreeWndProc;
static DWORD WINAPI newTreeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static LPARAM last_item=-1;
  if (uMsg == WM_KEYDOWN && wParam == VK_SPACE)
  {
    SendMessage(m_curwnd,WM_TREEVIEW_KEYHACK,0,0);
    return 0;
  }
#ifdef NSIS_SUPPORT_CODECALLBACKS
  if (uMsg == WM_DESTROY) {
    last_item=-1;
  }
  if (uMsg == WM_MOUSEMOVE) {
    TVHITTESTINFO ht = hit_test(hwnd);
    if (ht.flags & (TVHT_ONITEMSTATEICON|TVHT_ONITEMLABEL|TVHT_ONITEMRIGHT|TVHT_ONITEM))
    {
      TVITEM hItem;

      hItem.hItem = ht.hItem;
      hItem.mask = TVIF_PARAM;

      TreeView_GetItem(hwnd, &hItem);

      lParam = hItem.lParam;
      uMsg = WM_USER+0x19;
    }
  }
  if (uMsg == WM_USER+0x19) {
    if (last_item != lParam)
    {
      last_item = lParam;

      mystrcpy(g_tmp, g_usrvars[0]);

      myitoa(g_usrvars[0], last_item);
      ExecuteCodeSegment(g_inst_header->code_onMouseOverSection,NULL);

      mystrcpy(g_usrvars[0], g_tmp);
    }
  }
#endif//NSIS_SUPPORT_CODECALLBACKS
  return CallWindowProc((WNDPROC)oldTreeWndProc,hwnd,uMsg,wParam,lParam);
}

int m_num_insttypes;

static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HTREEITEM *hTreeItems;
  static HIMAGELIST hImageList;
  HWND hwndCombo1 = GetUIItem(IDC_COMBO1);
  HWND hwndTree1 = GetUIItem(IDC_TREE1);
  if (uMsg == WM_INITDIALOG)
  {
    int doLines=0;
    HTREEITEM Par;
    HBITMAP hBMcheck1;
    int x;
    if (hTreeItems) GlobalFree(hTreeItems);
    hTreeItems=(HTREEITEM*)my_GlobalAlloc(sizeof(HTREEITEM)*num_sections);

    hBMcheck1=LoadBitmap(g_hInstance, MAKEINTRESOURCE(IDB_BITMAP1));
    SetUITextFromLang(IDC_INTROTEXT,LANGID_COMP_TEXT);
    SetUITextFromLang(IDC_TEXT1,LANGID_COMP_SUBTEXT(0));
    SetUITextFromLang(IDC_TEXT2,LANGID_COMP_SUBTEXT(1));

    oldTreeWndProc=SetWindowLong(hwndTree1,GWL_WNDPROC,(DWORD)newTreeWndProc);

    if (hImageList) ImageList_Destroy(hImageList);

    hImageList = ImageList_Create(16,16, ILC_COLOR32|ILC_MASK, 6, 0);
    ImageList_AddMasked(hImageList,hBMcheck1,RGB(255,0,255));

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

    for (x = 0; x < num_sections; x ++)
    {
      if (g_inst_section[x].name_ptr>=0)
      {
        TVINSERTSTRUCT tv;
        tv.hParent=Par;
        tv.hInsertAfter=TVI_LAST;
        tv.item.mask=TVIF_PARAM|TVIF_TEXT|TVIF_STATE;
        tv.item.lParam=x;
        tv.item.pszText=process_string_fromtab(ps_tmpbuf,g_inst_section[x].name_ptr);
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

        if (ps_tmpbuf[0] == '!' || *(WORD*)ps_tmpbuf == CHAR2_TO_WORD('-','!'))
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
    for (x = 0; x < num_sections; x ++)
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

    uMsg=WM_IN_UPDATEMSG;
  }
  if (uMsg == WM_USER+0x17)
  {
    int x=wParam;
    int ns=lParam;

    if (g_inst_section[x].name_ptr>=0 && ns >= 0)
    {
      TVITEM tv;
      tv.hItem=hTreeItems[x];
      tv.mask=TVIF_TEXT;
      tv.pszText=process_string_fromtab(ps_tmpbuf,ns);
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
           ht=hit_test(hwndTree1);
        }
        else
        {
          ht.hItem=TreeView_GetSelection(hwndTree1);
          if (ht.hItem) ht.flags=TVHT_ONITEMSTATEICON;
        }

        if ((TVHT_ONITEMSTATEICON|TVHT_ONITEMLABEL|TVHT_ONITEMRIGHT|TVHT_ONITEM) & ht.flags)
        {
          TVITEM hItem;
          hItem.hItem = ht.hItem;

          hItem.mask = TVIF_STATE|TVIF_PARAM;
          TreeView_GetItem(hwndTree1, &hItem);

          if (!(g_inst_section[hItem.lParam].default_state&DFS_RO))
          {
            if ((hItem.state >> 12) == 2) // already checked
            {
              g_inst_section[hItem.lParam].default_state&=~DFS_SET;
              CheckTreeItem(hwndTree1,&hItem,0);
            }
            else
            {
              g_inst_section[hItem.lParam].default_state|=DFS_SET;
              CheckTreeItem(hwndTree1,&hItem,1);
            }
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_COMPONENTPAGE)
            {
              extern HWND g_SectionHack;
              g_SectionHack=hwndDlg;
              ExecuteCodeSegment(g_inst_header->code_onSelChange,NULL);
              g_SectionHack=0;
            }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_COMPONENTPAGE
            {
              int r,x;
              // check to see which install type we are
              for (r = 0; r < m_num_insttypes; r ++)
              {
                HTREEITEM *ht=hTreeItems;
                section *t=g_inst_section;
                x=num_sections;
                while (x--)
                {
                  char c=GetStringFromStringTab(t->name_ptr)[0];
                  if (c && c!='-')
                  {
                    TV_ITEM hItem;
                    hItem.hItem=*ht;
                    if (g_inst_header->no_custom_instmode_flag==1)
                    {
                      int c=(t->default_state>>m_whichcfg)&1;
                      CheckTreeItem(hwndTree1, &hItem,c);
                    }
                    else if (!(t->default_state&DFS_RO))
                    {
                      hItem.mask=TVIF_STATE;
                      TreeView_GetItem(hwndTree1,&hItem);
                      if (!(t->default_state&(1<<r)) != !((hItem.state>>12)>1 )) break;
                    }
                  }
                  t++;
                  ht++;
                }
                if (x < 0) break;
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
#ifdef NSIS_SUPPORT_CODECALLBACKS
      else if (lpnmh->code == TVN_SELCHANGED) {
        SendMessage(hwndTree1, WM_USER+0x19, 0, ((LPNMTREEVIEW)lpnmh)->itemNew.lParam);
      }
#endif//NSIS_SUPPORT_CODECALLBACKS
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
          int x=num_sections;
          section *t=g_inst_section;
          HTREEITEM *ht=hTreeItems;
          while (x--)
          {
            if (t->name_ptr>=0 && !(t->default_state & DFS_RO))
            {
              TVITEM tv;
              int l=1;

              if (t->default_state & (1<<m_whichcfg))
              {
                l++;
                t->default_state|=DFS_SET;
              }
              else t->default_state&=~DFS_SET;

              // this can't happen because of the above if()
              //if (t->default_state & DFS_RO) l+=3;

              tv.hItem=*ht;
              tv.mask=TVIF_STATE;
              tv.state=INDEXTOSTATEIMAGEMASK(l);
              tv.stateMask = TVIS_STATEIMAGEMASK;

              TreeView_SetItem(hwndTree1,&tv);
              SetParentState(hwndTree1,&tv);
            }
            t++;
            ht++;
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
      ShowWindow(GetUIItem(IDC_TEXT2),c);
    }

    if (LANG_SPACE_REQ >= 0) {
      int x,total;
      char s[128];
      for (total=x=0; x < num_sections; x ++)
      {
        if (g_inst_section[x].default_state&DFS_SET)
          total+=g_inst_section[x].size_kb;
      }
      inttosizestr(total,mystrcpy(s,STR(LANG_SPACE_REQ)));
      SetUITextNT(IDC_SPACEREQUIRED,s);
    }
  }
  return HandleStaticBkColor();
}
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//NSIS_CONFIG_VISIBLE_SUPPORT

int ui_st_updateflag=0x3;

void NSISCALL update_status_text_from_lang(langid_t id, const char *text2)
{
  update_status_text(STR(GetLangString(id)), text2);
}

void NSISCALL update_status_text(const char *text1, const char *text2)
{
  static char tmp[NSIS_MAX_STRLEN];
  static LVITEM new_item = {LVIF_TEXT,0,0,0,0,tmp};
  if (insthwnd)
  {
    if (mystrlen(text1)+mystrlen(text2) >= sizeof(tmp)) return;
    wsprintf(tmp,"%s%s",text1,text2);
    if ((ui_st_updateflag&1))
    {
      // Changed by Amir Szekely 26th July 2002
      new_item.iItem=ListView_GetItemCount(insthwnd);
      ListView_InsertItem(insthwnd, &new_item);
      ListView_EnsureVisible(insthwnd, new_item.iItem, 0);
    }
    if ((ui_st_updateflag&2)) SetWindowText(insthwnd2,tmp);
  }
}


static DWORD WINAPI install_thread(LPVOID p)
{
  HWND hwndDlg=(HWND)p;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    if (ExecuteCodeSegment(g_inst_uninstheader->code,g_progresswnd)) m_abort++;
  }
  else
  {
#endif
    int m_inst_sec=0;
    while (m_inst_sec<num_sections && !m_abort)
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
        if (ExecuteCodeSegment(g_inst_section[m_inst_sec].code,g_progresswnd)) m_abort++;
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
    RECT r;
    int num=0;
    int x=0;
    LVCOLUMN lvc = {0, 0, -1, 0, 0, -1};
    int lb_bg=g_inst_cmnheader->lb_bg,lb_fg=g_inst_cmnheader->lb_fg;

    insthwndbutton=GetUIItem(IDC_SHOWDETAILS);
    insthwnd2=GetUIItem(IDC_INTROTEXT);
    insthwnd=GetUIItem(IDC_LIST1);
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    if (g_is_uninstaller)
    {
      num=g_inst_uninstheader->code_size;
    }
    else
#endif
    {
      log_printf3("New install of \"%s\" to \"%s\"",STR(LANG_NAME),state_install_directory);
      for (; x < num_sections; x ++)
      {
#ifdef NSIS_CONFIG_COMPONENTPAGE
        if (g_inst_section[x].default_state&DFS_SET)
#endif
          num+=g_inst_section[x].code_size;
      }
    }
    // Changed by Amir Szekely 26th July 2002
    ListView_InsertColumn(insthwnd, 0, &lvc);
    GetClientRect(insthwnd,&r);
    ListView_SetColumnWidth(insthwnd, 0, r.right-r.left-GetSystemMetrics(SM_CXHSCROLL));
#define LVS_EX_LABELTIP         0x00004000 // listview unfolds partly hidden labels if it does not have infotip text
    ListView_SetExtendedListViewStyleEx(insthwnd, LVS_EX_LABELTIP, LVS_EX_LABELTIP);
    if (lb_bg >= 0) {
      ListView_SetBkColor(insthwnd, lb_bg);
      ListView_SetTextBkColor(insthwnd, lb_bg);
    }
    if (lb_fg >= 0) {
      ListView_SetTextColor(insthwnd, lb_fg);
    }
    SetWindowText(insthwndbutton,STR(LANG_BTN_DETAILS));
    if (g_inst_cmnheader->show_details)
    {
      ShowWindow(insthwndbutton,SW_HIDE);
      if (g_inst_cmnheader->show_details != 2) ShowWindow(insthwnd,SW_SHOWNA);
      else insthwndbutton=NULL;
    }
    progress_bar_len=num;

    g_progresswnd=GetUIItem(IDC_PROGRESS1+(g_inst_cmnheader->progress_flags&1));
    ShowWindow(g_progresswnd,SW_SHOWNA);
    SendMessage(g_progresswnd,PBM_SETRANGE,0,MAKELPARAM(0,30000));
    if (g_inst_cmnheader->progress_flags&2)
    {
      SendMessage(g_progresswnd,PBM_SETBARCOLOR,0,lb_fg);
      SendMessage(g_progresswnd,PBM_SETBKCOLOR,0,lb_bg);
    }

    EnableWindow(m_hwndOK,0);
    EnableWindow(m_hwndCancel,0);
  }
  if (uMsg == WM_NOTIFY_START) {
    DWORD id;
    CloseHandle(CreateThread(NULL,0,install_thread,(LPVOID)hwndDlg,0,&id));
  }
  if (uMsg == WM_COMMAND && LOWORD(wParam) == IDC_SHOWDETAILS)
  {
    ShowWindow(GetUIItem(IDC_SHOWDETAILS),SW_HIDE);
    SendMessage(insthwnd,WM_VSCROLL,SB_BOTTOM,0);
    ShowWindow(insthwnd,SW_SHOWNA);
  }
  if (uMsg == WM_NOTIFY_INSTPROC_DONE)
  {
    if (g_quit_flag)
        EndDialog(g_hwnd,1);
    else if (!wParam)
    {
      HWND h=m_hwndOK;
      EnableWindow(h,1);
      if (!g_autoclose)
      {
        ShowWindow(g_hwnd,SW_SHOWNA);
        mystrcpy(g_tmp,g_caption);
        process_string_from_lang(g_tmp+mystrlen(g_tmp),LANGID_SUBCAPTION(g_max_page+1));
        update_status_text_from_lang(LANGID_COMPLETED,"");
        SetWindowText(g_hwnd,g_tmp);
        SetFocus(h);
      }
      else
      {
        outernotify(1);
      }
    }
    else
    {
      HWND h=m_hwndCancel;
      EnableWindow(h,1);
      SetFocus(h);
    }
  }
  //>>>Ximon Eighteen aka Sunjammer 30th August 2002
  //+++Popup "Copy Details To Clipboard" menu when RMB clicked in DetailView
  //+++Currently this has no language support for the popup menu tex
  if (uMsg == WM_NOTIFY && ((NMHDR*)lParam)->code == NM_RCLICK)
  {
    int count = ListView_GetItemCount(insthwnd);
    if (count > 0)
    {
      DWORD pos  = GetMessagePos();
      HMENU menu = CreatePopupMenu();
      AppendMenu(menu,MF_STRING,1,GetStringFromStringTab(GetLangString(LANGID_COPYDETAILS)));
    	if (1==TrackPopupMenu(
        menu,
        TPM_NONOTIFY|TPM_RETURNCMD,
        GET_X_LPARAM(pos),
        GET_Y_LPARAM(pos),
        0,insthwnd,0))
      {
        char textBuf[1024];
        int i,total = 0;
        LVITEM item;
        HGLOBAL memory;
        LPTSTR ptr,endPtr;

        // 1st pass - determine clipboard memory required.
        item.iSubItem   = 0;
        item.pszText    = textBuf;
        item.cchTextMax = 1023;
        for (i = 0; i < count; i++)
        {
          // Add 2 for the CR/LF combination that must follow every line.
          total += 2+SendMessage(insthwnd,LVM_GETITEMTEXT,i,(LPARAM)&item);
        }

        // 2nd pass - store detail view strings on the clipboard
        // Clipboard MSDN docs say mem must be GMEM_MOVEABLE
        OpenClipboard(0);
        EmptyClipboard();
        memory = GlobalAlloc(GMEM_MOVEABLE,total+1);
        ptr = GlobalLock(memory);
        endPtr = ptr+total+1;
        for (i = 0; i < count; i++)
        {
          // -2 to allow for CR/LF
          ListView_GetItemText(insthwnd,i,0,ptr,(endPtr-2)-ptr);
          while (*ptr) ptr++;
          *(WORD*)ptr = CHAR2_TO_WORD('\r','\n');
          ptr+=2;
        }
        *ptr++ = 0;
        GlobalUnlock(memory);
        SetClipboardData(CF_TEXT,memory);
        CloseClipboard();
      }
    }
  }
  //<<<
  return HandleStaticBkColor();
}
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
