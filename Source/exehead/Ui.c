/*
*  Copyright (C) 1999-2003 Nullsoft, Inc.
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

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
HICON g_hIcon;
#endif

int dlg_offset;
int ui_dlg_visible=0; // At start main window is not visible
int g_quit_flag; // set when Quit has been called (meaning bail out ASAP)

#if NSIS_MAX_INST_TYPES > 32 || NSIS_MAX_INST_TYPES < 1
#error invalid value for NSIS_MAX_INST_TYPES
#endif

int progress_bar_pos, progress_bar_len;

static char g_tmp[4096];

static int m_page=-1,m_retcode,m_delta=1;
static page *g_this_page;

#define NOTIFY_BYE_BYE 'x'

static void NSISCALL outernotify(char num) {
  if (num==NOTIFY_BYE_BYE)
    g_quit_flag++;
  m_delta=num;
  SendMessage(g_hwnd,WM_NOTIFY_OUTER_NEXT,(WPARAM)num,0); // it sends num again for plugins - DON'T REMOVE!
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static int CALLBACK WINAPI BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData);
#ifdef NSIS_CONFIG_LICENSEPAGE
static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif
static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK InstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif//NSIS_CONFIG_VISIBLE_SUPPORT

static DWORD WINAPI install_thread(LPVOID p);

HWND insthwnd, insthwnd2, insthwndbutton;

HWND m_curwnd;
static HWND m_bgwnd, m_hwndOK, m_hwndCancel;

static BOOL NSISCALL SetDlgItemTextFromLang_(HWND dlg, int id, int lid) {
  return my_SetDialogItemText(dlg,id+1000,GetNSISStringTT(lid));
}

static void NSISCALL SetNextDef()
{
  SendMessage(g_exec_flags.abort ? m_hwndCancel : m_hwndOK, BM_SETSTYLE, BS_DEFPUSHBUTTON, TRUE);
}

static void NSISCALL EnableNext(BOOL e)
{
  EnableWindow(m_hwndOK, e);
}

static void NSISCALL SetActiveCtl(HWND hDlg, HWND hCtl)
{
  SendMessage(hDlg, WM_NEXTDLGCTL, (WPARAM) hCtl, TRUE);
}

static void NSISCALL NotifyCurWnd(UINT uNotifyCode)
{
  if (m_curwnd)
    SendMessage(m_curwnd, uNotifyCode, 0, 0);
}

#define SetDlgItemTextFromLang(dlg,id,lid) SetDlgItemTextFromLang_(dlg,(id)-1000,lid)

#define SetUITextFromLang(it,la) SetDlgItemTextFromLang_(hwndDlg,(it)-1000,la)
#define SetUITextNT(it,text) my_SetDialogItemText(hwndDlg,it,text)
#define GetUIText(it,s) my_GetDialogItemText(it,s)
#define GetUIItem(it) GetDlgItem(hwndDlg,it)

#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
#define HandleStaticBkColor() _HandleStaticBkColor(uMsg, wParam, lParam)
static BOOL NSISCALL _HandleStaticBkColor(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if ((uMsg - WM_CTLCOLOREDIT) <= (WM_CTLCOLORSTATIC - WM_CTLCOLOREDIT))
  {
    ctlcolors *c = (ctlcolors *)GetWindowLong((HWND)lParam, GWL_USERDATA);

    if (c) {
      COLORREF text;
      LOGBRUSH lh;

      text = c->text;
      if (c->flags & CC_TEXT_SYS)
        text = GetSysColor(text);
      if (c->flags & CC_TEXT)
        SetTextColor((HDC)wParam, text);

      SetBkMode((HDC)wParam, c->bkmode);

      lh.lbColor = c->bkc;
      if (c->flags & CC_BK_SYS)
        lh.lbColor = GetSysColor(lh.lbColor);
      if (c->flags & CC_BK)
        SetBkColor((HDC)wParam, lh.lbColor);

      if (c->flags & CC_BKB)
      {
        lh.lbStyle = c->lbStyle;
        if (c->bkb)
          DeleteObject(c->bkb);
        c->bkb = CreateBrushIndirect(&lh);
      }

      return (BOOL)c->bkb;
    }
  }
  return 0;
}
#else
#define HandleStaticBkColor() 0
#endif//!NSIS_CONFIG_ENHANCEDUI_SUPPORT

#ifdef NSIS_CONFIG_LOG
#ifndef NSIS_CONFIG_LOG_ODS
void NSISCALL build_g_logfile()
{
  lstrcat(addtrailingslash(mystrcpy(g_log_file,state_install_directory)),"install.log");
}
#endif
#endif

int *cur_langtable;

static void NSISCALL set_language()
{
  LANGID lang_mask=(LANGID)~0;
  LANGID lang=state_language[0]?myatoi(state_language):GetUserDefaultLangID();
  char *language_table=0;
  int lang_num;

lang_again:
  lang_num=g_blocks[NB_LANGTABLES].num;
  while (lang_num--) {
    language_table=((char*)g_blocks[NB_LANGTABLES].offset)+lang_num*g_header->langtable_size;
    if (!((lang ^ *(LANGID*)language_table) & lang_mask)) {
      dlg_offset=*(int*)(language_table+sizeof(LANGID));
      g_exec_flags.rtl=*(int*)(language_table+sizeof(LANGID)+sizeof(int));
      cur_langtable=(int*)(language_table+sizeof(LANGID)+2*sizeof(int));
      break;
    }
  }
  if (!cur_langtable) {
    if (lang_mask == (LANGID)~0)
      lang_mask=0x3ff; // primary lang
    else // we already tried once and we still don't have a language table
      lang_mask=0; // first lang
    goto lang_again;
  }

  myitoa(state_language, *(LANGID*)language_table);
  {
    char *caption = GetNSISString(g_caption,LANG_CAPTION);
#ifdef NSIS_SUPPORT_BGBG
    my_SetWindowText(m_bgwnd, caption);
#endif
  }
}

__forceinline int NSISCALL ui_doinstall(void)
{
  header *header = g_header;
  static WNDCLASS wc; // richedit subclassing and bgbg creation
  g_exec_flags.autoclose=g_flags&CH_FLAGS_AUTO_CLOSE;

  set_language();

  if (!is_valid_instpath(state_install_directory))
  {
    if (header->install_reg_key_ptr)
    {
      myRegGetStr(
        (HKEY)header->install_reg_rootkey,
        GetNSISStringNP(header->install_reg_key_ptr),
        GetNSISStringNP(header->install_reg_value_ptr),
        ps_tmpbuf
      );
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
          if (!lstrcmpi(e, ".exe")) // check extension
          {
            DWORD d;
            d=GetFileAttributes(p);
            if (d == (DWORD)-1 || !(d&FILE_ATTRIBUTE_DIRECTORY))
            {
              // if there is no back-slash, the string will become empty, but that's ok because
              // it would make an invalid instdir anyway
              trimslashtoend(p);
            }
          }
        }

        mystrcpy(state_install_directory,p);
      }
    }
  }
  if (!is_valid_instpath(state_install_directory))
  {
    GetNSISString(state_install_directory,header->install_directory_ptr);
  }

#ifdef NSIS_CONFIG_LOG
  if (g_flags & CH_FLAGS_SILENT_LOG && !g_is_uninstaller)
  {
#ifndef NSIS_CONFIG_LOG_ODS
    build_g_logfile();
#endif
    log_dolog=1;
  }
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  g_hIcon=LoadImage(g_hInstance,MAKEINTRESOURCE(IDI_ICON2),IMAGE_ICON,0,0,LR_DEFAULTSIZE|LR_SHARED);
#ifdef NSIS_SUPPORT_BGBG
  if (header->bg_color1 != -1)
  {
    RECT vp;
    extern LRESULT CALLBACK BG_WndProc(HWND, UINT, WPARAM, LPARAM);
    wc.lpfnWndProc = BG_WndProc;
    wc.hInstance = g_hInstance;
    wc.hIcon = g_hIcon;
    //wc.hCursor = LoadCursor(NULL,IDC_ARROW);
    wc.lpszClassName = "_Nb";

    if (!RegisterClass(&wc)) return 0;

    SystemParametersInfo(SPI_GETWORKAREA, 0, &vp, 0);

    m_bgwnd = CreateWindowEx(WS_EX_TOOLWINDOW,"_Nb",0,WS_POPUP,
      vp.left,vp.top,vp.right-vp.left,vp.bottom-vp.top,0,NULL,g_hInstance,NULL);
  }

#ifdef NSIS_SUPPORT_CODECALLBACKS
  g_hwnd=m_bgwnd;
#endif//NSIS_SUPPORT_CODECALLBACKS

#endif//NSIS_SUPPORT_BGBG

#endif//NSIS_CONFIG_VISIBLE_SUPPORT

#ifdef NSIS_SUPPORT_CODECALLBACKS
  // Select language
  if (ExecuteCodeSegment(header->code_onInit,NULL)) return 1;
  set_language();
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT

#ifdef NSIS_SUPPORT_CODECALLBACKS
#ifdef NSIS_SUPPORT_BGBG
  g_hwnd=NULL;
#endif//NSIS_SUPPORT_BGBG
#endif//NSIS_SUPPORT_CODECALLBACKS

#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (!g_exec_flags.silent)
#endif//NSIS_CONFIG_SILENT_SUPPORT
  {
#ifdef NSIS_SUPPORT_BGBG
    ShowWindow(m_bgwnd, SW_SHOW);
#endif//NSIS_SUPPORT_BGBG

#ifdef NSIS_CONFIG_LICENSEPAGE
    { // load richedit DLL
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

    {
      int ret=DialogBox(g_hInstance,MAKEINTRESOURCE(IDD_INST+dlg_offset),0,DialogProc);
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
      ExecuteCodeSegment(header->code_onGUIEnd,NULL);
#endif
      return ret;
    }
  }
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  else
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
  {
    if (install_thread(NULL))
    {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (!g_quit_flag) ExecuteCodeSegment(header->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
      return 1;
    }
#ifdef NSIS_SUPPORT_CODECALLBACKS
    ExecuteCodeSegment(header->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    return 0;
  }
#endif//NSIS_CONFIG_SILENT_SUPPORT
}


#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
static int CALLBACK WINAPI BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
  if (uMsg==BFFM_INITIALIZED)
  {
    my_GetDialogItemText(IDC_DIR,(char*)lpData);
    SendMessage(hwnd,BFFM_SETSELECTION,(WPARAM)1,lpData);
  }
  if (uMsg==BFFM_SELCHANGED)
  {
    SendMessage(
      hwnd,
      BFFM_ENABLEOK,
      0,
      SHGetPathFromIDList((LPITEMIDLIST)lParam,(char*)lpData)
#ifdef NSIS_SUPPORT_CODECALLBACKS
      && !ExecuteCodeSegment(g_header->code_onVerifyInstDir,NULL)
#endif
    );
  }
  return 0;
}

BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG || uMsg == WM_NOTIFY_OUTER_NEXT)
  {
    page *this_page;
    static DLGPROC winprocs[]=
    {
#ifdef NSIS_CONFIG_LICENSEPAGE
      LicenseProc,
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
      SelProc,
#endif
      DirProc,
      InstProc,
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      UninstProc
#endif
    };

    if (uMsg == WM_INITDIALOG)
    {
      g_hwnd=hwndDlg;
      m_hwndOK=GetDlgItem(hwndDlg,IDOK);
      m_hwndCancel=GetDlgItem(hwndDlg,IDCANCEL);
      SetDlgItemTextFromLang(hwndDlg,IDC_VERSTR,LANG_BRANDING);
      SetClassLong(hwndDlg,GCL_HICON,(long)g_hIcon);
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
      g_quit_flag = ExecuteCodeSegment(g_header->code_onGUIInit,NULL);
#endif
        //ShowWindow(hwndDlg, SW_SHOW);
    }

    this_page=g_pages+m_page;

    if (m_page>=0) {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      // Call leave function. If Abort used don't move to the next page.
      // But if quit called we must exit now
      if (m_delta==1) if (ExecuteCodeSegment(this_page->leavefunc,NULL)) return !g_quit_flag;
#endif

      // if the last page was a custom page, wait for it to finish by itself.
      // if it doesn't, it's a BAD plugin.
      // plugins should react to WM_NOTIFY_OUTER_NEXT.
      if (!this_page->dlg_id) return 0;
    }

    NotifyCurWnd(WM_NOTIFY_INIGO_MONTOYA);

nextPage:
    m_page+=m_delta;
    this_page+=m_delta;

#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (m_page==g_blocks[NB_PAGES].num) ExecuteCodeSegment(g_header->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    if (g_quit_flag || (unsigned int)m_page >= (unsigned int)g_blocks[NB_PAGES].num)
    {
      DestroyWindow(m_curwnd);
      g_hwnd = 0;
      EndDialog(hwndDlg,m_retcode);
    }
    else
    {
      HWND hwndtmp;

      int pflags = this_page->flags;

      GetNSISString(state_click_next, this_page->clicknext);
      SetDlgItemTextFromLang(hwndDlg, IDOK, this_page->next);
      SetDlgItemTextFromLang(hwndDlg, IDC_BACK, this_page->back);
      SetDlgItemTextFromLang(hwndDlg, IDCANCEL, this_page->cancel);

      hwndtmp = GetDlgItem(hwndDlg, IDC_BACK);

      if (g_exec_flags.abort)
      {
        pflags &= ~(PF_BACK_ENABLE | PF_NEXT_ENABLE);
        pflags |= PF_CANCEL_ENABLE;
      }

      ShowWindow(hwndtmp, pflags & PF_BACK_SHOW);// SW_HIDE = 0, PF_BACK_SHOW = SW_SHOWNA = 8
      EnableWindow(hwndtmp, pflags & PF_BACK_ENABLE);
      EnableNext(pflags & PF_NEXT_ENABLE);
      EnableWindow(m_hwndCancel, pflags & PF_CANCEL_ENABLE);

      SendMessage(hwndtmp, BM_SETSTYLE, BS_PUSHBUTTON, TRUE);

      if (g_exec_flags.abort)
      {
        SendMessage(hwndDlg, DM_SETDEFID, IDCANCEL, 0);
        SetActiveCtl(hwndDlg, m_hwndCancel);
      }
      else
      {
        SetActiveCtl(hwndDlg, m_hwndOK);
      }

      mystrcpy(g_tmp,g_caption);
      GetNSISString(g_tmp+mystrlen(g_tmp),this_page->caption);
      my_SetWindowText(hwndDlg,g_tmp);

#ifdef NSIS_SUPPORT_CODECALLBACKS
      // custom page or user used abort in prefunc
      if (ExecuteCodeSegment(this_page->prefunc, NULL) || !this_page->dlg_id) {
        goto nextPage;
      }
#endif //NSIS_SUPPORT_CODECALLBACKS

      if (this_page->wndproc_id != PWP_COMPLETED)
      {
        DestroyWindow(m_curwnd);
      }
      else {
        if (!g_exec_flags.abort && g_exec_flags.autoclose)
          goto nextPage;
        // no need to go to skipPage because PWP_COMPLETED always follows PWP_INSTFILES
        return FALSE;
      }

      // update g_this_page for the dialog proc
      g_this_page=this_page;

      if (this_page->dlg_id > 0) // NSIS page
      {
        m_curwnd=CreateDialogParam(
          g_hInstance,
          MAKEINTRESOURCE(this_page->dlg_id+dlg_offset),
          hwndDlg,winprocs[this_page->wndproc_id],(LPARAM)this_page
        );
        if (m_curwnd)
        {
          RECT r;

          SetDlgItemTextFromLang(m_curwnd,IDC_INTROTEXT,this_page->parms[0]);

          GetWindowRect(GetDlgItem(hwndDlg,IDC_CHILDRECT),&r);
          ScreenToClient(hwndDlg,(LPPOINT)&r);
          SetWindowPos(m_curwnd,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
#ifdef NSIS_SUPPORT_CODECALLBACKS
          ExecuteCodeSegment(this_page->showfunc,NULL);
#endif //NSIS_SUPPORT_CODECALLBACKS
          ShowWindow(m_curwnd,SW_SHOWNA);
          NotifyCurWnd(WM_NOTIFY_START);
        }
      }
    }

skipPage:

    if (!ui_dlg_visible && m_curwnd)
    {
      ShowWindow(hwndDlg, SW_SHOWDEFAULT);
      ui_dlg_visible = 1;
    }

    return FALSE;
  }

#ifdef NSIS_SUPPORT_BGBG
  if (uMsg == WM_WINDOWPOSCHANGED)
  {
    SetWindowPos(m_bgwnd, hwndDlg, 0, 0, 0, 0, SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);
  }
  if (uMsg == WM_SIZE) {
    ShowWindow(m_bgwnd, wParam == SIZE_MINIMIZED ? SW_HIDE : SW_SHOW);
  }
#endif //NSIS_SUPPORT_BGBG

  if (uMsg == WM_NOTIFY_CUSTOM_READY) {
    DestroyWindow(m_curwnd);
    m_curwnd = (HWND)wParam;
    goto skipPage;
  }
  if (uMsg == WM_CLOSE && m_page == g_blocks[NB_PAGES].num - 1)
  {
    if (!IsWindowEnabled(m_hwndCancel))
    {
      uMsg = WM_COMMAND;
      wParam = IDOK;
    }
  }
  if (uMsg == WM_COMMAND)
  {
    int id = LOWORD(wParam);
    HWND hCtl = GetDlgItem(hwndDlg, id);
    if (hCtl)
    {
      SendMessage(hCtl, BM_SETSTATE, FALSE, 0);
      if (!IsWindowEnabled(hCtl))
        return 0;
    }

    if (id == IDOK)
    {
      outernotify(1);
    }
    if (id == IDC_BACK && m_page>0)
    {
      outernotify(-1);
    }
    if (id == IDCANCEL)
    {
      if (g_exec_flags.abort)
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        ExecuteCodeSegment(g_header->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
        m_retcode=2;
        outernotify(NOTIFY_BYE_BYE);
      }
      else
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (!ExecuteCodeSegment(g_header->code_onUserAbort,NULL))
#endif//NSIS_SUPPORT_CODECALLBACKS
        {
          m_retcode=1;
          outernotify(NOTIFY_BYE_BYE);
        }
      }
    }
    else
    {
      // Forward WM_COMMANDs to inner dialogs, can be custom ones.
      // Without this, enter on buttons in inner dialogs won't work.
      SendMessage(m_curwnd, WM_COMMAND, wParam, lParam);
    }
  }
  return HandleStaticBkColor();
}

#ifdef NSIS_CONFIG_LICENSEPAGE

#define _RICHEDIT_VER 0x0200
#include <RichEdit.h>
#undef _RICHEDIT_VER
static DWORD dwRead;
DWORD CALLBACK StreamLicense(DWORD dwCookie, LPBYTE pbBuff, LONG cb, LONG *pcb)
{
  lstrcpyn(pbBuff,(char*)dwCookie+dwRead,cb);
  *pcb=mystrlen(pbBuff);
  dwRead+=*pcb;
  return 0;
}

#define this_page ((page*)lParam)

static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  page *m_this_page=g_this_page;
  HWND hwLicense;
  static int ignoreWMCommand;

  if (uMsg == WM_INITDIALOG)
  {
    char *l = (char *)GetNSISStringNP(GetNSISTab(this_page->parms[1]));
    int lt = *l;
    EDITSTREAM es = {
      (DWORD)(++l),
      0,
      StreamLicense
    };

    int selected = (this_page->flags & PF_LICENSE_SELECTED) | !(this_page->flags & PF_LICENSE_FORCE_SELECTION);

    SetUITextFromLang(IDC_LICENSEAGREE,this_page->parms[2]);
    SetUITextFromLang(IDC_LICENSEDISAGREE,this_page->parms[3]);
    CheckDlgButton(hwndDlg,IDC_LICENSEAGREE+!selected,BST_CHECKED);
    EnableNext(selected);

    hwLicense=GetUIItem(IDC_EDIT1);
    SetActiveCtl(hwndDlg, hwLicense);
    SendMessage(hwLicense,EM_AUTOURLDETECT,TRUE,0);
#define lbg g_header->license_bg
    SendMessage(hwLicense,EM_SETBKGNDCOLOR,0,lbg>=0?lbg:GetSysColor(-lbg));
#undef lbg
    SendMessage(hwLicense,EM_SETEVENTMASK,0,ENM_LINK|ENM_KEYEVENTS); //XGE 8th September 2002 Or'd in ENM_KEYEVENTS
    dwRead=0;
    SendMessage(hwLicense,EM_EXLIMITTEXT,0,mystrlen(l));
    SendMessage(hwLicense,EM_STREAMIN,lt,(LPARAM)&es);
    ignoreWMCommand = 0;
    return FALSE;
  }
  if (uMsg == WM_COMMAND && HIWORD(wParam) == BN_CLICKED && !ignoreWMCommand) {
    if (m_this_page->flags & PF_LICENSE_FORCE_SELECTION) {
      int is = SendMessage(GetUIItem(IDC_LICENSEAGREE), BM_GETCHECK, 0, 0) & BST_CHECKED;
      m_this_page->flags &= ~PF_LICENSE_SELECTED;
      m_this_page->flags |= is;
      EnableNext(is);
      SetNextDef();
    }
  }
  if (uMsg == WM_NOTIFY) {
    hwLicense=GetUIItem(IDC_EDIT1);
    #define nmhdr ((NMHDR *)lParam)
    #define enlink ((ENLINK *)lParam)
    #define msgfilter ((MSGFILTER *)lParam)
    if (nmhdr->code==EN_LINK) {
      if (enlink->msg==WM_LBUTTONDOWN) {
        TEXTRANGE tr = {
          enlink->chrg.cpMin,
          enlink->chrg.cpMax,
          ps_tmpbuf
        };
        if (tr.chrg.cpMax-tr.chrg.cpMin < sizeof(ps_tmpbuf)) {
          SendMessage(hwLicense,EM_GETTEXTRANGE,0,(LPARAM)&tr);
          SetCursor(LoadCursor(0,IDC_WAIT));
          ShellExecute(hwndDlg,"open",tr.lpstrText,NULL,NULL,SW_SHOWNORMAL);
          SetCursor(LoadCursor(0,IDC_ARROW));
        }
      }
      if (enlink->msg==WM_SETCURSOR) {
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
    if (nmhdr->code==EN_MSGFILTER)
    {
      if (msgfilter->msg==WM_KEYDOWN)
      {
        if (msgfilter->wParam==VK_RETURN) {
          SendMessage(g_hwnd, WM_COMMAND, IDOK, 0);
        }
        if (msgfilter->wParam==VK_ESCAPE) {
          SendMessage(g_hwnd, WM_CLOSE, 0, 0);
        }
        return 1;
      }
    }
    #undef nmhdr
    #undef enlink
    #undef msgfilter
  }
  if (uMsg == WM_NOTIFY_INIGO_MONTOYA)
  {
    ignoreWMCommand++;
  }
  return HandleStaticBkColor();
}
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    SetUITextFromLang(IDC_UNINSTFROM,this_page->parms[1]);
    SetUITextNT(IDC_EDIT1,g_usrvars[this_page->parms[4]]);
  }
  return HandleStaticBkColor();
}
#endif


static char * NSISCALL inttosizestr(int kb, char *str)
{
  char scalestr[32], byte[32];
  char sh=20;
  char s=0;
  int scale=LANG_GIGA;
  if (kb < 1024) { sh=0; scale=LANG_KILO; }
  else if (kb < 1024*1024) { sh=10; scale=LANG_MEGA; }
  else if (GetVersion()&0x80000000) s='+';//only display the + on GB shown on win9x.
  wsprintf(
    str+mystrlen(str),
    "%d.%d%s%s%c",
    kb>>sh,
    ((kb*10)>>sh)%10,
    GetNSISString(scalestr,scale),
    GetNSISString(byte,LANG_BYTE),
    s
  );
  return str;
}

static int NSISCALL _sumsecsfield(int idx)
{
  int x,total;
  section *sections = g_sections;
  for (total = x = 0; x < num_sections; x++)
  {
#ifdef NSIS_CONFIG_COMPONENTPAGE
    if (sections[x].flags & SF_SELECTED)
#endif
      total += sections[x].fields[idx];
  }
  return total;
}

#define sumsecsfield(x) _sumsecsfield(SECTION_OFFSET(x))

static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static int dontsetdefstyle;
  page *thispage = g_this_page;
  char *dir = g_usrvars[thispage->parms[4]];
  int browse_text = thispage->parms[3];
  if (uMsg == WM_NOTIFY_INIGO_MONTOYA)
  {
    GetUIText(IDC_DIR,dir);
    validate_filename(dir);
#ifdef NSIS_CONFIG_LOG
#ifndef NSIS_CONFIG_LOG_ODS
    build_g_logfile();
#endif
    log_dolog = IsDlgButtonChecked(hwndDlg,IDC_CHECK1);
#endif
  }
  if (uMsg == WM_INITDIALOG)
  {
#ifdef NSIS_CONFIG_LOG
    if (GetAsyncKeyState(VK_SHIFT)&0x8000)
    {
      HWND h=GetUIItem(IDC_CHECK1);
      SetUITextFromLang(IDC_CHECK1,LANG_LOG_INSTALL_PROCESS);
      ShowWindow(h,SW_SHOWNA);
    }
#endif
    if (validpathspec(dir) && !skip_root(dir))
      addtrailingslash(dir);
    SetUITextNT(IDC_DIR,dir);
    SetUITextFromLang(IDC_BROWSE,this_page->parms[2]);
    SetUITextFromLang(IDC_SELDIRTEXT,this_page->parms[1]);
    SetActiveCtl(hwndDlg, GetUIItem(IDC_DIR));
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam);
    if (id == IDC_DIR && HIWORD(wParam) == EN_CHANGE)
    {
      uMsg = WM_IN_UPDATEMSG;
    }
    if (id == IDC_BROWSE)
    {
      BROWSEINFO bi = {0,};
      ITEMIDLIST *idlist;
      bi.hwndOwner = hwndDlg;
      bi.pszDisplayName = g_tmp;
      bi.lpfn = BrowseCallbackProc;
      bi.lParam = (LPARAM)dir;
      bi.lpszTitle = GetNSISStringTT(browse_text);
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x0040
#endif
      bi.ulFlags = BIF_RETURNONLYFSDIRS | BIF_NEWDIALOGSTYLE;
      idlist = SHBrowseForFolder(&bi);
      if (idlist)
      {
        // Get and free idlist
        my_PIDL2Path(g_tmp, idlist);

        if (g_header->install_directory_auto_append)
        {
          const char *post_str=ps_tmpbuf;
          GetNSISStringTT(g_header->install_directory_auto_append);
          // name gives just the folder name
          if (lstrcmpi(post_str,g_tmp))
          {
            lstrcat(addtrailingslash(dir),post_str);
          }
        }

        dontsetdefstyle++;
        SetUITextNT(IDC_DIR,dir);
      }
    }
  }
  if (uMsg == WM_IN_UPDATEMSG || uMsg == WM_NOTIFY_START)
  {
    static char s[NSIS_MAX_STRLEN];
    char *p;
    int error = 0;
    int total, available=-1;
    DWORD spc,bps,fc,tc;

    GetUIText(IDC_DIR,dir);
    if (!is_valid_instpath(dir))
      error = NSIS_INSTDIR_INVALID;

    mystrcpy(s,dir);
    p=skip_root(s);
    if (p)
      *p=0;

    if (GetDiskFreeSpace(s,&spc,&bps,&fc,&tc))
    {
      DWORD r=MulDiv(bps*spc,fc,1<<10);
      if (r > 0x7fffffff) r=0x7fffffff;
      available=(int)r;
    }

    total = sumsecsfield(size_kb);

    if ((unsigned int)available < (unsigned int)total)
      error = NSIS_INSTDIR_NOT_ENOUGH_SPACE;

    if (LANG_STR_TAB(LANG_SPACE_REQ)) {
      SetUITextNT(IDC_SPACEREQUIRED,inttosizestr(total,GetNSISString(s,LANG_SPACE_REQ)));
      if (available != -1)
        SetUITextNT(IDC_SPACEAVAILABLE,inttosizestr(available,GetNSISString(s,LANG_SPACE_AVAIL)));
      else
        SetUITextNT(IDC_SPACEAVAILABLE,"");
    }

    g_exec_flags.instdir_error = error;

#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (!error)
      error = ExecuteCodeSegment(g_header->code_onVerifyInstDir,NULL);
#endif

    if (thispage->flags & PF_DIR_NO_BTN_DISABLE)
      error = 0;

    EnableNext(!error);
    if (!error && !dontsetdefstyle)
      SetNextDef();
    dontsetdefstyle = 0;
  }
  return HandleStaticBkColor();
}

#ifdef NSIS_CONFIG_COMPONENTPAGE

#ifndef TVITEM
#define TVITEM TV_ITEM
#endif

static int NSISCALL SetChildrenStates(HWND hwTree, HTREEITEM hItem, int iChecked)
{
  int iCheckedChildren = 0, iChildren = 0, *pFlags, iState = 1;
  HTREEITEM hItrItem;
  TVITEM tvItem;

  hItrItem = TreeView_GetChild(hwTree, hItem);
  while (hItrItem)
  {
    iCheckedChildren += SetChildrenStates(hwTree, hItrItem, iChecked);
    iChildren++;
    hItrItem = TreeView_GetNextSibling(hwTree, hItrItem);
  }

  tvItem.hItem = hItem;
  tvItem.mask = TVIF_PARAM | TVIF_STATE;
  tvItem.stateMask = TVIS_STATEIMAGEMASK;
  TreeView_GetItem(hwTree, &tvItem);

  pFlags = &(g_sections[tvItem.lParam].flags);

  if (*pFlags & SF_RO)
  {
    iState = 4;
    iChecked = *pFlags & SF_SELECTED;
  }

  *pFlags &= ~(SF_SELECTED|SF_PSELECTED);

  if (iCheckedChildren || iChecked)
  {
    iState++;
    if (iChildren == iCheckedChildren)
    {
      *pFlags |= SF_SELECTED;
    }
    else
    {
      iState++;
      *pFlags |= SF_PSELECTED;
    }
    iChecked = 1;
  }

  tvItem.state = INDEXTOSTATEIMAGEMASK(iState);

  TreeView_SetItem(hwTree, &tvItem);

  return iChecked;
}

static void NSISCALL SetParentState(HWND hwTree, HTREEITEM hItem)
{
  TVITEM tvItem;
  int iState = 0, iItrState, *iFlags;

  HTREEITEM hParent = TreeView_GetParent(hwTree, hItem);
  if (!hParent)
    return;

  hItem = TreeView_GetChild(hwTree, hParent);

  tvItem.mask = TVIF_STATE | TVIF_PARAM;
  tvItem.stateMask = TVIS_STATEIMAGEMASK;

  while (hItem) {
    tvItem.hItem = hItem;
    TreeView_GetItem(hwTree, &tvItem);
    iItrState = tvItem.state >> 12;
    iState |= iItrState % 3 ? iItrState % 3 : 3;
    hItem = TreeView_GetNextSibling(hwTree, hItem);
  }

  tvItem.hItem = hParent;
  TreeView_GetItem(hwTree, &tvItem);

  iFlags = &(g_sections[tvItem.lParam].flags);
  *iFlags &= ~(SF_SELECTED|SF_PSELECTED);

  if (iState == 2)
    *iFlags |= SF_SELECTED;
  if (iState == 3)
    *iFlags |= SF_PSELECTED;

  tvItem.state = INDEXTOSTATEIMAGEMASK(iState);
  TreeView_SetItem(hwTree, &tvItem);

  SetParentState(hwTree, hParent);
}

#define CheckTreeItem(h, i, c) { SetChildrenStates(h, i, c); SetParentState(h, i); }

HTREEITEM NSISCALL TreeHitTest(HWND tree)
{
  TVHITTESTINFO ht;
  DWORD dwpos = GetMessagePos();

  ht.pt.x = GET_X_LPARAM(dwpos);
  ht.pt.y = GET_Y_LPARAM(dwpos);
  ScreenToClient(tree, &ht.pt);

  TreeView_HitTest(tree, &ht);

  if (ht.flags & (TVHT_ONITEMSTATEICON|TVHT_ONITEMLABEL|TVHT_ONITEMRIGHT|TVHT_ONITEM))
    return ht.hItem;

  return 0;
}

static LONG oldTreeWndProc;
static DWORD WINAPI newTreeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static LPARAM last_item=-1;
  if (uMsg == WM_CHAR && wParam == VK_SPACE)
  {
    NotifyCurWnd(WM_TREEVIEW_KEYHACK);
  }
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
  if (uMsg == WM_DESTROY) {
    last_item=-1;
  }
  if (uMsg == WM_MOUSEMOVE) {
    TVITEM tvItem;

    if (IsWindowVisible(hwnd)) {
      tvItem.hItem = TreeHitTest(hwnd);

      lParam = -1;

      if (tvItem.hItem)
      {
        tvItem.mask = TVIF_PARAM;

        TreeView_GetItem(hwnd, &tvItem);

        lParam = tvItem.lParam;
      }
      uMsg = WM_NOTIFY_SELCHANGE;
    }
  }
  if (uMsg == WM_NOTIFY_SELCHANGE) {
    if (last_item != lParam)
    {
      last_item = lParam;

      mystrcpy(g_tmp, g_usrvars[0]);

      myitoa(g_usrvars[0], lParam);

      ExecuteCodeSegment(g_header->code_onMouseOverSection,NULL);

      mystrcpy(g_usrvars[0], g_tmp);
    }
  }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_ENHANCEDUI_SUPPORT
  return CallWindowProc((WNDPROC)oldTreeWndProc,hwnd,uMsg,wParam,lParam);
}

static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HTREEITEM *hTreeItems;
  static HIMAGELIST hImageList;
  HWND hwndCombo1 = GetUIItem(IDC_COMBO1);
  HWND hwndTree1 = GetUIItem(IDC_TREE1);
  extern HWND g_SectionHack;
  section *sections=g_sections;
  int *install_types=g_header->install_types;
  if (uMsg == WM_INITDIALOG)
  {
    int doLines=0;
    HTREEITEM Par;
    HBITMAP hBMcheck1;
    int x, lastGoodX, i, noCombo=2;

    g_SectionHack=hwndDlg;

    hTreeItems=(HTREEITEM*)my_GlobalAlloc(sizeof(HTREEITEM)*num_sections);

    hBMcheck1=LoadBitmap(g_hInstance, MAKEINTRESOURCE(IDB_BITMAP1));

    oldTreeWndProc=SetWindowLong(hwndTree1,GWL_WNDPROC,(long)newTreeWndProc);

    hImageList = ImageList_Create(16,16, ILC_COLOR32|ILC_MASK, 6, 0);
    ImageList_AddMasked(hImageList,hBMcheck1,RGB(255,0,255));

    TreeView_SetImageList(hwndTree1, hImageList, TVSIL_STATE);

#ifndef TVM_SETITEMHEIGHT
#define TVM_SETITEMHEIGHT (TV_FIRST + 27)
#endif
#ifndef TVM_GETITEMHEIGHT
#define TVM_GETITEMHEIGHT (TV_FIRST + 28)
#endif

    if (SendMessage(hwndTree1, TVM_GETITEMHEIGHT, 0, 0) < 16)
      SendMessage(hwndTree1, TVM_SETITEMHEIGHT, 16, 0);

    DeleteObject(hBMcheck1);

    for (i = 0; i < NSIS_MAX_INST_TYPES+1; i++)
    {
      if (install_types[i])
      {
        int j;
        if (i != NSIS_MAX_INST_TYPES) noCombo = 0;
        GetNSISString(g_tmp,install_types[i]);
        j=SendMessage(hwndCombo1,CB_ADDSTRING,0,(LPARAM)g_tmp);
        SendMessage(hwndCombo1,CB_SETITEMDATA,j,i);
        if (i == g_exec_flags.cur_insttype)
          SendMessage(hwndCombo1, CB_SETCURSEL, j, 0);
      }
    }

    SetUITextFromLang(IDC_TEXT1,this_page->parms[1+noCombo]);
    SetUITextFromLang(IDC_TEXT2,this_page->parms[2+noCombo]);

    Par=NULL;

    for (lastGoodX = x = 0; x < num_sections; x ++)
    {
      section *sec=sections+x;

      if (sec->name_ptr)
      {
        TVINSERTSTRUCT tv;
        tv.hParent=Par;
        tv.hInsertAfter=TVI_LAST;
        tv.item.mask=TVIF_PARAM|TVIF_TEXT|TVIF_STATE;
        tv.item.lParam=x;
        tv.item.pszText=GetNSISStringTT(sec->name_ptr);
        tv.item.stateMask=TVIS_STATEIMAGEMASK|TVIS_EXPANDED|TVIS_BOLD;

        {
          int l=1;
          // Sf_SELECTED == 1
          l += sec->flags & SF_SELECTED;
          //if (sec->flags & SF_SELECTED) l++;
          if (sec->flags & SF_RO) l+=3;

          tv.item.state=INDEXTOSTATEIMAGEMASK(l);
        }

        //if (sec->flags&SF_BOLD)
        {
          // SF_BOLD << 1 == 16 == TVIS_BOLD
          tv.item.state|=(sec->flags&SF_BOLD)<<1;
        }

        if (sec->flags&SF_SUBSEC)
        {
          tv.item.mask|=TVIF_CHILDREN;
          tv.item.cChildren=1;
          //if (sec->flags&SF_EXPAND)
            // TVIS_EXPANDED == SF_EXPAND
            tv.item.state|=sec->flags&SF_EXPAND;
          Par = hTreeItems[x] = TreeView_InsertItem(hwndTree1,&tv);
          doLines=1;
        }
        else if (sec->flags&SF_SUBSECEND)
        {
          SetParentState(hwndTree1,hTreeItems[lastGoodX]);
          Par=TreeView_GetParent(hwndTree1,Par);
        }
        else
        {
          lastGoodX = x;
          hTreeItems[x] = TreeView_InsertItem(hwndTree1,&tv);
        }
      }
    }
    if (!doLines)
    {
      SetWindowLong(hwndTree1,GWL_STYLE,GetWindowLong(hwndTree1,GWL_STYLE)&~(TVS_LINESATROOT));
    }
    SendMessage(hwndTree1,WM_VSCROLL,SB_TOP,0);

    if (!noCombo)
    {
      ShowWindow(hwndCombo1, SW_SHOW);
      SetActiveCtl(hwndDlg, hwndCombo1);
    }
    else
      SetActiveCtl(hwndDlg, hwndTree1);

    uMsg = g_exec_flags.insttype_changed ? WM_NOTIFY_INSTTYPE_CHANGE : WM_IN_UPDATEMSG;
  }
  if (uMsg == WM_NOTIFY_SECTEXT) // update text
  {
    int x=wParam;
    int ns=lParam;
    TVITEM tv;

    if (tv.hItem=hTreeItems[x])
    {
      tv.mask=TVIF_TEXT;
      tv.pszText=GetNSISStringTT(ns);
      TreeView_SetItem(hwndTree1,&tv);
    }
  }
  if (uMsg == WM_NOTIFY_SECFLAGS) // change flags
  {
    int flags = sections[wParam].flags;
    TVITEM tvItem;

    if (!(tvItem.hItem = hTreeItems[wParam])) return 0;
    tvItem.mask = TVIF_STATE;
    tvItem.stateMask = TVIS_BOLD;
    tvItem.state = 0;
    //if (flags&SF_BOLD) tvItem.state |= TVIS_BOLD;
    // SF_BOLD << 1 == 16 == TVIS_BOLD
    tvItem.state|=(flags&SF_BOLD)<<1;
    TreeView_SetItem(hwndTree1, &tvItem);

    TreeView_Expand(hwndTree1, tvItem.hItem, flags & SF_EXPAND ? TVE_EXPAND : TVE_COLLAPSE);

    if ((flags & (SF_PSELECTED | SF_SELECTED)) != SF_PSELECTED)
    {
      CheckTreeItem(hwndTree1, tvItem.hItem, flags & SF_SELECTED);
    }
  }
  if (uMsg == WM_NOTIFY || uMsg == WM_TREEVIEW_KEYHACK)
  {
    LPNMHDR lpnmh = (LPNMHDR) lParam;
    if (uMsg == WM_TREEVIEW_KEYHACK || lpnmh->idFrom == IDC_TREE1)
    {
      if (!(g_flags&CH_FLAGS_NO_CUSTOM) && (uMsg == WM_TREEVIEW_KEYHACK || lpnmh->code == NM_CLICK))
      {
        TVITEM tvItem;

        if (uMsg != WM_TREEVIEW_KEYHACK)
          tvItem.hItem=TreeHitTest(hwndTree1);
        else
          tvItem.hItem=TreeView_GetSelection(hwndTree1);

        if (tvItem.hItem)
        {
          int iState;

          tvItem.mask = TVIF_STATE|TVIF_PARAM;
          TreeView_GetItem(hwndTree1, &tvItem);

          iState = tvItem.state >> 12;

          if (iState < 4) // not RO
          {
            if (iState == 2) // already checked
            {
              sections[tvItem.lParam].flags&=~SF_SELECTED;
              CheckTreeItem(hwndTree1,tvItem.hItem,0);
            }
            else
            {
              sections[tvItem.lParam].flags|=SF_SELECTED;
              CheckTreeItem(hwndTree1,tvItem.hItem,1);
            }
            lParam = 0;
            wParam = 1;
            uMsg = WM_IN_UPDATEMSG;
          } // not ro
        } // was valid click
      } // was click or hack
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
      if (lpnmh)
      {
        if (lpnmh->code == TVN_SELCHANGED)
        {
          SendMessage(hwndTree1, WM_NOTIFY_SELCHANGE, 0, ((LPNMTREEVIEW)lpnmh)->itemNew.lParam);
        }
        if (lpnmh->code == TVN_ITEMEXPANDED)
        {
          LPNMTREEVIEW pnmtv = (LPNMTREEVIEW) lpnmh;
          if (pnmtv->action == TVE_EXPAND)
            sections[pnmtv->itemNew.lParam].flags |= SF_EXPAND;
          else
            sections[pnmtv->itemNew.lParam].flags &= ~SF_EXPAND;
        }
      }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_ENHANCEDUI_SUPPORT
    }
  }
  if (uMsg == WM_MOUSEMOVE)
  {
    SendMessage(hwndTree1, WM_MOUSEMOVE, 0, 0);
  }
  if (uMsg == WM_NOTIFY_INSTTYPE_CHANGE ||
     (uMsg == WM_COMMAND && LOWORD(wParam)==IDC_COMBO1 && HIWORD(wParam)==CBN_SELCHANGE))
  {
    int t=SendMessage(hwndCombo1,CB_GETCURSEL,0,0);
    if (uMsg == WM_NOTIFY_INSTTYPE_CHANGE || t != CB_ERR)
    {
      int whichcfg=SendMessage(hwndCombo1,CB_GETITEMDATA,t,0);
      if (uMsg == WM_NOTIFY_INSTTYPE_CHANGE)
      {
        whichcfg = g_exec_flags.cur_insttype;
        g_exec_flags.insttype_changed = 0;
      }
      else lParam = 1;

      if (whichcfg == CB_ERR || !(install_types[whichcfg]))
        whichcfg = NSIS_MAX_INST_TYPES;

      if (whichcfg != NSIS_MAX_INST_TYPES) // not custom
      {
        int x=num_sections;
        section *t=sections;
        HTREEITEM *ht=hTreeItems;
        while (x--)
        {
          TVITEM tv;
          int l=1;

          if (t->install_types & (1<<whichcfg))
          {
            l++;
            t->flags|=SF_SELECTED;
          }
          else t->flags&=~SF_SELECTED;

          if (t->flags&SF_RO) l+=3;

          if (tv.hItem=*ht) {
            tv.mask=TVIF_STATE;
            tv.state=INDEXTOSTATEIMAGEMASK(l);
            tv.stateMask=TVIS_STATEIMAGEMASK;

            TreeView_SetItem(hwndTree1,&tv);
            SetParentState(hwndTree1,tv.hItem);
          }
          t++;
          ht++;
        }
        SendMessage(hwndTree1,WM_VSCROLL,SB_TOP,0);
      }

      g_exec_flags.cur_insttype=whichcfg;

      uMsg = WM_IN_UPDATEMSG;
    }
  }
  if (uMsg == WM_NOTIFY_INIGO_MONTOYA)
  {
    if (hImageList) ImageList_Destroy(hImageList);
    if (hTreeItems) GlobalFree(hTreeItems);
    hImageList=NULL;
    hTreeItems=NULL;
    g_SectionHack=NULL;
  }
  if (uMsg == WM_IN_UPDATEMSG)
  {
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_COMPONENTPAGE)
  if ( wParam )
    ExecuteCodeSegment(g_header->code_onSelChange,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_COMPONENTPAGE

    if (g_flags & CH_FLAGS_COMP_ONLY_ON_CUSTOM)
    {
      int c = (g_exec_flags.cur_insttype == NSIS_MAX_INST_TYPES) << 3;// SW_SHOWNA=8, SW_HIDE=0
      ShowWindow(hwndTree1, c);
      ShowWindow(GetUIItem(IDC_TEXT2), c);
    }
    else if (!lParam)
    {
      int r,x,cbi;
      // check to see which install type we are
      for (r = 0, cbi = 0; r < NSIS_MAX_INST_TYPES; r ++)
      {
        HTREEITEM *ht=hTreeItems;
        section *t=sections;
        x=num_sections;

        if (!install_types[r]) continue;

        while (x--)
        {
          if (*ht && !(t->flags&(SF_SUBSEC|SF_SUBSECEND)))
          {
            TVITEM tvItem;
            tvItem.hItem=*ht;
            tvItem.mask=TVIF_STATE;
            TreeView_GetItem(hwndTree1,&tvItem);
            if (!(t->install_types&(1<<r)) != !((tvItem.state>>12 != 1 && (tvItem.state>>12 != 4)))) break;
          }
          t++;
          ht++;
        }
        if (x < 0) break;

        cbi++;
      }

      g_exec_flags.cur_insttype=r;
      SendMessage(hwndCombo1,CB_SETCURSEL,cbi,0);
    } // end of typecheckshit

    if (LANG_STR_TAB(LANG_SPACE_REQ)) {
      SetUITextNT(IDC_SPACEREQUIRED,inttosizestr(sumsecsfield(size_kb),GetNSISString(g_tmp,LANG_SPACE_REQ)));

    }
  }

  return HandleStaticBkColor();
}
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//NSIS_CONFIG_VISIBLE_SUPPORT

int ui_st_updateflag=0x6;

void NSISCALL update_status_text(int strtab, const char *text) {
  static char tmp[NSIS_MAX_STRLEN*2];
  LVITEM new_item;
  HWND linsthwnd = insthwnd;
  if (linsthwnd)
  {
    int updateflag = ui_st_updateflag;
    int tmplen;

    if (!(updateflag & 1))
      GetNSISString(tmp, strtab);

    tmplen = mystrlen(tmp);

    if (text)
    {
      if (tmplen + mystrlen(text) >= sizeof(tmp)) return;
      lstrcat(tmp, text);
    }

    if ((updateflag & 4)) my_SetWindowText(insthwnd2, tmp);
    if ((updateflag & 2))
    {
      new_item.mask = LVIF_TEXT;
      new_item.pszText = tmp;
      new_item.iItem = ListView_GetItemCount(linsthwnd) - (updateflag & 1);
      new_item.iSubItem = 0;
      SendMessage(linsthwnd, (updateflag & 1) ? LVM_SETITEM : LVM_INSERTITEM, 0, (LPARAM) &new_item);
      ListView_EnsureVisible(linsthwnd, new_item.iItem, 0);
    }

    if (updateflag & 1)
      tmp[tmplen] = 0;
  }
}

static DWORD WINAPI install_thread(LPVOID p)
{
  int m_inst_sec=0;
  HWND progresswnd = (HWND)p;

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
  {
    extern HRESULT g_hres;
    g_hres|=OleInitialize(NULL);
  }
#endif

  while (m_inst_sec<num_sections)
  {
#ifdef NSIS_CONFIG_COMPONENTPAGE
    if (g_sections[m_inst_sec].flags&SF_SELECTED)
#endif
    {
      log_printf2("Section: \"%s\"",GetNSISStringTT(g_sections[m_inst_sec].name_ptr));
      if (ExecuteCodeSegment(g_sections[m_inst_sec].code,progresswnd))
      {
        g_exec_flags.abort++;
        break;
      }
    }
#ifdef NSIS_CONFIG_COMPONENTPAGE
    else
    {
      log_printf2("Skipping section: \"%s\"",GetNSISStringTT(g_sections[m_inst_sec].name_ptr));
    }
#endif
    m_inst_sec++;
  }
  NotifyCurWnd(WM_NOTIFY_INSTPROC_DONE);

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
  OleUninitialize();
#endif

  return g_exec_flags.abort;
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT

// listview unfolds partly hidden labels if it does not have infotip text
#define LVS_EX_LABELTIP 0x00004000

static BOOL CALLBACK InstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  HWND linsthwnd=insthwnd;
  if (uMsg == WM_INITDIALOG)
  {
    RECT r;
    LVCOLUMN lvc = {LVCF_WIDTH, 0, -1, 0, 0, -1};
    int lb_bg=g_header->lb_bg,lb_fg=g_header->lb_fg;

    insthwndbutton=GetUIItem(IDC_SHOWDETAILS);
    insthwnd2=GetUIItem(IDC_INTROTEXT);
    linsthwnd=insthwnd=GetUIItem(IDC_LIST1);

    progress_bar_len=sumsecsfield(code_size);
    progress_bar_pos=0;

    log_printf3("New install of \"%s\" to \"%s\"",GetNSISStringTT(LANG_NAME),state_install_directory);

    GetClientRect(linsthwnd, &r);
    lvc.cx = r.right - GetSystemMetrics(SM_CXHSCROLL);
    ListView_InsertColumn(linsthwnd, 0, &lvc);

    ListView_SetExtendedListViewStyleEx(linsthwnd, LVS_EX_LABELTIP, LVS_EX_LABELTIP);
    if (lb_bg >= 0) {
      ListView_SetBkColor(linsthwnd, lb_bg);
      ListView_SetTextBkColor(linsthwnd, lb_bg);
    }
    if (lb_fg >= 0) {
      ListView_SetTextColor(linsthwnd, lb_fg);
    }
    SetUITextFromLang(IDC_SHOWDETAILS,this_page->parms[1]);
    if (g_flags&(CH_FLAGS_DETAILS_SHOWDETAILS|CH_FLAGS_DETAILS_NEVERSHOW))
    {
      ShowWindow(insthwndbutton,SW_HIDE);
      if (!(g_flags&CH_FLAGS_DETAILS_NEVERSHOW)) ShowWindow(linsthwnd,SW_SHOWNA);
      else insthwndbutton=NULL;
    }

    {
      HWND progresswnd=GetUIItem(IDC_PROGRESS);
      SendMessage(progresswnd,PBM_SETRANGE,0,MAKELPARAM(0,30000));
      if (g_flags&CH_FLAGS_PROGRESS_COLORED)
      {
        SendMessage(progresswnd,PBM_SETBARCOLOR,0,lb_fg);
        SendMessage(progresswnd,PBM_SETBKCOLOR,0,lb_bg);
      }
    }

    return FALSE;
  }
  if (uMsg == WM_NOTIFY_START) {
    DWORD id;
    CloseHandle(CreateThread(NULL,0,install_thread,GetUIItem(IDC_PROGRESS),0,&id));
  }
  if (uMsg == WM_COMMAND && LOWORD(wParam) == IDC_SHOWDETAILS)
  {
    ShowWindow(insthwndbutton,SW_HIDE);
    ShowWindow(linsthwnd,SW_SHOWNA);
    SetNextDef();
  }
  if (uMsg == WM_NOTIFY_INSTPROC_DONE)
  {
    if (g_quit_flag)
    {
      m_retcode=1;
      outernotify(NOTIFY_BYE_BYE);
    }
    else
    {
      ShowWindow(g_hwnd,SW_SHOWNA);
      if (!g_exec_flags.abort)
        update_status_text(g_this_page->parms[2],0);
      outernotify(1);
    }
  }
  //>>>Ximon Eighteen aka Sunjammer 30th August 2002
  //+++Popup "Copy Details To Clipboard" menu when RMB clicked in DetailView
  //+++Currently this has no language support for the popup menu tex
  if (uMsg == WM_CONTEXTMENU && wParam == (WPARAM) linsthwnd)
  {
    int count = ListView_GetItemCount(linsthwnd);
    if (count > 0)
    {
      HMENU menu = CreatePopupMenu();
      POINT pt;
      AppendMenu(menu,MF_STRING,1,GetNSISStringTT(LANG_COPYDETAILS));
      if (lParam == ((UINT)-1))
      {
        RECT r;
        GetWindowRect(linsthwnd, &r);
        pt.x = r.left;
        pt.y = r.top;
      }
      else
      {
        pt.x = GET_X_LPARAM(lParam);
        pt.y = GET_Y_LPARAM(lParam);
      }
      if (1==TrackPopupMenu(
        menu,
        TPM_NONOTIFY|TPM_RETURNCMD,
        pt.x,
        pt.y,
        0,linsthwnd,0))
      {
        int i,total = 1; // 1 for the null char
        LVITEM item;
        HGLOBAL memory;
        LPTSTR ptr;//,endPtr;

        // 1st pass - determine clipboard memory required.
        item.iSubItem   = 0;
        item.pszText    = g_tmp;
        item.cchTextMax = sizeof(g_tmp) - 1;
        i = count;
        while (i--)
          // Add 2 for the CR/LF combination that must follow every line.
          total += 2+SendMessage(linsthwnd,LVM_GETITEMTEXT,i,(LPARAM)&item);

        // 2nd pass - store detail view strings on the clipboard
        // Clipboard MSDN docs say mem must be GMEM_MOVEABLE
        OpenClipboard(0);
        EmptyClipboard();
        memory = GlobalAlloc(GHND,total);
        ptr = GlobalLock(memory);
        //endPtr = ptr+total-2; // -2 to allow for CR/LF
        i = 0;
        do {
          item.pszText = ptr;
          item.cchTextMax = total;
          SendMessage(linsthwnd,LVM_GETITEMTEXT,i,(LPARAM)&item);
          //while (*ptr) ptr++;
          ptr += mystrlen(ptr);
          *(WORD*)ptr = CHAR2_TO_WORD('\r','\n');
          ptr+=2;
        } while (++i < count);
        // memory is auto zeroed when allocated with GHND - *ptr = 0;
        GlobalUnlock(memory);
        SetClipboardData(CF_TEXT,memory);
        CloseClipboard();
      }
    }
    return FALSE;
  }
  //<<<
  return HandleStaticBkColor();
}
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
