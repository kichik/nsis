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

// Added by Amir Szekely 3rd August 2002
char *language_tables;
int *cur_language_table;
int dlg_offset;

int g_quit_flag; // set when Quit has been called (meaning bail out ASAP)

#if NSIS_MAX_INST_TYPES >= 31 || NSIS_MAX_INST_TYPES < 1
#error invalid value for NSIS_MAX_INST_TYPES
#endif

int progress_bar_pos, progress_bar_len;
int g_is_uninstaller;

HWND g_progresswnd;

static char g_tmp[4096];

int num_sections;

// sent to the last child window to tell it that the install thread is done
#define WM_NOTIFY_INSTPROC_DONE (WM_USER+0x4)

// sent to every child window to tell it it can start executing NSIS code
#define WM_NOTIFY_START (WM_USER+0x5)

// sent to the outer window to tell it to go to the next inner window
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)

// sent to every child window to tell it it is closing soon
#define WM_NOTIFY_INIGO_MONTOYA (WM_USER+0xb)

// update message used by DirProc and SelProc for space display
#define WM_IN_UPDATEMSG (WM_USER+0xf)

#define WM_NOTIFY_CUSTOM_READY (WM_USER+0xd)

#define WM_TREEVIEW_KEYHACK (WM_USER+0x13)

static int m_page=-1,m_retcode,m_delta=1;

#define NOTIFY_BYE_BYE 'x'

static void NSISCALL outernotify(char num) {
  if (num==NOTIFY_BYE_BYE)
    g_quit_flag++;
  m_delta=num;
  SendMessage(g_hwnd,WM_NOTIFY_OUTER_NEXT,(WPARAM)num,0); // it sends num again for plugins - DON'T REMOVE!
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

HWND insthwnd, insthwnd2,insthwndbutton;

void *g_inst_combinedheader;
page *g_inst_page;
section *g_inst_section;
entry *g_inst_entry;

static HWND m_curwnd, m_bgwnd, m_hwndOK, m_hwndCancel;
static int m_whichcfg;

static BOOL NSISCALL SetDlgItemTextFromLang_(HWND dlg, int id, int lid) {
  return my_SetDialogItemText(dlg,id+1000,LANG_STR(lid));
}

#define SetDlgItemTextFromLang(dlg,id,lid) SetDlgItemTextFromLang_(dlg,(id)-1000,lid)

#define SetUITextFromLang(it,la) SetDlgItemTextFromLang_(hwndDlg,(it)-1000,la)
#define SetUITextNT(it,text) my_SetDialogItemText(hwndDlg,it,text)
#define GetUIText(it,s,ss) my_GetDialogItemText(hwndDlg,it,s,ss)
#define GetUIItem(it) GetDlgItem(hwndDlg,it)

#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
#define HandleStaticBkColor() _HandleStaticBkColor(uMsg, wParam, lParam)
static BOOL NSISCALL _HandleStaticBkColor(UINT uMsg, WPARAM wParam, LPARAM lParam) {
  switch (uMsg) {
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLOREDIT:
    case WM_CTLCOLORDLG:
    case WM_CTLCOLORBTN:
    {
      BOOL brush = (BOOL)GetWindowLong((HWND)lParam, GWL_USERDATA);
      if (!brush) return 0;
      if (brush == -1) {
        COLORREF dlgColor = GetSysColor(COLOR_BTNFACE);
        SetBkColor((HDC)wParam, dlgColor);
        SetTextColor((HDC)wParam, dlgColor);
        return (BOOL)GetStockObject(NULL_BRUSH);
      }
      SetBkMode((HDC)wParam, TRANSPARENT);
      return brush;
    }
  }
  return 0;
}
#else
#define HandleStaticBkColor() 0
#endif//!NSIS_CONFIG_ENHANCEDUI_SUPPORT

#ifdef NSIS_CONFIG_LOG
void NSISCALL build_g_logfile()
{
  lstrcat(addtrailingslash(mystrcpy(g_log_file,state_install_directory)),"install.log");
}
#endif

static void NSISCALL set_language()
{
  int i;
  LANGID lang_mask=~(LANGID)0;
  LANGID lang=myatoi(state_language);
  char *language_table=0;
  int lang_num=g_inst_cmnheader->language_tables_num;

lang_again:
  for (i = 0; i < lang_num; i++) {
    language_table=language_tables+i*g_inst_cmnheader->language_table_size;
    if (!((lang ^ *(LANGID*)language_table) & lang_mask)) {
      dlg_offset=*(int*)(language_table+sizeof(LANGID));
      cur_language_table=(int*)(language_table+sizeof(LANGID)+sizeof(int));
      break;
    }
  }
  if (i == lang_num) {
    if (lang_mask == ~(LANGID)0)
      lang_mask=0x3ff; // primary lang
    else // we already tried once and we still don't have a language table
      lang_mask=0; // first lang
    goto lang_again;
  }

  myitoa(state_language, *(LANGID*)language_table);
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  my_SetWindowText(m_bgwnd,process_string_fromtab(g_caption,LANG_CAPTION));
#endif
}

int NSISCALL ui_doinstall(void)
{
  common_header *inst_cmnheader=g_inst_cmnheader;
  static WNDCLASS wc; // richedit subclassing and bgbg creation
  g_flags.autoclose=inst_flags&CH_FLAGS_AUTO_CLOSE;
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (!g_is_uninstaller)
#endif
  {
    if (!is_valid_instpath(state_install_directory))
    {
      if (g_inst_header->install_reg_key_ptr)
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
    if (inst_flags&CH_FLAGS_SILENT_LOG)
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
    language_tables=(void*)(g_db_strtab+inst_cmnheader->num_string_bytes);

    myitoa(state_language, GetUserDefaultLangID());
    set_language();
  }

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
  if (!(inst_flags&(CH_FLAGS_SILENT|CH_FLAGS_SILENT_LOG)))
#endif//NSIS_CONFIG_SILENT_SUPPORT
  {
    g_hIcon=LoadImage(g_hInstance,MAKEINTRESOURCE(IDI_ICON2),IMAGE_ICON,0,0,LR_DEFAULTSIZE|LR_SHARED);
    m_bgwnd=0;
#ifdef NSIS_SUPPORT_BGBG
    if (inst_cmnheader->bg_color1 != -1)
    {
      RECT vp;
      extern int bg_color1, bg_color2, bg_textcolor;
      extern LRESULT CALLBACK BG_WndProc(HWND, UINT, WPARAM, LPARAM);
      wc.lpfnWndProc = BG_WndProc;
      wc.hInstance = g_hInstance;
      wc.hIcon = g_hIcon;
      //wc.hCursor = LoadCursor(NULL,IDC_ARROW);
      wc.lpszClassName = "_Nb";

      if (!RegisterClass(&wc)) return 0;

      bg_color1=inst_cmnheader->bg_color1;
      bg_color2=inst_cmnheader->bg_color2;
      bg_textcolor=inst_cmnheader->bg_textcolor;

      SystemParametersInfo(SPI_GETWORKAREA, 0, &vp, 0);

      m_bgwnd = CreateWindowEx(WS_EX_TOOLWINDOW,"_Nb",0,WS_POPUP,
        vp.left,vp.top,vp.right-vp.left,vp.bottom-vp.top,0,NULL,g_hInstance,NULL);
    }
#endif//NSIS_SUPPORT_BGBG
#ifdef NSIS_SUPPORT_CODECALLBACKS
    g_hwnd=m_bgwnd;
    // Select language
    if (ExecuteCodeSegment(inst_cmnheader->code_onInit,NULL)) return 1;
    set_language();
    g_hwnd=NULL;
    ShowWindow(m_bgwnd, SW_SHOW);
#endif//NSIS_SUPPORT_CODECALLBACKS

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

    return DialogBox(g_hInstance,MAKEINTRESOURCE(IDD_INST+dlg_offset),0,DialogProc);
  }
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
#ifdef NSIS_CONFIG_SILENT_SUPPORT
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  else
#endif
  {
#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (ExecuteCodeSegment(inst_cmnheader->code_onInit,NULL)) return 1;
    set_language();
#endif//NSIS_SUPPORT_CODECALLBACKS
    if (install_thread(NULL))
    {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (!g_quit_flag) ExecuteCodeSegment(inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
      return 1;
    }
#ifdef NSIS_SUPPORT_CODECALLBACKS
    ExecuteCodeSegment(inst_cmnheader->code_onInstSuccess,NULL);
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
    my_GetDialogItemText((HWND)lpData,IDC_DIR,g_tmp,sizeof(g_tmp));
    SendMessage(hwnd,BFFM_SETSELECTION,(WPARAM)1,(LPARAM)g_tmp);
  }
  return 0;
}


BOOL CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG || uMsg == WM_NOTIFY_OUTER_NEXT)
  {
    page *this_page;
    static struct
    {
      unsigned int id;
      DLGPROC proc;
    }
    windows[]=
    {
#ifdef NSIS_CONFIG_LICENSEPAGE
      {IDD_LICENSE,LicenseProc},
#endif
#ifdef NSIS_CONFIG_COMPONENTPAGE
      {IDD_SELCOM,SelProc},
#endif
      {IDD_DIR,DirProc},
      {IDD_INSTFILES,InstProc},
      {0,NULL}, // imaginary completed page
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
      {IDD_UNINST,UninstProc}
#endif
    };

    if (uMsg == WM_INITDIALOG)
    {
      g_hwnd=hwndDlg;
      m_hwndOK=GetDlgItem(hwndDlg,IDOK);
      m_hwndCancel=GetDlgItem(hwndDlg,IDCANCEL);
      SetDlgItemTextFromLang(hwndDlg,IDC_VERSTR,LANG_BRANDING);
      SetClassLong(hwndDlg,GCL_HICON,(long)g_hIcon);
      SetDlgItemTextFromLang(hwndDlg,IDCANCEL,LANG_BTN_CANCEL);
      SetDlgItemTextFromLang(hwndDlg,IDC_BACK,LANG_BTN_BACK);
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
      if (!(g_quit_flag = ExecuteCodeSegment(g_inst_cmnheader->code_onGUIInit,NULL)))
#endif
        ShowWindow(hwndDlg,SW_SHOW);
    }

    this_page=g_inst_page+m_page;

    if (m_page>=0) {
#ifdef NSIS_SUPPORT_CODECALLBACKS
      // Call leave function. If Abort used don't move to the next page.
      if (m_delta==1) if (ExecuteCodeSegment(this_page->leavefunc,NULL)) return 1;
#endif

      // if the last page was a custom page, wait for it to finish by itself.
      // if it doesn't, it's a BAD plugin.
      // plugins should react to WM_NOTIFY_OUTER_NEXT.
      if (this_page->id<0) return 0;
    }

nextPage:

    m_page+=m_delta;
    this_page+=m_delta;

#ifdef NSIS_SUPPORT_CODECALLBACKS
    if (m_page==g_inst_cmnheader->num_pages) ExecuteCodeSegment(g_inst_cmnheader->code_onInstSuccess,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS

    if (g_quit_flag || m_page < 0 || m_page == g_inst_cmnheader->num_pages)
    {
      DestroyWindow(m_curwnd);
      EndDialog(hwndDlg,m_retcode);
    }
    else
    {
      HWND hwndtmp;

      SetDlgItemTextFromLang(hwndDlg,IDOK,this_page->next);
      
      hwndtmp=GetDlgItem(hwndDlg,IDC_BACK);
      if (g_flags.abort)
      {
        this_page->button_states|=16|4;
        SendMessage(g_hwnd,DM_SETDEFID,IDCANCEL,0);
      }
      else SendMessage(g_hwnd,DM_SETDEFID,IDOK,0);
      SetWindowLong(hwndtmp,GWL_STYLE,GetWindowLong(hwndtmp,GWL_STYLE)&~BS_DEFPUSHBUTTON);
      ShowWindow(hwndtmp,this_page->button_states&SW_SHOWNA);// SW_HIDE = 0, SW_SHOWNA = 8
      EnableWindow(hwndtmp,this_page->button_states&2);
      EnableWindow(m_hwndOK,!(this_page->button_states&16));
      EnableWindow(m_hwndCancel,this_page->button_states&4);

      mystrcpy(g_tmp,g_caption);
      process_string_fromtab(g_tmp+mystrlen(g_tmp),this_page->caption);
      my_SetWindowText(hwndDlg,g_tmp);

      SendMessage(m_curwnd, WM_NOTIFY_INIGO_MONTOYA, 0, 0);

#ifdef NSIS_SUPPORT_CODECALLBACKS
      if (ExecuteCodeSegment(this_page->prefunc,NULL) || this_page->id<0)
        goto nextPage;
#endif //NSIS_SUPPORT_CODECALLBACKS

      if (this_page->id!=NSIS_PAGE_COMPLETED) DestroyWindow(m_curwnd);
      else {
        if (g_flags.abort) SetFocus(m_hwndCancel);
        else if (g_flags.autoclose) goto nextPage;
        return 0;
      }

      if (this_page->id>=0) // NSIS page
      {
        m_curwnd=CreateDialog(
          g_hInstance,
          MAKEINTRESOURCE(windows[this_page->id].id+dlg_offset),
          hwndDlg,windows[this_page->id].proc
        );
        if (m_curwnd)
        {
          RECT r;
          GetWindowRect(GetDlgItem(hwndDlg,IDC_CHILDRECT),&r);
          ScreenToClient(hwndDlg,(LPPOINT)&r);
          SetWindowPos(m_curwnd,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
#ifdef NSIS_SUPPORT_CODECALLBACKS
          ExecuteCodeSegment(this_page->showfunc,NULL);
#endif //NSIS_SUPPORT_CODECALLBACKS
          ShowWindow(m_curwnd,SW_SHOWNA);
          SendMessage(m_curwnd, WM_NOTIFY_START, 0, 0);
        }

        //XGE 5th September 2002 - Do *not* move the focus to the OK button if we are
        //on the license page, instead we want the focus left alone because in
        //WM_INITDIALOG it is given to the richedit control.
        if (this_page->id != NSIS_PAGE_LICENSE)
          SetFocus(m_hwndOK);
        //XGE End
      }
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
  }
  if (uMsg == WM_CLOSE)
  {
    if (!IsWindowEnabled(m_hwndCancel) && IsWindowEnabled(m_hwndOK))
    {
      uMsg = WM_COMMAND;
      wParam = IDOK;
    }
  }
  if (uMsg == WM_COMMAND)
  {
    int id=LOWORD(wParam);

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
      if (g_flags.abort)
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        ExecuteCodeSegment(g_inst_cmnheader->code_onInstFailed,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS
        m_retcode=2;
        outernotify(NOTIFY_BYE_BYE);
      }
      else
      {
#ifdef NSIS_SUPPORT_CODECALLBACKS
        if (!ExecuteCodeSegment(g_inst_cmnheader->code_onUserAbort,NULL))
#endif//NSIS_SUPPORT_CODECALLBACKS
        {
          m_retcode=1;
          outernotify(NOTIFY_BYE_BYE);
        }
      }
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

static BOOL CALLBACK LicenseProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  HWND hwLicense;
  static unsigned int uLastAcceptState;
  if (uMsg == WM_INITDIALOG)
  {
    EDITSTREAM es={(DWORD)LANG_STR(LANG_LICENSE_DATA),0,StreamLicense};

    SetUITextFromLang(IDC_LICENSEAGREE,LANG_BTN_LICENSE_AGREE);
    SetUITextFromLang(IDC_LICENSEDISAGREE,LANG_BTN_LICENSE_DISAGREE);
    SendMessage(GetUIItem(IDC_LICENSEAGREE+!uLastAcceptState), BM_SETCHECK, BST_CHECKED, 0);
    EnableWindow(m_hwndOK, uLastAcceptState | !(inst_flags&CH_FLAGS_LICENSE_FORCE_SELECTION));

    hwLicense=GetUIItem(IDC_EDIT1);
    SendMessage(hwLicense,EM_AUTOURLDETECT,TRUE,0);
    SendMessage(hwLicense,EM_SETBKGNDCOLOR,0,g_inst_header->license_bg>=0?g_inst_header->license_bg:GetSysColor(COLOR_BTNFACE));
    SendMessage(hwLicense,EM_SETEVENTMASK,0,ENM_LINK|ENM_KEYEVENTS); //XGE 8th September 2002 Or'd in ENM_KEYEVENTS
    dwRead=0;
    SendMessage(hwLicense,EM_EXLIMITTEXT,0,mystrlen((char*)es.dwCookie));
    SendMessage(hwLicense,EM_STREAMIN,(((char*)es.dwCookie)[0]=='{')?SF_RTF:SF_TEXT,(LPARAM)&es);
    SetUITextFromLang(IDC_INTROTEXT,LANG_LICENSE_TEXT);
    //XGE 5th September 2002 - place the initial focus in the richedit control
    SetFocus(hwLicense);
    return FALSE;
    //End Xge
  }
  if (uMsg == WM_COMMAND && HIWORD(wParam) == BN_CLICKED) {
    if (inst_flags&CH_FLAGS_LICENSE_FORCE_SELECTION)
      EnableWindow(
        m_hwndOK,
        uLastAcceptState = SendMessage(GetUIItem(IDC_LICENSEAGREE), BM_GETCHECK, 0, 0) & BST_CHECKED
      );
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
        if (msgfilter->wParam==VK_RETURN && IsWindowEnabled(m_hwndOK)) {
          outernotify(1);
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
  return HandleStaticBkColor();
}
#endif

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
static BOOL CALLBACK UninstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    SetUITextFromLang(IDC_INTROTEXT,LANG_UNINST_TEXT);
    SetUITextFromLang(IDC_UNINSTFROM,LANG_UNINST_SUBTEXT);
    SetUITextNT(IDC_EDIT1,state_install_directory);
  }
  return HandleStaticBkColor();
}
#endif


static char * NSISCALL inttosizestr(int kb, char *str)
{
  char sh=20;
  char s=0;
  int scale=LANG_GIGA;
  if (kb < 1024) { sh=0; scale=LANG_KILO; }
  else if (kb < 1024*1024) { sh=10; scale=LANG_MEGA; }
  else if (GetVersion()&0x80000000) s='+';//only display the + on GB shown on win9x.
  wsprintf(str+mystrlen(str),"%d.%d%s%s%c",kb>>sh,((kb*10)>>sh)%10,LANG_STR(scale),LANG_STR(LANG_BYTE),s);
  return str;
}

static BOOL CALLBACK DirProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_NOTIFY_INIGO_MONTOYA)
  {
    GetUIText(IDC_DIR,state_install_directory,NSIS_MAX_STRLEN);
    validate_filename(state_install_directory);
#ifdef NSIS_CONFIG_LOG
    build_g_logfile();
    log_dolog = IsDlgButtonChecked(hwndDlg,IDC_CHECK1);
#endif
  }
  if (uMsg == WM_INITDIALOG)
  {
#ifdef NSIS_CONFIG_LOG
    if (GetAsyncKeyState(VK_SHIFT)&0x8000)
    {
      HWND h=GetUIItem(IDC_CHECK1);
      my_SetWindowText(h,LANG_STR(LANG_LOG_INSTALL_PROCESS));
      ShowWindow(h,SW_SHOWNA);
    }
#endif
    SetUITextNT(IDC_DIR,state_install_directory);
    SetUITextFromLang(IDC_INTROTEXT,LANG_DIR_TEXT);
    SetUITextFromLang(IDC_BROWSE,LANG_BTN_BROWSE);
    SetUITextFromLang(IDC_SELDIRTEXT,LANG_DIR_SUBTEXT);
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
          post_str=process_string(p);
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

    mystrcpy(s,state_install_directory);
    if (s[1] == ':') s[3]=0;
    else if (*(WORD*)s == CHAR2_TO_WORD('\\','\\')) // \\ path
    {
      char *p = mystrstr(s+2,"\\");
      if (p) {
        p = mystrstr(p+1,"\\");
        if (p) *p = 0;
      }
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
      if (g_inst_section[x].flags&SF_SELECTED)
#endif
       total+=g_inst_section[x].size_kb;
    }

    if (LANG_STR_TAB(LANG_SPACE_REQ)) {
      SetUITextNT(IDC_SPACEREQUIRED,inttosizestr(total,mystrcpy(s,LANG_STR(LANG_SPACE_REQ))));
      if (available != -1)
        SetUITextNT(IDC_SPACEAVAILABLE,inttosizestr(available,mystrcpy(s,LANG_STR(LANG_SPACE_AVAIL))));
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
  
  pFlags = &(g_inst_section[tvItem.lParam].flags);

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

  iFlags = &(g_inst_section[tvItem.lParam].flags);
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
  MapWindowPoints(HWND_DESKTOP, tree, &ht.pt, 1);

  TreeView_HitTest(tree, &ht);
  
  if (ht.flags & (TVHT_ONITEMSTATEICON|TVHT_ONITEMLABEL|TVHT_ONITEMRIGHT|TVHT_ONITEM))
    return ht.hItem;

  return 0;
}

static LONG oldTreeWndProc;
static DWORD WINAPI newTreeWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static LPARAM last_item=-1;
  if (uMsg == WM_KEYDOWN && wParam == VK_SPACE)
  {
    SendMessage(m_curwnd,WM_TREEVIEW_KEYHACK,0,0);
  }
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
  if (uMsg == WM_DESTROY) {
    last_item=-1;
  }
  if (uMsg == WM_MOUSEMOVE) {
    TVITEM tvItem;

    if (GetWindowLong(hwnd, GWL_STYLE) & WS_VISIBLE) {
      tvItem.hItem = TreeHitTest(hwnd);
      
      lParam = -1;

      if (tvItem.hItem)
      {
        tvItem.mask = TVIF_PARAM;

        TreeView_GetItem(hwnd, &tvItem);
        
        lParam = tvItem.lParam;
      }
      uMsg = WM_USER+0x19;
    }
  }
  if (uMsg == WM_USER+0x19) {
    if (last_item != lParam)
    {
      last_item = lParam;

      mystrcpy(g_tmp, g_usrvars[0]);

      myitoa(g_usrvars[0], lParam);

      ExecuteCodeSegment(g_inst_header->code_onMouseOverSection,NULL);

      mystrcpy(g_usrvars[0], g_tmp);
    }
  }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_ENHANCEDUI_SUPPORT
  return CallWindowProc((WNDPROC)oldTreeWndProc,hwnd,uMsg,wParam,lParam);
}

int m_num_insttypes;

static BOOL CALLBACK SelProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static HTREEITEM *hTreeItems;
  static HIMAGELIST hImageList;
  HWND hwndCombo1 = GetUIItem(IDC_COMBO1);
  HWND hwndTree1 = GetUIItem(IDC_TREE1);
  extern HWND g_SectionHack;
  if (uMsg == WM_INITDIALOG)
  {
    int doLines=0;
    HTREEITEM Par;
    HBITMAP hBMcheck1;
    int x, lastGoodX;

    g_SectionHack=hwndDlg;

    if (hTreeItems) GlobalFree(hTreeItems);
    hTreeItems=(HTREEITEM*)my_GlobalAlloc(sizeof(HTREEITEM)*num_sections);

    hBMcheck1=LoadBitmap(g_hInstance, MAKEINTRESOURCE(IDB_BITMAP1));
    SetUITextFromLang(IDC_INTROTEXT,LANG_COMP_TEXT);
    SetUITextFromLang(IDC_TEXT1,LANG_COMP_SUBTEXT(0));
    SetUITextFromLang(IDC_TEXT2,LANG_COMP_SUBTEXT(1));

    oldTreeWndProc=SetWindowLong(hwndTree1,GWL_WNDPROC,(DWORD)newTreeWndProc);

    if (hImageList) ImageList_Destroy(hImageList);

    hImageList = ImageList_Create(16,16, ILC_COLOR32|ILC_MASK, 6, 0);
    ImageList_AddMasked(hImageList,hBMcheck1,RGB(255,0,255));

    TreeView_SetImageList(hwndTree1, hImageList, TVSIL_STATE);

    DeleteObject(hBMcheck1);

    if (!g_inst_header->install_types_ptr[0])
    {
      ShowWindow(hwndCombo1,SW_HIDE);
    }
    else
    {
      for (m_num_insttypes = 0; m_num_insttypes < NSIS_MAX_INST_TYPES &&
           g_inst_header->install_types_ptr[m_num_insttypes]; m_num_insttypes ++)
      {
        SendMessage(hwndCombo1,CB_ADDSTRING,0,(LPARAM)GetStringFromStringTab(g_inst_header->install_types_ptr[m_num_insttypes]));
      }
      if (!(inst_flags&CH_FLAGS_NO_CUSTOM))
        SendMessage(hwndCombo1,CB_ADDSTRING,0,(LPARAM)LANG_STR(LANG_COMP_CUSTOM));
      SendMessage(hwndCombo1,CB_SETCURSEL,m_whichcfg,0);
    }

    Par=NULL;

    for (lastGoodX = x = 0; x < num_sections; x ++)
    {
      section *sec=g_inst_section+x;

      if (m_num_insttypes && m_whichcfg != m_num_insttypes)
      {
        if ((sec->install_types>>m_whichcfg) & 1)
          sec->flags|=SF_SELECTED;
        else
          sec->flags&=~SF_SELECTED;
      }

      if (sec->name_ptr)
      {
        TVINSERTSTRUCT tv;
        tv.hParent=Par;
        tv.hInsertAfter=TVI_LAST;
        tv.item.mask=TVIF_PARAM|TVIF_TEXT|TVIF_STATE;
        tv.item.lParam=x;
        process_string_fromtab(0,sec->name_ptr);
        tv.item.pszText=ps_tmpbuf;
        tv.item.stateMask=TVIS_STATEIMAGEMASK|TVIS_EXPANDED;

        {
          int l=1;
          if (sec->flags & SF_SELECTED) l++;
          if (sec->flags & SF_RO) l+=3;

          tv.item.state=INDEXTOSTATEIMAGEMASK(l);
        }

        if (sec->flags&SF_BOLD)
        {
          tv.item.stateMask|=TVIS_BOLD;
          tv.item.state|=TVIS_BOLD;
        }

        if (sec->flags&SF_SUBSEC)
        {
          tv.item.mask|=TVIF_CHILDREN;
          tv.item.cChildren=1;
          if (sec->flags&SF_EXPAND)
            tv.item.state|=TVIS_EXPANDED;
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

    uMsg=WM_IN_UPDATEMSG;
  }
  if (uMsg == WM_USER+0x17) // update text
  {
    int x=wParam;
    int ns=lParam;
    TVITEM tv;

    if (tv.hItem=hTreeItems[x])
    {
      tv.mask=TVIF_TEXT;
      process_string_fromtab(0,ns);
      tv.pszText=ps_tmpbuf;
      TreeView_SetItem(hwndTree1,&tv);
    }
  }
  if (uMsg == WM_USER+0x18) // change flags
  {
    int flags = g_inst_section[wParam].flags;
    TVITEM tvItem;

    if (!(tvItem.hItem = hTreeItems[wParam])) return 0;
    tvItem.mask = TVIF_STATE;
    tvItem.stateMask = TVIS_BOLD;
    tvItem.state = 0;
    if (flags&SF_BOLD) tvItem.state |= TVIS_BOLD;
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
      if (!(inst_flags&CH_FLAGS_NO_CUSTOM) && (uMsg == WM_TREEVIEW_KEYHACK || lpnmh->code == NM_CLICK))
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
              g_inst_section[tvItem.lParam].flags&=~SF_SELECTED;
              CheckTreeItem(hwndTree1,tvItem.hItem,0);
            }
            else
            {
              g_inst_section[tvItem.lParam].flags|=SF_SELECTED;
              CheckTreeItem(hwndTree1,tvItem.hItem,1);
            }
            lParam = 0;
            uMsg = WM_IN_UPDATEMSG;
          } // not ro
        } // was valid click
      } // was click or hack
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_ENHANCEDUI_SUPPORT)
      if (lpnmh)
      {
        if (lpnmh->code == TVN_SELCHANGED)
          SendMessage(hwndTree1, WM_USER+0x19, 0, ((LPNMTREEVIEW)lpnmh)->itemNew.lParam);
        if (lpnmh->code == TVN_ITEMEXPANDED)
        {
          LPNMTREEVIEW pnmtv = (LPNMTREEVIEW) lpnmh;
          if (pnmtv->action == TVE_EXPAND)
            g_inst_section[pnmtv->itemNew.lParam].flags |= SF_EXPAND;
          else
            g_inst_section[pnmtv->itemNew.lParam].flags &= ~SF_EXPAND;
        }
      }
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_ENHANCEDUI_SUPPORT
    }
  }
  if (uMsg == WM_MOUSEMOVE)
  {
    SendMessage(hwndTree1, WM_MOUSEMOVE, 0, 0);
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
        if (m_whichcfg != m_num_insttypes) // not custom
        {
          int x=num_sections;
          section *t=g_inst_section;
          HTREEITEM *ht=hTreeItems;
          while (x--)
          {
            TVITEM tv;
            int l=1;

            if (t->install_types & (1<<m_whichcfg))
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
        lParam = 1;
        uMsg = WM_IN_UPDATEMSG;
      }
    }
  }
  if (uMsg == WM_NOTIFY_INIGO_MONTOYA)
  {
    if (hImageList) ImageList_Destroy(hImageList);
    if (hTreeItems) GlobalFree(hTreeItems);
    hTreeItems=0;
    g_SectionHack=0;
  }
  if (uMsg == WM_IN_UPDATEMSG)
  {
#if defined(NSIS_SUPPORT_CODECALLBACKS) && defined(NSIS_CONFIG_COMPONENTPAGE)
    ExecuteCodeSegment(g_inst_header->code_onSelChange,NULL);
#endif//NSIS_SUPPORT_CODECALLBACKS && NSIS_CONFIG_COMPONENTPAGE
    if (inst_flags&CH_FLAGS_COMP_ONLY_ON_CUSTOM)
    {
      int c=(m_whichcfg == m_num_insttypes && m_num_insttypes)<<3;// SW_SHOWNA=8, SW_HIDE=0
      ShowWindow(hwndTree1,c);
      ShowWindow(GetUIItem(IDC_TEXT2),c);
    }
    else if (!lParam)
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
      }

      m_whichcfg=r;
      SendMessage(hwndCombo1,CB_SETCURSEL,m_whichcfg,0);
    } // end of typecheckshit

    if (LANG_STR_TAB(LANG_SPACE_REQ)) {
      int x,total;
      char s[128];
      for (total=x=0; x < num_sections; x ++)
      {
        if (g_inst_section[x].flags&SF_SELECTED)
          total+=g_inst_section[x].size_kb;
      }
      SetUITextNT(IDC_SPACEREQUIRED,inttosizestr(total,mystrcpy(s,LANG_STR(LANG_SPACE_REQ))));
    }
  }
  return HandleStaticBkColor();
}
#endif//NSIS_CONFIG_COMPONENTPAGE

#endif//NSIS_CONFIG_VISIBLE_SUPPORT

int ui_st_updateflag=0x3;

void NSISCALL update_status_text_from_lang(int id, const char *text2)
{
  update_status_text(LANG_STR(id), text2);
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
    if ((ui_st_updateflag&2)) my_SetWindowText(insthwnd2,tmp);
  }
}


static DWORD WINAPI install_thread(LPVOID p)
{
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (g_is_uninstaller)
  {
    if (ExecuteCodeSegment(g_inst_uninstheader->code,g_progresswnd)) g_flags.abort++;
  }
  else
  {
#endif
    int m_inst_sec=0;
    while (m_inst_sec<num_sections && !g_flags.abort)
    {
#ifdef NSIS_CONFIG_COMPONENTPAGE
      if (g_inst_section[m_inst_sec].flags&SF_SELECTED)
#endif
      {
        log_printf2("Section: \"%s\"",GetStringFromStringTab(g_inst_section[m_inst_sec].name_ptr));
        if (ExecuteCodeSegment(g_inst_section[m_inst_sec].code,g_progresswnd)) g_flags.abort++;
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
  if (m_curwnd) SendMessage(m_curwnd,WM_NOTIFY_INSTPROC_DONE,0,0);
  return g_flags.abort;
}

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
static BOOL CALLBACK InstProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_INITDIALOG)
  {
    RECT r;
    int num=0;
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
      int x;
      log_printf3("New install of \"%s\" to \"%s\"",LANG_STR(LANG_NAME),state_install_directory);
      for (x=0; x < num_sections; x ++)
      {
#ifdef NSIS_CONFIG_COMPONENTPAGE
        if (g_inst_section[x].flags&SF_SELECTED)
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
    my_SetWindowText(insthwndbutton,LANG_STR(LANG_BTN_DETAILS));
    if (inst_flags&(CH_FLAGS_DETAILS_SHOWDETAILS|CH_FLAGS_DETAILS_NEVERSHOW))
    {
      ShowWindow(insthwndbutton,SW_HIDE);
      if (!(inst_flags&CH_FLAGS_DETAILS_NEVERSHOW)) ShowWindow(insthwnd,SW_SHOWNA);
      else insthwndbutton=NULL;
    }
    progress_bar_len=num;

    g_progresswnd=GetUIItem(IDC_PROGRESS);
    SendMessage(g_progresswnd,PBM_SETRANGE,0,MAKELPARAM(0,30000));
    if (inst_flags&CH_FLAGS_PROGRESS_COLORED)
    {
      SendMessage(g_progresswnd,PBM_SETBARCOLOR,0,lb_fg);
      SendMessage(g_progresswnd,PBM_SETBKCOLOR,0,lb_bg);
    }

    EnableWindow(m_hwndOK,0);

    return FALSE;
  }
  if (uMsg == WM_NOTIFY_START) {
    DWORD id;
    CloseHandle(CreateThread(NULL,0,install_thread,0,0,&id));
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
    {
      m_retcode=1;
      outernotify(NOTIFY_BYE_BYE);
    }
    else
    {
      ShowWindow(g_hwnd,SW_SHOWNA);
      if (!g_flags.abort)
        update_status_text_from_lang(LANG_COMPLETED,"");
      outernotify(1);
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
      AppendMenu(menu,MF_STRING,1,LANG_STR(LANG_COPYDETAILS));
    	if (1==TrackPopupMenu(
        menu,
        TPM_NONOTIFY|TPM_RETURNCMD,
        GET_X_LPARAM(pos),
        GET_Y_LPARAM(pos),
        0,insthwnd,0))
      {
        char textBuf[1024];
        int i,total = 1; // 1 for the null char
        LVITEM item;
        HGLOBAL memory;
        LPTSTR ptr;//,endPtr;

        // 1st pass - determine clipboard memory required.
        item.iSubItem   = 0;
        item.pszText    = textBuf;
        item.cchTextMax = 1023;
        i = count;
        while (i--)
          // Add 2 for the CR/LF combination that must follow every line.
          total += 2+SendMessage(insthwnd,LVM_GETITEMTEXT,i,(LPARAM)&item);

        // 2nd pass - store detail view strings on the clipboard
        // Clipboard MSDN docs say mem must be GMEM_MOVEABLE
        OpenClipboard(0);
        EmptyClipboard();
        memory = GlobalAlloc(GHND,total);
        ptr = GlobalLock(memory);
        //endPtr = ptr+total-2; // -2 to allow for CR/LF
        i = 0;
        do {
          ListView_GetItemText(insthwnd,i,0,ptr,total);
          while (*ptr) ptr++;
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
