#include <windows.h>
#include <shlobj.h>
#include <shellapi.h>
#include "fileform.h"
#include "util.h"
#include "state.h"
#include "ui.h"
#include "exec.h"
#include "lang.h"
#include "resource.h"

#define EXEC_ERROR 0x7FFFFFFF

#ifdef NSIS_CONFIG_COMPONENTPAGE
HWND g_SectionHack;
#endif

#ifdef NSIS_SUPPORT_STACK
typedef struct _stack_t {
  struct _stack_t *next;
  char text[NSIS_MAX_STRLEN];
} stack_t;

static stack_t *g_st;
#endif

static int exec_errorflag;
#ifdef NSIS_SUPPORT_REBOOT
static int exec_rebootflag;
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
HBITMAP g_hBrandingBitmap = 0;
#endif

#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
char plugins_temp_dir[NSIS_MAX_STRLEN]="";
#endif

extern HWND m_curwnd;

static WIN32_FIND_DATA * NSISCALL file_exists(char *buf)
{
  HANDLE h;
  static WIN32_FIND_DATA fd;
  h = FindFirstFile(buf,&fd);
  if (h != INVALID_HANDLE_VALUE)
  {
    FindClose(h);
    return &fd;
  }
  return NULL;
}

#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
// based loosely on code from Tim Kosse
// in win9x this isn't necessary (RegDeleteKey() can delete a tree of keys),
// but in win2k you need to do this manually.
static LONG NSISCALL myRegDeleteKeyEx(HKEY thiskey, LPCTSTR lpSubKey, int onlyifempty)
{
	HKEY key;
	int retval=RegOpenKeyEx(thiskey,lpSubKey,0,KEY_ALL_ACCESS,&key);
	if (retval==ERROR_SUCCESS)
	{
    // NB - don't change this to static (recursive function)
		char buffer[MAX_PATH+1];
		while (RegEnumKey(key,0,buffer,MAX_PATH+1)==ERROR_SUCCESS)
    {
      if (onlyifempty)
      {
    		RegCloseKey(key);
        return !ERROR_SUCCESS;
      }
      if ((retval=myRegDeleteKeyEx(key,buffer,0)) != ERROR_SUCCESS) break;
    }
		RegCloseKey(key);
		retval=RegDeleteKey(thiskey,lpSubKey);
	}
	return retval;
}
#endif//NSIS_SUPPORT_REGISTRYFUNCTIONS

extern char g_all_user_var_flag;

static int NSISCALL ExecuteEntry(entry *entry_);

static int NSISCALL resolveaddr(int v)
{
  if (v<0) return myatoi(g_usrvars[-(v+1)]);  // if <0, that means we
  return v;
}

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress)
{
  while (pos >= 0)
  {
    int rv;
    if (g_inst_entry[pos].which == EW_RET) return 0;
    rv=ExecuteEntry(g_inst_entry + pos);
    if (rv == EXEC_ERROR) return EXEC_ERROR;

    rv=resolveaddr(rv);

    if (!rv) { rv++; pos++; }
    else
    {
      int t=pos;
      rv--; // rv is decremented here by 1, since it was +1 on the other end.
      pos=rv; // set new position
      rv-=t; // set rv to delta for progress adjustment
    }

    if (hwndProgress)
    {
      extern int progress_bar_pos, progress_bar_len;
      progress_bar_pos+=rv;
      SendMessage(hwndProgress,PBM_SETPOS,MulDiv(progress_bar_pos,30000,progress_bar_len+!progress_bar_len),0);
    }
  }
  return 0;
}

static char bufs[5][NSIS_MAX_STRLEN];
static int *parms;

static int NSISCALL process_string_fromparm_toint(int id_)
{
  return myatoi(process_string(ps_tmpbuf,GetStringFromStringTab(parms[id_])));
}

// NB - USE CAUTION when rearranging code to make use of the new return value of
// this function - be sure the parm being accessed is not modified before the call.
static char * NSISCALL process_string_fromparm_tobuf(int id_)
{
  return process_string_fromtab(bufs[id_ >> 4], parms[id_ & 0xF]);
}

// returns EXEC_ERROR on error
// returns 0, advance position by 1
// otherwise, returns new_position+1
static int NSISCALL ExecuteEntry(entry *entry_)
{
  char *buf0 = bufs[0];
  char *buf1 = bufs[1];
  char *buf2 = bufs[2];
  char *buf3 = bufs[3];
  char *buf4 = bufs[4];

  // changed by Amir Szekely 28 August 2002
  // shaves off 0.5KB
  int parm0 = (parms = entry_->offsets)[0]; // the ordering of these makes a size diff (4 bytes) -Justin
  char *var0 = g_usrvars[parm0];
  int parm1 = parms[1];
  char *var1 = g_usrvars[parm1];
  int parm2 = parms[2];
  char *var2 = g_usrvars[parm2];
  int parm3 = parms[3];
  char *var3 = g_usrvars[parm3];
  int parm4 = parms[4];
  int parm5 = parms[5];
//char *var4 = g_usrvars[parm4];  // not used yet
//char *var5 = g_usrvars[parm5];
  int which = entry_->which;
  switch (which)
  {
    case EW_NOP:
      log_printf2("Jump: %d",parm0);
    return parm0;
    case EW_ABORT:
      {
        char *buf0=process_string_fromparm_tobuf(0x00);
        log_printf2("Aborting: \"%s\"",buf0);
        update_status_text("",buf0);
      }
    return EXEC_ERROR;
    case EW_QUIT:
      g_quit_flag++;
      if (g_hwnd) PostQuitMessage(0); // make sure we bail out fast.
    return EXEC_ERROR;
    case EW_CALL:
      {
        int v=resolveaddr(parm0)-1;  // address is -1, since we encode it as +1
        log_printf2("Call: %d",v);
        return ExecuteCodeSegment(v,NULL);
      }
    case EW_UPDATETEXT:
      if (parm1) {
        static int old_st_updateflag=3;
        if (parm1&8) ui_st_updateflag=old_st_updateflag;
        else {
          old_st_updateflag=ui_st_updateflag;
          ui_st_updateflag=parm1;
        }
      }
      else
      {
        char *buf3=process_string_fromparm_tobuf(0x30);
        log_printf2("detailprint: %s",buf3);
        update_status_text(buf3,"");
      }
    return 0;
    case EW_SLEEP:
      {
        int x=process_string_fromparm_toint(0);
        if (x < 1) x=1;
        log_printf2("Sleep(%d)",x);
        Sleep(x);
      }
    return 0;
    case EW_SETSFCONTEXT:
      g_all_user_var_flag=parm0;
    return 0;
    case EW_HIDEWINDOW:
      log_printf("HideWindow");
      ShowWindow(g_hwnd,SW_HIDE);
    return 0;
    case EW_BRINGTOFRONT:
      log_printf("BringToFront");
      ShowWindow(g_hwnd,SW_SHOW);
      SetForegroundWindow(g_hwnd);
    return 0;
    case EW_SETWINDOWCLOSE:
      g_autoclose=parm0;
    return 0;
    case EW_CHDETAILSVIEW:
      if (insthwndbutton) ShowWindow(insthwndbutton,parm1);
      if (insthwnd) ShowWindow(insthwnd,parm0);
    return 0;
    case EW_SETFILEATTRIBUTES: {
      char *buf0=process_string_fromparm_tobuf(0x00);
      log_printf3("SetFileAttributes: \"%s\":%08X",buf0,parm1);
      if (!SetFileAttributes(buf0,parm1))
      {
        exec_errorflag++;
        log_printf("SetFileAttributes failed.");
      }
    }
    return 0;
    case EW_CREATEDIR: {
      char *buf1=process_string_fromparm_tobuf(0x10);
      log_printf3("CreateDirectory: \"%s\" (%d)",buf1,parm1);
      if (parm1)
      {
        update_status_text_from_lang(LANG_OUTPUTDIR,buf1);
        mystrcpy(state_output_directory,buf1);
      }
      else update_status_text_from_lang(LANG_CREATEDIR,buf1);
      {
        char *tp;
        char *p;
        p=buf1;
        while (*p == ' ') p=CharNext(p);
        if (*p) {
          tp=CharNext(p);
          if (*(WORD*)tp == CHAR2_TO_WORD(':','\\')) p=tp+2;
          else if (*(WORD*)p == CHAR2_TO_WORD('\\','\\'))
          {
            int x;
            for (x = 0; x < 2; x ++)
            {
              while (*p != '\\' && *p) p=CharNext(p); // skip host then share
              if (*p) p=CharNext(p);
            }

          }
          else return 0;
          while (*p)
          {
            while (*p != '\\' && *p) p=CharNext(p);
            if (!*p) CreateDirectory(buf1,NULL);
            else
            {
              *p=0;
              CreateDirectory(buf1,NULL);
              *p++ = '\\';
            }
          }
        }
      }
    }
    return 0;
    case EW_IFFILEEXISTS: {
      char *buf0=process_string_fromparm_tobuf(0x00);
      if (file_exists(buf0))
      {
        log_printf3("IfFileExists: file \"%s\" exists, jumping %d",buf0,parm1);
        return parm1;
      }
      log_printf3("IfFileExists: file \"%s\" does not exist, jumping %d",buf0,parm2);
    }
    return parm2;
    case EW_IFERRORS:
      {
        int f=exec_errorflag;
        exec_errorflag=parm2;
        if (f)
        {
          return parm0;
        }
      }
    return parm1;
#ifdef NSIS_SUPPORT_RENAME
    case EW_RENAME:
      {
        char *buf0=process_string_fromparm_tobuf(0x00);
        char *buf1=process_string_fromparm_tobuf(0x11);
        mystrcpy(buf3,buf0);
        if (mystrlen(buf0)+mystrlen(buf1) < NSIS_MAX_STRLEN-3)
        {
          lstrcat(buf3,"->");
          lstrcat(buf3,buf1);
        }
        log_printf2("Rename: %s",buf3);
        if (MoveFile(buf0,buf1))
        {
          update_status_text_from_lang(LANG_RENAME,buf3);
        }
        else
        {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
          if (parm2 && file_exists(buf0))
          {
#ifdef NSIS_SUPPORT_REBOOT
            exec_rebootflag++;
#endif
            MoveFileOnReboot(buf0,buf1);
            update_status_text_from_lang(LANG_RENAMEONREBOOT,buf3);
            log_printf2("Rename on reboot: %s",buf3);
          }
          else
#endif
          {
            exec_errorflag++;
            log_printf2("Rename failed: %s",buf3);
          }
        }
      }
    return 0;
#endif//NSIS_SUPPORT_RENAME
#ifdef NSIS_SUPPORT_FNUTIL
    case EW_GETFULLPATHNAME:
      {
        char *fp;
        char *p=var0;
        char *buf0=process_string_fromparm_tobuf(0x01);
        if (!GetFullPathName(buf0,NSIS_MAX_STRLEN,p,&fp))
        {
          exec_errorflag++;
          *p=0;
        }
        else if (fp>buf0 && *fp)
        {
          WIN32_FIND_DATA *fd=file_exists(buf0);
          if (fd)
          {
            mystrcpy(fp,fd->cFileName);
          }
          else
          {
            exec_errorflag++;
            *p=0;
          }
        }
        if (!parm2) GetShortPathName(p,p,NSIS_MAX_STRLEN);
      }
    return 0;
    case EW_SEARCHPATH:
      {
        char *fp;
        char *p=var0;
        char *buf0=process_string_fromparm_tobuf(0x01);
        if (!SearchPath(NULL,buf0,NULL,NSIS_MAX_STRLEN,p,&fp))
        {
          exec_errorflag++;
          p[0]=0;
        }
      }
    return 0;
    case EW_GETTEMPFILENAME:
      {
        char *textout=var0;
        if (!GetTempFileName(temp_directory,"nst",0,textout))
        {
          exec_errorflag++;
          *textout=0;
        }
      }
    return 0;
#endif
#ifdef NSIS_SUPPORT_FILE
    case EW_EXTRACTFILE:
      {
        HANDLE hOut;
        int ret;
        char *buf3=process_string_fromparm_tobuf(0x31);
        #define overwriteflag parm0

        log_printf3("File: overwriteflag=%d, name=\"%s\"",overwriteflag,buf3);
        if (validpathspec(buf3))
        {
          mystrcpy(buf0,buf3);
        }
        else lstrcat(addtrailingslash(mystrcpy(buf0,state_output_directory)),buf3);
      _tryagain:
        if (!overwriteflag)
        {
          int attr=GetFileAttributes(buf0);
          if (attr & FILE_ATTRIBUTE_READONLY)
            SetFileAttributes(buf0,attr^FILE_ATTRIBUTE_READONLY);
        }
        if (overwriteflag == 3) // check date and time
        {
          WIN32_FIND_DATA *ffd=file_exists(buf0);
          overwriteflag=1; // if it doesn't exist, fall back to no overwrites (since it shouldn't matter anyway)
          if (ffd)
          {
            overwriteflag=(CompareFileTime(&ffd->ftLastWriteTime,(FILETIME*)(parms+3)) >= 0);  // if first one is newer, then don't overwrite
          }
        }
        hOut=myOpenFile(buf0,GENERIC_WRITE,(overwriteflag==1)?CREATE_NEW:CREATE_ALWAYS);
        if (hOut == INVALID_HANDLE_VALUE)
        {
          if (overwriteflag)
          {
            update_status_text_from_lang(LANG_SKIPPED,buf3);
            if (overwriteflag==2) exec_errorflag++;
            log_printf3("File: skipped: \"%s\" (overwriteflag=%d)",buf0,overwriteflag);
            return 0;
          }
          log_printf2("File: error creating \"%s\"",buf0);
          mystrcpy(buf2,g_usrvars[0]);//save $0
          mystrcpy(g_usrvars[0],buf0);

          process_string_fromtab(buf1,LANG_FILEERR);
          mystrcpy(g_usrvars[0],buf2); // restore $0

          switch (my_MessageBox(buf1,MB_ABORTRETRYIGNORE|MB_ICONSTOP))
          {
            case IDRETRY:
              log_printf("File: error, user retry");
              goto _tryagain;
            case IDIGNORE:
              log_printf("File: error, user cancel");
              exec_errorflag++;
              return 0;
            default:
              log_printf("File: error, user abort");
              update_status_text_from_lang(LANG_CANTWRITE,buf0);
            return EXEC_ERROR;
          }
        }

        update_status_text_from_lang(LANG_EXTRACT,buf3);
        ret=GetCompressedDataFromDataBlock(parm2,hOut);

        log_printf3("File: wrote %d to \"%s\"",ret,buf0);

        if (parm3 != 0xffffffff || parm4 != 0xffffffff)
          SetFileTime(hOut,(FILETIME*)(parms+3),NULL,(FILETIME*)(parms+3));

        CloseHandle(hOut);

        if (ret < 0)
        {
          if (ret == -2)
          {
            wsprintf(buf0,"%s%s",LANG_STR(LANG_ERRORWRITING),buf3);
          }
          else
          {
            mystrcpy(buf0,LANG_STR(LANG_ERRORDECOMPRESSING));
          }
          log_printf2("%s",buf0);
          my_MessageBox(buf0,MB_OK|MB_ICONSTOP);
          return EXEC_ERROR;
        }

        #undef overwriteflag
      }
    return 0;
#endif//NSIS_SUPPORT_FILE
#ifdef NSIS_SUPPORT_DELETE
    case EW_DELETEFILE:
      {
		    HANDLE h;
		    WIN32_FIND_DATA fd;
        char *buf1=process_string_fromparm_tobuf(0x10);
        mystrcpy(buf0,buf1);
        log_printf2("Delete: \"%s\"",buf0);
        trimslashtoend(buf0);
        h=FindFirstFile(buf1,&fd);
		    if (h != INVALID_HANDLE_VALUE)
		    {
          do
          {
			      if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
            {
              wsprintf(buf1,"%s\\%s",buf0,fd.cFileName);
              if (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
                SetFileAttributes(buf1,fd.dwFileAttributes^FILE_ATTRIBUTE_READONLY);
              if (DeleteFile(buf1))
              {
                log_printf2("Delete: DeleteFile(\"%s\")",buf1);
                update_status_text_from_lang(LANG_DELETEFILE,buf1);
              }
              else
              {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
                if (parm1)
                {
#ifdef NSIS_SUPPORT_REBOOT
                  exec_rebootflag++;
#endif
                  log_printf2("Delete: DeleteFile on Reboot(\"%s\")",buf1);
                  update_status_text_from_lang(LANG_DELETEONREBOOT,buf1);
                  MoveFileOnReboot(buf1,NULL);
                }
                else
#endif
                {
                  exec_errorflag++;
                }
              }
            }
          } while (FindNextFile(h,&fd));
			    FindClose(h);
    		}
      }
    return 0;
#endif//NSIS_SUPPORT_DELETE
#ifdef NSIS_SUPPORT_MESSAGEBOX
    case EW_MESSAGEBOX: // MessageBox
      {
        int v;
        char *buf3=process_string_fromparm_tobuf(0x31);
        log_printf3("MessageBox: %d,\"%s\"",parm0,buf3);
        v=my_MessageBox(buf3,parm0);
        if (v)
        {
          if (v==(parm2&0xffff))
          {
            return parm3;
          }
          if (v==(parm2>>16))
          {
            return parm4;
          }
        }
        else exec_errorflag++;
      }
    return 0;
#endif//NSIS_SUPPORT_MESSAGEBOX
#ifdef NSIS_SUPPORT_RMDIR
    case EW_RMDIR:
      {
        char *buf0=process_string_fromparm_tobuf(0x00);
        log_printf2("RMDir: \"%s\"",buf0);

        if (lastchar(buf0)=='\\') trimslashtoend(buf0);

        doRMDir(buf0,parm1);
        if (file_exists(buf0)) exec_errorflag++;
      }
    return 0;
#endif//NSIS_SUPPORT_RMDIR
#ifdef NSIS_SUPPORT_STROPTS
    case EW_STRLEN: {
      char *buf0=process_string_fromparm_tobuf(0x01);
      myitoa(var0,mystrlen(buf0));
    }
    return 0;
    case EW_ASSIGNVAR:
      {
        int newlen=process_string_fromparm_toint(2);
        int start=process_string_fromparm_toint(3);
        int l;
        char *p=var0;
        char *buf0=process_string_fromparm_tobuf(0x01);
        *p=0;
        if (!parm2 || newlen)
        {
          l=mystrlen(buf0);

          if (start<0) start=l+start;
          if (start>=0)
          {
            if (start>l) start=l;
            mystrcpy(p,buf0+start);
            if (newlen)
            {
              if (newlen<0) newlen=mystrlen(p)+newlen;
              if (newlen<0) newlen=0;
              if (newlen < NSIS_MAX_STRLEN) p[newlen]=0;
            }
          }
        }
      }
    return 0;
    case EW_STRCMP: {
      char *buf2=process_string_fromparm_tobuf(0x20);
      char *buf3=process_string_fromparm_tobuf(0x31);
      if (!lstrcmpi(buf2,buf3)) return parm2;
    }
    return parm3;
#endif//NSIS_SUPPORT_STROPTS
#ifdef NSIS_SUPPORT_ENVIRONMENT
    case EW_READENVSTR:
      {
        char *p=var0;
        char *buf0=process_string_fromparm_tobuf(0x01);
        if (parm2)
        {
          if (!GetEnvironmentVariable(buf0,p,NSIS_MAX_STRLEN))
          {
            exec_errorflag++;
            *p=0;
          }
        }
        else
        {
          ExpandEnvironmentStrings(buf0,p,NSIS_MAX_STRLEN);
        }
        p[NSIS_MAX_STRLEN-1]=0;
      }
    return 0;
#endif//NSIS_SUPPORT_ENVIRONMENT
#ifdef NSIS_SUPPORT_INTOPTS
    case EW_INTCMP:
      {
        int v,v2;
        v=process_string_fromparm_toint(0);
        v2=process_string_fromparm_toint(1);
        if (v<v2) return parm3;
        if (v>v2) return parm4;
      }
    return parm2;
    case EW_INTCMPU:
      {
        unsigned int v,v2;
        v=(unsigned int)process_string_fromparm_toint(0);
        v2=(unsigned int)process_string_fromparm_toint(1);
        if (v<v2) return parm3;
        if (v>v2) return parm4;
      }
    return parm2;
    case EW_INTOP:
      {
        int v,v2;
        char *p=var0;
        v=process_string_fromparm_toint(1);
        v2=process_string_fromparm_toint(2);
        switch (parm3)
        {
          case 0: v+=v2; break;
          case 1: v-=v2; break;
          case 2: v*=v2; break;
          case 3: if (v2) v/=v2; else { v=0; exec_errorflag++; } break;
          case 4: v|=v2; break;
          case 5: v&=v2; break;
          case 6: v^=v2; break;
          case 7: v=~v; break;
          case 8: v=!v; break;
          case 9: v=v||v2; break;
          case 10: v=v&&v2; break;
          case 11: if (v2) v%=v2; else { v=0; exec_errorflag++; } break;
        }
        myitoa(p,v);
      }
    return 0;
    case EW_INTFMT: {
      char *buf0=process_string_fromparm_tobuf(0x01);
      wsprintf(var0,
               buf0,
               process_string_fromparm_toint(2));
    }
    return 0;
#endif//NSIS_SUPPORT_INTOPTS
#ifdef NSIS_SUPPORT_STACK
    case EW_PUSHPOP:
      {
        stack_t *s=g_st;
        int cnt=parm2;
        if (cnt) //Exch contributed by Fritz Elfert
        {
          while (cnt--&&s) s=s->next;
          if (!s)
          {
            log_printf2("Exch: stack < %d elements",parm2);
            break;
          }
          mystrcpy(buf0,s->text);
          mystrcpy(s->text,g_st->text);
          mystrcpy(g_st->text,buf0);
        }
        else if (parm1)
        {
          if (!s)
          {
            log_printf("Pop: stack empty");
            exec_errorflag++;
            return 0;
          }
          mystrcpy(var0,s->text);
          g_st=s->next;
          GlobalFree((HGLOBAL)s);
        }
        else
        {
          s=(stack_t*)my_GlobalAlloc(sizeof(stack_t));
          process_string_fromtab(s->text,parm0);
          s->next=g_st;
          g_st=s;
        }
      }
    return 0;
#endif//NSIS_SUPPORT_STACK
#ifdef NSIS_SUPPORT_HWNDS
    case EW_FINDWINDOW:
    case EW_SENDMESSAGE:
      {
        int v;
        int b3=process_string_fromparm_toint(3);
        int b4=process_string_fromparm_toint(4);

        if (which == EW_SENDMESSAGE)
        {
          HWND hwnd=(HWND)process_string_fromparm_toint(1);
          int msg=process_string_fromparm_toint(2);
          if (parm5&1)
          {
            b3=(int)process_string_fromparm_tobuf(0x03);
          }
          if (parm5&2)
          {
            b4=(int)process_string_fromparm_tobuf(0x14);
          }

          if (parm5>>2) exec_errorflag += !SendMessageTimeout(hwnd,msg,b3,b4,SMTO_NORMAL,parm5>>2,(LPDWORD)&v);
          else v=SendMessage(hwnd,msg,b3,b4);
        }
        else
        {
          char *buf0=process_string_fromparm_tobuf(0x01);
          char *buf1=process_string_fromparm_tobuf(0x12);
          v=(int)FindWindowEx((HWND)b3,(HWND)b4,buf0[0]?buf0:NULL,buf1[0]?buf1:NULL);
        }

        if (parm0>=0)
          myitoa(var0,v);
      }
    return 0;
    case EW_ISWINDOW:
        if (IsWindow((HWND)process_string_fromparm_toint(0))) return parm1;
    return parm2;
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case EW_GETDLGITEM:
      myitoa(
        var0,
        (int)GetDlgItem(
          (HWND)process_string_fromparm_toint(1),
          process_string_fromparm_toint(2)
        )
      );
    return 0;
    case EW_SETSTATICBKCOLOR:
      SetWindowLong(
        (HWND)process_string_fromparm_toint(0),
        GWL_USERDATA,
        (LONG)CreateSolidBrush(parm1)
      );
    return 0;
    case EW_SETBRANDINGIMAGE:
    {
      RECT r;
      HWND hwImage = GetDlgItem(g_hwnd, parm1);
      GetWindowRect(hwImage, &r);
      if (g_hBrandingBitmap) DeleteObject(g_hBrandingBitmap);
      g_hBrandingBitmap=LoadImage(
        0,
        process_string_fromparm_tobuf(0x00),
        IMAGE_BITMAP,
        parm2?r.right-r.left:0,
        parm2?r.bottom-r.top:0,
        LR_LOADFROMFILE
      );
      SendMessage(
        hwImage,
        STM_SETIMAGE,
        IMAGE_BITMAP,
        (LPARAM)g_hBrandingBitmap
      );
    }
    return 0;
    case EW_CREATEFONT:
    {
      static LOGFONT f;
      f.lfHeight=-MulDiv(process_string_fromparm_toint(2),GetDeviceCaps(GetDC(g_hwnd),LOGPIXELSY),72);
      f.lfWeight=process_string_fromparm_toint(3);
      f.lfItalic=parm4&1;
      f.lfUnderline=parm4&2;
      f.lfStrikeOut=parm4&4;
      f.lfCharSet=DEFAULT_CHARSET; 
      process_string_fromtab(f.lfFaceName,parm1);
      myitoa(var0,(int)CreateFontIndirect(&f));
    }
    return 0;
    case EW_SHOWWINDOW:
      ShowWindow((HWND)process_string_fromparm_toint(0),process_string_fromparm_toint(1));
    return 0;
#endif//NSIS_CONFIG_ENHANCEDUI_SUPPORT
#endif//NSIS_SUPPORT_HWNDS
#ifdef NSIS_SUPPORT_SHELLEXECUTE
    case EW_SHELLEXEC: // this uses improvements of Andras Varga
      {
        int x;
        char *buf0=process_string_fromparm_tobuf(0x00);
        char *buf1=process_string_fromparm_tobuf(0x11);
        char *buf2=process_string_fromparm_tobuf(0x22);
        wsprintf(buf3,"%s %s",buf0,buf1);
        update_status_text_from_lang(LANG_EXECSHELL, buf3);
        x=(int)ShellExecute(g_hwnd,buf0[0]?buf0:NULL,buf1,buf2[0]?buf2:NULL,state_output_directory,parm3);
        if (x < 33)
        {
          log_printf5("ExecShell: warning: error (\"%s\": file:\"%s\" params:\"%s\")=%d",buf0,buf1,buf2,x);
          exec_errorflag++;
        }
        else
        {
          log_printf4("ExecShell: success (\"%s\": file:\"%s\" params:\"%s\")",buf0,buf1,buf2);
        }
      }
    return 0;
#endif//NSIS_SUPPORT_SHELLEXECUTE
#ifdef NSIS_SUPPORT_EXECUTE
    case EW_EXECUTE:
      {
        HANDLE hProc;
        char *buf0=process_string_fromparm_tobuf(0x00);
        log_printf2("Exec: command=\"%s\"",buf0);
        update_status_text_from_lang(LANG_EXECUTE,buf0);

        hProc=myCreateProcess(buf0,state_output_directory);

        if (hProc)
        {
          log_printf2("Exec: success (\"%s\")",buf0);
          if (parm1)
          {
            DWORD lExitCode;
            while (WaitForSingleObject(hProc,100) == WAIT_TIMEOUT)
            {
              static MSG msg;
              while (PeekMessage(&msg,NULL,WM_PAINT,WM_PAINT,PM_REMOVE))
                DispatchMessage(&msg);
            }
            GetExitCodeProcess(hProc, &lExitCode);

            if (parm2>=0) myitoa(var2,lExitCode);
            else if (lExitCode) exec_errorflag++;
          }
          CloseHandle( hProc );
        }
        else
        {
          exec_errorflag++;
          log_printf2("Exec: failed createprocess (\"%s\")",buf0);
        }
      }
    return 0;
#endif//NSIS_SUPPORT_EXECUTE
#ifdef NSIS_SUPPORT_GETFILETIME
    case EW_GETFILETIME:
      // this new implementation based on one by Dave Bau
      // used FindFirstFile instead of GetFileTime to better handle files that are locked.
      // also allows GetFileTime to be passed a wildcard.
      {
        WIN32_FIND_DATA *ffd;
        char *highout=var1;
        char *lowout=var2;
        char *buf0=process_string_fromparm_tobuf(0x00);

        ffd=file_exists(buf0);
        if (ffd)
        {
          myitoa(lowout,ffd->ftLastWriteTime.dwLowDateTime);
          myitoa(highout,ffd->ftLastWriteTime.dwHighDateTime);
        }
        else
        {
          *lowout=*highout=0;
          exec_errorflag++;
        }
      }
    return 0;
#endif//NSIS_SUPPORT_GETFILETIME
#ifdef NSIS_SUPPORT_GETDLLVERSION
    case EW_GETDLLVERSION:
      {
        char *highout=var1;
        char *lowout=var2;
        DWORD s1;
        DWORD t[4]; // our two members are the 3rd and 4th..
        VS_FIXEDFILEINFO *pvsf1=(VS_FIXEDFILEINFO*)t;
        DWORD d;
        char *buf0=process_string_fromparm_tobuf(0x00);
        s1=GetFileVersionInfoSize(buf0,&d);
        *lowout=*highout=0;
        exec_errorflag++;
        if (s1)
        {
          void *b1;
          b1=my_GlobalAlloc(s1);
          if (b1)
          {
            UINT uLen;
            if (GetFileVersionInfo(buf0,0,s1,b1) && VerQueryValue(b1,"\\",(void*)&pvsf1,&uLen))
            {
              myitoa(highout,pvsf1->dwFileVersionMS);
              myitoa(lowout,pvsf1->dwFileVersionLS);

              exec_errorflag--;
            }
            GlobalFree(b1);
          }
        }
      }
      return 0;
#endif//NSIS_SUPPORT_GETDLLVERSION
#ifdef NSIS_SUPPORT_ACTIVEXREG
    case EW_REGISTERDLL:
      {
        HRESULT hres=OleInitialize(NULL);
        exec_errorflag++;
        if (hres == S_FALSE || hres == S_OK)
        {
          HANDLE h;
          char *buf0=process_string_fromparm_tobuf(0x00);
          char *buf1=process_string_fromparm_tobuf(0x11);

          h=LoadLibrary(buf0);
          if (h)
          {
            FARPROC funke = GetProcAddress(h,buf1);
            if (funke)
            {
              exec_errorflag--;
              if (parm2)
              {
                char *buf2=process_string_fromparm_tobuf(0x22);

                // suggested by Kevin Gadd (janusfury)
                mystrcpy(buf3, buf0);
                trimslashtoend(buf3);
                SetCurrentDirectory(buf3);

                update_status_text(buf2,buf0);
                if (funke()) exec_errorflag++;

                SetCurrentDirectory(state_exe_directory);
              }
              else
              {
                void (*func)(HWND,int,char*,void*);
                func=(void*)funke;
                func(g_hwnd,NSIS_MAX_STRLEN,(char*)g_usrvars,
#ifdef NSIS_SUPPORT_STACK
                (void*)&g_st);
#else
                NULL);
#endif
              }
            }
            else
            {
              update_status_text_from_lang(LANG_CANNOTFINDSYMBOL,buf1);
              log_printf3("Error registering DLL: %s not found in %s",buf1,buf0);
            }
            if (!parm3) while (FreeLibrary(h));
            // saves 2 bytes - FreeLibrary((HANDLE)((unsigned long)h&(unsigned long)parm3));
          }
          else
          {
            update_status_text_from_lang(LANG_COULDNOTLOAD,buf0);
            log_printf2("Error registering DLL: Could not load %s",buf0);
          }
          OleUninitialize();
        }
        else
        {
          update_status_text_from_lang(LANG_NOOLE,buf0);
          log_printf("Error registering DLL: Could not initialize OLE");
        }
      }
    return 0;
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
    case EW_CREATESHORTCUT: {
      char *buf2=process_string_fromparm_tobuf(0x20);
      char *buf1=process_string_fromparm_tobuf(0x11);
      char *buf0=process_string_fromparm_tobuf(0x02);
      char *buf3=process_string_fromparm_tobuf(0x33);
      char *buf4=process_string_fromparm_tobuf(0x45);

      HRESULT hres;
      int rv=1;
      IShellLink* psl;

      log_printf8("CreateShortCut: out: \"%s\", in: \"%s %s\", icon: %s,%d, sw=%d, hk=%d",
        buf2,buf1,buf0,buf3,parm4&0xff,(parm4&0xff00)>>8,parm4>>16);

      hres=OleInitialize(NULL);
      if (hres == S_FALSE || hres == S_OK) {

        hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                                  &IID_IShellLink, (void **) &psl);
        if (SUCCEEDED(hres))
        {
          IPersistFile* ppf;

          hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile, (void **) &ppf);
          if (SUCCEEDED(hres))
          {

             hres = psl->lpVtbl->SetPath(psl,buf1);
             psl->lpVtbl->SetWorkingDirectory(psl,state_output_directory);
             if ((parm4&0xff00)>>8) psl->lpVtbl->SetShowCmd(psl,(parm4&0xff00)>>8);
             psl->lpVtbl->SetHotkey(psl,(unsigned short)(parm4>>16));
             if (buf3[0]) psl->lpVtbl->SetIconLocation(psl,buf3,parm4&0xff);
             psl->lpVtbl->SetArguments(psl,buf0);
             psl->lpVtbl->SetDescription(psl,buf4);

             if (SUCCEEDED(hres))
             {
                WCHAR wsz[1024];
                MultiByteToWideChar(CP_ACP, 0, buf2, -1, wsz, 1024);
                hres=ppf->lpVtbl->Save(ppf,(const WCHAR*)wsz,TRUE);
                if (SUCCEEDED(hres)) rv=0;
             }
            ppf->lpVtbl->Release(ppf);
          }
          psl->lpVtbl->Release(psl);
        }
        OleUninitialize();
      }

      if (rv)
      {
        exec_errorflag++;
        update_status_text_from_lang(LANG_ERRORCREATINGSHORTCUT,buf2);
      }
      else
      {
        update_status_text_from_lang(LANG_CREATESHORTCUT,buf2);
      }
    }
    return 0;
#endif//NSIS_SUPPORT_CREATESHORTCUT
#ifdef NSIS_SUPPORT_COPYFILES
    case EW_COPYFILES: // CopyFile (added by NOP)
      {
        int res;
		    SHFILEOPSTRUCT op;
        char *buf0=process_string_fromparm_tobuf(0x00);
        char *buf1=process_string_fromparm_tobuf(0x11);
        log_printf3("CopyFiles \"%s\"->\"%s\"",buf0,buf1);
			  op.hwnd=g_hwnd;
			  op.wFunc=FO_COPY;
        buf0[mystrlen(buf0)+1]=0;
        buf1[mystrlen(buf1)+1]=0;

        wsprintf(buf2,"%s%s",LANG_STR(LANG_COPYTO),buf1);

        op.pFrom=buf0;
        op.pTo=buf1;
        op.lpszProgressTitle=buf2;
			  op.fFlags=parm2;
        update_status_text("",buf2);
			  res=SHFileOperation(&op);
			  if (res)
        { // some of these changes were from Edgewise (wiked_edge@yahoo.com)
          update_status_text_from_lang(LANG_COPYFAILED,"");
          exec_errorflag++;
			  }
    	}
		return 0;
#endif//NSIS_SUPPORT_COPYFILES
#ifdef NSIS_SUPPORT_REBOOT
    case EW_REBOOT:
      exec_errorflag++;
      {
        HANDLE h=LoadLibrary("advapi32.dll");
        if (h)
        {
          BOOL (WINAPI *OPT)(HANDLE, DWORD,PHANDLE);
          BOOL (WINAPI *LPV)(LPCTSTR,LPCTSTR,PLUID);
          BOOL (WINAPI *ATP)(HANDLE,BOOL,PTOKEN_PRIVILEGES,DWORD,PTOKEN_PRIVILEGES,PDWORD);
          OPT=(void*)GetProcAddress(h,"OpenProcessToken");
          LPV=(void*)GetProcAddress(h,"LookupPrivilegeValueA");
          ATP=(void*)GetProcAddress(h,"AdjustTokenPrivileges");
          if (OPT && LPV && ATP)
          {
            HANDLE hToken;
            TOKEN_PRIVILEGES tkp;
            if (OPT(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
            {
              LPV(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);
              tkp.PrivilegeCount = 1;
              tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
              ATP(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES)NULL, 0);
            }
          }
        }

        if (ExitWindowsEx(EWX_REBOOT|EWX_FORCE,0)) ExitProcess(0);

        FreeLibrary(h);

        return 0;
      }
    break;
    case EW_IFREBOOTFLAG: return parms[!exec_rebootflag];
    case EW_SETREBOOTFLAG: exec_rebootflag=parm0; return 0;
#endif//NSIS_SUPPORT_REBOOT
#ifdef NSIS_SUPPORT_INIFILES
    case EW_WRITEINI:
      {
        char *sec=0, *key=0, *str=0;
#ifdef NSIS_CONFIG_LOG
        mystrcpy(buf1,"<RM>");
        mystrcpy(buf2,buf1);
#endif
        if (parm0)
        {
          sec=process_string_fromparm_tobuf(0x00);
        }
        if (parm1)
        {
          key=process_string_fromparm_tobuf(0x11);
        }
        if (parm2)
        {
          str=process_string_fromparm_tobuf(0x22);
        }
        buf3=process_string_fromparm_tobuf(0x33);
        log_printf5("WriteINIStr: wrote [%s] %s=%s in %s",buf0,buf1,buf2,buf3);
        if (!WritePrivateProfileString(sec,key,str,buf3)) exec_errorflag++;
      }
    return 0;
    case EW_READINISTR:
      {
        static const char *errstr="!N~";
        char *p=var0;
        char *buf0=process_string_fromparm_tobuf(0x01);
        char *buf1=process_string_fromparm_tobuf(0x12);
        char *buf2=process_string_fromparm_tobuf(0x23);
        GetPrivateProfileString(buf0,buf1,errstr,p,NSIS_MAX_STRLEN-1,buf2);
        if (*((int*)errstr) == *((int*)p))
        {
          exec_errorflag++;
          p[0]=0;
        }
      }
    return 0;
#endif//NSIS_SUPPORT_INIFILES
#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
    case EW_DELREG:
      {
        int rootkey=parm0;
        char *buf3=process_string_fromparm_tobuf(0x31);
        exec_errorflag++;
        if (!parm3)
        {
          HKEY hKey;
          if (RegOpenKeyEx((HKEY)rootkey,buf3,0,KEY_ALL_ACCESS,&hKey) == ERROR_SUCCESS)
          {
            char *buf0=process_string_fromparm_tobuf(0x02);
            log_printf4("DeleteRegValue: %d\\%s\\%s",rootkey,buf3,buf0);
            if (RegDeleteValue(hKey,buf0) == ERROR_SUCCESS) exec_errorflag--;
            RegCloseKey(hKey);
          }
        }
        else
        {
          log_printf3("DeleteRegKey: %d\\%s",rootkey,buf3);
          if (myRegDeleteKeyEx((HKEY)rootkey,buf3,parm3&2) == ERROR_SUCCESS) exec_errorflag--;
        }
      }
    return 0;
    case EW_WRITEREG: // write registry value
      {
        HKEY hKey;
        int rootkey=parm0;
        int type=parm4;
        char *buf1=process_string_fromparm_tobuf(0x12);
        char *buf3=process_string_fromparm_tobuf(0x31);
        exec_errorflag++;
        if (RegCreateKey((HKEY)rootkey,buf3,&hKey) == ERROR_SUCCESS)
        {
          if (type <= 1)
          {
            char *buf2=process_string_fromparm_tobuf(0x23);
            if (RegSetValueEx(hKey,buf1,0,type==1?REG_SZ:REG_EXPAND_SZ,buf2,mystrlen(buf2)+1) == ERROR_SUCCESS) exec_errorflag--;
            log_printf5("WriteRegStr: set %d\\%s\\%s to %s",rootkey,buf3,buf1,buf2);
          }
          else if (type == 2)
          {
            DWORD l;
            l=process_string_fromparm_toint(3);
            if (RegSetValueEx(hKey,buf1,0,REG_DWORD,(unsigned char*)&l,4) == ERROR_SUCCESS) exec_errorflag--;
            log_printf5("WriteRegDWORD: set %d\\%s\\%s to %d",rootkey,buf3,buf1,l);
          }
          else if (type == 3)
          {
            int len=GetCompressedDataFromDataBlockToMemory(parm3, buf2, NSIS_MAX_STRLEN);
            if (len >= 0)
            {
              if (RegSetValueEx(hKey,buf1,0,REG_BINARY,buf2,len) == ERROR_SUCCESS) exec_errorflag--;
            }
            log_printf5("WriteRegBin: set %d\\%s\\%s with %d bytes",rootkey,buf3,buf1,len);

          }
          RegCloseKey(hKey);
        }
        else { log_printf3("WriteReg: error creating key %d\\%s",rootkey,buf3); }
      }
    return 0;
    case EW_READREGSTR: // read registry string
      {
        HKEY hKey;
        char *p=var0;
        int rootkey=parm1;
        char *buf0=process_string_fromparm_tobuf(0x02); // buf0 == subkey
        char *buf1=process_string_fromparm_tobuf(0x13); // buf1 == key name
        p[0]=0;
        if (RegOpenKeyEx((HKEY)rootkey,buf0,0,KEY_READ,&hKey) == ERROR_SUCCESS)
        {
			    DWORD l = NSIS_MAX_STRLEN;
			    DWORD t;

          if (RegQueryValueEx(hKey,buf1,NULL,&t,p,&l ) != ERROR_SUCCESS ||
              (t != REG_DWORD && t != REG_SZ && t != REG_EXPAND_SZ))
          {
            p[0]=0;
            exec_errorflag++;
          }
          else
          {
            if (t==REG_DWORD)
            {
              if (!parm4) exec_errorflag++;
              myitoa(p,*((DWORD*)p));
            }
            else if (parm4) exec_errorflag++;
          }
          RegCloseKey(hKey);
        }
        else exec_errorflag++;
     }
    return 0;
    case EW_REGENUM:
      {
        HKEY key;
        char *p=var0;
        int b=process_string_fromparm_toint(3);
        char *buf1=process_string_fromparm_tobuf(0x12);
        p[0]=0;
        if (RegOpenKeyEx((HKEY)parm1,buf1,0,KEY_ALL_ACCESS,&key) == ERROR_SUCCESS)
        {
          DWORD d=NSIS_MAX_STRLEN-1;
          if (parm4) RegEnumKey(key,b,p,d);
          else RegEnumValue(key,b,p,&d,NULL,NULL,NULL,NULL);
          p[NSIS_MAX_STRLEN-1]=0;
          RegCloseKey(key);
        }
        else exec_errorflag++;
      }

    return 0;
#endif//NSIS_SUPPORT_REGISTRYFUNCTIONS
#ifdef NSIS_SUPPORT_FILEFUNCTIONS
    case EW_FCLOSE:
      {
        char *t=var0;
        if (*t) CloseHandle((HANDLE)myatoi(t));
      }
    return 0;
    case EW_FOPEN:
      {
        HANDLE h;
        char *handleout=var3;
        char *buf0=process_string_fromparm_tobuf(0x00);
        h=myOpenFile(buf0,parm1,parm2);
        if (h == INVALID_HANDLE_VALUE)
        {
          *handleout=0;
          exec_errorflag++;
        }
        else
        {
          myitoa(handleout,(int)h);
        }
      }
    return 0;
    case EW_FPUTS:
      {
        DWORD dw;
        int l;
        char *t=var0;
        if (parm2)
        {
          ((unsigned char *)buf1)[0]=process_string_fromparm_toint(1)&0xff;
          l=1;
        }
        else
        {
          l=mystrlen(process_string_fromparm_tobuf(0x11));
        }
        if (!*t || !WriteFile((HANDLE)myatoi(t),buf1,l,&dw,NULL))
        {
          exec_errorflag++;
        }
      }
    return 0;
    case EW_FGETS:
      {
        char *textout=var1;
        DWORD dw;
        int rpos=0;
        char *hptr=var0;
        int maxlen=process_string_fromparm_toint(2);
        if (maxlen<1) return 0;
        if (maxlen > NSIS_MAX_STRLEN-1) maxlen=NSIS_MAX_STRLEN-1;
        if (*hptr)
        {
          char lc=0;
          int rcnt=0;
          HANDLE h=(HANDLE)myatoi(hptr);
          while (rpos<maxlen)
          {
            char c;
            if (!ReadFile(h,&c,1,&dw,NULL) || dw != 1) break;
            if (parm3)
            {
              myitoa(textout,(unsigned int)(unsigned char)c);
              return 0;
            }
            if (!c) break;
            if (lc == '\r' || lc == '\n')
            {
              if (lc == c || (c != '\r' && c != '\n')) SetFilePointer(h,-1,NULL,FILE_CURRENT);
              else textout[rpos++]=c;
              break;
            }
            textout[rpos++]=c;
            lc=c;
          }
        }
        textout[rpos]=0;
        if (!rpos) exec_errorflag++;
      }
    return 0;
    case EW_FSEEK:
      {
        char *t=var0;
        if (*t)
        {
          DWORD v=SetFilePointer((HANDLE)myatoi(t),process_string_fromparm_toint(1),NULL,parm2);

          if (parm3>=0)
          {
            myitoa(var3,v);
          }
        }
      }
    return 0;
#endif//NSIS_SUPPORT_FILEFUNCTIONS
#ifdef NSIS_SUPPORT_FINDFIRST
    case EW_FINDCLOSE:
      {
        char *t=var0;
        if (*t) FindClose((HANDLE)myatoi(t));
      }
    return 0;
    case EW_FINDNEXT:
      {
        char *textout=var0;
        char *t=var1;
        WIN32_FIND_DATA fd;
        if (*t && FindNextFile((HANDLE)myatoi(t),&fd))
        {
          mystrcpy(textout,fd.cFileName);
        }
        else
        {
          exec_errorflag++;
          *textout=0;
        }

      }
    return 0;
    case EW_FINDFIRST:
      {
        char *textout=var1;
        char *handleout=var2;
        HANDLE h;
        WIN32_FIND_DATA fd;
        char *buf0=process_string_fromparm_tobuf(0x00);
        h=FindFirstFile(buf0,&fd);
        if (h == INVALID_HANDLE_VALUE)
        {
          *handleout=0;
          *textout=0;
          exec_errorflag++;
        }
        else
        {
          myitoa(handleout,(int)h);
          mystrcpy(textout,fd.cFileName);
        }
      }
    return 0;
#endif//NSIS_SUPPORT_FINDFIRST
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    case EW_WRITEUNINSTALLER:
      {
        int ret=-666;
        HANDLE hFile;
        char *buf0=process_string_fromparm_tobuf(0x00);

        if (validpathspec(buf0))
        {
          mystrcpy(buf1,buf0);
        }
        else
        {
          lstrcat(addtrailingslash(mystrcpy(buf1,state_install_directory)),buf0);
        }


        hFile=myOpenFile(buf1,GENERIC_WRITE,CREATE_ALWAYS);
        if (hFile != INVALID_HANDLE_VALUE)
        {
          unsigned char *filebuf;
          filebuf=(unsigned char *)my_GlobalAlloc(g_filehdrsize);
          if (filebuf)
          {
            int fixoffs=0;
            DWORD lout;
            SetSelfFilePointer(0,FILE_BEGIN);
            ReadSelfFile((char*)filebuf,g_filehdrsize);
            if (g_inst_header->uninstdata_offset != -1)
            {
              // Changed by Amir Szekely 11th July 2002
              unsigned char* seeker;
              unsigned char* unicon_data = seeker = (unsigned char*)my_GlobalAlloc(g_inst_header->uninsticon_size);
              if (unicon_data) {
                GetCompressedDataFromDataBlockToMemory(g_inst_header->uninstdata_offset,
                  unicon_data,g_inst_header->uninsticon_size);
                //for (i = 0; i < *(DWORD*)unicon_data; i++) {
                while (*seeker) {
                  DWORD dwSize, dwOffset;
                  dwSize = *(DWORD*)seeker;
                  seeker += sizeof(DWORD);
                  dwOffset = *(DWORD*)seeker;
                  seeker += sizeof(DWORD);
                  mini_memcpy(filebuf+dwOffset, seeker, dwSize);
                  seeker += dwSize;
                }
                GlobalFree(unicon_data);
              }
            }
            WriteFile(hFile,(char*)filebuf,g_filehdrsize,&lout,NULL);
            GlobalFree(filebuf);
            ret=GetCompressedDataFromDataBlock(-1,hFile);
          }
          CloseHandle(hFile);
        }
        log_printf3("created uninstaller: %d, \"%s\"",ret,buf1);
        if (ret < 0)
        {
          update_status_text_from_lang(LANG_ERRORCREATING,buf0);
          DeleteFile(buf1);
          exec_errorflag++;
        }
        else
          update_status_text_from_lang(LANG_CREATEDUNINST,buf0);
      }
    return 0;
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
#ifdef NSIS_CONFIG_LOG
    case EW_LOG:
      if (parm0)
      {
        log_printf2("settings logging to %d",parm1);
        log_dolog=parm1;
        log_printf2("logging set to %d",parm1);
        if (!g_log_file && log_dolog) build_g_logfile();
      }
      else
      {
        char *buf0=process_string_fromparm_tobuf(0x01);
        log_printf2("%s",buf0);
      }
    return 0;
#endif//NSIS_CONFIG_LOG
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case EW_SECTIONSET:
      {
        int x=process_string_fromparm_toint(0);
        if (g_inst_section && x >= 0 && x < g_inst_header->num_sections)
        {
          int z=0;
          if (g_SectionHack)
          {
            int a;
            for (a = 0; a < x; a ++) if (g_inst_section[a].name_ptr) z++;
          }

          if (parm1==0) //set text
          {
            if (g_SectionHack)
            {
              SendMessage(g_SectionHack,WM_USER+0x17,x,parm2);
            }
            g_inst_section[x].name_ptr=parm2;
          }
          else if (parm1==1) // get text
          {
            process_string_fromtab(var2,g_inst_section[x].name_ptr);
          }
          else if (parm1==2) // set flags
          {
            g_inst_section[x].flags=process_string_fromparm_toint(2);
            if (g_SectionHack)
            {
              SendMessage(g_SectionHack,WM_USER+0x18,x,(LPARAM)(g_inst_section[x].flags&SF_SELECTED));
            }
          }
          else // get flags
          {
            myitoa(var2,g_inst_section[x].flags);
          }
        }
        else exec_errorflag++;
      }
    return 0;
#endif//NSIS_CONFIG_COMPONENTPAGE

    // Added by Ximon Eighteen 5th August 2002
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    case EW_PLUGINCOMMANDPREP:
      // $0 temp plug-ins dir
      if (!*plugins_temp_dir) mystrcpy(plugins_temp_dir,g_usrvars[0]);
    return 0;
#endif // NSIS_CONFIG_PLUGIN_SUPPORT
  }
  my_MessageBox(LANG_STR(LANG_INSTCORRUPTED),MB_OK|MB_ICONSTOP);
  return EXEC_ERROR;
}
