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

static WIN32_FIND_DATA *file_exists(char *buf)
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
static LONG myRegDeleteKeyEx(HKEY thiskey, LPCTSTR lpSubKey, int onlyifempty)
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

static int ExecuteEntry(entry *entries, int pos);

static int resolveaddr(int v)
{
  if (v<0) return myatoi(g_usrvars[-(v+1)]);  // if <0, that means we
  return v;
}

int ExecuteCodeSegment(entry *entries, int pos, HWND hwndProgress)
{
  while (pos >= 0)
  {
    int rv;
    if (entries[pos].which == EW_RET) return 0;
    rv=ExecuteEntry(entries,pos);
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
      progress_bar_len+=(!progress_bar_len);
      SendMessage(hwndProgress,PBM_SETPOS,MulDiv(progress_bar_pos,30000,progress_bar_len),0);
    }
  }
  return 0;
}

// returns EXEC_ERROR on error
// returns 0, advance position by 1
// otherwise, returns new_position+1
static int ExecuteEntry(entry *entries, int pos)
{
  static char buf[NSIS_MAX_STRLEN],buf2[NSIS_MAX_STRLEN],buf3[NSIS_MAX_STRLEN],buf4[NSIS_MAX_STRLEN];
  int *parms=entries[pos].offsets;
  int which=entries[pos].which;
  switch (which)
  {
    case EW_NOP:
      log_printf2("Jump: %d",parms[0]);
    return parms[0];
    case EW_ABORT:
      {
        process_string_fromtab(buf,parms[0]);
        log_printf2("Aborting: \"%s\"",buf);
        update_status_text("",buf);
      }
    return EXEC_ERROR;
    case EW_QUIT:
      g_quit_flag++;
      if (g_hwnd) PostQuitMessage(0); // make sure we bail out fast.
    return EXEC_ERROR;
    case EW_CALL:
      {
        int v=resolveaddr(parms[0])-1;  // address is -1, since we encode it as +1
        log_printf2("Call: %d",v);
        return ExecuteCodeSegment(entries,v,NULL);
      }
    case EW_UPDATETEXT:
      if (parms[1]) ui_st_updateflag=parms[1];
      else
      {
        process_string_fromtab(buf4,parms[0]);
        log_printf2("detailprint: %s",buf4);
        update_status_text(buf4,"");
      }
    return 0;
    case EW_SLEEP:
      {
        int x=process_string_fromtab_toint(parms[0]);
        if (x < 1) x=1;
        log_printf2("Sleep(%d)",x);
        Sleep(x);
      }
    return 0;
    case EW_SETSFCONTEXT:
      g_all_user_var_flag=parms[0];
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
      g_autoclose=parms[0];
    return 0;
    case EW_CHDETAILSVIEW:
      if (insthwndbutton) ShowWindow(insthwndbutton,parms[1]);
      if (insthwnd) ShowWindow(insthwnd,parms[0]);
    return 0;
    case EW_SETFILEATTRIBUTES:
      process_string_fromtab(buf,parms[0]);
      log_printf3("SetFileAttributes: \"%s\":%08X",buf,parms[1]);
      if (!SetFileAttributes(buf,parms[1]))
      {
        exec_errorflag++;
        log_printf("SetFileAttributes failed.");
      }
    return 0;
    case EW_CREATEDIR:
      process_string_fromtab(buf2,parms[0]);
      log_printf3("CreateDirectory: \"%s\" (%d)",buf2,parms[1]);
      if (parms[1])
      {
        update_status_text_from_lang(LANGID_OUTPUTDIR,buf2);
        mystrcpy(state_output_directory,buf2);
      }
      else update_status_text_from_lang(LANGID_CREATEDIR,buf2);
      recursive_create_directory(buf2);
    return 0;
    case EW_IFFILEEXISTS:
      process_string_fromtab(buf,parms[0]);
      if (file_exists(buf))
      {
        log_printf3("IfFileExists: file \"%s\" exists, jumping %d",buf,parms[1]);
        return parms[1];
      }
      log_printf3("IfFileExists: file \"%s\" does not exist, jumping %d",buf,parms[2]);
    return parms[2];
    case EW_IFERRORS:
      {
        int f=exec_errorflag;
        exec_errorflag=parms[2];
        if (f)
        {
          return parms[0];
        }
      }
    return parms[1];
#ifdef NSIS_SUPPORT_RENAME
    case EW_RENAME:
      {
        process_string_fromtab(buf,parms[0]);
        process_string_fromtab(buf2,parms[1]);
        mystrcpy(buf4,buf);
        if (mystrlen(buf)+mystrlen(buf2) < NSIS_MAX_STRLEN-3)
        {
          lstrcat(buf4,"->");
          lstrcat(buf4,buf2);
        }
        log_printf2("Rename: %s",buf4);
        if (MoveFile(buf,buf2))
        {
          update_status_text_from_lang(LANGID_RENAME,buf4);
        }
        else
        {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
          if (parms[2] && file_exists(buf))
          {
#ifdef NSIS_SUPPORT_REBOOT
            exec_rebootflag++;
#endif
            MoveFileOnReboot(buf,buf2);
            update_status_text_from_lang(LANGID_RENAMEONREBOOT,buf4);
            log_printf2("Rename on reboot: %s",buf4);
          }
          else
#endif
          {
            exec_errorflag++;
            log_printf2("Rename failed: %s",buf4);
          }
        }
      }
    return 0;
#endif//NSIS_SUPPORT_RENAME
#ifdef NSIS_SUPPORT_FNUTIL
    case EW_GETFULLPATHNAME:
      {
        char *p=g_usrvars[parms[0]];
        char *fp;
        process_string_fromtab(buf,parms[1]);
        if (!GetFullPathName(buf,NSIS_MAX_STRLEN,p,&fp))
        {
          exec_errorflag++;
          *p=0;
        }
        else if (fp>buf && *fp)
        {
          WIN32_FIND_DATA *fd=file_exists(buf);
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
        if (!parms[2]) GetShortPathName(p,p,NSIS_MAX_STRLEN);
      }
    return 0;
    case EW_SEARCHPATH:
      {
        char *fp;
        char *p=g_usrvars[parms[0]];
        process_string_fromtab(buf,parms[1]);
        if (!SearchPath(NULL,buf,NULL,NSIS_MAX_STRLEN,p,&fp))
        {
          p[0]=0;
          exec_errorflag++;
        }
      }
    return 0;
    case EW_GETTEMPFILENAME:
      {
        char *textout=g_usrvars[parms[0]];
        if (!GetTempPath(NSIS_MAX_STRLEN,buf) || !GetTempFileName(buf,"nst",0,textout))
        {
          *textout=0;
          exec_errorflag++;
        }
      }
    return 0;
#endif
#ifdef NSIS_SUPPORT_FILE
    case EW_EXTRACTFILE:
      {
        HANDLE hOut;
        int ret;
        int overwriteflag=parms[0];
        addtrailingslash(mystrcpy(buf,state_output_directory));

        process_string_fromtab(buf4,parms[1]);
        log_printf3("File: overwriteflag=%d, name=\"%s\"",overwriteflag,buf4);
        if (validpathspec(buf4))
        {
          mystrcpy(buf,buf4);
        }
        else lstrcat(buf,buf4);
      _tryagain:
        if (!overwriteflag)
        {
          int attr=GetFileAttributes(buf);
          if (attr & FILE_ATTRIBUTE_READONLY)
            SetFileAttributes(buf,attr^FILE_ATTRIBUTE_READONLY);
        }
        if (overwriteflag == 3) // check date and time
        {
          WIN32_FIND_DATA *ffd=file_exists(buf);
          overwriteflag=1; // if it doesn't exist, fall back to no overwrites (since it shouldn't matter anyway)
          if (ffd)
          {
            overwriteflag=(CompareFileTime(&ffd->ftLastWriteTime,(FILETIME*)(parms+3)) >= 0);  // if first one is newer, then don't overwrite
          }
        }
        hOut=myOpenFile(buf,GENERIC_WRITE,(overwriteflag==1)?CREATE_NEW:CREATE_ALWAYS);
        if (hOut == INVALID_HANDLE_VALUE)
        {
          if (overwriteflag)
          {
            update_status_text_from_lang(LANGID_SKIPPED,buf4);
            if (overwriteflag==2) exec_errorflag++;
            log_printf3("File: skipped: \"%s\" (overwriteflag=%d)",buf,overwriteflag);
            return 0;
          }
          log_printf2("File: error creating \"%s\"",buf);
          mystrcpy(buf3,g_usrvars[0]);//save $0
          mystrcpy(g_usrvars[0],buf);

          process_string_from_lang(buf2,LANGID_FILEERR);
          mystrcpy(g_usrvars[0],buf3); // restore $0

          switch (my_MessageBox(buf2,MB_ABORTRETRYIGNORE|MB_ICONSTOP))
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
              update_status_text_from_lang(LANGID_CANTWRITE,buf);
            return EXEC_ERROR;
          }
        }

        update_status_text_from_lang(LANGID_EXTRACT,buf4);
        ret=GetCompressedDataFromDataBlock(parms[2],hOut);

        log_printf3("File: wrote %d to \"%s\"",ret,buf);

        if (parms[3] != 0xffffffff || parms[4] != 0xffffffff)
          SetFileTime(hOut,(FILETIME*)(parms+3),NULL,(FILETIME*)(parms+3));

        CloseHandle(hOut);

        if (ret < 0)
        {
          if (ret == -2)
          {
            wsprintf(buf,"%s%s",STR(LANG_ERRORWRITING),buf4);
          }
          else
          {
            mystrcpy(buf,STR(LANG_ERRORDECOMPRESSING));
          }
          log_printf2("%s",buf);
          my_MessageBox(buf,MB_OK|MB_ICONSTOP);
          return EXEC_ERROR;
        }
      }
    return 0;
#endif//NSIS_SUPPORT_FILE
#ifdef NSIS_SUPPORT_DELETE
    case EW_DELETEFILE:
      {
		    HANDLE h;
		    WIN32_FIND_DATA fd;
        process_string_fromtab(buf2,parms[0]);
        mystrcpy(buf,buf2);
        log_printf2("Delete: \"%s\"",buf);
        trimslashtoend(buf);
    		h=FindFirstFile(buf2,&fd);
		    if (h != INVALID_HANDLE_VALUE)
		    {
          do
          {
			      if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
            {
              wsprintf(buf2,"%s\\%s",buf,fd.cFileName);
              if (fd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
                SetFileAttributes(buf2,fd.dwFileAttributes^FILE_ATTRIBUTE_READONLY);
              if (DeleteFile(buf2))
              {
                log_printf2("Delete: DeleteFile(\"%s\")",buf2);
                update_status_text_from_lang(LANGID_DELETEFILE,buf2);
              }
              else
              {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
                if (parms[1])
                {
#ifdef NSIS_SUPPORT_REBOOT
                  exec_rebootflag++;
#endif
                  log_printf2("Delete: DeleteFile on Reboot(\"%s\")",buf2);
                  update_status_text_from_lang(LANGID_DELETEONREBOOT,buf2);
                  MoveFileOnReboot(buf2,NULL);
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
        process_string_fromtab(buf4,parms[1]);
        log_printf3("MessageBox: %d,\"%s\"",parms[0],buf4);
        v=my_MessageBox(buf4,parms[0]);
        if (v)
        {
          if (v==(parms[2]&0xffff))
          {
            return parms[3];
          }
          if (v==(parms[2]>>16))
          {
            return parms[4];
          }
        }
        else exec_errorflag++;
      }
    return 0;
#endif//NSIS_SUPPORT_MESSAGEBOX
#ifdef NSIS_SUPPORT_RMDIR
    case EW_RMDIR:
      {
        process_string_fromtab(buf,parms[0]);
        log_printf2("RMDir: \"%s\"",buf);

        if (lastchar(buf)=='\\') trimslashtoend(buf);

        doRMDir(buf,parms[1]);
        if (file_exists(buf)) exec_errorflag++;
      }
    return 0;
#endif//NSIS_SUPPORT_RMDIR
#ifdef NSIS_SUPPORT_STROPTS
    case EW_STRLEN:
      process_string_fromtab(buf,parms[1]);
      myitoa(g_usrvars[parms[0]],mystrlen(buf));
    return 0;
    case EW_ASSIGNVAR:
      {
        int newlen=process_string_fromtab_toint(parms[2]);
        int start=process_string_fromtab_toint(parms[3]);
        int l;
        char *p=g_usrvars[parms[0]];
        process_string_fromtab(buf,parms[1]);
        *p=0;
        if (parms[2] < 0 || newlen)
        {
          l=mystrlen(buf);

          if (start<0) start=l+start;
          if (start>=0)
          {
            if (start>l) start=l;
            mystrcpy(p,buf+start);
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
    case EW_STRCMP:
      process_string_fromtab(buf3,parms[0]);
      process_string_fromtab(buf4,parms[1]);
      if (!lstrcmpi(buf3,buf4)) return parms[2];
    return parms[3];
#endif//NSIS_SUPPORT_STROPTS
#ifdef NSIS_SUPPORT_ENVIRONMENT
    case EW_READENVSTR:
      {
        char *p=g_usrvars[parms[0]];
        process_string_fromtab(buf,parms[1]);
        if (parms[2])
        {
          if (!GetEnvironmentVariable(buf,p,NSIS_MAX_STRLEN))
          {
            *p=0;
            exec_errorflag++;
          }
        }
        else
        {
          ExpandEnvironmentStrings(buf,p,NSIS_MAX_STRLEN);
        }
        p[NSIS_MAX_STRLEN-1]=0;
      }
    return 0;
#endif//NSIS_SUPPORT_ENVIRONMENT
#ifdef NSIS_SUPPORT_INTOPTS
    case EW_INTCMP:
      {
        int v,v2;
        v=process_string_fromtab_toint(parms[0]);
        v2=process_string_fromtab_toint(parms[1]);
        if (v<v2) return parms[3];
        if (v>v2) return parms[4];
      }
    return parms[2];
    case EW_INTCMPU:
      {
        unsigned int v,v2;
        v=(unsigned int)process_string_fromtab_toint(parms[0]);
        v2=(unsigned int)process_string_fromtab_toint(parms[1]);
        if (v<v2) return parms[3];
        if (v>v2) return parms[4];
      }
    return parms[2];
    case EW_INTOP:
      {
        int v,v2;
        char *p=g_usrvars[parms[0]];
        v=process_string_fromtab_toint(parms[1]);
        v2=process_string_fromtab_toint(parms[2]);
        switch (parms[3])
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
    case EW_INTFMT:
      process_string_fromtab(buf,parms[1]);
      wsprintf(g_usrvars[parms[0]],
               buf,
               process_string_fromtab_toint(parms[2]));
    return 0;
#endif//NSIS_SUPPORT_INTOPTS
#ifdef NSIS_SUPPORT_STACK
    case EW_PUSHPOP:
      {
        stack_t *s=g_st;
        int cnt=parms[2];
        if (cnt) //Exch contributed by Fritz Elfert
        {
          while (cnt--&&s) s=s->next;
          if (!s)
          {
            log_printf2("Exch: stack < %d elements",parms[2]);
            break;
          }
          mystrcpy(buf,s->text);
          mystrcpy(s->text,g_st->text);
          mystrcpy(g_st->text,buf);
        }
        else if (parms[1])
        {
          if (!s)
          {
            log_printf("Pop: stack empty");
            exec_errorflag++;
            return 0;
          }
          mystrcpy(g_usrvars[parms[0]],s->text);
          g_st=s->next;
          GlobalFree((HGLOBAL)s);
        }
        else
        {
          s=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t));
          process_string_fromtab(s->text,parms[0]);
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
        int b3=process_string_fromtab_toint(parms[3]);
        int b4=process_string_fromtab_toint(parms[4]);

        process_string_fromtab(buf,parms[1]);
        process_string_fromtab(buf2,parms[2]);

        if (which == EW_SENDMESSAGE) v=SendMessage((HWND)myatoi(buf),myatoi(buf2),b3,b4);
        else v=(int)FindWindowEx((HWND)b3,(HWND)b4,buf[0]?buf:NULL,buf2[0]?buf2:NULL);

        if (parms[0]>=0)
          myitoa(g_usrvars[parms[0]],v);
      }
    return 0;
    case EW_ISWINDOW:
        if (IsWindow((HWND)process_string_fromtab_toint(parms[0]))) return parms[1];
    return parms[2];
    case EW_SETDLGITEMTEXT:
      process_string_fromtab(buf,parms[2]);
      SetDlgItemText(parms[0]?g_hwnd:m_curwnd,parms[1],buf);
    return 0;
#endif
#ifdef NSIS_SUPPORT_SHELLEXECUTE
    case EW_SHELLEXEC: // this uses improvements of Andras Varga
      {
        int x;
        process_string_fromtab(buf,parms[0]);
        process_string_fromtab(buf2,parms[1]);
        process_string_fromtab(buf3,parms[2]);
        wsprintf(buf4,"%s %s",buf,buf2);
        update_status_text_from_lang(LANGID_EXECSHELL, buf4);
        x=(int)ShellExecute(g_hwnd,buf[0]?buf:NULL,buf2,buf3[0]?buf3:NULL,state_output_directory,parms[3]);
        if (x < 33)
        {
          log_printf5("ExecShell: warning: error (\"%s\": file:\"%s\" params:\"%s\")=%d",buf,buf2,buf3,x);
          exec_errorflag++;
        }
        else
        {
          log_printf4("ExecShell: success (\"%s\": file:\"%s\" params:\"%s\")",buf,buf2,buf3);
        }
      }
    return 0;
#endif//NSIS_SUPPORT_SHELLEXECUTE
#ifdef NSIS_SUPPORT_EXECUTE
    case EW_EXECUTE:
      {
        HANDLE hProc;
        process_string_fromtab(buf,parms[0]);
        log_printf2("Exec: command=\"%s\"",buf);
        update_status_text_from_lang(LANGID_EXECUTE,buf);

        hProc=myCreateProcess(buf,*state_output_directory?state_output_directory:NULL);

        if (hProc)
        {
          log_printf2("Exec: success (\"%s\")",buf);
          if (parms[1])
          {
            DWORD lExitCode;
            while (WaitForSingleObject(hProc,100) == WAIT_TIMEOUT)
            {
              static MSG msg;
              while (PeekMessage(&msg,NULL,WM_PAINT,WM_PAINT,PM_REMOVE))
                DispatchMessage(&msg);
            }
            GetExitCodeProcess(hProc, &lExitCode);

            if (parms[2]>=0) myitoa(g_usrvars[parms[2]],lExitCode);
            else if (lExitCode) exec_errorflag++;
          }
          CloseHandle( hProc );
        }
        else
        {
          exec_errorflag++;
          log_printf2("Exec: failed createprocess (\"%s\")",buf);
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
        char *highout=g_usrvars[parms[1]];
        char *lowout=g_usrvars[parms[2]];
        process_string_fromtab(buf,parms[0]);

        ffd=file_exists(buf);
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
        char *highout=g_usrvars[parms[1]];
        char *lowout=g_usrvars[parms[2]];
        DWORD s1;
        DWORD t[4]; // our two members are the 3rd and 4th..
        VS_FIXEDFILEINFO *pvsf1=(VS_FIXEDFILEINFO*)t;
        DWORD d;
        process_string_fromtab(buf,parms[0]);
        s1=GetFileVersionInfoSize(buf,&d);
        *lowout=*highout=0;
        exec_errorflag++;
        if (s1)
        {
          void *b1;
          b1=(void*)GlobalAlloc(GPTR,s1);
          if (b1)
          {
            UINT uLen;
            if (GetFileVersionInfo(buf,0,s1,b1) && VerQueryValue(b1,"\\",(void*)&pvsf1,&uLen))
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
          process_string_fromtab(buf,parms[0]);
          process_string_fromtab(buf2,parms[1]);

          h=LoadLibrary(buf);
          if (h)
          {
            FARPROC funke = GetProcAddress(h,buf2);
            if (funke)
            {
              exec_errorflag--;
              if (parms[2]<0)
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
              else
              {
                process_string_fromtab(buf3,parms[2]);
                update_status_text(buf3,buf);
                funke();
              }
            }
            else
            {
              update_status_text_from_lang(LANGID_CANNOTFINDSYMBOL,buf2);
              log_printf3("Error registering DLL: %s not found in %s",buf2,buf);
            }
            FreeLibrary(h);
          }
          else
          {
            update_status_text_from_lang(LANGID_COULDNOTLOAD,buf);
            log_printf2("Error registering DLL: Could not load %s",buf);
          }
          OleUninitialize();
        }
        else
        {
          update_status_text_from_lang(LANGID_NOOLE,buf);
          log_printf("Error registering DLL: Could not initialize OLE");
        }
      }
    return 0;
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
    case EW_CREATESHORTCUT:
      process_string_fromtab(buf3,parms[0]);
      process_string_fromtab(buf2,parms[1]);
      process_string_fromtab(buf, parms[2]);
      process_string_fromtab(buf4,parms[3]);

      log_printf8("CreateShortCut: out: \"%s\", in: \"%s %s\", icon: %s,%d, sw=%d, hk=%d",
          buf3,buf2,buf,buf4,parms[4]&0xff,(parms[4]&0xff00)>>8,parms[4]>>16);

      if (CreateShortCut(g_hwnd, buf3, buf4[0]?buf4:NULL, parms[4]&0xff, buf2, buf[0]?buf:NULL,
          state_output_directory,(parms[4]&0xff00)>>8,parms[4]>>16))
      {
        exec_errorflag++;
        update_status_text_from_lang(LANGID_ERRORCREATINGSHORTCUT,buf3);
      }
      else
      {
        update_status_text_from_lang(LANGID_CREATESHORTCUT,buf3);
      }
    return 0;
#endif//NSIS_SUPPORT_CREATESHORTCUT
#ifdef NSIS_SUPPORT_COPYFILES
    case EW_COPYFILES: // CopyFile (added by NOP)
      {
        int res;
		    SHFILEOPSTRUCT op;
        process_string_fromtab(buf,parms[0]);
        process_string_fromtab(buf2,parms[1]);
			  log_printf3("CopyFiles \"%s\"->\"%s\"",buf,buf2);
			  op.hwnd=g_hwnd;
			  op.wFunc=FO_COPY;
        buf[mystrlen(buf)+1]=0;
        buf2[mystrlen(buf2)+1]=0;

        wsprintf(buf3,"%s%s",STR(LANG_COPYTO),buf2);

        op.pFrom=buf;
			  op.pTo=buf2;
        op.lpszProgressTitle=buf3;
			  op.fFlags=parms[2];
        update_status_text("",buf3);
			  res=SHFileOperation(&op);
			  if (res)
        { // some of these changes were from Edgewise (wiked_edge@yahoo.com)
          update_status_text_from_lang(LANGID_COPYFAILED,"");
          exec_errorflag++;
			  }
    	}
		return 0;
#endif//NSIS_SUPPORT_COPYFILES
#ifdef NSIS_SUPPORT_REBOOT
    case EW_REBOOT:
      exec_errorflag++;
      if (parms[0] == 0xbadf00d)
      {
        HANDLE h=LoadLibrary("advapi32.dll");
        if (h)
        {
          BOOL (*OPT)(HANDLE, DWORD,PHANDLE);
          BOOL (*LPV)(LPCTSTR,LPCTSTR,PLUID);
          BOOL (*ATP)(HANDLE,BOOL,PTOKEN_PRIVILEGES,DWORD,PTOKEN_PRIVILEGES,PDWORD);
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
    case EW_SETREBOOTFLAG: exec_rebootflag=parms[0]; return 0;
#endif//NSIS_SUPPORT_REBOOT
#ifdef NSIS_SUPPORT_INIFILES
    case EW_WRITEINI:
      {
        char *sec, *ent;
        sec=ent=0;
#ifdef NSIS_CONFIG_LOG
        mystrcpy(buf2,"<RM>");
        mystrcpy(buf3,buf2);
#endif
        process_string_fromtab(buf,parms[0]);
        if (parms[1]>=0)
        {
          process_string_fromtab(buf2,parms[1]);
          sec=buf2;
        }
        if (parms[2]>=0)
        {
          process_string_fromtab(buf3,parms[2]);
          ent=buf3;
        }
        process_string_fromtab(buf4,parms[3]);
        log_printf5("WriteINIStr: wrote [%s] %s=%s in %s",buf,buf2,buf3,buf4);
        if (!WritePrivateProfileString(buf,sec,ent,buf4)) exec_errorflag++;
      }
    return 0;
    case EW_READINISTR:
      {
        static const char *errstr="!N~";
        char *p=g_usrvars[parms[0]];
        process_string_fromtab(buf,parms[1]);
        process_string_fromtab(buf2,parms[2]);
        process_string_fromtab(buf3,parms[3]);
        GetPrivateProfileString(buf,buf2,errstr,p,NSIS_MAX_STRLEN-1,buf3);
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
        int rootkey=parms[0];
        exec_errorflag++;
        process_string_fromtab(buf4,parms[1]);
        if (parms[2] != -1)
        {
          HKEY hKey;
          if (RegOpenKeyEx((HKEY)rootkey,buf4,0,KEY_ALL_ACCESS,&hKey) == ERROR_SUCCESS)
          {
            process_string_fromtab(buf,parms[2]);
            log_printf4("DeleteRegValue: %d\\%s\\%s",rootkey,buf4,buf);
            if (RegDeleteValue(hKey,buf) == ERROR_SUCCESS) exec_errorflag--;
            RegCloseKey(hKey);
          }
        }
        else
        {
          log_printf3("DeleteRegKey: %d\\%s",rootkey,buf4);
          if (myRegDeleteKeyEx((HKEY)rootkey,buf4,parms[3]) == ERROR_SUCCESS) exec_errorflag--;
        }
      }
    return 0;
    case EW_WRITEREG: // write registry value
      {
        HKEY hKey;
        int rootkey=parms[0];
        int type=parms[4];
        exec_errorflag++;
        process_string_fromtab(buf2,parms[2]);
        process_string_fromtab(buf4,parms[1]);
        if (RegCreateKey((HKEY)rootkey,buf4,&hKey) == ERROR_SUCCESS)
        {
          if (type <= 1)
          {
            process_string_fromtab(buf3,parms[3]);
            if (RegSetValueEx(hKey,buf2,0,type==1?REG_SZ:REG_EXPAND_SZ,buf3,mystrlen(buf3)+1) == ERROR_SUCCESS) exec_errorflag--;
            log_printf5("WriteRegStr: set %d\\%s\\%s to %s",rootkey,buf4,buf2,buf3);
          }
          else if (type == 2)
          {
            DWORD l;
            l=process_string_fromtab_toint(parms[3]);
            if (RegSetValueEx(hKey,buf2,0,REG_DWORD,(unsigned char*)&l,4) == ERROR_SUCCESS) exec_errorflag--;
            log_printf5("WriteRegDWORD: set %d\\%s\\%s to %d",rootkey,buf4,buf2,l);
          }
          else if (type == 3)
          {
            int len=GetCompressedDataFromDataBlockToMemory(parms[3], buf3, NSIS_MAX_STRLEN);
            if (len >= 0)
            {
              if (RegSetValueEx(hKey,buf2,0,REG_BINARY,buf3,len) == ERROR_SUCCESS) exec_errorflag--;
            }
            log_printf5("WriteRegBin: set %d\\%s\\%s with %d bytes",rootkey,buf4,buf2,len);

          }
          RegCloseKey(hKey);
        }
        else { log_printf3("WriteReg: error creating key %d\\%s",rootkey,buf4); }
      }
    return 0;
    case EW_READREGSTR: // read registry string
      {
        HKEY hKey;
        char *p=g_usrvars[parms[0]];
        int rootkey=parms[1];
        process_string_fromtab(buf,parms[2]); // buf == subkey
        process_string_fromtab(buf2,parms[3]); // buf == key name
        p[0]=0;
 		    if (RegOpenKeyEx((HKEY)rootkey,buf,0,KEY_READ,&hKey) == ERROR_SUCCESS)
        {
			    DWORD l = NSIS_MAX_STRLEN;
			    DWORD t;

          if (RegQueryValueEx(hKey,buf2,NULL,&t,p,&l ) != ERROR_SUCCESS ||
              (t != REG_DWORD && t != REG_SZ && t != REG_EXPAND_SZ))
          {
            p[0]=0;
            exec_errorflag++;
          }
          else
          {
            if (t==REG_DWORD)
            {
              if (!parms[4]) exec_errorflag++;
              myitoa(p,*((DWORD*)p));
            }
            else if (parms[4]) exec_errorflag++;
          }
          RegCloseKey(hKey);
        }
        else exec_errorflag++;
     }
    return 0;
    case EW_REGENUM:
      {
        HKEY key;
        char *p=g_usrvars[parms[0]];
        int b=process_string_fromtab_toint(parms[3]);
        process_string_fromtab(buf2,parms[2]);
        p[0]=0;
      	if (RegOpenKeyEx((HKEY)parms[1],buf2,0,KEY_ALL_ACCESS,&key) == ERROR_SUCCESS)
        {
          DWORD d=NSIS_MAX_STRLEN-1;
          if (parms[4]) RegEnumKey(key,b,p,d);
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
        char *t=g_usrvars[parms[0]];
        if (*t) CloseHandle((HANDLE)myatoi(t));
      }
    return 0;
    case EW_FOPEN:
      {
        HANDLE h;
        char *handleout=g_usrvars[parms[3]];
        process_string_fromtab(buf,parms[0]);
        h=myOpenFile(buf,parms[1],parms[2]);
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
        char *t=g_usrvars[parms[0]];
        if (parms[2])
        {
          ((unsigned char *)buf2)[0]=process_string_fromtab_toint(parms[1])&0xff;
          l=1;
        }
        else
        {
          process_string_fromtab(buf2,parms[1]);
          l=mystrlen(buf2);
        }
        if (!*t || !WriteFile((HANDLE)myatoi(t),buf2,l,&dw,NULL))
        {
          exec_errorflag++;
        }
      }
    return 0;
    case EW_FGETS:
      {
        char *textout=g_usrvars[parms[1]];
        DWORD dw;
        int rpos=0;
        char *hptr=g_usrvars[parms[0]];
        int maxlen=process_string_fromtab_toint(parms[2]);
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
            if (parms[3])
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
        char *t=g_usrvars[parms[0]];
        if (*t)
        {
          DWORD v=SetFilePointer((HANDLE)myatoi(t),process_string_fromtab_toint(parms[1]),NULL,parms[2]);

          if (parms[3]>=0)
          {
            myitoa(g_usrvars[parms[3]],v);
          }
        }
      }
    return 0;
#endif//NSIS_SUPPORT_FILEFUNCTIONS
#ifdef NSIS_SUPPORT_FINDFIRST
    case EW_FINDCLOSE:
      {
        char *t=g_usrvars[parms[0]];
        if (*t) FindClose((HANDLE)myatoi(t));
      }
    return 0;
    case EW_FINDNEXT:
      {
        char *textout=g_usrvars[parms[0]];
        char *t=g_usrvars[parms[1]];
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
        char *textout=g_usrvars[parms[1]];
        char *handleout=g_usrvars[parms[2]];
        HANDLE h;
        WIN32_FIND_DATA fd;
        process_string_fromtab(buf,parms[0]);
        h=FindFirstFile(buf,&fd);
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
        process_string_fromtab(buf,parms[0]);

        if (validpathspec(buf))
        {
          mystrcpy(buf2,buf);
        }
        else
        {
          addtrailingslash(mystrcpy(buf2,state_install_directory));
          lstrcat(buf2,buf);
        }


        hFile=myOpenFile(buf2,GENERIC_WRITE,CREATE_ALWAYS);
        if (hFile != INVALID_HANDLE_VALUE)
        {
          unsigned char *filebuf;
          DWORD l;
          filebuf=(unsigned char *)GlobalAlloc(GMEM_FIXED,g_filehdrsize);
          if (filebuf)
          {
            int fixoffs=0;
            SetFilePointer(g_db_hFile,0,NULL,FILE_BEGIN);
            ReadFile(g_db_hFile,(char*)filebuf,g_filehdrsize,&l,NULL);
            if (g_inst_header->uninstdata_offset != -1)
            {
              // Changed by Amir Szekely 11th July 2002
              unsigned char* unicon_data = (unsigned char*)GlobalAlloc(GMEM_FIXED, g_inst_header->uninsticon_size);
              if (unicon_data) {
                DWORD i;
                unsigned char* seeker = unicon_data + sizeof(DWORD);
                GetCompressedDataFromDataBlockToMemory(g_inst_header->uninstdata_offset,
                  unicon_data,g_inst_header->uninsticon_size);
                for (i = 0; i < *(DWORD*)unicon_data; i++) {
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
            WriteFile(hFile,(char*)filebuf,g_filehdrsize,&l,NULL);
            GlobalFree(filebuf);
            ret=GetCompressedDataFromDataBlock(-1,hFile);
          }
          CloseHandle(hFile);
        }
        log_printf3("created uninstaller: %d, \"%s\"",ret,buf2);
        if (ret < 0)
        {
          update_status_text_from_lang(LANGID_ERRORCREATING,buf);
          DeleteFile(buf2);
        }
        else
          update_status_text_from_lang(LANGID_CREATEDUNINST,buf);
      }
    return 0;
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
#ifdef NSIS_CONFIG_LOG
    case EW_LOG:
      if (parms[0])
      {
        log_printf2("settings logging to %d",parms[1]);
        log_dolog=parms[1];
        log_printf2("logging set to %d",parms[1]);
        if (!g_log_file && log_dolg) build_g_logfile();
      }
      else
      {
        process_string_fromtab(buf,parms[1]);
        log_printf2("%s",buf);
      }
    return 0;
#endif//NSIS_CONFIG_LOG
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case EW_SECTIONSET:
      {
        int x=process_string_fromtab_toint(parms[0]);
        if (g_inst_section && x >= 0 && x < g_inst_header->num_sections)
        {
          int z=0;
          if (g_SectionHack)
          {
            int a;
            for (a = 0; a < x; a ++) if (g_inst_section[a].name_ptr>=0) z++;
          }

          if (parms[1]==0) //set text
          {
            if (g_SectionHack)
            {
              SendMessage(g_SectionHack,WM_USER+0x17,x,parms[2]);
            }
            g_inst_section[x].name_ptr=parms[2];
          }
          else if (parms[1]==1) // get text
          {
            process_string_fromtab(g_usrvars[parms[2]],g_inst_section[x].name_ptr);
          }
          else if (parms[1]==2) // set flags
          {
            g_inst_section[x].default_state=process_string_fromtab_toint(parms[2]);
            if (g_SectionHack)
            {
              SendMessage(g_SectionHack,WM_USER+0x18,x,(LPARAM)!!(g_inst_section[x].default_state&DFS_SET));
            }
          }
          else // get flags
          {
            myitoa(g_usrvars[parms[2]],g_inst_section[x].default_state);
          }
        }
        else exec_errorflag++;
      }
    return 0;
#endif//NSIS_CONFIG_COMPONENTPAGE
// Added by Amir Szekely 21st 2002
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case EW_SETBRANDINGIMAGE:
    {
      RECT r;
      HWND hwImage = GetDlgItem(g_hwnd, parms[1]);
      GetWindowRect(hwImage, &r);
      process_string_fromtab(buf, parms[0]);
      if (g_hBrandingBitmap) DeleteObject(g_hBrandingBitmap);
      g_hBrandingBitmap=LoadImage(
        0,
        buf,
        IMAGE_BITMAP,
        parms[2]?r.right-r.left:0,
        parms[2]?r.bottom-r.top:0,
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
#endif //NSIS_CONFIG_VISIBLE_SUPPORT
// Added by Ximon Eighteen 5th August 2002
#ifdef NSIS_CONFIG_PLUGIN_SUPPORT
    case EW_PLUGINCOMMANDPREP:
      // parms[0] = dll name

      if (!*plugins_temp_dir) mystrcpy(plugins_temp_dir,g_usrvars[0]);
    return 0;
#endif // NSIS_CONFIG_PLUGIN_SUPPORT
  }
  my_MessageBox(STR(LANG_INSTCORRUPTED),MB_OK|MB_ICONSTOP);
  return EXEC_ERROR;
}
