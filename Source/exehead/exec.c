#include "../Platform.h"
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

exec_flags g_exec_flags;

#if defined(NSIS_SUPPORT_ACTIVEXREG) || defined(NSIS_SUPPORT_CREATESHORTCUT)
HRESULT g_hres;
#endif

#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
// based loosely on code from Tim Kosse
// in win9x this isn't necessary (RegDeleteKey() can delete a tree of keys),
// but in win2k you need to do this manually.
static LONG NSISCALL myRegDeleteKeyEx(HKEY thiskey, LPCTSTR lpSubKey, int onlyifempty)
{
  HKEY key;
  int retval=RegOpenKeyEx(thiskey,lpSubKey,0,KEY_ENUMERATE_SUB_KEYS,&key);
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

static int NSISCALL ExecuteEntry(entry *entry_);

#define resolveaddr(v) ((v<0) ? myatoi(g_usrvars[-(v+1)]) : v)

int NSISCALL ExecuteCodeSegment(int pos, HWND hwndProgress)
{
  while (pos >= 0)
  {
    int rv;
    if (g_entries[pos].which == EW_RET) return 0;
    rv=ExecuteEntry(g_entries + pos);
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

void NSISCALL update_status_text_buf1(int strtab)
{
  update_status_text(strtab, bufs[1]);
}

static int NSISCALL GetIntFromParm(int id_)
{
  return myatoi(GetNSISStringTT(parms[id_]));
}

// NB - USE CAUTION when rearranging code to make use of the new return value of
// this function - be sure the parm being accessed is not modified before the call.
// Use a negative number to get the string validated as a file name
static char * NSISCALL GetStringFromParm(int id_)
{
  int id = id_ < 0 ? -id_ : id_;
  char *result = GetNSISString(bufs[id >> 4], parms[id & 0xF]);
  if (id_ < 0) validate_filename(result);
  return result;
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
  //char *buf4 = bufs[4];

  char *var0;
  char *var1;
  //char *var2;
  //char *var3;
  //char *var4;
  //char *var5;

#ifdef NSIS_CONFIG_COMPONENTPAGE
  HWND hwSectionHack = g_SectionHack;
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  // Saves 8 bytes
  HWND mainHwnd = g_hwnd;
#define g_hwnd mainHwnd
#endif

  int exec_error = 0;

  entry lent = *entry_;

#define which (lent.which)
#define parm0 (lent.offsets[0])
#define parm1 (lent.offsets[1])
#define parm2 (lent.offsets[2])
#define parm3 (lent.offsets[3])
#define parm4 (lent.offsets[4])
#define parm5 (lent.offsets[5])

  var0 = g_usrvars[parm0];
  var1 = g_usrvars[parm1];
  // not used yet
  //var2 = g_usrvars[parm2];
  //var3 = g_usrvars[parm3];
  //var4 = g_usrvars[parm4];
  //var5 = g_usrvars[parm5];

  parms = lent.offsets;

  switch (which)
  {
    case EW_NOP:
      log_printf2("Jump: %d",parm0);
    return parm0;
    case EW_ABORT:
      {
        log_printf2("Aborting: \"%s\"",GetStringFromParm(0x00));
        update_status_text(parm0,0);
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
        static int old_st_updateflag=6;
        if (parm1&8) ui_st_updateflag=old_st_updateflag;
        else {
          old_st_updateflag=ui_st_updateflag;
          ui_st_updateflag=parm1;
        }
      }
      else
      {
        log_printf2("detailprint: %s",GetStringFromParm(0x00));
        update_status_text(parm0,0);
      }
    break;
    case EW_SLEEP:
      {
        int x=GetIntFromParm(0);
        log_printf2("Sleep(%d)",x);
        Sleep(max(x,1));
      }
    break;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case EW_BRINGTOFRONT:
      log_printf("BringToFront");
      SetForegroundWindow(g_hwnd);
    break;
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
    case EW_SETFLAG:
      FIELDN(g_exec_flags,parm0)=GetIntFromParm(1);
    break;
    case EW_IFFLAG:
    {
      int f=lent.offsets[!FIELDN(g_exec_flags,parm2)];
      FIELDN(g_exec_flags,parm2)&=parm3;
      return f;
    }
    case EW_GETFLAG:
      myitoa(var0,FIELDN(g_exec_flags,parm1));
    break;
#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
    case EW_CHDETAILSVIEW:
      if (insthwndbutton) ShowWindow(insthwndbutton,parm1);
      if (insthwnd) ShowWindow(insthwnd,parm0);
    break;
#endif//NSIS_CONFIG_VISIBLE_SUPPORT
    case EW_SETFILEATTRIBUTES:
    {
      char *buf1=GetStringFromParm(-0x10);
      log_printf3("SetFileAttributes: \"%s\":%08X",buf1,parm1);
      if (!SetFileAttributes(buf1,parm1))
      {
        exec_error++;
        log_printf("SetFileAttributes failed.");
      }
    }
    break;
    case EW_CREATEDIR: {
      char *buf1=GetStringFromParm(-0x10);
      log_printf3("CreateDirectory: \"%s\" (%d)",buf1,parm1);
      {
        char *p = skip_root(buf1);
        char c = 'c';
        if (*buf1 && p)
        {
          while (c)
          {
            WIN32_FIND_DATA *fd;
            p = findchar(p, '\\');
            c=*p;
            *p=0;
            fd = file_exists(buf1);
            if (!fd) {
              if (!CreateDirectory(buf1,NULL))
                exec_error++;
            }
            else if ((fd->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0) exec_error++;
            *p++ = c;
          }
        }
      }
      if (parm1)
      {
        update_status_text_buf1(LANG_OUTPUTDIR);
        mystrcpy(state_output_directory,buf1);
        SetCurrentDirectory(buf1);
      }
      else update_status_text_buf1(LANG_CREATEDIR);
    }
    break;
    case EW_IFFILEEXISTS:
    {
      char *buf0=GetStringFromParm(0x00);
      if (file_exists(buf0))
      {
        log_printf3("IfFileExists: file \"%s\" exists, jumping %d",buf0,parm1);
        return parm1;
      }
      log_printf3("IfFileExists: file \"%s\" does not exist, jumping %d",buf0,parm2);
    }
    return parm2;
#ifdef NSIS_SUPPORT_RENAME
    case EW_RENAME:
      {
        char *buf3=GetStringFromParm(-0x30);
        char *buf2=GetStringFromParm(-0x21);
        mystrcpy(buf1,buf3);
        if (mystrlen(buf3)+mystrlen(buf2) < NSIS_MAX_STRLEN-3)
        {
          lstrcat(buf1,"->");
          lstrcat(buf1,buf2);
        }
        log_printf2("Rename: %s",buf1);
        if (MoveFile(buf3,buf2))
        {
          update_status_text_buf1(LANG_RENAME);
        }
        else
        {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
          if (parm2 && file_exists(buf3))
          {
            MoveFileOnReboot(buf3,buf2);
            update_status_text_buf1(LANG_RENAMEONREBOOT);
            log_printf2("Rename on reboot: %s",buf1);
          }
          else
#endif
          {
            exec_error++;
            log_printf2("Rename failed: %s",buf1);
          }
        }
      }
    break;
#endif//NSIS_SUPPORT_RENAME
#ifdef NSIS_SUPPORT_FNUTIL
    case EW_GETFULLPATHNAME:
      {
        char *fp;
        char *p=var1;
        char *buf0=GetStringFromParm(0x00);
        if (!GetFullPathName(buf0,NSIS_MAX_STRLEN,p,&fp))
        {
          exec_error++;
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
            exec_error++;
            *p=0;
          }
        }
        if (!parm2) GetShortPathName(p,p,NSIS_MAX_STRLEN);
      }
    break;
    case EW_SEARCHPATH:
      {
        char *fp;
        char *p=var0;
        char *buf0=GetStringFromParm(-0x01);
        if (!SearchPath(NULL,buf0,NULL,NSIS_MAX_STRLEN,p,&fp))
        {
          exec_error++;
          p[0]=0;
        }
      }
    break;
    case EW_GETTEMPFILENAME:
      {
        char *textout=var0;
        if (!my_GetTempFileName(textout, GetStringFromParm(-0x11)))
          exec_error++;
      }
    break;
#endif
#ifdef NSIS_SUPPORT_FILE
    case EW_EXTRACTFILE:
      {
        HANDLE hOut;
        int ret;
        char *buf3 = GetStringFromParm(0x31);
        int overwriteflag = parm0 & 7;

        log_printf4("File: overwriteflag=%d, allowskipfilesflag=%d, name=\"%s\"",overwriteflag,(parm0>>3)&MB_ABORTRETRYIGNORE,buf3);
        if (validpathspec(buf3))
        {
          mystrcpy(buf0,buf3);
        }
        else lstrcat(addtrailingslash(mystrcpy(buf0,state_output_directory)),buf3);
        validate_filename(buf0);
      _tryagain:
        if (overwriteflag >= 3) // check date and time
        {
          WIN32_FIND_DATA *ffd=file_exists(buf0);
          // if it doesn't exist, overwrite flag will be off (though it doesn't really matter)
          int cmp=0;
          if (ffd)
          {
            cmp=CompareFileTime(&ffd->ftLastWriteTime, (FILETIME*)(lent.offsets + 3));
          }
          overwriteflag=!(cmp & (0x80000000 | (overwriteflag - 3)));
        }
        // remove read only flag if overwrite mode is on
        if (!overwriteflag)
        {
          int attr=GetFileAttributes(buf0);
          SetFileAttributes(buf0,attr&(~FILE_ATTRIBUTE_READONLY));
        }
        hOut=myOpenFile(buf0,GENERIC_WRITE,(overwriteflag==1)?CREATE_NEW:CREATE_ALWAYS);
        if (hOut == INVALID_HANDLE_VALUE)
        {
          if (overwriteflag)
          {
            update_status_text(LANG_SKIPPED,buf3);
            if (overwriteflag==2) g_exec_flags.exec_error++;
            log_printf3("File: skipped: \"%s\" (overwriteflag=%d)",buf0,overwriteflag);
            break;
          }
          log_printf2("File: error creating \"%s\"",buf0);
          mystrcpy(buf2,g_usrvars[0]); //save $0
          mystrcpy(g_usrvars[0],buf0);

          GetNSISString(buf1,parm5);
          mystrcpy(g_usrvars[0],buf2); // restore $0

          // Modified by ramon 23 May 2003
          switch (my_MessageBox(buf1, parm0>>3))
          {
            case IDRETRY:
              log_printf("File: error, user retry");
              goto _tryagain;
            case IDIGNORE:
              log_printf("File: error, user cancel");
              g_exec_flags.exec_error++;
              return 0;
            default:
              log_printf("File: error, user abort");
              update_status_text(LANG_CANTWRITE,buf0);
            return EXEC_ERROR;
          }
        }

        update_status_text(LANG_EXTRACT,buf3);
        {
          ui_st_updateflag++;
          ret=GetCompressedDataFromDataBlock(parm2,hOut);
          ui_st_updateflag--;
        }

        log_printf3("File: wrote %d to \"%s\"",ret,buf0);

        if (parm3 != 0xffffffff || parm4 != 0xffffffff)
          SetFileTime(hOut,(FILETIME*)(lent.offsets+3),NULL,(FILETIME*)(lent.offsets+3));

        CloseHandle(hOut);

        if (ret < 0)
        {
          if (ret == -2)
          {
            GetNSISString(buf0,LANG_ERRORWRITING);
            lstrcat(buf0,buf3);
          }
          else
          {
            GetNSISString(buf0,LANG_ERRORDECOMPRESSING);
          }
          log_printf2("%s",buf0);
          my_MessageBox(buf0,MB_OK|MB_ICONSTOP|(IDOK<<20));
          return EXEC_ERROR;
        }
      }
    break;
#endif//NSIS_SUPPORT_FILE
#ifdef NSIS_SUPPORT_DELETE
    case EW_DELETEFILE:
      {
        HANDLE h;
        WIN32_FIND_DATA fd;
        char *buf1=GetStringFromParm(0x10);
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
                update_status_text_buf1(LANG_DELETEFILE);
              }
              else
              {
#ifdef NSIS_SUPPORT_MOVEONREBOOT
                if (parm1)
                {
                  log_printf2("Delete: DeleteFile on Reboot(\"%s\")",buf1);
                  update_status_text_buf1(LANG_DELETEONREBOOT);
                  MoveFileOnReboot(buf1,NULL);
                }
                else
#endif
                {
                  exec_error++;
                }
              }
            }
          } while (FindNextFile(h,&fd));
          FindClose(h);
        }
      }
    break;
#endif//NSIS_SUPPORT_DELETE
#ifdef NSIS_SUPPORT_MESSAGEBOX
    case EW_MESSAGEBOX: // MessageBox
      {
        int v;
        char *buf3=GetStringFromParm(0x31);
        log_printf3("MessageBox: %d,\"%s\"",parm0,buf3);
        v=my_MessageBox(buf3,parm0);
        if (v)
        {
          if (v==parm2)
          {
            return parm3;
          }
          if (v==parm4)
          {
            return parm5;
          }
        }
        else exec_error++;
      }
    break;
#endif//NSIS_SUPPORT_MESSAGEBOX
#ifdef NSIS_SUPPORT_RMDIR
    case EW_RMDIR:
      {
        char *buf1=GetStringFromParm(-0x10);
        log_printf2("RMDir: \"%s\"",buf1);

        doRMDir(buf1,parm1);
        if (file_exists(buf1) && parm1!=2) exec_error++;
        else update_status_text_buf1(LANG_REMOVEDIR);
      }
    break;
#endif//NSIS_SUPPORT_RMDIR
#ifdef NSIS_SUPPORT_STROPTS
    case EW_STRLEN:
    {
      char *buf0=GetStringFromParm(0x01);
      myitoa(var0,mystrlen(buf0));
    }
    break;
    case EW_ASSIGNVAR:
      {
        int newlen=GetIntFromParm(2);
        int start=GetIntFromParm(3);
        int l;
        char *p=var0;
        char *buf0=GetStringFromParm(0x01);
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
    break;
    case EW_STRCMP: {
      char *buf2=GetStringFromParm(0x20);
      char *buf3=GetStringFromParm(0x31);
      if (!lstrcmpi(buf2,buf3)) return parm2;
    }
    return parm3;
#endif//NSIS_SUPPORT_STROPTS
#ifdef NSIS_SUPPORT_ENVIRONMENT
    case EW_READENVSTR:
      {
        char *p=var0;
        char *buf0=GetStringFromParm(0x01);
        if (parm2)
        {
          if (!GetEnvironmentVariable(buf0,p,NSIS_MAX_STRLEN))
          {
            exec_error++;
            *p=0;
          }
        }
        else
        {
          ExpandEnvironmentStrings(buf0,p,NSIS_MAX_STRLEN);
        }
        p[NSIS_MAX_STRLEN-1]=0;
      }
    break;
#endif//NSIS_SUPPORT_ENVIRONMENT
#ifdef NSIS_SUPPORT_INTOPTS
    case EW_INTCMP:
      {
        int v,v2;
        v=GetIntFromParm(0);
        v2=GetIntFromParm(1);
        if (!parm5) {
          // signed
          if (v<v2) return parm3;
          if (v>v2) return parm4;
        }
        else {
          // unsigned
          if ((unsigned int)v<(unsigned int)v2) return parm3;
          if ((unsigned int)v>(unsigned int)v2) return parm4;
        }
      }
    return parm2;
    case EW_INTOP:
      {
        int v,v2;
        char *p=var0;
        v=GetIntFromParm(1);
        v2=GetIntFromParm(2);
        switch (parm3)
        {
          case 0: v+=v2; break;
          case 1: v-=v2; break;
          case 2: v*=v2; break;
          case 3: if (v2) v/=v2; else { v=0; exec_error++; } break;
          case 4: v|=v2; break;
          case 5: v&=v2; break;
          case 6: v^=v2; break;
          case 7: v=!v; break;
          case 8: v=v||v2; break;
          case 9: v=v&&v2; break;
          case 10: if (v2) v%=v2; else { v=0; exec_error++; } break;
        }
        myitoa(p,v);
      }
    break;
    case EW_INTFMT: {
      char *buf0=GetStringFromParm(0x01);
      wsprintf(var0,
               buf0,
               GetIntFromParm(2));
    }
    break;
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
            my_MessageBox(GetNSISStringTT(LANG_INSTCORRUPTED),MB_OK|MB_ICONSTOP|(IDOK<<20));
            return EXEC_ERROR;
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
            exec_error++;
            break;
          }
          mystrcpy(var0,s->text);
          g_st=s->next;
          GlobalFree((HGLOBAL)s);
        }
        else
        {
          s=(stack_t*)my_GlobalAlloc(sizeof(stack_t));
          GetNSISString(s->text,parm0);
          s->next=g_st;
          g_st=s;
        }
      }
    break;
#endif//NSIS_SUPPORT_STACK
#ifdef NSIS_SUPPORT_HWNDS
    case EW_FINDWINDOW:
    case EW_SENDMESSAGE:
      {
        int v;
        int b3=(int)GetStringFromParm(0x33);
        int b4=(int)GetStringFromParm(0x44);
        if (!(parm5&1)) b3=myatoi((char*)b3);
        if (!(parm5&2)) b4=myatoi((char*)b4);

        if (which == EW_SENDMESSAGE)
        {
          HWND hwnd=(HWND)GetIntFromParm(1);
          int msg=GetIntFromParm(2);

          if (parm5>>2) exec_error += !SendMessageTimeout(hwnd,msg,b3,b4,SMTO_NORMAL,parm5>>2,(LPDWORD)&v);
          else v=SendMessage(hwnd,msg,b3,b4);
        }
        else
        {
          char *buf0=GetStringFromParm(0x01);
          char *buf1=GetStringFromParm(0x12);
          v=(int)FindWindowEx((HWND)b3,(HWND)b4,buf0[0]?buf0:NULL,buf1[0]?buf1:NULL);
        }

        if (parm0>=0)
          myitoa(var0,v);
      }
    break;
    case EW_ISWINDOW:
        if (IsWindow((HWND)GetIntFromParm(0))) return parm1;
    return parm2;
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
    case EW_GETDLGITEM:
      myitoa(
        var0,
        (int)GetDlgItem(
          (HWND)GetIntFromParm(1),
          GetIntFromParm(2)
        )
      );
    break;
    case EW_SETCTLCOLORS:
    {
      ctlcolors *c = (ctlcolors *)(g_blocks[NB_CTLCOLORS].offset + parm1);
      SetWindowLong((HWND) GetIntFromParm(0), GWL_USERDATA, (long) c);
    }
    break;
    case EW_SETBRANDINGIMAGE:
    {
      RECT r;
      HANDLE hImage;
      HWND hwImage=GetDlgItem(g_hwnd, parm1);
      GetClientRect(hwImage, &r);
      hImage=LoadImage(
        0,
        GetStringFromParm(0x00),
        IMAGE_BITMAP,
        parm2*r.right,
        parm2*r.bottom,
        LR_LOADFROMFILE
      );
      hImage = (HANDLE)SendMessage(
        hwImage,
        STM_SETIMAGE,
        IMAGE_BITMAP,
        (LPARAM)hImage
      );
      // delete old image
      if (hImage) DeleteObject(hImage);
    }
    break;
    case EW_CREATEFONT:
    {
      static LOGFONT f;
      f.lfHeight=-MulDiv(GetIntFromParm(2),GetDeviceCaps(GetDC(g_hwnd),LOGPIXELSY),72);
      f.lfWeight=GetIntFromParm(3);
      f.lfItalic=parm4&1;
      f.lfUnderline=parm4&2;
      f.lfStrikeOut=parm4&4;
      f.lfCharSet=DEFAULT_CHARSET;
      GetNSISString(f.lfFaceName,parm1);
      myitoa(var0,(int)CreateFontIndirect(&f));
    }
    break;
    case EW_SHOWWINDOW:
      if (parm2) log_printf("HideWindow");
      if (!parm3)
        ShowWindow((HWND)GetIntFromParm(0),GetIntFromParm(1));
      else
        EnableWindow((HWND)GetIntFromParm(0),GetIntFromParm(1));
    break;
#endif//NSIS_CONFIG_ENHANCEDUI_SUPPORT
#endif//NSIS_SUPPORT_HWNDS
#ifdef NSIS_SUPPORT_SHELLEXECUTE
    case EW_SHELLEXEC: // this uses improvements of Andras Varga
      {
        int x;
        char *buf0=GetStringFromParm(0x00);
        char *buf3=GetStringFromParm(0x31);
        char *buf2=GetStringFromParm(0x22);
        wsprintf(buf1,"%s %s",buf0,buf3);
        update_status_text_buf1(LANG_EXECSHELL);
        x=(int)ShellExecute(g_hwnd,buf0[0]?buf0:NULL,buf3,buf2[0]?buf2:NULL,state_output_directory,parm3);
        if (x < 33)
        {
          log_printf5("ExecShell: warning: error (\"%s\": file:\"%s\" params:\"%s\")=%d",buf0,buf3,buf2,x);
          exec_error++;
        }
        else
        {
          log_printf4("ExecShell: success (\"%s\": file:\"%s\" params:\"%s\")",buf0,buf3,buf2);
        }
      }
    break;
#endif//NSIS_SUPPORT_SHELLEXECUTE
#ifdef NSIS_SUPPORT_EXECUTE
    case EW_EXECUTE:
      {
        HANDLE hProc;
        char *buf0=GetStringFromParm(0x00);
        log_printf2("Exec: command=\"%s\"",buf0);
        update_status_text(LANG_EXECUTE,buf0);

        hProc=myCreateProcess(buf0,state_output_directory);

        if (hProc)
        {
          log_printf2("Exec: success (\"%s\")",buf0);
          if (parm2)
          {
            DWORD lExitCode;
            while (WaitForSingleObject(hProc,100) == WAIT_TIMEOUT)
            {
              MSG msg;
              while (PeekMessage(&msg,NULL,WM_PAINT,WM_PAINT,PM_REMOVE))
                DispatchMessage(&msg);
            }
            GetExitCodeProcess(hProc, &lExitCode);

            if (parm1>=0) myitoa(var1,lExitCode);
            else if (lExitCode) exec_error++;
          }
          CloseHandle( hProc );
        }
        else
        {
          exec_error++;
          log_printf2("Exec: failed createprocess (\"%s\")",buf0);
        }
      }
    break;
#endif//NSIS_SUPPORT_EXECUTE
#ifdef NSIS_SUPPORT_GETFILETIME
    case EW_GETFILETIME:
      // this new implementation based on one by Dave Bau
      // used FindFirstFile instead of GetFileTime to better handle files that are locked.
      // also allows GetFileTime to be passed a wildcard.
      {
        WIN32_FIND_DATA *ffd;
        char *highout=var0;
        char *lowout=var1;
        char *buf0=GetStringFromParm(0x02);

        ffd=file_exists(buf0);
        if (ffd)
        {
          myitoa(lowout,ffd->ftLastWriteTime.dwLowDateTime);
          myitoa(highout,ffd->ftLastWriteTime.dwHighDateTime);
        }
        else
        {
          *lowout=*highout=0;
          exec_error++;
        }
      }
    break;
#endif//NSIS_SUPPORT_GETFILETIME
#ifdef NSIS_SUPPORT_GETDLLVERSION
    case EW_GETDLLVERSION:
      {
        char *highout=var0;
        char *lowout=var1;
        DWORD s1;
        DWORD t[4]; // our two members are the 3rd and 4th..
        VS_FIXEDFILEINFO *pvsf1=(VS_FIXEDFILEINFO*)t;
        DWORD d;
        char *buf1=GetStringFromParm(-0x12);
        s1=GetFileVersionInfoSize(buf1,&d);
        *lowout=*highout=0;
        exec_error++;
        if (s1)
        {
          void *b1;
          b1=my_GlobalAlloc(s1);
          if (b1)
          {
            UINT uLen;
            if (GetFileVersionInfo(buf1,0,s1,b1) && VerQueryValue(b1,"\\",(void*)&pvsf1,&uLen))
            {
              myitoa(highout,pvsf1->dwFileVersionMS);
              myitoa(lowout,pvsf1->dwFileVersionLS);

              exec_error--;
            }
            GlobalFree(b1);
          }
        }
      }
      break;
#endif//NSIS_SUPPORT_GETDLLVERSION
#ifdef NSIS_SUPPORT_ACTIVEXREG
    case EW_REGISTERDLL:
      {
        exec_error++;
        SetErrorMode(SEM_NOOPENFILEERRORBOX | SEM_FAILCRITICALERRORS);
        if (SUCCEEDED(g_hres))
        {
          HANDLE h;
          char *buf1=GetStringFromParm(-0x10);
          char *buf0=GetStringFromParm(0x01);

          h=GetModuleHandle(buf1);
          if (!h)
            h=LoadLibrary(buf1);
          if (h)
          {
            FARPROC funke = GetProcAddress(h,buf0);
            if (funke)
            {
              exec_error--;
              if (parm2)
              {
                update_status_text_buf1(parm2);
                if (funke()) exec_error++;
              }
              else
              {
                void (*func)(HWND,int,char*,void*);
                func=(void*)funke;
                func(
                  g_hwnd,
                  NSIS_MAX_STRLEN,
                  (char*)g_usrvars,
#ifdef NSIS_SUPPORT_STACK
                  (void*)&g_st
#else
                  NULL
#endif//NSIS_SUPPORT_STACK
                );
              }
            }
            else
            {
              update_status_text(LANG_CANNOTFINDSYMBOL,buf0);
              log_printf3("Error registering DLL: %s not found in %s",buf0,buf1);
            }
            if (!parm3) FreeLibrary(h);
          }
          else
          {
            update_status_text_buf1(LANG_COULDNOTLOAD);
            log_printf2("Error registering DLL: Could not load %s",buf1);
          }
        }
        else
        {
          update_status_text_buf1(LANG_NOOLE);
          log_printf("Error registering DLL: Could not initialize OLE");
        }
        SetErrorMode(0);
      }
    break;
#endif
#ifdef NSIS_SUPPORT_CREATESHORTCUT
    case EW_CREATESHORTCUT:
    {
      char *buf1=GetStringFromParm(-0x10);
      char *buf2=GetStringFromParm(-0x21);
      char *buf0=GetStringFromParm(0x02);
      char *buf3=GetStringFromParm(-0x33);
      char *buf4=GetStringFromParm(0x45);

      HRESULT hres;
      IShellLink* psl;

      if (!validpathspec(buf2))
        GetStringFromParm(0x11);

      log_printf8("CreateShortCut: out: \"%s\", in: \"%s %s\", icon: %s,%d, sw=%d, hk=%d",
        buf1,buf2,buf0,buf3,parm4&0xff,(parm4&0xff00)>>8,parm4>>16);

      hres = CoCreateInstance(&CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
                                &IID_IShellLink, (void **) &psl);
      if (SUCCEEDED(hres))
      {
        IPersistFile* ppf;

        hres = psl->lpVtbl->QueryInterface(psl,&IID_IPersistFile, (void **) &ppf);
        if (SUCCEEDED(hres))
        {
          hres = psl->lpVtbl->SetPath(psl,buf2);
          psl->lpVtbl->SetWorkingDirectory(psl,state_output_directory);
          if ((parm4&0xff00)>>8) psl->lpVtbl->SetShowCmd(psl,(parm4&0xff00)>>8);
          psl->lpVtbl->SetHotkey(psl,(unsigned short)(parm4>>16));
          if (buf3[0]) psl->lpVtbl->SetIconLocation(psl,buf3,parm4&0xff);
          psl->lpVtbl->SetArguments(psl,buf0);
          psl->lpVtbl->SetDescription(psl,buf4);

          if (SUCCEEDED(hres))
          {
             static WCHAR wsz[1024];
             wsz[0]=0;
             MultiByteToWideChar(CP_ACP, 0, buf1, -1, wsz, 1024);
             hres=ppf->lpVtbl->Save(ppf,(const WCHAR*)wsz,TRUE);
          }
          ppf->lpVtbl->Release(ppf);
        }
        psl->lpVtbl->Release(psl);
      }

      if (FAILED(hres))
      {
        exec_error++;
        update_status_text_buf1(LANG_ERRORCREATINGSHORTCUT);
      }
      else
      {
        update_status_text_buf1(LANG_CREATESHORTCUT);
      }
    }
    break;
#endif//NSIS_SUPPORT_CREATESHORTCUT
#ifdef NSIS_SUPPORT_COPYFILES
    case EW_COPYFILES: // CopyFile (added by NOP)
      {
        int res;
        SHFILEOPSTRUCT op;
        char *buf0=GetStringFromParm(0x00);
        char *buf1=GetStringFromParm(0x11);
        log_printf3("CopyFiles \"%s\"->\"%s\"",buf0,buf1);
        op.hwnd=g_hwnd;
        op.wFunc=FO_COPY;
        buf0[mystrlen(buf0)+1]=0;
        buf1[mystrlen(buf1)+1]=0;

        GetNSISString(buf2,LANG_COPYTO);
        lstrcat(buf2,buf1);

        op.pFrom=buf0;
        op.pTo=buf1;
        op.lpszProgressTitle=buf2;
        op.fFlags=parm2;
        update_status_text(0,buf2);
        res=SHFileOperation(&op);
        if (res)
        { // some of these changes were from Edgewise (wiked_edge@yahoo.com)
          update_status_text(LANG_COPYFAILED,0);
          exec_error++;
        }
      }
    break;
#endif//NSIS_SUPPORT_COPYFILES
#ifdef NSIS_SUPPORT_REBOOT
    case EW_REBOOT:
      if (parm0!=0xbadf00d)
      {
        my_MessageBox(GetNSISStringTT(LANG_INSTCORRUPTED),MB_OK|MB_ICONSTOP|(IDOK<<20));
        return EXEC_ERROR;
      }
      g_exec_flags.exec_error++;
      {
        HANDLE h=LoadLibrary("ADVAPI32.dll");
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
          FreeLibrary(h);
        }

        if (ExitWindowsEx(EWX_REBOOT,0))
        {
          g_quit_flag++;
          PostQuitMessage(0);
          return EXEC_ERROR;
        }
      }
    break;
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
          sec=GetStringFromParm(0x00);
        }
        if (parm1)
        {
          key=GetStringFromParm(0x11);
        }
        if (parm4)
        {
          str=GetStringFromParm(0x22);
        }
        buf3=GetStringFromParm(-0x33);
        log_printf5("WriteINIStr: wrote [%s] %s=%s in %s",buf0,buf1,buf2,buf3);
        if (!WritePrivateProfileString(sec,key,str,buf3)) exec_error++;
      }
    break;
    case EW_READINISTR:
      {
        DWORD errstr = CHAR4_TO_DWORD('!', 'N', '~', 0);
        char *p=var0;
        char *buf0=GetStringFromParm(0x01);
        char *buf1=GetStringFromParm(0x12);
        char *buf2=GetStringFromParm(-0x23);
        GetPrivateProfileString(buf0,buf1,(LPCSTR)&errstr,p,NSIS_MAX_STRLEN-1,buf2);
        if (*(DWORD*)p == errstr)
        {
          exec_error++;
          p[0]=0;
        }
      }
    break;
#endif//NSIS_SUPPORT_INIFILES
#ifdef NSIS_SUPPORT_REGISTRYFUNCTIONS
    case EW_DELREG:
      {
        int rootkey=parm0;
        char *buf3=GetStringFromParm(0x31);
        exec_error++;
        if (!parm3)
        {
          HKEY hKey;
          if (RegOpenKeyEx((HKEY)rootkey,buf3,0,KEY_SET_VALUE,&hKey) == ERROR_SUCCESS)
          {
            char *buf0=GetStringFromParm(0x02);
            log_printf4("DeleteRegValue: %d\\%s\\%s",rootkey,buf3,buf0);
            if (RegDeleteValue(hKey,buf0) == ERROR_SUCCESS) exec_error--;
            RegCloseKey(hKey);
          }
        }
        else
        {
          log_printf3("DeleteRegKey: %d\\%s",rootkey,buf3);
          if (myRegDeleteKeyEx((HKEY)rootkey,buf3,parm3&2) == ERROR_SUCCESS) exec_error--;
        }
      }
    break;
    case EW_WRITEREG: // write registry value
      {
        HKEY hKey;
        int rootkey=parm0;
        int type=parm4;
        int rtype=parm5;
        char *buf1=GetStringFromParm(0x12);
        char *buf3=GetStringFromParm(0x31);
        exec_error++;
        if (RegCreateKey((HKEY)rootkey,buf3,&hKey) == ERROR_SUCCESS)
        {
          LPBYTE data = (LPBYTE) buf2;
          DWORD size = 0;
          if (type == REG_SZ)
          {
            GetStringFromParm(0x23);
            size = mystrlen((char *) data) + 1;
            log_printf5("WriteRegStr: set %d\\%s\\%s to %s",rootkey,buf3,buf1,buf2);
          }
          if (type == REG_DWORD)
          {
            *(LPDWORD) data = GetIntFromParm(3);
            size = sizeof(DWORD);
            log_printf5("WriteRegDWORD: set %d\\%s\\%s to %d",rootkey,buf3,buf1,*(LPDWORD)data);
          }
          if (type == REG_BINARY)
          {
            size = GetCompressedDataFromDataBlockToMemory(parm3, data, NSIS_MAX_STRLEN);
            log_printf5("WriteRegBin: set %d\\%s\\%s with %d bytes",rootkey,buf3,buf1,size);
          }
          if (size >= 0 && RegSetValueEx(hKey,buf1,0,rtype,data,size) == ERROR_SUCCESS)
            exec_error--;
          RegCloseKey(hKey);
        }
        else { log_printf3("WriteReg: error creating key %d\\%s",rootkey,buf3); }
      }
    break;
    case EW_READREGSTR: // read registry string
      {
        HKEY hKey;
        char *p=var0;
        int rootkey=parm1;
        char *buf0=GetStringFromParm(0x02); // buf0 == subkey
        char *buf1=GetStringFromParm(0x13); // buf1 == key name
        p[0]=0;
        if (RegOpenKeyEx((HKEY)rootkey,buf0,0,KEY_READ,&hKey) == ERROR_SUCCESS)
        {
          DWORD l = NSIS_MAX_STRLEN;
          DWORD t;

          if (RegQueryValueEx(hKey,buf1,NULL,&t,p,&l ) != ERROR_SUCCESS ||
              (t != REG_DWORD && t != REG_SZ && t != REG_EXPAND_SZ))
          {
            p[0]=0;
            exec_error++;
          }
          else
          {
            if (t==REG_DWORD)
            {
              if (!parm4) exec_error++;
              myitoa(p,*((DWORD*)p));
            }
            else if (parm4) exec_error++;
          }
          RegCloseKey(hKey);
        }
        else exec_error++;
     }
    break;
    case EW_REGENUM:
      {
        HKEY key;
        char *p=var0;
        int b=GetIntFromParm(3);
        char *buf1=GetStringFromParm(0x12);
        p[0]=0;
        if (RegOpenKeyEx((HKEY)parm1,buf1,0,KEY_READ,&key) == ERROR_SUCCESS)
        {
          DWORD d=NSIS_MAX_STRLEN-1;
          if (parm4) RegEnumKey(key,b,p,d);
          else RegEnumValue(key,b,p,&d,NULL,NULL,NULL,NULL);
          p[NSIS_MAX_STRLEN-1]=0;
          RegCloseKey(key);
        }
        else exec_error++;
      }

    break;
#endif//NSIS_SUPPORT_REGISTRYFUNCTIONS
#ifdef NSIS_SUPPORT_FILEFUNCTIONS
    case EW_FCLOSE:
      {
        char *t=var0;
        if (*t) CloseHandle((HANDLE)myatoi(t));
      }
    break;
    case EW_FOPEN:
      {
        HANDLE h;
        char *handleout=var0;
        char *buf1=GetStringFromParm(-0x13);
        h=myOpenFile(buf1,parm1,parm2);
        if (h == INVALID_HANDLE_VALUE)
        {
          *handleout=0;
          exec_error++;
        }
        else
        {
          myitoa(handleout,(int)h);
        }
      }
    break;
    case EW_FPUTS:
      {
        DWORD dw;
        int l;
        char *t=var0;
        if (parm2)
        {
          ((unsigned char *)buf1)[0]=GetIntFromParm(1)&0xff;
          l=1;
        }
        else
        {
          l=mystrlen(GetStringFromParm(0x11));
        }
        if (!*t || !WriteFile((HANDLE)myatoi(t),buf1,l,&dw,NULL))
        {
          exec_error++;
        }
      }
    break;
    case EW_FGETS:
      {
        char *textout=var1;
        DWORD dw;
        int rpos=0;
        char *hptr=var0;
        int maxlen=GetIntFromParm(2);
        if (maxlen<1) break;
        if (maxlen > NSIS_MAX_STRLEN-1) maxlen=NSIS_MAX_STRLEN-1;
        if (*hptr)
        {
          char lc=0;
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
        if (!rpos) exec_error++;
      }
    break;
    case EW_FSEEK:
      {
        char *t=var0;
        if (*t)
        {
          DWORD v=SetFilePointer((HANDLE)myatoi(t),GetIntFromParm(2),NULL,parm3);

          if (parm1>=0)
          {
            myitoa(var1,v);
          }
        }
      }
    break;
#endif//NSIS_SUPPORT_FILEFUNCTIONS
#ifdef NSIS_SUPPORT_FINDFIRST
    case EW_FINDCLOSE:
      {
        char *t=var0;
        if (*t) FindClose((HANDLE)myatoi(t));
      }
    break;
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
          exec_error++;
          *textout=0;
        }

      }
    break;
    case EW_FINDFIRST:
      {
        char *textout=var0;
        char *handleout=var1;
        HANDLE h;
        WIN32_FIND_DATA fd;
        char *buf0=GetStringFromParm(0x02);
        h=FindFirstFile(buf0,&fd);
        if (h == INVALID_HANDLE_VALUE)
        {
          *handleout=0;
          *textout=0;
          exec_error++;
        }
        else
        {
          myitoa(handleout,(int)h);
          mystrcpy(textout,fd.cFileName);
        }
      }
    break;
#endif//NSIS_SUPPORT_FINDFIRST
#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
    case EW_WRITEUNINSTALLER:
      {
        int ret=-666;
        HANDLE hFile;
        char *buf0=GetStringFromParm(0x00);

        if (validpathspec(buf0))
        {
          mystrcpy(buf1,buf0);
        }
        else
        {
          lstrcat(addtrailingslash(mystrcpy(buf1,state_install_directory)),buf0);
        }
        validate_filename(buf1);

        hFile=myOpenFile(buf1,GENERIC_WRITE,CREATE_ALWAYS);
        if (hFile != INVALID_HANDLE_VALUE)
        {
          unsigned char *filebuf;
          int filehdrsize = g_filehdrsize;
          filebuf=(unsigned char *)my_GlobalAlloc(filehdrsize);
          if (filebuf)
          {
            DWORD lout;
            SetSelfFilePointer(0);
            ReadSelfFile((char*)filebuf,filehdrsize);
            {
              unsigned char* seeker;
              unsigned char* unicon_data = seeker = (unsigned char*)my_GlobalAlloc(parm2);
              if (unicon_data) {
                GetCompressedDataFromDataBlockToMemory(parm1,unicon_data,parm2);
                while (*seeker) {
                  struct icondata {
                    DWORD dwSize;
                    DWORD dwOffset;
                  } id = *(struct icondata *) seeker;
                  seeker += sizeof(struct icondata);
                  mini_memcpy(filebuf+id.dwOffset, seeker, id.dwSize);
                  seeker += id.dwSize;
                }
                GlobalFree(unicon_data);
              }
            }
            WriteFile(hFile,(char*)filebuf,filehdrsize,&lout,NULL);
            GlobalFree(filebuf);
            ret=GetCompressedDataFromDataBlock(-1,hFile);
          }
          CloseHandle(hFile);
        }
        log_printf3("created uninstaller: %d, \"%s\"",ret,buf1);
        {
          int str = LANG_CREATEDUNINST;
          if (ret < 0)
          {
            str = LANG_ERRORCREATING;
            DeleteFile(buf1);
            exec_error++;
          }
          update_status_text_buf1(str);
        }
      }
    break;
#endif//NSIS_CONFIG_UNINSTALL_SUPPORT
#ifdef NSIS_CONFIG_LOG
    case EW_LOG:
      if (parm0)
      {
        log_printf2("settings logging to %d",parm1);
        log_dolog=parm1;
        log_printf2("logging set to %d",parm1);
#ifndef NSIS_CONFIG_LOG_ODS
        if (parm1) build_g_logfile();
#endif
      }
      else
      {
        char *buf0=GetStringFromParm(0x01);
        log_printf2("%s",buf0);
      }
    break;
#endif//NSIS_CONFIG_LOG
#ifdef NSIS_CONFIG_COMPONENTPAGE
    case EW_SECTIONSET:
    {
      int x=GetIntFromParm(0);
      if ((unsigned int)x < (unsigned int)num_sections)
      {
        section *sec=g_sections+x;
        if (parm2>=0) // get something
        {
          int res=((int*)sec)[parm2];
          if (!parm2)
          {
            // getting text
            GetNSISString(var1,res);
          }
          else
          {
            // getting number
            myitoa(var1,res);
          }
        }
        else // set something
        {
          parm2=-parm2-1;
          if (parm2)
          {
            // not setting text, get int
            parm1=GetIntFromParm(1);
          }
          else
          {
            // setting text, send the message to do it
            SendMessage(hwSectionHack,WM_NOTIFY_SECTEXT,x,parm1);
          }
          ((int*)sec)[parm2]=parm1;
          if (parm2)
          {
            // update tree view
            SendMessage(hwSectionHack,WM_NOTIFY_SECFLAGS,x,0);
          }
        }
      }
      else exec_error++;
    }
    break;
    case EW_INSTTYPESET:
    {
      int x=GetIntFromParm(0);

      if (parm3)
      {
        g_exec_flags.insttype_changed++;
        SendMessage(hwSectionHack,WM_NOTIFY_INSTTYPE_CHANGE,0,0);
      }
      else if ((unsigned int)x < (unsigned int)NSIS_MAX_INST_TYPES)
      {
        if (parm2) // set text
        {
          g_header->install_types[x] = parm1;
        }
        else // get text
        {
          GetNSISString(var1,g_header->install_types[x]);
        }
      }
      else exec_error++;
    }
    break;
#endif//NSIS_CONFIG_COMPONENTPAGE

#ifdef NSIS_LOCKWINDOW_SUPPORT
    case EW_LOCKWINDOW:
    {
      // ui_dlg_visible is 1 or 0, so is parm0
      SendMessage(g_hwnd, WM_SETREDRAW, parm0 & ui_dlg_visible, 0);
      if ( parm0 )
        InvalidateRect(g_hwnd, NULL, FALSE);
    }
#endif //NSIS_LOCKWINDOW_SUPPORT
  }

  g_exec_flags.exec_error += exec_error;

  return 0;
}
