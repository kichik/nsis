#include <windows.h>
#include "../exdll/exdll.h"
#include "resource.h"

HINSTANCE g_hInstance;

HWND hwParent;
HWND hwChild;
HWND g_hwStartMenuSelect;
HWND g_hwDirList;

char buf[MAX_PATH];
char text[1024];
char progname[1024];
char lastused[1024];
char checkbox[1024];

int autoadd = 0;
int g_done = 0;
int noicon = 0;
int rtl = 0;

void *lpWndProcOld;

BOOL CALLBACK dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK ParentWndProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
void AddFolderFromReg(HKEY rootKey);

void __declspec(dllexport) Select(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  HWND hwStartMenuSelect;

  hwParent = hwndParent;

  EXDLL_INIT();

  {
    hwChild = GetDlgItem(hwndParent, 1018);
    if (!hwChild)
    {
      pushstring("error finding childwnd");
      return;
    }

    popstring(buf);

    while (buf[0] == '/')
    {
      if (!lstrcmpi(buf+1, "noicon"))
      {
        noicon = 1;
      }
      else if (!lstrcmpi(buf+1, "rtl"))
      {
        rtl = 1;
      }
      else if (!lstrcmpi(buf+1, "text"))
      {
        popstring(text);
      }
      else if (!lstrcmpi(buf+1, "autoadd"))
      {
        autoadd = 1;
      }
      else if (!lstrcmpi(buf+1, "lastused"))
      {
        popstring(lastused);
      }
      else if (!lstrcmpi(buf+1, "checknoshortcuts"))
      {
        popstring(checkbox);
      }
      if (popstring(buf))
        *buf = 0;
    }
    if (*buf)
      lstrcpy(progname, buf);
    else
    {
      pushstring("error reading parameters");
      return;
    }

    hwStartMenuSelect = CreateDialog(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), hwndParent, dlgProc);
    g_hwStartMenuSelect = hwStartMenuSelect;
    if (!hwStartMenuSelect)
    {
      pushstring("error creating dialog");
      g_done = 1;
    }
    else
    {
      lpWndProcOld = (void *) SetWindowLong(hwndParent, DWL_DLGPROC, (long) ParentWndProc);
    }

    while (!g_done)
    {
      MSG msg;
      int nResult = GetMessage(&msg, NULL, 0, 0);
      if (!IsDialogMessage(hwStartMenuSelect,&msg) && !IsDialogMessage(hwndParent,&msg) && !TranslateMessage(&msg))
        DispatchMessage(&msg);
    }
    DestroyWindow(hwStartMenuSelect);

    SetWindowLong(hwndParent, DWL_DLGPROC, (long) lpWndProcOld);
  }
}

static BOOL CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  BOOL bRes = CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
  if (message == WM_NOTIFY_OUTER_NEXT && !bRes)
  {
    // if leave function didn't abort (lRes != 0 in that case)
    PostMessage(g_hwStartMenuSelect,WM_USER+666,wParam,0);
  }
  return bRes;
}

#define ProgressiveSetWindowPos(hwWindow, x, cx, cy) \
  SetWindowPos( \
    hwWindow, \
    0, \
    x, \
    y_offset, \
    cx, \
    cy, \
    SWP_NOACTIVATE \
  ); \
   \
  y_offset += cy + 5;

BOOL CALLBACK dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  HWND hwLocation = GetDlgItem(hwndDlg, IDC_LOCATION);
  HWND hwDirList = GetDlgItem(hwndDlg, IDC_DIRLIST);
  HWND hwCheckBox = GetDlgItem(hwndDlg, IDC_CHECK);

  switch (uMsg)
  {
    case WM_INITDIALOG:
    {
      HWND hwIcon;
      HWND hwText;

      RECT dialog_r, temp_r;

      HFONT hFont = (HFONT)SendMessage(hwParent, WM_GETFONT, 0, 0);

      int y_offset = 0;

      int width, height;

      GetWindowRect(hwChild, &dialog_r);
      ScreenToClient(hwParent, (LPPOINT) &dialog_r);
      ScreenToClient(hwParent, ((LPPOINT) &dialog_r)+1);

      width = dialog_r.right - dialog_r.left;
      height = dialog_r.bottom - dialog_r.top;

      SetWindowPos(
        hwndDlg,
        0,
        dialog_r.left,
        dialog_r.top,
        width,
        height,
        SWP_NOZORDER | SWP_NOACTIVATE
      );

      hwIcon = GetDlgItem(hwndDlg, IDC_NSISICON);
      hwText = GetDlgItem(hwndDlg, IDC_TEXT);
      g_hwDirList = hwDirList;

      SendMessage(hwndDlg, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwIcon, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwText, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwLocation, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwDirList, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwCheckBox, WM_SETFONT, (WPARAM) hFont, TRUE);

      if (rtl)
      {
        long s;
        s = GetWindowLong(hwText, GWL_STYLE);
        SetWindowLong(hwText, GWL_STYLE, (s & ~SS_LEFT) | SS_RIGHT);
        s = GetWindowLong(hwLocation, GWL_STYLE);
        SetWindowLong(hwLocation, GWL_STYLE, (s & ~ES_LEFT) | ES_RIGHT);
        s = GetWindowLong(hwDirList, GWL_EXSTYLE);
        SetWindowLong(hwDirList, GWL_EXSTYLE, s | WS_EX_RIGHT | WS_EX_RTLREADING);
        s = GetWindowLong(hwCheckBox, GWL_STYLE);
        SetWindowLong(hwCheckBox, GWL_STYLE, s | BS_RIGHT | BS_LEFTTEXT);
      }

      if (!noicon)
      {
        SendMessage(
          hwIcon,
          STM_SETIMAGE,
          IMAGE_ICON,
          (LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103))
        );
      }

      GetClientRect(hwIcon, &temp_r);

      SetWindowPos(
        hwIcon,
        0,
        rtl ? width - temp_r.right : 0,
        0,
        temp_r.right,
        temp_r.bottom,
        SWP_NOACTIVATE | (noicon ? SWP_HIDEWINDOW : 0)
      );

      if (rtl)
      {
        ProgressiveSetWindowPos(
          hwText,
          0,
          width - (noicon ? 0 : temp_r.right + 5),
          temp_r.bottom + 2
        );
      }
      else
      {
        ProgressiveSetWindowPos(
          hwText,
          noicon ? 0 : temp_r.right + 5,
          width - (noicon ? 0 : temp_r.right + 5),
          temp_r.bottom + 2
        );
      }

      SetWindowText(hwText, *text ? text : "Select the Start Menu folder in which you would like to create the program's shortcuts:");

      GetWindowRect(hwLocation, &temp_r);

      ProgressiveSetWindowPos(
        hwLocation,
        0,
        width,
        temp_r.bottom - temp_r.top
      );

      if (*lastused == '>')
      {
        CheckDlgButton(hwndDlg, IDC_CHECK, BST_CHECKED);
        lstrcpy(lastused, lstrcpy(buf, lastused) + 1);
        EnableWindow(hwDirList, FALSE);
        EnableWindow(hwLocation, FALSE);
      }

      SetWindowText(hwLocation, *lastused ? lastused : progname);

      GetWindowRect(hwCheckBox, &temp_r);
      ScreenToClient(hwndDlg, ((LPPOINT) &temp_r));
      ScreenToClient(hwndDlg, ((LPPOINT) &temp_r) + 1);

      ProgressiveSetWindowPos(
        hwDirList,
        0,
        width,
        height - y_offset - (*checkbox ? temp_r.bottom - temp_r.top + 5 : 0)
      );

      ProgressiveSetWindowPos(
        hwCheckBox,
        0,
        width,
        temp_r.bottom - temp_r.top
      );

      if (*checkbox)
      {
        ShowWindow(hwCheckBox, SW_SHOWNA);
        SetWindowText(hwCheckBox, checkbox);
      }

      AddFolderFromReg(HKEY_LOCAL_MACHINE);
      AddFolderFromReg(HKEY_CURRENT_USER);

      // Tell NSIS to remove old inner dialog and pass handle of the new inner dialog
      SendMessage(hwParent, WM_NOTIFY_CUSTOM_READY, (WPARAM)hwndDlg, 0);
      ShowWindow(hwndDlg, SW_SHOWNA);
      SetFocus(GetDlgItem(hwParent, IDOK));
    }
    break;
    case WM_COMMAND:
      if (LOWORD(wParam) == IDC_DIRLIST && HIWORD(wParam) == LBN_SELCHANGE)
      {
        SendMessage(hwDirList, LB_GETTEXT, SendMessage(hwDirList, LB_GETCURSEL, 0, 0), (WPARAM)buf);
        if (autoadd)
          lstrcat(lstrcat(buf, "\\"), progname);
        SetWindowText(hwLocation, buf);
      }
      else if (LOWORD(wParam) == IDC_CHECK && HIWORD(wParam) == BN_CLICKED)
      {
        BOOL bEnable = IsDlgButtonChecked(hwndDlg, IDC_CHECK) != BST_CHECKED;
        EnableWindow(hwDirList, bEnable);
        EnableWindow(hwLocation, bEnable);
      }
    break;
    case WM_USER+666:
      g_done = 1;
      if (wParam == NOTIFY_BYE_BYE)
        pushstring("cancel");
      else
      {
        if (IsDlgButtonChecked(hwndDlg, IDC_CHECK) == BST_CHECKED)
        {
          short *sbuf = (short *) buf;
          *sbuf = *(short *) ">";
        }
        else *buf = 0;
        GetWindowText(hwLocation, buf + (*buf ? 1 : 0), MAX_PATH);
        pushstring(buf);
        pushstring("success");
      }
    break;
  }
	return 0;
}

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
	return TRUE;
}

void AddFolderFromReg(HKEY rootKey)
{
  DWORD dwResult;
  DWORD dwLength = MAX_PATH;
  DWORD dwType = REG_SZ;
  HKEY hKey;

  //DWORD idx;
  WIN32_FIND_DATA FileData;
  HANDLE hSearch;

  char szName[20] = "Common Programs";

  dwResult = RegOpenKeyEx(
    rootKey,
		"Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
		0,
	  KEY_READ,
		&hKey
  );

  if (dwResult == ERROR_SUCCESS)
  {
    dwResult = RegQueryValueEx(
      hKey,
      rootKey == HKEY_LOCAL_MACHINE ? szName : szName + 7,
			NULL,
			&dwType,
			(BYTE *) buf,
			&dwLength
    );
    RegCloseKey(hKey);
  }

  lstrcat(buf, "\\*.*");
  hSearch = FindFirstFile(buf, &FileData);
  if (hSearch != INVALID_HANDLE_VALUE) 
  {
    do
    {
      if (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      {
        if (*(WORD*)FileData.cFileName != *(WORD*)".")
        {
          if (*(WORD*)FileData.cFileName != *(WORD*)".." || FileData.cFileName[2])
          {
            if (SendMessage(g_hwDirList, LB_FINDSTRINGEXACT, -1, (LPARAM)FileData.cFileName) == LB_ERR)
              SendMessage(g_hwDirList, LB_ADDSTRING, 0, (LPARAM)FileData.cFileName);
            /*idx = */
            /*SendMessage(hwDirList, LB_SETITEMDATA, (WPARAM)idx,
              (LPARAM)ExtractAssociatedIcon(g_hInstance, FileData.cFileName, (WORD*)&idx));*/
          }
        }
	  }
    } while (FindNextFile(hSearch, &FileData));
    FindClose(hSearch);
  }
}