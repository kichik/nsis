#include <windows.h>
#include "../exdll/exdll.h"
#include "resource.h"

HINSTANCE g_hInstance;

HWND hwParent;
HWND hwChild;
HWND hwStartMenuSelect;
HWND hwIcon;
HWND hwText;
HWND hwLocation;
HWND hwDirList;
HWND hwCheckBox;

char buf[MAX_PATH];
char text[1024];
char progname[1024];
char lastused[1024];
char checkbox[1024];

int autoadd = 0;
int g_done = 0;
int noicon = 0;

void *lpWndProcOld;

BOOL CALLBACK dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK ParentWndProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
void AddFolderFromReg(HKEY rootKey);

void __declspec(dllexport) Select(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  hwParent = hwndParent;

  EXDLL_INIT();

  {
    int cw_vis;

    hwChild = FindWindowEx(hwndParent, NULL, "#32770", NULL); // find window to replace
    if (!hwChild) hwChild = GetDlgItem(hwndParent, 1018);
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
    if (*buf) lstrcpy(progname, buf);
    else
    {
      pushstring("error reading parameters");
      return;
    }

    cw_vis = IsWindowVisible(hwChild);
    if (cw_vis) ShowWindow(hwChild, SW_HIDE);

    hwStartMenuSelect = CreateDialog(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), hwndParent, dlgProc);
    if (!hwStartMenuSelect)
    {
      pushstring("error creating dialog");
      g_done = 1;
    }
    else
    {
      lpWndProcOld = (void *) SetWindowLong(hwndParent, GWL_WNDPROC, (long) ParentWndProc);
    }

    while (!g_done)
    {
      MSG msg;
      int nResult = GetMessage(&msg, NULL, 0, 0);
      if (!IsDialogMessage(hwStartMenuSelect,&msg) && !IsDialogMessage(hwndParent,&msg) && !TranslateMessage(&msg))
        DispatchMessage(&msg);
    }
    DestroyWindow(hwStartMenuSelect);

    SetWindowLong(hwndParent, GWL_WNDPROC, (long) lpWndProcOld);

    if (cw_vis) ShowWindow(hwChild, SW_SHOW);
  }
}

static LRESULT CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  if (message == WM_NOTIFY_OUTER_NEXT)
  {
    PostMessage(hwStartMenuSelect,WM_USER+666,wParam,0);
  }
  return CallWindowProc((long (__stdcall *)(struct HWND__ *,unsigned int,unsigned int,long))lpWndProcOld,hwnd,message,wParam,lParam);
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
  switch (uMsg)
  {
    case WM_INITDIALOG:
    {
      RECT dialog_r, temp_r;

      HFONT hFont = (HFONT)SendMessage(hwParent, WM_GETFONT, 0, 0);

      int y_offset = 0;

      GetWindowRect(hwChild, &dialog_r);
      ScreenToClient(hwParent, (LPPOINT) &dialog_r);
      ScreenToClient(hwParent, ((LPPOINT) &dialog_r)+1);
      SetWindowPos(
        hwndDlg,
        0,
        dialog_r.left,
        dialog_r.top,
        dialog_r.right - dialog_r.left,
        dialog_r.bottom - dialog_r.top,
        SWP_NOZORDER | SWP_NOACTIVATE
      );

      hwIcon = GetDlgItem(hwndDlg, IDC_NSISICON);
      hwText = GetDlgItem(hwndDlg, IDC_TEXT);
      hwLocation = GetDlgItem(hwndDlg, IDC_LOCATION);
      hwDirList = GetDlgItem(hwndDlg, IDC_DIRLIST);
      hwCheckBox = GetDlgItem(hwndDlg, IDC_CHECK);

      SendMessage(hwndDlg, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwIcon, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwText, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwLocation, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwDirList, WM_SETFONT, (WPARAM) hFont, TRUE);
      SendMessage(hwCheckBox, WM_SETFONT, (WPARAM) hFont, TRUE);

      if (!noicon)
      {
        SendMessage(
          hwIcon,
          STM_SETIMAGE,
          IMAGE_ICON,
          (LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103))
        );
      }
      SetWindowPos(
        hwIcon,
        0,
        0,
        0,
        32,
        32,
        SWP_NOACTIVATE | (noicon ? SWP_HIDEWINDOW : 0)
      );

      if (!*text)
        lstrcpy(text, "Select the Start Menu folder in which you would like to create the program's shortcuts:");

      GetWindowRect(hwIcon, &temp_r);
      temp_r.right += 5;
      temp_r.bottom += 5;
      ScreenToClient(hwndDlg, ((LPPOINT) &temp_r) + 1);

      ProgressiveSetWindowPos(
        hwText,
        noicon ? 0 : temp_r.right,
        dialog_r.right - dialog_r.left - (noicon ? 0 : temp_r.right),
        temp_r.bottom + 2
      );

      SetWindowText(hwText, text);

      GetWindowRect(hwLocation, &temp_r);

      ProgressiveSetWindowPos(
        hwLocation,
        0,
        dialog_r.right - dialog_r.left,
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
        dialog_r.right - dialog_r.left,
        dialog_r.bottom - dialog_r.top - y_offset - (*checkbox ? temp_r.bottom - temp_r.top + 5 : 0)
      );

      ProgressiveSetWindowPos(
        hwCheckBox,
        0,
        dialog_r.right - dialog_r.left,
        temp_r.bottom - temp_r.top
      );

      if (*checkbox)
      {
        ShowWindow(hwCheckBox, SW_SHOWNA);
        SetWindowText(hwCheckBox, checkbox);
      }

      AddFolderFromReg(HKEY_LOCAL_MACHINE);
      AddFolderFromReg(HKEY_CURRENT_USER);

      SendMessage(hwParent, WM_NOTIFY_CUSTOM_READY, 0, 0);
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
        BOOL bEnable = IsDlgButtonChecked(hwStartMenuSelect, IDC_CHECK) != BST_CHECKED;
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
        if (IsDlgButtonChecked(hwStartMenuSelect, IDC_CHECK) == BST_CHECKED)
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

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
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

  lstrcat(buf, "\\*.");
  hSearch = FindFirstFile(buf, &FileData);
  if (hSearch != INVALID_HANDLE_VALUE) do
  {
    if (*(WORD*)FileData.cFileName != *(WORD*)".")
    {
      if (*(WORD*)FileData.cFileName != *(WORD*)".." || FileData.cFileName[2])
      {
        if (SendMessage(hwDirList, LB_FINDSTRINGEXACT, -1, (LPARAM)FileData.cFileName) == LB_ERR)
          SendMessage(hwDirList, LB_ADDSTRING, 0, (LPARAM)FileData.cFileName);
        /*idx = */
        /*SendMessage(hwDirList, LB_SETITEMDATA, (WPARAM)idx,
          (LPARAM)ExtractAssociatedIcon(g_hInstance, FileData.cFileName, (WORD*)&idx));*/
      }
    }
  } while (FindNextFile(hSearch, &FileData));
}