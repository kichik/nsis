#include <windows.h>
#include <shlobj.h>
#include "../exdll/exdll.h"
#include "resource.h"

HINSTANCE g_hInstance;

HWND hwParent;
HWND hwChild;
HWND g_hwStartMenuSelect;
HWND g_hwDirList;

char buf[1024];
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
void AddFolderFromReg(int nFolder);

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

void AddRTLStyle(HWND hWnd, long dwStyle)
{
  long s;

  s = GetWindowLong(hWnd, GWL_STYLE);
  SetWindowLong(hWnd, GWL_STYLE, s | dwStyle);
  s = GetWindowLong(hWnd, GWL_EXSTYLE);
  SetWindowLong(hWnd, GWL_EXSTYLE, s | WS_EX_RIGHT | WS_EX_RTLREADING);
}

#define ProgressiveSetWindowPos(hwWindow, x, cx, cy) \
  MoveWindow( \
    hwWindow, \
    x, \
    y_offset, \
    cx, \
    cy, \
    FALSE \
  ); \
   \
  y_offset += cy + 3;

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

      HFONT hFont = (HFONT) SendMessage(hwParent, WM_GETFONT, 0, 0);

      int y_offset = 0;

      int width, height;

      int baseUnitY;

      // Init dialog unit conversion

      {
        TEXTMETRIC tm;
        HDC hDC;

        hDC = GetDC(hwndDlg);
        SelectObject(hDC, hFont);

        GetTextMetrics(hDC, &tm);
        baseUnitY = tm.tmHeight;

        ReleaseDC(hwndDlg, hDC);
      }

      GetWindowRect(hwChild, &dialog_r);
      ScreenToClient(hwParent, (LPPOINT) &dialog_r);
      ScreenToClient(hwParent, ((LPPOINT) &dialog_r) + 1);

      width = dialog_r.right - dialog_r.left;
      height = dialog_r.bottom - dialog_r.top;

      MoveWindow(
        hwndDlg,
        dialog_r.left,
        dialog_r.top,
        width,
        height,
        FALSE
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
        AddRTLStyle(hwText, SS_RIGHT);
        AddRTLStyle(hwLocation, ES_RIGHT);
        AddRTLStyle(hwDirList, 0);
        AddRTLStyle(hwCheckBox, BS_RIGHT | BS_LEFTTEXT);
      }

      GetClientRect(hwIcon, &temp_r);

      if (!noicon)
      {
        SendMessage(
          hwIcon,
          STM_SETIMAGE,
          IMAGE_ICON,
          (LPARAM)LoadIcon(GetModuleHandle(0), MAKEINTRESOURCE(103))
        );

        MoveWindow(
          hwIcon,
          rtl ? width - temp_r.right : 0,
          0,
          temp_r.right,
          temp_r.bottom,
          FALSE
        );

        temp_r.right += 3;
      }
      else
      {
        ShowWindow(hwIcon, SW_HIDE);
        
        temp_r.right = 0;
      }

      if (rtl)
      {
        ProgressiveSetWindowPos(
          hwText,
          0,
          width - temp_r.right,
          3 * baseUnitY //MulDiv(24, baseUnitY, 8);
        );
      }
      else
      {
        ProgressiveSetWindowPos(
          hwText,
          temp_r.right,
          width - temp_r.right + 3,
          3 * baseUnitY //MulDiv(24, baseUnitY, 8);
        );
      }

      SetWindowText(hwText, *text ? text : "Select the Start Menu folder in which you would like to create the program's shortcuts:");

      ProgressiveSetWindowPos(
        hwLocation,
        0,
        width,
        MulDiv(12, baseUnitY, 8)
      );

      if (*lastused == '>')
      {
        CheckDlgButton(hwndDlg, IDC_CHECK, BST_CHECKED);
        lstrcpy(lastused, lstrcpy(buf, lastused) + 1);
        EnableWindow(hwDirList, FALSE);
        EnableWindow(hwLocation, FALSE);
      }

      SetWindowText(hwLocation, *lastused ? lastused : progname);

      temp_r.bottom = MulDiv(8, baseUnitY, 8);

      ProgressiveSetWindowPos(
        hwDirList,
        0,
        width,
        height - y_offset - (*checkbox ? temp_r.bottom + 3 : 0)
      );

      if (*checkbox)
      {
        ProgressiveSetWindowPos(
          hwCheckBox,
          0,
          width,
          temp_r.bottom
        );

        ShowWindow(hwCheckBox, SW_SHOWNA);
        SetWindowText(hwCheckBox, checkbox);
      }

      AddFolderFromReg(CSIDL_COMMON_PROGRAMS);
      AddFolderFromReg(CSIDL_PROGRAMS);

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

void AddFolderFromReg(int nFolder)
{
  //DWORD idx;
  WIN32_FIND_DATA FileData;
  HANDLE hSearch;

  LPMALLOC ppMalloc;
  if (SHGetMalloc(&ppMalloc) == NOERROR)
  {
    LPITEMIDLIST ppidl;

    buf[0] = 0;
    if (SHGetSpecialFolderLocation(hwParent, nFolder, &ppidl) == S_OK)
    {
      SHGetPathFromIDList(ppidl, buf);
      ppMalloc->lpVtbl->Free(ppMalloc, ppidl);
    }

    ppMalloc->lpVtbl->Release(ppMalloc);
  }

  if (!buf[0])
    return;

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