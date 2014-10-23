#include <windows.h>
#include <shlobj.h>
#include <nsis/pluginapi.h> // nsis plugin
#include "resource.h"

HINSTANCE g_hInstance;

HWND hwParent;
HWND hwChild;
HWND g_hwStartMenuSelect;
HWND g_hwDirList;

TCHAR buf[1024];
TCHAR text[1024];
TCHAR progname[1024];
TCHAR lastused[1024];
TCHAR checkbox[1024];

int autoadd;
int g_done;
int noicon;
int rtl;

WNDPROC lpWndProcOld;

void (__stdcall *validate_filename)(LPTSTR);

INT_PTR CALLBACK dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
static INT_PTR CALLBACK ParentWndProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
void AddFolderFromReg(int nFolder);

static UINT_PTR PluginCallback(enum NSPIM msg)
{
  return 0;
}

void __declspec(dllexport) Init(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop, extra_parameters *extra)
{
  HWND hwStartMenuSelect;

  hwParent = hwndParent;

  validate_filename = extra->validate_filename;

  EXDLL_INIT();

  extra->RegisterPluginCallback(g_hInstance, PluginCallback);

  g_done = 0;
  noicon = 0;
  rtl = 0;
  autoadd = 0;

  text[0] = 0;
  progname[0] = 0;
  lastused[0] = 0;
  checkbox[0] = 0;

  g_hwStartMenuSelect = NULL;

  {
    hwChild = GetDlgItem(hwndParent, 1018);
    if (!hwChild)
    {
      pushstring(_T("error finding childwnd"));
      return;
    }

    popstring(buf);

    while (buf[0] == _T('/'))
    {
      if (!lstrcmpi(buf+1, _T("noicon")))
      {
        noicon = 1;
      }
      else if (!lstrcmpi(buf+1, _T("rtl")))
      {
        rtl = 1;
      }
      else if (!lstrcmpi(buf+1, _T("text")))
      {
        popstring(text);
      }
      else if (!lstrcmpi(buf+1, _T("autoadd")))
      {
        autoadd = 1;
      }
      else if (!lstrcmpi(buf+1, _T("lastused")))
      {
        popstring(lastused);
      }
      else if (!lstrcmpi(buf+1, _T("checknoshortcuts")))
      {
        popstring(checkbox);
      }
      
      if (popstring(buf))
      {
        *buf = 0;
      }
    }

    if (*buf)
    {
      lstrcpy(progname, buf);
    }
    else
    {
      pushstring(_T("error reading parameters"));
      return;
    }

    hwStartMenuSelect = CreateDialog(g_hInstance, MAKEINTRESOURCE(IDD_DIALOG), hwndParent, dlgProc);
    g_hwStartMenuSelect = hwStartMenuSelect;
    if (!hwStartMenuSelect)
    {
      pushstring(_T("error creating dialog"));
      return;
    }
    else
    {
      lpWndProcOld = (WNDPROC) SetWindowLongPtr(hwndParent, DWLP_DLGPROC, (LONG_PTR) ParentWndProc);
      wsprintf(buf, _T("%u"), hwStartMenuSelect);
      pushstring(buf);
    }
  }
}

void __declspec(dllexport) Show(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop)
{
  HWND hwStartMenuSelect = g_hwStartMenuSelect;

  if (!hwStartMenuSelect)
  {
    return;
  }

  while (!g_done)
  {
    MSG msg;
    GetMessage(&msg, NULL, 0, 0);
    if (!IsDialogMessage(hwStartMenuSelect,&msg) && !IsDialogMessage(hwndParent,&msg) && !TranslateMessage(&msg))
      DispatchMessage(&msg);
  }
  DestroyWindow(hwStartMenuSelect);

  SetWindowLongPtr(hwndParent, DWLP_DLGPROC, (LONG_PTR) lpWndProcOld);
}

void __declspec(dllexport) Select(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop, extra_parameters *extra)
{
  Init(hwndParent, string_size, variables, stacktop, extra);
  if (g_hwStartMenuSelect)
  {
    popstring(buf);
    Show(hwndParent, string_size, variables, stacktop);
  }
}

static INT_PTR CALLBACK ParentWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  INT_PTR bRes = CallWindowProc(lpWndProcOld,hwnd,message,wParam,lParam);
  if (message == WM_NOTIFY_OUTER_NEXT && !bRes)
  {
    // if leave function didn't abort (lRes != 0 in that case)
    PostMessage(g_hwStartMenuSelect,WM_USER+666,wParam,0);
  }
  return bRes;
}

void AddRTLStyle(HWND hWnd, long dwStyle)
{
  LONG_PTR s;

  s = GetWindowLongPtr(hWnd, GWL_STYLE);
  SetWindowLongPtr(hWnd, GWL_STYLE, s | dwStyle);
  s = GetWindowLongPtr(hWnd, GWL_EXSTYLE);
  SetWindowLongPtr(hWnd, GWL_EXSTYLE, s | WS_EX_RIGHT | WS_EX_RTLREADING);
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

INT_PTR CALLBACK dlgProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
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

      SetWindowText(hwText, *text ? text : _T("Select the Start Menu folder in which you would like to create the program's shortcuts:"));

      ProgressiveSetWindowPos(
        hwLocation,
        0,
        width,
        MulDiv(12, baseUnitY, 8)
      );

      if (*lastused == _T('>'))
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
      if (IsDlgButtonChecked(hwndDlg, IDC_CHECK) == BST_CHECKED)
        SendMessage(hwndDlg, WM_NEXTDLGCTL, (WPARAM) hwCheckBox, TRUE);
      else
        SendMessage(hwndDlg, WM_NEXTDLGCTL, (WPARAM) hwLocation, TRUE);
    }
    break;
    case WM_COMMAND:
      if (LOWORD(wParam) == IDC_DIRLIST && HIWORD(wParam) == LBN_SELCHANGE)
      {
        LRESULT selection = SendMessage(hwDirList, LB_GETCURSEL, 0, 0);
        if (selection != LB_ERR)
        {
          SendMessage(hwDirList, LB_GETTEXT, selection, (WPARAM)buf);
          if (autoadd)
            lstrcat(lstrcat(buf, _T("\\")), progname);
          SetWindowText(hwLocation, buf);
        }
      }
      else if (LOWORD(wParam) == IDC_CHECK && HIWORD(wParam) == BN_CLICKED)
      {
        BOOL bEnable = IsDlgButtonChecked(hwndDlg, IDC_CHECK) != BST_CHECKED;
        EnableWindow(hwDirList, bEnable);
        EnableWindow(hwLocation, bEnable);
        if (bEnable)
          goto ValidateLocation;
        *buf = _T('!'); //This only needs to be != 0, actual value does not matter
        goto SetOkBtn;
      }
      else if (LOWORD(wParam) == IDC_LOCATION && HIWORD(wParam) == EN_CHANGE)
      {
        ValidateLocation:
        GetWindowText(hwLocation, buf, MAX_PATH);
        validate_filename(buf);
        SetOkBtn:
        EnableWindow(GetDlgItem(hwParent, IDOK), *buf != _T('\0'));
      }
    break;
    case WM_USER+666:
      g_done = 1;
      if (wParam == NOTIFY_BYE_BYE)
        pushstring(_T("cancel"));
      else
      {
        GetWindowText(hwLocation, buf + 1, MAX_PATH);
        validate_filename(buf);
        if (IsDlgButtonChecked(hwndDlg, IDC_CHECK) == BST_CHECKED)
        {
          buf[0] = _T('>');
          pushstring(buf);
        }
        else
        {
          pushstring(buf + 1);
        }
        pushstring(_T("success"));
      }
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLOREDIT:
    case WM_CTLCOLORDLG:
    case WM_CTLCOLORBTN:
    case WM_CTLCOLORLISTBOX:
      // let the NSIS window handle colors, it knows best
      return SendMessage(hwParent, uMsg, wParam, lParam);
    break;
  }
  return FALSE;
}

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
  return TRUE;
}

void AddFolderFromReg(int nFolder)
{
  //DWORD idx;
  WIN32_FIND_DATA FileData;
  HANDLE hSearch;
  LPITEMIDLIST ppidl;

  buf[0] = 0;
  if (SHGetSpecialFolderLocation(hwParent, nFolder, &ppidl) == S_OK)
  {
    SHGetPathFromIDList(ppidl, buf);
    CoTaskMemFree(ppidl);
  }

  if (!buf[0])
    return;

  lstrcat(buf, _T("\\*.*"));
  hSearch = FindFirstFile(buf, &FileData);
  if (hSearch != INVALID_HANDLE_VALUE) 
  {
    do
    {
      if (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      {
        if (lstrcmp(FileData.cFileName, _T(".")) != 0)
        {
          if (lstrcmp(FileData.cFileName, _T("..")) != 0)
          {
            if (SendMessage(g_hwDirList, LB_FINDSTRINGEXACT, (WPARAM) -1, (LPARAM)FileData.cFileName) == LB_ERR)
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
