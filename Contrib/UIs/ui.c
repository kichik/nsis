// ui.cpp : Defines the entry point for the application.
//
// Unicode support by Jim Park -- 08/10/2007

#include "../../Source/Platform.h"
#include <windows.h>
#include <commctrl.h>
#include "resource.h"

HINSTANCE g_hInstance;
HWND m_curwnd;

const TCHAR* windows[] = {
  MAKEINTRESOURCE(IDD_LICENSE),
  MAKEINTRESOURCE(IDD_LICENSE_FSRB),
  MAKEINTRESOURCE(IDD_LICENSE_FSCB),
  MAKEINTRESOURCE(IDD_SELCOM),
  MAKEINTRESOURCE(IDD_DIR),
  MAKEINTRESOURCE(IDD_INSTFILES),
  MAKEINTRESOURCE(IDD_UNINST)
};

INT_PTR CALLBACK GenericProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam) {
  static LOGBRUSH b = {BS_SOLID, RGB(255,0,0), 0};
  static HBRUSH red;

  if (!red)
    red = CreateBrushIndirect(&b);

  switch (uMsg) {
    case WM_CTLCOLORSTATIC:
      return (INT_PTR)red;
  }
  return FALSE;
}

INT_PTR CALLBACK DialogProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam) {
  static int i = -1;
  switch (uMsg) {
  case WM_INITDIALOG:
    SetWindowText(hwndDlg, _T("NSIS User Interface - Testing"));
    SetWindowText(GetDlgItem(hwndDlg, IDC_VERSTR), _T("NSIS version"));
    SetWindowText(GetDlgItem(hwndDlg, IDC_BACK), _T("< Back"));
    SetWindowText(GetDlgItem(hwndDlg, IDOK), _T("Next >"));
    SetWindowText(GetDlgItem(hwndDlg, IDCANCEL), _T("Cancel"));
    ShowWindow(GetDlgItem(hwndDlg, IDC_BACK), SW_SHOW);
    ShowWindow(GetDlgItem(hwndDlg, IDC_CHILDRECT), SW_SHOW);
    SendMessage(hwndDlg, WM_COMMAND, MAKEWORD(IDOK, 0), 0);
    ShowWindow(hwndDlg, SW_SHOW);
    break;
  case WM_COMMAND:
    switch (LOWORD(wParam)) {
    case IDOK:
    case IDC_BACK:
      i+=(LOWORD(wParam)==IDOK)?1:-1;
      if (i < 0) {
        i++;
        break;
      }
      if (i >= (int)sizeof(windows)/sizeof(TCHAR*)) {
        i--;
        break;
      }
      if (m_curwnd) DestroyWindow(m_curwnd);
      m_curwnd=CreateDialog(g_hInstance,windows[i],hwndDlg,GenericProc);
      if (m_curwnd)
      {
        RECT r;
        GetWindowRect(GetDlgItem(hwndDlg,IDC_CHILDRECT),&r);
        ScreenToClient(hwndDlg,(LPPOINT)&r);
        SetWindowPos(m_curwnd,0,r.left,r.top,0,0,SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
        ShowWindow(m_curwnd,SW_SHOWNA);
      }
      break;
    default:
      EndDialog(hwndDlg, 0);
      PostQuitMessage(0);
      break;
    }
    break;
    }
    return FALSE;
}

NSIS_ENTRYPOINT_SIMPLEGUI
int WINAPI _tWinMain(HINSTANCE hInst,HINSTANCE hOldInst,LPTSTR CmdLineParams,int ShowCmd)
{
  InitCommonControls();
  g_hInstance = hInst;
  LoadLibrary(_T("RichEd32.dll"));
  return (int) DialogBox(g_hInstance,MAKEINTRESOURCE(IDD_INST),0,DialogProc);
}

