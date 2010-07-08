// ui.cpp : Defines the entry point for the application.
//
// Unicode support by Jim Park -- 08/10/2007

#include <windows.h>
#include <commctrl.h>
#include "../ExDLL/nsis_tchar.h"
#include "resource.h"

HINSTANCE g_hInstance;
HWND m_curwnd;

TCHAR* windows[] = {
  MAKEINTRESOURCE(IDD_LICENSE),
  MAKEINTRESOURCE(IDD_SELCOM),
  MAKEINTRESOURCE(IDD_DIR),
  MAKEINTRESOURCE(IDD_INSTFILES),
  MAKEINTRESOURCE(IDD_UNINST)
};

BOOL CALLBACK GenericProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam) {
  static LOGBRUSH b = {BS_SOLID, RGB(255,0,0), 0};
  static HBRUSH red;

  if (!red)
    red = CreateBrushIndirect(&b);

  switch (uMsg) {
    case WM_CTLCOLORSTATIC:
      return (int)red;
  }
  return 0;
}

BOOL CALLBACK DialogProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam) {
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
	return 0;
}

int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
  InitCommonControls();

  LoadLibrary(_T("RichEd32.dll"));

  g_hInstance = GetModuleHandle(0);

	DialogBox(
		GetModuleHandle(0),
		MAKEINTRESOURCE(IDD_INST),
		0,
		DialogProc
	);

	ExitProcess(0);

	return 0;
}
