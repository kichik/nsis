// ui.cpp : Defines the entry point for the application.
//

#include <windows.h>
#include <commctrl.h>
#include "resource.h"

HINSTANCE g_hInstance;
HWND m_curwnd;

char* windows[] = {
  MAKEINTRESOURCE(IDD_LICENSE),
  MAKEINTRESOURCE(IDD_SELCOM),
  MAKEINTRESOURCE(IDD_DIR),
  MAKEINTRESOURCE(IDD_INSTFILES),
  MAKEINTRESOURCE(IDD_UNINST)
};

BOOL CALLBACK GenericProc(HWND hwndDlg,UINT uMsg,WPARAM wParam,LPARAM lParam) {
  static LOGBRUSH b = {BS_SOLID, RGB(255,0,0), 0};
  static HBRUSH red = CreateBrushIndirect(&b);
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
		SetWindowText(hwndDlg, "NSIS User Interface - Testing");
		SetWindowText(GetDlgItem(hwndDlg, IDC_VERSTR), "NSIS version");
		SetWindowText(GetDlgItem(hwndDlg, IDC_BACK), "< Back");
		SetWindowText(GetDlgItem(hwndDlg, IDOK), "Next >");
		SetWindowText(GetDlgItem(hwndDlg, IDCANCEL), "Cancel");
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
      if (i >= (int)sizeof(windows)/sizeof(char*)) {
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

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
  InitCommonControls();

  LoadLibrary("RichEd32.dll");

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
