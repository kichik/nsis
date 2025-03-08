// Unicode support by Jim Park -- 08/22/2007

#define WIN32_LEAN_AND_MEAN 
#include <windows.h>
#include <wininet.h>

#include <nsis/pluginapi.h> // nsis plugin

#define NSISFunction(funcname) void __declspec(dllexport) funcname(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop, extra_parameters *extra)

BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  return TRUE;
}

/*************\
 *   LOADER  *
\*************/

HMODULE NSISCALL LoadSystemLibrary(LPCSTR name) {
  LPCTSTR fmt = sizeof(*fmt) > 1 ? TEXT("%s%S.dll") : TEXT("%s%s.dll"); // The module name is always ANSI
  BYTE bytebuf[(MAX_PATH+1+20+1+3+!0) * sizeof(*fmt)]; // 20+4 is more than enough for 
  LPTSTR path = (LPTSTR) bytebuf;                      // the dllnames we are using.

  UINT cch = GetSystemDirectory(path, MAX_PATH);
  if (cch > MAX_PATH) // MAX_PATH was somehow not large enough and we don't support 
    cch = 0;          // \\?\ paths so we have to settle for just the name.
  wsprintf(path + cch, fmt, TEXT("\\") + (!cch || path[cch-1] == '\\'), name);

  return LoadLibrary(path);
}

FARPROC GetWinInetFunc(LPCSTR funcname) {
  HMODULE hWinInet = LoadSystemLibrary("WININET");
  return hWinInet ? GetProcAddress(hWinInet, funcname) : (FARPROC) hWinInet;
}


/*************\
 * FUNCTIONS *
\*************/

NSISFunction(AutodialOnline) {
  typedef BOOL (WINAPI *fInternetAutodial)(DWORD, HWND);
  fInternetAutodial pInternetAutodial = (fInternetAutodial) GetWinInetFunc("InternetAutodial");
  if (!pInternetAutodial) {
    extra->exec_flags->exec_error++;
    return;
  }

  EXDLL_INIT();

  if (pInternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE, 0))
    pushstring(_T("online"));
  else
    pushstring(_T("offline"));
}

NSISFunction(AutodialUnattended) {
  typedef BOOL (WINAPI *fInternetAutodial)(DWORD, HWND);
  fInternetAutodial pInternetAutodial = (fInternetAutodial) GetWinInetFunc("InternetAutodial");
  if (!pInternetAutodial) {
    extra->exec_flags->exec_error++;
    return;
  }

  EXDLL_INIT();

  if (pInternetAutodial(INTERNET_AUTODIAL_FORCE_UNATTENDED , 0))
    pushstring(_T("online"));
  else
    pushstring(_T("offline"));
}

NSISFunction(AttemptConnect) {
  typedef DWORD (WINAPI *fAttemptConn)(DWORD);
  fAttemptConn pInternetAttemptConnect = (fAttemptConn) GetWinInetFunc("InternetAttemptConnect");
  if (!pInternetAttemptConnect) {
    extra->exec_flags->exec_error++;
    return;
  }

  EXDLL_INIT();

  if (pInternetAttemptConnect(0) == ERROR_SUCCESS)
    pushstring(_T("online"));
  else
    pushstring(_T("offline"));
}

NSISFunction(GetConnectedState) {
  DWORD dwState;

  typedef BOOL (WINAPI *fGetConState)(LPDWORD, DWORD);
  fGetConState pInternetGetConnectedState = (fGetConState) GetWinInetFunc("InternetGetConnectedState");
  if (!pInternetGetConnectedState) {
    extra->exec_flags->exec_error++;
    return;
  }

  EXDLL_INIT();

  if (pInternetGetConnectedState(&dwState, 0))
    pushstring(_T("online"));
  else
    pushstring(_T("offline"));
}

NSISFunction(AutodialHangup) {
  typedef BOOL (WINAPI *fAutodial)(DWORD);
  fAutodial pInternetAutodialHangup = (fAutodial) GetWinInetFunc("InternetAutodialHangup");
  if (!pInternetAutodialHangup) {
    extra->exec_flags->exec_error++;
    return;
  }

  EXDLL_INIT();

  if (pInternetAutodialHangup(0))
    pushstring(_T("success"));
  else
    pushstring(_T("failure"));
}
