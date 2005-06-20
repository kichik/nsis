#define WIN32_LEAN_AND_MEAN 
#include <windows.h>
#include <wininet.h>

#include "../ExDLL/exdll.h"

#define NSISFunction(funcname) void __declspec(dllexport) funcname(HWND hwndParent, int string_size, char *variables, stack_t **stacktop, extra_parameters *extra)

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
	return TRUE;
}

/*************\
 *   LOADER  *
\*************/

HMODULE hWinInet = NULL;

FARPROC GetWinInetFunc(char *func) {
  hWinInet = LoadLibrary("wininet.dll");
  if (hWinInet)
    return GetProcAddress(hWinInet, func);
  return NULL;
}

void FreeWinInet() {
  if (hWinInet)
    FreeLibrary(hWinInet);
  hWinInet = NULL;
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
		pushstring("online");
	else
		pushstring("offline");

  FreeWinInet();
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
		pushstring("online");
	else
		pushstring("offline");

  FreeWinInet();
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
		pushstring("online");
	else
		pushstring("offline");

  FreeWinInet();
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
		pushstring("online");
	else
		pushstring("offline");

  FreeWinInet();
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
		pushstring("success");
	else
		pushstring("failure");

  FreeWinInet();
}
