#define WIN32_LEAN_AND_MEAN 
#include <windows.h>
#include <wininet.h>

#include "../ExDLL/exdll.h"

#define NSISFunction(funcname) void __declspec(dllexport) funcname(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)

HMODULE hWinInet;

BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
	return TRUE;
}

/*************\
 * FUNCTIONS *
\*************/

NSISFunction(AutodialOnline) {
	EXDLL_INIT();
	if (InternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE, 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AutodialUnattended) {
	EXDLL_INIT();
	if (InternetAutodial(INTERNET_AUTODIAL_FORCE_UNATTENDED , 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AttemptConnect) {
	EXDLL_INIT();
	if (InternetAttemptConnect(0) == ERROR_SUCCESS)
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(GetConnectedState) {
	DWORD dwState;
	EXDLL_INIT();

	if (InternetGetConnectedState(&dwState, 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AutodialHangup) {
	EXDLL_INIT();
	if (InternetAutodialHangup(0))
		pushstring("success");
	else
		pushstring("failure");

}
