#define WIN32_LEAN_AND_MEAN 
#include <windows.h>
#include <WinInet.h>

#include "../exdll/exdll.h"

HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

#define NSISFunction(funcname) void __declspec(dllexport) funcname(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
#define NSISGetVars g_hwndParent=hwndParent;g_stringsize=string_size;g_stacktop=stacktop;g_variables=variables

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
	return TRUE;
}

/*************\
 * FUNCTIONS *
\*************/

NSISFunction(AutodialOnline) {
	NSISGetVars;
	if (InternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE, 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AutodialUnattended) {
	NSISGetVars;
	if (InternetAutodial(INTERNET_AUTODIAL_FORCE_UNATTENDED , 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AttemptConnect) {
	NSISGetVars;
	if (InternetAttemptConnect(0) == ERROR_SUCCESS)
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(GetConnectedState) {
	DWORD dwState;
	NSISGetVars;

	if (InternetGetConnectedState(&dwState, 0))
		pushstring("online");
	else
		pushstring("offline");
}

NSISFunction(AutodialHangup) {
	NSISGetVars;
	if (InternetAutodialHangup(0))
		pushstring("success");
	else
		pushstring("failure");

}