#define WIN32_LEAN_AND_MEAN 
#include <windows.h>
#include <WinInet.h>

typedef struct _stack_t {
	struct _stack_t *next;
	char text[1]; // this should be the length of string_size
} stack_t;

int popstring(char *str); // 0 on success, 1 on empty stack
void pushstring(char *str);

HWND g_hwndParent;
int g_stringsize;
stack_t **g_stacktop;
char *g_variables;

#define NSISFunction(funcname) void __declspec(dllexport) funcname(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
#define NSISGetVars g_hwndParent=hwndParent;g_stringsize=string_size;g_stacktop=stacktop;g_variables=variables

/****************\
 * LOOK HERE    *
\****************/

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

/****************\
 * STOP LOOKING *
\****************/

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
	return TRUE;
}

int popstring(char *str) {
	stack_t *th;
	if (!g_stacktop || !*g_stacktop) return 1;
	th=(*g_stacktop);
	lstrcpy(str,th->text);
	*g_stacktop = th->next;
	GlobalFree((HGLOBAL)th);
	return 0;
}

void pushstring(char *str) {
	stack_t *th;
	if (!g_stacktop) return;
	th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
	lstrcpyn(th->text,str,g_stringsize);
	th->next=*g_stacktop;
	*g_stacktop=th;
}