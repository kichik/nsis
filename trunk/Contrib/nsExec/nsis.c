#include <windows.h>
#include "nsis.h"

int popstring(char *str) {
	stack_t *th;
	if (!g_stacktop||!*g_stacktop) return 1;
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

char *getuservariable(int varnum) {
	if (varnum<0||varnum >= __INST_LAST) return NULL;
	return g_variables+varnum*g_stringsize;
}
