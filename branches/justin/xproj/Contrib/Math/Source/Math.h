#pragma once

#include <nsis/pluginapi.h> // nsis plugin

#ifdef _DEBUG
//#define _DEBUG_LEAKS
#endif

#ifdef _DEBUG_LEAKS

#define dbgGlobalAlloc(a, b) watchGlobalAlloc(a, b)
#define dbgGlobalFree(a) watchGlobalFree(a)
#define dbgGlobalCheck() watchGlobal();
void watchGlobal();
void watchGlobalFree(HGLOBAL block);
HGLOBAL watchGlobalAlloc(UINT Flags, UINT size);

#else

#define dbgGlobalAlloc(a, b) GlobalAlloc(a, b)
#define dbgGlobalFree(a) GlobalFree(a)
#define dbgGlobalCheck() {};

#endif

char *AllocString();
ExpressionItem *AllocItem();
ExpressionItem *AllocArray(int size);
ExpressionItem *CopyItem(ExpressionItem *item, int NeedConst = 0);
