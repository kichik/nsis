#pragma once

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



// only include this file from one place in your DLL.
// (it is all static, if you use it in two places it will fail)

#define Math_INIT()           {  \
        g_stringsize=string_size; \
        g_stacktop=stacktop;      \
        g_variables=variables; }

// For page showing plug-ins
#define WM_NOTIFY_OUTER_NEXT (WM_USER+0x8)
#define WM_NOTIFY_CUSTOM_READY (WM_USER+0xd)
#define NOTIFY_BYE_BYE 'x'

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

extern unsigned int g_stringsize;
extern stack_t **g_stacktop;
extern char *g_variables;

enum
{
INST_0,         // $0
INST_1,         // $1
INST_2,         // $2
INST_3,         // $3
INST_4,         // $4
INST_5,         // $5
INST_6,         // $6
INST_7,         // $7
INST_8,         // $8
INST_9,         // $9
INST_R0,        // $R0
INST_R1,        // $R1
INST_R2,        // $R2
INST_R3,        // $R3
INST_R4,        // $R4
INST_R5,        // $R5
INST_R6,        // $R6
INST_R7,        // $R7
INST_R8,        // $R8
INST_R9,        // $R9
INST_CMDLINE,   // $CMDLINE
INST_INSTDIR,   // $INSTDIR
INST_OUTDIR,    // $OUTDIR
INST_EXEDIR,    // $EXEDIR
INST_LANG,      // $LANGUAGE
__INST_LAST
};


// utility functions (not required but often useful)
int popstring(char *str);
void pushstring(char *str);
char *getuservariable(int varnum);
void setuservariable(int varnum, char *var);
char *AllocString();
ExpressionItem *AllocItem();
ExpressionItem *AllocArray(int size);
ExpressionItem *CopyItem(ExpressionItem *item, int NeedConst = 0);
