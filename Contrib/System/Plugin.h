#pragma once

typedef struct _stack_t {
  struct _stack_t *next;
  char text[1]; // this should be the length of string_size
} stack_t;

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

#define PLUGINFUNCTION(name) void __declspec(dllexport) name(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) { \
/*  g_hwndParent=hwndParent; */\
  g_stringsize=string_size; \
  g_stacktop=stacktop; \
  g_variables=variables; 
#define PLUGINFUNCTIONEND }

#define PLUGINFUNCTIONSHORT(name) void __declspec(dllexport) name(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) { \
  g_stringsize=string_size; \
  g_stacktop=stacktop; 
#define PLUGINFUNCTIONEND }

extern char *AllocStr(char *str);
extern void myitoa64(__int64 i, char *buffer);
extern char *AllocString();
extern char *getuservariable(int varnum);
extern void setuservariable(int varnum, char *var);
extern char* popstring();  // NULL - stack empty
extern char* pushstring(char *str);
extern __int64 myatoi(char *s);
extern int popint();  // -1 -> stack empty
extern void pushint(int value);

extern HWND g_hwndParent;
extern int g_stringsize;
extern stack_t **g_stacktop;
extern char *g_variables;
