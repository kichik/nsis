#ifndef ___PLUGIN__H___
#define ___PLUGIN__H___

#include <nsis/pluginapi.h> // nsis plugin

#define PLUGINFUNCTION(name) \
  void __declspec(dllexport) name( \
    HWND hwndParent, int string_size, char *variables, stack_t **stacktop, extra_parameters *extra) { \
  /*g_hwndParent=hwndParent;*/ \
  EXDLL_INIT(); \
  extra->RegisterPluginCallback(g_hInstance, NSISCallback);
#define PLUGINFUNCTIONEND }

#define PLUGINFUNCTIONSHORT(name) void __declspec(dllexport) name(HWND hwndParent, int string_size, char *variables, stack_t **stacktop) { \
  g_stringsize=string_size; \
  g_stacktop=stacktop; 

extern char *AllocStr(char *str);
extern void myitoa64(__int64 i, char *buffer);
extern char *AllocString();
extern char *system_getuservariable(int varnum);
extern char *system_setuservariable(int varnum, char *var);
extern char* system_popstring();  // NULL - stack empty
extern char* system_pushstring(char *str);
extern __int64 myatoi64(char *s);
extern int popint64();  // -1 -> stack empty
extern void system_pushint(int value);

extern HANDLE GlobalCopy(HANDLE Old);
extern char *copymem(char *output, char *input, int size);

extern UINT_PTR NSISCallback(enum NSPIM);

extern HWND g_hwndParent;
extern HINSTANCE g_hInstance;

#endif
