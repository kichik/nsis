#ifndef ___PLUGIN__H___
#define ___PLUGIN__H___

#include <nsis/pluginapi.h> // nsis plugin

// Always use system* functions to keep the size down
#define pushstring error(use system_pushstring)
#define pushint error(use system_pushint)

#define popint system_popint
#define myatoi(str) ( (int) myatoi64(str) ) 
#ifdef _WIN64
#	error TODO
#else
#	define system_pushintptr system_pushint
#	define popintptr popint
#	define StrToIntPtr(str) ( (INT_PTR)myatoi((str)) )
#endif

#define BUGBUG64(brokencast) (brokencast) // Cast that needs fixing on x64

#define PLUGINFUNCTION(name) \
  void __declspec(dllexport) name( \
    HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop, extra_parameters *extra) { \
  /*g_hwndParent=hwndParent;*/ \
  EXDLL_INIT(); \
  extra->RegisterPluginCallback(g_hInstance, NSISCallback);
#define PLUGINFUNCTIONEND }

#define PLUGINFUNCTIONSHORT(name) void __declspec(dllexport) name(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop) { \
  g_stringsize=string_size; \
  g_stacktop=stacktop; 

extern TCHAR *AllocStr(TCHAR *str);
extern void myitoa64(__int64 i, TCHAR *buffer);
extern TCHAR *AllocString();
extern TCHAR *system_getuservariable(int varnum);
extern TCHAR *system_setuservariable(int varnum, TCHAR *var);
extern TCHAR* system_popstring();  // NULL - stack empty
extern TCHAR* system_pushstring(TCHAR *str);
extern __int64 myatoi64(TCHAR *s);
extern int system_popint();  // -1 -> stack empty
extern void system_pushint(int value);

extern HANDLE GlobalCopy(HANDLE Old);
extern void *copymem(void *output, void *input, size_t cbSize);

extern UINT_PTR NSISCallback(enum NSPIM);

extern HWND g_hwndParent;
extern HINSTANCE g_hInstance;

#endif
