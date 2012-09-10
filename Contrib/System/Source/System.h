#ifndef ___SYSTEM__H___
#define ___SYSTEM__H___

// This should probably be moved to platform.h at some point

#if defined(_M_X64) || defined(__amd64__)
#    define SYSTEM_X64
#elif defined(_M_IX86) || defined(__i386__) || defined(_X86_)
#    define SYSTEM_X86
#else
#    error "Unknown architecture!"
#endif



// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the SYSTEM_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// SYSTEM_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.

#ifdef SYSTEM_EXPORTS
#define SYSTEM_API __declspec(dllexport)
#else
#define SYSTEM_API __declspec(dllimport)
#endif

#define NEW_STACK_SIZE     256*256

// Proc types:
#define PT_NOTHING      0
#define PT_PROC         1
#define PT_STRUCT       2
#define PT_VTABLEPROC   3

// Proc results:
#define PR_OK           0
#define PR_ERROR        -1
#define PR_CALLBACK     1

// Real world argument types
#define PAT_VOID        0
#define PAT_INT         1
#define PAT_LONG        2
#define PAT_STRING      3
#define PAT_WSTRING     4
#define PAT_GUID        5
#define PAT_CALLBACK    6
#ifdef _UNICODE
#define PAT_TSTRING     PAT_WSTRING
#else
#define PAT_TSTRING     PAT_STRING
#endif
#define PAT_PTR ( (4==sizeof(void*)) ? PAT_INT : PAT_LONG )

// Input/Output Source/Destination
#define IOT_NONE    0
#define IOT_STACK   -1
#define IOT_REG     1
#define IOT_INLINE  (__INST_LAST+1) // should replace pointer to inline input
// #define INLINE_INPUT -> any other value, will contain pointer to input string

// Options
#define POPT_CDECL      0x1    // (Option & 0x1) == 0x1 -> cdecl, otherwise stdcall
#define POPT_PERMANENT  0x2    // Permanent proc, will not be destroyed after calling
#define POPT_ALWRETURN  0x4    // Always return
#define POPT_NEVERREDEF 0x8    // Never redefine
#define POPT_GENSTACK   0x10   // Use general stack (non temporary for callback)
#define POPT_ERROR      0x20   // Call GetLastError after proc and push it to stack
#define POPT_UNLOAD     0x40   // unload dll after call
#define POPT_CLONE      0x80   // This is clone callback

// Our single proc parameter
typedef struct
{
    int Type;
    int Option; // -1 -> Pointer, 1-... -> Special+1
    int Value;  // it can hold any 4 byte value BUGBUG: What about pointers on Win64?
    int _value; // value buffer for structures > 4 bytes (I hope 8 bytes will be enough)
    int Size; // Value real size (should be either 1 or 2 (the number of pushes))
    int Input; //BUGBUG: What about pointers on Win64?
    int Output;
    HGLOBAL allocatedBlock; // block allocated for passing string, wstring or guid param
} ProcParameter;

// Our single proc (Since the user will free proc with GlobalFree, 
// I've declared all variables as statics)
typedef struct tag_SystemProc SystemProc;
struct tag_SystemProc
{
    int ProcType;
    int ProcResult;
    TCHAR DllName[1024];
    TCHAR ProcName[1024];
    HMODULE Dll;
    HANDLE  Proc;
    int Options;
    int ParamCount;
    ProcParameter Params[100];  // I hope nobody will use more than 100 params

    // Callback specific
    int CallbackIndex;
    int ArgsSize;
    // Clone of current element (used for multi-level callbacks)
    SystemProc *Clone;
};

typedef struct tag_CallbackThunk CallbackThunk;
struct tag_CallbackThunk
{
    #ifdef SYSTEM_X86
        /*
        #pragma pack(push,1)
        char mov_eax_imm;
        int sysprocptr;
        char reljmp_imm;
        int realprocaddr;
        #pragma pack(pop)
        */
        char asm_code[10];
    #else
        #error "Asm thunk not implemeted for this architecture!"
    #endif

    CallbackThunk* pNext;
};

// Free() only knows about pNext in CallbackThunk, it does not know anything about the assembly, that is where this helper comes in...
#ifdef SYSTEM_X86
#   define GetAssociatedSysProcFromCallbackThunkPtr(pCbT) ( (SystemProc*)  *(unsigned int*) (((char*)(pCbT))+1) )
#else
#   error "GetAssociatedSysProcFromCallbackThunkPtr not defined for the current architecture!"
#endif


extern const int ParamSizeByType[];   // Size of every parameter type (*4 bytes)

extern HANDLE CreateCallback(SystemProc *cbproc);
extern SystemProc *PrepareProc(BOOL NeedForCall);
extern void ParamAllocate(SystemProc *proc);
extern void ParamsDeAllocate(SystemProc *proc);
extern void ParamsIn(SystemProc *proc);
extern void ParamsOut(SystemProc *proc);
extern SystemProc *CallProc(SystemProc *proc);
extern SystemProc *CallBack(SystemProc *proc);
extern SystemProc *RealCallBack();
extern void CallStruct(SystemProc *proc);

#ifdef _UNICODE
#   define STRSET2CH(str, c1, c2) ( *(DWORD*)(str) = ((c1)|(c2)<<16) )
#else
#   define STRSET2CH(str, c1, c2) ( *(WORD*)(str) = ((c1)|(c2)<<8) )
#endif

#endif
