// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the SYSTEM_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// SYSTEM_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#pragma once

#ifdef SYSTEM_EXPORTS
#define SYSTEM_API __declspec(dllexport)
#else
#define SYSTEM_API __declspec(dllimport)
#endif

// Real world types
#define	PT_VOID		0
#define PT_INT		1
#define	PT_LONG		2
#define PT_STRING	3
#define PT_BOOLEAN	4

// Input/Output Source/Destination
#define	IOT_NONE	0
#define	IOT_STACK	-1
#define	IOT_REG		1

// Our single proc parameter
typedef struct
{
	int Type;
	BOOL IsPointer;
	int Value;	// it can hold any value
    int Input;
	int Output;
} ProcParameter;

// Our single proc
typedef struct 
{
	HANDLE dll;
	HANDLE proc;
	int ParamCount;
	ProcParameter Params[20];	// I hope nobody will use more than 20 params
} SystemProc;

extern SystemProc *ParseProc(char *ProcID);
extern BOOL ParseParam(SystemProc *proc, char *ParamID);
extern void ParamsInput(SystemProc *proc);
extern void ParamsOutput(SystemProc *proc);
extern void SystemCall(SystemProc *proc);
