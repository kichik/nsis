// System.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "Plugin.h"
#include "System.h"

HINSTANCE g_hInstance;

#define PT_STRING_SIZE 2048
#define PARAMOKCHECK(expr) if (expr) { pushint(0); return; }

PLUGINFUNCTION(PartAddr)
	char buffer[1024];
	SystemProc *proc;

	// Retrieve ProcID
	PARAMOKCHECK(popstring(buffer) == 0);
	// Try to initialize proc ...
	proc = ParseProc(buffer);
	// ... and return it to nsis!
	pushint((int) proc);
PLUGINFUNCTIONEND

PLUGINFUNCTION(FullAddr)
	char procid[1024], paramid[1024];
	SystemProc *proc;

	// Retrieve ProcID and ParamId
	PARAMOKCHECK((popstring(procid) == 0) || (popstring(paramid) == 0))
	// Try to initialize proc ...
	proc = ParseProc(procid);
	PARAMOKCHECK(proc == NULL)
	// Try to initialize params ...
	PARAMOKCHECK(ParseParam(proc, paramid) == FALSE)
	// ... and return it to nsis!
	pushint((int) proc);
PLUGINFUNCTIONEND

PLUGINFUNCTION(ShortAddr)
	char paramid[1024];
	SystemProc *proc;

	// Retrieve Proc and ParamId
	PARAMOKCHECK((popint(&((int)proc)) == 0) || (popstring(paramid) == 0))
	// Try to initialize params ...
	PARAMOKCHECK(ParseParam(proc, paramid) == FALSE)
	// ... and return it to nsis!
	pushint((int) proc);
PLUGINFUNCTIONEND

PLUGINFUNCTION(Call)
	SystemProc *proc;

	// Retrieve Proc
	PARAMOKCHECK(popint(&((int)proc)) == 0)
	
	// Run the proc
	SystemCall(proc);
PLUGINFUNCTIONEND

PLUGINFUNCTION(ShortCall)
	char paramid[1024];
	SystemProc *proc;

	// Retrieve Proc and ParamId
	PARAMOKCHECK((popint(&((int)proc)) == 0) || (popstring(paramid) == 0))
	// Try to initialize params ...
	PARAMOKCHECK(ParseParam(proc, paramid) == FALSE)
	
	// Run the proc
	SystemCall(proc);
PLUGINFUNCTIONEND

PLUGINFUNCTION(FullCall)
	char procid[1024], paramid[1024];
	SystemProc *proc;

	// Retrieve ProcID and ParamId
	PARAMOKCHECK((popstring(procid) == 0) || (popstring(paramid) == 0))
	// Try to initialize proc ...
	proc = ParseProc(procid);
	PARAMOKCHECK(proc == NULL)
	// Try to initialize params ...
	PARAMOKCHECK(ParseParam(proc, paramid) == FALSE)
	// ... and return it to nsis!
	
	// Run the proc
	SystemCall(proc);

	// We've created it, we've to destroyit
	GlobalFree(proc);
PLUGINFUNCTIONEND

SystemProc *ParseProc(char *ProcID)
{
	SystemProc *proc;
	char dllname[1024], procname[256], *p1, *p2;

	// Extract dllname
	p1 = ProcID;
	p2 = dllname;
	while (*p1 && (*p1 != '?')) *(p2++) = *(p1++);
	*p2 = 0;
	if ((lstrlen(dllname) == 0) || (*p1 == 0)) return NULL;

	// Extract procname
	p1++; 
	p2 = procname;
	while (*p1 && (*p1 != '?')) *(p2++) = *(p1++);
	*p2 = 0;
	if ((lstrlen(procname) == 0) || (*p1 == 0)) return NULL;

	// Ok, check If there is at least 1 param
	p1++;
	if (*p1 == 0) return NULL;

	// Allocate memory for new Proc
	proc = (SystemProc*) GlobalAlloc(GPTR, sizeof(SystemProc));

	// Ok, retrieve dll handle
	proc->dll = GetModuleHandle(dllname);
	if (proc->dll == NULL) 
	{
		// Dll is not loaded already
		proc->dll = LoadLibrary(dllname);
		if (proc->dll == NULL)
		{
			// Dll not found
			GlobalFree(proc);
			return NULL;
		}
	}

	// Dll succesfully loaded, now we should Get Proc Address
	proc->proc = GetProcAddress(proc->dll, procname);
	if (proc->proc == NULL)
	{
		// Proc is not loaded succesfully
		GlobalFree(proc);
		return NULL;
	}

	// Allright, lets parse parameters
	proc->ParamCount = 0; 
	while (*p1 && (*p1 != '?'))
	{
		// Memory is initialized to zeros with GlobalAlloc, so we don't need
		// to set defaults for Parameters
		
		// We should check for pointer
		if ((*p1 == 'p') || (*p1 == 'P')) 
		{
			proc->Params[proc->ParamCount].IsPointer = TRUE;
			// Check for next character to be valid
			p1++;
			if ((*p1 == 0) || (*p1 == '?')) break;
		}

		switch(*p1)
		{
		case 'v':
		case 'V': 
			proc->Params[proc->ParamCount].Type = PT_VOID;
			break;
		case 'i':
		case 'I':
			proc->Params[proc->ParamCount].Type = PT_INT; 
			break;
		case 'l':
		case 'L':
			proc->Params[proc->ParamCount].Type = PT_LONG; 
			break;
		case 's':
		case 'S':
			proc->Params[proc->ParamCount].Type = PT_STRING; 
			break;
		case 'b':
		case 'B':
			proc->Params[proc->ParamCount].Type = PT_BOOLEAN; 
			break;
		};

		// Move to next character
		proc->ParamCount++;
		p1++;
	}
	return proc;
}

BOOL ParseParam(SystemProc *proc, char *ParamID)
{
	char *p1;
	ProcParameter *par;
	
	par = proc->Params + 1;
	p1 = ParamID;

	// Allright, lets parse input parameters
	while (*p1 && (*p1 != '?'))
	{
		if ((*p1 == 's') || (*p1 == 'S')) par->Input = IOT_STACK;
		else if ((*p1 == 'n') || (*p1 == 'N')) par->Input = IOT_NONE;
		else if ((*p1 >= '0') && (*p1 <= '9')) par->Input = (*p1-'0')+1;
		else if ((*p1 >= 'a') && (*p1 <= 'o')) par->Input = (*p1-'a')+11;
		else if ((*p1 >= 'A') && (*p1 <= 'O')) par->Input = (*p1-'A')+11;

		// Move to next param & character
		par++;
		p1++;
	}

	if (*p1++ == 0) return TRUE;
	par = proc->Params;

	// Allright, lets parse output parameters
	while (*p1)
	{
		if ((*p1 == 's') || (*p1 == 'S')) par->Output = IOT_STACK;
		else if ((*p1 == 'n') || (*p1 == 'N')) par->Output = IOT_NONE;
		else if ((*p1 >= '0') && (*p1 <= '9')) par->Output = (*p1-'0')+1;
		else if ((*p1 >= 'a') && (*p1 <= 'o')) par->Output = (*p1-'a')+11;
		else if ((*p1 >= 'A') && (*p1 <= 'O')) par->Output = (*p1-'A')+11;

		// Move to next param & character
		par++;
		p1++;
	}

	return TRUE;
}

void ParamsInput(SystemProc *proc)
{
	int i;
	ProcParameter *par;
	char buffer[PT_STRING_SIZE], *realbuf;

	par = proc->Params + 1;
	for (i = 1; i < proc->ParamCount; i++, par++)
	{
		// Step 1: retrive value
		if (par->Input == IOT_STACK) popstring(realbuf = buffer);
		else if (par->Input > 0) realbuf = getuservariable(par->Input - 1);
		else *(realbuf = buffer) = 0;

		// Step 2: place it
		switch (par->Type)
		{
		// TODO: PT_VOID input????
		//case PT_VOID:
		case PT_INT:
			{
				if (par->IsPointer) 
				{
					par->Value = (int) GlobalAlloc(GPTR, sizeof(int));
					*((int*) par->Value) = myatoi(realbuf);
				} else par->Value = myatoi(realbuf);
			}
			break;
		case PT_LONG:
			{
				if (par->IsPointer)
				{
					par->Value = (int) GlobalAlloc(GPTR, sizeof(ULARGE_INTEGER));
					((ULARGE_INTEGER*) par->Value)->LowPart = myatoi(realbuf);
					((ULARGE_INTEGER*) par->Value)->HighPart = 0; // TODO: 64 bit atoi conversion
				} else; // TODO: PT_LONG direct input????
			}
			break;
		case PT_STRING:
			{
				if (par->IsPointer)
				{
					par->Value = (int) GlobalAlloc(GPTR, sizeof(int));
					*((int*)par->Value) = GlobalAlloc(GPTR, PT_STRING_SIZE);
					lstrcpy(*((LPCSTR*) par->Value), realbuf);
				} else
				{
					par->Value = (int) GlobalAlloc(GPTR, PT_STRING_SIZE);
					lstrcpy(par->Value, realbuf);
				}
			}
			break;
		// TODO: PT_BOOLEAN support ???
		//case PT_BOLEAN: ;
		}
	}
}

void ParamsOutput(SystemProc *proc)
{
	int i;
	ProcParameter *par;
	char buffer[PT_STRING_SIZE];

	par = proc->Params;
	for (i = 0; i < proc->ParamCount; i++, par++)
	{
		// Step 1: retrieve value
		switch (par->Type)
		{
		case PT_VOID:
			{
				if (par->IsPointer); // TODO: Pointer To Void Output
				else *buffer = 0;
			}
			break;
		case PT_INT:
			{
				if (par->IsPointer) 
				{
					wsprintf(buffer, "%d", *((int*) par->Value));
					GlobalFree(par->Value);
				} else wsprintf(buffer, "%d", par->Value);
			}
			break;
		case PT_LONG:
			{
				if (par->IsPointer)
				{
					wsprintf(buffer, "%ld", *((ULARGE_INTEGER*) par->Value));
					GlobalFree(par->Value);
				} else; // TODO: PT_LONG direct output????
			}
			break;
		case PT_STRING:
			{
				if (par->IsPointer)
				{
					lstrcpy(buffer, *((LPCSTR*) par->Value));
					GlobalFree(*((int*)par->Value));
					GlobalFree(par->Value);
				} else
				{
					lstrcpy(buffer, par->Value);
					GlobalFree((HANDLE) par->Value);
				}
			}
			break;
		// TODO: PT_BOOLEAN support ???
		//case PT_BOLEAN: ;
		}

		// Step 2: place it
		if (par->Output == IOT_STACK) pushstring(buffer);
		else if (par->Output > 0) setuservariable(par->Output - 1, buffer);
	}
}

void SystemCall(SystemProc *proc)
{
	int z;
	ProcParameter *par;

	// Prepare input arguments
	ParamsInput(proc);
	
	// Push arguments to stack
	par = proc->Params+proc->ParamCount-1;
	while (par > proc->Params)
	{
		z = par->Value;
		_asm
		{
			push	z
		}		
		par--;
	}

	// Call the proc and save return
	z = proc->proc;
	_asm
	{
		call	z
		mov		z, eax
	}
	proc->Params[0].Value = z;

	// Prepare output parameters
	ParamsOutput(proc);
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
        g_hInstance=hInst;

        return TRUE;
}

