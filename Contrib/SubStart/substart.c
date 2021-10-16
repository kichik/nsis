/*
 * substart.c - This app runs the executable of the same name in the 'Bin' 
 *              sub-folder and passes along the command line options.
 *
 * Copyright (c) 2013-2021 Anders Kjersem
 *
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 *
 * Licence details can be found in the file COPYING.
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include <windows.h>
#include <tchar.h>

#define SUBFOLDER _T("Bin")

/*
 * Leaking things Windows is going to clean up after us anyway avoids 
 * linking to a couple of functions, this saves just enough space 
 * to get the file size down to 2 KiB (MSVC 7.1)
 */
#ifdef _DEBUG
#	define CANLEAK(cod) cod
#else
#	define CANLEAK(cod)
#endif

#define MemFree LocalFree
static void* MemReAlloc(void*OrgMem, size_t cb)
{
	if (!OrgMem) return LocalAlloc(LMEM_FIXED, cb);
	return LocalReAlloc(OrgMem, cb, LMEM_MOVEABLE);
}
static void MemZero(void*pMem, size_t cb)
{
	char*p=(char*)pMem;
	for(; cb;) p[--cb] = 0;
}

static UINT SIOPut(LPCSTR Str, UINT cch)
{
	HANDLE hSO = GetStdHandle(STD_OUTPUT_HANDLE);
	DWORD cbio;
	return WriteFile(hSO, Str, cch, &cbio, 0) ? cch : 0;
}
static UINT SIOFmtPut(LPCSTR Fmt, ...)
{
	UINT cch;
	char buf[150]; // Plenty for our simple strings
	va_list val;
	va_start(val, Fmt);
	cch = wvsprintfA(buf, Fmt, val);
	cch = SIOPut(buf, cch);
	va_end(val);
	return cch;
}

void mainCRTStartup()
{
	const UINT cchSubDir = (sizeof(SUBFOLDER) / sizeof(TCHAR)) - 1;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	UINT ec, cch, cchParams;
	TCHAR *p = GetCommandLine(), *cmd = 0;

	if (*p == _T('\"'))
		do ++p; while(*p && *p != _T('\"'));
	else
		while(*p && *p > _T(' ')) ++p;

	/* Skip end quote and whitespace */
	do if (!*p) break; else ++p; while(*p <= ' ');

	ec = ERROR_OUTOFMEMORY;
	cchParams = lstrlen(p), cch = MAX_PATH;
	for (;;)
	{
		TCHAR *mem;
		UINT cchTot = 1 + cch + 1 + cchSubDir + 2 + cchParams + 1, cchSelf;
		mem = (TCHAR*) MemReAlloc(cmd, cchTot * sizeof(TCHAR));
		if (!mem) goto app_die;
		cmd = mem;
		cchSelf = GetModuleFileName(NULL, cmd + 1, cch);
		if (!cchSelf) goto app_diegle;
		if (cchSelf < cch)
		{
			/* Insert subfolder before the filename */
			TCHAR *src = cmd + cchSelf + 1, *dst = src + cchSubDir + 1;
			for(; src > cmd; --src, --dst)
			{
				*dst = *src;
				if (_T('\\') == *src || _T('/')  == *src) break;
			}
			*++src = _T('\0');
			lstrcat(src, SUBFOLDER);
			src[cchSubDir] = _T('\\');

			/* Quote path and append parameters */
			cmd[0] = _T('\"');
			lstrcat(cmd, _T("\" "));
			lstrcat(cmd, p);
			break;
		}
		cch *= 2;
	}

	MemZero(&si, sizeof(si));
	si.cb = sizeof(si);

	if (CreateProcess(NULL, cmd, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))
	{
		DWORD forkec;
		WaitForSingleObject(pi.hProcess, INFINITE);
		GetExitCodeProcess(pi.hProcess, &forkec);
		ec = forkec;
		CANLEAK(CloseHandle(pi.hProcess));
		CANLEAK(CloseHandle(pi.hThread));
		goto app_end;
	}

app_diegle:
	ec = GetLastError();
app_die:
	SIOFmtPut("Unable to start child process, error %#x\n", ec);
app_end:
	CANLEAK(MemFree(cmd));
	ExitProcess(ec);
}
