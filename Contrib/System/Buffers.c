#include "stdafx.h"
#include "Plugin.h"
#include "System.h"
#include "Buffers.h"

PLUGINFUNCTION(AllocCopy)
	int mem;

	if (popint(&mem) == 0)
	{
		pushint(0);
		return;
	}

	mem = (int) GlobalCopy((HANDLE) mem);
	pushint(mem);
PLUGINFUNCTIONEND

PLUGINFUNCTION(Alloc)
	int size;
	int mem;

	if (popint(&size) == 0)
	{
		pushint(0);
		return;
	}

	mem = (int) GlobalAlloc(GPTR, size);
	pushint(mem);
PLUGINFUNCTIONEND

PLUGINFUNCTION(Free)
	int mem;

	if ((popint(&mem) == 0) || (mem == 0))
	{
		pushstring("false");
		return;
	}
	if ((GlobalFree((HANDLE) mem) == NULL)) pushstring("true");
	else pushstring("false");
PLUGINFUNCTIONEND

/*typedef BOOL (__stdcall *GetDiskSpace)
(
  LPCTSTR lpDirectoryName,                 // directory name
  PULARGE_INTEGER lpFreeBytesAvailable,    // bytes available to caller
  PULARGE_INTEGER lpTotalNumberOfBytes,    // bytes on disk
  PULARGE_INTEGER lpTotalNumberOfFreeBytes // free bytes on disk
);*/

/*PLUGINFUNCTION(MyFunction)
		GetDiskSpace proc;
		ULARGE_INTEGER i1, i2, i3;
		BOOL check;

		proc = (GetDiskSpace) GetProcAddress(GetModuleHandle("kernel32.dll"), "GetDiskFreeSpaceExA");
		check = proc(NULL, &i1, &i2, &i3);

        _asm
        {
                push    ecx
                lea             ecx, i3
                push    ecx
                lea             ecx, i2
                push    ecx
                lea             ecx, i1
                push    ecx
                push    0

                call    proc

				mov				check, eax
//                add             esp, 16
                pop             ecx
        }

	char buf[1024];
    wsprintf(buf,"$0=%s\n",getuservariable(INST_0));
    MessageBox(g_hwndParent,buf,0,MB_OK);
PLUGINFUNCTIONEND*/

HANDLE GlobalCopy(HANDLE Old)
{
	SIZE_T size;
	HANDLE n;

	size = GlobalSize(Old);
	n = GlobalAlloc(GPTR, size);
	CopyMemory(n, Old, size);
	return n;
}