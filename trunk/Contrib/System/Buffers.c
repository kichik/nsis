#include "stdafx.h"
#include "Plugin.h"
#include "System.h"
#include "Buffers.h"

PLUGINFUNCTIONSHORT(Alloc)
{
	int size;
	if ((size = popint()) == 0)
	{
		pushint(0);
		return;
	}
	pushint((int) GlobalAlloc(GPTR, size));
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Copy)
{
    int size = 0;
    HANDLE source, dest;
	char *str;
    // Get the string
	if ((str = popstring()) == NULL) return;

    // Check for size option
    if (str[0] == '/')
    {
        size = (int) myatoi(str+1);
        dest = popint();
    }
    else dest = (HANDLE) myatoi(str+1);
    source = popint();

    // Ok, check the size
    if (size == 0) size = (int) GlobalSize(source);
    // and the destinantion
    if ((int) dest == 0) dest = GlobalAlloc((GPTR), size);

    // COPY!
    copymem(dest, source, size);

    GlobalFree(str);
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Free)
{
	char *str;
    // Get the string
	if ((str = popstring()) == NULL) return;
    // Check for callback clear
    if (lstrcmpi(str,"/callback") == 0)
    {
        SystemProc *proc, *next;
	    next = (SystemProc*) popint();
        // Clear all the clone queue of callback
        while ((proc = next) != NULL)
        {
            next = proc->Clone;
            GlobalFree((HANDLE)proc);
        }
    }
    else
        GlobalFree((HANDLE) myatoi(str)); 
    GlobalFree(str);
}
PLUGINFUNCTIONEND

char *copymem(char *output, char *input, int size)
{
    char *out = output;
    while (size-- > 0) *(out++) = *(input++);
    return output;
}

HANDLE GlobalCopy(HANDLE Old)
{
	SIZE_T size = GlobalSize(Old);
    return copymem(GlobalAlloc(GPTR, size), Old, (int) size);
}