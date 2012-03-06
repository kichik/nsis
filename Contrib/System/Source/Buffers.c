// Unicode support by Jim Park -- 08/23/2007

#include "stdafx.h"
#include "Plugin.h"
#include "System.h"
#include "Buffers.h"

typedef struct tagTempStack TempStack;
struct tagTempStack
{
    TempStack *Next;
    TCHAR Data[0];
};
TempStack *tempstack = NULL;

static void AllocWorker(unsigned int mult)
{
    int size;
    if ((size = popint()) == 0)
    {
        system_pushint(0);
        return;
    }
    system_pushintptr((INT_PTR) GlobalAlloc(GPTR, size * mult));
}

PLUGINFUNCTIONSHORT(Alloc)
{
    AllocWorker(sizeof(unsigned char));
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(StrAlloc)
{
    AllocWorker(sizeof(TCHAR));
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Copy)
{
    int size = 0;
    HANDLE source, dest;
    TCHAR *str;
    // Get the string
    if ((str = system_popstring()) == NULL) return;

    // Check for size option
    if (str[0] == _T('/'))
    {
        size = (SIZE_T) myatoi(str+1);
        dest = (HANDLE) popintptr();
    }
    else dest = (HANDLE) myatoi(str);
    source = (HANDLE) popintptr();

    // Ok, check the size
    if (size == 0) size = (SIZE_T) GlobalSize(source);
    // and the destinantion
    if ((int) dest == 0) 
    {
        dest = GlobalAlloc((GPTR), size);
        system_pushintptr((INT_PTR) dest);
    }

    // COPY!
    copymem(dest, source, size);

    GlobalFree(str);
}
PLUGINFUNCTIONEND

PLUGINFUNCTION(Store)
{
    TempStack *tmp;
    int size = ((INST_R9+1)*g_stringsize*sizeof(TCHAR));

    TCHAR *command, *cmd = command = system_popstring();
    while (*cmd != 0)
    {
        switch (*(cmd++))
        {
        case _T('s'):
        case _T('S'):
            // Store the whole variables range
            tmp = (TempStack*) GlobalAlloc(GPTR, sizeof(TempStack)+size);
            tmp->Next = tempstack;
            tempstack = tmp;

            // Fill with data
            copymem(tempstack->Data, g_variables, size);
            break;
        case _T('l'):
        case _T('L'):
            if (tempstack == NULL) break;

            // Fill with data
            copymem(g_variables, tempstack->Data, size);

            // Restore stack
            tmp = tempstack->Next;
            GlobalFree((HANDLE) tempstack);
            tempstack = tmp;
            break;
        case _T('P'):
            *cmd += 10;
        case _T('p'):
            GlobalFree((HANDLE) system_pushstring(system_getuservariable(*(cmd++)-_T('0'))));
            break;
        case _T('R'):
            *cmd += 10;
        case _T('r'):
            GlobalFree((HANDLE) system_setuservariable(*(cmd++)-_T('0'), system_popstring()));
            break;
        }
    }

    GlobalFree((HANDLE) command);
}
PLUGINFUNCTIONEND
