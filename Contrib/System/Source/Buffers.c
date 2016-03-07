#include "stdafx.h"
#include "Plugin.h"
#include "System.h"
#include "Buffers.h"

typedef struct tagTempStack TempStack;
struct tagTempStack
{
    TempStack *Next;
    char Data[0];
};
TempStack *tempstack = NULL;

PLUGINFUNCTIONSHORT(Alloc)
{
    int size;
    if ((size = popint64()) == 0)
    {
        system_pushint(0);
        return;
    }
    system_pushint((int) GlobalAlloc(GPTR, size));
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Copy)
{
    int size = 0;
    HANDLE source, dest;
    char *str;
    // Get the string
    if ((str = system_popstring()) == NULL) return;

    // Check for size option
    if (str[0] == '/')
    {
        size = (int) myatoi64(str+1);
        dest = (HANDLE) popint64();
    }
    else dest = (HANDLE)(INT_PTR) myatoi64(str);
    source = (HANDLE) popint64();

    // Ok, check the size
    if (size == 0) size = (int) GlobalSize(source);
    // and the destinantion
    if ((int) dest == 0) 
    {
        dest = GlobalAlloc((GPTR), size);
        system_pushint((int) dest);
    }

    // COPY!
    copymem(dest, source, size);

    GlobalFree(str);
}
PLUGINFUNCTIONEND

PLUGINFUNCTIONSHORT(Free)
{
    GlobalFree((HANDLE) popint64());
}
PLUGINFUNCTIONEND

PLUGINFUNCTION(Store)
{
    TempStack *tmp;
    int size = ((INST_R9+1)*g_stringsize);    

    char *command, *cmd = command = system_popstring();
    while (*cmd != 0)
    {
        switch (*(cmd++))
        {
        case 's':
        case 'S':
            // Store the whole variables range
            tmp = (TempStack*) GlobalAlloc(GPTR, sizeof(TempStack)+size);
            tmp->Next = tempstack;
            tempstack = tmp;

            // Fill with data
            copymem(tempstack->Data, g_variables, size);
            break;
        case 'l':
        case 'L':
            if (tempstack == NULL) break;

            // Fill with data
            copymem(g_variables, tempstack->Data, size);

            // Restore stack
            tmp = tempstack->Next;
            GlobalFree((HANDLE) tempstack);
            tempstack = tmp;
            break;
        case 'P':
            *cmd += 10;
        case 'p':
            GlobalFree((HANDLE) system_pushstring(system_getuservariable(*(cmd++)-'0')));
            break;
        case 'R':
            *cmd += 10;
        case 'r':
            GlobalFree((HANDLE) system_setuservariable(*(cmd++)-'0', system_popstring()));
            break;
        }
    }

    GlobalFree((HANDLE) command);
}
PLUGINFUNCTIONEND
