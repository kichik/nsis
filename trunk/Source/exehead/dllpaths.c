

#include "dllpaths.h"
#include <windows.h>
#include "util.h"


static int    initialised = 0;
static int    numPackedPathIds;
static int*   packedPathIds;
static char** newPaths;


void* DllPathsAlloc(long bytes)
{
  return HeapAlloc(
    GetProcessHeap(),
    HEAP_ZERO_MEMORY,
    bytes);
}

void DllPathsFree(void* ptr)
{
  HeapFree(GetProcessHeap(),0,ptr);
}

void DllPathsCleanup(void)
{
  if (initialised)
  {
    if (packedPathIds)
    {
      int i;
      DllPathsFree(packedPathIds);
      for (i = 0; i < numPackedPathIds; i++)
      {
        DeleteFile(newPaths[i]);
        DllPathsFree(newPaths[i]);
      }
      DllPathsFree(newPaths);
    }

    numPackedPathIds = 0;
    packedPathIds    = 0;
    newPaths         = 0;
    initialised      = 0;
  }
}

void DllPathsInitialise(void)
{
  if (!initialised)
  {
    numPackedPathIds = 0;
    packedPathIds    = 0;
    newPaths         = 0;
    initialised      = 1;
  }
}

void DllPathsAdd(int n,char* path)
{
  DllPathsInitialise();
  if (!DllPathsDetermined(n))
  {
    int*   newIntArray  = (int*)DllPathsAlloc(sizeof(int)*(numPackedPathIds+1));
    char** newCharArray = (char**)DllPathsAlloc(sizeof(char*)*(numPackedPathIds+1));
    mini_memcpy(newIntArray,packedPathIds,numPackedPathIds*sizeof(int));
    mini_memcpy(newCharArray,newPaths,numPackedPathIds*sizeof(char*));
    DllPathsFree(packedPathIds);
    DllPathsFree(newPaths);
    packedPathIds = newIntArray;
    newPaths      = newCharArray;
    packedPathIds[numPackedPathIds] = n;
    newPaths[numPackedPathIds] = (char*)DllPathsAlloc(sizeof(char)*(lstrlen(path)+1));
    lstrcpy(newPaths[numPackedPathIds],path);
    numPackedPathIds++;
  }
}

char* DllPathsDetermined(int n)
{
  int i;
  DllPathsInitialise();
  for (i = 0; i < numPackedPathIds; i++)
  {
    if (packedPathIds[i] == n)
      return newPaths[i];
  }
  return 0;
}
