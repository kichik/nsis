

#ifndef __X18_EXTERNALCOMMANDS_H
#define __X18_EXTERNALCOMMANDS_H


#include <windows.h>
#include <stdio.h>
#include "strlist.h"
#include <vector>


class ExternalCommands
{
  public:
    void  FindCommands(char*,bool);
    bool  IsExternalCommand(char*);
    char* GetExternalCommandDll(char*);
    int   GetDllDataHandle(char*);
    void  StoreDllDataHandle(char*,int);

  protected:
    DefineList       m_commands;
    std::vector<int> m_dataHandles;

    void GetExports(char*,bool);
};


#endif