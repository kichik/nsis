

#ifndef __X18_PLUGINS_H
#define __X18_PLUGINS_H


#include <windows.h>
#include <stdio.h>
#include "strlist.h"
#include <vector>

struct DLL {
  char *name;
  bool stored;
};

class Plugins
{
  public:
    void  FindCommands(char*,bool);
    bool  IsPluginCommand(char*);
    char* GetPluginDll(char*);
    void  DLLStored(char*);
    bool  IsDLLStored(char*);

  protected:
    DefineList       m_commands;
    std::vector<DLL> m_storedDLLs;

    void GetExports(char*,bool);
};


#endif