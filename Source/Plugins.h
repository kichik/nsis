

#ifndef __X18_PLUGINS_H
#define __X18_PLUGINS_H


#include <windows.h>
#include <stdio.h>
#include "strlist.h"
#include <vector>

class Plugins
{
  public:
    void  FindCommands(char*,bool);
    bool  IsPluginCommand(char*);
    char* GetPluginDll(int, char*, int*);
    void  SetDllDataHandle(int, char*, int);

  protected:
    DefineList m_commands;
    GrowBuf    m_dataHandles;
    GrowBuf    m_uninstDataHandles;
    int        m_funcsCount;

    void GetExports(char*,bool);
};


#endif