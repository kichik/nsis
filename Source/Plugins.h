

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
    char* GetPluginDll(char*);
    void  StoreInstDLL(char*);
    void  StoreUninstDLL(char*);
    char* GetInstDLL(int);
    char* GetUninstDLL(int);

  protected:
    DefineList         m_commands;
    std::vector<char*> m_storedDLLs;
    std::vector<char*> m_installDLLs;
    std::vector<char*> m_uninstallDLLs;

    void GetExports(char*,bool);
};


#endif