#ifndef __X18_PLUGINS_H
#define __X18_PLUGINS_H

#include "Platform.h"
#include "strlist.h"

struct plugin {
  int name;
  int path;
  int dataHandle;
  int unDataHandle;
};

class PluginsList : public SortedStringListND<struct plugin>
{
  public:
    int add(const char *name, const char *path);
    char *get(char **name, int *dataHandle=0, int *uninstDataHandle=0);
    void setDataHandle(const char *name, int dataHandle, int uninstDataHandle);
};

class Plugins
{
  public:
    void  FindCommands(char*,bool);
    bool  IsPluginCommand(char*);
    char* GetPluginDll(int, char**, int*);
    void  SetDllDataHandle(int, char*, int);

  protected:
    PluginsList m_list;

    void GetExports(char*,bool);
};

#endif
