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
    PluginsList() { }
    ~PluginsList() { }

    int add(const char *name, const char *path)
    {
      int pos=SortedStringListND<struct plugin>::add(name);
      if (pos == -1) return 1;

      ((struct plugin*)gr.get())[pos].path=strings.add(path, strlen(path)+1);
      ((struct plugin*)gr.get())[pos].dataHandle=-1;
      ((struct plugin*)gr.get())[pos].unDataHandle=-1;

      return 0;
    }

    char *get(char **name, int *dataHandle=0, int *uninstDataHandle=0)
    {
      if (dataHandle) *dataHandle=-1;
      if (uninstDataHandle) *uninstDataHandle=-1;
      int v=SortedStringListND<struct plugin>::find(*name);
      if (v==-1) return NULL;
      strcpy(*name, (char*)strings.get()+((struct plugin*)gr.get())[v].name);
      if (dataHandle) *dataHandle=((struct plugin*)gr.get())[v].dataHandle;
      if (uninstDataHandle) *uninstDataHandle=((struct plugin*)gr.get())[v].unDataHandle;
      return (char*)strings.get()+((struct plugin*)gr.get())[v].path;
    }

    void setDataHandle(const char *name, int dataHandle, int uninstDataHandle)
    {
      int v=SortedStringListND<struct plugin>::find(name);
      if (v==-1) return;
      ((struct plugin*)gr.get())[v].dataHandle=dataHandle;
      ((struct plugin*)gr.get())[v].unDataHandle=uninstDataHandle;
    }
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