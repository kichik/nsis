// uservars.h by Ramon 10 Jun 2003

#ifndef ___USERVARS___H_____
#define ___USERVARS___H_____

#ifdef NSIS_SUPPORT_NAMED_USERVARS

#include "Lang.h"

class UserVarsStringList : public LangStringList
{
  public:
    UserVarsStringList()
    {
    }
    ~UserVarsStringList() { }

    int get(char *name, size_t n_chars = -1)
    {
      int v=SortedStringListND<struct langstring>::find(name, n_chars);
      if (v==-1) return -1;
      return (((struct langstring*)gr.get())[v].index);
    }
};
#endif //NSIS_SUPPORT_NAMED_USERVARS

#endif
