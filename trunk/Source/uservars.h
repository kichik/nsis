// uservars.h by Ramon 10 Jun 2003

#ifndef ___USERVARS___H_____
#define ___USERVARS___H_____

#include "Lang.h"

struct uservarstring {
  int name;
  int index;
  int pos;
  int reference;
};

class UserVarsStringList : public SortedStringListND<struct uservarstring>
{
  public:
    UserVarsStringList()
    {
      index = 0;
    }
    ~UserVarsStringList() { }

    int add(const char *name, int ref_count = 0 )
    {
      int pos=SortedStringListND<struct uservarstring>::add(name);
      if (pos == -1) return -1;

      ((struct uservarstring*)gr.get())[pos].index = index;
      ((struct uservarstring*)gr.get())[pos].pos = pos;
      ((struct uservarstring*)gr.get())[pos].reference = ref_count;

      int temp = index;
      index++;

      return temp;
    }

    int get(char *name, int n_chars = -1)
    {
      int v=SortedStringListND<struct uservarstring>::find(name, n_chars);
      if (v==-1) return -1;
      return (((struct uservarstring*)gr.get())[v].index);
    }

    int getnum()
    {
      return index;
    }

    int get_reference(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return -1;
      return (((struct uservarstring*)gr.get())[pos].reference);
    }

    int inc_reference(int idx)
    {
      int pos=get_internal_idx(idx);
      ((struct uservarstring*)gr.get())[pos].reference++;
      return (((struct uservarstring*)gr.get())[pos].reference)-1;
    }

    char *idx2name(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return NULL;
      struct uservarstring *data=(struct uservarstring *)gr.get();      
      return ((char*)strings.get() + data[pos].name);
    }

  private:
    int index;
    int get_internal_idx(int idx)
    {
      struct uservarstring *data=(struct uservarstring *)gr.get();      
      for (int i = 0; i < index; i++)
      {
        if (data[i].index == idx)
        {
          return i;
        }
      }
      return -1;
    }
};

#endif
