// constants.h by Ramon 18 Nov 2003

#ifndef ___CONSTANTS___H_____
#define ___CONSTANTS___H_____

#include "lang.h"

struct constantstring {
  int name;
  int index;
  int pos;
  int value1;
  int value2;
};

class ConstantsStringList : public SortedStringListND<struct constantstring>
{
  public:
    ConstantsStringList()
    {
      index = 0;
    }
    ~ConstantsStringList() { }

    int add(const char *name, int value1, int value2)
    {
      int pos=SortedStringListND<struct constantstring>::add(name);
      if (pos == -1) return -1;

      ((struct constantstring*)gr.get())[pos].index = index;
      ((struct constantstring*)gr.get())[pos].pos = pos;
      ((struct constantstring*)gr.get())[pos].value1 = value1;
      ((struct constantstring*)gr.get())[pos].value2 = value2;

      int temp = index;
      index++;

      return temp;
    }

    int get(char *name, int n_chars = -1)
    {
      int v=SortedStringListND<struct constantstring>::find(name, n_chars);
      if (v==-1) return -1;
      return (((struct constantstring*)gr.get())[v].index);
    }

    int getnum()
    {
      return index;
    }

    int get_value1(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return -1;
      return (((struct constantstring*)gr.get())[pos].value1);
    }

    int get_value2(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return -1;
      return (((struct constantstring*)gr.get())[pos].value2);
    }

    char *idx2name(int idx)
    {
      int pos=get_internal_idx(idx);
      if (pos==-1) return NULL;
      struct constantstring *data=(struct constantstring *)gr.get();      
      return ((char*)strings.get() + data[pos].name);
    }

  private:
    int index;
    int get_internal_idx(int idx)
    {
      struct constantstring *data=(struct constantstring *)gr.get();      
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
