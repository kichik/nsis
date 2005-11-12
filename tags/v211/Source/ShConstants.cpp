#include "ShConstants.h"

ConstantsStringList::ConstantsStringList()
{
  index = 0;
}

int ConstantsStringList::add(const char *name, int value1, int value2)
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

int ConstantsStringList::get(char *name, int n_chars /*= -1*/)
{
  int v=SortedStringListND<struct constantstring>::find(name, n_chars);
  if (v==-1) return -1;
  return (((struct constantstring*)gr.get())[v].index);
}

int ConstantsStringList::getnum()
{
  return index;
}

int ConstantsStringList::get_value1(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*)gr.get())[pos].value1);
}

int ConstantsStringList::get_value2(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return -1;
  return (((struct constantstring*)gr.get())[pos].value2);
}

char* ConstantsStringList::idx2name(int idx)
{
  int pos=get_internal_idx(idx);
  if (pos==-1) return NULL;
  struct constantstring *data=(struct constantstring *)gr.get();      
  return ((char*)strings.get() + data[pos].name);
}

int ConstantsStringList::get_internal_idx(int idx)
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
