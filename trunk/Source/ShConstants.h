// constants.h by Ramon 18 Nov 2003

#ifndef ___CONSTANTS___H_____
#define ___CONSTANTS___H_____

#include "strlist.h"

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
    ConstantsStringList();

    int add(const char *name, int value1, int value2);
    int get(char *name, int n_chars = -1);
    int getnum();
    int get_value1(int idx);
    int get_value2(int idx);
    char *idx2name(int idx);

  private:
    int index;
    int get_internal_idx(int idx);
};

#endif
