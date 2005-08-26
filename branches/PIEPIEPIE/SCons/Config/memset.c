#include <stdlib.h> // for size_t
void *memset(void *mem, int c, size_t len)
{
  char *p=(char*)mem;
  while (len-- > 0)
  {
    *p++=c;
  }
  return mem;
}
