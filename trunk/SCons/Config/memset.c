#include <stdlib.h> // for size_t

void *memset(void *mem, int c, size_t len)
{
  /*
  ** Prevent MSVC 14.00.40310.41-AMD64 from generating a recursive call to memset
  **
  ** #pragma optimize("", off) + #pragma optimize("ty", on) can also
  ** be used but it generates a lot more code.
  */
#if defined(_MSC_VER) && _MSC_VER > 1200 && _MSC_FULL_VER <= 140040310
  volatile 
#endif
  char *p=(char*)mem;
  while (len-- > 0)
  {
    *p++=c;
  }
  return mem;
}
