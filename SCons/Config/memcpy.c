#include <stdlib.h> // for size_t
void *memcpy(void *out, const void *in, size_t len)
{
  char *c_out=(char*)out;
  char *c_in=(char *)in;
  while (len-- > 0)
  {
    *c_out++=*c_in++;
  }
  return out;
}
