/* 
  Copyright (c) 2002 Robert Rainwater
  Contributors: Justin Frankel, Fritz Elfert, and Amir Szekely

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

*/
#include <windows.h>
#include "noclib.h"

// kickik's clib methods
char *my_strrchr(const char *string, int c) {
  for (int i=lstrlen(string); i>=0; i--)
    if (string[i]==c) return (char*)&string[i];
  return 0;
}

char *my_strstr(char *i, char *s) {
  if (lstrlen(i)>=lstrlen(s)) while (i[lstrlen(s)-1])  {
    int l=lstrlen(s)+1;
    char *ii=i;
    char *is=s;
    while (--l>0) {
      if (*ii != *is) break;
      ii++;
      is++;
    }
    if (l==0) return i;
    i++;
  }
  return NULL;
}

void *my_memset(void *dest, int c, size_t count) {
  for (size_t i=0; i<count;i++) ((char*)dest)[i]=c;
  return dest;
}

// iceman_k's clib methods
int lstrncmp(char *s1, const char *s2, int chars)
{
    while ((chars > 0) && (*s1) && (*s2) && (*(s1) == *(s2))) chars--, s1++, s2++;
    if ((chars == 0) || (*s1 == *s2)) return 0;
    return (*s1 - *s2);
}

int lstrncmpi(char *s1, const char *s2, int chars)
{
  while (chars-- && *s1 && *s2)
  {
    char ss1=*s1++;
    char ss2=*s2++;
    if (ss1>='a' && ss1 <= 'z') ss1+='A'-'a';
    if (ss2>='a' && ss2 <= 'z') ss2+='A'-'a';
    if (ss1 != ss2) return ss1-ss2;
  }
  return 0;
}
