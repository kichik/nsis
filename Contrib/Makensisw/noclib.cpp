/* 
  Copyright (c) 2002 Robert Rainwater
  Portions Copyright (c) 2002 Justin Frankel and Fritz Elfert

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
/*
Contribution by kickik
*/
#include <windows.h>
#include "noclib.h"

char *my_strrchr(const char *string, int c) {
	for (int i = lstrlen(string); i >= 0; i--)
		if (string[i] == c)
			return (char*)&string[i];
	return 0;
}

void *my_memset(void *dest, int c, size_t count) {
	for (size_t i = 0; i < count; i++) ((char*)dest)[i]=c;
	return dest;
}

char *my_strstr(const char *string, const char *strCharSet) {
	if (!*strCharSet) return (char*)string;
	size_t chklen=lstrlen(string)-lstrlen(strCharSet);
	char *s1, *s2;
	for (size_t i = 0; i < chklen; i++) {
		s1=&((char*)string)[i];
		s2=(char*)strCharSet;
		while (*s1++ == *s2++)
			if (!*s2)
				return &((char*)string)[i];
	}
	return 0;
}