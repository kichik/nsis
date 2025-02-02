/*
 * crt_replacements.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 */

#include "../Platform.h"

#if defined(_MSC_VER)
  #pragma function(memcpy)
#endif
void *memcpy(void *dest, const void *src, size_t count)
{
  char *dest8 = (char *)dest;
  const char *src8 = (const char *)src;
  while (count--) *dest8++ = *src8++;
  return dest;
}

#if defined(_MSC_VER)
  #pragma function(memmove)
#endif
void *memmove(void *dest, const void *src, unsigned int n)
{
  char *pcDstn =(char *)dest;
  const char *pcSource =(char *)src;
  if((pcSource < pcDstn) && (pcDstn < pcSource + n))
    for (pcDstn += n, pcSource += n; n--;) *--pcDstn = *--pcSource;
  else
    while(n--) *pcDstn++ = *pcSource++;
  return dest;
}

void *malloc(size_t size)
{
  return GlobalAlloc(GPTR, size);
}

void *calloc(size_t num, size_t size)
{
  void *mem = malloc(num * size);
  return memset(mem, 0, num * size);
}

void free(void *ptr)
{
  GlobalFree(ptr);
}


#if defined(_MSC_VER) && defined(_M_IX86)

#define CRT_LOWORD(x) dword ptr [x+0]
#define CRT_HIWORD(x) dword ptr [x+4]

// Helper to multiply two (u)int64, only needed on x86 MSVC
__declspec(naked) void _allmul()
{
  #define A       esp + 8       // stack address of a
  #define B       esp + 16      // stack address of b

  __asm
  {
    push    ebx

    mov     eax, CRT_HIWORD(A)
    mov     ecx, CRT_LOWORD(B)
    mul     ecx                     ;eax has AHI, ecx has BLO, so AHI * BLO
    mov     ebx, eax                ;save result

    mov     eax, CRT_LOWORD(A)
    mul     CRT_HIWORD(B)           ;ALO * BHI
    add     ebx, eax                ;ebx = ((ALO * BHI) + (AHI * BLO))

    mov     eax, CRT_LOWORD(A)      ;ecx = BLO
    mul     ecx                     ;so edx:eax = ALO*BLO
    add     edx, ebx                ;now edx has all the LO*HI stuff

    pop     ebx

    ret     16                      ;callee restores the stack
  }

  #undef A
  #undef B
}

__declspec(naked) void _allshl()
{
  __asm
  {
    ; Handle shifts of 64 or more bits (all get 0)
    cmp     cl, 64
    jae     short RETZERO

    ; Handle shifts of between 0 and 31 bits
    cmp     cl, 32
    jae     short MORE32
    shld    edx,eax,cl
    shl     eax,cl
    ret

    ; Handle shifts of between 32 and 63 bits
MORE32:
    mov     edx,eax
    xor     eax,eax
    and     cl,31
    shl     edx,cl
    ret

    ; return 0 in edx:eax
RETZERO:
    xor     eax,eax
    xor     edx,edx
    ret
  }
}

#endif
