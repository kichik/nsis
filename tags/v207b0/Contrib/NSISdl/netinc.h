/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: netinc.h - network includes and portability defines (used internally)
** License: see jnetlib.h
*/

#ifndef _NETINC_H_
#define _NETINC_H_

#include <windows.h>
#include "util.h"

#define strcasecmp(x,y) stricmp(x,y)
#define ERRNO (WSAGetLastError())
#define SET_SOCK_BLOCK(s,block) { unsigned long __i=block?0:1; ioctlsocket(s,FIONBIO,&__i); }
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EINPROGRESS WSAEWOULDBLOCK
#define memset mini_memset
#define memcpy mini_memcpy
#define strcpy lstrcpy
#define strncpy lstrcpyn
#define strcat lstrcat
#define strlen lstrlen
#define malloc(x) (new char[x])
#define free(x) {delete x;}
typedef int socklen_t;

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

#ifndef INADDR_ANY
#define INADDR_ANY 0
#endif

#ifndef SHUT_RDWR
#define SHUT_RDWR 2
#endif

#endif //_NETINC_H_
