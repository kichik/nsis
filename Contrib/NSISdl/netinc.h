/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: netinc.h - network includes and portability defines (used internally)
** License: see License.txt
**
** Unicode support by Jim Park -- 08/24/2007
*/

#ifndef _NETINC_H_
#define _NETINC_H_

#include <windows.h>
#include "util.h"

#define strcasecmp(x,y) stricmp(x,y)
#define ERRNO (WSAGetLastError())
#define SET_SOCK_BLOCK(s,block) { unsigned long __i=block?0:1; ioctlsocket(s,FIONBIO,&__i); }
#ifndef EWOULDBLOCK
#define EWOULDBLOCK WSAEWOULDBLOCK
#endif
#ifndef EINPROGRESS
#define EINPROGRESS WSAEWOULDBLOCK
#endif
#define memset mini_memset
#define memcpy mini_memcpy
// Jim Park: For Unicode support, we need to distinguish whether we are working on
// Unicode or ANSI.
#define strcpy lstrcpyA
#define strncpy lstrcpynA
#define strcat lstrcatA
#define strlen lstrlenA
#define malloc(x) (new char[x])
#define free(x) {delete [] x;}
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

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

#define PORTABLE_SOCKET SOCKET

#endif //_NETINC_H_
