/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: asyncdns.cpp - JNL portable asynchronous DNS implementation
** License: see License.txt
**
** Unicode support by Jim Park -- 08/24/2007
*/

// Jim Park: For Unicode Support, all string functions must explicitly use
// ANSI versions if UNICODE is defined.

#include "netinc.h"
#include "util.h"
#include "asyncdns.h"

JNL_AsyncDNS::JNL_AsyncDNS()
{
  m_thread=0;
  m_addr=0;
  m_hostname[0]=0;
}

JNL_AsyncDNS::~JNL_AsyncDNS()
{
  wait_for_thread_death();
}

DWORD WINAPI JNL_AsyncDNS::_threadfunc(LPVOID _d)
{
  JNL_AsyncDNS *_this=(JNL_AsyncDNS*)_d;
  struct hostent *hostentry;
  hostentry=::gethostbyname(_this->m_hostname);
  if (hostentry)
  {
    _this->m_addr=*((int*)hostentry->h_addr);
  }
  else
    _this->m_addr=INADDR_NONE;
  return 0;
}

int JNL_AsyncDNS::resolve(char *hostname, unsigned long *addr)
{
  // return 0 on success, 1 on wait, -1 on unresolvable
  unsigned long ip=inet_addr(hostname);
  if (ip != INADDR_NONE) 
  {
    *addr=ip;
    return 0;
  }

  if (lstrcmpiA(m_hostname,hostname)) m_addr=0;
  else if (m_addr == INADDR_NONE)
  {
    wait_for_thread_death();
    return -1;
  }
  else if (m_addr)
  {
    *addr=m_addr;
    wait_for_thread_death();
    return 0;
  }
  lstrcpyA(m_hostname,hostname);

  if (!m_thread)
  {
    DWORD id;
    m_thread=CreateThread(NULL,0,_threadfunc,(LPVOID)this,0,&id);
    if (!m_thread) return -1;
  }
  return 1;
}  

void JNL_AsyncDNS::wait_for_thread_death()
{
  if (m_thread)
  {
    WaitForSingleObject(m_thread,INFINITE);
    CloseHandle(m_thread);
  }

  m_thread=0;
}
