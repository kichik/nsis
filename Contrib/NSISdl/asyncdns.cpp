/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: asyncdns.cpp - JNL portable asynchronous DNS implementation
** License: see jnetlib.h
*/


#include "netinc.h"
#include "util.h"
#include "asyncdns.h"

JNL_AsyncDNS::JNL_AsyncDNS(int max_cache_entries)
{
  m_thread_kill=1;
  m_thread=0;
  m_addr=0;
  m_hostname[0]=0;
}

JNL_AsyncDNS::~JNL_AsyncDNS()
{
  m_thread_kill=1;

  if (m_thread)
  {
    WaitForSingleObject(m_thread,INFINITE);
    CloseHandle(m_thread);
  }
}

unsigned long WINAPI JNL_AsyncDNS::_threadfunc(LPVOID _d)
{
  JNL_AsyncDNS *_this=(JNL_AsyncDNS*)_d;
  int nowinsock=JNL::open_socketlib();
  struct hostent *hostentry;
  hostentry=::gethostbyname(_this->m_hostname);
  if (hostentry)
  {
    _this->m_addr=*((int*)hostentry->h_addr);
  }
  else
    _this->m_addr=INADDR_NONE;
  if (!nowinsock) JNL::close_socketlib();
  _this->m_thread_kill=1;
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

  if (lstrcmpi(m_hostname,hostname)) m_addr=0;
  else if (m_addr == INADDR_NONE) return -1;
  else if (m_addr)
  {
    *addr=m_addr;
    return 0;
  }
  lstrcpy(m_hostname,hostname);

  if (m_thread_kill)
  {
    DWORD id;
    if (m_thread) return -1;
    m_thread_kill=0;
    m_thread=CreateThread(NULL,0,_threadfunc,(LPVOID)this,0,&id);
    if (!m_thread) return -1;
  }
  return 1;
}  
