/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: asyncdns.h - JNL portable asynchronous DNS interface
** License: see jnetlib.h
**
** Usage:
**   1. Create JNL_AsyncDNS object, optionally with the number of cache entries.
**   2. call resolve() to resolve a hostname into an address. The return value of 
**      resolve is 0 on success (host successfully resolved), 1 on wait (meaning
**      try calling resolve() with the same hostname in a few hundred milliseconds 
**      or so), or -1 on error (i.e. the host can't resolve).
**   4. enjoy.
*/

#ifndef _ASYNCDNS_H_
#define _ASYNCDNS_H_

class JNL_AsyncDNS
{
public:
  JNL_AsyncDNS(int max_cache_entries=64);
  ~JNL_AsyncDNS();

  int resolve(char *hostname, unsigned long *addr); // return 0 on success, 1 on wait, -1 on unresolvable

private:
  void wait_for_thread_death();

  char m_hostname[256];
  unsigned long m_addr;

  HANDLE m_thread;
  static unsigned long WINAPI _threadfunc(LPVOID _d);

};

#endif //_ASYNCDNS_H_
