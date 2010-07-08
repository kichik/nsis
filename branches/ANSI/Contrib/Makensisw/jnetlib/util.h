/*
** JNetLib
** Copyright (C) 2000-2001 Nullsoft, Inc.
** Author: Justin Frankel
** File: util.h - JNL interface for basic network utilities
** License: zlib
**
** routines you may be interested in:
**   JNL::open_socketlib(); 
**    opens the socket library. Call this once before using any network
**    code. If you create a new thread, call this again. Only really an
**    issue for Win32 support, but use it anyway for portability/
**
**   JNL::close_Socketlib();
**    closes the socketlib. Call this when you're done with the network,
**    after all your JNetLib objects have been destroyed.
**
**   unsigned long JNL::ipstr_to_addr(const char *cp); 
**    gives you the integer representation of a ip address in dotted 
**    decimal form.
**
**  JNL::addr_to_ipstr(unsigned long addr, char *host, int maxhostlen);
**    gives you the dotted decimal notation of an integer ip address.
**
** Reviewed for Unicode Support by Jim Park -- 08/17/2007
** Note: The functions that work on char's should be explicitely set to use the
** ANSI versions.  Some of the functions like wprintf() are #defined to be
** the wide-char versions when _UNICODE is defined.  So these must be explictly
** set to use the ANSI versions.
*/

#ifndef _UTIL_H_
#define _UTIL_H_

class JNL
{
  public:
    static int open_socketlib();
    static void close_socketlib();
    static unsigned long ipstr_to_addr(const char *cp);
    static void addr_to_ipstr(unsigned long addr, char *host, int maxhostlen);
};

int my_atoi(char *p);

#endif //_UTIL_H_
