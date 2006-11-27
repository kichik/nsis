/*
 * mmap.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "mmap.h"

#include <cstdio> // for f*
#include <cassert> // for assert
#ifndef _WIN32
#  include <sys/types.h> // for freebsd
#  include <sys/mman.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  include <unistd.h>
#endif

// ========
// MMapFile
// ========

int MMapFile::m_iAllocationGranularity = 0;

MMapFile::MMapFile()
{
#ifdef _WIN32
  m_hFile = INVALID_HANDLE_VALUE;
  m_hFileMap = NULL;
#else
  m_hFile = NULL;
  m_hFileDesc = -1;
#endif

  m_pView = NULL;
  m_iSize = 0;
  m_bReadOnly = FALSE;
  m_bTempHandle = FALSE;

  if (!m_iAllocationGranularity)
  {
#ifdef _WIN32
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    m_iAllocationGranularity = (int) si.dwAllocationGranularity;
#else
    m_iAllocationGranularity = getpagesize();
#endif
  }
}

MMapFile::~MMapFile()
{
  clear();
}

void MMapFile::clear()
{
  release();

#ifdef _WIN32
  if (m_hFileMap)
    CloseHandle(m_hFileMap);
  if (m_bTempHandle && m_hFile != INVALID_HANDLE_VALUE)
    CloseHandle(m_hFile);

  m_hFile = INVALID_HANDLE_VALUE;
  m_hFileMap = 0;
#else
  if (m_bTempHandle && m_hFile)
    fclose(m_hFile);

  m_hFile = NULL;
#endif
}

void MMapFile::setro(BOOL bRO)
{
  m_bReadOnly = bRO;
}

#ifdef _WIN32
int MMapFile::setfile(HANDLE hFile, DWORD dwSize)
#else
int MMapFile::setfile(int hFile, DWORD dwSize)
#endif
{
  clear();

#ifdef _WIN32
  m_hFile = hFile;
#else
  m_hFileDesc = hFile;
#endif
  m_bTempHandle = FALSE;

#ifdef _WIN32
  if (m_hFile == INVALID_HANDLE_VALUE)
#else
  if (m_hFileDesc == -1)
#endif
    return 0;

  m_iSize = (int) dwSize;

  if (m_iSize <= 0)
    return 0;

#ifdef _WIN32
  m_hFileMap = CreateFileMapping(m_hFile, NULL, PAGE_READONLY, 0, m_iSize, NULL);

  if (!m_hFileMap)
    return 0;
#endif

  m_bReadOnly = TRUE;

  return 1;
}

void MMapFile::resize(int newsize)
{
  release();

  if (newsize > m_iSize)
  {
#ifdef _WIN32
    if (m_hFileMap)
      CloseHandle(m_hFileMap);

    m_hFileMap = 0;
#endif

    m_iSize = newsize;

#ifdef _WIN32
    if (m_hFile == INVALID_HANDLE_VALUE)
    {
      char buf[MAX_PATH], buf2[MAX_PATH];

      GetTempPath(MAX_PATH, buf);
      GetTempFileName(buf, "nsd", 0, buf2);

      m_hFile = CreateFile(
        buf2,
        GENERIC_READ | GENERIC_WRITE,
        0,
        NULL,
        CREATE_ALWAYS,
        FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE | FILE_FLAG_SEQUENTIAL_SCAN,
        NULL
      );

      m_bTempHandle = TRUE;
    }

    if (m_hFile != INVALID_HANDLE_VALUE)
    {
      m_hFileMap = CreateFileMapping(
        m_hFile,
        NULL,
        m_bReadOnly ? PAGE_READONLY : PAGE_READWRITE,
        0,
        m_iSize,
        NULL
      );
    }
#else
    if (m_hFile == NULL)
    {
      m_hFile = tmpfile();
      if (m_hFile != NULL)
      {
        m_hFileDesc = fileno(m_hFile);
        m_bTempHandle = TRUE;
      }
    }

    // resize
    if (m_hFileDesc != -1)
    {
      unsigned char c = 0;

      if (lseek(m_hFileDesc, m_iSize, SEEK_SET) != (off_t)-1)
      {
        if (read(m_hFileDesc, &c, 1) != -1)
        {
          if (lseek(m_hFileDesc, m_iSize, SEEK_SET) != (off_t)-1)
          {
            if (write(m_hFileDesc, &c, 1) != -1)
            {
              return; // no errors
            }
          }
        }
      }
    }

    m_hFileDesc = -1; // some error occurred, bail
#endif

#ifdef _WIN32
    if (!m_hFileMap)
#else
    if (m_hFileDesc == -1)
#endif
    {
      extern FILE *g_output;
      extern void quit(); extern int g_display_errors;
      if (g_display_errors)
      {
        fprintf(g_output,"\nInternal compiler error #12345: error creating mmap the size of %d.\n", m_iSize);
        fflush(g_output);
      }
      quit();
    }
  }
}

int MMapFile::getsize() const
{
  return m_iSize;
}

void *MMapFile::get(int offset, int size) const
{
  return get(offset, &size);
}

void *MMapFile::get(int offset, int *sizep) const
{
  if (!sizep)
    return NULL;

  assert(!m_pView);

  int size = *sizep;

  if (!m_iSize || offset + size > m_iSize)
  {
    extern FILE *g_output;
    extern void quit(); extern int g_display_errors;
    if (g_display_errors) 
    {
      fprintf(g_output,"\nInternal compiler error #12345: error mmapping file (%d, %d) is out of range.\n", offset, size);
      fflush(g_output);
    }
    quit();
  }

  // fix offset
  int alignedoffset = offset - (offset % m_iAllocationGranularity);
  size += offset - alignedoffset;

#ifdef _WIN32
  const_cast<MMapFile*>(this)->m_pView =
    MapViewOfFile(m_hFileMap, m_bReadOnly ? FILE_MAP_READ : FILE_MAP_WRITE, 0, alignedoffset, size);
#else
  const_cast<MMapFile*>(this)->m_pView =
    mmap(0, size, m_bReadOnly ? PROT_READ : PROT_READ | PROT_WRITE, MAP_SHARED, m_hFileDesc, alignedoffset);
  const_cast<MMapFile*>(this)->m_iMappedSize = *sizep = size;
#endif

#ifdef _WIN32
  if (!m_pView)
#else
  if (m_pView == MAP_FAILED)
#endif
  {
    extern FILE *g_output;
    extern void quit(); extern int g_display_errors;
    if (g_display_errors) 
    {
      fprintf(g_output,"\nInternal compiler error #12345: error mmapping datablock to %d.\n", size);
      fflush(g_output);
    }
    quit();
  }

  return (void *)((char *)m_pView + offset - alignedoffset);
}

void *MMapFile::getmore(int offset, int *size) const
{
  void *pView;
  void *pViewBackup = m_pView;
#ifndef _WIN32
  int iMappedSizeBackup = m_iMappedSize;
#endif
  const_cast<MMapFile*>(this)->m_pView = 0;
  pView = get(offset, size);
  const_cast<MMapFile*>(this)->m_pView = pViewBackup;
#ifndef _WIN32
  const_cast<MMapFile*>(this)->m_iMappedSize = iMappedSizeBackup;
#endif
  return pView;
}

void MMapFile::release()
{
  if (!m_pView)
    return;

#ifdef _WIN32
  UnmapViewOfFile(m_pView);
#else
  munmap(m_pView, m_iMappedSize);
#endif
  m_pView = NULL;
}

void MMapFile::release(void *pView, int size)
{
  if (!pView)
    return;

#ifdef _WIN32
  UnmapViewOfFile(pView);
#else
  munmap(pView, size);
#endif
}

void MMapFile::flush(int num)
{
  if (m_pView)
#ifdef _WIN32
    FlushViewOfFile(m_pView, num);
#else
    msync(m_pView, num, MS_SYNC);
#endif
}

// ========
// MMapFake
// ========

MMapFake::MMapFake()
{
  m_pMem = NULL;
  m_iSize = 0;
}

void MMapFake::set(const char *pMem, int iSize)
{
  m_pMem = pMem;
  m_iSize = iSize;
}

int MMapFake::getsize() const
{
  return m_iSize;
}

void *MMapFake::get(int offset, int size) const
{
  return get(offset, &size);
}

void *MMapFake::get(int offset, int *size) const
{
  if (!size || (offset + *size > m_iSize))
    return NULL;
  return (void *)(m_pMem + offset);
}

void *MMapFake::getmore(int offset, int *size) const
{
  return get(offset, size);
}

void MMapFake::resize(int n) {}
void MMapFake::release() {}
void MMapFake::release(void *p, int size) {}
void MMapFake::clear() {}
void MMapFake::setro(BOOL b) {}
void MMapFake::flush(BOOL b) {}

// =======
// MMapBuf
// =======

MMapBuf::MMapBuf() 
{ 
  m_gb_u=0;
  m_alloc=m_used=0;
}

MMapBuf::~MMapBuf() 
{ 
  m_fm.release();
}

int MMapBuf::add(const void *data, int len) 
{ 
  if (len <= 0) return 0;
  resize(getlen() + len);
  memcpy((char*)get(getlen() - len, len), data, len);
  release();
  return getlen() - len;
}

void MMapBuf::setro(BOOL bRO)
{
  m_fm.setro(bRO);
}

void MMapBuf::resize(int newlen)
{
  if (!m_gb_u && newlen < (16 << 20)) // still in db mode
  {
    m_gb.resize(newlen);
    return;
  }

  // not in db mode
  m_gb_u = 1;
  m_used = newlen;

  if (newlen > m_alloc)
  {
    m_alloc = newlen + (16 << 20); // add 16mb to top of mapping

    m_fm.resize(m_alloc);

    if (m_gb.getlen())
    {
      memcpy(m_fm.get(0, m_gb.getlen()), m_gb.get(), m_gb.getlen());
      m_fm.flush(m_gb.getlen());
      m_fm.release();
      m_gb.resize(0);
    }
  }
}

int MMapBuf::getsize() const
{
  if (m_gb_u)
    return m_fm.getsize();
  return m_gb.getlen();
}

int MMapBuf::getlen() const
{
  if (m_gb_u)
    return m_used;
  return m_gb.getlen();
}

void *MMapBuf::get() const
{
  return get(0, m_alloc);
}

void *MMapBuf::get(int offset, int *sizep) const
{
  if (!sizep)
    return NULL;
  int size = *sizep;
  return get(offset, size);
}

void *MMapBuf::get(int offset, int size) const
{
  if (m_gb_u)
    return m_fm.get(offset, size);
  return (void *) ((char *) m_gb.get() + offset);
}

void *MMapBuf::getmore(int offset, int *size) const
{
  if (m_gb_u)
    return m_fm.getmore(offset, size);
  return (void *) ((char *) m_gb.get() + offset);
}

void MMapBuf::release()
{
  if (m_gb_u)
    m_fm.release();
}

void MMapBuf::release(void *pView, int size)
{
  if (m_gb_u)
    m_fm.release(pView, size);
}

void MMapBuf::clear()
{
  if (m_gb_u)
    m_fm.clear();
}

void MMapBuf::flush(int num)
{
  if (m_gb_u)
    m_fm.flush(num);
}
