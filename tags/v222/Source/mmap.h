/*
 * mmap.h
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

#ifndef __MMAP_H_
#define __MMAP_H_

#include "Platform.h"
#include "growbuf.h"

#ifndef _WIN32
#include <cstdio> // for FILE*
#endif

class IMMap
{
  public:
    virtual void resize(int newlen)=0;
    virtual int  getsize() const=0;
    virtual void *get(int offset, int size) const=0;
    virtual void *get(int offset, int *size) const=0;
    virtual void *getmore(int offset, int *size) const=0;
    virtual void release()=0;
    virtual void release(void *view, int size)=0;
    virtual void clear()=0;
    virtual void setro(BOOL bRO)=0;
    virtual void flush(int num)=0;
    virtual ~IMMap() {}
};

class MMapFile : public IMMap
{
  private: // don't copy instances
    MMapFile(const MMapFile&);
    void operator=(const MMapFile&);

  public:
    MMapFile();
    virtual ~MMapFile();

    void clear();
    void setro(BOOL bRO);
#ifdef _WIN32
    int  setfile(HANDLE hFile, DWORD dwSize);
#else
    int  setfile(int hFile, DWORD dwSize);
#endif
    void resize(int newsize);
    int  getsize() const;
    void *get(int offset, int size) const;
    void *get(int offset, int *sizep) const;
    void *getmore(int offset, int *size) const;
    void release();
    void release(void *pView, int size);
    void flush(int num);

  private:
#ifdef _WIN32
    HANDLE m_hFile, m_hFileMap;
#else
    FILE *m_hFile;
    int m_hFileDesc;
    int m_iMappedSize;
#endif
    void *m_pView;
    int m_iSize;
    BOOL m_bReadOnly;
    BOOL m_bTempHandle;

    static int m_iAllocationGranularity;
};

class MMapFake : public IMMap
{
  private: // don't copy instances
    MMapFake(const MMapFake&);
    void operator=(const MMapFake&);
  public:
    MMapFake();

    void set(const char *pMem, int iSize);
    int  getsize() const;
    void *get(int offset, int size) const;
    void *get(int offset, int *size) const;
    void *getmore(int offset, int *size) const;

    void resize(int n);
    void release();
    void release(void *p, int size);
    void clear();
    void setro(BOOL b);
    void flush(BOOL b);

  private:
    const char *m_pMem;
    int m_iSize;
};

class MMapBuf : public IGrowBuf, public IMMap
{
  private: // don't copy instances
    MMapBuf(const MMapBuf&);
    void operator=(const MMapBuf&);

  public:
    MMapBuf();
    virtual ~MMapBuf();

    int  add(const void *data, int len);
    void setro(BOOL bRO);
    void resize(int newlen);
    int  getsize() const;
    int  getlen() const;
    void *get() const;
    void *get(int offset, int *sizep) const;
    void *get(int offset, int size) const;
    void *getmore(int offset, int *size) const;
    void release();
    void release(void *pView, int size);
    void clear();
    void flush(int num);

  private:
    GrowBuf m_gb;
    MMapFile m_fm;

    int m_gb_u;
    int m_alloc, m_used;
};

#endif//__MMAP_H_

