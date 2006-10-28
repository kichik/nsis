/*
 * growbuf.h
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

#ifndef __GROWBUF_H_
#define __GROWBUF_H_

class IGrowBuf
{
  public:
    virtual ~IGrowBuf() {}
    virtual int add(const void *data, int len)=0;
    virtual void resize(int newlen)=0;
    virtual int getlen() const=0;
    virtual void *get() const=0;
};

class GrowBuf : public IGrowBuf
{
  private: // don't copy instances
    GrowBuf(const GrowBuf&);
    void operator=(const GrowBuf&);

  public:
    GrowBuf();
    virtual ~GrowBuf();

    void set_zeroing(int zero);
    int add(const void *data, int len);
    void resize(int newlen);
    int getlen() const;
    void *get() const;

  private:
    void *m_s;
    int m_alloc;
    int m_used;
    int m_zero;

  protected:
    int m_bs;
};

class TinyGrowBuf : public GrowBuf {
  public:
    TinyGrowBuf() : GrowBuf() { m_bs=1024; }
};

#endif

