/*
 * growbuf.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2014 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/22/2007
 */

#ifndef __GROWBUF_H_
#define __GROWBUF_H_

/**
 * IGrowBuf is the interface to a buffer that grows as you
 * add to the buffer.
 */
class IGrowBuf
{
  public:
    virtual ~IGrowBuf() {}

    /**
     * Add data to the buffer.
     * @param data Pointer to the data to be stored.
     * @param len Size of the data in bytes.
     * @return the previous logical size in bytes before the addition.
     */
    virtual int add(const void *data, int len)=0;

    /**
     * Resizes the buffer to hold the number of bytes specified.
     * @param newlen the desired logical size of the buffer.
     */
    virtual void resize(int newlen)=0;

    /**
     * Get the length of the logical buffer in bytes.
     * @return the length in bytes
     */
    virtual int getlen() const=0;

    /**
     * Get the buffer itself.
     * @return Void pointer to the buffer.
     */
    virtual void *get() const=0;
};

/**
 * GrowBuf implements IGrowBuf and grows in 32K chunks.
 */
class GrowBuf : public IGrowBuf
{
  private: // don't copy instances
    GrowBuf(const GrowBuf&);
    void operator=(const GrowBuf&);

  public:
    GrowBuf();
    virtual ~GrowBuf();

    /**
     * Set whether to zero out buffer
     * @param zero A boolean value.
     */
    void set_zeroing(int zero);

    /**
     * Add data to the buffer.
     * @param data Pointer to the data to be stored.
     * @param len Size of the data in bytes.
     * @return the previous logical size in bytes before the addition.
     */
    int add(const void *data, int len);

    /**
     * Resizes the buffer to hold the number of bytes specified.
     * @param newlen the desired logical size of the buffer.
     */
    void resize(int newlen);

    /**
     * Get the length of the logical buffer in bytes.
     * Setting the newlen to 0 will cause the buffer to be at most
     * 2*m_bs bytes long.  (It will free the buffer if > 2*m_bs.)
     *
     * @return the length in bytes
     */
    int getlen() const;

    /**
     * Get the buffer itself.
     * @return Void pointer to the buffer.
     */
    void *get() const;

    void swap(GrowBuf&other);

  private:
    void *m_s;    /* the storage buffer */
    int m_alloc;  /* allocated bytes */
    int m_used;   /* how many bytes of the buffer is used? */
    int m_zero;   /* should storage be zeroed out? */

  protected:
    int m_bs;     // byte-size to grow by
};

/**
 * TinyGrowBuf is a derived class that grows the buffer
 * in tiny increments.
 */
class TinyGrowBuf : public GrowBuf {
  public:
    TinyGrowBuf() : GrowBuf() { m_bs=1024; }
};

#endif

