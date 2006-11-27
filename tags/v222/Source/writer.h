/*
 * writer.h
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

#ifndef ___WRITER__H___
#define ___WRITER__H___

#include "exehead/config.h"
#include "growbuf.h"
#include "crc32.h"
#include <stdio.h>

class writer_sink {
public:
  writer_sink() {}
  virtual ~writer_sink() {}

  virtual void write_byte(const unsigned char b);
  virtual void write_short(const short s);
  virtual void write_int(const int i);
  virtual void write_int_array(const int i[], const size_t len);
  virtual void write_string(const char *s);
  virtual void write_string(const char *s, const size_t size);
  virtual void write_growbuf(const IGrowBuf *b);

  virtual void write_data(const void *data, const size_t size) = 0;

};

class writer {
public:
  writer(writer_sink *sink) : m_sink(sink) {}
  virtual ~writer() {}

protected:
  writer_sink *m_sink;

};

class growbuf_writer_sink : public writer_sink {
public:
  growbuf_writer_sink(IGrowBuf *buf) : m_buf(buf) {}

  virtual void write_data(const void *data, const size_t size);

private:
  IGrowBuf *m_buf;

};

class file_writer_sink : public writer_sink {
public:
  file_writer_sink(FILE *fp) : m_fp(fp) {}

  virtual void write_data(const void *data, const size_t size);

private:
  FILE *m_fp;

};

#ifdef NSIS_CONFIG_CRC_SUPPORT
class crc_writer_sink : public writer_sink {
public:
  crc_writer_sink(crc32_t *crc) : m_crc(crc) {}

  virtual void write_data(const void *data, const size_t size);

private:
  crc32_t *m_crc;

};
#endif

#endif//!___WRITER__H___
