/*
 * writer.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2021 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Unicode support by Jim Park -- 08/13/2007
 */

#ifndef ___WRITER__H___
#define ___WRITER__H___

#include "exehead/config.h"
#include "growbuf.h"
#include "crc32.h"
#include <stdio.h>
#include "tchar.h"

class writer_target_info {
public:
  writer_target_info(bool Unicode = 0, bool Cpu64 = 0) { m_f = (Unicode ? F_U : 0) | (Cpu64 ? F_64 : 0); }
  bool is_unicode() const { return !!(m_f & F_U); }
  bool is_64bit() const { return !!(m_f & F_64); }
protected:
  enum { F_U = 0x01, F_64 = 0x80 };
  BYTE m_f;
};

class writer_sink {
public:
  writer_sink(const writer_target_info&ti) : m_ti(ti) {}
  virtual ~writer_sink() {}

  virtual void write_byte(const unsigned char b);
  virtual void write_short(const short s);
  virtual void write_int(const int i);
  virtual void write_int64(const INT64 i);
  virtual void write_int_array(const int i[], const size_t len);
  virtual void write_string(const TCHAR *s, size_t size);
  virtual void write_growbuf(const IGrowBuf *b);

  virtual void write_data(const void *data, const size_t size) = 0;
  virtual const writer_target_info& get_target_info() const { return m_ti; }
protected:
  writer_target_info m_ti;
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
  typedef IGrowBuf sink_type;
  growbuf_writer_sink(sink_type *buf, const writer_target_info&ti) : writer_sink(ti), m_buf(buf) {  }

  virtual void write_data(const void *data, const size_t size);

private:
  sink_type *m_buf;

};

class file_writer_sink : public writer_sink {
public:
  file_writer_sink(FILE *fp, const writer_target_info&ti) : writer_sink(ti), m_fp(fp) {}

  virtual void write_data(const void *data, const size_t size);

private:
  FILE *m_fp;

};

#ifdef NSIS_CONFIG_CRC_SUPPORT
class crc_writer_sink : public writer_sink {
public:
  crc_writer_sink(crc32_t *crc) : writer_sink(writer_target_info()), m_crc(crc) {}

  virtual void write_data(const void *data, const size_t size);

private:
  crc32_t *m_crc;

};
#endif

#endif//!___WRITER__H___
