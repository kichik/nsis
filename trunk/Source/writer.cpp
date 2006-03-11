#include "exehead/config.h"
#include "writer.h"
#include "growbuf.h"
#include "util.h"
#include <string.h>
#include <stdlib.h>
#include <stdexcept>

void writer_sink::write_byte(const unsigned char b)
{
  write_data(&b, 1);
}

void writer_sink::write_short(const short s)
{
  short fs = FIX_ENDIAN_INT16(s);
  write_data(&fs, sizeof(short));
}

void writer_sink::write_int(const int i)
{
  int fi = FIX_ENDIAN_INT32(i);
  write_data(&fi, sizeof(int));
}

void writer_sink::write_int_array(const int i[], const size_t len)
{
  for (size_t l = 0; l < len; l++)
  {
    write_int(i[l]);
  }
}

void writer_sink::write_string(const char *s)
{
  write_data(s, strlen(s) + 1);
}

void writer_sink::write_string(const char *s, const size_t size)
{
  char *wb = new char[size];
  memset(wb, 0, size);
  strncpy(wb, s, size);
  write_data(wb, size);
  delete [] wb;
}

void writer_sink::write_growbuf(const IGrowBuf *b)
{
  write_data(b->get(), b->getlen());
}

void growbuf_writer_sink::write_data(const void *data, const size_t size)
{
  m_buf->add(data, size);
}

void file_writer_sink::write_data(const void *data, const size_t size)
{
  if (fwrite(data, 1, size, m_fp) != size)
  {
    throw std::runtime_error("error writing");
  }
}

#ifdef NSIS_CONFIG_CRC_SUPPORT
extern "C" unsigned long NSISCALL CRC32(unsigned long crc, const unsigned char *buf, unsigned int len);

void crc_writer_sink::write_data(const void *data, const size_t size)
{
  *m_crc = CRC32(*m_crc, (const unsigned char *) data, size);
}
#endif
