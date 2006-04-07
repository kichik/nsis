#ifndef ___MAKENSIS_FILEFORM_H___
#define ___MAKENSIS_FILEFORM_H___

#include "exehead/fileform.h"
#include "writer.h"

#define DECLARE_WRITER(x) \
  class x##_writer : public writer \
  { \
  public: \
    x##_writer(writer_sink *sink) : writer(sink) {} \
    void write(const x *data); \
    static void write_block(IGrowBuf *buf, writer_sink *sink) \
    { \
      x *arr = (x *) buf->get(); \
      size_t l = buf->getlen() / sizeof(x); \
      x##_writer writer(sink); \
      for (size_t i = 0; i < l; i++) \
      { \
        writer.write(&arr[i]); \
      } \
    } \
  }

DECLARE_WRITER(firstheader);
DECLARE_WRITER(block_header);
DECLARE_WRITER(header);
DECLARE_WRITER(section);
DECLARE_WRITER(entry);
DECLARE_WRITER(page);
DECLARE_WRITER(ctlcolors);
DECLARE_WRITER(LOGFONT);

class lang_table_writer : public writer
{
public:
  lang_table_writer(writer_sink *sink, const size_t lang_strings) :
    writer(sink), m_lang_strings(lang_strings) {}
  void write(const unsigned char *data);
  static void write_block(IGrowBuf *buf, writer_sink *sink, const size_t table_size);

private:
  size_t m_lang_strings;

};

#endif//!___MAKENSIS_FILEFORM_H___
