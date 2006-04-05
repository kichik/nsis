#include "fileform.h"
#include "exehead/config.h"
#include "Platform.h"

#include <cassert>

// these functions MUST be synchronized with the structures in Source/exehead/fileform.h !
// data must be written in the same order it's defined in Source/exehead/fileform.h
// in the future, i hope to get one of the two automtaically generated from the other

void firstheader_writer::write(const firstheader *data)
{
  m_sink->write_int(data->flags);
  m_sink->write_int(data->siginfo);
  m_sink->write_int_array(data->nsinst, 3);
  m_sink->write_int(data->length_of_header);
  m_sink->write_int(data->length_of_all_following_data);
}

void block_header_writer::write(const block_header *data)
{
  m_sink->write_int(data->offset);
  m_sink->write_int(data->num);
}

void header_writer::write(const header *data)
{
  m_sink->write_int(data->flags);

  block_header_writer bw(writer::m_sink);
  for (int i = 0; i < BLOCKS_NUM; i++)
  {
    bw.write(&data->blocks[i]);
  }

  m_sink->write_int(data->install_reg_rootkey);
  m_sink->write_int(data->install_reg_key_ptr);
  m_sink->write_int(data->install_reg_value_ptr);

#ifdef NSIS_SUPPORT_BGBG
  m_sink->write_int(data->bg_color1);
  m_sink->write_int(data->bg_color2);
  m_sink->write_int(data->bg_textcolor);
#endif

#ifdef NSIS_CONFIG_VISIBLE_SUPPORT
  m_sink->write_int(data->lb_bg);
  m_sink->write_int(data->lb_fg);
#endif

  m_sink->write_int(data->langtable_size);

#ifdef NSIS_CONFIG_LICENSEPAGE
  m_sink->write_int(data->license_bg);
#endif//NSIS_CONFIG_LICENSEPAGE

#ifdef NSIS_SUPPORT_CODECALLBACKS
  m_sink->write_int(data->code_onInit);
  m_sink->write_int(data->code_onInstSuccess);
  m_sink->write_int(data->code_onInstFailed);
  m_sink->write_int(data->code_onUserAbort);
#ifdef NSIS_CONFIG_ENHANCEDUI_SUPPORT
  m_sink->write_int(data->code_onGUIInit);
  m_sink->write_int(data->code_onGUIEnd);
  m_sink->write_int(data->code_onMouseOverSection);
#endif//NSIS_CONFIG_ENHANCEDUI_SUPPORT
  m_sink->write_int(data->code_onVerifyInstDir);
#ifdef NSIS_CONFIG_COMPONENTPAGE
  m_sink->write_int(data->code_onSelChange);
#endif//NSIS_CONFIG_COMPONENTPAGE
#ifdef NSIS_SUPPORT_REBOOT
  m_sink->write_int(data->code_onRebootFailed);
#endif//NSIS_SUPPORT_REBOOT
#endif//NSIS_SUPPORT_CODECALLBACKS

#ifdef NSIS_CONFIG_COMPONENTPAGE
  m_sink->write_int_array(data->install_types, NSIS_MAX_INST_TYPES + 1);
#endif

  m_sink->write_int(data->install_directory_ptr);
  m_sink->write_int(data->install_directory_auto_append);
}

void section_writer::write(const section *data)
{
  m_sink->write_int(data->name_ptr);
  m_sink->write_int(data->install_types);
  m_sink->write_int(data->flags);
  m_sink->write_int(data->code);
  m_sink->write_int(data->code_size);
  m_sink->write_int(data->size_kb);
  m_sink->write_string(data->name, NSIS_MAX_STRLEN);
}

void entry_writer::write(const entry *data)
{
  m_sink->write_int(data->which);
  m_sink->write_int_array(data->offsets, MAX_ENTRY_OFFSETS);
}

void page_writer::write(const page *data)
{
  m_sink->write_int(data->dlg_id);
  m_sink->write_int(data->wndproc_id);

#ifdef NSIS_SUPPORT_CODECALLBACKS
  m_sink->write_int(data->prefunc);
  m_sink->write_int(data->showfunc);
  m_sink->write_int(data->leavefunc);
#endif //NSIS_SUPPORT_CODECALLBACKS

  m_sink->write_int(data->flags);

  m_sink->write_int(data->caption);
  m_sink->write_int(data->back);
  m_sink->write_int(data->next);
  m_sink->write_int(data->clicknext);
  m_sink->write_int(data->cancel);

  m_sink->write_int_array(data->parms, 5);
}

void ctlcolors_writer::write(const ctlcolors *data)
{
  m_sink->write_int(data->text);
  m_sink->write_int(data->bkc);
  m_sink->write_int(data->lbStyle);
  m_sink->write_int((int) data->bkb);
  m_sink->write_int(data->bkmode);
  m_sink->write_int(data->flags);
}

void LOGFONT_writer::write(const LOGFONT *data)
{
  m_sink->write_int(data->lfHeight);
  m_sink->write_int(data->lfWidth);
  m_sink->write_int(data->lfEscapement);
  m_sink->write_int(data->lfOrientation);
  m_sink->write_int(data->lfWeight);
  m_sink->write_byte(data->lfItalic);
  m_sink->write_byte(data->lfUnderline);
  m_sink->write_byte(data->lfStrikeOut);
  m_sink->write_byte(data->lfCharSet);
  m_sink->write_byte(data->lfOutPrecision);
  m_sink->write_byte(data->lfClipPrecision);
  m_sink->write_byte(data->lfQuality);
  m_sink->write_byte(data->lfPitchAndFamily);
  m_sink->write_string(data->lfFaceName, LF_FACESIZE);
}

void lang_table_writer::write(const unsigned char *data)
{
  assert(sizeof(LANGID) == sizeof(short));

  m_sink->write_short(* (short *) data);
  data += sizeof(short);
  m_sink->write_int_array((int *) data, m_lang_strings + 2);
}

void lang_table_writer::write_block(IGrowBuf *buf, writer_sink *sink, const size_t table_size)
{
  unsigned char *tables = (unsigned char *) buf->get();
  size_t lang_strings = ( table_size - 2 * sizeof(int) - sizeof(LANGID) ) / sizeof(int);
  size_t l = buf->getlen() / table_size;
  lang_table_writer writer(sink, lang_strings);
  for (size_t i = 0; i < l; i++)
  {
    writer.write(tables + i * table_size);
  }
}
