// Stream/InByte.h

#include "LZMAState.h"

#ifndef __STREAM_INBYTE_H
#define __STREAM_INBYTE_H

class CInBuffer
{
  CLZMAStateP m_lzmaState;
public:
  void Init(CLZMAStateP lzmaState)
  {
    m_lzmaState = lzmaState;
  }
  BYTE ReadByte()
  {
    if (!m_lzmaState->avail_in)
    {
      LZMAGetIO(m_lzmaState);
    }
    if (!m_lzmaState->avail_in)
      return 0;
    m_lzmaState->avail_in--;
    return *m_lzmaState->next_in++;
  }
};

#endif
