// LZOutWindow.h

#ifndef __LZOUTWINDOW_H
#define __LZOUTWINDOW_H

#include "Types.h"
#include "LZMAState.h"
#include "LZMAConf.h"

class CLZOutWindow
{
public:
  BYTE  *_buffer;
  UINT32 _pos;
  UINT32 _windowSize;
  UINT32 _streamPos;
  CLZMAStateP m_lzmaState;
  void Init(CLZMAStateP lzmaState)
  {
    m_lzmaState = lzmaState;
    _buffer = (LPBYTE) lzmaState->Dictionary;
    _windowSize = lzmaState->DictionarySize;
    _streamPos = 0;
    _pos = 0;
  }
  void CopyBackBlock(UINT32 fromPos, int len)
  {
    if (fromPos >= _windowSize)
      fromPos += _windowSize;
    while (len--)
    {
      _buffer[_pos++] = _buffer[fromPos++];
      if (fromPos >= _windowSize)
        fromPos = 0;
      Flush();
    }
  }
  void PutOneByte(BYTE b)
  {
    _buffer[_pos++] = b;
    Flush();
  }
  void Flush()
  {
    UINT32 size = _pos - _streamPos;
    if (size < 65536 && _pos < _windowSize)
      return;

    CLZMAStateP lzmaState = m_lzmaState;
    while (size--)
    {
      if (!lzmaState->avail_out)
      {
        LZMAGetIO(lzmaState);
      }
      *lzmaState->next_out = _buffer[_streamPos];
      lzmaState->next_out++;
      lzmaState->avail_out--;
      _streamPos++;
    }
    if (_pos >= _windowSize)
    {
      _pos = 0;
      _streamPos = 0;
    }
  }
  BYTE GetOneByte(UINT32 index) const
  {
    UINT32 pos = _pos + index;
    if (pos >= _windowSize)
      pos += _windowSize;
    return _buffer[pos];
  }
};

#endif
