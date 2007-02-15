/*
 * BinTree.h
 * 
 * This file is a part of LZMA compression module for NSIS.
 * 
 * Original LZMA SDK Copyright (C) 1999-2006 Igor Pavlov
 * Modifications Copyright (C) 2003-2006 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the Common Public License version 1.0 (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "../LZInWindow.h"
#include "../IMatchFinder.h"
 
namespace BT_NAMESPACE {

typedef UInt32 CIndex;
const UInt32 kMaxValForNormalize = (UInt32(1) << 31) - 1;

class CMatchFinder: 
  public IMatchFinder,
  public CLZInWindow,
  public CMyUnknownImp,
  public IMatchFinderSetNumPasses
{
  UInt32 _cyclicBufferPos;
  UInt32 _cyclicBufferSize; // it must be historySize + 1
  UInt32 _matchMaxLen;
  CIndex *_hash;
  CIndex *_son;
  UInt32 _hashMask;
  UInt32 _cutValue;
  UInt32 _hashSizeSum;

  void Normalize();
  void FreeThisClassMemory();
  void FreeMemory();

  MY_UNKNOWN_IMP

  STDMETHOD(SetStream)(ISequentialInStream *inStream);
  STDMETHOD_(void, ReleaseStream)();
  STDMETHOD(Init)();
  HRESULT MovePos();
  STDMETHOD_(Byte, GetIndexByte)(Int32 index);
  STDMETHOD_(UInt32, GetMatchLen)(Int32 index, UInt32 back, UInt32 limit);
  STDMETHOD_(UInt32, GetNumAvailableBytes)();
  STDMETHOD_(const Byte *, GetPointerToCurrentPos)();
  STDMETHOD_(Int32, NeedChangeBufferPos)(UInt32 numCheckBytes);
  STDMETHOD_(void, ChangeBufferPos)();

  STDMETHOD(Create)(UInt32 historySize, UInt32 keepAddBufferBefore, 
      UInt32 matchMaxLen, UInt32 keepAddBufferAfter);
  STDMETHOD(GetMatches)(UInt32 *distances);
  STDMETHOD(Skip)(UInt32 num);

public:
  CMatchFinder();
  virtual ~CMatchFinder();
  virtual void SetNumPasses(UInt32 numPasses) { _cutValue = numPasses; }
};

}
