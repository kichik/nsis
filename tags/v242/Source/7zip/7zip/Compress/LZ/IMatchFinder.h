/*
 * IMatchFinder.h
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

#ifndef __IMATCHFINDER_H
#define __IMATCHFINDER_H

struct IInWindowStream: public IUnknown
{
  STDMETHOD(SetStream)(ISequentialInStream *inStream) PURE;
  STDMETHOD_(void, ReleaseStream)() PURE;
  STDMETHOD(Init)() PURE;
  STDMETHOD_(Byte, GetIndexByte)(Int32 index) PURE;
  STDMETHOD_(UInt32, GetMatchLen)(Int32 index, UInt32 distance, UInt32 limit) PURE;
  STDMETHOD_(UInt32, GetNumAvailableBytes)() PURE;
  STDMETHOD_(const Byte *, GetPointerToCurrentPos)() PURE;
  STDMETHOD_(Int32, NeedChangeBufferPos)(UInt32 numCheckBytes) PURE;
  STDMETHOD_(void, ChangeBufferPos)() PURE;
};
 
struct IMatchFinder: public IInWindowStream
{
  STDMETHOD(Create)(UInt32 historySize, UInt32 keepAddBufferBefore, 
      UInt32 matchMaxLen, UInt32 keepAddBufferAfter) PURE;
  STDMETHOD(GetMatches)(UInt32 *distances) PURE;
  STDMETHOD(Skip)(UInt32 num) PURE;
};

struct IMatchFinderSetNumPasses
{
  virtual void SetNumPasses(UInt32 numPasses) PURE;
};

#endif
