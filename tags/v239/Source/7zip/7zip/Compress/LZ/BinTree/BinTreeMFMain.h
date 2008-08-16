/*
 * BinTreeMFMain.h
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

// #include "StdAfx.h"

// #include "BinTreeMF.h"
#include "BinTreeMain.h"

namespace BT_NAMESPACE {

void CInTree2::BeforeMoveBlock()
{
  if (_callback)
    _callback->BeforeChangingBufferPos();
  CInTree::BeforeMoveBlock();
}

void CInTree2::AfterMoveBlock()
{
  CInTree::AfterMoveBlock();
  if (_callback)
    _callback->AfterChangingBufferPos();
}

STDMETHODIMP CMatchFinderBinTree::Init(ISequentialInStream *stream)
  { return _matchFinder.Init(stream); }

STDMETHODIMP_(void) CMatchFinderBinTree::ReleaseStream()
{ 
  // _matchFinder.ReleaseStream(); 
}

STDMETHODIMP CMatchFinderBinTree::MovePos()
  { return _matchFinder.MovePos(); }

STDMETHODIMP_(Byte) CMatchFinderBinTree::GetIndexByte(Int32 index)
  { return _matchFinder.GetIndexByte(index); }

STDMETHODIMP_(UInt32) CMatchFinderBinTree::GetMatchLen(Int32 index, 
    UInt32 back, UInt32 limit)
  { return _matchFinder.GetMatchLen(index, back, limit); }

STDMETHODIMP_(UInt32) CMatchFinderBinTree::GetNumAvailableBytes()
  { return _matchFinder.GetNumAvailableBytes(); }
  
STDMETHODIMP CMatchFinderBinTree::Create(UInt32 sizeHistory, 
      UInt32 keepAddBufferBefore, UInt32 matchMaxLen, 
      UInt32 keepAddBufferAfter)
{ 
  UInt32 windowReservSize = (sizeHistory + keepAddBufferBefore + 
      matchMaxLen + keepAddBufferAfter) / 2 + 256;
  // try 
  {
    return _matchFinder.Create(sizeHistory, keepAddBufferBefore, 
        matchMaxLen, keepAddBufferAfter, windowReservSize); 
  }
  /*
  catch(...)
  {
    return E_OUTOFMEMORY;
  }
  */
}

STDMETHODIMP_(UInt32) CMatchFinderBinTree::GetLongestMatch(UInt32 *distances)
  { return _matchFinder.GetLongestMatch(distances); }

STDMETHODIMP_(void) CMatchFinderBinTree::DummyLongestMatch()
  { _matchFinder.DummyLongestMatch(); }

STDMETHODIMP_(const Byte *) CMatchFinderBinTree::GetPointerToCurrentPos()
{
  return _matchFinder.GetPointerToCurrentPos();
}

// IMatchFinderSetCallback
STDMETHODIMP CMatchFinderBinTree::SetCallback(IMatchFinderCallback *callback)
{
  _matchFinder.SetCallback(callback);
  return S_OK;
}


}
