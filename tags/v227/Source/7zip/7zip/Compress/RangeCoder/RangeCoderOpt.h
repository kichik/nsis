/*
 * RangeCoderOpt.h
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

#ifndef __COMPRESS_RANGECODER_OPT_H
#define __COMPRESS_RANGECODER_OPT_H

#define RC_INIT_VAR \
  UInt32 range = rangeDecoder->Range; \
  UInt32 code = rangeDecoder->Code;        

#define RC_FLUSH_VAR \
  rangeDecoder->Range = range; \
  rangeDecoder->Code = code;

#define RC_NORMALIZE \
  if (range < NCompress::NRangeCoder::kTopValue) \
    { code = (code << 8) | rangeDecoder->Stream.ReadByte(); range <<= 8; }

#define RC_GETBIT2(numMoveBits, prob, mi, A0, A1) \
  { UInt32 bound = (range >> NCompress::NRangeCoder::kNumBitModelTotalBits) * prob; \
  if (code < bound) \
  { A0; range = bound; \
    prob += (NCompress::NRangeCoder::kBitModelTotal - prob) >> numMoveBits; \
    mi <<= 1; } \
  else \
  { A1; range -= bound; code -= bound; prob -= (prob) >> numMoveBits; \
    mi = (mi + mi) + 1; }} \
  RC_NORMALIZE

#define RC_GETBIT(numMoveBits, prob, mi) RC_GETBIT2(numMoveBits, prob, mi, ; , ;)

#endif
