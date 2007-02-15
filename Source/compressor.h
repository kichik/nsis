/*
 * compressor.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef __COMPRESSOR_H__
#define __COMPRESSOR_H__

#define C_OK 0
#define C_FINISHED 1

#define C_FINISH true

class ICompressor {
  public:
    virtual ~ICompressor() {}

    virtual int Init(int level, unsigned int dict_size) = 0;
    virtual int End() = 0;
    virtual int Compress(bool finish) = 0;

    virtual void SetNextIn(char *in, unsigned int size) = 0;
    virtual void SetNextOut(char *out, unsigned int size) = 0;

    virtual char* GetNextOut() = 0;

    virtual unsigned int GetAvailIn() = 0;
    virtual unsigned int GetAvailOut() = 0;

    virtual const char* GetName() = 0;

    virtual const char* GetErrStr(int err) = 0;
};

#endif
