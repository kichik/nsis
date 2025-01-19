// validateunicode.h
//
// This file is a part of Unicode NSIS.
//
// Copyright (C) 2009-2025 Jim Park
//
// Licensed under the zlib/libpng license (the "License");
// you may not use this file except in compliance with the License.
//
// This software is provided 'as-is', without any expressed or implied
// warranty.
//
// This class can be used to check a buffer to see if it has the expected
// Unicode encoding and look for byte order marks.

#ifndef _VALIDATEUNICODE_
#define _VALIDATEUNICODE_

#include "tchar.h"
#include <stdio.h>

class CValidateUnicode
{
	public:

		// Enum type for each Unicode encoding.
		enum FILE_TYPE
		{
			UTF_8 = 0,
			UTF_16LE,
			UTF_16BE,
			UTF_32LE,
			UTF_32BE,
			UNKNOWN
		};

		// Make sure that the buffer contains valid UTF-8 encoding.
		static int ValidateUTF8(unsigned char* buf, size_t characters);

		// Make sure that the buffer contains valid UTF-16LE encoding.
		static bool ValidateUTF16LE(unsigned char* buf, size_t bytes);

		// Make sure that the buffer contains valid UTF-16BE encoding.
		static bool ValidateUTF16BE(unsigned char* buf, size_t bytes);

		// Make sure that the buffer contains valid UTF-16 encoding.
		static bool ValidateUTF16(unsigned short* buf, size_t characters);

		// Does the buffer have a byte order mark?  And if so, what does it say?
		static FILE_TYPE CheckBOM(unsigned char* buf, size_t bytes);

		// Convert a FILE_TYPE enum to a string.
		static const TCHAR* TypeToName(FILE_TYPE ftype);

	protected:

		// Given the initial byte of a UTF-8 character, how many bytes are to
		// follow?
		static int GetBytesToFollow(unsigned char ch);
};

#endif
