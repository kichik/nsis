// validateunicode.cpp
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

#ifdef _UNICODE

#include "validateunicode.h"
#include <vector>

int CValidateUnicode::ValidateUTF8(unsigned char* buf, size_t characters)
{
	bool hasNonAscii = false;
	int bytesToFollow = 0;

	for ( ; characters != 0 ; --characters)
	{
        unsigned char ch = *buf++;
        if (bytesToFollow != 0) // in the middle of a multi-byte sequence?
        {
            if ((ch & 0xC0) != 0x80)
		    	return 0; // we expected a continuation byte
            hasNonAscii = true;
            --bytesToFollow;
        }
        else if (ch & 0x80)
        {
            if ((ch & 0xC0) == 0x80)
		    	return 0; // continuation byte outside multi-byte sequence
            else if ((ch & 0xE0) == 0xC0)
		    	bytesToFollow = 1;
            else if ((ch & 0xF0) == 0xE0)
		    	bytesToFollow = 2;
            else if ((ch & 0xF8) == 0xF0)
		    	bytesToFollow = 3;
            else
                return 0; // byte is invalid UTF-8 (outside RFC 3629)

        }
        else if (ch == 0 && characters != 1)
	        return 0; // NUL character in the middle of the buffer
	}
    if (bytesToFollow != 0)
        return 0; // end of buffer in the middle of a multi-byte sequence
    return hasNonAscii ? 2 : 1;
}

bool CValidateUnicode::ValidateUTF16LE(unsigned char* buf, size_t bytes)
{
	// We need to make sure the endianness matches the processor.
	// Intel x86 is little endian.
	return ValidateUTF16((unsigned short*)(buf), bytes/2);
}

bool CValidateUnicode::ValidateUTF16BE(unsigned char* buf, size_t bytes)
{
	std::vector<unsigned short> correctedBuf(bytes/2);

	for (size_t i = 0; i < bytes; i += 2)
	{
		correctedBuf[i/2] = buf[i] << 8 | buf[i+1];
	}

	return ValidateUTF16(&correctedBuf[0], correctedBuf.size());
}

bool CValidateUnicode::ValidateUTF16(unsigned short* buf, size_t characters)
{
	unsigned short ch;
	bool valid = true;

	while (valid && characters > 0)
	{
		// Last character may be 0.
		if ((ch = *buf) == 0 && characters != 1)
		{
			valid = false;
		}
		else if (ch >= 0xd800 && ch <= 0xdbff)
		{
			unsigned short trailing = *(++buf);
			--characters;
			// Unpaired leading surrogate found?
			if (trailing < 0xdc00 || trailing > 0xdfff)
			{
				valid = false;
			}
			// Invalid surrogate pairs found?
			else if ((ch == 0xd83f ||
						 ch == 0xd87f ||
						 ch == 0xd8bf ||
						 ch == 0xd8ff ||
						 ch == 0xd93f ||
						 ch == 0xd97f ||
						 ch == 0xd9bf ||
						 ch == 0xd9ff ||
						 ch == 0xda3f ||
						 ch == 0xdA7f ||
						 ch == 0xdabf ||
						 ch == 0xdaff ||
						 ch == 0xdb3f ||
						 ch == 0xdb7f ||
						 ch == 0xdbbf ||
						 ch == 0xdbff)
					  	&&
				     	(trailing == 0xdffe || trailing == 0xdfff))
			{
				valid = false;
			}
		}
		// Unpaired trailing surrogate!
		else if (ch >= 0xdc00 && ch <= 0xdfff)
		{
			valid = false;
		}
		// Invalid values
		else if (ch == 0xfffe || ch == 0xffff ||
				   (ch >= 0xfdd0 && ch <= 0xfdef))
		{
			valid = false;
		}

		++buf;
	   --characters;
	}

	return valid;
}

CValidateUnicode::FILE_TYPE CValidateUnicode::CheckBOM(
	unsigned char* buf,
	size_t         bytes)
{
	FILE_TYPE result = UNKNOWN;

	if (bytes >= 2)
	{
		if (buf[0] == 0xff && buf[1] == 0xfe)
		{
			result = UTF_16LE;
		}
		else if (buf[0] == 0xfe && buf[1] == 0xff)
		{
			result = UTF_16BE;
		}
		else if (bytes >= 3 &&
			    	buf[0] == 0xef &&
					buf[1] == 0xbb &&
					buf[2] == 0xbf)
		{
			result = UTF_8;
		}
		else if (bytes >= 4)
		{
			if (buf[0] == 0 &&
				 buf[1] == 0 &&
				 buf[2] == 0xfe &&
				 buf[3] == 0xff)
			{
				result = UTF_32BE;
			}
			else if (buf[0] == 0xff &&
					   buf[1] == 0xfe &&
						buf[2] == 0 &&
						buf[3] == 0)
			{
				result = UTF_32LE;
			}
		}
	}

	return result;
}

const TCHAR* CValidateUnicode::TypeToName(CValidateUnicode::FILE_TYPE ftype)
{
	static const TCHAR* names[] =
	{
		_T("UTF-8"),
		_T("UTF-16LE"),
		_T("UTF-16BE"),
		_T("UTF-32LE"),
		_T("UTF-32BE"),
		_T("UNKNOWN")
	};

	return names[ftype];
}

#endif
