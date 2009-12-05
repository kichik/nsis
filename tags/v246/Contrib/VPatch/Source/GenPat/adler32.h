//---------------------------------------------------------------------------
// Adler32
//---------------------------------------------------------------------------

#if !defined(Adler32_H)
  #define Adler32_H

  namespace Checksum {
    typedef unsigned int   uInt;  /* 16 bits or more */
    typedef unsigned long  uLong; /* 32 bits or more */
    typedef unsigned char  Byte;  /* 8 bits */
    
    uLong adler32(uLong adler, const Byte *buf, uInt len);
  }

#endif // Adler32_H


