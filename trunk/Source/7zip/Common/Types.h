// Common/Types.h

// #pragma once

#ifndef __COMMON_TYPES_H
#define __COMMON_TYPES_H

#ifdef _WIN32
#  include <basetsd.h>
#else
#  include "../../Platform.h"
#endif

typedef unsigned char   UINT8;
typedef unsigned short  UINT16;
typedef short INT16;
#ifndef _WINDOWS_ 
  // typedef unsigned long UINT32;
  typedef UINT8 BYTE;
#endif

#endif

