// DLLExports.cpp

// #include "StdAfx.h"

#ifdef WIN32
#  include <objbase.h>
#  include <initguid.h>
#endif

#include "../Platform.h"

#define INITGUID
#include "7zip/ICoder.h"
#include "7zip/Compress/LZ/IMatchFinder.h"
