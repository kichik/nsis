#include "exedata.h"

//#ifndef _DEBUG
#ifdef NSIS_CONFIG_COMPONENTPAGE
#include "exehead/Release-zlib/bitmap1.h"
#endif
#include "exehead/Release-zlib/icon.h"
#include "exehead/Release-zlib/unicon.h"
#include "exehead/Release-zlib/exehead_zlib.h"
#include "exehead/Release-bzip2/exehead_bzip2.h"
#include "exehead/Release-lzma/exehead_lzma.h"
/*#else
#ifdef NSIS_CONFIG_COMPONENTPAGE
#include "exehead/Debug-zlib/bitmap1.h"
#endif
#include "exehead/Debug-zlib/icon.h"
#include "exehead/Debug-zlib/unicon.h"
#include "exehead/Debug-zlib/exehead_zlib.h"
#include "exehead/Debug-bzip2/exehead_bzip2.h"
#include "exehead/Debug-lzma/exehead_lzma.h"
#endif*/

size_t zlib_exehead_size=sizeof(zlib_exehead);
size_t bzip2_exehead_size=sizeof(bzip2_exehead);
size_t lzma_exehead_size=sizeof(lzma_exehead);
size_t exehead_original_size=0;

size_t icondata_size=sizeof(icon_data)-22;
size_t unicondata_size=sizeof(unicon_data)-22;
