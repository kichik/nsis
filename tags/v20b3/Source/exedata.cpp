#include "exedata.h"

#ifdef NSIS_CONFIG_COMPONENTPAGE
#include "exehead/Release-zlib/bitmap1.h"
#endif
#include "exehead/Release-zlib/icon.h"
#include "exehead/Release-zlib/unicon.h"
#include "exehead/Release-zlib/exehead_zlib.h"
#include "exehead/Release-bzip2/exehead_bzip2.h"
// Changed by Amir Szekely 31st July 2002
int zlib_exeheader_size=sizeof(zlib_header_data);
int bzip2_exeheader_size=sizeof(bzip2_header_data);
int exeheader_size=0;

// Changed by Amir Szekely 8th July 2002
int icondata_size=sizeof(icon_data)-22;
int unicondata_size=sizeof(unicon_data)-22;
