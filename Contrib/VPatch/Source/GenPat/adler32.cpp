/* adler32.c -- compute the Adler-32 checksum of a data stream
 * Copyright (C) 1995-2003 Mark Adler

  THIS IS A MODIFIED VERSION OF THE ORIGINAL ZLIB adler32.c!

  The following was copied from zlib.h:

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Reviewed for Unicode support by Jim Park -- 08/29/2007
  (nothing changed.)
 */

#include "adler32.h"
#include <limits.h>
namespace zlib {
#include <zlib.h>
}

namespace Checksum {

  /* ========================================================================= */
  uLong adler32(uLong adler, const Byte *buf, uInt len) {
      return zlib::adler32(adler, buf, len);
  }
}
