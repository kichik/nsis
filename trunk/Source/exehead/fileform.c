#include <windows.h>
#include "fileform.h"
#include "util.h"
#include "state.h"

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
#ifdef NSIS_COMPRESS_USE_ZLIB
#include "../zlib/zlib.h"
#endif

#ifdef NSIS_COMPRESS_USE_BZIP2
#include "../bzip2/bzlib.h"
static int bz2_needreinit;
#define z_stream bz_stream
#define inflateInit(x) { if (BZ2_bzDecompressInit(x)<0) return -1; }
#define inflate(x) BZ2_bzDecompress(x)
#define inflateReset(x) { if (bz2_needreinit) { BZ2_bzDecompressEnd(x); inflateInit(x); } bz2_needreinit=1; }
#define Z_OK BZ_OK
#define Z_STREAM_END BZ_STREAM_END
#endif//NSIS_COMPRESS_USE_BZIP2

#endif

#include "ui.h"

static char *g_db_strtab;

static int g_db_offset;
HANDLE g_db_hFile;

#ifdef NSIS_COMPRESS_WHOLE
HANDLE dbd_hFile=INVALID_HANDLE_VALUE;
static int dbd_size, dbd_pos, dbd_srcpos, dbd_fulllen;
#endif//NSIS_COMPRESS_WHOLE

int isheader(firstheader *h)
{
  if ((h->flags & (~FH_FLAGS_MASK)) ||
      h->siginfo != FH_SIG ||
      h->nsinst[2] != FH_INT3 ||
      h->nsinst[1] != FH_INT2 ||
      h->nsinst[0] != FH_INT1) return 0;
  return h->length_of_all_following_data;
}


#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
static z_stream g_inflate_stream;
#endif

int loadHeaders(void)
{
  DWORD r;
  void *data;
  firstheader h;

  if (!ReadFile(g_db_hFile,(LPVOID)&h,sizeof(h),&r,NULL) || r != sizeof(h) || !isheader(&h)) return -1;

  data=(void*)GlobalAlloc(GMEM_FIXED,h.length_of_header);

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  inflateInit(&g_inflate_stream);
#endif

#ifdef NSIS_COMPRESS_WHOLE
  inflateReset(&g_inflate_stream);

  {
    char fn[MAX_PATH],fno[MAX_PATH];
    GetTempPath(sizeof(fn),fn);
    GetTempFileName(fn,"nsi",0,fno);
    dbd_hFile=CreateFile(fno,GENERIC_WRITE|GENERIC_READ,0,NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_TEMPORARY|FILE_FLAG_DELETE_ON_CLOSE,NULL);
    if (dbd_hFile == INVALID_HANDLE_VALUE) 
    {
      my_MessageBox("Error writing temp file",MB_OK);
      return -1;
    }
  }
  dbd_srcpos=SetFilePointer(g_db_hFile,0,NULL,FILE_CURRENT);
  dbd_fulllen=dbd_srcpos-sizeof(h)+h.length_of_all_following_data-((h.flags&FH_FLAGS_CRC)?4:0);
#endif

  if (GetCompressedDataFromDataBlockToMemory(-1,data,h.length_of_header) != h.length_of_header)
  {
    my_MessageBox("Error reading installer info block",MB_OK);
    GlobalFree((HGLOBAL)data);
    return -1;
  }

#ifndef NSIS_COMPRESS_WHOLE
  g_db_offset=SetFilePointer(g_db_hFile,0,NULL,FILE_CURRENT);
#else
  g_db_offset=dbd_pos;
#endif

  g_inst_combinedheader=data;

#ifdef NSIS_CONFIG_UNINSTALL_SUPPORT
  if (h.flags&FH_FLAGS_UNINSTALL)
  {
    g_is_uninstaller++;
    g_inst_entry=(entry *) ((g_inst_uninstheader) + 1);
  }
  else
#endif
  {
    g_inst_section=(section *) (g_inst_header + 1);
    g_inst_entry=(entry *) (g_inst_section + g_inst_header->num_sections);
  }
  g_db_strtab = (char *)(g_inst_entry + g_inst_cmnheader->num_entries);
  return 0;
}

const char *GetStringFromStringTab(int offs)
{
  if (offs < 0) return "";
  return g_db_strtab+offs;
}

#define IBUFSIZE 16384
#define OBUFSIZE 32768

// returns -3 if compression error/eof/etc

#ifndef NSIS_COMPRESS_WHOLE

static int _dodecomp(int offset, HANDLE hFileOut, char *outbuf, int outbuflen)
{
  static char inbuffer[IBUFSIZE+OBUFSIZE];
  char *outbuffer;
  int outbuffer_len=outbuf?outbuflen:OBUFSIZE;
  int retval=0;
  int input_len;
  DWORD r;

  outbuffer = outbuf?outbuf:(inbuffer+IBUFSIZE);

  if (offset>=0) 
  {
    /*
    int lp=SetFilePointer(g_db_hFile,0,NULL,FILE_CURRENT);
    if (lp > g_db_offset+offset)
    {
      char buf[1023];
      wsprintf(buf,"going from %d to %d",lp,g_db_offset+offset);
      MessageBox(NULL,buf,"seeking back",MB_OK);
    }
    */
    SetFilePointer(g_db_hFile,g_db_offset+offset,NULL,FILE_BEGIN);
  }

  if (!ReadFile(g_db_hFile,(LPVOID)&input_len,sizeof(int),&r,NULL)) return -3;

#ifdef NSIS_CONFIG_COMPRESSION_SUPPORT
  else if (input_len & 0x80000000) // compressed
  {
    inflateReset(&g_inflate_stream);
    input_len &= 0x7fffffff; // take off top bit.

    while (input_len > 0)
    {
      DWORD r;
      int err;

      if (!ReadFile(g_db_hFile,(LPVOID)inbuffer,min(input_len,IBUFSIZE),&r,NULL)) return -3;

      g_inflate_stream.next_in = inbuffer;
      g_inflate_stream.avail_in = r;
      input_len-=r;

      for (;;)
      {
        int u;
        g_inflate_stream.next_out = outbuffer;
        g_inflate_stream.avail_out = (unsigned int)outbuffer_len;

        err=inflate(&g_inflate_stream);

        if (err<0) return -4;

        u=(char*)g_inflate_stream.next_out - outbuffer;

        if (!u) break;

        if (!outbuf)
        {
          if (!WriteFile(hFileOut,outbuffer,u,&r,NULL) || (int)r != u) return -2;
          retval+=u;
        }
        else
        {
          retval+=u;
          outbuffer_len-=u;
          outbuffer=g_inflate_stream.next_out;
          if (outbuffer_len < 1) return retval;
        }
        if (err==Z_STREAM_END) return retval;
      }
    }
  }
#endif//NSIS_CONFIG_COMPRESSION_SUPPORT
  else
  {
    if (!outbuf)
    {
      while (input_len > 0)
      {
        DWORD t;
        if (!ReadFile(g_db_hFile,(LPVOID)inbuffer,min(input_len,outbuffer_len),&r,NULL)) return -3;
        if (!WriteFile(hFileOut,inbuffer,r,&t,NULL) || r!=t) return -2;
        retval+=r;
        input_len-=r;
      }
    }
    else
    {
      if (!ReadFile(g_db_hFile,(LPVOID)outbuf,min(input_len,outbuflen),&r,NULL)) return -3;
      retval=r;
    }
  }
  return retval;
}
#else//NSIS_COMPRESS_WHOLE

static char _inbuffer[IBUFSIZE];
static char _outbuffer[OBUFSIZE];
static int __ensuredata(int amount)
{
  int needed=amount-(dbd_size-dbd_pos);
  if (needed>0)
  {
    SetFilePointer(g_db_hFile,dbd_srcpos,NULL,FILE_BEGIN);
    SetFilePointer(dbd_hFile,dbd_size,NULL,FILE_BEGIN);
    for (;;)
    {
      int err;
      DWORD or;
      if (!ReadFile(g_db_hFile,(LPVOID)_inbuffer,min(IBUFSIZE,dbd_fulllen-dbd_srcpos),&or,NULL)) return -1;
      dbd_srcpos+=or;
      g_inflate_stream.next_in=_inbuffer;
      g_inflate_stream.avail_in=or;
      do
      {
        DWORD r,t;
        g_inflate_stream.next_out=_outbuffer;
        g_inflate_stream.avail_out=OBUFSIZE;
        err=inflate(&g_inflate_stream);
        if (err<0)
        {
          return -3;
        }
        r=g_inflate_stream.next_out-_outbuffer;
        if (r)
        {
          if (!WriteFile(dbd_hFile,_outbuffer,r,&t,NULL) || r != t)
          {
            return -2;
          }
          dbd_size+=r;
        }
        else if (g_inflate_stream.avail_in || !or) return -3;
        else break;
      }
      while (g_inflate_stream.avail_in);
      if (amount-(dbd_size-dbd_pos) <= 0) break;
    }
    SetFilePointer(dbd_hFile,dbd_pos,NULL,FILE_BEGIN);
  }
  return 0;
}


static int _dodecomp(int offset, HANDLE hFileOut, char *outbuf, int outbuflen)
{
  DWORD r;
  int input_len;
  int retval;
  if (offset>=0)
  {
    dbd_pos=g_db_offset+offset;
    SetFilePointer(dbd_hFile,dbd_pos,NULL,FILE_BEGIN);
  }
  retval=__ensuredata(sizeof(int));
  if (retval<0) return retval;

  if (!ReadFile(dbd_hFile,(LPVOID)&input_len,sizeof(int),&r,NULL) || r!=sizeof(int)) return -3;
  dbd_pos+=sizeof(int);

  retval=__ensuredata(input_len);
  if (retval < 0) return retval;

  if (!outbuf)
  {
    while (input_len > 0)
    {
      DWORD t;
      if (!ReadFile(dbd_hFile,(LPVOID)_inbuffer,min(input_len,IBUFSIZE),&r,NULL)) return -3;
      if (!WriteFile(hFileOut,_inbuffer,r,&t,NULL) || r!=t) return -2;
      retval+=r;
      input_len-=r;
      dbd_pos+=r;
    }
  }
  else
  {
    if (!ReadFile(dbd_hFile,(LPVOID)outbuf,min(input_len,outbuflen),&r,NULL)) return -3;
    retval=r;
    dbd_pos+=r;
  }
  return retval;
}
#endif//NSIS_COMPRESS_WHOLE


int GetCompressedDataFromDataBlock(int offset, HANDLE hFileOut)
{
  return _dodecomp(offset,hFileOut,NULL,0);
}

int GetCompressedDataFromDataBlockToMemory(int offset, char *out, int out_len)
{
  return _dodecomp(offset,NULL,out,out_len);
}
