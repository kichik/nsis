#include "../exehead/config.h"
#ifdef NSIS_COMPRESS_USE_ZLIB
/* inftrees.c -- generate Huffman trees for efficient decoding
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h 
 */

#include "zutil.h"
#include "inftrees.h"

struct internal_state  {int dummy;}; /* for buggy compilers */

/* simplify the use of the inflate_huft type with some defines */
#define exop word.what.Exop
#define bits word.what.Bits

/* Tables for deflate from PKZIP's appnote.txt. */
local const unsigned short  cplens[31] = { /* Copy lengths for literal codes 257..285 */
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0};
        /* see note #13 above about 258 */
local const unsigned short  cplext[31] = { /* Extra bits for literal codes 257..285 */
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 112, 112}; /* 112==invalid */
local const unsigned short  cpdist[30] = { /* Copy offsets for distance codes 0..29 */
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577};
local const unsigned short  cpdext[30] = { /* Extra bits for distance codes */
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13};

#define BMAX 15         /* maximum bit length of any code */

local int huft_build(
uIntf *b,               /* code lengths in bits (all assumed <= BMAX) */
uInt n,                 /* number of codes (assumed <= 288) */
uInt s,                 /* number of simple-valued codes (0..s-1) */
const unsigned short *d,         /* list of base values for non-simple codes */
const unsigned short *e,         /* list of extra bits for non-simple codes */
inflate_huft * FAR *t,  /* result: starting table */
uIntf *m,               /* maximum lookup bits, returns actual */
inflate_huft *hp,       /* space for trees */
uInt *hn)               /* working area: values in order of bit length */
{
  static uIntf v[288];             /* work area for huft_build */
  uInt a;                       /* counter for codes of length k */
  uInt c[BMAX+1];               /* bit length count table */
  uInt f;                       /* i repeats in table every f entries */
  int g;                        /* maximum code length */
  int h;                        /* table level */
  uInt i;              /* counter, current code */
  uInt j;              /* counter */
  int k;               /* number of bits in current code */
  int l;                        /* bits per table (returned in m) */
  uIntf *p;            /* pointer into c[], b[], or v[] */
  inflate_huft *q;              /* points to current table */
  struct inflate_huft_s r;      /* table entry for structure assignment */
  inflate_huft *u[BMAX];        /* table stack */
  int w;               /* bits before this table == (l * h) */
  uInt x[BMAX+1];               /* bit offsets, then code stack */
  uIntf *xp;                    /* pointer into x */
  int y;                        /* number of dummy codes added */
  uInt z;                       /* number of entries in current table */


  /* Generate counts for each bit length */
  p=c;
  y=16; while (y--) *p++ = 0;
  p = b;  
  i = n;
  do {
    c[*p++]++;                  /* assume all entries <= BMAX */
  } while (--i);
  if (c[0] == n)                /* null input--all zero length codes */
  {
    *t = (inflate_huft *)Z_NULL;
    *m = 0;
    return Z_OK;
  }


  /* Find minimum and maximum length, bound *m by those */
  l = *m;
  for (j = 1; j <= BMAX; j++)
    if (c[j])
      break;
  k = j;                        /* minimum code length */
  if ((uInt)l < j)
    l = j;
  for (i = BMAX; i; i--)
    if (c[i])
      break;
  g = i;                        /* maximum code length */
  if ((uInt)l > i)
    l = i;
  *m = l;


  /* Adjust last length count to fill out codes, if needed */
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return Z_DATA_ERROR;
  if ((y -= c[i]) < 0)
    return Z_DATA_ERROR;
  c[i] += y;


  /* Generate starting offsets into the value table for each length */
  x[1] = j = 0;
  p = c + 1;  xp = x + 2;
  while (--i) {                 /* note that i == g from above */
    *xp++ = (j += *p++);
  }


  /* Make a table of values in order of bit lengths */
  p = b;  i = 0;
  do {
    if ((j = *p++) != 0)
      v[x[j]++] = i;
  } while (++i < n);
  n = x[g];                     /* set n to length of v */


  /* Generate the Huffman codes and for each, make the table entries */
  x[0] = i = 0;                 /* first Huffman code is zero */
  p = v;                        /* grab values in bit order */
  h = -1;                       /* no tables yet--level -1 */
  w = -l;                       /* bits decoded == (l * h) */
  u[0] = (inflate_huft *)Z_NULL;        /* just to keep compilers happy */
  q = (inflate_huft *)Z_NULL;   /* ditto */
  z = 0;                        /* ditto */

  /* go through the bit lengths (k already is bits in shortest code) */
  for (; k <= g; k++)
  {
    a = c[k];
    while (a--)
    {
      /* here i is the Huffman code of length k bits for value *p */
      /* make tables up to required level */
      while (k > w + l)
      {
        h++;
        w += l;                 /* previous table always l bits */

        /* compute minimum size table less than or equal to l bits */
        z = g - w;
        z = z > (uInt)l ? l : z;        /* table size upper limit */
        if ((f = 1 << (j = k - w)) > a + 1)     /* try a k-w bit table */
        {                       /* too few codes for k-w bit table */
          f -= a + 1;           /* deduct codes from patterns left */
          xp = c + k;
          if (j < z)
            while (++j < z)     /* try smaller tables up to z bits */
            {
              if ((f <<= 1) <= *++xp)
                break;          /* enough codes to use up j bits */
              f -= *xp;         /* else deduct codes from patterns */
            }
        }
        z = 1 << j;             /* table entries for j-bit table */

        /* allocate new table */
        if (*hn + z > MANY)     /* (note: doesn't matter for fixed) */
          return Z_MEM_ERROR;   /* not enough memory */
        u[h] = q = hp + *hn;
        *hn += z;

        /* connect to last table, if there is one */
        if (h)
        {
          x[h] = i;             /* save pattern for backing up */
          r.bits = (Byte)l;     /* bits to dump before this table */
          r.exop = (Byte)j;     /* bits in this table */
          j = i >> (w - l);
          r.base = (uInt)(q - u[h-1] - j);   /* offset to this table */
          u[h-1][j] = r;        /* connect to last table */
        }
        else
          *t = q;               /* first table is returned result */
      }

      /* set up table entry in r */
      r.bits = (Byte)(k - w);
      if (p >= v + n)
        r.exop = 128 + 64;      /* out of values--invalid code */
      else if (*p < s)
      {
        r.exop = (Byte)(*p < 256 ? 0 : 32 + 64);     /* 256 is end-of-block */
        r.base = *p++;          /* simple code is just the value */
      }
      else
      {
        r.exop = (Byte)(e[*p - s] + 16 + 64);/* non-simple--look up in lists */
        r.base = d[*p++ - s];
      }

      /* fill code-like entries with r */
      f = 1 << (k - w);
      for (j = i >> w; j < z; j += f)
        q[j] = r;

      /* backwards increment the k-bit code i */
      for (j = 1 << (k - 1); i & j; j >>= 1)
        i ^= j;
      i ^= j;

      /* backup over finished tables */
      while ((i & ((1 << w) - 1)) != x[h])
      {
        h--;                    /* don't need to update q */
        w -= l;
      }
    }
  }


  /* Return Z_BUF_ERROR if we were given an incomplete table */
  return y != 0 && g != 1 ? Z_BUF_ERROR : Z_OK;
}

int inflate_trees_bits(c, bb, tb, hp)
uIntf *c;               /* 19 code lengths */
uIntf *bb;              /* bits tree desired/actual depth */
inflate_huft * FAR *tb; /* bits tree result */
inflate_huft *hp;       /* space for trees */
{
  int r;
  uInt hn = 0;          /* hufts used in space */

  r = huft_build(c, 19, 19, (short *)Z_NULL, (short*)Z_NULL,
                 tb, bb, hp, &hn);
  if (r == Z_BUF_ERROR || *bb == 0)
  {
    return Z_DATA_ERROR;
  }
  return r;
}

int inflate_trees_dynamic(nl, nd, c, bl, bd, tl, td, hp)
uInt nl;                /* number of literal/length codes */
uInt nd;                /* number of distance codes */
uIntf *c;               /* that many (total) code lengths */
uIntf *bl;              /* literal desired/actual bit depth */
uIntf *bd;              /* distance desired/actual bit depth */
inflate_huft * FAR *tl; /* literal/length tree result */
inflate_huft * FAR *td; /* distance tree result */
inflate_huft *hp;       /* space for trees */
{
  int r;
  uInt hn = 0;          /* hufts used in space */

  /* build literal/length tree */
  r = huft_build(c, nl, 257, cplens, cplext, tl, bl, hp, &hn);
  if (r != Z_OK || *bl == 0)
  {
    if (r != Z_MEM_ERROR) return Z_DATA_ERROR;
    return r;
  }

  /* build distance tree */
  r = huft_build(c + nl, nd, 0, cpdist, cpdext, td, bd, hp, &hn);
  if (r != Z_OK || (*bd == 0 && nl > 257))
  {
    if (r != Z_MEM_ERROR) return Z_DATA_ERROR;
    return r;
  }

  return Z_OK;
}


/* build fixed tables only once--keep them here */
local int fixed_built = 0;
#define FIXEDH 544      /* number of hufts used by fixed tables */
local inflate_huft fixed_mem[FIXEDH];
local uInt fixed_bl;
local uInt fixed_bd;
local inflate_huft *fixed_tl;
local inflate_huft *fixed_td;


int inflate_trees_fixed(bl, bd, tl, td)
uIntf *bl;               /* literal desired/actual bit depth */
uIntf *bd;               /* distance desired/actual bit depth */
inflate_huft * FAR *tl;  /* literal/length tree result */
inflate_huft * FAR *td;  /* distance tree result */
{
  /* build fixed tables if not already */
  if (!fixed_built)
  {
    int k;              /* temporary variable */
    uInt f = 0;         /* number of hufts used in fixed_mem */
    static uIntf c[288];           /* length list for huft_build */

    /* literal table */
    for (k = 0; k < 144; k++) c[k] = 8;
    for (; k < 256; k++) c[k] = 9;
    for (; k < 280; k++) c[k] = 7;
    for (; k < 288; k++) c[k] = 8;
    fixed_bl = 9;
    huft_build(c, 288, 257, cplens, cplext, &fixed_tl, &fixed_bl, fixed_mem, &f);

    /* distance table */
    for (k = 0; k < 30; k++) c[k] = 5;
    fixed_bd = 5;
    huft_build(c, 30, 0, cpdist, cpdext, &fixed_td, &fixed_bd, fixed_mem, &f);

    /* done */
    fixed_built = 1;
  }
  *bl = fixed_bl;
  *bd = fixed_bd;
  *tl = fixed_tl;
  *td = fixed_td;
  return Z_OK;
}
#endif