#include "../exehead/config.h"
#ifdef NSIS_COMPRESS_USE_ZLIB

#include "zutil.h"


/* defines for inflate input/output */
/*   update pointers and return */
#define UPDBITS {s->bitb=b;s->bitk=k;}
#define UPDIN {z->avail_in=n;z->next_in=p;}
#define UPDOUT {s->write=q;}
#define UPDATE {UPDBITS UPDIN UPDOUT}
#define LEAVE return __myleave(z,r,b,k,p,n,q);

/*   get bytes and bits */
#define LOADIN {p=z->next_in;n=z->avail_in;b=s->bitb;k=s->bitk;}


#define NEEDBYTE {if(n)r=Z_OK;else LEAVE}
#define NEXTBYTE (n--,*p++)
#define NEEDBITS(j) {while(k<(j)){NEEDBYTE;b|=((uLong)NEXTBYTE)<<k;k+=8;}}

#define DUMPBITS(j) {b>>=(j);k-=(j);}
/*   output bytes */
#define WAVAIL (uInt)(q<s->read?s->read-q-1:s->end-q)
#define LOADOUT {q=s->write;m=(uInt)WAVAIL;}
#define WRAP {if(q==s->end&&s->read!=s->window){q=s->window;m=(uInt)WAVAIL;}}
#define FLUSH {UPDOUT r=inflate_flush(z,r); LOADOUT}
#define NEEDOUT {if(m==0){WRAP if(m==0){FLUSH WRAP if(m==0) LEAVE}}r=Z_OK;}
#define OUTBYTE(a) {*q++=(Byte)(a);m--;}
/*   load local pointers */
#define LOAD {LOADIN LOADOUT}

#define FIXEDH 544      /* number of hufts used by fixed tables */



typedef struct inflate_blocks_state FAR inflate_blocks_statef;
#define exop word.what.Exop
#define bits word.what.Bits

/* And'ing with mask[n] masks the lower n bits */
local unsigned short inflate_mask[17] = {
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
    0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff
};
local const char border[] = { /* Order of the bit length code lengths */
        16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

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

        /* build fixed tables only once--keep them here */
local char fixed_built = 0;
local inflate_huft fixed_mem[FIXEDH];
local uInt fixed_bl=9;
local uInt fixed_bd=5;
local inflate_huft *fixed_tl;
local inflate_huft *fixed_td;


local void inflate_codes_new(c,bl, bd, tl, td)
inflate_codes_statef *c;
uInt bl, bd;
inflate_huft *tl;
inflate_huft *td; /* need separate declaration for Borland C++ */
{
  c->mode = START;
  c->lbits = (Byte)bl;
  c->dbits = (Byte)bd;
  c->ltree = tl;
  c->dtree = td;
}


/* copy as much as possible from the sliding window to the output area */
local int inflate_flush(z, r)
z_streamp z;
int r;
{
  inflate_blocks_statef *s=&z->blocks;
  uInt n;
  Bytef *p;
  Bytef *q;

  /* local copies of source and destination pointers */
  p = z->next_out;
  q = s->read;

  /* compute number of bytes to copy as far as end of window */
  n = (uInt)((q <= s->write ? s->write : s->end) - q);
  if (n > z->avail_out) n = z->avail_out;
  if (n && r == Z_BUF_ERROR) r = Z_OK;

  /* update counters */
  z->avail_out -= n;
//  z->total_out += n;

  /* copy as far as end of window */
  zmemcpy(p, q, n);
  p += n;
  q += n;

  /* see if more to copy at beginning of window */
  if (q == s->end)
  {
    /* wrap pointers */
    q = s->window;
    if (s->write == s->end)
      s->write = s->window;

    /* compute bytes to copy */
    n = (uInt)(s->write - q);
    if (n > z->avail_out) n = z->avail_out;
    if (n && r == Z_BUF_ERROR) r = Z_OK;

    /* update counters */
    z->avail_out -= n;
    //z->total_out += n;

    /* copy */
    zmemcpy(p, q, n);
    p += n;
    q += n;
  }

  /* update pointers */
  z->next_out = p;
  s->read = q;

  /* done */
  return r;
}

local int __myleave(z_streamp z, int r, int b, int k, Bytef *p, int n, Bytef *q)
{
  inflate_blocks_statef *s = &z->blocks;
  UPDATE 
  return inflate_flush(z,r);
}

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
      int nextw=w;
      /* here i is the Huffman code of length k bits for value *p */
      /* make tables up to required level */
      while (k > (nextw=w + l))
      {
        h++;

        /* compute minimum size table less than or equal to l bits */
        z = g - nextw;
        z = z > (uInt)l ? l : z;        /* table size upper limit */
        if ((f = 1 << (j = k - nextw)) > a + 1)     /* try a k-w bit table */
        {                       /* too few codes for k-w bit table */
          f -= a + 1;           /* deduct codes from patterns left */
          xp = c + k;
          if (j < z)
            while (++j < z && (f <<= 1) > *++xp)     /* try smaller tables up to z bits */
            {
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
          j = i >> w;
          r.base = (uInt)(q - u[h-1] - j);   /* offset to this table */
          u[h-1][j] = r;        /* connect to last table */
        }
        else
          *t = q;               /* first table is returned result */
        w=nextw;                 /* previous table always l bits */
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
  return (y != 0 && g != 1) ? Z_BUF_ERROR : Z_OK;
}

void inflateReset(z_streamp z)
{
  inflate_blocks_statef *s=&z->blocks;
  s->mode = TYPE;
  s->bitk = s->bitb = 0;
  s->read = s->write = s->window;
  s->end = s->window + (1 << DEF_WBITS);
}

int inflate(z_streamp z)
{
  inflate_blocks_statef *s=&z->blocks;

  // lousy two bytes saved by doing this
  struct
  {
    uInt t;               /* temporary storage */
    uLong b;              /* bit buffer */
    uInt k;               /* bits in bit buffer */
    Bytef *p;             /* input data pointer */
    uInt n;               /* bytes available there */
    Bytef *q;             /* output window write pointer */
    uInt m;               /* bytes to end of window or read pointer */
  } _state;

int r=Z_OK;

#define t _state.t
#define b _state.b
#define k _state.k
#define p _state.p
#define n _state.n
#define q _state.q
#define m _state.m

  /* copy input/output information to locals (UPDATE macro restores) */
  LOAD

  /* process input based on current state */
  for (;;) switch (s->mode)
  {
    case TYPE:
      NEEDBITS(3)
      t = (uInt)b & 7;
      DUMPBITS(3)
      s->last = t & 1;
      switch (t >> 1)
      {
        case 0:                         /* stored */
          Tracev((stderr, "inflate:     stored block%s\n",
                 s->last ? " (last)" : ""));
          DUMPBITS(k&7)
          s->mode = LENS;               /* get length of stored block */
          break;
        case 1:                         /* fixed */
          Tracev((stderr, "inflate:     fixed codes block%s\n",
                 s->last ? " (last)" : ""));
          {
            if (!fixed_built)
            {
              int _k;              /* temporary variable */
              uInt f = 0;         /* number of hufts used in fixed_mem */
              static uIntf c[288];           /* length list for huft_build */

              /* literal table */
              for (_k = 0; _k < 288; _k++) 
              {
                char v=8;
                if (_k > 143)
                {
                  if (_k < 256) v++;
                  else if (_k < 280) v--;
                }
                c[_k] = v;
              }
          //    fixed_bl = 9;
              huft_build(c, 288, 257, cplens, cplext, &fixed_tl, &fixed_bl, fixed_mem, &f);

              /* distance table */
              for (_k = 0; _k < 30; _k++) c[_k] = 5;
            //  fixed_bd = 5;
              huft_build(c, 30, 0, cpdist, cpdext, &fixed_td, &fixed_bd, fixed_mem, &f);

              /* done */
              fixed_built++;
            }

            inflate_codes_new(&s->sub.decode.t_codes,fixed_bl, fixed_bd, fixed_tl, fixed_td);
          }
          s->mode = CODES;
          break;
        case 2:                         /* dynamic */
          Tracev((stderr, "inflate:     dynamic codes block%s\n",
                 s->last ? " (last)" : ""));
          s->mode = TABLE;
          break;
        default:                         /* illegal */
          s->mode = BAD;
          r = Z_DATA_ERROR;
          LEAVE
      }
      break;
    case LENS:
      NEEDBITS(16)
      s->sub.left = (uInt)b & 0xffff;
      b = k = 0;                      /* dump bits */
      Tracev((stderr, "inflate:       stored length %u\n", s->sub.left));
      s->mode = s->sub.left ? STORED : (s->last ? DRY : TYPE);
      break;
    case STORED:
      if (n == 0)
        LEAVE
      NEEDOUT
      t = s->sub.left;
      if (t > n) t = n;
      if (t > m) t = m;
      zmemcpy(q, p, t);
      p += t;  n -= t;
      q += t;  m -= t;
      if (!(s->sub.left -= t))
        s->mode = s->last ? DRY : TYPE;
      break;
    case TABLE:
      NEEDBITS(14)
      s->sub.trees.table = t = (uInt)b & 0x3fff;
      if ((t & 0x1f) > 29 || ((t >> 5) & 0x1f) > 29)
      {
        s->mode = BAD;
        r = Z_DATA_ERROR;
        LEAVE
      }
//      t = 258 + (t & 0x1f) + ((t >> 5) & 0x1f);
      DUMPBITS(14)
      s->sub.trees.index = 0;
      Tracev((stderr, "inflate:       table sizes ok\n"));
      s->mode = BTREE;
    case BTREE:
      while (s->sub.trees.index < 4 + (s->sub.trees.table >> 10))
      {
        NEEDBITS(3)
        s->sub.trees.t_blens[border[s->sub.trees.index++]] = (uInt)b & 7;
        DUMPBITS(3)
      }
      while (s->sub.trees.index < 19)
        s->sub.trees.t_blens[border[s->sub.trees.index++]] = 0;
      s->sub.trees.bb = 7;

      {
        uInt hn = 0;          /* hufts used in space */

        t = huft_build(s->sub.trees.t_blens, 19, 19, (short *)Z_NULL, (short*)Z_NULL,
                 &s->sub.trees.tb, &s->sub.trees.bb, s->hufts, &hn);
        if (t == Z_BUF_ERROR || s->sub.trees.bb == 0) t=Z_DATA_ERROR;
      }

      if (t != Z_OK)
      {
        r = t;
        s->mode = BAD;
        LEAVE
      }
      s->sub.trees.index = 0;
      Tracev((stderr, "inflate:       bits tree ok\n"));
      s->mode = DTREE;
    case DTREE:
      while (t = s->sub.trees.table,
             s->sub.trees.index < 258 + (t & 0x1f) + ((t >> 5) & 0x1f))
      {
        inflate_huft *h;
        uInt i, j, c;

        t = s->sub.trees.bb;
        NEEDBITS(t)
        h = s->sub.trees.tb + ((uInt)b & (uInt)inflate_mask[t]);
        t = h->bits;
        c = h->base;
        if (c < 16)
        {
          DUMPBITS(t)
          s->sub.trees.t_blens[s->sub.trees.index++] = c;
        }
        else /* c == 16..18 */
        {
          if (c == 18)
          {
            i=7;
            j=11;          
          }
          else
          {
            i=c-14;
            j=3;
          }
          NEEDBITS(t+i)
          DUMPBITS(t)
          j += (uInt)b & (uInt)inflate_mask[i];
          DUMPBITS(i)
          i = s->sub.trees.index;
          t = s->sub.trees.table;
          if (i + j > 258 + (t & 0x1f) + ((t >> 5) & 0x1f) ||
              (c == 16 && i < 1))
          {
            s->mode = BAD;
            r = Z_DATA_ERROR;
            LEAVE
          }
          c = c == 16 ? s->sub.trees.t_blens[i - 1] : 0;
          do {
            s->sub.trees.t_blens[i++] = c;
          } while (--j);
          s->sub.trees.index = i;
        }
      }
      s->sub.trees.tb = Z_NULL;
      {
        uInt hn = 0;          /* hufts used in space */
        uInt bl, bd;
        inflate_huft *tl, *td;
        int nl,nd;
        t = s->sub.trees.table;

        nl=257 + (t & 0x1f);
        nd=1 + ((t >> 5) & 0x1f);
        bl = 9;         /* must be <= 9 for lookahead assumptions */
        bd = 6;         /* must be <= 9 for lookahead assumptions */

        t = huft_build(s->sub.trees.t_blens, nl, 257, cplens, cplext, &tl, &bl, s->hufts, &hn);
        if (t != Z_OK || bl == 0) t=Z_DATA_ERROR;
        else
        {
          /* build distance tree */
          t = huft_build(s->sub.trees.t_blens + nl, nd, 0, cpdist, cpdext, &td, &bd, s->hufts, &hn);
          if (t != Z_OK || (bd == 0 && nl > 257))
          {
              t=Z_DATA_ERROR;
          }
        }

        if (t != Z_OK)
        {
          s->mode = BAD;
          r = t;
          LEAVE
        }
        Tracev((stderr, "inflate:       trees ok\n"));
        inflate_codes_new(&s->sub.decode.t_codes,bl, bd, tl, td);
      }
      s->mode = CODES;
    case CODES:
      UPDATE
      {
        inflate_huft *j;      /* temporary pointer */
        uInt e;               /* extra bits or operation */
        Bytef *f;             /* pointer to copy strings from */
        inflate_codes_statef *c = &s->sub.decode.t_codes;  /* codes state */

        int done = 0;

        /* process input and output based on current state */
        while (!done) switch (c->mode)
        {             /* waiting for "i:"=input, "o:"=output, "x:"=nothing */
          case START:         /* x: set up for LEN */
            c->sub.code.need = c->lbits;
            c->sub.code.tree = c->ltree;
            c->mode = LEN;
          case LEN:           /* i: get length/literal/eob next */
            t = c->sub.code.need;
            NEEDBITS(t)
            j = c->sub.code.tree + ((uInt)b & (uInt)inflate_mask[t]);
            DUMPBITS(j->bits)
            e = (uInt)(j->exop);
            if (e == 0)               /* literal */
            {
              c->sub.lit = j->base;
              c->mode = LIT;
              break;
            }
            if (e & 16)               /* length */
            {
              c->sub.copy.get = e & 15;
              c->len = j->base;
              c->mode = LENEXT;
              break;
            }
            if ((e & 64) == 0)        /* next table */
            {
              c->sub.code.need = e;
              c->sub.code.tree = j + j->base;
              break;
            }
            if (e & 32)               /* end of block */
            {
              c->mode = WASH;
              break;
            }
          goto badcode;
          case LENEXT:        /* i: getting length extra (have base) */
            t = c->sub.copy.get;
            NEEDBITS(t)
            c->len += (uInt)b & (uInt)inflate_mask[t];
            DUMPBITS(t)
            c->sub.code.need = c->dbits;
            c->sub.code.tree = c->dtree;
            c->mode = DIST;
          case DIST:          /* i: get distance next */
            t = c->sub.code.need;
            NEEDBITS(t)
            j = c->sub.code.tree + ((uInt)b & (uInt)inflate_mask[t]);
            DUMPBITS(j->bits)
            e = (uInt)(j->exop);
            if (e & 16)               /* distance */
            {
              c->sub.copy.get = e & 15;
              c->sub.copy.dist = j->base;
              c->mode = DISTEXT;
              break;
            }
            if ((e & 64) == 0)        /* next table */
            {
              c->sub.code.need = e;
              c->sub.code.tree = j + j->base;
              break;
            }
            goto badcode;
      //      c->mode = BADCODE;        /* invalid code */
        //    r = Z_DATA_ERROR;
          //  LEAVE
          case DISTEXT:       /* i: getting distance extra */
            t = c->sub.copy.get;
            NEEDBITS(t)
            c->sub.copy.dist += (uInt)b & (uInt)inflate_mask[t];
            DUMPBITS(t)
            c->mode = COPY;
          case COPY:          /* o: copying bytes in window, waiting for space */
            f = (uInt)(q - s->window) < c->sub.copy.dist ?
                s->end - (c->sub.copy.dist - (q - s->window)) :
                q - c->sub.copy.dist;

            while (c->len)
            {
              NEEDOUT
              OUTBYTE(*f++)
              if (f == s->end)
                f = s->window;
              c->len--;
            }
            c->mode = START;
            break;
          case LIT:           /* o: got literal, waiting for output space */
            NEEDOUT
            OUTBYTE(c->sub.lit)
            c->mode = START;
            break;
          case WASH:          /* o: got eob, possibly more output */
            if (k > 7)        /* return unused byte, if any */
            {
              k -= 8;
              n++;
              p--;            /* can always return one */
            }
            FLUSH
            if (s->read != s->write)
            {
              r = inflate_flush(z,r);
              done = 1;
              break;
            }
            c->mode = END;
          case END:
            r = inflate_flush(z,Z_STREAM_END);
            done = 1;
            break;
          default:
          badcode:
            r = inflate_flush(z,Z_STREAM_ERROR);
            done = 1;
            break;
        }
        UPDATE
      }
      if (r != Z_STREAM_END)
        return inflate_flush(z, r);
      r = Z_OK;
      LOAD
      Tracev((stderr, "inflate:       codes end, %lu total out\n",
              z->total_out + (q >= s->read ? q - s->read :
              (s->end - s->read) + (q - s->window))));
      if (!s->last)
      {
        s->mode = TYPE;
        break;
      }
      s->mode = DRY;
    case DRY:
      FLUSH
      if (s->read != s->write)
        LEAVE
      s->mode = DONE;
    case DONE:
      r = Z_STREAM_END;
      LEAVE
//    case BAD:
  //    r = Z_DATA_ERROR;
    //  LEAVE
    default: // we'll call Z_STREAM_ERROR if BAD anyway
      r = Z_STREAM_ERROR;
      LEAVE
  }
}

#undef t
#undef b
#undef k
#undef p
#undef n
#undef q
#undef m


#endif