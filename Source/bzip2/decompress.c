/*
 * This file is a part of the bzip2 compression module for NSIS.
 * 
 * Copyright and license information can be found below.
 * Modifications Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * The original zlib source code is available at
 * http://www.bzip.org/
 * 
 * This modification is not compatible with the original bzip2.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "bzlib.h"

/*-------------------------------------------------------------*/
/*--- Decompression machinery                               ---*/
/*---                                          decompress.c ---*/
/*-------------------------------------------------------------*/

/*--
  This file is a part of bzip2 and/or libbzip2, a program and
  library for lossless, block-sorting data compression.

  Copyright (C) 1996-2000 Julian R Seward.  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software.  If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  3. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  4. The name of the author may not be used to endorse or promote
     products derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Julian Seward, Cambridge, UK.
  jseward@acm.org
  bzip2/libbzip2 version 1.0 of 21 March 2000

  This program is based on (at least) the work of:
     Mike Burrows
     David Wheeler
     Peter Fenwick
     Alistair Moffat
     Radford Neal
     Ian H. Witten
     Robert Sedgewick
     Jon L. Bentley

  For more information on these sources, see the manual.
--*/


/*---------------------------------------------------*/
#define RETURN(rrr)                               \
   { retVal = rrr; goto save_state_and_return; };


static int NSISCALL __mygetbits(int *vtmp, int nnn, DState* s)
{
   for (;;) {
      if (s->bsLive >= nnn) {
         UInt32 v;
         v = (s->bsBuff >>
             (s->bsLive-nnn)) & ((1 << nnn)-1);
         s->bsLive -= nnn;
         *vtmp = v;
        return 0;
      }
      if (s->avail_in == 0) return 1;
      s->bsBuff = (s->bsBuff << 8) | ((UInt32) (*((UChar*)(s->next_in))));
      s->bsLive += 8;
      s->next_in++;
      s->avail_in--;
   }
}

#define GET_BITS(lll,vvv,nnn)                     \
   case lll: s->state = lll;                      \
    if (__mygetbits(&vvv,nnn,s)) RETURN(BZ_OK)

#define GET_UCHAR(lll,uuu)                        \
   GET_BITS(lll,uuu,8)

#define GET_BIT(lll,uuu)                          \
   GET_BITS(lll,uuu,1)

static int NSISCALL getmtf1(DState_save *sv,DState* s)
{
   if (sv->groupPos == 0) {
      sv->groupNo++;
      if (sv->groupNo >= sv->nSelectors) return 1;
      sv->groupPos = BZ_G_SIZE;
      sv->gSel = s->selector[sv->groupNo];
      sv->gMinlen = s->minLens[sv->gSel];
      sv->gLimit = &(s->limit[sv->gSel][0]);
      sv->gPerm = &(s->perm[sv->gSel][0]);
      sv->gBase = &(s->base[sv->gSel][0]);
   }
   sv->groupPos--;
   sv->zn = sv->gMinlen;
   return 0;
}

/*---------------------------------------------------*/
#define GET_MTF_VAL(label1,label2,lval)           \
{                                                 \
   if (getmtf1(&sv,s)) RETURN(BZ_DATA_ERROR);  \
   GET_BITS(label1, zvec, zn);                    \
   for (;;)  {                                    \
      if (zn > 20 /* the longest code */) RETURN(BZ_DATA_ERROR);                   \
      if (zvec <= gLimit[zn]) break;              \
      zn++;                                       \
      GET_BIT(label2, zj);                        \
      zvec = (zvec << 1) | zj;                    \
   };                                             \
   if (zvec - gBase[zn] < 0                       \
       || zvec - gBase[zn] >= BZ_MAX_ALPHA_SIZE)  \
      RETURN(BZ_DATA_ERROR);                      \
   lval = gPerm[zvec - gBase[zn]];                \
}


/*---------------------------------------------------*/
Int32 NSISCALL BZ2_decompress ( DState* s )
{
   Int32 uc;
   Int32      retVal;
   Int32      minLen, maxLen;

   /* stuff that needs to be saved/restored */
   DState_save sv;

   /*restore from the save area*/
   sv=s->save;//mini_memcpy(&sv, &(s->save), sizeof(sv));

   #define i (sv.i)
   #define j (sv.j)
   #define t (sv.t)
   #define alphaSize (sv.alphaSize)
   #define nGroups (sv.nGroups)
   #define nSelectors (sv.nSelectors)
   #define EOB (sv.EOB)
   #define groupNo (sv.groupNo)
   #define groupPos (sv.groupPos)
   #define nextSym (sv.nextSym)
   #define nblockMAX (sv.nblockMAX)
   #define nblock (sv.nblock)
   #define es (sv.es)
   #define N (sv.N)
   #define curr (sv.curr)
   #define zt (sv.zt)
   #define zn (sv.zn)
   #define zvec (sv.zvec)
   #define zj (sv.zj)
   #define gSel (sv.gSel)
   #define gMinlen (sv.gMinlen)
   #define gLimit (sv.gLimit)
   #define gBase (sv.gBase)
   #define gPerm (sv.gPerm)

   retVal = BZ_OK;

   switch (s->state) {


      GET_UCHAR(BZ_X_BLKHDR_1, uc);

      if (uc == 0x17)
      {
        s->state = BZ_X_IDLE;
        RETURN(BZ_STREAM_END);
      }
      if (uc != 0x31) RETURN(BZ_DATA_ERROR);

      s->origPtr = 0;
      GET_UCHAR(BZ_X_ORIGPTR_1, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      GET_UCHAR(BZ_X_ORIGPTR_2, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      GET_UCHAR(BZ_X_ORIGPTR_3, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);

      if (s->origPtr < 0)
         RETURN(BZ_DATA_ERROR);
      if (s->origPtr > 10 + NSIS_COMPRESS_BZIP2_LEVEL*100000)
         RETURN(BZ_DATA_ERROR);

      /*--- Receive the mapping table ---*/
      for (i = 0; i < 16; i++) {
         GET_BIT(BZ_X_MAPPING_1, uc);
         if (uc == 1)
            s->inUse16[i] = True; else
            s->inUse16[i] = False;
      }

      for (i = 0; i < 256; i++) s->inUse[i] = False;

      for (i = 0; i < 16; i++)
         if (s->inUse16[i])
            for (j = 0; j < 16; j++) {
               GET_BIT(BZ_X_MAPPING_2, uc);
               if (uc == 1) s->inUse[i * 16 + j] = True;
            }
      {
         Int32 qi;
         s->nInUse = 0;
         for (qi = 0; qi < 256; qi++)
            if (s->inUse[qi])
               s->seqToUnseq[s->nInUse++] = qi;
      }

      if (s->nInUse == 0) RETURN(BZ_DATA_ERROR);
      alphaSize = s->nInUse+2;

      /*--- Now the selectors ---*/
      GET_BITS(BZ_X_SELECTOR_1, nGroups, 3);
      if (nGroups < 2 || nGroups > 6) RETURN(BZ_DATA_ERROR);
      GET_BITS(BZ_X_SELECTOR_2, nSelectors, 15);
      if (nSelectors < 1) RETURN(BZ_DATA_ERROR);
      for (i = 0; i < nSelectors; i++) {
         j = 0;
         while (True) {
            GET_BIT(BZ_X_SELECTOR_3, uc);
            if (uc == 0) break;
            j++;
            if (j >= nGroups) RETURN(BZ_DATA_ERROR);
         }
         s->selectorMtf[i] = j;
      }

      /*--- Undo the MTF values for the selectors. ---*/
      {
         UChar pos[BZ_N_GROUPS], tmp, v;
         for (v = 0; v < nGroups; v++) pos[v] = v;

         for (i = 0; i < nSelectors; i++) {
            v = s->selectorMtf[i];
            tmp = pos[v];
            while (v > 0) { pos[v] = pos[v-1]; v--; }
            pos[0] = tmp;
            s->selector[i] = tmp;
         }
      }

      /*--- Now the coding tables ---*/
      for (t = 0; t < nGroups; t++) {
         GET_BITS(BZ_X_CODING_1, curr, 5);
         for (i = 0; i < alphaSize; i++) {
            while (True) {
               if (curr < 1 || curr > 20) RETURN(BZ_DATA_ERROR);
               GET_BIT(BZ_X_CODING_2, uc);
               if (uc == 0) break;
               GET_BIT(BZ_X_CODING_3, uc);
               if (uc == 0) curr++; else curr--;
            }
            s->len[t][i] = curr;
         }
      }

      /*--- Create the Huffman decoding tables ---*/
      for (t = 0; t < nGroups; t++) {
         minLen = 32;
         maxLen = 0;
         for (i = 0; i < alphaSize; i++) {
            if (s->len[t][i] > maxLen) maxLen = s->len[t][i];
            if (s->len[t][i] < minLen) minLen = s->len[t][i];
         }
         BZ2_hbCreateDecodeTables (
            &(s->limit[t][0]),
            &(s->base[t][0]),
            &(s->perm[t][0]),
            &(s->len[t][0]),
            minLen, maxLen, alphaSize
         );
         s->minLens[t] = minLen;
      }

      /*--- Now the MTF values ---*/

      EOB      = s->nInUse+1;
      nblockMAX = NSIS_COMPRESS_BZIP2_LEVEL*100000;
      groupNo  = -1;
      groupPos = 0;

      for (i = 0; i <= 255; i++) s->unzftab[i] = 0;

      /*-- MTF init --*/
      {
         Int32 ii, jj, kk = MTFA_SIZE-1;
         for (ii = 256 / MTFL_SIZE - 1; ii >= 0; ii--) {
            for (jj = MTFL_SIZE-1; jj >= 0; jj--) {
               s->mtfa[kk] = (UChar)(ii * MTFL_SIZE + jj);
               kk--;
            }
            s->mtfbase[ii] = kk + 1;
         }
      }
      /*-- end MTF init --*/

      nblock = 0;
      GET_MTF_VAL(BZ_X_MTF_1, BZ_X_MTF_2, nextSym);

      while (True) {

         if (nextSym == EOB) break;

         if (nextSym == BZ_RUNA || nextSym == BZ_RUNB) {

            es = -1;
            N = 1;
            while (nextSym == BZ_RUNA || nextSym == BZ_RUNB)
            {
               if (nextSym == BZ_RUNA) es += N;
               N = N << 1;
               if (nextSym == BZ_RUNB) es += N;
               GET_MTF_VAL(BZ_X_MTF_3, BZ_X_MTF_4, nextSym);
            }

            es++;
            uc = s->seqToUnseq[ s->mtfa[s->mtfbase[0]] ];
            s->unzftab[uc] += es;

#ifdef NSIS_COMPRESS_BZIP2_SMALLMODE
              while (es > 0) {
                 if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);
                 s->ll16[nblock] = (UInt16)uc;
                 nblock++;
                 es--;
              }
#else
             while (es > 0) {
                if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);
                s->tt[nblock] = (UInt32)uc;
                nblock++;
                es--;
             }
#endif
            continue;

         } else {

            if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);

            /*-- uc = MTF ( nextSym-1 ) --*/
            {
               Int32 ii, jj, kk, pp, lno, off;
               UInt32 nn;
               nn = (UInt32)(nextSym - 1);

               if (nn < MTFL_SIZE) {
                  /* avoid general-case expense */
                  pp = s->mtfbase[0];
                  uc = s->mtfa[pp+nn];
                  /*while (nn > 3) {
                     Int32 z = pp+nn;
                     s->mtfa[(z)  ] = s->mtfa[(z)-1];
                     s->mtfa[(z)-1] = s->mtfa[(z)-2];
                     s->mtfa[(z)-2] = s->mtfa[(z)-3];
                     s->mtfa[(z)-3] = s->mtfa[(z)-4];
                     nn -= 4;
                  }
                  */
                  while (nn > 0) {
                     s->mtfa[(pp+nn)] = s->mtfa[(pp+nn)-1]; nn--;
                  };
                  s->mtfa[pp] = uc;
               } else {
                  /* general case */
                  lno = nn / MTFL_SIZE;
                  off = nn % MTFL_SIZE;
                  pp = s->mtfbase[lno] + off;
                  uc = s->mtfa[pp];
                  while (pp > s->mtfbase[lno]) {
                     s->mtfa[pp] = s->mtfa[pp-1]; pp--;
                  };
                  s->mtfbase[lno]++;
                  while (lno > 0) {
                     s->mtfbase[lno]--;
                     s->mtfa[s->mtfbase[lno]]
                        = s->mtfa[s->mtfbase[lno-1] + MTFL_SIZE - 1];
                     lno--;
                  }
                  s->mtfbase[0]--;
                  s->mtfa[s->mtfbase[0]] = uc;
                  if (s->mtfbase[0] == 0) {
                     kk = MTFA_SIZE-1;
                     for (ii = 256 / MTFL_SIZE-1; ii >= 0; ii--) {
                        for (jj = MTFL_SIZE-1; jj >= 0; jj--) {
                           s->mtfa[kk] = s->mtfa[s->mtfbase[ii] + jj];
                           kk--;
                        }
                        s->mtfbase[ii] = kk + 1;
                     }
                  }
               }
            }
            /*-- end uc = MTF ( nextSym-1 ) --*/

            s->unzftab[s->seqToUnseq[uc]]++;
#ifdef NSIS_COMPRESS_BZIP2_SMALLMODE
            s->ll16[nblock] = (UInt16)(s->seqToUnseq[uc]);
#else
            s->tt[nblock]   = (UInt32)(s->seqToUnseq[uc]);
#endif
            nblock++;

            GET_MTF_VAL(BZ_X_MTF_5, BZ_X_MTF_6, nextSym);
            continue;
         }
      }

      /* Now we know what nblock is, we can do a better sanity
         check on s->origPtr.
      */
      if (s->origPtr < 0 || s->origPtr >= nblock)
         RETURN(BZ_DATA_ERROR);

      s->state_out_len = 0;
      s->state_out_ch  = 0;
      s->state = BZ_X_OUTPUT;

      /*-- Set up cftab to facilitate generation of T^(-1) --*/
      s->cftab[0] = 0;
      for (i = 1; i <= 256; i++) s->cftab[i] = s->unzftab[i-1]+s->cftab[i-1];
//      for (i = 1; i <= 256; i++) s->cftab[i] += s->cftab[i-1];

#ifdef NSIS_COMPRESS_BZIP2_SMALLMODE
      {
         /*-- Make a copy of cftab, used in generation of T --*/
         for (i = 0; i <= 256; i++) s->cftabCopy[i] = s->cftab[i];

         /*-- compute the T vector --*/
         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->ll16[i]);
            SET_LL(i, s->cftabCopy[uc]);
            s->cftabCopy[uc]++;
         }

         /*-- Compute T^(-1) by pointer reversal on T --*/
         i = s->origPtr;
         j = GET_LL(i);
         do {
            Int32 tmp = GET_LL(j);
            SET_LL(j, i);
            i = j;
            j = tmp;
         }
            while (i != s->origPtr);

         s->tPos = s->origPtr;
         s->nblock_used = 0;
         BZ_GET_SMALL(s->k0); s->nblock_used++;
      }
#else//!small

         /*-- compute the T^(-1) vector --*/
         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->tt[i] & 0xff);
            s->tt[s->cftab[uc]] |= (i << 8);
            s->cftab[uc]++;
         }

         s->tPos = s->tt[s->origPtr] >> 8;
         s->nblock_used = 0;
         BZ_GET_FAST(s->k0); s->nblock_used++;
#endif
      RETURN(BZ_OK);

      default: AssertH ( False, 4001 );
   }

   AssertH ( False, 4002 );

   save_state_and_return:

   s->save=sv; //mini_memcpy(&(s->save), &sv, sizeof(sv));

   #undef i
   #undef j
   #undef t
   #undef alphaSize
   #undef nGroups
   #undef nSelectors
   #undef EOB
   #undef groupNo
   #undef groupPos
   #undef nextSym
   #undef nblockMAX
   #undef nblock
   #undef es
   #undef N
   #undef curr
   #undef zt
   #undef zn
   #undef zvec
   #undef zj
   #undef gSel
   #undef gMinlen
   #undef gLimit
   #undef gBase
   #undef gPerm

   return retVal;
}


/*-------------------------------------------------------------*/
/*--- end                                      decompress.c ---*/
/*-------------------------------------------------------------*/
