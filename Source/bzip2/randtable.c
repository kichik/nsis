#include "../exehead/config.h"
#if (defined(NSIS_COMPRESS_USE_BZIP2) && defined(NSIS_CONFIG_COMPRESSION_SUPPORT)) || !defined(EXEHEAD)

/*-------------------------------------------------------------*/
/*--- Table for randomising repetitive blocks               ---*/
/*---                                           randtable.c ---*/
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


#include "bzlib_private.h"

// smaller data mode that saves 384 bytes of data, but adds 66 bytes of code.
#ifdef EXEHEAD

#define PACK4(a,b,c,d) (UChar)((a)<<0), (UChar)(((a)>>8)|((b)<<2)), (UChar)(((b)>>6)|((c)<<4)), (UChar)(((c)>>4)|((d)<<6)), (UChar)((d)>>2)

// Four 10-bit values per five bytes
// aaaaaaaa bbbbbbaa ccccbbbb ddcccccc dddddddd
//        0      09     09     09      9

static UChar BZ2_rNumsPacked[640]=
{
  PACK4(619,720,127,481),
  PACK4(931,816,813,233),
  PACK4(566,247,985,724),
  PACK4(205,454,863,491),
  PACK4(741,242,949,214),
  PACK4(733,859,335,708),
  PACK4(621,574, 73,654),
  PACK4(730,472,419,436),
  PACK4(278,496,867,210),
  PACK4(399,680,480, 51),
  PACK4(878,465,811,169),
  PACK4(869,675,611,697),
  PACK4(867,561,862,687),
  PACK4(507,283,482,129),
  PACK4(807,591,733,623),
  PACK4(150,238, 59,379),
  PACK4(684,877,625,169),
  PACK4(643,105,170,607),
  PACK4(520,932,727,476),
  PACK4(693,425,174,647),
  PACK4( 73,122,335,530),
  PACK4(442,853,695,249),
  PACK4(445,515,909,545),
  PACK4(703,919,874,474),
  PACK4(882,500,594,612),
  PACK4(641,801,220,162),
  PACK4(819,984,589,513),
  PACK4(495,799,161,604),
  PACK4(958,533,221,400),
  PACK4(386,867,600,782),
  PACK4(382,596,414,171),
  PACK4(516,375,682,485),
  PACK4(911,276, 98,553),
  PACK4(163,354,666,933),
  PACK4(424,341,533,870),
  PACK4(227,730,475,186),
  PACK4(263,647,537,686),
  PACK4(600,224,469, 68),
  PACK4(770,919,190,373),
  PACK4(294,822,808,206),
  PACK4(184,943,795,384),
  PACK4(383,461,404,758),
  PACK4(839,887,715, 67),
  PACK4(618,276,204,918),
  PACK4(873,777,604,560),
  PACK4(951,160,578,722),
  PACK4( 79,804, 96,409),
  PACK4(713,940,652,934),
  PACK4(970,447,318,353),
  PACK4(859,672,112,785),
  PACK4(645,863,803,350),
  PACK4(139, 93,354, 99),
  PACK4(820,908,609,772),
  PACK4(154,274,580,184),
  PACK4( 79,626,630,742),
  PACK4(653,282,762,623),
  PACK4(680, 81,927,626),
  PACK4(789,125,411,521),
  PACK4(938,300,821, 78),
  PACK4(343,175,128,250),
  PACK4(170,774,972,275),
  PACK4(999,639,495, 78),
  PACK4(352,126,857,956),
  PACK4(358,619,580,124),
  PACK4(737,594,701,612),
  PACK4(669,112,134,694),
  PACK4(363,992,809,743),
  PACK4(168,974,944,375),
  PACK4(748, 52,600,747),
  PACK4(642,182,862, 81),
  PACK4(344,805,988,739),
  PACK4(511,655,814,334),
  PACK4(249,515,897,955),
  PACK4(664,981,649,113),
  PACK4(974,459,893,228),
  PACK4(433,837,553,268),
  PACK4(926,240,102,654),
  PACK4(459, 51,686,754),
  PACK4(806,760,493,403),
  PACK4(415,394,687,700),
  PACK4(946,670,656,610),
  PACK4(738,392,760,799),
  PACK4(887,653,978,321),
  PACK4(576,617,626,502),
  PACK4(894,679,243,440),
  PACK4(680,879,194,572),
  PACK4(640,724,926, 56),
  PACK4(204,700,707,151),
  PACK4(457,449,797,195),
  PACK4(791,558,945,679),
  PACK4(297, 59, 87,824),
  PACK4(713,663,412,693),
  PACK4(342,606,134,108),
  PACK4(571,364,631,212),
  PACK4(174,643,304,329),
  PACK4(343, 97,430,751),
  PACK4(497,314,983,374),
  PACK4(822,928,140,206),
  PACK4( 73,263,980,736),
  PACK4(876,478,430,305),
  PACK4(170,514,364,692),
  PACK4(829, 82,855,953),
  PACK4(676,246,369,970),
  PACK4(294,750,807,827),
  PACK4(150,790,288,923),
  PACK4(804,378,215,828),
  PACK4(592,281,565,555),
  PACK4(710, 82,896,831),
  PACK4(547,261,524,462),
  PACK4(293,465,502, 56),
  PACK4(661,821,976,991),
  PACK4(658,869,905,758),
  PACK4(745,193,768,550),
  PACK4(608,933,378,286),
  PACK4(215,979,792,961),
  PACK4( 61,688,793,644),
  PACK4(986,403,106,366),
  PACK4(905,644,372,567),
  PACK4(466,434,645,210),
  PACK4(389,550,919,135),
  PACK4(780,773,635,389),
  PACK4(707,100,626,958),
  PACK4(165,504,920,176),
  PACK4(193,713,857,265),
  PACK4(203, 50,668,108),
  PACK4(645,990,626,197),
  PACK4(510,357,358,850),
  PACK4(858,364,936,638)
};

/*---------------------------------------------*/
Int16 BZ2_rNums[512];

#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#error This routine is currently little-endian specific!
#endif

void NSISCALL genrtable()
{
  unsigned char x=640/5;
  Int16 *t=BZ2_rNums;
  UChar *it=BZ2_rNumsPacked;
  do {
    unsigned char l=0;
    do {
      *t++ = (Int16)(((*(UInt16 *)(it++))>>l) & 1023);
    } while ((l+=2) < 8);
    it++;
  } while (--x);
/*
  x=512/4;
  t=BZ2_rNums;
  do {
    char z[20];
    wsprintf(z, "%d,%d,%d,%d\n", t[0], t[1], t[2], t[3]);
    t += 4;
    OutputDebugString(z);
  } while (--x);
*/
}

#else

Int16 BZ2_rNums[512] = {
   619, 720, 127, 481, 931, 816, 813, 233, 566, 247,
   985, 724, 205, 454, 863, 491, 741, 242, 949, 214,
   733, 859, 335, 708, 621, 574, 73 , 654, 730, 472,
   419, 436, 278, 496, 867, 210, 399, 680, 480, 51,
   878, 465, 811, 169, 869, 675, 611, 697, 867, 561,
   862, 687, 507, 283, 482, 129, 807, 591, 733, 623,
   150, 238, 59 , 379, 684, 877, 625, 169, 643, 105,
   170, 607, 520, 932, 727, 476, 693, 425, 174, 647,
   73 , 122, 335, 530, 442, 853, 695, 249, 445, 515,
   909, 545, 703, 919, 874, 474, 882, 500, 594, 612,
   641, 801, 220, 162, 819, 984, 589, 513, 495, 799,
   161, 604, 958, 533, 221, 400, 386, 867, 600, 782,
   382, 596, 414, 171, 516, 375, 682, 485, 911, 276,
   98 , 553, 163, 354, 666, 933, 424, 341, 533, 870,
   227, 730, 475, 186, 263, 647, 537, 686, 600, 224,
   469, 68 , 770, 919, 190, 373, 294, 822, 808, 206,
   184, 943, 795, 384, 383, 461, 404, 758, 839, 887,
   715, 67 , 618, 276, 204, 918, 873, 777, 604, 560,
   951, 160, 578, 722, 79 , 804, 96 , 409, 713, 940,
   652, 934, 970, 447, 318, 353, 859, 672, 112, 785,
   645, 863, 803, 350, 139, 93 , 354, 99 , 820, 908,
   609, 772, 154, 274, 580, 184, 79 , 626, 630, 742,
   653, 282, 762, 623, 680, 81 , 927, 626, 789, 125,
   411, 521, 938, 300, 821, 78 , 343, 175, 128, 250,
   170, 774, 972, 275, 999, 639, 495, 78 , 352, 126,
   857, 956, 358, 619, 580, 124, 737, 594, 701, 612,
   669, 112, 134, 694, 363, 992, 809, 743, 168, 974,
   944, 375, 748, 52 , 600, 747, 642, 182, 862, 81,
   344, 805, 988, 739, 511, 655, 814, 334, 249, 515,
   897, 955, 664, 981, 649, 113, 974, 459, 893, 228,
   433, 837, 553, 268, 926, 240, 102, 654, 459, 51,
   686, 754, 806, 760, 493, 403, 415, 394, 687, 700,
   946, 670, 656, 610, 738, 392, 760, 799, 887, 653,
   978, 321, 576, 617, 626, 502, 894, 679, 243, 440,
   680, 879, 194, 572, 640, 724, 926, 56 , 204, 700,
   707, 151, 457, 449, 797, 195, 791, 558, 945, 679,
   297, 59 , 87 , 824, 713, 663, 412, 693, 342, 606,
   134, 108, 571, 364, 631, 212, 174, 643, 304, 329,
   343, 97 , 430, 751, 497, 314, 983, 374, 822, 928,
   140, 206, 73 , 263, 980, 736, 876, 478, 430, 305,
   170, 514, 364, 692, 829, 82 , 855, 953, 676, 246,
   369, 970, 294, 750, 807, 827, 150, 790, 288, 923,
   804, 378, 215, 828, 592, 281, 565, 555, 710, 82,
   896, 831, 547, 261, 524, 462, 293, 465, 502, 56,
   661, 821, 976, 991, 658, 869, 905, 758, 745, 193,
   768, 550, 608, 933, 378, 286, 215, 979, 792, 961,
   61 , 688, 793, 644, 986, 403, 106, 366, 905, 644,
   372, 567, 466, 434, 645, 210, 389, 550, 919, 135,
   780, 773, 635, 389, 707, 100, 626, 958, 165, 504,
   920, 176, 193, 713, 857, 265, 203, 50 , 668, 108,
   645, 990, 626, 197, 510, 357, 358, 850, 858, 364,
   936, 638
};
#endif

/*-------------------------------------------------------------*/
/*--- end                                       randtable.c ---*/
/*-------------------------------------------------------------*/
#endif
