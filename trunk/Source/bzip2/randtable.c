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


// smaller data mode that saves 336 bytes of data, but adds 60 bytes of code.
#if 1

#define PACK3(x,y,z) ((z) << 20)|((y) << 10)|(x)

static Int32 BZ2_rNumsPacked[171]=
{
  PACK3(619,720,127),
  PACK3(481,931,816),
  PACK3(813,233,566),
  PACK3(247,985,724),
  PACK3(205,454,863),
  PACK3(491,741,242),
  PACK3(949,214,733),
  PACK3(859,335,708),
  PACK3(621,574,73),
  PACK3(654,730,472),
  PACK3(419,436,278),
  PACK3(496,867,210),
  PACK3(399,680,480),
  PACK3(51,878,465),
  PACK3(811,169,869),
  PACK3(675,611,697),
  PACK3(867,561,862),
  PACK3(687,507,283),
  PACK3(482,129,807),
  PACK3(591,733,623),
  PACK3(150,238,59),
  PACK3(379,684,877),
  PACK3(625,169,643),
  PACK3(105,170,607),
  PACK3(520,932,727),
  PACK3(476,693,425),
  PACK3(174,647,73),
  PACK3(122,335,530),
  PACK3(442,853,695),
  PACK3(249,445,515),
  PACK3(909,545,703),
  PACK3(919,874,474),
  PACK3(882,500,594),
  PACK3(612,641,801),
  PACK3(220,162,819),
  PACK3(984,589,513),
  PACK3(495,799,161),
  PACK3(604,958,533),
  PACK3(221,400,386),
  PACK3(867,600,782),
  PACK3(382,596,414),
  PACK3(171,516,375),
  PACK3(682,485,911),
  PACK3(276,98,553),
  PACK3(163,354,666),
  PACK3(933,424,341),
  PACK3(533,870,227),
  PACK3(730,475,186),
  PACK3(263,647,537),
  PACK3(686,600,224),
  PACK3(469,68,770),
  PACK3(919,190,373),
  PACK3(294,822,808),
  PACK3(206,184,943),
  PACK3(795,384,383),
  PACK3(461,404,758),
  PACK3(839,887,715),
  PACK3(67,618,276),
  PACK3(204,918,873),
  PACK3(777,604,560),
  PACK3(951,160,578),
  PACK3(722,79,804),
  PACK3(96,409,713),
  PACK3(940,652,934),
  PACK3(970,447,318),
  PACK3(353,859,672),
  PACK3(112,785,645),
  PACK3(863,803,350),
  PACK3(139,93,354),
  PACK3(99,820,908),
  PACK3(609,772,154),
  PACK3(274,580,184),
  PACK3(79,626,630),
  PACK3(742,653,282),
  PACK3(762,623,680),
  PACK3(81,927,626),
  PACK3(789,125,411),
  PACK3(521,938,300),
  PACK3(821,78,343),
  PACK3(175,128,250),
  PACK3(170,774,972),
  PACK3(275,999,639),
  PACK3(495,78,352),
  PACK3(126,857,956),
  PACK3(358,619,580),
  PACK3(124,737,594),
  PACK3(701,612,669),
  PACK3(112,134,694),
  PACK3(363,992,809),
  PACK3(743,168,974),
  PACK3(944,375,748),
  PACK3(52,600,747),
  PACK3(642,182,862),
  PACK3(81,344,805),
  PACK3(988,739,511),
  PACK3(655,814,334),
  PACK3(249,515,897),
  PACK3(955,664,981),
  PACK3(649,113,974),
  PACK3(459,893,228),
  PACK3(433,837,553),
  PACK3(268,926,240),
  PACK3(102,654,459),
  PACK3(51,686,754),
  PACK3(806,760,493),
  PACK3(403,415,394),
  PACK3(687,700,946),
  PACK3(670,656,610),
  PACK3(738,392,760),
  PACK3(799,887,653),
  PACK3(978,321,576),
  PACK3(617,626,502),
  PACK3(894,679,243),
  PACK3(440,680,879),
  PACK3(194,572,640),
  PACK3(724,926,56),
  PACK3(204,700,707),
  PACK3(151,457,449),
  PACK3(797,195,791),
  PACK3(558,945,679),
  PACK3(297,59,87),
  PACK3(824,713,663),
  PACK3(412,693,342),
  PACK3(606,134,108),
  PACK3(571,364,631),
  PACK3(212,174,643),
  PACK3(304,329,343),
  PACK3(97,430,751),
  PACK3(497,314,983),
  PACK3(374,822,928),
  PACK3(140,206,73),
  PACK3(263,980,736),
  PACK3(876,478,430),
  PACK3(305,170,514),
  PACK3(364,692,829),
  PACK3(82,855,953),
  PACK3(676,246,369),
  PACK3(970,294,750),
  PACK3(807,827,150),
  PACK3(790,288,923),
  PACK3(804,378,215),
  PACK3(828,592,281),
  PACK3(565,555,710),
  PACK3(82,896,831),
  PACK3(547,261,524),
  PACK3(462,293,465),
  PACK3(502,56,661),
  PACK3(821,976,991),
  PACK3(658,869,905),
  PACK3(758,745,193),
  PACK3(768,550,608),
  PACK3(933,378,286),
  PACK3(215,979,792),
  PACK3(961,61,688),
  PACK3(793,644,986),
  PACK3(403,106,366),
  PACK3(905,644,372),
  PACK3(567,466,434),
  PACK3(645,210,389),
  PACK3(550,919,135),
  PACK3(780,773,635),
  PACK3(389,707,100),
  PACK3(626,958,165),
  PACK3(504,920,176),
  PACK3(193,713,857),
  PACK3(265,203,50),
  PACK3(668,108,645),
  PACK3(990,626,197),
  PACK3(510,357,358),
  PACK3(850,858,364),
  PACK3(936,638,0),
};

/*---------------------------------------------*/
Int16 BZ2_rNums[513];

void NSISCALL genrtable()
{
  unsigned char x=171;
  Int16 *t=BZ2_rNums;
  Int32 *it=BZ2_rNumsPacked;
  while (x--)
  {
    Int32 i=*it++;
    char l=3;
    while (l--)
    {
      *t++=i&1023;
      i>>=10;
    }
  }
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