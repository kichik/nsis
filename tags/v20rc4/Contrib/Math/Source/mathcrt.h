/*---------------------------------------------------------------------------*/
/* math.h - mathematics header file                                          */
/*---------------------------------------------------------------------------*/
#pragma once

extern "C"
{

extern double _infinity;
#define HUGE_VAL    _infinity

#define acos(x)     _facos(x)
#define asin(x)     _fasin(x)
#define atan(x)     _fatan(x)
#define atan2(x, y) _fatan2(x, y)
#define ceil(x)     _fceil(x)
#define cos(x)      _fcos(x)
#define cosh(x)     _fcosh(x)
#define exp(x)      _fexp(x)
#define fabs(x)     _fabs(x)
#define floor(x)    _floor(x)
#define fmod(x, y)  _fmod(x, y)
#define frexp(x, n) _frexp(x, n)
#define ldexp(x, n) _fldexp(x, n)
#define log(x)      _flog(x)
#define log10(x)    _flog10(x)
#define modf(x, y)  _fmodf(x, y)
#define pow(x, y)   _fpow(x, y)
#define sin(x)      _fsin(x)
#define sinh(x)     _fsinh(x)
#define sqrt(x)     _fsqrt(x)
#define tan(x)      _ftan(x)
#define tanh(x)     _ftanh(x)

/*---------------------------------------------------------------------------*/
/* function prototpes                                                        */
/*---------------------------------------------------------------------------*/
double acos(double x);
double asin(double x);
double atan(double x);
double atan2(double x, double y);
double ceil(double x);
double cos(double x);
double cosh(double x);
double exp(double x);
double fabs(double x);
double floor(double x);
double fmod(double x, double y);
double frexp(double x, int *n);
double ldexp(double x, int n);
double log(double x);
double log10(double x);
double modf(double x, double *y);
double pow(double x, double y);
double sin(double x);
double sinh(double x);
double sqrt(double x);
double tan(double x);
double tanh(double x);

/*---------------------------------------------------------------------------*/
/* float.h - floating point include file                                     */
/*---------------------------------------------------------------------------*/
#define    FLT_RADIX             2
#define    FLT_ROUNDS            1
#define    FLT_DIG               6
#define    FLT_EPSILON           1.192092896e-07F
#define    FLT_MANT_DIG         24
#define    FLT_MAX               3.402823466e+38F
#define    FLT_MAX_EXP          38
#define    FLT_MIN               1.175494351e-38F
#define    FLT_MIN_EXP         (-37)

#define    DBL_DIG              15
#define    DBL_EPSILON           2.2204460492503131e-016
#define    DBL_MANT_DIG         53
#define    DBL_MAX               1.7976931348623158e+308
#define    DBL_MAX_EXP          308
#define    DBL_MIN               2.2250738585072014e-308
#define    DBL_MIN_EXP        (-307)

/*---------------------------------------------------------------------------*/
/* function prototpes                                                        */
/*---------------------------------------------------------------------------*/
void _fpreset(void);

}