// Unicode support by Jim Park -- 08/22/2007

#include <windows.h>
#include <stdio.h>
#include "MyMath.h"
#include "Math.h"

// Converts String to Int (Dec, Hex) or Float value
void StringToItem(TCHAR *&s, ExpressionItem *item, int options)
{
  item->type = IT_CONST | ITC_INT;
  __int64 &v=*((__int64*)&(item->param1));
  v = 0;

  // Check for right input
  if (!s) return;

  // String-value
  if ((((options & (STI_FLOAT | STI_INT)) == 0) || *s == _T('\'') || *s == _T('\"') || *s == _T('`') ||
      ((*s != _T('+')) && (*s != _T('-')) && ((*s < _T('0')) || (*s > _T('9')))))
      && (options & STI_STRING))
  {
      // end of string char
      TCHAR eol = 0;
      if (*s == _T('\'') || *s == _T('\"') || *s == _T('`')) eol = *s;
      else s--;

      item->type = IT_CONST | ITC_STRING;
      // allocate memory buffer for string
      TCHAR *sp;
      sp = *((TCHAR**)&(item->param1)) = AllocString();
      while (*(++s) && (*s != eol))
      {
        *(sp++) = *s;
      }
      if (*s == eol) s++;
      *sp = 0;
  } else
  {
      // strip leading spaces and tabs
      while ((*s == _T(' ')) || (*s == _T('\t'))) s++;
  // Hex-value
  if ((options & STI_INT) && *s == _T('0') && (s[1] == _T('x') || s[1] == _T('X')))
  {
    s++;
    while (*(s+1) == _T('0')) s++;
    for (;;)
    {
      int c=*(++s);
      if (c >= _T('0') && c <= _T('9')) c-=_T('0');
      else if (c >= _T('a') && c <= _T('f')) c-=_T('a')-10;
      else if (c >= _T('A') && c <= _T('F')) c-=_T('A')-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  // Dec-value, possible floating-point
  else
  {
    int sign=0, numsignif = 0;
    if (*s == _T('-')) sign++; else s--;
    while (*(s+1) == _T('0')) s++;
    for (;;)
    {
      int c=*(++s) - _T('0'); numsignif++;
      if ((options & STI_FLOAT) &&
          ((c == (_T('e')-_T('0'))) || (c==(_T('E')-_T('0'))) || (c==(_T('.')-_T('0')))
          || (numsignif > 18)))
      {
          // Switch to floating point conversion rountine
          item->type = IT_CONST | ITC_FLOAT;
          double& d = *((double*)&(item->param1));
          d = (double) v;

          while ((c >= 0) && (c <= 9))
          {
              d = d*10.0 + (double) c;
              c=*(++s) - _T('0');
          }

          // sub-decimal part
          if (c == (_T('.')-_T('0')))
          {
            double pwr = 1.0, dec = 0.0;
            for (;;)
            {
              c=*(++s) - _T('0');
              if ((c < 0) || (c > 9)) break;
              dec = dec*10.0 + (double) c;
              pwr *= 10.0;
            }
            d += dec/pwr;
          }
          // exponental part
          if ((c == (_T('E')-_T('0'))) || (c == (_T('e')-_T('0'))))
          {
              int expc = 0, esign = 0;
              s++;
              // detect exponential sign
              if ((*s == _T('+')) || (*s == _T('-')))
                  esign = (*s == _T('-'));
              else s--;

              // detect exp value
              for (;;)
              {
                c=*(++s) - _T('0');
                if ((c < 0) || (c > 9)) break;
                expc = expc*10 + c;
              }

              if (expc >= DBL_MAX_EXP)
              {
                  d = HUGE_VAL;
                  expc = 0;
              }

              double pwr = 1;
              while (expc > 99) { pwr *= 1.0e100; expc -= 100; }
              while (expc > 9) { pwr *= 1.0e10; expc -= 10; }
              while (expc) { pwr *= 10.0; expc--; }
              if (esign) d /= pwr;
              else d *= pwr;
          }
          if (sign) d = -d;
          return;
      }
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
    if (sign) v = -v;
    if ((options & STI_FLOAT) && ((options & STI_INT) == 0))
    {
        double& d = *((double*)&(item->param1));
        d = (double) v;
        item->type = IT_CONST | ITC_FLOAT;
    }
  }
  }
}

void ItemToString(TCHAR *sbuf, ExpressionItem *item)
{
    if ((item == NULL) || ((item->type & ITEMTYPE) != IT_CONST))
    {
        *sbuf = 0;
        return;
    }

    switch (item->type & ITEMSUBTYPE)
    {
    case ITC_STRING:
        {
        TCHAR *ptr = *((TCHAR**)&(item->param1));
        while ( (*(sbuf++) = *(ptr++)) );
        }
        break;
    case ITC_ARRAY:
        {
            ArrayDesc *ad = (ArrayDesc *) item->param1;
            for (int index = 0; index < ad->count; index++)
                if ((ad->array[index]) &&
                    ((ad->array[index]->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST | ITC_INT)))
                    if ((*(sbuf++) = (TCHAR) *((__int64*)&(ad->array[index]->param1))) == 0)
                        break;
        }
        break;
    case ITC_FLOAT:
        FloatFormat(sbuf, *((double*)&(item->param1)), 6);
        break;
    case ITC_INT:
        itoa64(*((__int64*)&(item->param1)), sbuf);
        break;
    }
}

void itoa64(__int64 i, TCHAR *buffer)
{
    TCHAR buf[128], *b = buf;

    if (i < 0)
    {
        *(buffer++) = _T('-');
        i = -i;
    }
    if (i == 0) *(buffer++) = _T('0');
    else
    {
        while (i > 0)
        {
            *(b++) = _T('0') + ((TCHAR) (i%10));
            i /= 10;
        }
        while (b > buf) *(buffer++) = *(--b);
    }
    *buffer = 0;
}

#define POS_INFINITY  _T("#INF")
#define NEG_INFINITY  _T("-#INF")

void FloatFormat(TCHAR *s, double value, int options)
{
    TCHAR format[128];
    int prec = options & 0xF;

    *s = 0;

    if(value == HUGE_VAL)
    {
        lstrcpy(s, POS_INFINITY);
        return;
    } else if(value == -HUGE_VAL) {
        lstrcpy(s, NEG_INFINITY);
        return;
    }

    if (options & FF_NOEXP)
    {
        _stprintf(format, _T("%%.%df"), prec);
    }
    else if (options & FF_EXP)
    {
        _stprintf(format, _T("%%.%de"), prec);
    }
    else if (options & FF_LEXP)
    {
        _stprintf(format, _T("%%.%dE"), prec);
    }
    else
    {
        _stprintf(format, _T("%%.%dg"), prec);
    }

    _stprintf(s, format, value);
}

int lstrcmpn(TCHAR *s1, const TCHAR *s2, int chars)
{
    while ((chars > 0) && (*s1) && (*s2) && (*(s1) == *(s2))) chars--, s1++, s2++;
    if ((chars == 0) || (*s1 == *s2)) return 0;
    return (*s1 - *s2);
}
