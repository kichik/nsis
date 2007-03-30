#include <windows.h>
#include <stdio.h>
#include "MyMath.h"
#include "Math.h"

// Converts String to Int (Dec, Hex) or Float value
void StringToItem(char *&s, ExpressionItem *item, int options)
{
  item->type = IT_CONST | ITC_INT;
  __int64 &v=*((__int64*)&(item->param1));
  v = 0;

  // Check for right input
  if (!s) return;

  // String-value
  if ((((options & (STI_FLOAT | STI_INT)) == 0) || *s == '\'' || *s == '\"' || *s == '`' ||
      ((*s != '+') && (*s != '-') && ((*s < '0') || (*s > '9'))))
      && (options & STI_STRING))
  {
      // end of string char
      char eol = 0;
      if (*s == '\'' || *s == '\"' || *s == '`') eol = *s;
      else s--;

      item->type = IT_CONST | ITC_STRING;
      // allocate memory buffer for string
      char *sp;
      sp = *((char**)&(item->param1)) = AllocString();
      while (*(++s) && (*s != eol))
      {
        *(sp++) = *s;
      }
      if (*s == eol) s++;
      *sp = 0;
  } else
  {
      // strip leading spaces and tabs
      while ((*s == ' ') || (*s == '\t')) s++;
  // Hex-value
  if ((options & STI_INT) && *s == '0' && (s[1] == 'x' || s[1] == 'X'))
  {
    s++;
    while (*(s+1) == '0') *s++;
    for (;;)
    {
      int c=*(++s);
      if (c >= '0' && c <= '9') c-='0';
      else if (c >= 'a' && c <= 'f') c-='a'-10;
      else if (c >= 'A' && c <= 'F') c-='A'-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  // Dec-value, possible floating-point
  else
  {
    int sign=0, numsignif = 0;
    if (*s == '-') sign++; else s--;
    while (*(s+1) == '0') *s++;
    for (;;)
    {
      int c=*(++s) - '0'; numsignif++;
      if ((options & STI_FLOAT) &&
          ((c == ('e'-'0')) || (c==('E'-'0')) || (c==('.'-'0'))
          || (numsignif > 18)))
      {
          // Switch to floating point conversion rountine
          item->type = IT_CONST | ITC_FLOAT;
          double& d = *((double*)&(item->param1));
          d = (double) v;

          while ((c >= 0) && (c <= 9))
          {
              d = d*10.0 + (double) c;
              c=*(++s) - '0';
          }

          // sub-decimal part
          if (c == ('.'-'0'))
          {
            double pwr = 1.0, dec = 0.0;
            for (;;)
            {
              c=*(++s) - '0';
              if ((c < 0) || (c > 9)) break;
              dec = dec*10.0 + (double) c;
              pwr *= 10.0;
            }
            d += dec/pwr;
          }
          // exponental part
          if ((c == ('E'-'0')) || (c == ('e'-'0')))
          {
              int expc = 0, esign = 0;
              s++;
              // detect exponential sign
              if ((*s == '+') || (*s == '-'))
                  esign = (*s == '-');
              else s--;

              // detect exp value
              for (;;)
              {
                c=*(++s) - '0';
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

void ItemToString(char *sbuf, ExpressionItem *item)
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
        char *ptr = *((char**)&(item->param1));
        while (*(sbuf++) = *(ptr++));
        }
        break;
    case ITC_ARRAY:
        {
            ArrayDesc *ad = (ArrayDesc *) item->param1;
            for (int index = 0; index < ad->count; index++)
                if ((ad->array[index]) &&
                    ((ad->array[index]->type & (ITEMTYPE|ITEMSUBTYPE)) == (IT_CONST | ITC_INT)))
                    if ((*(sbuf++) = (char) *((__int64*)&(ad->array[index]->param1))) == 0)
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

void itoa64(__int64 i, char *buffer)
{
    char buf[128], *b = buf;

    if (i < 0)
    {
        *(buffer++) = '-';
        i = -i;
    }
    if (i == 0) *(buffer++) = '0';
    else
    {
        while (i > 0)
        {
            *(b++) = '0' + ((char) (i%10));
            i /= 10;
        }
        while (b > buf) *(buffer++) = *(--b);
    }
    *buffer = 0;
}

#define POS_INFINITY  "#INF"
#define NEG_INFINITY "-#INF"

void FloatFormat(char *s, double value, int options)
{
    char format[128];
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
        sprintf(format, "%%.%df", prec);
    }
    else if (options & FF_EXP)
    {
        sprintf(format, "%%.%de", prec);
    }
    else if (options & FF_LEXP)
    {
        sprintf(format, "%%.%dE", prec);
    }
    else
    {
        sprintf(format, "%%.%dg", prec);
    }

    sprintf(s, format, value);
}

int lstrcmpn(char *s1, const char *s2, int chars)
{
    while ((chars > 0) && (*s1) && (*s2) && (*(s1) == *(s2))) chars--, s1++, s2++;
    if ((chars == 0) || (*s1 == *s2)) return 0;
    return (*s1 - *s2);
}
