#include <windows.h>
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

#define _FLOAT_ROUND_ADJUST (double)5e-15
extern "C"
    int _ftol(double num);

int _floatp10(double *fnum, int *fsign, int prec)
{
  int fpower, _d;
  fcvt(*fnum, prec, &fpower, &_d);

  if (*fnum < 0)
    *fsign = -1;
  else
    *fsign = 1;

  return fpower;
}

#define POS_INFINITY  "#INF"
#define NEG_INFINITY "-#INF"

void FloatFormatF(char *s, double value, int prec)
{
    int fpower, fsign, fdigit, fprec = 0, fzfill = 0;

    fpower = _floatp10(&value, &fsign, prec);
    if(fsign < 0) *s++ = '-';
    if(fpower < 0)
    {
        *s++ = '0';
        fpower++;
        fzfill++;
    } else {
        while(fpower >= 0)
        {
           if(fprec < 16)
           {
                fdigit = (int)value;
                *s++ = (char)((char)fdigit + (char)48);
                value -= (double)fdigit;
                value *= (double)10;
                value += _FLOAT_ROUND_ADJUST;
                fprec++;
           } else {
                *s++ = '0';
           }
           fpower--;
        }
        fpower = 0;
    }
    if(prec)
    {
            *s++ = '.';
            while(prec)
            {
              if(fzfill && fpower < 0)
              {
                *s++ = '0';
                fpower++;
              } else {
                if(fprec < 16)
                {
                  fdigit = (int)value;
                  *s++ = (unsigned char)((unsigned char)fdigit +
                                         (unsigned char)48);

                  value -= (double)fdigit;
                  value *= (double)10;
                  value += _FLOAT_ROUND_ADJUST;
                  fprec++;
                } else {
                  *s++ = '0';
                }
              }
              prec--;
            }
    }
    *s = '\0';
}

void FloatFormatE(char *s, double fnum, int options)
{
    int fpower, fsign, fdigit, fprec = 0, prec, fzfill = 0;
    double sfnum;

    prec = options & 0xF;

    sfnum = fnum;
    fpower = _floatp10(&sfnum, &fsign, -999);
    fpower = _floatp10(&fnum, &fsign, prec - fpower);
    if(fsign < 0) *s++ = '-';
    fdigit = (int)fnum;
    *s++ = (char)((char)fdigit + (char)48);
    fnum -= (double)fdigit;
    fnum *= (double)10;
    fnum += _FLOAT_ROUND_ADJUST;
    if(prec)
    {
        *s++ = '.';
        while(prec)
        {
            if(fprec < 16)
            {
                fdigit = (int)fnum;
                *s++ = (unsigned char)((unsigned char)fdigit +
                    (unsigned char)48);
                fnum -= (double)fdigit;
                fnum *= (double)10;
                fnum += _FLOAT_ROUND_ADJUST;
                fprec++;
            } else *s++ = '0';
            prec--;
        }
    }
    *s++ = ((options & FF_LEXP)?('E'):('e'));
    if(fpower >= 0)
    {
        *s++ = '+';
    } else {
        *s++ = '-';
        fpower = -fpower;
    }
    if(fpower < 10) *s++ = '0';
    itoa64(fpower, s);
}

void FloatFormat(char *s, double value, int options)
{
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

    int decpt, sign;

    char *res = fcvt(value, prec, &decpt, &sign);

    if (res)
    {
        lstrcpyn(s, res, decpt + 1);
        lstrcat(s, ".");
        lstrcpy(s + decpt + 1, res + decpt);
    }

    /*if (options & FF_NOEXP) FloatFormatF(s, value, prec);
    else
    if (options & FF_EXP) FloatFormatE(s, value, options);
    else
    {
        double sfnum = value;
        int fsign, fpower;
        fpower = _floatp10(&sfnum, &fsign, -999);
        sfnum = value;
        fpower = _floatp10(&sfnum, &fsign, prec - fpower);

        if((value != 0.0) && ((fpower < -4) || (fpower >= prec)))
            FloatFormatE(s, value, options);
        else
        {
            prec -= (fpower + 1);
            if(prec <= 0) prec = 1;
            FloatFormatF(s, value, prec);
        }
    }*/
}

int lstrcmpn(char *s1, const char *s2, int chars)
{
    while ((chars > 0) && (*s1) && (*s2) && (*(s1) == *(s2))) chars--, s1++, s2++;
    if ((chars == 0) || (*s1 == *s2)) return 0;
    return (*s1 - *s2);
}