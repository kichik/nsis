/*
 * ustring.c: Unicode string routines
 */

#include <wchar.h>
#include <time.h>
#include "halibut.h"

wchar_t *ustrdup(wchar_t * s)
{
  wchar_t *r;
  if (s)
  {
    r = mknewa(wchar_t, 1 + ustrlen(s));
    ustrcpy(r, s);
  } else
  {
    r = mknew(wchar_t);
    *r = 0;
  }
  return r;
}

char *ustrtoa(wchar_t * s, char *outbuf, int size)
{
  char *p;
  if (!s)
  {
    *outbuf = '\0';
    return outbuf;
  }
  for (p = outbuf; *s && p < outbuf + size; p++, s++)
    *p = *(char*)s;
  if (p < outbuf + size)
    *p = '\0';
  else
    outbuf[size - 1] = '\0';
  return outbuf;
}

int ustrlen(wchar_t * s)
{
  int len = 0;
  while (*s++)
    len++;
  return len;
}

wchar_t *uadv(wchar_t * s)
{
  return s + 1 + ustrlen(s);
}

wchar_t *ustrcpy(wchar_t * dest, wchar_t * source)
{
  wchar_t *ret = dest;
  do
  {
    *dest++ = *source;
  }
  while (*source++);
  return ret;
}

int ustrcmp(wchar_t * lhs, wchar_t * rhs)
{
  if (!lhs && !rhs)
    return 0;
  if (!lhs)
    return -1;
  if (!rhs)
    return +1;
  while (*lhs && *rhs && *lhs == *rhs)
    lhs++, rhs++;
  if (*lhs < *rhs)
    return -1;
  else if (*lhs > *rhs)
    return 1;
  return 0;
}

wchar_t utolower(wchar_t c)
{
  if (c == L'\0')
    return c;                   /* this property needed by ustricmp */
  /* FIXME: this doesn't even come close */
  if (c >= 'A' && c <= 'Z')
    c += 'a' - 'A';
  return c;
}

int ustricmp(wchar_t * lhs, wchar_t * rhs)
{
  wchar_t lc, rc;
  while ((lc = utolower(*lhs)) == (rc = utolower(*rhs)) && lc && rc)
    lhs++, rhs++;
  if (!lc && !rc)
    return 0;
  if (lc < rc)
    return -1;
  else
    return 1;
}

wchar_t *ustrlow(wchar_t * s)
{
  wchar_t *p = s;
  while (*p)
  {
    *p = utolower(*p);
    p++;
  }
  return s;
}

int utoi(wchar_t * s)
{
  int sign = +1;
  int n;

  if (*s == L'-')
  {
    s++;
    sign = -1;
  }

  n = 0;
  while (*s && *s >= L'0' && *s <= L'9')
  {
    n *= 10;
    n += (*s - '0');
    s++;
  }

  return n;
}

int utob(wchar_t * s)
{
  if (!ustricmp(s, L"yes") || !ustricmp(s, L"y") ||
      !ustricmp(s, L"true") || !ustricmp(s, L"t"))
    return TRUE;
  return FALSE;
}

int uisdigit(wchar_t c)
{
  return c >= L'0' && c <= L'9';
}

#define USTRFTIME_DELTA 128
wchar_t *ustrftime(wchar_t * wfmt, struct tm * timespec)
{
  void *blk = NULL;
  wchar_t *wblk, *wp;
  char *fmt, *text, *p;
  size_t size = 0;
  size_t len;

  /*
   * strftime has the entertaining property that it returns 0
   * _either_ on out-of-space _or_ on successful generation of
   * the empty string. Hence we must ensure our format can never
   * generate the empty string. Somebody throw a custard pie at
   * whoever was responsible for that. Please?
   */
  if (wfmt)
  {
    len = ustrlen(wfmt);
    fmt = mknewa(char, 2 + len);
    ustrtoa(wfmt, fmt + 1, len + 1);
    fmt[0] = ' ';
  } else
    fmt = " %c";

  while (1)
  {
    size += USTRFTIME_DELTA;
    blk = resize((char *) blk, size);
    len = strftime((char *) blk, size - 1, fmt, timespec);
    if (len > 0)
      break;
  }

  /* Note: +1 for the terminating 0, -1 for the initial space in fmt */
  wblk = resize((wchar_t *) blk, len);
  text = mknewa(char, len);
  strftime(text, len, fmt + 1, timespec);
  /*
   * We operate in the C locale, so this all ought to be kosher
   * ASCII. If we ever move outside ASCII machines, we may need
   * to make this more portable...
   */
  for (wp = wblk, p = text; *p; p++, wp++)
    *wp = *p;
  *wp = 0;
  if (wfmt)
    sfree(fmt);
  sfree(text);
  return wblk;
}
