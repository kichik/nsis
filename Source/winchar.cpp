#include "Platform.h"
#include "winchar.h"
#include "util.h"

#include <stdexcept>

using std::runtime_error;

WCHAR *winchar_fromansi(const char* s, unsigned int codepage/*=CP_ACP*/)
{
  int l = MultiByteToWideChar(codepage, 0, s, -1, 0, 0);
  if (l == 0)
    throw runtime_error("Unicode conversion failed");

  WCHAR *ws = new WCHAR[l + 1];

  if (MultiByteToWideChar(codepage, 0, s, -1, ws, l + 1) == 0)
    throw runtime_error("Unicode conversion failed");

  return ws;
}

char *winchar_toansi(const WCHAR* ws, unsigned int codepage/*=CP_ACP*/)
{
  int l = WideCharToMultiByte(codepage, 0, ws, -1, 0, 0, 0, 0);
  if (l == 0)
    throw runtime_error("Unicode conversion failed");

  char *s = new char[l + 1];

  if (WideCharToMultiByte(codepage, 0, ws, -1, s, l + 1, 0, 0) == 0)
    throw runtime_error("Unicode conversion failed");

  return s;
}

WCHAR *winchar_strcpy(WCHAR *ws1, const WCHAR *ws2)
{
  WCHAR *ret = ws1;

  while (*ws2)
  {
    *ws1++ = *ws2++;
  }

  *ws1 = 0;

  return ret;
}

WCHAR *winchar_strncpy(WCHAR *ws1, const WCHAR *ws2, size_t n)
{
  WCHAR *ret = ws1;

  while (n && *ws2)
  {
    *ws1++ = *ws2++;
    n--;
  }

  while (n--)
  {
    *ws1++ = 0;
  }

  return ret;
}

size_t winchar_strlen(WCHAR *ws)
{
  size_t len = 0;

  while (*ws++)
  {
    len++;
  }

  return len;
}

int winchar_strcmp(const WCHAR *ws1, const WCHAR *ws2)
{
  WCHAR diff = 0;

  do
  {
    diff = *ws1 - *ws2;
  }
  while (*ws1++ && *ws2++);

  return static_cast<int>(diff);
}

int winchar_stoi(const WCHAR *ws)
{
  char *s = winchar_toansi(ws);

  int ret = atoi(s);

  delete [] s;

  return ret;
}
