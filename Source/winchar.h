#include "Platform.h"

WCHAR *winchar_fromansi(const char* s, unsigned int codepage = CP_ACP);
char *winchar_toansi(const WCHAR* ws, unsigned int codepage = CP_ACP);
WCHAR *winchar_strcpy(WCHAR *ws1, const WCHAR *ws2);
WCHAR *winchar_strncpy(WCHAR *ws1, const WCHAR *ws2, size_t n);
size_t winchar_strlen(WCHAR *ws);
WCHAR *winchar_strdup(WCHAR *ws);
int winchar_strcmp(const WCHAR *ws1, const WCHAR *ws2);
int winchar_stoi(const WCHAR *ws);
