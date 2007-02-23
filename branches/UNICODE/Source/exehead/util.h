/*
 * util.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2007 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "../Platform.h"
#include "config.h"
#include <shlobj.h>

extern TCHAR ps_tmpbuf[NSIS_MAX_STRLEN*2];
TCHAR * NSISCALL GetNSISString(TCHAR *outbuf, int strtab);
#define GetNSISStringTT(strtab) GetNSISString(0, (strtab))
#define GetNSISStringNP(strtab) ((const TCHAR *)g_blocks[NB_STRINGS].offset+(strtab))
#define GetNSISTab(strtab) (strtab < 0 ? LANG_STR_TAB(strtab) : strtab)
void NSISCALL myRegGetStr(HKEY root, const TCHAR *sub, const TCHAR *name, TCHAR *out);
int NSISCALL myatoi(TCHAR *s);
void NSISCALL myitoa(TCHAR *s, int d);
TCHAR * NSISCALL mystrcpy(TCHAR *out, const TCHAR *in);
int NSISCALL mystrlen(const TCHAR *in);
TCHAR * NSISCALL mystrcat(TCHAR *out, const TCHAR *concat);
TCHAR * NSISCALL mystrstr(TCHAR *a, TCHAR *b);
WIN32_FIND_DATA * NSISCALL file_exists(TCHAR *buf);
TCHAR * NSISCALL my_GetTempFileName(TCHAR *buf, const TCHAR *dir);
void NSISCALL FreePIDL(LPITEMIDLIST idl);

//BOOL NSISCALL my_SetWindowText(HWND hWnd, const char *val);
#define my_SetWindowText SetWindowText
BOOL NSISCALL my_SetDialogItemText(HWND dlg, UINT idx, const TCHAR *val);
//#define my_SetDialogItemText SetDlgItemText
//int NSISCALL my_GetWindowText(HWND hWnd, char *val, int size);
#define my_GetWindowText GetWindowText
int NSISCALL my_GetDialogItemText(UINT idx, TCHAR *val);
//#define my_GetDialogItemText GetDlgItemText

#ifdef NSIS_CONFIG_LOG
extern TCHAR log_text[2048];
void NSISCALL log_write(int close);
const TCHAR * _RegKeyHandleToName(HKEY hKey);
void _LogData2Hex(TCHAR *buf, size_t buflen, unsigned char *data, size_t datalen);
void log_printf(TCHAR *format, ...);
#define log_printf2(x1,x2) log_printf(x1,x2);
#define log_printf3(x1,x2,x3) log_printf(x1,x2,x3);
#define log_printf4(x1,x2,x3,x4) log_printf(x1,x2,x3,x4);
#define log_printf5(x1,x2,x3,x4,x5) log_printf(x1,x2,x3,x4,x5);
#define log_printf6(x1,x2,x3,x4,x5,x6) log_printf(x1,x2,x3,x4,x5,x6);
#define log_printf7(x1,x2,x3,x4,x5,x6,x7) log_printf(x1,x2,x3,x4,x5,x6,x7);
#define log_printf8(x1,x2,x3,x4,x5,x6,x7,x8) log_printf(x1,x2,x3,x4,x5,x6,x7,x8);
#define RegKeyHandleToName(x1) _RegKeyHandleToName(x1);
#define LogData2Hex(x1,x2,x3,x4) _LogData2Hex(x1,x2,x3,x4);
extern int log_dolog;
extern TCHAR g_log_file[1024];
#else
#define log_printf(x1)
#define log_printf2(x1,x2)
#define log_printf3(x1,x2,x3)
#define log_printf4(x1,x2,x3,x4)
#define log_printf5(x1,x2,x3,x4,x5)
#define log_printf6(x1,x2,x3,x4,x5,x6)
#define log_printf7(x1,x2,x3,x4,x5,x6,x7)
#define log_printf8(x1,x2,x3,x4,x5,x6,x7,x8)
#define RegKeyHandleToName(x1) NULL
#define LogData2Hex(x1,x2,x3,x4)
#endif

HANDLE NSISCALL myCreateProcess(TCHAR *cmd, TCHAR *dir);
int NSISCALL my_MessageBox(const TCHAR *text, UINT type);

void NSISCALL myDelete(TCHAR *buf, int flags);

HANDLE NSISCALL myOpenFile(const TCHAR *fn, DWORD da, DWORD cd);
int NSISCALL validpathspec(TCHAR *ubuf);
TCHAR * NSISCALL addtrailingslash(TCHAR *str);
//char NSISCALL lastchar(const char *str);
#define lastchar(str) *CharPrev(str,str+mystrlen(str))
TCHAR * NSISCALL findchar(TCHAR *str, TCHAR c);
void NSISCALL trimslashtoend(TCHAR *buf);
TCHAR * NSISCALL skip_root(TCHAR *path);
int NSISCALL is_valid_instpath(TCHAR *s);
void NSISCALL validate_filename(TCHAR *fn);
void NSISCALL MoveFileOnReboot(LPCTSTR pszExisting, LPCTSTR pszNew);
void NSISCALL mini_memcpy(void *out, const void *in, size_t len);
void NSISCALL remove_ro_attr(TCHAR *file);

void * NSISCALL myGetProcAddress(TCHAR *dll, char *func);
void NSISCALL MessageLoop(UINT uCheckedMsg);

// Turn a pair of chars into a word
// Turn four chars into a dword
#ifdef UNICODE
#define DTCHAR DWORD
#define QTCHAR ULONGLONG
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((DWORD)(b))|((a)<<16))
#define CHAR4_TO_DWORD(a,b,c,d) (((QTCHAR)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<32))
#else
#define CHAR2_TO_WORD(a,b) (((DWORD)(a))|((b)<<16))
#define CHAR4_TO_DWORD(a,b,c,d) (((QTCHAR)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<32))
#endif
#else // UNICODE
#define DTCHAR WORD
#define QTCHAR DWORD
#ifdef __BIG_ENDIAN__ // Not very likely, but, still...
#define CHAR2_TO_WORD(a,b) (((WORD)(b))|((a)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(c,d))|(CHAR2_TO_WORD(a,b)<<16))
#else
#define CHAR2_TO_WORD(a,b) (((WORD)(a))|((b)<<8))
#define CHAR4_TO_DWORD(a,b,c,d) (((DWORD)CHAR2_TO_WORD(a,b))|(CHAR2_TO_WORD(c,d)<<16))
#endif
#endif
