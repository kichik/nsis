/*
 * DialogTemplate.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2002-2025 Amir Szekely <kichik@netvision.net.il> and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 *
 * Reviewed for Unicode support by Jim Park -- 08/21/2007
 */

#if !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
#define AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Platform.h"
#include "winchar.h"

#include <vector>
#include <stdexcept>

#ifndef __BIG_ENDIAN__
#  define EXTENDED_DIALOG ((DWORD) 0xFFFF0001)
#else
#  define EXTENDED_DIALOG ((DWORD) 0x0100FFFF)
#endif

struct DialogItemTemplate {
  DWORD  dwHelpId; // Extended only

  short  sX;
  short  sY;
  short  sWidth;
  short  sHeight;
  DWORD  dwExtStyle;
  DWORD  dwStyle;
  WORD   wId;

  WINWCHAR *szClass;
  WINWCHAR *szTitle;
  char  *szCreationData;

  WORD  wCreateDataSize;
};

#pragma pack(1)

#ifndef _WIN32
typedef struct {
    DWORD style;
    DWORD dwExtendedStyle;
    WORD cdit;
    short x;
    short y;
    short cx;
    short cy;
} DLGTEMPLATE;
#endif

typedef struct {
  WORD   dlgVer;
  WORD   signature;
  DWORD  helpID;
  DWORD  exStyle;
  DWORD  style;
  WORD   cDlgItems;
  short  x;
  short  y;
  short  cx;
  short  cy;
} DLGTEMPLATEEX;

#ifndef _WIN32
typedef struct {
    DWORD style;
    DWORD dwExtendedStyle;
    short x;
    short y;
    short cx;
    short cy;
    WORD id;
} DLGITEMTEMPLATE;
#endif

typedef struct {
  DWORD  helpID;
  DWORD  exStyle;
  DWORD  style;
  short  x;
  short  y;
  short  cx;
  short  cy;
  WORD   id;
  WORD   _microsoft_docs_are_wrong;
} DLGITEMTEMPLATEEX;

#pragma pack()

class CDialogTemplate {
public:
  CDialogTemplate(BYTE* pbData, bool build_unicode, unsigned int uCodePage=CP_ACP);
  virtual ~CDialogTemplate();

  short GetWidth();
  short GetHeight();
  DialogItemTemplate* GetItem(WORD wId);
  DialogItemTemplate* GetItemByIdx(DWORD i);
  int   RemoveItem(WORD wId);
  void  SetFont(TCHAR* szFaceName, WORD wFontSize);
  void  AddItem(DialogItemTemplate item);
#ifdef _WIN32
  HWND  CreateDummyDialog();
#endif
  void  MoveAll(short x, short y);
  void  Resize(short x, short y);
  void  PixelsToDlgUnits(short& x, short& y);
  void  DlgUnitsToPixels(short& x, short& y);
#ifdef _WIN32
  static inline bool SupportsDialogUnitComputation() { return true; }
  void  PixelsToDlgUnits(SIZE& siz);
  SIZE  GetStringSize(WORD id, TCHAR *str);
  void  RTrimToString(WORD id, TCHAR *str, int margins);
  void  LTrimToString(WORD id, TCHAR *str, int margins);
  void  CTrimToString(WORD id, TCHAR *str, int margins);
#else
  static inline bool SupportsDialogUnitComputation() { return false; }
#endif
  void  ConvertToRTL();
  BYTE* Save(DWORD& dwSize);
  static void FreeSavedTemplate(BYTE*pDT) { delete [] pDT; }
  DWORD GetSize();

private:
  bool  m_bExtended;
  bool  m_build_unicode;

  DWORD m_dwHelpId; // Extended only

  short m_sX;
  short m_sY;
  short m_sWidth;
  short m_sHeight;
  DWORD m_dwExtStyle;
  DWORD m_dwStyle;

  WINWCHAR *m_szMenu;
  WINWCHAR *m_szClass;
  WINWCHAR *m_szTitle;

   // Only if DS_FONT style is set
  short m_sFontSize;
  short m_sFontWeight; // Extended only
  BYTE  m_bItalic; // Extended only
  BYTE  m_bCharset; // Extended only
  WINWCHAR *m_szFont;

  // For (en/de)coding Unicode
  unsigned int m_uCodePage;

  // Items vector
  std::vector<DialogItemTemplate*> m_vItems;
};

#endif // !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
