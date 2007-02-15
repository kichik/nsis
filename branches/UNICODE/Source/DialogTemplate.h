/*
 * DialogTemplate.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 2002 Amir Szekely <kichik@netvision.net.il>
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#if !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
#define AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Platform.h"

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

  char  *szClass;
  char  *szTitle;
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
  WORD   _miscrosoft_docs_are_wrong;
} DLGITEMTEMPLATEEX;

#pragma pack()

class CDialogTemplate {
public:
  CDialogTemplate(BYTE* pbData, unsigned int uCodePage=CP_ACP);
  virtual ~CDialogTemplate();

  short GetWidth();
  short GetHeight();
  DialogItemTemplate* GetItem(WORD wId);
  DialogItemTemplate* GetItemByIdx(DWORD i);
  int   RemoveItem(WORD wId);
  void  SetFont(char* szFaceName, WORD wFontSize);
  void  AddItem(DialogItemTemplate item);
#ifdef _WIN32
  HWND  CreateDummyDialog();
#endif
  void  MoveAll(short x, short y);
  void  Resize(short x, short y);
#ifdef _WIN32
  void  PixelsToDlgUnits(short& x, short& y);
  void  DlgUnitsToPixels(short& x, short& y);
  SIZE  GetStringSize(WORD id, char *str);
  void  RTrimToString(WORD id, char *str, int margins);
  void  LTrimToString(WORD id, char *str, int margins);
  void  CTrimToString(WORD id, char *str, int margins);
#endif
  void  ConvertToRTL();
  BYTE* Save(DWORD& dwSize);
  DWORD GetSize();

private:
  bool  m_bExtended;

  DWORD m_dwHelpId; // Extended only

  short m_sX;
  short m_sY;
  short m_sWidth;
  short m_sHeight;
  DWORD m_dwExtStyle;
  DWORD m_dwStyle;

  char* m_szMenu;
  char* m_szClass;
  char* m_szTitle;

   // Only if DS_FONT style is set
  short m_sFontSize;
  short m_sFontWeight; // Extended only
  BYTE  m_bItalic; // Extended only
  BYTE  m_bCharset; // Extended only
  char* m_szFont;

  // For (en/de)coding Unicode
  unsigned int m_uCodePage;

  // Items vector
  std::vector<DialogItemTemplate*> m_vItems;
};

#endif // !defined(AFX_DIALOGTEMPLATE_H__C5A973AF_0F56_4BEC_814A_79318E2EB4AC__INCLUDED_)
