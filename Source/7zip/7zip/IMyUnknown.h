// IMyUnknown.h

// #pragma once

#ifndef __MYUNKNOWN_H
#define __MYUNKNOWN_H

#ifdef _WIN32

// #include <guiddef.h>
#include <basetyps.h>

#else

#include "../../Platform.h"

#include <string>

#define HRESULT LONG

#if ((_MSC_VER >= 800) || defined(_STDCALL_SUPPORTED))
#  define STDMETHODCALLTYPE __stdcall
#else
#  if defined(__GNUC__) && defined(__i386__)
#    define STDMETHODCALLTYPE  __attribute__((__stdcall__))
#  else
#    define STDMETHODCALLTYPE
#  endif
#endif

#define PROPID ULONG
#define STDMETHOD_(t, f) virtual t STDMETHODCALLTYPE f
#define STDMETHOD(f) STDMETHOD_(HRESULT, f)
#define STDMETHODIMP_(type) type STDMETHODCALLTYPE
#define STDMETHODIMP STDMETHODIMP_(HRESULT)

#define VT_UI4 1
#define VT_BSTR 2

typedef struct _PROPVARIANT
{
  WORD vt;
  ULONG ulVal;
  wchar_t *bstrVal;
} PROPVARIANT;

#define S_OK 0
#define E_NOINTERFACE 0x80000001
#define E_ABORT 0x80000002
#define E_INVALIDARG 0x80070057
#define E_FAIL 0x80004005
#define E_OUTOFMEMORY 0x8007000E

#define PURE = 0;

typedef struct {
  unsigned long  Data1;
  unsigned short Data2;
  unsigned short Data3;
  unsigned char Data4[8];
} GUID;

#ifdef __cplusplus
    #define MY_EXTERN_C    extern "C"
#else
    #define MY_EXTERN_C    extern
#endif

#ifdef INITGUID
  #define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
      MY_EXTERN_C const GUID name = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }
#else
  #define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
      MY_EXTERN_C const GUID name
#endif

#ifdef __cplusplus
typedef const GUID & REFGUID;
#else
#define REFGUID const GUID * __MIDL_CONST
#endif

#define MIDL_INTERFACE(x) struct

inline int operator==(REFGUID g1, REFGUID g2)
{ 
  for (unsigned int i = 0; i < sizeof(g1); i++)
    if (((unsigned char *)&g1)[i] != ((unsigned char *)&g2)[i])
      return false;
  return true;
}
inline int operator!=(REFGUID g1, REFGUID g2)
  { return !(g1 == g2); }

struct IUnknown
{
  STDMETHOD(QueryInterface) (REFGUID iid, void **outObject) PURE;
  STDMETHOD_(ULONG, AddRef)() PURE;
  STDMETHOD_(ULONG, Release)() PURE;
};

#endif
  
#endif
