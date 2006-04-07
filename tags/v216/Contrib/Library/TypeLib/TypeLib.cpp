/*

  NSIS plug-in for Type Library Registration/UnRegistration
  Written by Joost Verburg

*/

#include <windows.h>
#include "../../ExDLL/exdll.h"

#define NSISFunction(funcname) extern "C" void __declspec(dllexport) funcname(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)

extern "C" BOOL WINAPI DllMain(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  return TRUE;
}

// Functions

NSISFunction(Register) {

  EXDLL_INIT();

  char filename[1024];
  popstring(filename);

  wchar_t ole_filename[1024];
  MultiByteToWideChar(CP_ACP, 0, filename, 1024, ole_filename, 1024);

  ITypeLib* typeLib;
  HRESULT hr;

  hr = LoadTypeLib(ole_filename, &typeLib);

  if (SUCCEEDED(hr)) {

    RegisterTypeLib(typeLib, ole_filename, NULL);

    hr = typeLib->Release();

  }

}

NSISFunction(UnRegister) {

  EXDLL_INIT();

  char filename[1024];
  popstring(filename);

  wchar_t ole_filename[1024];
  MultiByteToWideChar(CP_ACP, 0, filename, 1024, ole_filename, 1024);

  ITypeLib* typeLib;
  HRESULT hr;

  hr = LoadTypeLibEx(ole_filename, REGKIND_NONE, &typeLib);

  if (SUCCEEDED(hr))
  {

    TLIBATTR* typelibAttr;

    hr = typeLib->GetLibAttr(&typelibAttr);

    if(SUCCEEDED(hr))
    {

      UnRegisterTypeLib(typelibAttr->guid,
        typelibAttr->wMajorVerNum,
        typelibAttr->wMinorVerNum,
        typelibAttr->lcid,
        typelibAttr->syskind);

      typeLib->ReleaseTLibAttr(typelibAttr);

    }

    typeLib->Release();

  }

}

NSISFunction(GetLibVersion) {

  EXDLL_INIT();

  char filename[1024];
  popstring(filename);

  wchar_t ole_filename[1024];
  MultiByteToWideChar(CP_ACP, 0, filename, 1024, ole_filename, 1024);

  ITypeLib* typeLib;
  HRESULT hr;

  hr = LoadTypeLib(ole_filename, &typeLib);

  if (SUCCEEDED(hr))
  {

    TLIBATTR* typelibAttr;

    hr = typeLib->GetLibAttr(&typelibAttr);

    if (SUCCEEDED(hr))
    {

      char buf[33];

      wsprintf(buf, "%d", typelibAttr->wMajorVerNum);
      pushstring(buf);
      wsprintf(buf, "%d", typelibAttr->wMinorVerNum);
      pushstring(buf);

      typeLib->ReleaseTLibAttr(typelibAttr);

    }

    typeLib->Release();

  }

}
