/*

  NSIS plug-in for Type Library Registration/UnRegistration
  Written by Joost Verburg

  Unicode support by Jim Park -- 08/23/2007

*/

#include <windows.h>
#include <nsis/pluginapi.h> // nsis plugin
#include <nsis/nsis_tchar.h>

#define NSISFunction(funcname) extern "C" void __declspec(dllexport) funcname(HWND hwndParent, int string_size, TCHAR *variables, stack_t **stacktop)

extern "C" BOOL WINAPI DllMain(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  return TRUE;
}

NSISFunction(Register) {

  EXDLL_INIT();

  wchar_t ole_filename[1024];
  PopStringW(ole_filename);

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

  wchar_t ole_filename[1024];
  PopStringW(ole_filename);

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

  wchar_t ole_filename[1024];
  PopStringW(ole_filename);

  ITypeLib* typeLib;
  HRESULT hr;

  hr = LoadTypeLib(ole_filename, &typeLib);

  if (SUCCEEDED(hr))
  {

    TLIBATTR* typelibAttr;

    hr = typeLib->GetLibAttr(&typelibAttr);

    if (SUCCEEDED(hr))
    {

      TCHAR buf[33];

      wsprintf(buf, _T("%d"), typelibAttr->wMajorVerNum);
      pushstring(buf);
      wsprintf(buf, _T("%d"), typelibAttr->wMinorVerNum);
      pushstring(buf);

      typeLib->ReleaseTLibAttr(typelibAttr);

    }
    else
    {
      pushstring(_T("0"));
      pushstring(_T("0"));
    }

    typeLib->Release();

  }
  else
  {
    pushstring(_T("0"));
    pushstring(_T("0"));
  }

}
