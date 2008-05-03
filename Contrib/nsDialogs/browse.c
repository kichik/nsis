#include <windows.h>
#include <shlobj.h>

#include "defs.h"
#include "nsis.h"

int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData) {
  if (uMsg == BFFM_INITIALIZED)
    SendMessage(hwnd, BFFM_SETSELECTION, TRUE, pData);

  return 0;
}

void __declspec(dllexport) SelectFolderDialog(HWND hwndParent, int string_size, char *variables, stack_t **stacktop, extra_parameters *extra)
{
  BROWSEINFO bi;

  char result[MAX_PATH];
  char initial[MAX_PATH];
  char title[1024];
  LPITEMIDLIST resultPIDL;

  EXDLL_INIT();

  if (popstring(title, sizeof(initial)))
  {
    pushstring("error");
    return;
  }

  if (popstring(initial, sizeof(title)))
  {
    pushstring("error");
    return;
  }

  bi.hwndOwner = hwndParent;
  bi.pidlRoot = NULL;
  bi.pszDisplayName = result;
  bi.lpszTitle = title;
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x0040
#endif
  bi.ulFlags = BIF_STATUSTEXT | BIF_RETURNONLYFSDIRS | BIF_NEWDIALOGSTYLE;
  bi.lpfn = BrowseCallbackProc;
  bi.lParam = (LPARAM) initial;
  bi.iImage = 0;

  /*if (pField->pszRoot) {
    LPSHELLFOLDER sf;
    ULONG eaten;
    LPITEMIDLIST root;
    int ccRoot = (lstrlen(pField->pszRoot) * 2) + 2;
    LPWSTR pwszRoot = (LPWSTR) MALLOC(ccRoot);
    MultiByteToWideChar(CP_ACP, 0, pField->pszRoot, -1, pwszRoot, ccRoot);
    SHGetDesktopFolder(&sf);
    sf->ParseDisplayName(hConfigWindow, NULL, pwszRoot, &eaten, &root, NULL);
    bi.pidlRoot = root;
    sf->Release();
    FREE(pwszRoot);
  }*/

  resultPIDL = SHBrowseForFolder(&bi);
  if (!resultPIDL)
  {
    pushstring("error");
    return;
  }

  if (SHGetPathFromIDList(resultPIDL, result))
  {
    pushstring(result);
  }
  else
  {
    pushstring("error");
  }

  CoTaskMemFree(resultPIDL);
}

void __declspec(dllexport) SelectFileDialog(HWND hwndParent, int string_size, char *variables, stack_t **stacktop, extra_parameters *extra)
{
  OPENFILENAME ofn={0,}; // XXX WTF
  int save;
  char type[5];
  char path[1024];
  char filter[1024];
  char currentDirectory[1024];

  EXDLL_INIT();

  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.hwndOwner = hwndParent;
  ofn.lpstrFilter = filter;
  ofn.lpstrFile = path;
  ofn.nMaxFile  = sizeof(path);
  //ofn.Flags = pField->nFlags & (OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_CREATEPROMPT | OFN_EXPLORER);
  ofn.Flags = OFN_CREATEPROMPT | OFN_EXPLORER;

  popstring(type, sizeof(type));
  popstring(path, sizeof(path));
  popstring(filter, sizeof(filter));

  save = !lstrcmpi(type, "save");

  if (!filter[0])
  {
    lstrcpy(filter, "All Files|*.*");
  }

  {
    // Convert the filter to the format required by Windows: NULL after each
    // item followed by a terminating NULL
    char *p = filter;
    while (*p) // XXX take care for 1024
    {
      if (*p == '|')
      {
        *p++ = 0;
      }
      else
      {
        p = CharNext(p);
      }
    }
    p++;
    *p = 0;
  }

  GetCurrentDirectory(sizeof(currentDirectory), currentDirectory); // save working dir

  if ((save ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn)))
  {
    pushstring(path);
  }
  else if (CommDlgExtendedError() == FNERR_INVALIDFILENAME)
  {
    *path = '\0';
    if ((save ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn)))
    {
      pushstring(path);
    }
    else
    {
      pushstring("");
    }
  }
  else
  {
    pushstring("");
  }

  // restore working dir
  // OFN_NOCHANGEDIR doesn't always work (see MSDN)
  SetCurrentDirectory(currentDirectory);
}