#include <Windows.h>
#include <Mmsystem.h>
#include "../exdll/exdll.h"

#undef EXDLL_INIT

#define EXDLL_INIT() {  \
        g_stringsize=string_size; \
        g_stacktop=stacktop; }

#define NSISFunc(name) extern "C" void __declspec(dllexport) name(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)

char szTemp[2048];
HWND hWndImage, hWndParent;

HINSTANCE g_hInstance;

CRITICAL_SECTION CriticalSection;

void ECS() {
  EnterCriticalSection(&CriticalSection);
}

void LCS() {
  LeaveCriticalSection(&CriticalSection);
}

enum {
  MIL_DUMMY,
  MIL_GRADIENT,
  MIL_BITMAP,
  MIL_TRANSPARENT_BITMAP,
  MIL_TEXT
};

struct myImageList {
  BYTE iType;
  union {
    HBITMAP hBitmap;
    char *szText;
    COLORREF cGradientFrom;
  };
  RECT rPos;
  union {
    COLORREF cTransparent;
    COLORREF cTextColor;
    COLORREF cGradientTo;
  };
  HFONT hFont;

  BOOL bReady;

  myImageList *next;
} bgBitmap;

unsigned int uWndWidth, uWndHeight;

void *oldProc;
LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);
int myatoi(char *s);
COLORREF GetColor();
void GetXY(LPPOINT lpPoint);

BOOL bReturn;

NSISFunc(SetReturn) {
  EXDLL_INIT();

  popstring(szTemp);
  bReturn = !lstrcmpi(szTemp, "on");
}

static void my_pushstring(char *str)
{
  stack_t *th;
  if (!g_stacktop || !bReturn) return;
  th=(stack_t*)GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize);
  lstrcpyn(th->text,str,g_stringsize);
  th->next=*g_stacktop;
  *g_stacktop=th;
}

NSISFunc(SetBg) {
  EXDLL_INIT();

  ECS();

  if (!hWndImage) {
    hWndParent = hwndParent;

    if (!hwndParent) {
      my_pushstring("can't find parent window");
      return;
    }

    WNDCLASSEX wc = {
      sizeof(WNDCLASSEX),
      CS_VREDRAW|CS_HREDRAW,
      WndProc,
      0,
      0,
      g_hInstance,
      0,
      LoadCursor(0, IDC_ARROW),
      0,
      0,
      "NSISBGImage",
      0
    };
    ATOM atomClass = RegisterClassEx(&wc);
    if (!atomClass) {
      my_pushstring("can't create window");
      return;
    }

    hWndImage = CreateWindowEx(
      WS_EX_TOOLWINDOW,
      (LPSTR)atomClass,
      0,
      WS_CLIPSIBLINGS|WS_POPUP,
      0,
      0,
      0,
      0,
      0,
      0,
      g_hInstance,
      0
    );
    if (!hWndImage) {
      my_pushstring("can't create window");
      return;
    }

    oldProc = (void *)SetWindowLong(hwndParent, GWL_WNDPROC, (long)WndProc);
  }

  if (bgBitmap.iType == MIL_BITMAP) DeleteObject(bgBitmap.hBitmap);

  unsigned int uScrWidth = GetSystemMetrics(SM_CXSCREEN);
  unsigned int uScrHeight = GetSystemMetrics(SM_CYSCREEN);

  bgBitmap.iType = MIL_BITMAP;
  bgBitmap.rPos.right = 0;
  bgBitmap.rPos.bottom = 0;
  uWndWidth = uScrWidth;
  uWndHeight = uScrHeight;

  popstring(szTemp);
  if (!lstrcmpi(szTemp, "/GRADIENT")) {
    bgBitmap.cGradientFrom = GetColor();
    bgBitmap.cGradientTo = GetColor();

    bgBitmap.iType = MIL_GRADIENT;

    goto done;
  }
  if (!lstrcmpi(szTemp, "/FILLSCREEN")) {
    bgBitmap.rPos.right = uScrWidth;
    bgBitmap.rPos.bottom = uScrHeight;
    popstring(szTemp);
  }
  else if (!lstrcmpi(szTemp, "/TILED")) {
    popstring(szTemp);
  }
  else {
    uWndWidth = 0;
    uWndHeight = 0;
  }

  BITMAP bBitmap;

  bgBitmap.hBitmap = (HBITMAP)LoadImage(0, szTemp, IMAGE_BITMAP, bgBitmap.rPos.right, bgBitmap.rPos.bottom, LR_LOADFROMFILE);
  if (!bgBitmap.hBitmap) {
    my_pushstring("can't load bitmap");
    return;
  }

  if (!GetObject(bgBitmap.hBitmap, sizeof(bBitmap), (void *)&bBitmap)) {
    my_pushstring("can't load bitmap");
    return;
  }
  if (!bgBitmap.rPos.right) {
    bgBitmap.rPos.right = bBitmap.bmWidth;
    bgBitmap.rPos.bottom = bBitmap.bmHeight;
  }
  if (!uWndWidth) {
    uWndWidth = bBitmap.bmWidth;
    uWndHeight = bBitmap.bmHeight;
  }

done:

  bgBitmap.bReady = TRUE;

  LCS();

  if (hWndImage) {
    SetWindowPos(
      hWndImage,
      hWndParent,
      (uScrWidth-uWndWidth)/2,
      (uScrHeight-uWndHeight)/2,
      uWndWidth,
      uWndHeight,
      SWP_NOACTIVATE
    );
  }

  my_pushstring("success");
}

NSISFunc(AddImage) {
  ECS();

  myImageList *newImg = (myImageList *)GlobalAlloc(GPTR, sizeof(myImageList));
  if (!newImg) {
    my_pushstring("memory allocation error");
    return;
  }

  newImg->iType = MIL_BITMAP;
  newImg->cTransparent = -1;

  popstring(szTemp);
  if (!lstrcmpi(szTemp, "/TRANSPARENT")) {
    newImg->iType = MIL_TRANSPARENT_BITMAP;
    newImg->cTransparent = GetColor();
    popstring(szTemp);
  }

  newImg->hBitmap = (HBITMAP)LoadImage(0, szTemp, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
  if (!newImg->hBitmap) {
    my_pushstring("can't load bitmap");
    return;
  }

  GetXY(LPPOINT(&newImg->rPos));

  BITMAP bBitmap;

  if (!GetObject(newImg->hBitmap, sizeof(bBitmap), (void *)&bBitmap)) {
    my_pushstring("can't load bitmap");
    return;
  }
  newImg->rPos.right = newImg->rPos.left + bBitmap.bmWidth;
  newImg->rPos.bottom = newImg->rPos.top + bBitmap.bmHeight;

  myImageList *img = &bgBitmap;
  while (img->next) img = img->next;
  img->next = newImg;

  my_pushstring("success");

  LCS();
}

NSISFunc(AddText) {
  ECS();

  myImageList *newImg = (myImageList *)GlobalAlloc(GPTR, sizeof(myImageList));
  if (!newImg) {
    my_pushstring("memory allocation error");
    return;
  }

  newImg->iType = MIL_TEXT;

  popstring(szTemp);
  newImg->szText = (char *)GlobalAlloc(GPTR, lstrlen(szTemp)+1);
  if (!newImg->szText) {
    my_pushstring("memory allocation error");
    return;
  }
  lstrcpy(newImg->szText, szTemp);

  popstring(szTemp);
  newImg->hFont = (HFONT)myatoi(szTemp);
  newImg->cTextColor = GetColor();
  
  GetXY(LPPOINT(&newImg->rPos));
  GetXY(LPPOINT(&newImg->rPos) + 1);

  myImageList *img = &bgBitmap;
  while (img->next) img = img->next;
  img->next = newImg;

  my_pushstring("success");

  LCS();
}

NSISFunc(Redraw) {
  RedrawWindow(hWndImage, 0, 0, RDW_INVALIDATE | RDW_UPDATENOW);
  ShowWindow(hWndImage, SW_SHOWNA);
}

NSISFunc(Clear) {
  ECS();

  BOOL bIsFirst = TRUE;

  myImageList *img = &bgBitmap;
  while (img) {
    switch (img->iType) {
    	case MIL_BITMAP:
      case MIL_TRANSPARENT_BITMAP:
        DeleteObject(img->hBitmap);
        break;
      case MIL_TEXT:
        GlobalFree(img->szText);
        break;
    }

    myImageList *thisImg = img;

    img = img->next;

    if (!bIsFirst)
      GlobalFree(thisImg);
    else
      bIsFirst = FALSE;
  }

  bgBitmap.next = 0;
  bgBitmap.bReady = FALSE;

  LCS();
}

NSISFunc(Destroy) {
  SetWindowLong(hWndParent, GWL_WNDPROC, (long)oldProc);
  SendMessage(hWndImage, WM_CLOSE, 0, 0);
  hWndImage = 0;
  Clear(0, 0, 0, 0);
  UnregisterClass("NSISBGImage", g_hInstance);
}

NSISFunc(Sound) {
  char szLoop[] = {'/', 'L', 'O', 'O', 'P', 0};
  char szWait[] = {'/', 'W', 'A', 'I', 'T', 0};
  char szStop[] = {'/', 'S', 'T', 'O', 'P', 0};

  DWORD flags = SND_FILENAME | SND_NODEFAULT;
  g_stacktop=stacktop;
  popstring(szTemp);
  if (lstrcmpi(szTemp, szWait))
    flags |= SND_ASYNC;
  else
    popstring(szTemp);
  if (!lstrcmpi(szTemp, szLoop)) {
    flags |= SND_LOOP;
    popstring(szTemp);
  }
  PlaySound(lstrcmpi(szTemp, szStop) ? szTemp : 0, 0, flags);
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
  if (hwnd == hWndParent) {
    if (message == WM_SIZE) {
      ShowWindow(hWndImage, wParam == SIZE_MINIMIZED ? SW_HIDE : SW_SHOW);
    }
    if (message == WM_WINDOWPOSCHANGED) {
      SetWindowPos(hWndImage, hWndParent, 0, 0, 0, 0, SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);
    }
    return CallWindowProc(
      (long (__stdcall *)(HWND,unsigned int,unsigned int,long))oldProc,
      hwnd,
      message,
      wParam,
      lParam
    );
  }
  switch (message) {
    case WM_PAINT:
    if (bgBitmap.bReady) {
      ECS();

      PAINTSTRUCT ps;
      HDC hdc = BeginPaint(hwnd, &ps);

      if (bgBitmap.iType == MIL_BITMAP) {
        HDC cdc = CreateCompatibleDC(hdc);
        SelectObject(cdc, bgBitmap.hBitmap);
        for (unsigned int x = 0; x < uWndWidth; x += bgBitmap.rPos.right) {
          for (unsigned int y = 0; y < uWndHeight; y += bgBitmap.rPos.bottom) {
            BitBlt(hdc, x, y, bgBitmap.rPos.right, bgBitmap.rPos.bottom, cdc, 0, 0, SRCCOPY);
          }
        }
        DeleteDC(cdc);
      }
      else {
        int r = GetRValue(bgBitmap.cGradientFrom) << 10;
        int g = GetGValue(bgBitmap.cGradientFrom) << 10;
        int b = GetBValue(bgBitmap.cGradientFrom) << 10;
        int dr = ((GetRValue(bgBitmap.cGradientTo) << 10) - r) / (int)uWndHeight * 4;
        int dg = ((GetGValue(bgBitmap.cGradientTo) << 10) - g) / (int)uWndHeight * 4;
        int db = ((GetBValue(bgBitmap.cGradientTo) << 10) - b) / (int)uWndHeight * 4;
        RECT rect;
        rect.left = 0;
        rect.top = 0;
        rect.right = uWndWidth;
        rect.bottom = 4;
        while (rect.top < (int)uWndHeight)
        {
          HBRUSH brush = CreateSolidBrush(RGB(r>>10,g>>10,b>>10));
          FillRect(hdc, &rect, brush);
          DeleteObject(brush);
          rect.top+=4;
          rect.bottom+=4;
          r+=dr;
          g+=dg;
          b+=db;
        }
      }

      myImageList *img = bgBitmap.next;
      while (img) {
        if (img->iType == MIL_TEXT) {
          SetBkMode(hdc, TRANSPARENT);

          SetTextColor(hdc, img->cTextColor);
          SelectObject(hdc, img->hFont);
          DrawText(hdc, img->szText, -1, &img->rPos, DT_TOP | DT_LEFT | DT_NOPREFIX | DT_WORDBREAK);
        }
        else if (img->iType == MIL_BITMAP) {
          HDC cdc = CreateCompatibleDC(hdc);
          SelectObject(cdc, img->hBitmap);
          BitBlt(hdc, img->rPos.left, img->rPos.top, img->rPos.right - img->rPos.left, img->rPos.bottom - img->rPos.top, cdc, 0, 0, SRCCOPY);
          DeleteDC(cdc);
        }
        else {
          COLORREF   cColor;
          HBITMAP    bmAndBack, bmAndObject, bmAndMem, bmSave;
          HBITMAP    bmBackOld, bmObjectOld, bmMemOld, bmSaveOld;
          HDC        hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave;
          POINT      ptSize;

          HBITMAP hBitmap = img->hBitmap;

          hdcTemp = CreateCompatibleDC(hdc);
          SelectObject(hdcTemp, hBitmap);   // Select the bitmap

          ptSize.x = img->rPos.right - img->rPos.left;
          ptSize.y = img->rPos.bottom - img->rPos.top;
          DPtoLP(hdcTemp, &ptSize, 1);  // Convert from device to logical points

          // Create some DCs to hold temporary data.
          hdcBack   = CreateCompatibleDC(hdc);
          hdcObject = CreateCompatibleDC(hdc);
          hdcMem    = CreateCompatibleDC(hdc);
          hdcSave   = CreateCompatibleDC(hdc);

          // Create a bitmap for each DC. DCs are required for a number of
          // GDI functions.

          // Monochrome DC
          bmAndBack   = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);

          // Monochrome DC
          bmAndObject = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);

          bmAndMem    = CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
          bmSave      = CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

          // Each DC must select a bitmap object to store pixel data.
          bmBackOld   = (HBITMAP)SelectObject(hdcBack, bmAndBack);
          bmObjectOld = (HBITMAP)SelectObject(hdcObject, bmAndObject);
          bmMemOld    = (HBITMAP)SelectObject(hdcMem, bmAndMem);
          bmSaveOld   = (HBITMAP)SelectObject(hdcSave, bmSave);

          // Set proper mapping mode.
          SetMapMode(hdcTemp, GetMapMode(hdc));

          // Save the bitmap sent here, because it will be overwritten.
          BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY);

          // Set the background color of the source DC to the color.
          // contained in the parts of the bitmap that should be transparent
          cColor = SetBkColor(hdcTemp, img->cTransparent);

          // Create the object mask for the bitmap by performing a BitBlt
          // from the source bitmap to a monochrome bitmap.
          BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
              SRCCOPY);

          // Set the background color of the source DC back to the original
          // color.
          SetBkColor(hdcTemp, cColor);

          // Create the inverse of the object mask.
          BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
              NOTSRCCOPY);

          // Copy the background of the main DC to the destination.
          BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, img->rPos.left, img->rPos.top,
              SRCCOPY);

          // Mask out the places where the bitmap will be placed.
          BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

          // Mask out the transparent colored pixels on the bitmap.
          BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);

          // XOR the bitmap with the background on the destination DC.
          BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCPAINT);

          // Copy the destination to the screen.
          BitBlt(hdc, img->rPos.left, img->rPos.top, ptSize.x, ptSize.y, hdcMem, 0, 0,
              SRCCOPY);

          // Place the original bitmap back into the bitmap sent here.
          BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

          // Delete the memory bitmaps.
          DeleteObject(SelectObject(hdcBack, bmBackOld));
          DeleteObject(SelectObject(hdcObject, bmObjectOld));
          DeleteObject(SelectObject(hdcMem, bmMemOld));
          DeleteObject(SelectObject(hdcSave, bmSaveOld));

          // Delete the memory DCs.
          DeleteDC(hdcMem);
          DeleteDC(hdcBack);
          DeleteDC(hdcObject);
          DeleteDC(hdcSave);
          DeleteDC(hdcTemp);
        }
        img = img->next;
      }

      LCS();

      EndPaint(hwnd, &ps);
    }
    break;
    case WM_WINDOWPOSCHANGING:
      {
        LPWINDOWPOS wp = (LPWINDOWPOS) lParam;
        wp->flags |= SWP_NOACTIVATE;
        wp->hwndInsertAfter = hWndParent;
        break;
      }
    case WM_CLOSE:
      DestroyWindow(hwnd);
    break;
    default:
      return DefWindowProc(hwnd, message, wParam, lParam);
  }
  return 0;
}

COLORREF GetColor() {
  int iRed, iGreen, iBlue;
  popstring(szTemp);
  iRed = myatoi(szTemp);
  popstring(szTemp);
  iGreen = myatoi(szTemp);
  popstring(szTemp);
  iBlue = myatoi(szTemp);
  return RGB(iRed, iGreen, iBlue);
}

void GetXY(LPPOINT lpPoint) {
  popstring(szTemp);
  int iPosTemp = myatoi(szTemp);
  if (iPosTemp < 0) iPosTemp = iPosTemp + (int)uWndWidth;
  lpPoint->x = (unsigned int)iPosTemp;

  popstring(szTemp);
  iPosTemp = myatoi(szTemp);
  if (iPosTemp < 0) iPosTemp = iPosTemp + (int)uWndHeight;
  lpPoint->y = (unsigned int)iPosTemp;
}

int myatoi(char *s)
{
  unsigned int v=0;
  if (*s == '0' && (s[1] == 'x' || s[1] == 'X'))
  {
    s+=2;
    for (;;)
    {
      int c=*s++;
      if (c >= '0' && c <= '9') c-='0';
      else if (c >= 'a' && c <= 'f') c-='a'-10;
      else if (c >= 'A' && c <= 'F') c-='A'-10;
      else break;
      v<<=4;
      v+=c;
    }
  }
  else if (*s == '0' && s[1] <= '7' && s[1] >= '0')
  {
    s++;
    for (;;)
    {
      int c=*s++;
      if (c >= '0' && c <= '7') c-='0';
      else break;
      v<<=3;
      v+=c;
    }
  }
  else
  {
    int sign=0;
    if (*s == '-') { s++; sign++; }
    for (;;)
    {
      int c=*s++ - '0';
      if (c < 0 || c > 9) break;
      v*=10;
      v+=c;
    }
    if (sign) return -(int) v;
  }
  return (int)v;
}

BOOL WINAPI _DllMainCRTStartup(HINSTANCE hInst, ULONG ul_reason_for_call, LPVOID lpReserved) {
  g_hInstance=hInst;
  switch (ul_reason_for_call) {
  	case DLL_PROCESS_ATTACH:
      InitializeCriticalSection(&CriticalSection);
      break;
    case DLL_PROCESS_DETACH:
      DeleteCriticalSection(&CriticalSection);
      break;
  }
  return TRUE;
}