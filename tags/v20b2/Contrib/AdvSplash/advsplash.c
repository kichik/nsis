// For layered windows
#define _WIN32_WINNT 0x0500

#include <windows.h>
#include "../exdll/exdll.h"

HINSTANCE g_hInstance;

#define RESOLUTION 20 // 50 fps ;)
#define KEYCOLOR_8BIT_RGBSUPPORT 1      // Includes (1) code for 
                                        // specifing key color for 8bit images as RGB

BITMAP bm;
HBITMAP g_hbm;
int g_rv=-1;
int resolution = RESOLUTION;
int sleep_val, fadein_val, fadeout_val, state, timeleft, keycolor, nt50, alphaparam;
int call = -1;

typedef BOOL (_stdcall *_tSetLayeredWindowAttributesProc)(HWND hwnd, // handle to the layered window
  COLORREF crKey,      // specifies the color key
  BYTE bAlpha,         // value for the blend function
  DWORD dwFlags        // action
);


_tSetLayeredWindowAttributesProc SetLayeredWindowAttributesProc;

static LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (uMsg == WM_CREATE)
  {
    RECT vp;

    SystemParametersInfo(SPI_GETWORKAREA, 0, &vp, 0);
    SetWindowLong(hwnd,GWL_STYLE,0);
    SetWindowPos(hwnd,NULL,
      vp.left+(vp.right-vp.left-bm.bmWidth)/2,
      vp.top+(vp.bottom-vp.top-bm.bmHeight)/2,
      bm.bmWidth,bm.bmHeight,
      SWP_NOZORDER);
    ShowWindow(hwnd,SW_SHOW);

    return 0;
  }
  if (uMsg == WM_PAINT)
  {
    PAINTSTRUCT ps;
    RECT r;

    HDC curdc=BeginPaint(hwnd,&ps);
    HDC hdc=CreateCompatibleDC(curdc);
    HBITMAP oldbm;
    oldbm=(HBITMAP)SelectObject(hdc,g_hbm);
    GetClientRect(hwnd,&r);

    BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0,SRCCOPY);
        
    SelectObject(hdc,oldbm);
    DeleteDC(hdc);
    EndPaint(hwnd,&ps);
    return 0;
  }
  if (uMsg == WM_CLOSE) return 0;
  if (uMsg == WM_TIMER || uMsg == WM_LBUTTONDOWN)
  {
    g_rv=(uMsg == WM_LBUTTONDOWN);
    DestroyWindow(hwnd);
  }
  return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
  g_hInstance=hInst;
        return TRUE;
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

void CALLBACK TimeProc(
  UINT uID,      
  UINT uMsg,     
  DWORD dwUser,  
  DWORD dw1,     
  DWORD dw2)
{
        switch (state)
        {
        // FadeIN
        case 0: if (timeleft == 0)
                {
                                        timeleft = sleep_val;
                    state++;
                                        if (nt50) call = 255;                                   
                } else { call = ((fadein_val-timeleft)*255)/fadein_val; break; }
        // Sleep
        case 1: if (timeleft == 0)
                {
                    timeleft = fadeout_val;
                    state++;                                
                } else break;
        // FadeOUT
        case 2: if (timeleft == 0)
                {
                                        PostMessage((HWND)dwUser, WM_TIMER, 0, 0);
                                        return;
                } else { call = ((timeleft)*255)/fadeout_val; break;    }
        }
        // Transparency value aquired, and could be set...
        if ((call >= 0) && nt50)
                SetLayeredWindowAttributesProc((HWND)dwUser, keycolor, 
                                        call, 
                                        alphaparam);
        call = -1;
        // Time is running out...
        timeleft--;
}

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  char fn[MAX_PATH];
  char temp[64];
  char *sleep=temp;

  EXDLL_INIT();

  popstring(temp);
  sleep_val = myatoi(temp) / RESOLUTION;
  popstring(temp);
  fadein_val = myatoi(temp) / RESOLUTION;
  popstring(temp);
  fadeout_val = myatoi(temp) / RESOLUTION;
  popstring(temp);
  keycolor = myatoi(temp);
  popstring(fn);

  // Check for winXP/2k
  nt50 = (LOBYTE(LOWORD(GetVersion())) >= 5);
  if (!nt50)
  {
    // Fading is unsupported at old windows versions...
        resolution = (sleep_val + fadein_val + fadeout_val) * RESOLUTION; 
        fadeout_val = fadein_val = 0;
        sleep_val = 1;
  } else alphaparam = LWA_ALPHA | ((keycolor == -1)?(0):(LWA_COLORKEY));        

  if (fn[0] && sleep_val>0)
  {
    MSG msg;
    char classname[4]="_sp";
    static WNDCLASS wc;
    wc.lpfnWndProc = WndProc;
    wc.hInstance = g_hInstance;
    wc.hCursor = LoadCursor(NULL,IDC_ARROW);
    wc.lpszClassName = classname;
    if (RegisterClass(&wc)) 
    {
      char fn2[MAX_PATH];
      lstrcpy(fn2,fn);
      lstrcat(fn,".bmp");
      lstrcat(fn2,".wav");
      g_hbm=LoadImage(NULL,fn,IMAGE_BITMAP,0,0,LR_CREATEDIBSECTION|LR_LOADFROMFILE);
      if (g_hbm) 
      {
        HWND myWnd;
                UINT timerEvent;

                // Get Bitmap Information
                GetObject(g_hbm, sizeof(bm), (LPSTR)&bm);

                myWnd = CreateWindowEx(WS_EX_TOOLWINDOW | ((nt50)?(WS_EX_LAYERED):(0)),classname,classname,
                                        0,0,0,0,0,(HWND)hwndParent,NULL,g_hInstance,NULL);

                // Set transparency / key color
                if (nt50)
                {
                        // Get blending proc address
                        HANDLE user32 = GetModuleHandle("user32");
                        SetLayeredWindowAttributesProc = (_tSetLayeredWindowAttributesProc) GetProcAddress(user32, "SetLayeredWindowAttributes");
                        // Use win2k method
                        SetLayeredWindowAttributesProc(myWnd, keycolor, 
                                (fadein_val > 0)?(0):(255), 
                                alphaparam);
                } else
                        if (keycolor != -1)
                        {
                                // Use simpliest region method
                                int x, y, wdelta;
                                HRGN region, cutrgn;
                                BYTE *bmp = bm.bmBits;

                                region = CreateRectRgn(0,0,bm.bmWidth,bm.bmHeight);
                                //region = CreateRectRgn(0,0,0,0);
                                
                                if (bm.bmBitsPixel == 8)
                                {
#if (KEYCOLOR_8BIT_RGBSUPPORT == 1)
                                        HDC hMemDC;
                                        HBITMAP hOldBitmap;
                                        int rgb[256];
                                        int nColors;

                                        // Find out how many colors are in the color table
                                        nColors = 1 << bm.bmBitsPixel;
                                        // Create a memory DC and select the DIBSection into it
                                        hMemDC = CreateCompatibleDC(NULL);
                                        hOldBitmap = SelectObject(hMemDC,g_hbm);
                                        // Get the DIBSection's color table
                                        GetDIBColorTable(hMemDC,0,nColors,(RGBQUAD*)rgb);

                                        // Find our keycolor at palette
                                        if (keycolor < 0x1000000)
                                        {
                                                for (x = 0; x < nColors; x++)
                                                        if (rgb[x] == keycolor) { keycolor = x; break; }
                                        } else keycolor &= 0xff;

                                        // Free used objects
                                        SelectObject(hMemDC,hOldBitmap);
                                        DeleteDC(hMemDC);
#endif

                                        // Bitmap is DWORD aligned by width
                                        //wdelta = (( bm.bmWidth + 3 ) & ~3) - bm.bmWidth;
                                        wdelta = ((bm.bmWidth + 3) & 3) ^ 3;
                                        // Search for transparent pixels 
                                        for (y = bm.bmHeight-1; y >= 0; y--, bmp += wdelta)
                                                for (x = 0; x < bm.bmWidth; )
                                                        if (*bmp == (BYTE) keycolor) 
                                                        {
                                                                int j = x;
                                                                while ((x < bm.bmWidth) && (*bmp == (BYTE) keycolor)) x++, bmp++;

                                                                // Cut transparent pixels from the original region
                                                                cutrgn = CreateRectRgn(j, y, x, y+1);
                                                                CombineRgn(region, region, cutrgn, RGN_XOR);
                                                                DeleteObject(cutrgn);
                                                        } else bmp++, x++;
                                } else if (bm.bmBitsPixel == 24)
                                {
                                        // Bitmap is DWORD aligned by width
                                        wdelta = ((bm.bmWidth*3 + 3 ) & 3) ^ 3;
                                        // Search for transparent pixels 
                                        for (y = bm.bmHeight-1; y >= 0; y--, bmp += wdelta)
                                                for (x = 0; x < bm.bmWidth; )
                                                        if ((*(int*)bmp & 0xFFFFFF) == keycolor) 
                                                        {
                                                                int j = x;
                                                                while ((x < bm.bmWidth) && ((*(int*)bmp & 0xFFFFFF) == keycolor)) bmp+=3, x++;

                                                                // Cut transparent pixels from the original region
                                                                cutrgn = CreateRectRgn(j, y, x, y+1);
                                                                CombineRgn(region, region, cutrgn, RGN_XOR);
                                                                DeleteObject(cutrgn);
                                                        } else bmp += 3, x++;
                                }
                                
                                // Set resulting region.
                                SetWindowRgn(myWnd, region, TRUE);
                        }

                PlaySound(fn2,NULL,SND_ASYNC|SND_FILENAME|SND_NODEFAULT);

                // Start up timer...
                state = 0; timeleft = fadein_val;
                timerEvent = timeSetEvent(resolution, RESOLUTION/4, TimeProc, (DWORD_PTR)myWnd, TIME_PERIODIC);

        while (IsWindow(myWnd) && GetMessage(&msg,myWnd,0,0))
        {
          DispatchMessage(&msg);
        }

        // Kill the timer...
        timeKillEvent(timerEvent);
        
        // Stop currently playing wave, we want to exit
        PlaySound(0,0,0);

        DeleteObject(g_hbm);
      }

	  // We should UnRegister class, since Windows NT series never does this by itself
	  UnregisterClass(wc.lpszClassName, g_hInstance);
    }
  }
  wsprintf(temp,"%d",g_rv);
  pushstring(temp);
}

#ifdef _DEBUG
void main()
{
}
#endif