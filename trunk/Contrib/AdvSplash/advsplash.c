// For layered windows
#define _WIN32_WINNT 0x0500

#include <windows.h>
#include <windowsx.h>
#include "..\exdll\exdll.h"

HINSTANCE g_hInstance;

#define RESOLUTION 32 // 30 fps ;) (32? I like SHR more than iDIV ;)

BITMAP bm;
HBITMAP g_hbm;
int g_rv;
int resolution;
int sleep_val, fadein_val, fadeout_val, state, timeleft, keycolor, nt50, alphaparam;
const char classname[4]="_sp";

typedef BOOL (_stdcall *_tSetLayeredWindowAttributesProc)(HWND hwnd, // handle to the layered window
  COLORREF crKey,      // specifies the color key
  BYTE bAlpha,         // value for the blend function
  DWORD dwFlags        // action
);
_tSetLayeredWindowAttributesProc SetLayeredWindowAttributesProc;

static LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    RECT r;
    HDC curdc = NULL;
    HDC hdc;
    HBITMAP oldbm;

    switch (uMsg)
    {
    case WM_CREATE:
        SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0);
        SetWindowLong(hwnd,GWL_STYLE,0);
        SetWindowPos(hwnd,NULL,
        r.left+(r.right-r.left-bm.bmWidth)/2,
        r.top+(r.bottom-r.top-bm.bmHeight)/2,
        bm.bmWidth,bm.bmHeight,
        SWP_NOZORDER | SWP_SHOWWINDOW);
        return 0;

    case WM_PAINT:
        curdc=BeginPaint(hwnd,&ps);
        hdc=CreateCompatibleDC(curdc);
        GetClientRect(hwnd,&r);

        oldbm = SelectObject(hdc, g_hbm);
        BitBlt(curdc,r.left,r.top,r.right-r.left,r.bottom-r.top,hdc,0,0, SRCCOPY);
        
        SelectObject(hdc,oldbm);
        DeleteDC(hdc);  
        EndPaint(hwnd,&ps);

    case WM_CLOSE:
        return 0;

    case WM_TIMER:
    case WM_LBUTTONDOWN:
        g_rv=(uMsg == WM_LBUTTONDOWN);
        DestroyWindow(hwnd);
        break;
    }
    return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

void SetTransparentRegion(HWND myWnd)
{
    HDC dc;
    int x, y;
    HRGN region, cutrgn;
    BITMAPINFO bmi;
    int size = bm.bmWidth * bm.bmHeight*4;
    int *bmp = GlobalAlloc(GPTR, size);
    bmi.bmiHeader.biBitCount = 32;
    bmi.bmiHeader.biCompression = BI_RGB;
    bmi.bmiHeader.biHeight = bm.bmHeight;
    bmi.bmiHeader.biPlanes = 1;
    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = bm.bmWidth;
    bmi.bmiHeader.biClrUsed = 0;
    bmi.bmiHeader.biClrImportant = 0;

    dc = CreateCompatibleDC(NULL);
    SelectObject(dc, g_hbm);

    x = GetDIBits(dc, g_hbm, 0, bm.bmHeight, bmp, &bmi, DIB_RGB_COLORS);

    region = CreateRectRgn(0,0,bm.bmWidth,bm.bmHeight);

    // Search for transparent pixels 
    for (y = bm.bmHeight-1; y >= 0; y--)
        for (x = 0; x < bm.bmWidth; )
        if ((*bmp & 0xFFFFFF) == keycolor) 
        {
            int j = x;
            while ((x < bm.bmWidth) && ((*bmp & 0xFFFFFF) == keycolor)) bmp++, x++;

            // Cut transparent pixels from the original region
            cutrgn = CreateRectRgn(j, y, x, y+1);
            CombineRgn(region, region, cutrgn, RGN_XOR);
            DeleteObject(cutrgn);
        } else bmp++, x++;
                                
    // Set resulting region.
    SetWindowRgn(myWnd, region, TRUE);
    DeleteObject(region);
    DeleteObject(dc);
    GlobalFree(bmp);
}

BOOL WINAPI _DllMainCRTStartup(HANDLE hInst, ULONG ul_reason_for_call, LPVOID lpReserved)
{
    g_hInstance=hInst;
    return TRUE;
}

void CALLBACK TimeProc(
  UINT uID,      
  UINT uMsg,     
  DWORD dwUser,  
  DWORD dw1,     
  DWORD dw2)
{
        int call = -1;
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
                    // fadeout
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
        
        // Time is running out...
        timeleft--;
}

int myatoi(char *s);

void __declspec(dllexport) show(HWND hwndParent, int string_size, char *variables, stack_t **stacktop)
{
  DEVMODE dm;
  char fn[MAX_PATH];
  char temp[64];

  g_rv = -1;
  resolution = RESOLUTION;

  EXDLL_INIT();

  popstring(temp);
  sleep_val = myatoi(temp);
  popstring(temp);
  fadein_val = myatoi(temp);
  popstring(temp);
  fadeout_val = myatoi(temp);
  popstring(temp);
  keycolor = myatoi(temp);
  popstring(fn);

  dm.dmSize = sizeof(DEVMODE);
  EnumDisplaySettings(NULL, ENUM_CURRENT_SETTINGS, &dm);
  // Check for winXP/2k at 32 bpp transparency
  nt50 = (LOBYTE(LOWORD(GetVersion())) >= 5) && !((dm.dmBitsPerPel < 32) && (keycolor != -1));
  if (!nt50)
  {
      // Fading+transparency is unsupported at old windows versions...
      resolution = sleep_val + fadein_val + fadeout_val; 
      fadeout_val = fadein_val = 0;
      sleep_val = 1;
  } else 
  {
      // div them by resolution
      sleep_val >>= 5;
      fadein_val >>= 5;
      fadeout_val >>= 5;

      alphaparam = LWA_ALPHA | ((keycolor == -1)?(0):(LWA_COLORKEY));        
      keycolor = ((keycolor & 0xFF) << 16) + (keycolor & 0xFF00) + ((keycolor & 0xFF0000) >> 16);
  }

  if (fn[0] && ((sleep_val+fadein_val+fadeout_val)>0))
  {
    MSG msg;
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
                            // transparency mode                                
                            SetTransparentRegion(myWnd);
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
