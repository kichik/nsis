#include <windows.h>
#include "resource.h"
#include "config.h"
#include "fileform.h"
#include "state.h"
#include "ui.h"
#include "util.h"

#ifdef NSIS_SUPPORT_BGBG

int bg_color1, bg_color2, bg_textcolor;

LRESULT CALLBACK BG_WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
    case WM_PAINT:
      {
        static PAINTSTRUCT ps;
        HDC hdc=BeginPaint(hwnd,&ps);
        RECT r;
        int y,ry;
        GetClientRect(hwnd,&r);
        // this portion by Drew Davidson, drewdavidson@mindspring.com
        ry=r.bottom;
        y=0; //r.top

        // JF: made slower, reduced to 4 pixels high, because I like how it looks better/
        while (y < r.bottom)
        {
          int rv,gv,bv;
		      RECT rect;
		      HBRUSH brush;
          rv = (GetRValue(bg_color2) * y + GetRValue(bg_color1) * ry) / r.bottom;
          gv = (GetGValue(bg_color2) * y + GetGValue(bg_color1) * ry) / r.bottom;
          bv = (GetBValue(bg_color2) * y + GetBValue(bg_color1) * ry) / r.bottom;
		      brush = CreateSolidBrush(RGB(rv,gv,bv));
		      SetRect(&rect, 0 /*r.left*/, y, r.right, y+4);
		      // note that we don't need to do "SelectObject(hdc, brush)"
		      // because FillRect lets us specify the brush as a parameter.
		      FillRect(hdc, &rect, brush);
		      DeleteObject(brush);
          ry-=4;
          y+=4;
        }

        if (bg_textcolor != -1)
        {
          HFONT newFont, oldFont;
          newFont = CreateFont(40,0,0,0,FW_BOLD,TRUE,FALSE,FALSE,DEFAULT_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,"Garamond");
          if (newFont)
          {
            static char buf[256];
            r.left+=16;
            r.top+=8;
            my_GetWindowText(hwnd,buf,sizeof(buf));
            SetBkMode(hdc,TRANSPARENT);
            SetTextColor(hdc,bg_textcolor);
            oldFont = SelectObject(hdc,newFont);
            DrawText(hdc,buf,-1,&r,DT_TOP|DT_LEFT|DT_SINGLELINE|DT_NOPREFIX);
            SelectObject(hdc,oldFont);
            DeleteObject(newFont);
          }
        }
        EndPaint(hwnd,&ps);
      }
    return 0;
  }
  return DefWindowProc(hwnd,uMsg,wParam,lParam);
}

#endif //NSIS_SUPPORT_BGBG
