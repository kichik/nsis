#include <windows.h>
#include "resource.h"
#include "config.h"
#include "fileform.h"
#include "state.h"
#include "ui.h"
#include "util.h"

#ifdef NSIS_SUPPORT_BGBG

#define c1 header->bg_color1
#define c2 header->bg_color2

LRESULT CALLBACK BG_WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
    case WM_WINDOWPOSCHANGING:
      {
        LPWINDOWPOS wp = (LPWINDOWPOS) lParam;
        wp->flags |= SWP_NOACTIVATE;
        wp->hwndInsertAfter = g_hwnd;
        break;
      }
    case WM_PAINT:
      {
        header *header = g_header;

        PAINTSTRUCT ps;
        HDC hdc=BeginPaint(hwnd,&ps);
        RECT r;
        LOGBRUSH lh;
        int ry;

        lh.lbStyle = BS_SOLID;

        GetClientRect(hwnd,&r);
        // this portion by Drew Davidson, drewdavidson@mindspring.com
        ry=r.bottom;

        // JF: made slower, reduced to 4 pixels high, because I like how it looks better/
        while (r.top < ry)
        {
          int rv,gv,bv;
          HBRUSH brush;
          rv = (GetRValue(c2) * r.top + GetRValue(c1) * (ry-r.top)) / ry;
          gv = (GetGValue(c2) * r.top + GetGValue(c1) * (ry-r.top)) / ry;
          bv = (GetBValue(c2) * r.top + GetBValue(c1) * (ry-r.top)) / ry;
          lh.lbColor = RGB(rv,gv,bv);
          brush = CreateBrushIndirect(&lh);
          // note that we don't need to do "SelectObject(hdc, brush)"
          // because FillRect lets us specify the brush as a parameter.
          FillRect(hdc, &r, brush);
          DeleteObject(brush);
          r.top+=4;
          r.bottom+=4;
        }

        if (header->bg_textcolor != -1)
        {
          HFONT oldFont;
          HFONT newFont = CreateFont(
            40,
            0,
            0,
            0,
            FW_BOLD,
            TRUE,
            FALSE,
            FALSE,
            DEFAULT_CHARSET,
            OUT_DEFAULT_PRECIS,
            CLIP_DEFAULT_PRECIS,
            DEFAULT_QUALITY,
            DEFAULT_PITCH,
            "Garamond"
          );
          if (newFont)
          {
            r.left=16;
            r.top=8;
            SetBkMode(hdc,TRANSPARENT);
            SetTextColor(hdc,header->bg_textcolor);
            oldFont = SelectObject(hdc,newFont);
            DrawText(hdc,g_caption,-1,&r,DT_TOP|DT_LEFT|DT_SINGLELINE|DT_NOPREFIX);
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
