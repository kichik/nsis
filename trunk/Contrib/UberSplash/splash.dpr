{****************************************}
Program splash;

uses
  Windows,
  Kol;

type
        PByteArray = ^TByteArray;
        TByteArray = array[0..32767] of Byte;
var
  Applet,                     {A main form or application}
  PaintBox : pControl;             {A simple static paintbox}
  BitMap   : pBitmap;              {A bitmap}
  i,j,j2,wait,inSpd,outSpd   : Word;    {temp values...}
  WindowRgn, RowRgn  : HRGN;       {Regions for transparency}
  FadeIn,FadeOut     : pTimer;     {Timers...}
  FadeInFlag         : Boolean;    {Switch Fade In/Out Flag}
  ScannedLine        : PByteArray; {Array for RGB data from BitMap}

 //This handles the OnPaint event from the Paintbox It draws the bitmap on the PaintBox's Canvas
 procedure paint(dummy:pointer;sender:pcontrol;DC:HDC);
 begin
   Bitmap.Draw(DC,0,0);
 end;

 procedure FadeInAlpha(Sender: PObj);
 begin
  if FadeInFlag then
        if Applet.AlphaBlend < (255 - inSpd) then Applet.AlphaBlend := Applet.AlphaBlend + inSpd;
  if not FadeInFlag then begin
        if Applet.AlphaBlend > outSpd then Applet.AlphaBlend := Applet.AlphaBlend - outSpd;
        if Applet.AlphaBlend < outSpd + 1 then begin
                Applet.AlphaBlend := 0;
                Applet.Close;
        end;
  end;
 end;

 procedure FadeOutAlpha(Sender: PObj);
 begin
        if Applet.AlphaBlend > inSpd then
                FadeInFlag := False
        else begin
                Applet.AlphaBlend := 0;
                Applet.Close;
        end;
 end;

begin
   if ParamStr(1) <> '' then begin
        if ParamStr(2) = '' then inSpd := 5 else inSpd := Str2Int(ParamStr(2));
        if ParamStr(3) = '' then wait := 2000 else wait := Str2Int(ParamStr(3));
        if ParamStr(4) = '' then outSpd := 25 else outSpd := Str2Int(ParamStr(4));
   end
   else begin
        if ParamStr(2) = '' then inSpd := 1 else inSpd := Str2Int(ParamStr(2));
        if ParamStr(3) = '' then wait := 1 else wait := Str2Int(ParamStr(3));
        if ParamStr(4) = '' then outSpd := 1 else outSpd := Str2Int(ParamStr(4));
   end;

  {Create the form}
  Applet:=NewForm(nil,'').SetSize(0,0);
  Applet.CenterOnParent;
  Applet.Visible := False;
  Applet.AlphaBlend := 0;
  Applet.Style := WS_POPUP;
  Applet.HasBorder := False;
  Applet.Border := 0;
  Applet.Color := clBlack;


  {Create the bitmap itself}
  BitMap:=NewBitMap(0,0);
  BitMap.Clear;
  //LoadFromFile has built in error checking...
  if ParamStr(1) <> '' then BitMap.LoadFromFile(ParamStr(1));

  {Create the Paintbox to draw the bitmap on}
  PaintBox:=NewPaintbox(Applet).setalign(caClient);
  PaintBox.Width:=BitMap.Width;
  PaintBox.Height:=BitMap.Height;
  PaintBox.OnPaint:=TOnPaint(MakeMethod(nil,@Paint));

  {Do some housekeeping}
  Applet.ClientHeight:=Bitmap.Height+10;
  Applet.ClientWidth :=Bitmap.Width+10;
  Applet.Top := Applet.Top - Applet.Height div 2;
  Applet.Left := Applet.Left - Applet.Width div 2;


  {Cut out magic color}
  WindowRgn := CreateRectRgn(0,0,0,0);
  for i:= 0 to BitMap.Height - 1 do begin
        ScannedLine := BitMap.ScanLine[i];
        j := 0;
        while (j < BitMap.Width - 1) do begin
                if ((ScannedLine[j * 3 + 2]*256*256)+(ScannedLine[j * 3 + 1]*256)+ScannedLine[j * 3] = clLime) then j := j + 1
                else begin
                        j2 := j;
                        while (j2 < BitMap.Width - 1) and ((ScannedLine[j2 * 3 + 2]*256*256)+(ScannedLine[j2 * 3 + 1]*256)+ScannedLine[j2 * 3] <> clLime) do j2 := j2 + 1;
                        RowRgn := CreateRectRgn(j,i,j2,i+1);
                        CombineRgn(WindowRgn, WindowRgn, RowRgn, RGN_OR);
                        DeleteObject(RowRgn);
                        j := j2;
                end;
        end;
  end;
  SetWindowRgn(Applet.Handle,WindowRgn,true);

  //remove taskbar icon and get ready to rock...
  ShowWindow(Applet.Handle, SW_HIDE);
  SetWindowLong(Applet.Handle, GWL_EXSTYLE,GetWindowLong(Applet.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  ShowWindow(Applet.Handle, SW_SHOW);
  ShowWindow(Applet.Handle, SW_SHOWDEFAULT);
  BringWindowToTop(Applet.GetWindowHandle);
  Applet.DoSetFocus;
  Applet.StayOnTop := True;
  Applet.Visible := True;

  {start timers}
  FadeInFlag := True;

  FadeIn := NewTimer(2);
  FadeIn.OnTimer := TOnEvent(MakeMethod(nil,@FadeInAlpha));
  FadeIn.Enabled := True;

  FadeOut := NewTimer(wait);
  FadeOut.OnTimer := TOnEvent(MakeMethod(nil,@FadeOutAlpha));
  FadeOut.Enabled := True;

  {Run splash}
  Run(Applet);

  {Free the bitmap:it has no parent}
  Bitmap.free;

end.
