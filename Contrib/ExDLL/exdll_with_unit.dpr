{
  NSIS ExDLL2 example
  Original is ExDLL
  (C) 2001 - Peter Windridge

  Changed with delphi unit nsis.pas
  by bernhard mayer

  Tested in Delphi 7.0
}

library exdll;

uses
  nsis, windows;

function ex_dll(hwndParent: HWND; string_size: integer; variables: PChar; stacktop: pointer):integer; cdecl;
var
  c: PChar;
  buf: array[0..1024] of char;
begin
  // set up global variables
  Init(hwndParent,string_size,variables,stacktop);

  c:=GetUserVariable(INST_0);
  MessageBox(g_hwndParent,c,'The value of $0',MB_OK);
  PopString(@buf);
  MessageBox(g_hwndParent,@buf,'pop',MB_OK);
  PushString(PChar('Hello, this is a push'));

  Result:=1;
end;

exports ex_dll;

begin
end.
