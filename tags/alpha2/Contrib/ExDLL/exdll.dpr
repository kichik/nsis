{
  NSIS ExDLL example
  (C) 2001 - Peter Windridge

  Fixed and formatted by Alexander Tereschenko
  http://futuris.plastiqueweb.com/

  Tested in Delphi 6.01
}

library exdll;

uses Windows;

type
  VarConstants = (
    INST_0,
    INST_1,       // $1
    INST_2,       // $2
    INST_3,       // $3
    INST_4,       // $4
    INST_5,       // $5
    INST_6,       // $6
    INST_7,       // $7
    INST_8,       // $8
    INST_9,       // $9
    INST_R0,      // $R0
    INST_R1,      // $R1
    INST_R2,      // $R2
    INST_R3,      // $R3
    INST_R4,      // $R4
    INST_R5,      // $R5
    INST_R6,      // $R6
    INST_R7,      // $R7
    INST_R8,      // $R8
    INST_R9,      // $R9
    INST_CMDLINE, // $CMDLINE
    INST_INSTDIR, // $INSTDIR
    INST_OUTDIR,  // $OUTDIR
    INST_EXEDIR,  // $EXEDIR
    __INST_LAST
    );
  TVariableList = INST_0..__INST_LAST;
  pstack_t = ^stack_t;
  stack_t = record
    next: pstack_t;
    text: PChar;
  end;

var
  g_stringsize: integer;
  g_stacktop: ^pstack_t;
  g_variables: PChar;
  g_hwndParent: HWND;

function PopString(str: PChar):integer;
var
  th: pstack_t;
begin
  if integer(g_stacktop^) = 0 then
    begin
    Result:=1;
    Exit;
    end;
  th:=g_stacktop^;
  lstrcpy(str,@th.text);
  g_stacktop^ := th.next;
  GlobalFree(HGLOBAL(th));
  Result:=0;
end;

function PushString(str: PChar):integer;
var
  th: pstack_t;
begin
  if integer(g_stacktop) = 0 then
    begin
    Result:=1;
    Exit;
    end;
  th:=pstack_t(GlobalAlloc(GPTR,sizeof(stack_t)+g_stringsize));
  lstrcpyn(@th.text,str,g_stringsize);
  th.next:=g_stacktop^;
  g_stacktop^:=th;
  Result:=0;
end;

function GetUserVariable(varnum: TVariableList):PChar;
begin
  if (integer(varnum) < 0) or (integer(varnum) >= integer(__INST_LAST)) then
    begin
    Result:='';
    Exit;
    end;
  Result:=g_variables+integer(varnum)*g_stringsize;
end;

function ex_dll(hwndParent: HWND; string_size: integer; variables: PChar; stacktop: pointer):integer; cdecl;
var
  c: PChar;
  buf: array[0..1024] of char;
begin
  // set up global variables
  g_stringsize:=string_size;
  g_hwndParent:=hwndParent;
  g_stringsize:=string_size;
  g_stacktop:=stacktop;
  g_variables:=variables;

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
