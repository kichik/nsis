{
    Original Code from
    (C) 2001 - Peter Windridge

    Code in seperate unit and some changes
    2003 by Bernhard Mayer

    simple include this unit in your plugin project and export
    functions as needed
}


unit nsis;

interface

uses
  windows;

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
    INST_LANG,    // $LANGUAGE
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

procedure Init(hwndParent: HWND; string_size: integer; variables: PChar; stacktop: pointer);
function PopString(str: PChar):integer;
function PushString(str: PChar):integer;
function GetUserVariable(varnum: TVariableList):PChar;
function SetUserVariable(varnum: TVariableList; value: PChar):integer;

implementation

procedure Init(hwndParent: HWND; string_size: integer; variables: PChar; stacktop: pointer);
begin
  g_stringsize:=string_size;
  g_hwndParent:=hwndParent;
  g_stacktop:=stacktop;
  g_variables:=variables;
end;

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

procedure SetUserVariable(varnum: TVariableList; value: PChar);
begin
  if (value <> nil) and (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    begin
      lstrcpy(g_variables+integer(varnum)*g_stringsize,value);
    end;
end;

begin
end.
