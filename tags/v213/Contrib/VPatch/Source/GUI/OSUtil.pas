unit OSUtil;

interface

procedure OpenLink(ALink: String); forward;
function GetTempDir: String; forward;
function GetTempFile: String; forward;
procedure ExecProgram(GameFolder: String; S: String; JustEXE: String; AWaitUntilFinish: Boolean); forward;
procedure ExecWaitBatchFile(AFolder: String; AName: String); forward;
procedure OSMoveFile(ASource,ADest: String); forward;

implementation

uses
  Windows, SysUtils, ShellAPI, Forms, Dialogs;

// does not support command-line arguments right now
procedure ExecWaitBatchFile(AFolder: String; AName: String);
var
  S, JustEXE: String;
  StartUp: STARTUPINFO;
  ProcInfo: PROCESS_INFORMATION;
begin
  S:='"'+AName+'"';
  JustExe:=AName;
  StartUp.lpReserved:=nil;
  StartUp.lpDesktop:=nil;
  StartUp.lpTitle:=nil;
  StartUp.dwFlags:=STARTF_USESHOWWINDOW;
  StartUp.wShowWindow:=SW_SHOWMAXIMIZED;
  StartUp.cbReserved2:=0;
  StartUp.lpReserved2:=nil;

  //it works now... but don't ask me how - it's taken me hours just to get this working :)
  if not CreateProcess(PChar(JustEXE),PChar(S),nil,nil,False,NORMAL_PRIORITY_CLASS,nil,PChar(AFolder),StartUp,ProcInfo) then
    raise Exception.Create('CreateProcess error:'+IntToStr(GetLastError));
  if not (ProcInfo.hThread=0) then CloseHandle(ProcInfo.hThread);
  if not (ProcInfo.hProcess=0) then begin
    WaitForSingleObject(ProcInfo.hProcess,INFINITE);
    CloseHandle(ProcInfo.hProcess);
  end;
end;


procedure OSMoveFile(ASource,ADest: String);
begin
  if not MoveFileEx(@ASource[1],@ADest[1],MOVEFILE_COPY_ALLOWED+MOVEFILE_REPLACE_EXISTING+MOVEFILE_WRITE_THROUGH) then
    raise Exception.Create('File movement failed in OSMoveFile: '+ASource+'; '+ADest);
end;

procedure OpenLink(ALink: String);
var
  StartDoc: Integer;
begin
  StartDoc:=ShellExecute(Application.Handle, 'open', PChar(ALink), nil, nil, SW_SHOWNORMAL);
  If StartDoc <= 32 Then begin
    MessageDlg('Error occured while opening link: '+ALink,mtWarning,[mbOK],0);
  end;
end;

procedure ExecProgram(GameFolder: String; S: String; JustEXE: String; AWaitUntilFinish: Boolean);
var
  StartUp: STARTUPINFO;
  ProcInfo: PROCESS_INFORMATION;
begin
  StartUp.lpReserved:=nil;
  StartUp.lpDesktop:=nil;
  StartUp.lpTitle:=nil;
  StartUp.dwFlags:=STARTF_USESHOWWINDOW;
  StartUp.wShowWindow:=SW_SHOWMAXIMIZED;
  StartUp.cbReserved2:=0;
  StartUp.lpReserved2:=nil;

  //it works now... but don't ask me how - it's taken me hours just to get this working :)
  if not CreateProcess(PChar(JustEXE),PChar(S),nil,nil,False,NORMAL_PRIORITY_CLASS,nil,PChar(GameFolder),StartUp,ProcInfo) then
    raise Exception.Create('CreateProcess error:'+IntToStr(GetLastError));
  if not (ProcInfo.hThread=0) then CloseHandle(ProcInfo.hThread);
  if not (ProcInfo.hProcess=0) then begin
    if AWaitUntilFinish then
      WaitForSingleObject(ProcInfo.hProcess,INFINITE);
    CloseHandle(ProcInfo.hProcess);
  end;
end;

var
  TempDir: String;
  strBuffer: Array[0..1024] of Char;
  nBufferLength: Cardinal;

  function GetTempFile;
  var
    Prefix: String;
    lngLength: Integer;
  begin
    PreFix:='TMP';
    strBuffer:='';
    lngLength:=GetTempFileName(PChar(TempDir),PChar(PreFix),0,strBuffer);
    if not (lngLength=0) then begin
      GetTempFile:=strBuffer;
    end;
  end;

function GetTempDir: String;
begin
  GetTempDir:=TempDir;
end;

begin
  // get temporary folder
  nBufferLength:=1024;
  strBuffer:='';
  GetTempPath(nBufferLength,strBuffer);
  TempDir:=strBuffer;
end.
