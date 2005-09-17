unit DLLWrapper;

interface

uses Classes, SysUtils;

  function DoGenerate(const Source, Target: String; Stream: TStream; Config: String): Integer; forward;

var
  WaitAfterGenerate: Boolean = False;
  OptimalPatches: Boolean = False;

implementation

uses
  OSUtil, Forms;

function DoGenerate(const Source, Target: String; Stream: TStream; Config: String): Integer;
var
  F: TextFile;
  Temp, BatchFile: String;
  fs: TFileStream;
begin
  BatchFile:=ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + PathDelim +'~generate_patch.bat';
  AssignFile(F,BatchFile);
  Rewrite(F);
  Temp:=GetTempFile;
  WriteLn(F,'@cls');
  WriteLn(F,'@echo Generating '+ExtractFileName(Source)+' to '+ExtractFileName(Target)+'...');
  Write(F,'genpat.exe "', Source, '" "', Target, '" "', Temp, '" /b='+Config);
  if OptimalPatches then begin
    Write(F,' /o');
  end;
  WriteLn(F,'');
  if WaitAfterGenerate then begin
    WriteLn(F,'@echo.');
    WriteLn(F,'@pause');
  end;
  CloseFile(F);

  ExecWaitBatchFile(ExtractFilePath(BatchFile),BatchFile);

  fs:=TFileStream.Create(Temp,fmOpenRead);
  Stream.CopyFrom(fs,fs.Size);
  Result:=fs.Size;
  fs.Free;
  DeleteFile(Temp);
  DeleteFile(BatchFile);
end;

end.
