program VAppend;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  fs, fo: File;
  Patch: String;
  OutFile: String = 'VPATCH.EXE';
  Runtime: String = 'VPATCH.BIN';
  o: LongWord;
  Buf: Array[0..4095] of Byte;
  Size, BufSize: Integer;

begin
  WriteLn('VAppend v2.0');
  WriteLn('============');
  WriteLn;
  WriteLn('(c) 2001-2002 Van de Sande Productions');
  WriteLn('Website: http://www.tibed.net/vpatch');
  WriteLn('E-mail:  koen@tibed.net');
  WriteLn;
  if ParamCount = 0 then begin
    WriteLn('Use this program to append .PAT files to the VPatch runtime.');
    WriteLn;
    WriteLn('  VAPPEND (patch file) [output file] [runtime]');
    WriteLn;
    WriteLn('By default, the output file is VPATCH.EXE and the runtime is VPATCH.BIN');
  end;
  if not FileExists(ParamStr(1)) then begin
    WriteLn('ERROR: Patch file not found');
    Exit;
  end;
  Patch := ParamStr(1);
  if ParamCount > 1 then OutFile := ParamStr(2);
  if ParamCount > 2 then Runtime := ParamStr(3);
  WriteLn('Patch:   '+Patch);
  WriteLn('Runtime: '+Runtime);
  WriteLn('Output:  '+OutFile);

  AssignFile(fo,OutFile);
  Rewrite(fo,1);
  //copy the runtime
  AssignFile(fs,Runtime);
  FileMode:=fmOpenRead;
  Reset(fs,1);
  BufSize:=4096;
  o:=FileSize(fs);            //patch start offset
  Size:=FileSize(fs);
  while Size>0 do begin
    if Size-BufSize<0 then BufSize:=Size;
    BlockRead(fs,Buf,BufSize);
    BlockWrite(fo,Buf,BufSize);
    Dec(Size,BufSize);
  end;
  CloseFile(fs);
  //do the patch
  AssignFile(fs,Patch);
  FileMode:=fmOpenRead;
  Reset(fs,1);
  BufSize:=4096;
  Size:=FileSize(fs);
  while Size>0 do begin
    if Size-BufSize<0 then BufSize:=Size;
    BlockRead(fs,Buf,BufSize);
    BlockWrite(fo,Buf,BufSize);
    Dec(Size,BufSize);
  end;
  CloseFile(fs);

  BlockWrite(fo,o,SizeOf(o));
  CloseFile(fo);
  WriteLn('Created.');
end.
 