program GenPat2;

{
  VPatch 2 - Patch Generator
  ===============================

  (c) 2001-2003 Van de Sande Productions

  This is the main program unit for the commandline version. It implements
  commandline options (like /b=) and displays help if no options are given.

  What's new
  ----------
  2.1   20031219    Koen            Added error checking, handling, shouldn't
                                    crash when invalid arguments, returns
                                    exit codes now.
  2.0   20030811    Koen            Initial documentation
}

{$APPTYPE CONSOLE}
uses
  PatchGenerator in 'PatchGenerator.pas',
  VDSP_CRC in 'VDSP_CRC.pas',
  Sysutils,
  TreeCode in 'TreeCode.pas';

type
  TEventHandler = class
    procedure PrintDebug(S: String);
  end;

    procedure TEventhandler.PrintDebug(S: String);
    begin
      WriteLn(S);
    end;

{$DEFINE READCONFIG}          //try to read genpat.ini?
{.$DEFINE AUTOWAIT}           //have /wait command line switch on by default?
                              //useful when debugging

var
  Config: TextFile;
  T1,T2: TDateTime;
  d, i: Integer;
  S,Key: String;
  SourceFile, TargetFile, PatchFile: String;
  ShowDebug, ShowHelp: Boolean;
  PG: TPatchGenerator;
  EV: TEventHandler;

begin
  EV:=TEventHandler.Create;
  PG:=TPatchGenerator.Create;
  PG.StartBlockSize:=64;

  WriteLn('GenPat v2.1');
  WriteLn('===========');
  WriteLn;
  WriteLn('(c) 2001-2003 Van de Sande Productions');
  WriteLn('Website: http://www.tibed.net/vpatch');
  WriteLn('E-mail:  koen@tibed.net');
  WriteLn;
  ShowDebug:=FindCmdLineSwitch('debug',['/'],True);
  if ShowDebug then
    DebugEvent:=EV.PrintDebug;

{$IFDEF READCONFIG}
  if FileExists('genpat.ini') then begin
    AssignFile(Config,'genpat.ini');
    Reset(Config);
    while not eof(Config) do begin
      ReadLn(Config,S);
      d:=Pos('=',S);
      if not (d=0) then begin
        Key:=LowerCase(Copy(S,1,d-1));
        S:=Copy(S,d+1,Length(S));
        if CompareStr(Key,'startblocksize')=0 then PG.StartBlockSize:=StrToInt(S);
      end;
    end;
    CloseFile(Config);
  end;
{$ENDIF}
  i:=0;
  for d:=1 to ParamCount do begin
    if CompareStr(LowerCase(Copy(ParamStr(d),1,3)),'/b=')=0 then begin
      PG.StartBlockSize:=StrToInt(Copy(ParamStr(d),4,10));
    end else begin
      // not a parameter?
      if not (ParamStr(d)[1] = '/') then begin
        if i = 2 then begin
          PatchFile:=ParamStr(d);
          Inc(i);
        end;
        if i = 1 then begin
          TargetFile:=ParamStr(d);
          Inc(i);
        end;
        if i = 0 then begin
          SourceFile:=ParamStr(d);
          Inc(i);
        end;
      end;
    end;
  end;

  ShowHelp:=False;
  if(CompareStr(PatchFile,'')=0) then ShowHelp:=True;
  if SourceFile = '' then ShowHelp:=True;
  if TargetFile = '' then ShowHelp:=True;

  if ShowHelp then begin
    WriteLn('This program will take (sourcefile) as input and create a (patchfile).');
    WriteLn('With this patchfile, you can convert a (sourcefile) into (targetfile).');
    WriteLn;
    WriteLn('Command line info:');
    WriteLn('  GENPAT (sourcefile) (targetfile) (patchfile)');
    WriteLn;
    WriteLn('Command line options (you do not need them):');
    WriteLn('/B=(BlockSize)             Set blocksize (def=64), multiple of 2');
    WriteLn('/NOEQUALERROR              Exit code becomes 0 instead of 10 when');
    WriteLn('                           two files with equal CRC are encountered');
    WriteLn('                           (patch file will remain unchanged)');
    WriteLn('/DEBUG                     Show runtime debug information');
    WriteLn;
    WriteLn('Note: filenames should never start with / character!');
    WriteLn;
    WriteLn('Possible exit codes:');
    WriteLn('  0  Success');
    WriteLn('  1  Arguments missing');
    WriteLn('  2  Source file not found');
    WriteLn('  3  Target file not found');
    WriteLn('  4  Unknown error while reading existing patch file');
    WriteLn('  5  Unknown error while generating patch');
    WriteLn('  6  Unknown error while writing patch file to disk');
    WriteLn('  10 CRC of source and target file are equal (impossible with /NOEQUALERROR)');
    WriteLn('  11 Not enough memory for source file');
    WriteLn('  12 Not enough memory for target file');
    PG.Free;
    ExitCode:=1;
    Exit;
  end;

  // stop if file error, result shown above
  if not FileExists(SourceFile) then begin
    WriteLn('Error: Source file not found');
    PG.Free;
    ExitCode:=2;
    Exit;
  end;
  if not FileExists(TargetFile) then begin
    WriteLn('Error: Target file not found');
    PG.Free;
    ExitCode:=3;
    Exit;
  end;

  if FileExists(PatchFile) then begin
    WriteLn('Using existing file to include patches in: '+PatchFile);
    try
      PG.LoadFromFile(PatchFile);
    except
      on E: Exception do begin
        WriteLn('Error: Reading existing patch file failed');
        WriteLn('Error message: ', E.ClassName, ': ', E.Message);
        PG.Free;
        ExitCode:=4;
        Exit;
      end;
    end;
  end;

  WriteLn('Source (original) file: ', SourceFile);
  WriteLn('Target (newer) file:    ', TargetFile);

  T1:=Now;

  // create patch file, with error handling
  try
    i:=PG.CreatePatch(SourceFile,TargetFile);
  except
    on E: Exception do begin
      WriteLn('Error: Generating patch failed');
      WriteLn('Error message: ', E.ClassName, ': ', E.Message);
      PG.Free;
      ExitCode:=5;
      Exit;
    end;
  end;

  if(i < 0) then begin
    if(i = -1) then begin
      if not FindCmdLineSwitch('noequalerror',['/'],True) then
        WriteLn('Error: CRC of source and target file are equal');
    end;
    if(i = -2) then WriteLn('Error: Not enough memory for source file');
    if(i = -3) then WriteLn('Error: Not enough memory for target file');
    ExitCode:=9 - i;

    if(i = -1) and (FindCmdLineSwitch('noequalerror',['/'],True)) then begin
      WriteLn('Equal CRCs ignored (no patch will be written and exit code is 0)');
      ExitCode:=0;
    end;
  end else begin
    WriteLn('Patch body size: '+IntToStr(i));
    try
      PG.WriteToFile(PatchFile);
    except
      on E: Exception do begin
        WriteLn('Error: Writing patch to file ' + PatchFile + ' failed');
        WriteLn('Error message: ', E.ClassName, ': ', E.Message);
        PG.Free;
        ExitCode:=6;
        Exit;
      end;
    end;

    T2:=Now;
    Write('Time taken for generation: ');
    WriteLn(FloatToStr((T2-T1)*24*60*60),'s');
    WriteLn;

    ExitCode:=0;
  end;

  PG.Free;
end.
