program GenPat2;

{
  VPatch 2 - Patch Generator
  ===============================

  (c) 2001-2003 Van de Sande Productions

  This is the main program unit for the commandline version. It implements
  commandline options (like /b=) and displays help if no options are given.

  What's new
  ----------
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
  d: Integer;
  S,Key: String;
  ShowDebug: Boolean;
  PG: TPatchGenerator;
  EV: TEventHandler;
begin
  EV:=TEventHandler.Create;
  PG:=TPatchGenerator.Create;
  PG.StartBlockSize:=64;

  WriteLn('GenPat v2.0 final');
  WriteLn('=================');
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
  for d:=1 to ParamCount do begin
    if CompareStr(LowerCase(Copy(ParamStr(d),1,3)),'/b=')=0 then begin
      PG.StartBlockSize:=StrToInt(Copy(ParamStr(d),4,10));
    end;
  end;

  if (CompareStr(ParamStr(1),'')=0) or (CompareStr(ParamStr(2),'')=0) or (CompareStr(ParamStr(3),'')=0) then begin
    WriteLn('This program will take (sourcefile) as input and create a (patchfile).');
    WriteLn('With this patchfile, you can convert a (sourcefile) into (targetfile).');
    WriteLn;
    WriteLn('Command line info:');
    WriteLn('  GENPAT (sourcefile) (targetfile) (patchfile)');
    WriteLn;
    WriteLn('Command line options (you do not need them):');
    WriteLn('/B=(BlockSize)             Set blocksize (def=64), multiple of 2');
    WriteLn('/WAIT                      Wait for a keypress after program complete');
    WriteLn('/DEBUG                     Show runtime debug information');
    WriteLn('Note: all these parameters must be *after* the filenames!');
    WriteLn;
    Write('Press a enter to exit ');
    ReadLn(S);
    Exit;
  end;

  if FileExists(ParamStr(3)) then begin
    WriteLn('Using existing file to include patches in: '+ParamStr(3));
    PG.LoadFromFile(ParamStr(3));
  end;

  T1:=Now;
  WriteLn('Patch body size: '+IntToStr(PG.CreatePatch(ParamStr(1),ParamStr(2))));
  PG.WriteToFile(ParamStr(3));

  T2:=Now;
  Write('GenPat.exe finished execution in: ');
  WriteLn(FloatToStr((T2-T1)*24*60*60),'s');
  WriteLn;
{$IFNDEF AUTOWAIT}
  if FindCmdLineSwitch('wait',['/'],True) then begin
{$ENDIF}
    WriteLn;
    WriteLn('Press a key');
    ReadLn(S);
{$IFNDEF AUTOWAIT}
  end;
{$ENDIF}
  PG.Free;
end.
