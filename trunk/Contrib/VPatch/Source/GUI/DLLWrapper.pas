unit DLLWrapper;

interface

uses Classes, SysUtils;

  function DoGenerate(const Source, Target: String; Stream: TStream; Config: String): Integer; forward;

implementation

uses PatchGenerator;

function DoGenerate(const Source, Target: String; Stream: TStream; Config: String): Integer;
var
  PG: TPatchGenerator;
  a: Integer;
begin
  WriteLn('Generating '+ExtractFileName(Source)+' to '+ExtractFileName(Target)+'...');

  PG:=TPatchGenerator.Create;
  PG.StartBlockSize:=512;
  PG.MinimumBlockSize:=512;
  PG.BlockDivider:=2;
  PG.StepSize:=256;
  try
    a:=Pos(',',Config);
    if(a=0) then a:=Length(Config)+1;
    PG.StartBlockSize:=StrToInt(Copy(Config,1,a-1));
    Config:=Copy(Config,a+1,Length(Config));

    a:=Pos(',',Config);
    if(a=0) then a:=Length(Config)+1;
    PG.MinimumBlockSize:=StrToInt(Copy(Config,1,a-1));
    Config:=Copy(Config,a+1,Length(Config));

    a:=Pos(',',Config);
    if(a=0) then a:=Length(Config)+1;
    PG.BlockDivider:=StrToInt(Copy(Config,1,a-1));
    Config:=Copy(Config,a+1,Length(Config));

    a:=Pos(',',Config);
    if(a=0) then a:=Length(Config)+1;
    PG.StepSize:=StrToInt(Copy(Config,1,a-1));
  finally
  end;

  Result:=PG.CreatePatch(Source,Target);
  PG.WriteToStream(Stream);
  PG.Free;
  WriteLn(ExtractFileName(Source)+' -> '+ExtractFileName(Target)+': '+IntToStr(Result)+' bytes');
end;

end.
