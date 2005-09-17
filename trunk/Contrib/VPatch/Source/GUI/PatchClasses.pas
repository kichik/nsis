unit PatchClasses;

interface

uses Classes, sysutils, VDSP_CRC, DLLWrapper, Dialogs;

const
  DEFAULT_CONFIG = '64';

type
  TAbstractFile = record
    FileName: String;
    FriendlyName: String;
    CRC32: LongWord;  //the longword/integer sign is going to give problems again...
    Size: Integer;
    //not sure about this one yet...
    Cached: Boolean;  //True: we have cached the patch, using latest config
                      //False: a) we have nothing cached (size and start are -1)
                      //       b) we still have cache (start>0 and size too), but it's not generated using the latest config (we can keep it of course because the new config might be worse)   
    Cache: TMemoryStream;
  end;

  TPatchFile = class (TObject)
  private
    FIndex: Integer;
    ConfigID: String;
    FNew: TAbstractFile;
    FOld: Array of TAbstractFile;
  protected
    procedure SetNewFN(Value: String);
    function GetNewFN: String;
    procedure SetOldFN(i: Integer; FileName: String);
    function GetOldFN(Index: Integer): String;
    function GetOldVersionCount: Integer;
    procedure ResetCache; overload;
    procedure ResetCache(OldIndex: Integer); overload;
    procedure InvalidateCache; overload;
    procedure InvalidateCache(Index: Integer); overload;
    function GetCached(Index: Integer): Boolean;
    function GetConfig: String;
    procedure SetConfig(Value: String);
  public
    constructor Create(Index: Integer; FileName: String); overload;
    constructor Create(Index: Integer; Stream: TStream); overload;
    destructor Destroy(); override;

    procedure AddOldVersion(const FileName: String);
    procedure RemoveOldVersion(const Index: Integer);
    property OldVersions[Index: Integer]: String read GetOldFN write SetOldFN;

    procedure Generate; overload;
    procedure Generate(const Index: Integer); overload;
    property Generated[Index: Integer]: Boolean read GetCached;
    function GetPatchSize(Index: Integer): Integer;

    procedure WritePatch(Index: Integer; Stream: TStream);

//  LoadFromStream not supported: Use Create(Index,Stream) instead!
//    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property NewVersion: String read GetNewFN write SetNewFN;
    property OldVersionCount: Integer read GetOldVersionCount;
    property Index: Integer read FIndex;
    property Config: String read GetConfig write SetConfig;
  end;

  TPatchProject = class (TObject)
  private
    FPat: Array of TPatchFile;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    constructor Create();
    destructor Destroy(); override;
    procedure AddNewVersion(FileName: String);
    function PatchFile(FileName: String): TPatchFile; overload;
    function PatchFile(Index: Integer): TPatchFile; overload;
    function GetPatchCount: Integer;
    procedure WritePatches(Stream: TStream);
    procedure Generate;
    procedure ResetCache;
  end;

implementation

  function ReadStreamString(Stream: TStream): String;
  var
    Buf: Array[0..512] of Char;
    i: LongInt;
    S: String;
    j: Integer;
  begin
    Stream.Read(i,SizeOf(i));
    if i>512 then raise Exception.Create('VPJ damaged: String too long (>512)');
    Stream.Read(Buf,i);
    for j:=1 to i do
      S:=S+Buf[j-1];
    ReadStreamString:=S;
  end;

//a private wrapper for the FileCRC function
function CalcCRC(FileName: String): Integer;
var
  fs: TFileStream;
begin
  CalcCRC:=0;
  fs:=nil;
  try
    fs:=TFileStream.Create(FileName,fmOpenRead);
    CalcCRC:=FileCRC(fs);
  finally
    fs.Free;
  end;
end;

function GetFileSize(FileName: String): Integer;
var
  fs: TFileStream;
begin
  GetFileSize:=0;
  fs:=nil;
  try
    fs:=TFileStream.Create(FileName,fmOpenRead);
    GetFileSize:=fs.Size;
  finally
    fs.Free;
  end;
end;

{ TPatchFile }

procedure TPatchFile.AddOldVersion(const FileName: String);
var
  i: Integer;
//  fs: TFileStream;
begin
  i:=Length(FOld);
  SetLength(FOld,i+1);
  FOld[i].Cache:=TMemoryStream.Create;
  SetOldFN(i,FileName);
end;

constructor TPatchFile.Create(Index: Integer; FileName: String);
//var
//  fs: TFileStream;
begin
  inherited Create();
  FIndex:=Index;
  SetLength(FOld,0);
  FNew.CRC32:=0;
  FNew.Size:=-1;
  SetNewFN(FileName);
  ConfigID:=DEFAULT_CONFIG;
   //just to be on the safe side
  //following is now done by SetNewFN :)
  //no it's not - because that one resets the cache!!!
  //doesn't matter, because we're not loading from stream!!!
{  FNew.FileName:=FileName;
  FNew.FriendlyName:=ExtractFileName(FileName);
  FNew.CRC32:=CalcCRC(FileName);
  FNew.Size:=GetFileSize(FileName);}
end;

constructor TPatchFile.Create(Index: Integer; Stream: TStream);
var
  i,q: LongInt;
  CSize: Integer;
  j: Integer;
begin
  inherited Create();
  FIndex:=Index;
  SetLength(FOld,0);
  FNew.CRC32:=0;
  FNew.Size:=-1; //just to be on the safe side

  //read configuration
  ConfigID:=ReadStreamString(Stream);

  //now load everything...
  FNew.FileName:=ReadStreamString(Stream);
  FNew.FriendlyName:=ReadStreamString(Stream);
  Stream.Read(FNew.CRC32,SizeOf(FNew.CRC32));
  Stream.Read(FNew.Size,SizeOf(FNew.Size));
  Stream.Read(i,SizeOf(i));
  SetLength(FOld,i);
  for j:=0 to i - 1 do begin
    FOld[j].FileName:=ReadStreamString(Stream);
    FOld[j].FriendlyName:=ReadStreamString(Stream);
    Stream.Read(FOld[j].CRC32,SizeOf(FOld[j].CRC32));
    Stream.Read(FOld[j].Size,SizeOf(FOld[j].Size));
    Stream.Read(q,SizeOf(q));
    FOld[j].Cached:=not (q=0);
    if FOld[j].Cached then begin
      Stream.Read(CSize,SizeOf(CSize));
      FOld[j].Cache:=TMemoryStream.Create;
      FOld[j].Cache.CopyFrom(Stream,CSize);
    end;
  end;
end;

destructor TPatchFile.Destroy;
begin
  SetLength(FOld,0);
  inherited;
end;

function TPatchFile.GetNewFN: String;
begin
  GetNewFN:=FNew.FileName;
end;

function TPatchFile.GetOldFN(Index: Integer): String;
begin
  Result:=FOld[Index].FileName;
  if FOld[Index].Cached then
    if FOld[Index].Cache.Size>0 then begin
      Result:=Result + ' ('+IntToStr(FOld[Index].Cache.Size)+' bytes to patch)';
    end;
end;

function TPatchFile.GetOldVersionCount: Integer;
begin
  GetOldVersionCount:=Length(FOld);
end;

procedure TPatchFile.ResetCache;
var
  i: Integer;
begin
  for i:=0 to Length(FOld)-1 do
    ResetCache(i);
end;

procedure TPatchFile.RemoveOldVersion(const Index: Integer);
var
  i: Integer;
begin
  FOld[Index].Cache.Free;
  for i:=Index to Length(FOld)-2 do begin
    FOld[i]:=FOld[i+1];
  end;
  SetLength(FOld,Length(FOld)-1);
end;

procedure TPatchFile.ResetCache(OldIndex: Integer);
begin
  FOld[OldIndex].Cached:=False;
  FOld[OldIndex].Size:=-1;
  FOld[OldIndex].Cache.Clear;
end;

procedure TPatchFile.SaveToStream(Stream: TStream);
  procedure WriteStreamString(Stream: TStream; const S: String);
  var
    i: LongInt;
    j: Integer;
    Buf: Array[0..512] of Char;
  begin
    i:=Length(S);
    Stream.Write(i,SizeOf(i));
    for j:=1 to i do
      Buf[j-1]:=S[j];
    Buf[i]:=#0;
    Stream.Write(Buf,i);
  end;
var
  i,q: LongInt;
  j: Integer;
  tmp: Integer;
begin
  //write config ID
  WriteStreamString(Stream,ConfigID);

  WriteStreamString(Stream,FNew.FileName);
  WriteStreamString(Stream,FNew.FriendlyName);
  Stream.Write(FNew.CRC32,SizeOf(FNew.CRC32));
  Stream.Write(FNew.Size,SizeOf(FNew.Size));

  i:=Length(FOld);
  Stream.Write(i,SizeOf(i));

  for j:=0 to i - 1 do begin
    WriteStreamString(Stream,FOld[j].FileName);
    WriteStreamString(Stream,FOld[j].FriendlyName);
    Stream.Write(FOld[j].CRC32,SizeOf(FOld[j].CRC32));
    Stream.Write(FOld[j].Size,SizeOf(FOld[j].Size));
    if FOld[j].Cached then q:=1 else q:=0;
    Stream.Write(q,SizeOf(q));
    if FOld[j].Cached then begin
      tmp:=FOld[j].Cache.Size;
      Stream.Write(tmp,SizeOf(tmp));
      FOld[j].Cache.Seek(0,soFromBeginning);
      Stream.CopyFrom(FOld[j].Cache,tmp);
    end;
  end;
end;

procedure TPatchFile.SetNewFN(Value: String);
var
  NewSize: Integer;
  NewCRC: LongWord;
begin
  FNew.FileName:=Value;
  FNew.Friendlyname:=ExtractFileName(Value);
  NewCRC:=CalcCRC(Value);
  NewSize:=GetFileSize(Value);
  //if any changes, then reset cache :)
  if not ((FNew.CRC32=NewCRC) and (FNew.Size=NewSize)) then begin
    FNew.CRC32:=NewCRC;
    FNew.Size:=NewSize;
    ResetCache;
  end;
end;

procedure TPatchFile.SetOldFN(i: Integer; FileName: String);
begin
  if((i>=0) and (i<Length(FOld))) then begin
    FOld[i].FileName:=FileName;
    FOld[i].FriendlyName:=ExtractFileName(FileName);
    FOld[i].CRC32:=CalcCRC(FileName);
    FOld[i].Size:=GetFileSize(FileName);
    ResetCache(i);
  end;
end;

procedure TPatchFile.Generate;
var
  i: Integer;
begin
  //generate all of them into the cache?
  for i:=0 to OldVersionCount - 1 do
    Generate(i);
end;

procedure TPatchFile.Generate(const Index: Integer);
var
  Size: Integer;
  fm: TMemoryStream;
begin
  fm:=TMemoryStream.Create;
  Size:=DoGenerate(FOld[Index].FileName,FNew.FileName,fm,ConfigID);
  if not (Size=-1) then begin
    if (FOld[Index].Cache.Size>Size) or (not FOld[Index].Cached) then begin //the new one is better
      FOld[Index].Cache.Clear;
      fm.Seek(8,soFromBeginning);
      FOld[Index].Cache.CopyFrom(fm,fm.Size-8);
    end;
    FOld[Index].Cached:=True;
  end;
  fm.Free;
end;

function TPatchFile.GetCached(Index: Integer): Boolean;
begin
  GetCached:=FOld[Index].Cached;
end;

function TPatchFile.GetConfig: String;
begin
  GetConfig:=ConfigID;
end;

procedure TPatchFile.SetConfig(Value: String);
begin
  if not Assigned(Self) then Exit;
  if not SameText(Value,ConfigID) then begin
    InvalidateCache;  //configuration changed, invalidate cache
  end;
  ConfigID:=Value;
end;

function TPatchFile.GetPatchSize(Index: Integer): Integer;
begin
  if Generated[Index] then begin
    GetPatchSize:=FOld[Index].Cache.Size;
  end else
    GetPatchSize:=-1;
end;

procedure TPatchFile.InvalidateCache;
var
  i: Integer;
begin
  for i:=0 to Length(FOld)-1 do
    InvalidateCache(i);
end;

procedure TPatchFile.InvalidateCache(Index: Integer);
begin
  FOld[Index].Cached:=False;
end;

procedure TPatchFile.WritePatch(Index: Integer; Stream: TStream);
begin
  if not FOld[Index].Cached then
    Generate(Index);
  if not FOld[Index].Cached then
    raise Exception.Create('Writing of patch failed: Could not generate all patches');
  FOld[Index].Cache.Seek(0,soFromBeginning);
  Stream.CopyFrom(FOld[Index].Cache,FOld[Index].Cache.Size);
end;

{ TPatchProject }

procedure TPatchProject.AddNewVersion(FileName: String);
var
  i: Integer;
begin
  i:=Length(FPat);
  SetLength(FPat,i+1);
  FPat[i]:=TPatchFile.Create(i,FileName);
  FPat[i].SetConfig(DEFAULT_CONFIG);
end;

constructor TPatchProject.Create;
begin
  inherited;
  SetLength(FPat,0);
end;

destructor TPatchProject.Destroy;
var
  i: Integer;
begin
  for i:=0 to Length(FPat)-1 do begin
    FPat[i].Free;
  end;
  SetLength(FPat,0);
  inherited;
end;

procedure TPatchProject.Generate;
var
  i: Integer;
begin
  for i:=0 to GetPatchCount - 1 do
    FPat[i].Generate;
end;

function TPatchProject.GetPatchCount: Integer;
begin
  GetPatchCount:=Length(FPat);
end;

procedure TPatchProject.LoadFromStream(Stream: TStream);
var
  MagicWord: Array[0..15] of Char;
  i: LongInt;
  j: Integer;
begin
  //first free all patchfiles
  for j:=0 to Length(FPat)-1 do begin
    FPat[j].Free;
    FPat[j]:=nil;
  end;
  Stream.Read(MagicWord,SizeOf(MagicWord));
  if SameText('VPatchProject 3'#26,MagicWord) then begin
    Stream.Read(i,SizeOf(i));           //4 dummy bytes
  end else
    raise Exception.Create('Error: file format incompatible (only version 3 and newer are supported).');

  Stream.Read(i,SizeOf(i));           // file count
  SetLength(FPat,i);
  for j:=0 to i - 1 do begin
    FPat[j]:=TPatchFile.Create(j,Stream);
  end;
end;

function TPatchProject.PatchFile(FileName: String): TPatchFile;
var
  i: Integer;
begin
  PatchFile:=nil;
  for i:=0 to Length(FPat) - 1 do begin
    if(CompareText(FPat[i].FNew.FileName,FileName)=0) then begin
      PatchFile:=FPat[i];
    end;
  end;
  for i:=0 to Length(FPat) - 1 do begin
    if(CompareText(FPat[i].FNew.FriendlyName,FileName)=0) then begin
      PatchFile:=FPat[i];
    end;
  end;
end;

function TPatchProject.PatchFile(Index: Integer): TPatchFile;
begin
  if (Index<Length(FPat)) and (Index>=0) then
    PatchFile:=FPat[Index]
  else
    PatchFile:=nil;
end;

procedure TPatchProject.ResetCache;
var
  i: Integer;
begin
  for i:=0 to Pred(Length(FPat)) do
    FPat[i].ResetCache;
end;

procedure TPatchProject.SaveToStream(Stream: TStream);
var
  HeadID: Array[0..15] of Char;
  i: LongInt;
  j: Integer;
begin
  HeadID:='VPatchProject 3'+#26;
  Stream.Write(HeadID,SizeOf(HeadID));
  //4 dummy bytes left
  i:=0;
  Stream.Write(i,SizeOf(i));
  i:=Length(FPat);
  Stream.Write(i,SizeOf(i));
  for j:=0 to i - 1 do begin
    FPat[j].SaveToStream(Stream);
  end;
end;

procedure TPatchProject.WritePatches(Stream: TStream);
var
  i,j,k,o: LongInt;
  q: LongWord;
begin
  k:=$54415056;
  o:=Stream.Position;
  Stream.Write(k,SizeOf(k));
  k:=0;
  Stream.Write(k,SizeOf(k));
  k:=0;
  for i:=0 to Length(FPat)-1 do begin
    for j:=0 to FPat[i].GetOldVersionCount - 1 do begin
      FPat[i].WritePatch(j,Stream);
      Inc(k);
    end;
  end;
  Stream.Seek(o+4,soFromBeginning);
  q:=k;
  // set the MD5 flag
  q:=q or $80000000;
  Stream.Write(q,SizeOf(q));
  Stream.Seek(Stream.Size,soFromBeginning);
  Stream.Write(o,SizeOf(o));
end;

end.
 