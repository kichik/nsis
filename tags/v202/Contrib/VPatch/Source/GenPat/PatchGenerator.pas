unit PatchGenerator;

{
  VPatch 2 - Patch Generator
  ==========================

  (c) 2002-2003 Van de Sande Productions

  This unit contains the 'core' functionality of VPatch. TPatchGenerator can
  load/create/save .PAT files and supports CreatePatch(Old, New) to generate
  new patches. The only configurable parameter is StartBlockSize.
  Though I cleaned up the code a little bit, there is very little documentation.
  That's why I will briefly explain the general workings of the current VPatch
  algoritm.
  There is a source file, which is divided into blocks of BlockSize. Block 1
  spans bytes 0-15, block 2 16-31, etc if blocksize = 16. For every block, a
  checksum is calculated and then the block is inserted into a binary search
  tree, which is sorted on this checksum.
  Now, the target file (new version) is traversed linearly. For a block at a
  certain position, the checksum is calculated. Then, a lookup is performed in
  the binary search tree to find all blocks in the source file which match this
  checksum. For every occurence, it is checked how many consecutive bytes match
  with this block (note: since the checksum is not unique, this can be 0 as well,
  but since all occurences are checked, the largest match is selected). Note also
  that this match length is not limited to BlockSize but can be larger as well;
  everything beyond the block end is checked as well (the block is merely used
  as a starting position for checking the match).
  For those biggest block matches between source/target files, a copy instruction
  will be generated in the patch file. For 'inbetween' or unmatchable blocks, the
  data of the new file is placed in the patch file. This involves some
  housekeeping, which is what most of the other code does.

  What's new
  ----------
  2.1   20031219    Koen            Added error checking to CreatePatch, returns
                                    negative numbers when there are errors.
  2.0   20030811    Koen            Initial documentation
}

interface

uses
  Classes,
  Sysutils,
  TreeCode,
  VDSP_CRC;

type
  TStatusNotifyEvent = procedure(S: String; Current, Total, Savings: Integer) of object;
  TDebugNotifyEvent = procedure(S: String) of object;
  PDataBlock = ^TDataBlock;
  TDataBlock = record
    SourceOffset: Integer;
    TargetOffset: Integer;
    Size: Integer;
    Next: PDataBlock;
  end;
  //internal structure for FindBlock
  TBlock = record
    Offset: Integer;
    Size: Integer;
  end;
  TPatchGenerator = class
  private
    noPat: Integer;
    PRay: Array of TDataBlock;
    NRay: Array of TDataBlock;

    FPatchData: TMemoryStream;
    FStartBlockSize: Integer;  //initial block size
    FBlockDivider: Integer;    //... block size is divided by this
    FMinimumBlockSize: Integer;//until this minimum is reached
    FStepSize: Integer;

    //input: ASubBlock, which is a pointer to the start of the block to look
    //for in ABlock. The entire ABlock is searched. The function returns the
    //offset of the block, when it is found. The ASize parameter contains the
    //size of this block
    procedure ShowDebug(S: String);

    function FindBlock(ASubBlock, ABlock, ABlockTree: Pointer;
      var ASubBlockStart: Integer; ASubBlockSize, ABlockSize,
      AMatchSize, ABlockTreeNodeCount: Integer; var ASize: Integer): Integer;

    procedure FindBlockSize(ASubBlock, ABlock: Pointer; ASubBlockSize,
      ABlockSize: Integer; var ASubStart, AStart, AFoundSize: Integer);
    function WritePatchToStream(Target: Pointer; SourceCRC,
      TargetCRC: Integer): Integer;

    procedure RemoveExistingPatch(ACRC: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function CreatePatch(SourceFileName, TargetFileName: String): Integer;
    property StartBlockSize: Integer read FStartBlockSize write FStartBlockSize;
    property BlockDivider: Integer read FBlockDivider write FBlockDivider;
    property MinimumBlockSize: Integer read FMinimumBlockSize write FMinimumBlockSize;
    property StepSize: Integer read FStepSize write FStepSize;
    function Size: Integer;
    procedure WriteToFile(AFileName: String);
    procedure WriteToStream(AStream: TStream);
    procedure LoadFromFile(AFileName: String);
  end;


const
  BUF_BLOCK_SIZE = 4096;
  INIT_BLOCK_COUNT=10000;

var
  DebugEvent: TDebugNotifyEvent = nil;

implementation

{ TPatchGenerator }

procedure TPatchGenerator.Clear;
begin
  FPatchData.Clear;
end;

constructor TPatchGenerator.Create;
begin
  inherited;
  FPatchData:=TMemoryStream.Create;
end;

function TPatchGenerator.CreatePatch(SourceFileName,
  TargetFileName: String): Integer;
var
  fsSource, fsTarget: TFileStream;
  fm: TMemoryStream;
  Source, Target: Pointer;
  SourceSize, TargetSize: Integer;
  SourceCRC, TargetCRC: Integer;
  SourceTree: Pointer;
  SourceTreeNodeCount: Cardinal;
  cBlockSize: Integer;

  o,i,lastO: Integer;
  Start,Siz,BetweenSiz: Integer;
  retTO: Integer;
  noN: Integer;
begin
  fsSource:=TFileStream.Create(SourceFileName,fmOpenRead+fmShareDenyNone);
  fsTarget:=TFileStream.Create(TargetFileName,fmOpenRead+fmShareDenyNone);
  fm:=TMemoryStream.Create;

  SetLength(PRay,INIT_BLOCK_COUNT);
  SetLength(NRay,INIT_BLOCK_COUNT);

  //Load those files into memory!
  SourceSize:=fsSource.Size;
  try
    GetMem(Source,SourceSize);
  except
    on EOutOfMemory do begin
      Result:=-2;           // not enough memory for source file
      Exit;
    end;
  end;
  fm.CopyFrom(fsSource,SourceSize);
  Move(fm.Memory^,Source^,SourceSize);
  SourceCRC:=FileCRC(fsSource);
  fsSource.Free;

  fm.Clear;
  TargetSize:=fsTarget.Size;
  try
    GetMem(Target,TargetSize);
  except
    on EOutOfMemory do begin
      FreeMem(Source,SourceSize);
      Result:=-3;           // not enough memory for target file
      Exit;
    end;
  end;
  fm.CopyFrom(fsTarget,TargetSize);
  Move(fm.Memory^,Target^,TargetSize);
  TargetCRC:=FileCRC(fsTarget);
  fsTarget.Free;
  fm.Free;

  if(SourceCRC = TargetCRC) then begin
    FreeMem(Source,SourceSize);
    FreeMem(Target,TargetSize);
    Result:=-1;
    Exit;
  end;

  PRay[0].TargetOffset:=0;
  PRay[0].SourceOffset:=0;
  PRay[0].Size:=0;
  noPat:=1;

  //termination block

  PRay[noPat].SourceOffset:=0;
  PRay[noPat].TargetOffset:=TargetSize;
  PRay[noPat].Size:=0;

  //we only have one pass in this mode
//  StartBlockSize:=16;
  MinimumBlockSize:=StartBlockSize;
  StepSize:=1;
  BlockDivider:=2;

  //because we are dividing first inside.
  cBlockSize:=StartBlockSize*BlockDivider;

  SourceTree:=nil;
  SourceTreeNodeCount:=BuildTree(Source,SourceTree,SourceSize,cBlockSize div BlockDivider);
  SortTree(SourceTree,SourceTreeNodeCount);

  //now, we must do the above again - with a smaller block size
  repeat
    if cBlockSize<=MinimumBlockSize then break;
    cBlockSize:=cBlockSize div BlockDivider;
    noN:=0;
    for i:=1 to noPat do begin
      //calculate location of the inbetween parts
      Start:=PRay[i-1].TargetOffset+PRay[i-1].Size;
      BetweenSiz:=PRay[i].TargetOffset-Start;

      NRay[noN].SourceOffset:=PRay[i-1].SourceOffset;
      NRay[noN].TargetOffset:=PRay[i-1].TargetOffset;
      NRay[noN].Size:=PRay[i-1].Size;
      Inc(noN);
      if BetweenSiz>0 then begin

        o:=Start;
        repeat
          //ShowDebug(PChar('DoFind '+IntToStr(o)));
          LastO:=o;
          retTO:=FindBlock(Target,Source,SourceTree,o,TargetSize,SourceSize,cBlockSize,SourceTreeNodeCount,Siz);
          if not (Siz=0) then
            ShowDebug(IntToStr(LastO)+'  ->  Source='+IntToStr(retTO)+' Target='+IntToStr(o)+' Size='+IntToStr(Siz));

          if Siz=0 then begin
            o:=LastO+StepSize;
          end else begin
            //we have found a block, let's add it!
            NRay[noN].SourceOffset:=retTO;
            NRay[noN].TargetOffset:=o;
            NRay[noN].Size:=Siz;
            Inc(noN);
            if noN>=Length(NRay) then begin
              SetLength(NRay,Length(NRay)*2);
              SetLength(PRay,Length(PRay)*2);
            end;
            Inc(o,Siz);
          end;

          //check to see if we're not inside another one.
          Siz:=NRay[noN].TargetOffset-NRay[noN-1].TargetOffset-NRay[noN-1].Size;
          If Siz<0 then begin  //that's impossible! (overlapping should be eliminated)
            NRay[noN].TargetOffset:=NRay[noN].TargetOffset-Siz;
            NRay[noN].Size:=NRay[noN].Size+Siz;
            NRay[noN].SourceOffset:=NRay[noN].SourceOffset-Siz;
          end;
        until o>Start+BetweenSiz;

      end;
    end;
    //I think the last termination block isn't copied: do so now.
    NRay[noN].SourceOffset:=PRay[noPat].SourceOffset;
    NRay[noN].TargetOffset:=PRay[noPat].TargetOffset;
    NRay[noN].Size:=PRay[noPat].Size;
    //copy back into PRay
    for i:=0 to noN do begin
      PRay[i].SourceOffset:=NRay[i].SourceOffset;
      PRay[i].TargetOffset:=NRay[i].TargetOffset;
      PRay[i].Size:=NRay[i].Size;
    end;
    noPat:=noN;
  until false;

  //writing is next!
  ShowDebug('Writing patch');

  Result:=WritePatchToStream(Target, SourceCRC, TargetCRC);

  ClearTree(SourceTree,SourceTreeNodeCount);
  FreeMem(Source,SourceSize);
  FreeMem(Target,TargetSize);
  ShowDebug('Done');
end;

destructor TPatchGenerator.Destroy;
begin
  FPatchData.Free;
  inherited;
end;

function TPatchGenerator.FindBlock(ASubBlock, ABlock, ABlockTree: Pointer; var ASubBlockStart: Integer;
  ASubBlockSize, ABlockSize, AMatchSize, ABlockTreeNodeCount: Integer; var ASize: Integer): Integer;
//This procedure locates location of a block in the target file
//Then, it calls FindBlockSize to determine size of this block
var
  MatchSize, FoundSize: Integer;
  q,r,i: Integer;
  FoundCache_SubOffset, FoundCache_Size, FoundCache_Offset: Integer;
  Checksum: Cardinal;
  PFound: PTreeNode;
  FoundCount: Integer;
begin
  //if we find nothing...

  FoundCache_Size:=0;
  FoundCache_Offset:=0;
  FoundCache_SubOffset:=ASubBlockStart;

  FindBlock:=0;
  ASize:=0;

  MatchSize:=AMatchSize;

  //we can only find MatchSize sized blocks in the tree!
  if MatchSize > ASubBlockSize - ASubBlockStart then Exit;
  if MatchSize = 0 then Exit;

  Checksum:=0;
  calculateChecksum(ASubBlock,ASubBlockStart,MatchSize,Checksum);
  PFound:=TreeFind(Checksum,ABlockTree,ABlockTreeNodeCount,FoundCount);

  for i:=0 to Pred(FoundCount) do begin
    FoundSize:=MatchSize;
    //q = offset in Block
    q:=PFound^.Offset;
    //r = offset in SubBlock
    r:=ASubBlockStart;
    FindBlockSize(ASubBlock, ABlock, ASubBlockSize, ABlockSize, r, q, FoundSize);
    if FoundSize>FoundCache_Size then begin
      FoundCache_SubOffset:=r;
      FoundCache_Offset:=q;
      FoundCache_Size:=FoundSize;
    end;
    ShowDebug('   Block Size   Start='+IntToStr(r)+' tarStart='+IntToStr(q)+' Size='+IntToStr(FoundSize));
    PFound:=PTreeNode(Integer(PFound)+SizeOf(TTreeNode));
  end;

  FindBlock:=FoundCache_Offset;
  ASize:=FoundCache_Size;
  ASubBlockStart:=FoundCache_SubOffset;
end;

procedure TPatchGenerator.FindBlockSize(ASubBlock, ABlock: Pointer; ASubBlockSize, ABlockSize: Integer; var ASubStart,AStart,AFoundSize: Integer);
var
  FoundSize: Integer;
  a,c,d,i: Integer;
  f1p,f2p,f1Size,f2Size: Integer;
  beforeSize: Integer;
  CurBufSize: Integer;
begin
  //OK, now let's go...
  //Trace after -> how long does this go on?
  f1p:=Integer(ASubBlock)+ASubStart;
  f2p:=Integer(ABlock)+AStart;
  f1Size:=ASubBlockSize-ASubStart;
  f2Size:=ABlockSize-AStart;
  FoundSize:=0;
  CurBufSize := BUF_BLOCK_SIZE; //size of the block we're checking
  while not (CurBufSize = 0) do begin
    //we need equal bytes from both... so if one of them EOF, it's the end.
    if FoundSize+CurBufSize>f1Size then CurBufSize:=f1Size - FoundSize;
    if FoundSize+CurBufSize>f2Size then CurBufSize:=f2Size - FoundSize;
    if CompareMem(Pointer(f1p),Pointer(f2p),CurBufSize) then begin
      Inc(FoundSize,CurBufSize);
      Inc(f1p,CurBufSize);
      Inc(f2p,CurBufSize);
    end
    else begin
      CurBufSize:=CurBufSize div 2;
    end;
  end;

  if FoundSize = 0 then begin AFoundSize:=0; Exit; end;

  //Trace before -> how much bytes are still the same before the block?
  //First, read 1 block from source and 1 block from target, start from back to compare how much they differ
  //just take BUF_BLOCK_SIZE as maximum size for the block before - that's surely
  //big enough!
  beforeSize:=BUF_BLOCK_SIZE;
  a:=ASubStart-beforeSize;
  if a<0 then begin
    a:=0;
    beforeSize:=ASubStart;
  end;
  //b is the current before block size
  c:=AStart-beforeSize;
  if c<0 then begin
    c:=0;
    beforeSize:=AStart;
    a:=ASubStart-beforeSize;
  end;
  //a=Offset in source
  //b=Size of beforeblock
  //c=offset in target

  d:=0;
  for i:=beforeSize-1 downto 0 do begin
    //if not (f1^[a+i]=f2^[c+i]) then begin
    if not (PByte(Integer(ASubBlock)+a+i)^=PByte(Integer(ABlock)+c+i)^) then begin
      //d=how many bytes before are the same?
      Break;
    end;
    Inc(d);
  end;
  Inc(FoundSize,d);
  Dec(ASubStart,d);
  Dec(AStart,d);
  AFoundSize:=FoundSize;
end;

function TPatchGenerator.Size: Integer;
begin
  Result:=FPatchData.Size;
end;

procedure TPatchGenerator.ShowDebug(S: String);
begin
  if Assigned(DebugEvent) then DebugEvent(S);
end;

function TPatchGenerator.WritePatchToStream(Target: Pointer; SourceCRC, TargetCRC: Integer): Integer;
var
  HeadID: Array[0..3] of Char;
  NoBlocks, NoBlocksOffset, BodySize, BodySizeOffset: Integer;
  b: Byte;
  w: Word;
  i, j: Integer;
  l: LongWord;
  Start, Siz: Integer;
  PTarget: Pointer;
begin
  RemoveExistingPatch(SourceCRC);
  with FPatchData do begin
    Seek(0,soFromEnd);
    if Size = 0 then begin
      HeadID:='VPAT';
      Write(HeadID,SizeOf(HeadID));
      l:=0;
      Write(l,SizeOf(l)); //NoFiles
    end;
    l:=0;
    NoBlocksOffset:=Position;
    Write(l,SizeOf(l)); //should become NoBlocks later
    Write(SourceCRC,SizeOf(SourceCRC)); //source CRC
    Write(TargetCRC,SizeOf(TargetCRC)); //target CRC
    BodySizeOffset:=Position;
    Write(l,SizeOf(l)); //should become BodySize (of this patch)

    NoBlocks:=0;
    BodySize:=0;
    //Write the patch...
    for i:=0 to noPat - 1 do begin
      //write char 1 - integer/copysource
      //write char 2 - long/copysource
      //write char 5 - integer/insidepatch
      //write char 6 - long/insidepatch

      Start:=PRay[i].TargetOffset+PRay[i].Size;
      Siz:= PRay[i+1].TargetOffset-Start;

      If Siz<0 then begin  //that's impossible! (overlapping should be eliminated)
        PRay[i+1].TargetOffset:=PRay[i+1].TargetOffset-Siz;
        PRay[i+1].Size:=PRay[i+1].Size+Siz;
        PRay[i+1].SourceOffset:=PRay[i+1].SourceOffset-Siz;
        Siz:=0;
      end;

      if not (PRay[i].Size=0) then begin
        if (PRay[i].Size<=255) then begin
          b:=1;
          Write(b,SizeOf(b));
          b:=PRay[i].Size;
          Write(b,SizeOf(b));
          Inc(BodySize,2);
        end else if PRay[i].Size<=65535 then begin
          b:=2;
          Write(b,SizeOf(b));
          w:=PRay[i].Size;
          Write(w,SizeOf(w));
          Inc(BodySize,3);
        end else begin
          b:=3;
          Write(b,SizeOf(b));
          Write(PRay[i].Size,SizeOf(Integer));
          Inc(BodySize,5);
        end;
        Write(PRay[i].SourceOffset,SizeOf(Integer));
        Inc(BodySize,SizeOf(Integer));
        Inc(NoBlocks);
      end;
      //Now write the writeblock
      If Not (Siz = 0) Then begin
        if Siz<=255 then begin
          b:=5;
          Write(b,SizeOf(b));
          b:=Siz;
          Write(b,SizeOf(b));
          Inc(BodySize,2);
        end else if Siz<=65535 then begin
          b:=6;
          Write(b,1);
          w:=Siz;
          Write(w,2);
          Inc(BodySize,3);
        end else begin
          b:=7;
          Write(b,1);
          Write(Siz,4);
          Inc(BodySize,5);
        end;
        PTarget:=Pointer(Integer(Target)+Start);
        j:=Start;
        repeat
          //read
          if (j+4096>Start+Siz) then begin
            Write(PTarget^,Start+Siz-j);
            break;
          end;
          Write(PTarget^,4096);
          Inc(j,4096);
          PTarget:=Pointer(Integer(PTarget)+4096);
        until false;
        Inc(BodySize,Siz);
        Inc(NoBlocks);
      end;
    end;
    Seek(NoBlocksOffset,soFromBeginning);
    Write(NoBlocks,SizeOf(NoBlocks));
    Seek(BodySizeOffset,soFromBeginning);
    Write(BodySize,SizeOf(BodySize));
    ShowDebug('Patch body size: '+IntToStr(BodySize));
    ShowDebug('Total patch size:'+IntToStr(Size));
    //now increase file count
    Seek(4,soFromBeginning);
    Read(i,SizeOf(i));
    Inc(i);
    Seek(4,soFromBeginning);
    Write(i,SizeOf(i));
    Seek(0,soFromEnd);
    Result:=BodySize;
  end;
end;

procedure TPatchGenerator.WriteToFile(AFileName: String);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(AFileName,fmCreate);
  FPatchData.Seek(0,soFromBeginning);
  fs.CopyFrom(FPatchData,FPatchData.Size);
  fs.Free;
end;

procedure TPatchGenerator.LoadFromFile(AFileName: String);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(AFileName,fmOpenRead);
  FPatchData.Clear;
  FPatchData.CopyFrom(fs,fs.Size);
  fs.Free;
end;

procedure TPatchGenerator.RemoveExistingPatch(ACRC: Integer);
var
  HeadID: Array[0..3] of Char;
  NoFiles, i, j, SourceCRC, MSize: Integer;
  StartPos: Integer;
  ms: TMemoryStream;
begin
  with FPatchData do begin
    if Size = 0 then Exit;
    Seek(0,soFromBeginning);
    Read(HeadID,SizeOf(HeadID));
    if HeadID = 'VPAT' then begin
      Read(NoFiles,SizeOf(NoFiles));
      for i:=0 to Pred(NoFiles) do begin
        if Position >= Size then Break;
        StartPos:=Position;
        Read(j,SizeOf(j));                 //NoBlocks
        Read(SourceCRC,SizeOf(SourceCRC)); //SourceCRC
        Read(j,SizeOf(j));                 //TargetCRC
        Read(j,SizeOf(j));                 //BodySize
        Seek(j,soFromCurrent);
        if SourceCRC = ACRC then begin
          ms:=TMemoryStream.Create;
          MSize:=Size-Position;
          if MSize > 0 then ms.CopyFrom(FPatchData,MSize);
          ms.Seek(0, soFromBeginning);
          FPatchData.Seek(StartPos,soFromBeginning);
          FPatchData.SetSize(Size - j - SizeOf(Integer) * 4);
          FPatchData.CopyFrom(ms,ms.Size);
          ms.Free;
          Dec(NoFiles);
          Seek(4,soFromBeginning);
          Write(NoFiles,SizeOf(NoFiles));
          Break;
        end;
      end;
    end;
  end;
end;

procedure TPatchGenerator.WriteToStream(AStream: TStream);
begin
  FPatchData.Seek(0,soFromBeginning);
  AStream.CopyFrom(FPatchData,FPatchData.Size);
end;

end.
