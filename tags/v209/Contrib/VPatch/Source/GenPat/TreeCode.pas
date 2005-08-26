unit TreeCode;

{
  VPatch 2 - Binary Checksum Tree
  ===============================

  (c) 2002-2003 Van de Sande Productions

  This unit implements a binary search tree, which is constructed from a memory
  block by BuildTree. This memory block is divided into equal-sized blocks of
  BlockSize, and for every block a checksum is calculated. Then, it is inserted
  in the binary tree, which is sorted on the checksum.
  The patch generator will search for the checksums using a binary search, which
  is O(log n) (much better than the old 1.x algoritm, which was O(n)).

  What's new
  ----------
  2.1   20031219    Koen            Fixed bug in TreeFind: when tree was a nil
                                    pointer, now returns instead of AVing
  2.0   20030811    Koen            Initial documentation
}

interface

type
  TSortStack=record
    lo,hi: Integer;
  end;
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    Checksum: Cardinal;
    Offset: Cardinal;
  end;

const
  TREENODE_SIZE = SizeOf(TTreeNode);  

procedure calculateChecksum(AData: Pointer; AStart, ASize: Cardinal; var K: Cardinal);
procedure calculateNext(AData: Pointer; AStart, ASize: Cardinal; var K: Cardinal);
function BuildTree(ASource: Pointer; var ATree: Pointer; ASourceSize, ABLOCKSIZE: Cardinal): Cardinal;
procedure SortTree(ATree: Pointer; ANodeCount: Cardinal);
procedure ClearTree(var ATree: Pointer; ANodeCount: Cardinal);
function TreeFind(AChecksum: Cardinal; ABlockTree: Pointer; ABlockTreeNodeCount: Integer; var FoundCount: Integer): PTreeNode;
function GetItem(ATree: Pointer; Index, ANodeCount: Cardinal): TTreeNode;

procedure Test;

implementation

uses SysUtils;

procedure calculateChecksum(AData: Pointer; AStart, ASize: Cardinal; var K: Cardinal);
var
  A,B,i,j: Cardinal;
begin
  A:=K and $0000FFFF;
  B:=(K and $FFFF0000) shr 16;
  j:=Cardinal(AData)+AStart;
  for i:=1 to ASize do begin
    A:=A + PByte(j)^;
    B:=B + (ASize-i+1)*PByte(j)^;
    Inc(j);
  end;
  K:=(A and $0000FFFF) or ((B and $0000FFFF) shl 16);
end;

procedure calculateNext(AData: Pointer; AStart, ASize: Cardinal; var K: Cardinal);
var
  A,B,j: Cardinal;
begin
  j:=Cardinal(AData)+AStart;
  A:=(K-PByte(j-1)^+PByte(j+ASize-1)^) and $0000FFFF;
  B:=((K shr 16)-ASize*PByte(j-1)^+A) and $0000FFFF;
  K:=A or (B shl 16);
end;

function BuildTree(ASource: Pointer; var ATree: Pointer; ASourceSize, ABLOCKSIZE: Cardinal): Cardinal;
var
  i, NodeCount: Cardinal;
  Node: TTreeNode;
begin
  Assert(not Assigned(ATree),'Cannot use initialized tree in BuildTree!');
  NodeCount:=ASourceSize div ABLOCKSIZE;
  GetMem(ATree,NodeCount*TREENODE_SIZE);
  if NodeCount > 0 then begin
    for i:=0 to Pred(NodeCount) do begin
      Node.Offset:=i*ABLOCKSIZE;
      Node.Checksum:=0;
      calculateChecksum(ASource,Node.Offset,ABLOCKSIZE,Node.Checksum);
      Move(Node,Pointer(Cardinal(ATree)+i*TREENODE_SIZE)^,TREENODE_SIZE);
    end;
  end;
  Result:=NodeCount;
end;

procedure SetItem(ATree: Pointer; Index, ANodeCount: Cardinal; New: TTreeNode);
var
  p: PTreeNode;
begin
  Assert(Index<ANodeCount,'Tree/GetItem: Index too big');
  p:=PTreeNode(Cardinal(ATree)+Index*TREENODE_SIZE);
  p^:=New;
end;

function GetItem(ATree: Pointer; Index, ANodeCount: Cardinal): TTreeNode;
var
  p: PTreeNode;
begin
  Assert(Index<ANodeCount,'Tree/GetItem: Index too big '+IntToStr(Index));
  p:=PTreeNode(Cardinal(ATree)+Index*TREENODE_SIZE);
  Result:=p^;
end;

procedure SortTree(ATree: Pointer; ANodeCount: Cardinal);
var
  compare: Cardinal;
  aStack: Array[1..128] of TSortStack;
  StackPtr: Integer;
  Mid,i,j,low,hi: Integer;
  Switcher: TTreeNode;
begin
  If ANodeCount = 0 Then Exit;
  StackPtr:=1;
  aStack[StackPtr].lo:=0;
  aStack[StackPtr].hi:=ANodeCount - 1;
  Inc(StackPtr);
  while not (StackPtr=1) do begin
    StackPtr:=StackPtr-1;
    low:=aStack[StackPtr].lo;
    hi:=aStack[StackPtr].hi;
    while true do begin
      i:=low;
      j:=hi;
      Mid:=(low + hi) div 2;
      compare:=PTreeNode(Integer(ATree)+Mid*TREENODE_SIZE)^.Checksum;
      while true do begin
        While PTreeNode(Integer(ATree)+i*TREENODE_SIZE)^.Checksum < compare do begin
          Inc(i);
        end;
        While PTreeNode(Integer(ATree)+j*TREENODE_SIZE)^.Checksum > compare do begin
          j:=j-1;
        end;
        If (i <= j) Then begin
          Move(Pointer(Integer(ATree)+j*TREENODE_SIZE)^,Switcher,TREENODE_SIZE);
          Move(Pointer(Integer(ATree)+i*TREENODE_SIZE)^,Pointer(Integer(ATree)+j*TREENODE_SIZE)^,TREENODE_SIZE);
          Move(Switcher,Pointer(Integer(ATree)+i*TREENODE_SIZE)^,TREENODE_SIZE);
          Inc(i);
          Dec(j);
        End;
        if not (i <= j) then break;
      end;
      If j - low < hi - i Then begin
        If i < hi Then begin
          aStack[StackPtr].lo:=i;
          aStack[StackPtr].hi:=hi;
          Inc(StackPtr);
        End;
        hi:=j;
      end Else begin
        If low < j Then begin
          aStack[StackPtr].lo:=low;
          aStack[StackPtr].hi:=j;
          Inc(StackPtr);
        End;
        low:=i;
      End;
      if not (low<hi) then break;
    end;
    if StackPtr=1 then break;
  end;
end;

procedure ClearTree(var ATree: Pointer; ANodeCount: Cardinal);
begin
  FreeMem(ATree,ANodeCount*TREENODE_SIZE);
  ATree:=nil;
end;

function TreeFind(AChecksum: Cardinal; ABlockTree: Pointer; ABlockTreeNodeCount: Integer; var FoundCount: Integer): PTreeNode;
var
  lo,mid,hi,m: Integer;
  tmp: Cardinal;
begin
  if not Assigned(ABlockTree) then begin
    FoundCount:=0; Result:=nil;
    Exit;
  end;
  lo:=0;
  hi:=ABlockTreeNodeCount-1;
  while true do begin
    mid:=(lo+hi) div 2;
    tmp:=PCardinal(Integer(ABlockTree)+mid*TREENODE_SIZE)^;
    if tmp = AChecksum then begin
      FoundCount:=1;
      m:=mid;
      Result:=PTreeNode(Integer(ABlockTree)+m*TREENODE_SIZE);
      while m > 0 do begin
        Dec(m);
        if PCardinal(Integer(ABlockTree)+m*TREENODE_SIZE)^ = tmp then begin
          Result:=PTreeNode(Integer(ABlockTree)+m*TREENODE_SIZE);
          Inc(FoundCount);
        end else
          Break;
      end;
      m:=mid;
      while m < ABlockTreeNodeCount-1 do begin
        Inc(m);
        if PCardinal(Integer(ABlockTree)+m*TREENODE_SIZE)^ = tmp then begin
          Inc(FoundCount);
        end else
          Break;
      end;
      Exit;
    end;
    if lo>=hi then Break;
    if AChecksum < tmp then begin
      hi:=mid-1;
    end else begin
      lo:=mid+1;
    end;
  end;
  FoundCount:=0; Result:=nil;
end;

procedure Test;
var
  p: Pointer;
  t: TTreeNode;
  r: PTreeNode;
  i,q: Integer;
  NC: Integer;
begin
  NC:=100;
  GetMem(p,800);
  for i:=0 to 99 do begin
    t.Offset:=i*100;
    t.Checksum:=i div 2;
    SetItem(p,i,NC,t);
  end;
  SortTree(p,NC);
  for i:=0 to 99 do begin
    t:=GetItem(p,i,NC);
    Write(IntToStr(t.Checksum)+'  ');
  end;
  r:=TreeFind(7,p,NC,q);
  WriteLn(IntToStr(q));
  t:=r^;
  WriteLn(IntToStr(t.Checksum)+'  ');
end;

end.
