unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Math;

type
  TfrmProg = class(TForm)
    prgFile: TProgressBar;
    lblFile: TLabel;
    lblNewFile: TLabel;
    prgNewFile: TProgressBar;
    lblTotal: TLabel;
    prgAll: TProgressBar;
    lblStatus: TLabel;
    shpFull: TShape;
    shpLeft: TShape;
    lblSize: TLabel;
    procedure GetStatusProc(S: PChar; Point, Total,
      CurrentSavings: Integer); stdcall;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FilePos,FileRange,AllPos,AllRange: Byte;
    CTotal: Integer;
    t2: TDateTime;
  end;

var
  frmProg: TfrmProg;

implementation

{$R *.dfm}

procedure TfrmProg.GetStatusProc(S: PChar; Point, Total, CurrentSavings: Integer); stdcall;
var
  a,b: Integer;
  j: Single;
begin
  if Length(S)>0 then
    lblStatus.Caption:=S;
  if (Total<0) then begin
    Total:=CTotal;
    if (Now-t2)*24*3600*10<8 then Exit; //update only every 800 milliseconds
  end;
  if (Total>=0) then CTotal:=Total;
  if (Total>=0) and (Point>=0) then begin
    a:=(Point*100) div Total;
    prgFile.Position:=a;
    b:=FilePos+(a*FileRange) div 100;
    prgNewFile.Position:=b;
    prgAll.Position:=AllPos+(b*AllRange) div 100;
  end;
  if (CurrentSavings>=0) and (Total>=0) then begin
    j:=(Total-CurrentSavings)*shpFull.Width/Total;
    shpLeft.Width:=Max(Round(j),3);
    lblSize.Caption:=IntToStr(Total-CurrentSavings)+' of '+IntToStr(Total)+' ('+IntToStr(CurrentSavings*100 div Total)+'%)';
  end;
  Refresh;
  t2:=Now;
end;

procedure TfrmProg.FormCreate(Sender: TObject);
begin
  FilePos:=0; FileRange:=100; AllPos:=0; AllRange:=100; CTotal:=-1;
  t2:=0;
end;

end.
