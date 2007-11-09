unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, Menus, PatchClasses, VirtualTrees, VDSP_CRC,
  ToolWin, ComCtrls, ImgList, ExtCtrls, Math, OSUtil;

const
  UntitledFile='Untitled.vpj';

type
  TfrmMain = class(TForm)
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveas: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    Label1: TLabel;
    grpConfig: TGroupBox;
    butAdd: TSpeedButton;
    OD: TOpenDialog;
    Label2: TLabel;
    txtNew: TEdit;
    Label3: TLabel;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    lstOld: TListBox;
    butOldAdd: TSpeedButton;
    butOldRemove: TSpeedButton;
    butNewEdit: TSpeedButton;
    Label4: TLabel;
    lstNew: TVirtualStringTree;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    IL: TImageList;
    mnuAction: TMenuItem;
    mnuGenGo: TMenuItem;
    barTool: TToolBar;
    toolNew: TToolButton;
    toolOpen: TToolButton;
    toolSave: TToolButton;
    toolGenGo: TToolButton;
    mnuCreateEXE: TMenuItem;
    dlgSaveExe: TSaveDialog;
    toolCreateEXE: TToolButton;
    barCool: TCoolBar;
    Label5: TLabel;
    Label7: TLabel;
    tbBlockSize: TTrackBar;
    txtStartBlockSize: TLabel;
    mnuClearcachedpatches: TMenuItem;
    mnuCreateDLL: TMenuItem;
    mnuCreatePAT: TMenuItem;
    toolCreateDLL: TToolButton;
    ToolButton1: TToolButton;
    toolCreatePAT: TToolButton;
    dlgSaveDLL: TSaveDialog;
    dlgSavePAT: TSaveDialog;
    chkOutputWait: TCheckBox;
    mnuWebsite: TMenuItem;
    Readme1: TMenuItem;
    N2: TMenuItem;
    ReadmeincludedwithNSIS1: TMenuItem;
    chkOptimal: TCheckBox;
    procedure butAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure UpdateStates;
    procedure ReloadNewTree;
    procedure SelectInNewTree(PatchIndex: Integer);
    procedure butNewEditClick(Sender: TObject);
    procedure lstNewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure butOldAddClick(Sender: TObject);
    procedure butOldRemoveClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuSaveasClick(Sender: TObject);
    procedure mnuGenGoClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuCreateEXEClick(Sender: TObject);
    procedure lstNewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure txtStartBlockSizeChange(Sender: TObject);
    procedure txtMinimumBlockSizeChange(Sender: TObject);
    procedure txtBlockDividerChange(Sender: TObject);
    procedure txtStepSizeChange(Sender: TObject);
    procedure tbBlockSizeChange(Sender: TObject);
    procedure mnuClearcachedpatchesClick(Sender: TObject);
    procedure mnuCreateDLLClick(Sender: TObject);
    procedure mnuCreatePATClick(Sender: TObject);
    procedure chkOutputWaitClick(Sender: TObject);
    procedure mnuWebsiteClick(Sender: TObject);
    procedure Readme1Click(Sender: TObject);
    procedure ReadmeincludedwithNSIS1Click(Sender: TObject);
    procedure chkOptimalClick(Sender: TObject);
  private
    { Private declarations }
//    MS: TModeSelector;
    dskName: String;
    function DoSave(const FileName: String; const Prompt: Boolean): String;
    procedure OpenAFile(FileName: String; AskSave: Boolean=True; PromptNew: Boolean=False);
    function CollectConfig: String;
    procedure SetConfigTextBoxes(Config: String);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  PP: TPatchProject = nil;

implementation

uses AboutForm, DLLWrapper;

{$R *.dfm}

procedure TfrmMain.butAddClick(Sender: TObject);
begin
  OD.Options:=OD.Options-[ofAllowMultiSelect];
  OD.Title:='Open the latest (new) version of a file...';
  OD.FileName:='';
  if OD.Execute then begin
    PP.AddNewVersion(OD.FileName);
    ReloadNewTree;
    SelectInNewTree(PP.PatchFile(OD.FileName).Index);
    butOldAdd.Click;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  grpConfig.Tag:=-1;
  dskName:=UntitledFile;
  lstNew.NodeDataSize:=SizeOf(Integer);
  OpenAFile('',False,False);  //don't prompt for New! that'll bug things
  ReloadNewTree;
  UpdateStates;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  PP.Free;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.UpdateStates;
begin
  Self.Caption:='VG - VPatch GUI - '+dskName;
//  grpConfig.Enabled:=not (lstNew.Tag=-1);
//  if not grpConfig.Enabled then grpConfig.Caption:='Select a file first';
  grpConfig.Enabled:=(lstNew.SelectedCount>0);
  if grpConfig.Tag=-1 then begin
    txtNew.Enabled:=False;
    butNewEdit.Enabled:=False;
    butNewEdit.Font.Color:=clInactiveCaption;
    butOldAdd.Enabled:=False;
    butOldAdd.Font.Color:=clInactiveCaption;
    butOldRemove.Enabled:=False;
    butOldRemove.Font.Color:=clInactiveCaption;
  end else begin
    txtNew.Enabled:=True;
    butNewEdit.Enabled:=True;
    butNewEdit.Font.Color:=clWindowText;
    butOldAdd.Enabled:=True;
    butOldAdd.Font.Color:=clWindowText;
//    butOldEdit.Enabled:=True;
    butOldRemove.Enabled:=True;
    butOldRemove.Font.Color:=clWindowText;
  end;
end;

procedure TfrmMain.ReloadNewTree;
var
  i: Integer;
  Node: PVirtualNode;
begin
  lstNew.BeginUpdate;
  lstNew.Clear;
  for i:=0 to PP.GetPatchCount - 1 do begin
    Node:=lstNew.AddChild(nil);
    PInteger(lstNew.GetNodeData(Node))^:=i;
  end;
  lstNew.EndUpdate;
end;

procedure TfrmMain.butNewEditClick(Sender: TObject);
var
  i: Integer;
begin
  OD.Options:=OD.Options-[ofAllowMultiSelect];
  OD.Title:='Select new version of file...';
  OD.FileName:=txtNew.Text;
  if OD.Execute then begin
    i:=grpConfig.Tag;
    PP.PatchFile(i).NewVersion:=OD.FileName;
    ReloadNewTree;
    lstNew.Selected[lstNew.GetFirstVisible]:=True;
  end;
end;

procedure TfrmMain.lstNewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i,j: Integer;
begin
  case lstNew.SelectedCount of
    0: Exit;
    1: begin
      if lstNew.Selected[Node] then begin
        i:=PInteger(lstNew.GetNodeData(Node))^;
        grpConfig.Caption:=ExtractFileName(PP.PatchFile(i).NewVersion);
        grpConfig.Tag:=i;
        txtNew.Text:=PP.PatchFile(i).NewVersion;
        lstOld.Clear;
        for j:=0 to PP.PatchFile(i).OldVersionCount - 1 do begin
          lstOld.Items.Add(PP.PatchFile(i).OldVersions[j]);
        end;
        SetConfigTextBoxes(PP.PatchFile(i).Config);
      end;
    end;
    else begin
      grpConfig.Tag:=-1;   //multiple files selected - only allow config changes
      txtNew.Text:='(multiple files selected)';
      lstOld.Clear;
    end;
  end;
  UpdateStates;
end;

procedure TfrmMain.butOldAddClick(Sender: TObject);
var
  i,j: Integer;
begin
  OD.Options:=OD.Options+[ofAllowMultiSelect];
  OD.Title:='Select old versions of '+grpConfig.Caption+'...';
  OD.FileName:='';
  if OD.Execute then begin
    i:=grpConfig.Tag;
    for j:=0 to OD.Files.Count - 1 do begin
      PP.PatchFile(i).AddOldVersion(OD.Files[j]);
      lstOld.Items.Add(OD.Files.Strings[j]);
    end;
  end;
end;

procedure TfrmMain.SelectInNewTree(PatchIndex: Integer);
var
  Node: PVirtualNode;
begin
  Node:=lstNew.GetFirstSelected;
  while Node<>nil do begin
    lstNew.Selected[Node]:=False;
    Node:=lstNew.GetNextSelected(Node);
  end;
  Node:=lstNew.GetFirst;
  while Node<>nil do begin
    if PInteger(lstNew.GetNodeData(Node))^=PatchIndex then begin
      lstNew.Selected[Node]:=True;
      lstNewChange(lstNew,Node);
      Exit;
    end;
    Node:=lstNew.GetNext(Node);
  end;
end;

procedure TfrmMain.butOldRemoveClick(Sender: TObject);
begin
  if lstOld.ItemIndex>=0 then begin
    PP.PatchFile(grpConfig.Tag).RemoveOldVersion(lstOld.ItemIndex);
    lstOld.Items.Delete(lstOld.ItemIndex);
  end;
end;

procedure TfrmMain.OpenAFile(FileName: String; AskSave: Boolean=True; PromptNew: Boolean=False);
var
  fs: TFileStream;
begin
  PP.Free; //confirm saving first?
  PP:=TPatchProject.Create;
  ReloadNewTree;
  if FileName<>'' then begin
      fs:=TFileStream.Create(FileName,fmOpenRead);
      try
        PP.LoadFromStream(fs);
      except
        on E: Exception do ShowMessage(E.Message);
      end;
      dskName:=FileName;
      ReloadNewTree;
      fs.Free;
  end else begin
    dskName:=UntitledFile;
    if PromptNew then butAddClick(Self);
  end;

  UpdateStates;
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  OpenAFile('',True,True);
end;

procedure TfrmMain.mnuOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    OpenAFile(dlgOpen.FileName,True);
    if lstNew.GetFirst <> nil then
      lstNew.Selected[lstNew.GetFirst]:=True;
  end;
end;

procedure TfrmMain.mnuSaveClick(Sender: TObject);
begin
  dskName:=DoSave(dskName,False);
  UpdateStates;
end;

procedure TfrmMain.mnuSaveasClick(Sender: TObject);
begin
  dskName:=DoSave(dskName,True);
  UpdateStates;
end;

function TfrmMain.DoSave(const FileName: String; const Prompt: Boolean): String;
var
  FN: String;
  fs: TFileStream;
begin
  DoSave:='';
  FN:=FileName;
  if Prompt or (CompareText(FileName,UntitledFile)=0) then begin
    if dlgSave.Execute then begin
      FN:=dlgSave.FileName;
      if ExtractFileExt(FN)='' then
        FN:=FN+'.vpj';
    end else begin
      DoSave:=FileName;
      Exit;
    end;
  end;
  //do actual saving to this file...
  fs:=TFileStream.Create(FN,fmCreate);
  PP.SaveToStream(fs);
  fs.Free;
  DoSave:=FN;
end;

procedure TfrmMain.mnuGenGoClick(Sender: TObject);
begin
  Self.Visible:=False;
  Cursor:=crHourGlass;
  PP.Generate;
  Cursor:=crDefault;
  Self.Visible:=True;
  SelectInNewTree(0);
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
var
  frmAbout: TfrmAbout;
begin
  frmAbout:=TfrmAbout.Create(Self);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmMain.mnuCreateEXEClick(Sender: TObject);
var
  fs: TFileStream;
  fr: TFileStream;
begin
  //first, select it on disk (where should the exe go?)
  if dlgSaveExe.FileName='' then dlgSaveExe.FileName:='VPatch.exe';
  if dlgSaveExe.Execute then begin
    fs:=nil;
    try
      fs:=TFileStream.Create(dlgSaveExe.FileName,fmCreate);
      fr:=nil;
      try
        fr:=TFileStream.Create(ExtractFilePath(Application.ExeName)+'vpatch.bin',fmOpenRead);
        fs.CopyFrom(fr,fr.Size);
      finally
        fr.Free;
      end;
      PP.WritePatches(fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmMain.mnuCreateDLLClick(Sender: TObject);
var
  fs: TFileStream;
  fr: TFileStream;
begin
  //first, select it on disk (where should the exe go?)
  if dlgSaveDLL.FileName='' then dlgSaveDLL.FileName:='VPatch.DLL';
  if dlgSaveDLL.Execute then begin
    fs:=nil;
    try
      fs:=TFileStream.Create(dlgSaveDLL.FileName,fmCreate);
      fr:=nil;
      try
        fr:=TFileStream.Create(ExtractFilePath(Application.ExeName)+'vpatchdll.bin',fmOpenRead);
        fs.CopyFrom(fr,fr.Size);
      finally
        fr.Free;
      end;
      PP.WritePatches(fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmMain.mnuCreatePATClick(Sender: TObject);
var
  fs: TFileStream;
begin
  //first, select it on disk (where should the exe go?)
  if dlgSavePAT.FileName='' then dlgSavePAT.FileName:='PatchData.pat';
  if dlgSavePAT.Execute then begin
    fs:=nil;
    try
      fs:=TFileStream.Create(dlgSavePAT.FileName,fmCreate);
      PP.WritePatches(fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TfrmMain.lstNewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  i: Integer;
begin
  i:=PInteger(lstNew.GetNodeData(Node))^;
  CellText:=ExtractFileName(PP.PatchFile(i).NewVersion);
end;

procedure TfrmMain.txtStartBlockSizeChange(Sender: TObject);
begin
  PP.PatchFile(grpConfig.Tag).Config:=CollectConfig;
end;

function TfrmMain.CollectConfig: String;
begin
  Result:=txtStartBlockSize.Caption;
end;

procedure TfrmMain.txtMinimumBlockSizeChange(Sender: TObject);
begin
  PP.PatchFile(grpConfig.Tag).Config:=CollectConfig;
end;

procedure TfrmMain.txtBlockDividerChange(Sender: TObject);
begin
  PP.PatchFile(grpConfig.Tag).Config:=CollectConfig;
end;

procedure TfrmMain.txtStepSizeChange(Sender: TObject);
begin
  PP.PatchFile(grpConfig.Tag).Config:=CollectConfig;
end;

procedure TfrmMain.SetConfigTextBoxes(Config: String);
var
  a,i: Integer;
begin
    a:=Pos(',',Config);
    if(a=0) then a:=Length(Config)+1;
    txtStartBlockSize.Caption:=Copy(Config,1,a-1);
    Config:=Copy(Config,a+1,Length(Config));

    a:=StrToInt(txtStartBlockSize.Caption);
    i:=-1;
    while not (a=0) do begin
      a:=a shr 1;
      Inc(i);
    end;
    tbBlockSize.Position := i;
end;

procedure TfrmMain.tbBlockSizeChange(Sender: TObject);
begin
  txtStartBlockSize.Caption:=IntToStr(1 shl tbBlockSize.Position);
  PP.PatchFile(grpConfig.Tag).Config:=CollectConfig;
end;

procedure TfrmMain.mnuClearcachedpatchesClick(Sender: TObject);
begin
  PP.ResetCache;
end;

procedure TfrmMain.chkOutputWaitClick(Sender: TObject);
begin
  WaitAfterGenerate:=chkOutputWait.Checked;
end;

procedure TfrmMain.mnuWebsiteClick(Sender: TObject);
begin
  OpenLink('http://www.tibed.net/vpatch');
end;

procedure TfrmMain.Readme1Click(Sender: TObject);
begin
  OpenLink('VPatch.htm');
end;

procedure TfrmMain.ReadmeincludedwithNSIS1Click(Sender: TObject);
begin
  OpenLink('Readme.html');
end;

procedure TfrmMain.chkOptimalClick(Sender: TObject);
begin
  OptimalPatches:=chkOptimal.Checked;
end;

initialization
  PP:=TPatchProject.Create;
end.
