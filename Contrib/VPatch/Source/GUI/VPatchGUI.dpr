program VPatchGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  PatchClasses in 'PatchClasses.pas',
  VDSP_CRC in '..\GenPat\VDSP_CRC.pas',
  DLLWrapper in 'DLLWrapper.pas',
  AboutForm in 'AboutForm.pas' {frmAbout},
  PatchGenerator in '..\GenPat\PatchGenerator.pas',
  TreeCode in '..\GenPat\TreeCode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
