program VPatchGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  PatchClasses in 'PatchClasses.pas',
  VDSP_CRC in '..\VDSP_CRC.pas',
  DLLWrapper in 'DLLWrapper.pas',
  AboutForm in 'AboutForm.pas' {frmAbout},
  PatchGenerator in '..\PatchGenerator.pas',
  TreeCode in '..\TreeCode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
