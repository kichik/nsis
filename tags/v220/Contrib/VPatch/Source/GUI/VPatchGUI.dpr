program VPatchGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  PatchClasses in 'PatchClasses.pas',
  DLLWrapper in 'DLLWrapper.pas',
  AboutForm in 'AboutForm.pas' {frmAbout},
  VDSP_CRC in 'vdsp_crc.pas',
  OSUtil in 'OSUtil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VPatch GUI';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
