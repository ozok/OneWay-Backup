program OneWayBackup;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  UnitMainForm in 'Interface\UnitMainForm.pas' {MainForm} ,
  UnitFileCompare in 'File\UnitFileCompare.pas',
  UnitFileCopyPair in 'File\UnitFileCopyPair.pas',
  UnitProjectFile in 'Project\UnitProjectFile.pas',
  UnitProjectSettingsForm
    in 'Interface\UnitProjectSettingsForm.pas' {ProjectSettingsForm} ,
  UnitFileCompareThread in 'File\UnitFileCompareThread.pas',
  UnitFolderCreatePair in 'File\UnitFolderCreatePair.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  // ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.Run;

end.
