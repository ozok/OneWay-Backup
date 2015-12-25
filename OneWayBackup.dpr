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
  UnitFolderCreatePair in 'File\UnitFolderCreatePair.pas',
  Vcl.Themes,
  Vcl.Styles,
  UnitEmailConfig in 'Interface\UnitEmailConfig.pas' {EmailConfForm} ,
  UnitLog in 'Interface\UnitLog.pas' {LogForm} ,
  UnitAbout in 'Interface\UnitAbout.pas' {AboutForm} ,
  UnitLogs in 'Interface\UnitLogs.pas' {LogsForm} ,
  UnitLogItems in 'LogItems\UnitLogItems.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  // ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProjectSettingsForm, ProjectSettingsForm);
  Application.CreateForm(TEmailConfForm, EmailConfForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TLogsForm, LogsForm);
  Application.Run;

end.
