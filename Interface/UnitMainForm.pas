unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  JvComponentBase, JvSearchFiles, UnitFileCompare, UnitFileCopyPair, UnitProjectFile,
  Vcl.Mask, JvExMask, JvToolEdit, IdBaseComponent, IdThreadComponent;

type
  TMainForm = class(TForm)
    JobsList: TListView;
    SearchFile: TJvSearchFiles;
    ProgressTimer: TTimer;
    RightPanel: TPanel;
    LeftPanel: TPanel;
    TopPanel: TPanel;
    BottomPanel: TPanel;
    LogList: TListBox;
    ProgressBar1: TProgressBar;
    StateLabel: TLabel;
    RunJobsBtn: TButton;
    ProjectNameLabel: TLabel;
    OperationThread: TIdThreadComponent;
    AddNewProjectBtn: TButton;
    StopBtn: TButton;
    EditProjectBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure RunJobsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchFileFindFile(Sender: TObject; const AName: string);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OperationThreadRun(Sender: TIdThreadComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure AddNewProjectBtnClick(Sender: TObject);
    procedure EditProjectBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    { Private declarations }
    FFiles: TStringList;
    FFileCompare: TFileComperator;
    FTotalCMDCount: integer;
    FInfo: string;
    FStateMsg: string;
    FStop: Boolean;

    procedure LoadProjects;
    procedure Log(const Str: string);

    procedure DisableUI;
    procedure EnableUI;
  public
    { Public declarations }
    FProjects: TProjectFiles;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses UnitProjectSettingsForm;

procedure TMainForm.AddNewProjectBtnClick(Sender: TObject);
begin
  ProjectSettingsForm.FProjectFile := '';
  ProjectSettingsForm.Show;
  Self.Enabled := False;
end;

procedure TMainForm.DisableUI;
begin
  JobsList.Enabled := False;
  RunJobsBtn.Enabled := False;
  AddNewProjectBtn.Enabled  := False;
  EditProjectBtn.Enabled := False;
  StopBtn.Enabled := True;
end;

procedure TMainForm.EditProjectBtnClick(Sender: TObject);
begin
  if JobsList.ItemIndex > -1 then
  begin
    ProjectSettingsForm.FProjectFile := FProjects[JobsList.ItemIndex].IniFilePath;
    ProjectSettingsForm.Show;
    Self.Enabled := False;
  end;
end;

procedure TMainForm.EnableUI;
begin
  JobsList.Enabled := True;
  RunJobsBtn.Enabled := True;
  AddNewProjectBtn.Enabled  := True;
  EditProjectBtn.Enabled := True;
  StopBtn.Enabled := False;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not OperationThread.Terminated then
  begin
    OperationThread.TerminateAndWaitFor;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  SearchFile.RecurseDepth := MaxInt;
  FFiles := TStringList.Create;
  FFileCompare := TFileComperator.Create;
  FProjects := TProjectFiles.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  FFiles.Free;
  FFileCompare.free;
  for I := 0 to FProjects.Count-1 do
  begin
    FProjects[i].Free;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  JobsList.Columns[1].Width := (JobsList.ClientWidth - JobsList.Columns[0].Width - 20) div 2;
  JobsList.Columns[2].Width := (JobsList.ClientWidth - JobsList.Columns[0].Width - 20) div 2;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadProjects;
end;

procedure TMainForm.LoadProjects;
var
  LSR: TSearchRec;
  LProjectFile: TProjectFile;
  LItem: TListItem;
begin
  if DirectoryExists(ExtractFileDir(Application.ExeName) + '\jobs\') then
  begin
    if FindFirst(ExtractFileDir(Application.ExeName) + '\jobs\' + '*.ini', faArchive, LSR) = 0 then
    begin
      repeat
        LProjectFile := TProjectFile.Create(ExtractFileDir(Application.ExeName) + '\jobs\' + LSR.Name);
        FProjects.Add(LProjectFile);

        LItem := JobsList.Items.Add;
        LItem.Caption := LProjectFile.ProjectName;
        LItem.SubItems.Add(LProjectFile.SourceFolder);
        LItem.SubItems.Add(LProjectFile.DestFolder);
        LItem.Checked := True;
      until FindNext(LSR) <> 0;
      FindClose(LSR);
    end;
  end
  else
  begin
    ForceDirectories(ExtractFileDir(Application.ExeName) + '\jobs')
  end;
end;

procedure TMainForm.Log(const Str: string);
begin
  LogList.Items.Add('[' + DateTimeToStr(Now) + '] ' + Str);
  LogList.ItemIndex := LogList.Items.Count-1;
end;

procedure TMainForm.OperationThreadRun(Sender: TIdThreadComponent);
var
  I, J: Integer;
  LSourceFile: string;
  LDestFile: string;
  LCounter1, LCounter2: integer;
  LCopy: Boolean;
  LCopiedCount: integer;
  LFileCopyPair: TFileCopyPair;
  LFileCopyPairs: TFileCopyPairs;
  LDateTime: TDateTime;
  LError: integer;
begin
  LDateTime := Now;
  ProgressTimer.Enabled := True;
  try
    for J := 0 to FProjects.Count-1 do
    begin
      Application.ProcessMessages;
      if FStop then
      begin
        Break;
      end;
      if JobsList.Items[J].Checked then
      begin
        ProjectNameLabel.Caption := 'Current Project: ' + FProjects[J].ProjectName;
        Log('Starting ' + FProjects[J].ProjectName);
        ProgressBar1.Position := 0;
        FFiles.Clear;
        if DirectoryExists(FProjects[J].SourceFolder) then
        begin
          SearchFile.RootDirectory := FProjects[J].SourceFolder;
          SearchFile.Search;
          Log('Found ' + FFiles.Count.ToString() + ' files');
          LCopiedCount := 0;
          FTotalCMDCount := 0;
          LFileCopyPairs := TFileCopyPairs.Create;
          try
            for I := 0 to FFiles.Count-1 do
            begin
              Application.ProcessMessages;
              if FStop then
              begin
                Break;
              end;
              LSourceFile := FFiles[i].Trim;
              FStateMsg := 'State: Looking for changes ' + i.ToString + '/' + FFiles.Count.ToString + ' (' + LSourceFile + ')';

              LCopy := false;
              LDestFile := LSourceFile.Replace(FProjects[J].SourceFolder, FProjects[J].DestFolder);
              if not FileExists(LDestFile) then
              begin
                LCopy := True;
              end
              else
              begin
                LCopy := not FFileCompare.CompareFiles(LError, LSourceFile, LDestFile, 16384);
              end;

              if LCopy then
              begin
                try
                  if not DirectoryExists(ExtractFileDir(LDestFile)) then
                  begin
                    ForceDirectories(ExtractFileDir(LDestFile));
                  end;
                  LFileCopyPair.SourceFile := LSourceFile;
                  LFileCopyPair.DestFile := LDestFile;
                  LFileCopyPairs.Add(LFileCopyPair);
                except on E: Exception do
                  begin
                    LogList.Items.Add(E.Message)
                  end;
                end;
              end;
            end;

            Log('Files to be copied: ' + LFileCopyPairs.Count.ToString());
            ProgressBar1.Max := LFileCopyPairs.Count;
            for I := 0 to LFileCopyPairs.Count-1 do
            begin
              Application.ProcessMessages;

              if FStop then
              begin
                Break;
              end;

              ProgressBar1.Position := i+1;
              if (i mod 10) = 0 then
              begin
                FStateMsg := 'State: Copying ' + i.ToString + '/' + LFileCopyPairs.Count.ToString + ' (' + LFileCopyPairs[i].DestFile + ')';
              end;
              try
                if not CopyFile(PWideChar(LFileCopyPairs[i].SourceFile), PWideChar(LFileCopyPairs[i].DestFile), false) then
                begin
                  RaiseLastOSError;
                end;
              except on E: Exception do
                Log('Error: ' + E.Message + ' ' + LFileCopyPairs[i].SourceFile);
              end;
            end;
          finally
            LFileCopyPairs.Free;
          end;
        end;
      end
      else
      begin
        Log('Skipping ' + FProjects[J].ProjectName);
      end;
    end;
  finally
    ProgressTimer.Enabled := False;
    StateLabel.Caption := FStateMsg;
    Log('Took ' + FormatDateTime('hh:nn:ss.zzz', Now - LDateTime));
    EnableUI;
    OperationThread.Terminate;
  end;
end;

procedure TMainForm.ProgressTimerTimer(Sender: TObject);
begin
  StateLabel.Caption := FStateMsg;
end;

procedure TMainForm.RunJobsBtnClick(Sender: TObject);
begin
  FStop := False;
  DisableUI;
  OperationThread.Start;
end;

procedure TMainForm.SearchFileFindFile(Sender: TObject; const AName: string);
begin
  FFiles.Add(AName);
  FStateMsg := 'State: Found ' + FFiles.Count.ToString + ' files';
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FStop := True;
end;

end.
