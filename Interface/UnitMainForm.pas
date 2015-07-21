unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  JvComponentBase, JvSearchFiles, UnitFileCompare, UnitFileCopyPair, UnitProjectFile,
  Vcl.Mask, JvExMask, JvToolEdit, IdBaseComponent, IdThreadComponent, CommCtrl;

type
  TMainForm = class(TForm)
    JobsList: TListView;
    SearchSourceFiles: TJvSearchFiles;
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
    AddNewProjectBtn: TButton;
    StopBtn: TButton;
    EditProjectBtn: TButton;
    OperationThread: TIdThreadComponent;
    SearchDestFiles: TJvSearchFiles;
    procedure FormCreate(Sender: TObject);
    procedure RunJobsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchSourceFilesFindFile(Sender: TObject; const AName: string);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OperationThreadRun(Sender: TIdThreadComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure AddNewProjectBtnClick(Sender: TObject);
    procedure EditProjectBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure JobsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FFiles: TStringList;
    FFileCompare: TFileComperator;
    FTotalCMDCount: integer;
    FInfo: string;
    FStateMsg: string;
    FProgress: integer;
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
    ProjectSettingsForm.FItemIndex := JobsList.ItemIndex;
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
  StateLabel.Caption := '';
  ProjectNameLabel.Caption := '';
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
  SearchSourceFiles.RecurseDepth := MaxInt;
  SearchDestFiles.RecurseDepth := MaxInt;
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

procedure TMainForm.JobsListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LListItem: TListItem;
  LHitTest: THitTests;
begin
  if JobsList.Items.Count > 0 then
  begin
    LListItem := JobsList.GetItemAt(x, y);
    if LListItem <> nil then
    begin
      if LListItem.Selected = false then
        LListItem.Selected := true;
    end
    else
      exit;
    LHitTest := JobsList.GetHitTestInfoAt(x, y);
    if LHitTest = [htOnStateIcon] then
    begin
      FProjects[LListItem.Index].Active := JobsList.Items[LListItem.Index].Checked;
      FProjects[LListItem.Index].Save;
    end;
  end;
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
        LItem.Checked := LProjectFile.Active;
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
  LFilesToDelete: TStringList;
begin
  LDateTime := Now;
  ProgressTimer.Enabled := True;
  try
    JobsList.ItemIndex := -1;
    for J := 0 to FProjects.Count-1 do
    begin
      Application.ProcessMessages;
      if FStop then
      begin
        Break;
      end;
      if JobsList.Items[J].Checked then
      begin
        // loop through all available projects
        JobsList.ItemIndex := J;
        if J > 0 then
        begin
          JobsList.Items[J-1].Selected := False;
        end;

        JobsList.Items[J].Selected := True;
        JobsList.Items[J].MakeVisible(False);

        ProjectNameLabel.Caption := FProjects[J].ProjectName;
        Log('Starting ' + FProjects[J].ProjectName);
        FProgress := 0;
        FFiles.Clear;
        if DirectoryExists(FProjects[J].SourceFolder) then
        begin
          SearchSourceFiles.RootDirectory := FProjects[J].SourceFolder;
          SearchSourceFiles.Search;
          Log('Found ' + FFiles.Count.ToString() + ' files');
          LCopiedCount := 0;
          FTotalCMDCount := 0;
          LFileCopyPairs := TFileCopyPairs.Create;
          try
            ProgressBar1.Max := FFiles.Count;
            for I := 0 to FFiles.Count-1 do
            begin
              FProgress := i+1;
              if FStop then
              begin
                Break;
              end;
              LSourceFile := FFiles[i].Trim;
              FStateMsg := 'Looking for changes ' + i.ToString + '/' + FFiles.Count.ToString + ' (' + LSourceFile + ')';

              LCopy := false;
              LDestFile := LSourceFile.Replace(FProjects[J].SourceFolder, FProjects[J].DestFolder);
              if not FileExists(LDestFile) then
              begin
                LCopy := True;
              end
              else
              begin
                LCopy := not FFileCompare.CompareFiles(LSourceFile, LDestFile, 131072*1024);
              end;

              if LCopy then
              begin
                if not DirectoryExists(ExtractFileDir(LDestFile)) then
                begin
                  ForceDirectories(ExtractFileDir(LDestFile));
                end;
                LFileCopyPair.SourceFile := LSourceFile;
                LFileCopyPair.DestFile := LDestFile;
                LFileCopyPairs.Add(LFileCopyPair);
              end;
            end;

            if LFileCopyPairs.Count > 0 then
            begin
              Log('Files to be copied: ' + LFileCopyPairs.Count.ToString());
            end
            else
            begin
              Log('No changes found.');
            end;
            ProgressBar1.Max := LFileCopyPairs.Count;
            for I := 0 to LFileCopyPairs.Count-1 do
            begin
              Application.ProcessMessages;

              if FStop then
              begin
                Break;
              end;

              FProgress := i+1;
              if (i mod 10) = 0 then
              begin
                FStateMsg := 'Copying ' + i.ToString + '/' + LFileCopyPairs.Count.ToString + ' (' + LFileCopyPairs[i].DestFile + ')';
              end;
              try
                if not CopyFile(PWideChar(LFileCopyPairs[i].SourceFile), PWideChar(LFileCopyPairs[i].DestFile), false) then
                begin
                  RaiseLastOSError;
                end;
              except on E: Exception do
                Log('Copy error: ' + E.Message + ' ' + LFileCopyPairs[i].SourceFile);
              end;
            end;

            if not FStop then
            begin
              if FProjects[J].DeleteFromDest then
              begin
                FFiles.Clear;
                Log('Searching destination for files to be deleted.');
                SearchDestFiles.RootDirectory :=  FProjects[J].DestFolder;
                SearchDestFiles.Search;

                LFilesToDelete := TStringList.Create();
                try
                  ProgressBar1.Max := FFiles.Count;
                  for I := 0 to FFiles.Count-1 do
                  begin
                    if FStop then
                    begin
                      Break;
                    end;
                    FProgress := I + 1;
                    LDestFile := FFiles[i].Trim;
                    FStateMsg := 'Searching file in source folder ' + i.ToString + '/' + FFiles.Count.ToString + ' (' + LDestFile + ')';

                    LSourceFile := LDestFile.Replace(FProjects[J].DestFolder, FProjects[J].SourceFolder);
                    if not FileExists(LSourceFile) then
                    begin
                      LFilesToDelete.Add(LDestFile);
                    end;
                  end;
                finally
                  if LFilesToDelete.Count > 0 then
                  begin
                    Log(LFilesToDelete.Count.ToString() + ' files from destination will be deleted.');
                    ProgressBar1.Max := LFilesToDelete.Count;
                    for I := 0 to LFilesToDelete.Count-1 do
                    begin
                      FProgress := I + 1;
                      try
                        DeleteFile(LFilesToDelete[i]);
                      Except on E: Exception do
                        Log('Delete error: ' + E.Message + ' ' + LFilesToDelete[i]);
                      end;
                    end;
                  end;
                  LFilesToDelete.Free;
                end;
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
    EnableUI;
    if FStop then
    begin
      Log('Stopped by user.');
    end
    else
    begin
      Log('Took ' + FormatDateTime('hh:nn:ss.zzz', Now - LDateTime));
    end;
    OperationThread.Terminate;
  end;
end;

procedure TMainForm.ProgressTimerTimer(Sender: TObject);
begin
  StateLabel.Caption := FStateMsg;
  ProgressBar1.Position := FProgress;
end;

procedure TMainForm.RunJobsBtnClick(Sender: TObject);
begin
  FStop := False;
  DisableUI;
  LogList.Items.Clear;
  OperationThread.Start;
end;

procedure TMainForm.SearchSourceFilesFindFile(Sender: TObject; const AName: string);
begin
  FFiles.Add(AName);
  FStateMsg := 'Found ' + FFiles.Count.ToString + ' files';
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  FStop := True;
  if SearchSourceFiles.Searching then
  begin
    SearchSourceFiles.Abort;
  end;
  if SearchDestFiles.Searching then
  begin
    SearchDestFiles.Abort;
  end;
  FFileCompare.Stop := True;
end;

end.
