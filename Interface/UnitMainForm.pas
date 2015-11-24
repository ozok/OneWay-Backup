unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, 
  Vcl.ExtCtrls, Vcl.StdCtrls, JvComponentBase, JvSearchFiles, UnitFileCompare, 
  UnitFileCopyPair, UnitProjectFile, Vcl.Mask, JvExMask, JvToolEdit, IdBaseComponent, 
  IdThreadComponent, CommCtrl, System.Win.TaskbarCore, Vcl.Taskbar, JvExComCtrls, 
  JvComCtrls, Generics.Collections, Vcl.Menus, ShellAPI, UnitFolderCreatePair, 
  IdMessage, IdComponent, IdTCPConnection, IdTCPClient, 
  IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase, IdSMTP, JvThreadTimer,
  sSkinProvider, sSkinManager, sPageControl, acProgressBar, sComboBox, sButton,
  sLabel, sListView, sPanel, sGauge, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IniFiles, System.ImageList,
  Vcl.ImgList, acAlphaImageList, Vcl.Buttons, sBitBtn, sCheckBox;

type
  TLogItem = record
    LogDate: TDateTime;
    LogStr: string;
  end;
  TLogItems = TList<TLogItem>;

type
  TMainForm = class(TForm)
    JobsList: TsListView;
    SearchSourceFiles: TJvSearchFiles;
    RightPanel: TsPanel;
    LeftPanel: TsPanel;
    TopPanel: TsPanel;
    BottomPanel: TsPanel;
    StateLabel: TsLabel;
    RunJobsBtn: TsButton;
    ProjectNameLabel: TsLabel;
    AddNewProjectBtn: TsButton;
    StopBtn: TsButton;
    EditProjectBtn: TsButton;
    OperationThread: TIdThreadComponent;
    SearchDestFiles: TJvSearchFiles;
    LogsPages: TsPageControl;
    TabSheet1: TsTabSheet;
    TabSheet2: TsTabSheet;
    Taskbar1: TTaskbar;
    TabSheet3: TsTabSheet;
    LogList: TsListView;
    ErrorLog: TsListView;
    FullLogList: TsListView;
    JobListMenu: TPopupMenu;
    O1: TMenuItem;
    O2: TMenuItem;
    ActivatePanel: TsPanel;
    SelectAllLabel: TsLabel;
    SelectNoneLabel: TsLabel;
    SelectReverseLabel: TsLabel;
    IdSMTP1: TIdSMTP;
    IdMessage1: TIdMessage;
    ChangesLabel: TsLabel;
    SpeedLabel: TsLabel;
    SpeedTimer: TJvThreadTimer;
    TimeLabel: TsLabel;
    PassedTimeTimer: TJvThreadTimer;
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    ConfEmailBtn: TsButton;
    sAlphaImageList1: TsAlphaImageList;
    ProgressTimer: TTimer;
    DeleteBtn: TsBitBtn;
    PreviewBtn: TsButton;
    ProgressBar: TsProgressBar;
    PercentageLabel: TsLabel;
    SendEmailBtn: TsCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RunJobsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchSourceFilesFindFile(Sender: TObject; const AName: string);
    procedure FormShow(Sender: TObject);
    procedure OperationThreadRun(Sender: TIdThreadComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure AddNewProjectBtnClick(Sender: TObject);
    procedure EditProjectBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure JobsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SearchSourceFilesFindDirectory(Sender: TObject;
      const AName: string);
    procedure OperationThreadException(Sender: TIdThreadComponent;
      AException: Exception);
    procedure SearchDestFilesFindDirectory(Sender: TObject;
      const AName: string);
    procedure LogListData(Sender: TObject; Item: TListItem);
    procedure ErrorLogData(Sender: TObject; Item: TListItem);
    procedure FullLogListData(Sender: TObject; Item: TListItem);
    procedure O1Click(Sender: TObject);
    procedure O2Click(Sender: TObject);
    procedure SelectAllLabelClick(Sender: TObject);
    procedure SelectNoneLabelClick(Sender: TObject);
    procedure SelectReverseLabelClick(Sender: TObject);
    procedure SpeedTimerTimer(Sender: TObject);
    procedure PassedTimeTimerTimer(Sender: TObject);
    procedure ConfEmailBtnClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure PreviewBtnClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
  private
    { Private declarations }
    FFiles: TStringList;
    FFolders: TStringList;
    FFileCompare: TFileComperator;
    FTotalCMDCount: integer;
    FStateMsg: string;
    FProgress: integer;
    FStop: Boolean;
    FLogLineToAdd: string;
    FMaxProgress: integer;
    FItemIndex: integer;
    FCurrentProjectName: string;
    FGeneralLogItems: TLogItems;
    FErrorLogItems: TLogItems;
    FAll, FRun, FExit, FShutDown: Boolean;
    FIgnoreTypeString: string;
    FChangeCount: integer;
    FTimeCounter: int64;
    FTotalTimeCounter: integer;
    FCompareMethodId: integer;
    FPreview: Boolean;
    FSendEmail: Boolean;
    FAppDataFolder: string;

    procedure Log(const Str: string);
    procedure LogError(const Str: string);
    procedure LogFull(const Str: string);
    procedure AddToLog();
    procedure AddToErrorLog();
    procedure AddToFullLog();
    procedure CloseQueue();
    procedure ShutDown();
    procedure UpdateChangeCount();
    procedure UpdateProgress();
    procedure UpdateState();
    procedure StopPassedTimeTimer();
    procedure StartSpeedTimer();
    procedure StopSpeedTimer();
    procedure StartProgressTimer();
    procedure StopProgressTimer();
    procedure ShowPreviewResults();

    procedure UpdateMaxProgres();
    procedure JumpToItem();
    procedure UpdateProjectName;

    procedure DisableUI;
    procedure EnableUI;

    function CheckIfFileCanBeAdded(const FilePath: string; const ProjectIgnoreFileTypes: string):Boolean;
  public
    { Public declarations }
    FProjects: TProjectFiles;
    FFullLogItems: TLogItems;
    LastLogFilePath: string;

    procedure SaveProjects;
    procedure LoadProjects;

    function CompareMethodToStr(const MethodId: integer):string;
  end;

var
  MainForm: TMainForm;

const
  PROGRAM_TITLE = 'OneWay Backup - Beta';

implementation

{$R *.dfm}

uses UnitProjectSettingsForm, UnitEmailConfig, UnitLog;

procedure TMainForm.AddNewProjectBtnClick(Sender: TObject);
begin
  ProjectSettingsForm.FItemIndex := -1;
  ProjectSettingsForm.DestDirEdit.Clear;
  ProjectSettingsForm.SourceDirEdit.Clear;
  ProjectSettingsForm.ProjectNameEdit.Clear;
  ProjectSettingsForm.BufferEdit.Text := '8192';
  ProjectSettingsForm.DeleteFromDestBtn.Checked := False;
  ProjectSettingsForm.Show;
  Self.Enabled := False;
end;

procedure TMainForm.AddToErrorLog;
begin   
  try
    LogError(FLogLineToAdd);
  except on E: Exception do
  end;
end;

procedure TMainForm.AddToFullLog;
begin   
  try
    LogFull(FLogLineToAdd);
  except on E: Exception do
  end;
end;

procedure TMainForm.AddToLog;
begin
  try
    Log(FLogLineToAdd);
  except on E: Exception do
  end;
end;

function TMainForm.CheckIfFileCanBeAdded(const FilePath,
  ProjectIgnoreFileTypes: string): Boolean;
var
  LSplitList: TStringList;
  I: Integer;
  LFileExt: string;
begin
  Result := True;

  LSplitList := TStringList.Create;
  try
    LFileExt := ExtractFileExt(FilePath).ToLower;

    LSplitList.StrictDelimiter := True;
    LSplitList.Delimiter := ';';
    LSplitList.DelimitedText := ProjectIgnoreFileTypes;
    if LSplitList.Count > 0 then
    begin
      for I := 0 to LSplitList.Count-1 do
      begin
        if LFileExt = LSplitList[i].ToLower then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  finally
    LSplitList.Free;
  end;
end;

procedure TMainForm.CloseQueue;
begin
  if FRun and FExit then
  begin
    Self.Close;
  end;
end;

function TMainForm.CompareMethodToStr(const MethodId: integer): string;
begin
  case MethodId of
    0:
      Result := 'Full file search';
    1:
      Result := 'Compare MD5';
    2:
      Result := 'Compare sizes';
  end;
end;

procedure TMainForm.ConfEmailBtnClick(Sender: TObject);
begin
  Self.Enabled := False;
  EmailConfForm.Show;
end;

procedure TMainForm.DeleteBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := JobsList.Items.Count-1 downto 0 do
  begin
    if JobsList.Items[i].Selected then
    begin
      FProjects.Delete(i);
      JobsList.Items.Delete(i);
    end;
  end;
  SaveProjects;
  FProjects.Clear;
  JobsList.Items.Clear;
  LoadProjects;
end;

procedure TMainForm.DisableUI;
begin
  JobsList.Enabled := False;
  RunJobsBtn.Enabled := False;
  AddNewProjectBtn.Enabled  := False;
  EditProjectBtn.Enabled := False;
  StopBtn.Enabled := True;
  ActivatePanel.Enabled := False;
  ConfEmailBtn.Enabled := False;
  DeleteBtn.Enabled := False;
  PreviewBtn.Enabled := False;
end;

procedure TMainForm.EditProjectBtnClick(Sender: TObject);
begin
  if JobsList.ItemIndex > -1 then
  begin
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
  ActivatePanel.Enabled := True;
  ConfEmailBtn.Enabled := True;
  StateLabel.Caption := '';
  ProjectNameLabel.Caption := '';
  ChangesLabel.Caption := '';
  SpeedLabel.Caption := '';
  TimeLabel.Caption := '';
  ProgressBar.Position := 0;
  Taskbar1.ProgressValue := 0;
  PercentageLabel.Caption := '0%';
  Self.Caption := PROGRAM_TITLE;
  DeleteBtn.Enabled := True;
  PreviewBtn.Enabled := True;
end;

procedure TMainForm.ErrorLogData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FErrorLogItems.Count then
  begin
    Item.Caption := DateTimeToStr(FErrorLogItems[Item.Index].LogDate);
    Item.SubItems.Add(FErrorLogItems[Item.Index].LogStr);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
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
  if not OperationThread.Terminated then
  begin
    OperationThread.Terminate;
    while not OperationThread.Terminated do
    begin
      OperationThread.Terminate;
    end;
  end;
  SaveProjects;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SearchSourceFiles.RecurseDepth := MaxInt;
  SearchDestFiles.RecurseDepth := MaxInt;
  FFiles := TStringList.Create;
  FFolders := TStringList.Create;
  FFileCompare := TFileComperator.Create;
  FProjects := TProjectFiles.Create;
  FFullLogItems := TLogItems.Create;
  FGeneralLogItems := TLogItems.Create;
  FErrorLogItems := TLogItems.Create;
  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '\logs\') then
  begin
    ForceDirectories(ExtractFileDir(Application.ExeName) + '\logs\')
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  FFiles.Free;
  FFolders.Free;
  FFileCompare.free;
  for I := 0 to FProjects.Count-1 do
  begin
    FProjects[i].Free;
  end;
  FFullLogItems.Free;
  FGeneralLogItems.Free;
  FErrorLogItems.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  JobsList.Columns[1].Width := (JobsList.ClientWidth - JobsList.Columns[0].Width - JobsList.Columns[3].Width - JobsList.Columns[4].Width - JobsList.Columns[5].Width - 20) div 2;
  JobsList.Columns[2].Width := (JobsList.ClientWidth - JobsList.Columns[0].Width - JobsList.Columns[3].Width - JobsList.Columns[4].Width - JobsList.Columns[5].Width - 20) div 2;

  ErrorLog.Columns[1].Width := ErrorLog.ClientWidth - 20 - ErrorLog.Columns[0].Width;
  FullLogList.Columns[1].Width := FullLogList.ClientWidth - 20 - FullLogList.Columns[0].Width;
  LogList.Columns[1].Width := LogList.ClientWidth - 20 - LogList.Columns[0].Width;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  I: Integer;
  LParamStr: string;
begin
  PercentageLabel.Parent := ProgressBar;
  PercentageLabel.AutoSize := False;
  PercentageLabel.Transparent := True;
  PercentageLabel.Top :=  0;
  PercentageLabel.Left :=  0;
  PercentageLabel.Width := ProgressBar.ClientWidth;
  PercentageLabel.Height := ProgressBar.ClientHeight;
  PercentageLabel.Alignment := taCenter;
  PercentageLabel.Layout := tlCenter;

  Self.Caption := PROGRAM_TITLE;
  LoadProjects;

  FAll := False;
  FRun := False;
  FExit := False;
  FShutDown := False;
  FCompareMethodId := -1;
  FSendEmail := False;

  for I := 1 to ParamCount do
  begin
    LParamStr := LowerCase(ParamStr(i));

    if LParamStr = '/all' then
    begin
      FAll := True;
    end
    else if LParamStr = '/run' then
    begin
      FRun := True;
    end
    else if LParamStr = '/exit' then
    begin
      FExit := True;
    end
    else if LParamStr = '/shutdown' then
    begin
      FShutDown := True;
    end
    else if LParamStr = '/sendmail' then
    begin
      FSendEmail := True;
    end
    else if LParamStr = '/method0' then
    begin
      FCompareMethodId := 0;
    end
    else if LParamStr = '/method1' then
    begin
      FCompareMethodId := 1;
    end
    else if LParamStr = '/method2' then
    begin
      FCompareMethodId := 2;
    end;
  end;

  if FRun then
  begin
    RunJobsBtnClick(Self);
  end;
end;

procedure TMainForm.FullLogListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FFullLogItems.Count then
  begin
    Item.Caption := DateTimeToStr(FFullLogItems[Item.Index].LogDate);
    Item.SubItems.Add(FFullLogItems[Item.Index].LogStr);
  end;
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
      SaveProjects;
    end;
  end;
end;

procedure TMainForm.JumpToItem;
begin
  JobsList.ItemIndex := FItemIndex;
  if FItemIndex > 0 then
  begin
    JobsList.Items[FItemIndex-1].Selected := False;
  end;

  JobsList.Items[FItemIndex].Selected := True;
  JobsList.Items[FItemIndex].MakeVisible(False);
end;

procedure TMainForm.LoadProjects;
var
  LProject: TProjectFile;
  LItem: TListItem;
  LProjectFile: TStringList;
  I: Integer;
  LLine: string;
  LSplitList: TStringList;
begin
  if FileExists(ExtractFileDir(Application.ExeName) + '\data.dat') then
  begin
    LProjectFile := TStringList.Create;
    try
      LProjectFile.LoadFromFile(ExtractFileDir(Application.ExeName) + '\data.dat', TEncoding.UTF8);

      LSplitList := TStringList.Create;
      try
        LSplitList.StrictDelimiter := True;
        LSplitList.Delimiter := '|';

        for I := 0 to LProjectFile.Count-1 do
        begin
          LLine := LProjectFile[i].Trim;
          LSplitList.DelimitedText := LLine;

          if LSplitList.Count = 8 then
          begin
            LProject := TProjectFile.Create();

            LProject.SourceFolder := LSplitList[0];
            LProject.DestFolder := LSplitList[1];
            LProject.ProjectName := LSplitList[2];
            if LSplitList[3] = 'True' then LProject.Active := True Else LProject.Active := False;
            if LSplitList[4] = 'True' then LProject.DeleteFromDest := True Else LProject.DeleteFromDest := False;
            LProject.BufferSize := LSplitList[5].ToInteger();
            LProject.IgnoredFileTypes := LSplitList[6];
            LProject.CompareMethod := LSplitList[7].ToInteger();
            FProjects.Add(LProject);

            LItem := JobsList.Items.Add;
            LItem.Caption := LProject.ProjectName;
            LItem.SubItems.Add(LProject.SourceFolder);
            LItem.SubItems.Add(LProject.DestFolder);
            LItem.SubItems.Add(LProject.BufferSize.ToString());
            LItem.SubItems.Add(LProject.IgnoredFileTypes);
            LItem.SubItems.Add(CompareMethodToStr(LProject.CompareMethod));
            LItem.Checked := LProject.Active;
          end;
        end;
      finally
        LSplitList.Free;
      end;
    finally
      LProjectFile.Free;
    end;
  end;
end;

procedure TMainForm.Log(const Str: string);
var
  LLogItem: TLogItem;
begin
  LLogItem.LogDate := Now;
  LLogItem.LogStr := Str;

  FFullLogItems.Add(LLogItem);
  FGeneralLogItems.Add(LLogItem);

  FullLogList.Items.Count := FFullLogItems.Count;
  FullLogList.ItemIndex := FullLogList.Items.Count-1;
  LogList.Items.Count := FGeneralLogItems.Count;
  LogList.ItemIndex := FGeneralLogItems.Count-1;

  LogList.Items[LogList.Items.Count-1].MakeVisible(False);
  FullLogList.Items[FullLogList.Items.Count-1].MakeVisible(False);
end;

procedure TMainForm.LogBtnClick(Sender: TObject);
begin
  LogForm.LogList.Items.Count := FFullLogItems.Count;
  LogForm.Show;
end;

procedure TMainForm.LogError(const Str: string);
var
  LLogItem: TLogItem;
begin
  LLogItem.LogDate := Now;
  LLogItem.LogStr := Str;

  FErrorLogItems.Add(LLogItem);
  FFullLogItems.Add(LLogItem);

  LogsPages.Pages[1].Caption := 'Error Log ('+ FErrorLogItems.Count.ToString() +')';

  FullLogList.Items.Count := FFullLogItems.Count;
  FullLogList.ItemIndex := FullLogList.Items.Count-1;
  ErrorLog.Items.Count := FErrorLogItems.Count;
  ErrorLog.ItemIndex := ErrorLog.Items.Count-1;

  ErrorLog.Items[ErrorLog.Items.Count-1].MakeVisible(False);
  FullLogList.Items[FullLogList.Items.Count-1].MakeVisible(False);
end;

procedure TMainForm.LogFull(const Str: string);
var
  LLogItem: TLogItem;
begin
  LLogItem.LogDate := Now;
  LLogItem.LogStr := Str;

  FFullLogItems.Add(LLogItem);

  FullLogList.Items.Count := FFullLogItems.Count;
  FullLogList.ItemIndex := FullLogList.Items.Count-1;

  FullLogList.Items[FullLogList.Items.Count-1].MakeVisible(False);
end;

procedure TMainForm.LogListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FGeneralLogItems.Count then
  begin
    Item.Caption := DateTimeToStr(FGeneralLogItems[Item.Index].LogDate);
    Item.SubItems.Add(FGeneralLogItems[Item.Index].LogStr);
  end;
end;

procedure TMainForm.O1Click(Sender: TObject);
begin
  if JobsList.ItemIndex > -1 then
  begin
    ShellExecute(handle, 'open', PWideChar(FProjects[JobsList.ItemIndex].SourceFolder), nil, nil, SW_NORMAL);
  end;
end;

procedure TMainForm.O2Click(Sender: TObject);
begin
  if JobsList.ItemIndex > -1 then
  begin
    ShellExecute(handle, 'open', PWideChar(FProjects[JobsList.ItemIndex].DestFolder), nil, nil, SW_NORMAL);
  end;
end;

procedure TMainForm.OperationThreadException(Sender: TIdThreadComponent;
  AException: Exception);
begin
  try
    FLogLineToAdd := 'Thread Exception: ' + AException.Message;
    OperationThread.Synchronize(AddToErrorLog);
  except on E: Exception do
  end;
end;

procedure TMainForm.OperationThreadRun(Sender: TIdThreadComponent);
const
  TAB = '    - ';
var
  I, J: Integer;
  LSourceFile: string;
  LDestFile: string;
  LCopy: Boolean;
  LCopiedCount: integer;
  LFileCopyPair: TFileCopyPair;
  LFileCopyPairs: TFileCopyPairs;
  LDateTime: TDateTime;
  LFilesToDelete: TStringList;
  LDirsToDelete: TStringList;
  LDirToCreate: TFolderCreatePair;
  LDirsToCreate: TFolderCreatePairs;
  LSourceDir, LDestDir: string;
  LLogFilePath: string;
  LLogFile: TStringList;
  LLogItem: TLogItem;
  LDiffReason: string;
  LEmailSetFile: TIniFile;
  LFrom, LTo, LHost, LPort, LPass, LUser: string;
  LCompareMethodId: integer;
begin
  OperationThread.Synchronize(StartProgressTimer);
  if FPreview then
  begin
    FLogLineToAdd := 'This is a preview';
    OperationThread.Synchronize(AddToLog);
  end;
  LDateTime := Now;
//  ProgressTimer.Enabled := True;
  try
    JobsList.ItemIndex := -1;
    for J := 0 to FProjects.Count-1 do
    begin
      if FStop then
      begin
        Break;
      end;
      if FProjects[J].Active or (FRun and FAll) then
      begin
        if FCompareMethodId = -1 then
        begin
          LCompareMethodId := FProjects[J].CompareMethod;
        end
        else
        begin
          LCompareMethodId := FCompareMethodId;
        end;

        // loop through all available projects
        FItemIndex := J;
        OperationThread.Synchronize(JumpToItem);

        FCurrentProjectName := FProjects[J].ProjectName;
        OperationThread.Synchronize(UpdateProjectName);
        FLogLineToAdd := '+ Starting ' + FProjects[J].ProjectName;
        OperationThread.Synchronize(AddToLog);
        FLogLineToAdd := TAB + 'Using ' + CompareMethodToStr(LCompareMethodId);
        OperationThread.Synchronize(AddToLog);
        FIgnoreTypeString := FProjects[J].IgnoredFileTypes;

        // reset lists
        FProgress := 0;
        FFiles.Clear;
        FFolders.Clear;
        FChangeCount := 0;
        OperationThread.Synchronize(UpdateChangeCount);
        if DirectoryExists(FProjects[J].SourceFolder) and (not FStop) then
        begin
          // list all the files in the source folder
          SearchSourceFiles.RootDirectory := FProjects[J].SourceFolder;
          SearchSourceFiles.Search;
          FLogLineToAdd := TAB + 'Found ' + FFiles.Count.ToString() + ' files';
          OperationThread.Synchronize(AddToLog);

          LCopiedCount := 0;
          FTotalCMDCount := 0;
          // create a list to hold the names of the files that will be copied
          LFileCopyPairs := TFileCopyPairs.Create;
          try
            FMaxProgress := FFiles.Count;
            OperationThread.Synchronize(UpdateMaxProgres);

{$REGION 'singlethreaded compare'}
            // compare all source files with their destination counterparts
            OperationThread.Synchronize(StartSpeedTimer);
            for I := 0 to FFiles.Count-1 do
            begin
              FProgress := i+1;
              if FStop then
              begin
                Break;
              end;
              LSourceFile := FFiles[i].Trim;
              if (i mod 50) = 0 then
              begin
                FStateMsg := 'Looking for changes ' + i.ToString + '/' + FFiles.Count.ToString + ' (' + LSourceFile + ')';
                OperationThread.Synchronize(UpdateState);
              end;

              LCopy := false;
              // replace source folder path with destination folder path
              LDestFile := LSourceFile.Replace(FProjects[J].SourceFolder, FProjects[J].DestFolder);
              if not FileExists(LDestFile) then
              begin
                LCopy := True;
                LDiffReason := '[DestDoesntExist] ';
              end
              else
              begin
                try
                  LCopy := not FFileCompare.CompareFiles(LSourceFile, LDestFile, FProjects[J].BufferSize, LCompareMethodId, LDiffReason);
                except on E: Exception do
                  begin
                    FLogLineToAdd := TAB + 'File read error: ' + E.Message + '[' + LSourceFile + ']';
                    OperationThread.Synchronize(AddToErrorLog);
                    Continue;
                  end;
                end;
              end;

              if LCopy then
              begin
                if not DirectoryExists(ExtractFileDir(LDestFile)) then
                begin
                  ForceDirectories(ExtractFileDir(LDestFile));
                end;
                LFileCopyPair.SourceFile := LSourceFile;
                LFileCopyPair.DestFile := LDestFile;
                FLogLineToAdd := TAB + LDiffReason + LSourceFile + ' will be copied to ' + LDestFile;
                OperationThread.Synchronize(AddToFullLog);
                LFileCopyPairs.Add(LFileCopyPair);
                FChangeCount := LFileCopyPairs.Count;
                OperationThread.Synchronize(UpdateChangeCount);
              end;
            end;
            OperationThread.Synchronize(StopSpeedTimer);
{$ENDREGION}

            // list directories that will be created at destination
            if not FStop then
            begin
              LDirsToCreate := TFolderCreatePairs.Create;
              try
                FMaxProgress := FFolders.Count;
                OperationThread.Synchronize(UpdateMaxProgres);
                for I := 0 to FFolders.Count-1 do
                begin
                  if FStop then
                    Break;

                  FProgress := i + 1;
                  LSourceDir := FFolders[i].Trim;

                  // check if directory exists in destination
                  if (FProgress mod 50) = 0 then
                  begin
                    FStateMsg := 'Checking directory ' + LSourceDir;
                    OperationThread.Synchronize(UpdateState);
                  end;
                  LDestDir := LSourceDir.Replace(FProjects[J].SourceFolder, FProjects[J].DestFolder);
                  if not DirectoryExists(LDestDir) then
                  begin
                    LDirToCreate.Directory := LDestDir;
                    LDirToCreate.Attributes := FileGetAttr(LSourceDir);
                    LDirsToCreate.Add(LDirToCreate);
                  end;
                end;
              finally
                if LDirsToCreate.Count > 0 then
                begin
                  FLogLineToAdd := TAB + 'Number of directories to be created: ' + LDirsToCreate.Count.ToString();
                  OperationThread.Synchronize(AddToLog);
                end
                else
                begin
                  FLogLineToAdd := TAB + 'No directories will be created at the output.';
                  OperationThread.Synchronize(AddToLog);
                end;
                // create all directories that do not exist in the destination
                FMaxProgress := LDirsToCreate.Count;
                OperationThread.Synchronize(UpdateMaxProgres);
                for I := 0 to LDirsToCreate.Count-1 do
                begin
                  if FStop then
                    Break;

                  FProgress := i + 1;
                  if (FProgress mod 50) = 0 then
                  begin
                    FStateMsg := 'Creating directory ' + LDirsToCreate[i].Directory;
                  end;
                  OperationThread.Synchronize(UpdateState);
                  try
                    if not FPreview then
                    begin
                      if not DirectoryExists(LDirsToCreate[i].Directory) then
                      begin
                        if not ForceDirectories(LDirsToCreate[i].Directory) then
                        begin
                          RaiseLastOSError;
                        end
                        else
                        begin
                          FLogLineToAdd := TAB + 'Created ' + LDirsToCreate[i].Directory;
                          OperationThread.Synchronize(AddToFullLog);
                        end;
                      end;
                    end;
                  except on E: Exception do
                  begin
                    FLogLineToAdd := TAB + 'Create dir error: ' + E.Message;
                    OperationThread.Synchronize(AddToErrorLog);
                    Continue;
                  end;
                  end;
                end;
                LDirsToCreate.Free;
              end;
            end;

            // copy all the altered files from source to destination
            if not FStop then
            begin
              if LFileCopyPairs.Count > 0 then
              begin
                FLogLineToAdd := TAB + 'Number of files to be copied: ' + LFileCopyPairs.Count.ToString();
                OperationThread.Synchronize(AddToLog);
              end
              else
              begin
                FLogLineToAdd := TAB + 'No changes found.';
                OperationThread.Synchronize(AddToLog);
              end;

              if not FPreview then
              begin
                FMaxProgress := LFileCopyPairs.Count;
                OperationThread.Synchronize(UpdateMaxProgres);
                OperationThread.Synchronize(StartSpeedTimer);
                for I := 0 to LFileCopyPairs.Count-1 do
                begin
                  if FStop then
                  begin
                    Break;
                  end;

                  FProgress := i+1;
                  if (i mod 20) = 0 then
                  begin
                    FStateMsg := 'Copying ' + i.ToString + '/' + LFileCopyPairs.Count.ToString + ' (' + LFileCopyPairs[i].DestFile + ')';
                    OperationThread.Synchronize(UpdateState);
                  end;
                  try
                    if FileExists(LFileCopyPairs[i].DestFile) then
                    begin
                      DeleteFile(LFileCopyPairs[i].DestFile);
                    end;

                    // report if copy fails
                    if not CopyFile(PWideChar(LFileCopyPairs[i].SourceFile), PWideChar(LFileCopyPairs[i].DestFile), false) then
                    begin
                      RaiseLastOSError;
                    end
                    else
                    begin
                      FLogLineToAdd := TAB + LFileCopyPairs[i].SourceFile + ' is copied to ' + LFileCopyPairs[i].DestFile;
                      OperationThread.Synchronize(AddToFullLog);
                    end;
                  except on E: Exception do
                    begin
                      FLogLineToAdd := TAB + 'Copy error: ' + E.Message + ' ' + LFileCopyPairs[i].SourceFile;
                      OperationThread.Synchronize(AddToErrorLog);
                      Continue;
                    end;
                  end;
                end;
                OperationThread.Synchronize(StopSpeedTimer);
              end;
            end;

            if not FStop then
            begin
              // option control
              if FProjects[J].DeleteFromDest then
              begin
                FFiles.Clear;
                FFolders.Clear;
                FLogLineToAdd := TAB + 'Searching destination for files/folders to be deleted.';
                OperationThread.Synchronize(AddToLog);
                SearchDestFiles.RootDirectory :=  FProjects[J].DestFolder;
                SearchDestFiles.Search;

                // delete file if it does not exist in source but exists in destination
                LFilesToDelete := TStringList.Create();
                try
                  FMaxProgress := FFiles.Count;
                  OperationThread.Synchronize(UpdateMaxProgres);
                  for I := 0 to FFiles.Count-1 do
                  begin
                    if FStop then
                    begin
                      Break;
                    end;
                    FProgress := I + 1;
                    LDestFile := FFiles[i].Trim;
                    if (FProgress mod 50) = 0 then
                    begin
                      FStateMsg := 'Searching file in source folder ' + i.ToString + '/' + FFiles.Count.ToString + ' (' + LDestFile + ')';
                      OperationThread.Synchronize(UpdateState);
                    end;

                    LSourceFile := LDestFile.Replace(FProjects[J].DestFolder, FProjects[J].SourceFolder);
                    if not FileExists(LSourceFile) then
                    begin
                      LFilesToDelete.Add(LDestFile);
                    end;
                  end;
                finally
                  if not FStop then
                  begin
                    if LFilesToDelete.Count > 0 then
                    begin
                      FLogLineToAdd := TAB + 'Number of files to be deleted at destination: ' +  LFilesToDelete.Count.ToString();
                      OperationThread.Synchronize(AddToLog);

                      if not FPreview then
                      begin
                        FMaxProgress := LFilesToDelete.Count;
                        OperationThread.Synchronize(UpdateMaxProgres);
                        for I := 0 to LFilesToDelete.Count-1 do
                        begin
                          if FStop then
                            Break;
                          FProgress := I + 1;
                          try
                            DeleteFile(LFilesToDelete[i]);
                            FLogLineToAdd := TAB + 'Deleted file:  ' + LFilesToDelete[i];
                            OperationThread.Synchronize(AddToFullLog);
                          Except on E: Exception do
                          begin
                            FLogLineToAdd := TAB + 'Delete error: ' + E.Message + ' ' + LFilesToDelete[i];
                            OperationThread.Synchronize(AddToErrorLog);
                            Continue;
                          end;
                          end;
                        end;
                      end;
                    end
                    else
                    begin
                      FLogLineToAdd := TAB + 'No files will be deleted in destination';
                      OperationThread.Synchronize(AddToLog);
                    end;
                  end;
                  LFilesToDelete.Free;
                end;

                // delete folder if it does not exist in source but exists in destination
                LDirsToDelete := TStringList.Create;
                try
                  FMaxProgress := FFolders.Count;
                  OperationThread.Synchronize(UpdateMaxProgres);
                  for I := 0 to FFolders.Count-1 do
                  begin
                    if FStop then
                      Break;

                    FProgress := I + 1;
                    LDestDir := FFolders[i].Trim;
                    if (FProgress mod 50) = 0 then
                    begin
                      FStateMsg := 'Searching folder in source folder ' + i.ToString + '/' + FFolders.Count.ToString + ' (' + LDestDir + ')';
                      OperationThread.Synchronize(UpdateState);
                    end;

                    LSourceDir := LDestDir.Replace(FProjects[J].DestFolder, FProjects[J].SourceFolder);
                    if not DirectoryExists(LSourceDir) then
                    begin
                      LDirsToDelete.Add(LDestDir);
                    end;
                  end;
                finally
                  if not FStop then
                  begin
                    if LDirsToDelete.Count > 0 then
                    begin
                      FLogLineToAdd := TAB + 'Number of folders to be deleted at the destination: ' + LDirsToDelete.Count.ToString();
                      OperationThread.Synchronize(AddToLog);

                      if not FPreview then
                      begin
                        FMaxProgress := LDirsToDelete.Count;
                        OperationThread.Synchronize(UpdateMaxProgres);
                        for I := 0 to LDirsToDelete.Count-1 do
                        begin
                          if FStop then
                            Break;
                          FProgress := I + 1;
                          try
                            RemoveDir(LDirsToDelete[i]);
                            FLogLineToAdd := TAB + 'Deleted dir: ' + LDirsToDelete[i];
                            OperationThread.Synchronize(AddToFullLog);
                          Except on E: Exception do
                            begin
                              FLogLineToAdd := TAB + 'Delete dir error: ' + E.Message + ' ' + LDirsToDelete[i];
                              OperationThread.Synchronize(AddToErrorLog);
                              Continue;
                            end;
                          end;
                        end;
                      end;
                    end
                    else
                    begin
                      FLogLineToAdd := TAB + 'No folders will be deleted in destination';
                      OperationThread.Synchronize(AddToLog);
                    end;
                  end;
                  LDirsToDelete.Free;
                end;
              end;
            end;

            if not FStop then
            begin
              // copy folder properties
              FFiles.Clear;
              FFolders.Clear;
              FLogLineToAdd := TAB + 'Reading source folder properties.';
              OperationThread.Synchronize(AddToLog);
              SearchDestFiles.RootDirectory :=  FProjects[J].SourceFolder;
              SearchDestFiles.Search;

              LDirsToCreate := TFolderCreatePairs.Create;
              try
                FMaxProgress := FFolders.Count;
                OperationThread.Synchronize(UpdateMaxProgres);
                for I := 0 to FFolders.Count-1 do
                begin
                  if FStop then
                    Break;

                  FProgress := i + 1;
                  LSourceDir := FFolders[i].Trim;

                  // check if directory exists in destination
                  if (FProgress mod 50) = 0 then
                  begin
                    FStateMsg := 'Checking directory ' + LSourceDir;
                    OperationThread.Synchronize(UpdateState);
                  end;
                  LDestDir := LSourceDir.Replace(FProjects[J].SourceFolder, FProjects[J].DestFolder);
                  if DirectoryExists(LDestDir) then
                  begin
                    LDirToCreate.Directory := LDestDir;
                    LDirToCreate.Attributes := FileGetAttr(LSourceDir);
                    LDirsToCreate.Add(LDirToCreate);
                  end;
                end;
              finally
                if LDirsToCreate.Count > 0 then
                begin
                  FLogLineToAdd := TAB + 'Number of directories found to write attributes: ' + LDirsToCreate.Count.ToString();
                  OperationThread.Synchronize(AddToLog);
                end
                else
                begin
                  FLogLineToAdd := TAB + 'No directories found to write attributes';
                  OperationThread.Synchronize(AddToLog);
                end;
                if not FPreview then
                begin
                  // create all directories that do not exist in the destination
                  FMaxProgress := LDirsToCreate.Count;
                  OperationThread.Synchronize(UpdateMaxProgres);
                  for I := 0 to LDirsToCreate.Count-1 do
                  begin
                    if FStop then
                      Break;

                    FProgress := i + 1;
                    if (FProgress mod 50) = 0 then
                    begin
                      FStateMsg := 'Writing attribute to ' + LDirsToCreate[i].Directory;
                      OperationThread.Synchronize(UpdateState);
                    end;
                    try
                      FileSetAttr(LDirsToCreate[i].Directory, LDirsToCreate[i].Attributes);
                    except on E: Exception do
                    begin
                      FLogLineToAdd := TAB + 'Change attribute error: ' + E.Message;
                      OperationThread.Synchronize(AddToErrorLog);
                      Continue;
                    end;
                    end;
                  end;
                end;
                LDirsToCreate.Free;
              end;
            end;
          finally
            LFileCopyPairs.Free;
          end;
        end;
        FLogLineToAdd := TAB + 'Finished ' + FProjects[J].ProjectName;
        OperationThread.Synchronize(AddToLog);
        FLogLineToAdd := '';
        OperationThread.Synchronize(AddToLog);
      end
      else
      begin
        FLogLineToAdd := 'Skipping ' + FProjects[J].ProjectName;
        OperationThread.Synchronize(AddToFullLog);
      end;
    end;
  finally
    OperationThread.Synchronize(StopSpeedTimer);
    OperationThread.Synchronize(UpdateState);
    LLogFilePath := ExtractFileDir(Application.ExeName) + '\logs\' + DateTimeToStr(Now).Replace('.', '').Replace(':', '').Replace(' ', '').Replace('\', '').Trim + '.log';
    if FStop then
    begin
      FLogLineToAdd := 'Stopped by user.';
      OperationThread.Synchronize(AddToLog);
    end
    else
    begin
      FLogLineToAdd := 'Backup done in ' + FormatDateTime('hh:nn:ss.zzz', Now - LDateTime);
      OperationThread.Synchronize(AddToLog);
    end;
    FStop := True;
    LLogFile := TStringList.Create;
    try
      for LLogItem in FFullLogItems do
      begin
        LLogFile.Add('[' + DateTimeToStr(LLogItem.LogDate) + '] ' + LLogItem.LogStr);
      end;
    finally
      if (not FPreview) and (SendEmailBtn.Checked or FSendEmail) then
      begin
        // send emails
        LEmailSetFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\email.ini');
        try
          with LEmailSetFile do
          begin
            LFrom := ReadString('EMail', 'From', '');
            LTo := ReadString('EMail', 'To', '');
            LHost := ReadString('EMail', 'Host', '');
            LPort := ReadString('EMail', 'Port', '');
            LUser := ReadString('EMail', 'User', '');
            LPass := ReadString('EMail', 'Pass', '');

            if (Length(LFrom) > 0) and (Length(LTo) > 0) and (Length(LHost) > 0) and (Length(LPort) > 0) and (Length(LUser) > 0) and (Length(LPass) > 0) then
            begin
              FLogLineToAdd := 'Sending email';
              OperationThread.Synchronize(AddToLog);
              IdMessage1.From.Address := LFrom;
              IdMessage1.Recipients.EMailAddresses := LTo;
              IdMessage1.Body.Text := LLogFile.Text;
              IdMessage1.Subject := 'OneWay Backup Report';
              try
                IdSMTP1.Host := LHost;
                IdSMTP1.Port := StrToInt(LPort);
                IdSMTP1.AuthType := satDefault;
                IdSMTP1.Username := LUser;
                IdSMTP1.Password := LPass;
                IdSMTP1.Connect;
                IdSMTP1.Send(IdMessage1);
                FLogLineToAdd := 'Sent email';
                OperationThread.Synchronize(AddToLog);
              except on E: Exception do
                begin
                  LLogFile.Add(E.Message);
                  FLogLineToAdd := 'Email send error: ' + E.Message;
                  OperationThread.Synchronize(AddToErrorLog);
                end;
              end;
            end;
          end;
        finally
          LEmailSetFile.Free;
        end;
      end;

      try
        LLogFile.SaveToFile(LLogFilePath, TEncoding.UTF8);
        LastLogFilePath := LLogFilePath;

      except on E: Exception do
        begin
          FLogLineToAdd := 'Log save error: ' + E.Message;
          OperationThread.Synchronize(AddToErrorLog);
          raise E;
        end;
      end;
      LLogFile.Free;
    end;
    OperationThread.Synchronize(StopPassedTimeTimer);
    OperationThread.Synchronize(StopProgressTimer);
    OperationThread.Synchronize(EnableUI);
    OperationThread.Synchronize(ShowPreviewResults);
    OperationThread.Synchronize(ShutDown);
    OperationThread.Synchronize(CloseQueue);
    OperationThread.Terminate;
  end;
end;

procedure TMainForm.PassedTimeTimerTimer(Sender: TObject);
begin
  if not FStop then
  begin
    Inc(FTotalTimeCounter);
    TimeLabel.Caption := 'Time: ' + Format('%.2d:%.2d', [FTotalTimeCounter div 60, FTotalTimeCounter mod 60]);
  end;
end;

procedure TMainForm.PreviewBtnClick(Sender: TObject);
begin
  FPreview := True;
  PassedTimeTimer.Enabled := True;
  FFullLogItems.Clear;
  FGeneralLogItems.Clear;
  FErrorLogItems.Clear;
  FullLogList.Items.Count := 0;
  ErrorLog.Items.Count := 0;
  LogList.Items.Count := 0;
  SpeedLabel.Caption := 'Speed: N/A';
  ChangesLabel.Caption := 'Changes found: 0';
  TimeLabel.Caption := 'Time: 00:00';
  LogsPages.Pages[1].Caption := 'Error Log';
  FTimeCounter := 0;
  FTotalTimeCounter := 0;

  FStop := False;
  DisableUI;
  LogList.Items.Clear;
  OperationThread.Start;
end;

procedure TMainForm.ProgressTimerTimer(Sender: TObject);
begin
  UpdateProgress;
end;

procedure TMainForm.RunJobsBtnClick(Sender: TObject);
begin
  FPreview := False;
  PassedTimeTimer.Enabled := True;
  FFullLogItems.Clear;
  FGeneralLogItems.Clear;
  FErrorLogItems.Clear;
  FullLogList.Items.Count := 0;
  ErrorLog.Items.Count := 0;
  LogList.Items.Count := 0;
  SpeedLabel.Caption := 'Speed: N/A';
  ChangesLabel.Caption := 'Changes found: 0';
  TimeLabel.Caption := 'Time: 00:00';
  LogsPages.Pages[1].Caption := 'Error Log';
  FTimeCounter := 0;
  FTotalTimeCounter := 0;

  FStop := False;
  DisableUI;
  LogList.Items.Clear;
  OperationThread.Start;
end;

procedure TMainForm.SaveProjects;
var
  LProjectFile: TStringList;
  LLine: string;
  I: Integer;
begin
  LProjectFile := TStringList.Create();
  try
    if FileExists(ExtractFileDir(Application.ExeName) + '\data.dat') then
    begin
      DeleteFile(ExtractFileDir(Application.ExeName) + '\data.dat');
    end;
    for I := 0 to FProjects.Count-1 do
    begin
      LLine := FProjects[i].SourceFolder + '|' + FProjects[i].DestFolder + '|'
        + FProjects[i].ProjectName + '|' + BoolToStr(FProjects[i].Active, True)
        + '|' + BoolToStr(FProjects[i].DeleteFromDest, True) + '|' + FProjects[i].BufferSize.ToString() + '|' + FProjects[i].IgnoredFileTypes + '|' + FProjects[i].CompareMethod.ToString;
      LProjectFile.Add(LLine);
    end;
  finally
    LProjectFile.SaveToFile(ExtractFileDir(Application.ExeName) + '\data.dat', TEncoding.UTF8);
    LProjectFile.Free;
  end;
end;

procedure TMainForm.SearchDestFilesFindDirectory(Sender: TObject;
  const AName: string);
begin
  FFolders.Add(AName);
end;

procedure TMainForm.SearchSourceFilesFindDirectory(Sender: TObject; const AName: string);
begin
  FFolders.Add(AName);
end;

procedure TMainForm.SearchSourceFilesFindFile(Sender: TObject; const AName: string);
begin
  if CheckIfFileCanBeAdded(AName, FIgnoreTypeString) then
  begin
    FFiles.Add(AName);
    if (FFiles.Count mod 100) = 0 then
    begin
      FStateMsg := 'Found ' + FFiles.Count.ToString + ' files';
      UpdateState;
    end;
  end;
end;

procedure TMainForm.SelectAllLabelClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FProjects.Count-1 do
  begin
    FProjects[i].Active := True;
  end;
  SaveProjects;
  for I := 0 to JobsList.Items.Count-1 do
  begin
    JobsList.Items[i].Checked := True;
  end;
end;

procedure TMainForm.SelectNoneLabelClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FProjects.Count-1 do
  begin
    FProjects[i].Active := False;
  end;
  SaveProjects;
  for I := 0 to JobsList.Items.Count-1 do
  begin
    JobsList.Items[i].Checked := False;
  end;
end;

procedure TMainForm.SelectReverseLabelClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FProjects.Count-1 do
  begin
    FProjects[i].Active := not FProjects[i].Active;
  end;
  SaveProjects;
  for I := 0 to JobsList.Items.Count-1 do
  begin
    JobsList.Items[i].Checked := not JobsList.Items[i].Checked;
  end;
end;

procedure TMainForm.ShowPreviewResults;
begin
  if FPreview then
  begin
    if not FShutDown then
    begin
      if not FExit then
      begin
        LogForm.LogList.Items.Count := FFullLogItems.Count;
        LogForm.Show;
      end;
    end;
  end;
end;

procedure TMainForm.ShutDown;
begin
  if FShutDown then
  begin
    WinExec('shutdown.exe -s -f -t 0' , SW_HIDE);
  end;
end;

procedure TMainForm.StartProgressTimer;
begin
  ProgressTimer.Enabled := True;
end;

procedure TMainForm.StartSpeedTimer;
begin
  SpeedTimer.Enabled := True;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
begin
  if ID_YES = Application.MessageBox('Do you want to stop backup process?', 'Question', MB_ICONQUESTION or MB_YESNO) then
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
end;

procedure TMainForm.StopPassedTimeTimer;
begin
  PassedTimeTimer.Enabled := False;
end;

procedure TMainForm.StopProgressTimer;
begin
  ProgressTimer.Enabled := False;
end;

procedure TMainForm.StopSpeedTimer;
begin
  SpeedTimer.Enabled := False;
   FTimeCounter := 0;
end;

procedure TMainForm.SpeedTimerTimer(Sender: TObject);
var
  LValue: Double;
begin
  Inc(FTimeCounter);
  LValue := FProgress / FTimeCounter;
  SpeedLabel.Caption := 'Speed: ' + Format('%.2f', [LValue]) + ' files per second';
end;

procedure TMainForm.UpdateChangeCount;
begin
  ChangesLabel.Caption := 'Changes found: ' + FloatToStr(FChangeCount);
end;

procedure TMainForm.UpdateMaxProgres;
begin
  ProgressBar.Max := FMaxProgress;
  Taskbar1.ProgressMaxValue := FMaxProgress;
end;

procedure TMainForm.UpdateProgress;
var
  LProgress: Extended;
  LProgressStr: string;
begin
  ProgressBar.Position := FProgress;
  Taskbar1.ProgressValue := FProgress;
  if FMaxProgress > 0 then
  begin
    LProgress := (10000 * FProgress) / FMaxProgress;
    LProgressStr :=  Format('%.2f', [LProgress/100]) + '%';
    Self.Caption := LProgressStr + ' [' + PROGRAM_TITLE + ']';
    PercentageLabel.Caption := LProgressStr;
  end;
end;

procedure TMainForm.UpdateProjectName;
begin
  ProjectNameLabel.Caption := FCurrentProjectName;
end;

procedure TMainForm.UpdateState;begin
  StateLabel.Caption := FStateMsg;
end;

end.