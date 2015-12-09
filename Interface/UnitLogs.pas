unit UnitLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  JvComponentBase, JvComputerInfoEx, Vcl.ComCtrls, UnitLogItems, ShellAPI;

type
  TLogsForm = class(TForm)
    LogsList: TListBox;
    Splitter1: TSplitter;
    Info: TJvComputerInfoEx;
    Panel1: TPanel;
    RefreshBtn: TButton;
    OpenLogBtn: TButton;
    OpenLogFolderBtn: TButton;
    ContentList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogsListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ContentListData(Sender: TObject; Item: TListItem);
    procedure FormResize(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure OpenLogBtnClick(Sender: TObject);
    procedure OpenLogFolderBtnClick(Sender: TObject);
    procedure ContentListCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FAppDataFolder: string;
    FLogFile: TLogFile;

    procedure PopulateLogsList;
  public
    { Public declarations }
  end;

var
  LogsForm: TLogsForm;

implementation

{$R *.dfm}

procedure TLogsForm.ContentListCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if SubItem = 1 then
  begin
    if item.SubItems[0] = 'Error' then
    begin
      Sender.Canvas.Font.Color := clRed;
    end
    else if Item.SubItems[0] = 'Info' then
    begin
      Sender.Canvas.Font.Color := clBlue;
    end
    else if Item.SubItems[0] = 'Success' then
    begin
      Sender.Canvas.Font.Color := clGreen;
    end
    else
    begin
      Sender.Canvas.Font.Color := clBlack;
    end;
  end
  else
  begin
    Sender.Canvas.Font.Color := clBlack;
  end;
end;

procedure TLogsForm.ContentListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FLogFile.Count then
  begin
    Item.Caption := FLogFile.LogItems[Item.Index].AddDate;
    Item.SubItems.Add(FLogFile.LogItems[Item.Index].LogType);
    Item.SubItems.Add(FLogFile.LogItems[Item.Index].Source);
    Item.SubItems.Add(FLogFile.LogItems[Item.Index].Operation);
    Item.SubItems.Add(FLogFile.LogItems[Item.Index].Destination);
    Item.SubItems.Add(FLogFile.LogItems[Item.Index].Reason);
  end;
end;

procedure TLogsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FLogFile.Clear;
end;

procedure TLogsForm.FormCreate(Sender: TObject);
begin
{$IFDEF PORTABLE}
  FAppDataFolder := ExtractFileDir(Application.ExeName);
{$ENDIF}
{$IFDEF INSTALLED}
  FAppDataFolder := Info.Folders.AppData + '\OneWayBackup';
{$ENDIF}
  FLogFile := TLogFile.Create;
end;

procedure TLogsForm.FormDestroy(Sender: TObject);
begin
  FLogFile.Free;
end;

procedure TLogsForm.FormResize(Sender: TObject);
begin
  try
//    ContentList.Columns[0].Width := ContentList.ClientWidth - 20;
  except on E: Exception do
  end;
end;

procedure TLogsForm.FormShow(Sender: TObject);
begin
  PopulateLogsList;
end;

procedure TLogsForm.LogsListClick(Sender: TObject);
begin
  if LogsList.ItemIndex > -1 then
  begin
    if FileExists(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]) then
    begin
      Self.Caption := 'Logs (Please wait...)';
      try
        FLogFile.LoadFromFile(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]);
        Self.Caption := FLogFile.Count.ToString();
        ContentList.Items.Count := FLogFile.Count;
      finally
        Self.Caption := 'Logs';
        ContentList.Refresh;
      end;
    end;
  end;
end;

procedure TLogsForm.OpenLogBtnClick(Sender: TObject);
begin  
  if LogsList.ItemIndex > -1 then
  begin   
    if FileExists(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]) then
    begin
      ShellExecute(Handle, 'open', PWideChar(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]), nil, nil, SW_SHOWNORMAL);
    end;
  end;
end;

procedure TLogsForm.OpenLogFolderBtnClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\')), nil, nil, SW_SHOWNORMAL);
end;

procedure TLogsForm.PopulateLogsList;
var
  LSearchRec: TSearchRec;
begin
  LogsList.Items.Clear;
  ContentList.Items.Count := 0;
  FLogFile.LogItems.Clear;
  if FindFirst(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + '*.csv', faAnyFile, LSearchRec) = 0 then
  begin
    try
      repeat
        LogsList.Items.Insert(0, LSearchRec.Name);
      until FindNext(LSearchRec) <> 0;
    finally
      FindClose(LSearchRec);
    end;
  end;

end;

procedure TLogsForm.RefreshBtnClick(Sender: TObject);
begin
  PopulateLogsList;
end;

end.
