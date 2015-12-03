unit UnitLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  JvComponentBase, JvComputerInfoEx, Vcl.ComCtrls;

type
  TLogsForm = class(TForm)
    LogsList: TListBox;
    Splitter1: TSplitter;
    Info: TJvComputerInfoEx;
    ContentList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogsListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ContentListData(Sender: TObject; Item: TListItem);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FAppDataFolder: string;
    FFileContent: TStringList;

    procedure PopulateLogsList;
  public
    { Public declarations }
  end;

var
  LogsForm: TLogsForm;

implementation

{$R *.dfm}

procedure TLogsForm.ContentListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < FFileContent.Count then
  begin
    item.Caption := FFileContent[Item.Index];
  end;
end;

procedure TLogsForm.FormCreate(Sender: TObject);
begin
{$IFDEF PORTABLE}
  FAppDataFolder := ExtractFileDir(Application.ExeName);
{$ENDIF}
{$IFDEF INSTALLED}
  FAppDataFolder := Info.Folders.AppData + '\OneWayBackup';
{$ENDIF}
  FFileContent := TStringList.Create;
end;

procedure TLogsForm.FormDestroy(Sender: TObject);
begin
  FFileContent.Free;
end;

procedure TLogsForm.FormResize(Sender: TObject);
begin
  try
    ContentList.Columns[0].Width := ContentList.ClientWidth - 20;
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
      FFileContent.LoadFromFile(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]);
      ContentList.Items.Count := FFileContent.Count;
    end;
  end;
end;

procedure TLogsForm.PopulateLogsList;
var
  LSearchRec: TSearchRec;
begin
  LogsList.Items.Clear;
  ContentList.Items.Count := 0;
  FFileContent.Clear;
  if FindFirst(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + '*.log', faAnyFile, LSearchRec) = 0 then
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

end.
