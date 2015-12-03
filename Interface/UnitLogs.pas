unit UnitLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  JvComponentBase, JvComputerInfoEx;

type
  TLogsForm = class(TForm)
    LogsList: TListBox;
    ContentList: TListBox;
    Splitter1: TSplitter;
    Info: TJvComputerInfoEx;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogsListClick(Sender: TObject);
  private
    { Private declarations }
    FAppDataFolder: string;

    procedure PopulateLogsList;
  public
    { Public declarations }
  end;

var
  LogsForm: TLogsForm;

implementation

{$R *.dfm}

procedure TLogsForm.FormCreate(Sender: TObject);
begin
{$IFDEF PORTABLE}
  FAppDataFolder := ExtractFileDir(Application.ExeName);
{$ENDIF}
{$IFDEF INSTALLED}
  FAppDataFolder := Info.Folders.AppData + '\OneWayBackup';
{$ENDIF}
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
      ContentList.Items.LoadFromFile(IncludeTrailingPathDelimiter(FAppDataFolder + '\logs\') + LogsList.Items[LogsList.ItemIndex]);
    end;
  end;
end;

procedure TLogsForm.PopulateLogsList;
var
  LSearchRec: TSearchRec;
begin
  LogsList.Items.Clear;
  ContentList.Items.Clear;
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
