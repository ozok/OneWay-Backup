unit UnitLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, 
  Vcl.StdCtrls, ShellApi, Vcl.ExtCtrls;

type
  TLogForm = class(TForm)
    LogFilePathLabel: TLabel;
    LogList: TListView;
    Panel1: TPanel;
    CopiedFileLabel: TLabel;
    DeletedFileLabel: TLabel;
    SkippedFileLabel: TLabel;
    ErrorLabel: TLabel;
                                   
    procedure FormResize(Sender: TObject);
    procedure LogListData(Sender: TObject; Item: TListItem);
    procedure FormShow(Sender: TObject);
    procedure LogFilePathLabelClick(Sender: TObject);
    procedure LogListCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogForm: TLogForm;

implementation

{$R *.dfm}

uses UnitMainForm;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CopiedFileLabel.Caption := 'Copied: N/A';
  DeletedFileLabel.Caption := 'Deleted: N/A';
  SkippedFileLabel.Caption := 'Skipped: N/A';
  ErrorLabel.Caption := 'Errors: N/A';
end;

procedure TLogForm.FormResize(Sender: TObject);
begin
//  LogList.Columns[1].Width := LogList.ClientWidth - 20 - LogList.Columns[0].Width;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin
  LogFilePathLabel.Caption := 'This log is saved to ' + MainForm.LastLogFilePath + '. Click to open.';
end;

procedure TLogForm.LogFilePathLabelClick(Sender: TObject);
begin
  if FileExists(MainForm.LastLogFilePath) then
  begin
    ShellExecute(Handle, 'open', PWideChar(MainForm.LastLogFilePath), nil, nil, SW_SHOWNORMAL);
  end
  else
  begin
    Application.MessageBox('Unable to find log file.', 'Error', MB_ICONERROR);
  end;
end;

procedure TLogForm.LogListCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if SubItem = 2 then
  begin
    if item.SubItems[1] = 'Error' then
    begin
      Sender.Canvas.Font.Color := clRed;
    end
    else if Item.SubItems[1] = 'Info' then
    begin
      Sender.Canvas.Font.Color := clBlue;
    end
    else if Item.SubItems[1] = 'Success' then
    begin
      Sender.Canvas.Font.Color := clGreen;
    end
    else if Item.SubItems[1] = 'Skip' then
    begin
      Sender.Canvas.Font.Color := clMaroon;
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

procedure TLogForm.LogListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < MainForm.FFullLogItems.Count then
  begin
    Item.Caption := Item.Index.ToString + '.';
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].AddDate);
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].LogType);
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].Source);
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].Operation);
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].Destination);
    Item.SubItems.Add(MainForm.FFullLogItems.LogItems[Item.Index].Reason);
  end;
end;

end.