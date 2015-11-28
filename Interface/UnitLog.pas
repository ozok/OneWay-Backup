unit UnitLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, 
  Vcl.StdCtrls, ShellApi;

type
  TLogForm = class(TForm)
    LogList: TListView;
    LogFilePathLabel: TLabel;
                                   
    procedure FormResize(Sender: TObject);
    procedure LogListData(Sender: TObject; Item: TListItem);
    procedure FormShow(Sender: TObject);
    procedure LogFilePathLabelClick(Sender: TObject);
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

procedure TLogForm.FormResize(Sender: TObject);
begin
  LogList.Columns[1].Width := LogList.ClientWidth - 20 - LogList.Columns[0].Width;
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

procedure TLogForm.LogListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < MainForm.FFullLogItems.Count then
  begin
    Item.Caption := DateTimeToStr(MainForm.FFullLogItems[Item.Index].LogDate);
    Item.SubItems.Add(MainForm.FFullLogItems[Item.Index].LogStr);
  end;
end;

end.