unit UnitLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, sListView;

type
  TLogForm = class(TForm)
    LogList: TsListView;
    procedure FormResize(Sender: TObject);
    procedure LogListData(Sender: TObject; Item: TListItem);
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

procedure TLogForm.LogListData(Sender: TObject; Item: TListItem);
begin
  if Item.Index < MainForm.FFullLogItems.Count then
  begin
    Item.Caption := DateTimeToStr(MainForm.FFullLogItems[Item.Index].LogDate);
    Item.SubItems.Add(MainForm.FFullLogItems[Item.Index].LogStr);
  end;
end;

end.
