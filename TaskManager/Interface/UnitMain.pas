unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitTaskItem, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    TasksList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FTasks: TTaskItems;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FTasks := TTaskItems.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTasks.Free;
end;

end.

