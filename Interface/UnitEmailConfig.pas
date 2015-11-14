unit UnitEmailConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, sButton, sLabel,
  sSpinEdit, sEdit, IniFiles;

type
  TEmailConfForm = class(TForm)
    FromEdit: TsEdit;
    ToEdit: TsEdit;
    HostEdit: TsEdit;
    UserNameEdit: TsEdit;
    PassEdit: TsEdit;
    PortEdit: TsSpinEdit;
    sLabel1: TsLabel;
    SaveBtn: TsButton;
    CancelBtn: TsButton;
    SendTestBtn: TsButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EmailConfForm: TEmailConfForm;

implementation

{$R *.dfm}

uses UnitMainForm;

procedure TEmailConfForm.CancelBtnClick(Sender: TObject);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
  Close;
end;

procedure TEmailConfForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TEmailConfForm.FormShow(Sender: TObject);
var
  LEmailSetFile: TIniFile;
begin
  LEmailSetFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\email.ini');
  try
    with LEmailSetFile do
    begin
      FromEdit.Text := ReadString('EMail', 'From', '');
      ToEdit.Text := ReadString('Email', 'To', '');
      HostEdit.Text := ReadString('Email', 'Host', '');
      PortEdit.Text := ReadString('Email', 'Port', '25');
      UserNameEdit.Text := ReadString('Email', 'User', '');
      PassEdit.Text := ReadString('Email', 'Pass', '');
    end;
  finally
    LEmailSetFile.Free;
  end;
end;

procedure TEmailConfForm.SaveBtnClick(Sender: TObject);
var
  LEmailSetFile: TIniFile;
begin
  LEmailSetFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\email.ini');
  try
    with LEmailSetFile do
    begin
      WriteString('EMail', 'From', FromEdit.Text);
      WriteString('Email', 'To', ToEdit.Text);
      WriteString('Email', 'Host', HostEdit.Text);
      WriteString('Email', 'Port', PortEdit.Text);
      WriteString('Email', 'User', UserNameEdit.Text);
      WriteString('Email', 'Pass', PassEdit.Text);
    end;
  finally
    LEmailSetFile.Free;
  end;
end;

end.
