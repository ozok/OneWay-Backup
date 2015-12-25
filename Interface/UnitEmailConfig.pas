{Copyright (c) <2015> <ozok26@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

unit UnitEmailConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,
  IdMessageClient, IdSMTPBase, IdSMTP, IdBaseComponent, IdMessage, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ComCtrls;

type
  TEmailConfForm = class(TForm)
    FromEdit: TEdit;
    ToEdit: TEdit;
    HostEdit: TEdit;
    UserNameEdit: TEdit;
    PassEdit: TEdit;
    sLabel1: TLabel;
    SaveBtn: TButton;
    CancelBtn: TButton;
    SendTestBtn: TButton;
    IdMessage1: TIdMessage;
    IdSMTP1: TIdSMTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    PortEdit: TJvSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ReportTypeList: TComboBox;
    Label7: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    procedure CancelBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SendTestBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EmailConfForm: TEmailConfForm;

implementation

{$R *.dfm}

uses
  UnitMainForm;

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

procedure TEmailConfForm.FormCreate(Sender: TObject);
var
  LEmailSetFile: TIniFile;
begin
  LEmailSetFile := TIniFile.Create(MainForm.AppDataFolder + '\email.ini');
  try
    with LEmailSetFile do
    begin
      FromEdit.Text := ReadString('EMail', 'From', '');
      ToEdit.Text := ReadString('Email', 'To', '');
      HostEdit.Text := ReadString('Email', 'Host', '');
      PortEdit.Text := ReadString('Email', 'Port', '25');
      UserNameEdit.Text := ReadString('Email', 'User', '');
      PassEdit.Text := ReadString('Email', 'Pass', '');
      ReportTypeList.ItemIndex := ReadInteger('Email', 'ReportType', 2);
    end;
  finally
    LEmailSetFile.Free;
  end;
end;

procedure TEmailConfForm.SaveBtnClick(Sender: TObject);
var
  LEmailSetFile: TIniFile;
begin
  LEmailSetFile := TIniFile.Create(MainForm.AppDataFolder + '\email.ini');
  try
    with LEmailSetFile do
    begin
      WriteString('EMail', 'From', FromEdit.Text);
      WriteString('Email', 'To', ToEdit.Text);
      WriteString('Email', 'Host', HostEdit.Text);
      WriteString('Email', 'Port', PortEdit.Text);
      WriteString('Email', 'User', UserNameEdit.Text);
      WriteString('Email', 'Pass', PassEdit.Text);
      WriteInteger('Email', 'ReportType', ReportTypeList.ItemIndex);
    end;
  finally
    LEmailSetFile.Free;
  end;
  Close;
end;

procedure TEmailConfForm.SendTestBtnClick(Sender: TObject);
begin
  if (Length(FromEdit.Text) > 0) and (Length(ToEdit.Text) > 0) and (Length(HostEdit.Text) > 0) and (Length(PortEdit.Text) > 0) and (Length(UserNameEdit.Text) > 0) and (Length(PassEdit.Text) > 0) then
  begin
    IdMessage1.From.Address := FromEdit.Text;
    IdMessage1.Recipients.EMailAddresses := ToEdit.Text;
    IdMessage1.Body.Text := 'OneWay Backup Test Mail';
    IdMessage1.Subject := 'OneWay Backup Test Mail';
    try
      IdSMTP1.Host := HostEdit.Text;
      IdSMTP1.Port := Round(PortEdit.Value);
      IdSMTP1.AuthType := satDefault;
      IdSMTP1.Username := UserNameEdit.Text;
      IdSMTP1.Password := PassEdit.Text;
      if IdSMTP1.Connected then
      begin
        IdSMTP1.Disconnect();
      end;
      IdSMTP1.UseTLS := utUseExplicitTLS;
      IdSMTP1.Connect;
      IdSMTP1.Send(IdMessage1);
      Application.MessageBox('Sent test email.', 'Info', MB_ICONINFORMATION);
    except
      on E: Exception do
      begin
        Application.MessageBox(PWideChar(E.Message), 'Error', MB_ICONERROR);
      end;
    end;
  end;
end;

end.

