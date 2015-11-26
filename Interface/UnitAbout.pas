unit UnitAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, sButton, sLabel, acPNG,
  Vcl.ExtCtrls, acImage, ShellAPI;

type
  TAboutForm = class(TForm)
    sImage1: TsImage;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    sLabel4: TsLabel;
    sButton1: TsButton;
    sButton2: TsButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sButton1Click(Sender: TObject);
    procedure sButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation
  uses UnitMainForm;

{$R *.dfm}

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  {$IFDEF WIN64}
  sLabel3.Caption := '64bit version';
  {$ELSE}
  sLabel3.Caption := '32bit version';
  {$ENDIF}

  {$IFDEF PORTABLE}
    sLabel4.Caption := 'Portable version';
  {$ENDIF}
  {$IFDEF INSTALLED}
    sLabel4.Caption := 'Installed version'
  {$ENDIF}
end;

procedure TAboutForm.sButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.sButton2Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://sourceforge.net/projects/oneway-backup/', nil, nil, SW_SHOWNORMAL);
end;

end.
