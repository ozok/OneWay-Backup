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

unit UnitAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ShellAPI;

type
  TAboutForm = class(TForm)
    sImage1: TImage;
    sLabel1: TLabel;
    sLabel2: TLabel;
    sLabel3: TLabel;
    sLabel4: TLabel;
    sButton1: TButton;
    sButton2: TButton;
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

uses
  UnitMainForm;

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
  ShellExecute(Handle, 'open', 'http://www.ozok26.com/oneway-backup-9', nil, nil, SW_SHOWNORMAL);
end;

end.

