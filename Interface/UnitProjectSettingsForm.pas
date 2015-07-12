unit UnitProjectSettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, UnitProjectFile, Vcl.ComCtrls;

type
  TProjectSettingsForm = class(TForm)
    DestDirEdit: TJvDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ProjectNameEdit: TEdit;
    SaveProjectBtn: TButton;
    SourceDirEdit: TJvDirectoryEdit;
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FProjectFile: string;
  end;

var
  ProjectSettingsForm: TProjectSettingsForm;

implementation

uses UnitMainForm;

{$R *.dfm}

procedure TProjectSettingsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MainForm.Enabled := True;
  MainForm.BringToFront;
end;

procedure TProjectSettingsForm.FormShow(Sender: TObject);
var
  LProjectFile: TProjectFile;
begin
  if Length(FProjectFile) > 0 then
  begin
    LProjectFile := TProjectFile.Create(FProjectFile);
    SourceDirEdit.Text := LProjectFile.SourceFolder;
    DestDirEdit.Text := LProjectFile.DestFolder;
    ProjectNameEdit.Text := LProjectFile.ProjectName;
  end;
end;

procedure TProjectSettingsForm.SaveProjectBtnClick(Sender: TObject);
var
  LProjectFile: TProjectFile;
  LGUID: TGUID;
  LItem: TListItem;
  LProjectFilePath: string;
begin
  if Length(SourceDirEdit.Text) > 0 then
  begin
    if Length(DestDirEdit.Text) > 0 then
    begin
      if Length(ProjectNameEdit.Text) > 0 then
      begin
        if Length(FProjectFile) > 0 then
        begin
          LProjectFilePath := FProjectFile;
        end
        else
        begin
          CreateGUID(LGUID);
          LProjectFilePath := ExtractFileDir(Application.ExeName) + '\jobs\' + GUIDToString(LGUID) + '.ini';
        end;

        LProjectFile := TProjectFile.Create(LProjectFilePath);
        LProjectFile.SourceFolder := SourceDirEdit.Text;
        LProjectFile.DestFolder := DestDirEdit.Text;
        LProjectFile.ProjectName := ProjectNameEdit.Text;
        LProjectFile.Save();
        MainForm.FProjects.Add(LProjectFile);

        LItem := MainForm.JobsList.Items.Add;
        LItem.Caption := LProjectFile.ProjectName;
        LItem.SubItems.Add(LProjectFile.SourceFolder);
        LItem.SubItems.Add(LProjectFile.DestFolder);
        LItem.Checked := True;
        Self.Close;
      end;
    end;
  end;
end;

end.
