unit UnitProjectSettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, UnitProjectFile, Vcl.ComCtrls, JvSpin;

type
  TProjectSettingsForm = class(TForm)
    DestDirEdit: TJvDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ProjectNameEdit: TEdit;
    SaveProjectBtn: TButton;
    SourceDirEdit: TJvDirectoryEdit;
    DeleteFromDestBtn: TCheckBox;
    BufferEdit: TJvSpinEdit;
    Label3: TLabel;
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FItemIndex: integer;
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
begin
  if FItemIndex > -1 then
  begin
    SourceDirEdit.Text := MainForm.FProjects[FItemIndex].SourceFolder;
    DestDirEdit.Text := MainForm.FProjects[FItemIndex].DestFolder;
    ProjectNameEdit.Text := MainForm.FProjects[FItemIndex].ProjectName;
    DeleteFromDestBtn.Checked := MainForm.FProjects[FItemIndex].DeleteFromDest;
    BufferEdit.Value := MainForm.FProjects[FItemIndex].BufferSize;
  end;
end;

procedure TProjectSettingsForm.SaveProjectBtnClick(Sender: TObject);
var
  LProjectFile: TProjectFile;
  LItem: TListItem;
begin
  if Length(SourceDirEdit.Text) > 0 then
  begin
    if Length(DestDirEdit.Text) > 0 then
    begin
      if Length(ProjectNameEdit.Text) > 0 then
      begin
        LProjectFile := TProjectFile.Create();
        LProjectFile.SourceFolder := SourceDirEdit.Text;
        LProjectFile.DestFolder := DestDirEdit.Text;
        LProjectFile.ProjectName := ProjectNameEdit.Text;
        LProjectFile.DeleteFromDest := DeleteFromDestBtn.Checked;
        LProjectFile.BufferSize := Round(BufferEdit.Value);
        LProjectFile.Active := True;
        if FItemIndex > -1 then
        begin
          MainForm.FProjects[FItemIndex] := LProjectFile;
        end
        else
        begin
          MainForm.FProjects.Add(LProjectFile);
        end;
        MainForm.SaveProjects;
        if FItemIndex = -1 then
        begin
          LItem := MainForm.JobsList.Items.Add;
        end
        else
        begin
          LItem := MainForm.JobsList.Items[FItemIndex];
        end;

        LItem.Caption := LProjectFile.ProjectName;
        LItem.SubItems.Add(LProjectFile.SourceFolder);
        LItem.SubItems.Add(LProjectFile.DestFolder);
        LItem.SubItems.Add(LProjectFile.BufferSize.ToString());
        LItem.Checked := LProjectFile.Active;
        Self.Close;
      end;
    end;
  end;
end;

end.
