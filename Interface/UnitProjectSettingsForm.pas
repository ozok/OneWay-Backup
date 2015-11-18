unit UnitProjectSettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, 
  Vcl.Mask, JvExMask, JvToolEdit, UnitProjectFile, Vcl.ComCtrls, JvSpin,
  sSkinProvider, sCheckBox, sButton, sEdit, sLabel, sSpinEdit, sMaskEdit,
  sCustomComboEdit, sToolEdit, sComboBox;

type
  TProjectSettingsForm = class(TForm)
    ProjectNameEdit: TsEdit;
    SaveProjectBtn: TsButton;
    DeleteFromDestBtn: TsCheckBox;
    IgnoreTypesEdit: TsEdit;
    sSkinProvider1: TsSkinProvider;
    SourceDirEdit: TsDirectoryEdit;
    DestDirEdit: TsDirectoryEdit;
    BufferEdit: TsSpinEdit;
    CompareMethodList: TsComboBox;
    SwapFoldersBTn: TsButton;
    procedure SaveProjectBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SwapFoldersBTnClick(Sender: TObject);
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
    IgnoreTypesEdit.Text := MainForm.FProjects[FItemIndex].IgnoredFileTypes;
    CompareMethodList.ItemIndex := MainForm.FProjects[FItemIndex].CompareMethod;
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
        LProjectFile.IgnoredFileTypes := IgnoreTypesEdit.Text;
        LProjectFile.CompareMethod := CompareMethodList.ItemIndex;
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
        LItem.SubItems.Add(LProjectFile.IgnoredFileTypes);
        LItem.SubItems.Add(MainForm.CompareMethodToStr(LProjectFile.CompareMethod));
        LItem.Checked := LProjectFile.Active;

        MainForm.JobsList.Items.Clear;
        MainForm.FProjects.Clear;
        MainForm.LoadProjects;

        Self.Close;
      end;
    end;
  end;
end;

procedure TProjectSettingsForm.SwapFoldersBTnClick(Sender: TObject);var
  LTmpStr: string;
begin
  LTmpStr := SourceDirEdit.Text;
  SourceDirEdit.Text := DestDirEdit.Text;
  DestDirEdit.TExt := LTmpStr;
end;

end.