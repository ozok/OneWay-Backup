object ProjectSettingsForm: TProjectSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project Settings'
  ClientHeight = 173
  ClientWidth = 693
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    693
    173)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Source Folder:'
    ExplicitLeft = 4
    ExplicitTop = 90
    ExplicitWidth = 70
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Destination folder:'
    ExplicitLeft = 4
    ExplicitTop = 136
    ExplicitWidth = 89
  end
  object Label4: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 95
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Project Name:'
    ExplicitLeft = 4
    ExplicitTop = 182
    ExplicitWidth = 68
  end
  object DestDirEdit: TJvDirectoryEdit
    AlignWithMargins = True
    Left = 3
    Top = 68
    Width = 687
    Height = 21
    Align = alTop
    DialogKind = dkWin32
    TabOrder = 1
    Text = ''
    ExplicitLeft = 4
    ExplicitTop = 155
    ExplicitWidth = 292
  end
  object ProjectNameEdit: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 114
    Width = 687
    Height = 21
    Align = alTop
    TabOrder = 2
    ExplicitLeft = 4
    ExplicitTop = 201
    ExplicitWidth = 292
  end
  object SaveProjectBtn: TButton
    Left = 560
    Top = 141
    Width = 125
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 3
    OnClick = SaveProjectBtnClick
  end
  object SourceDirEdit: TJvDirectoryEdit
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 687
    Height = 21
    Align = alTop
    DialogKind = dkWin32
    TabOrder = 0
    Text = ''
    ExplicitLeft = 4
    ExplicitTop = 109
    ExplicitWidth = 292
  end
end
