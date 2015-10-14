object ProjectSettingsForm: TProjectSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project Settings'
  ClientHeight = 217
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
    217)
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
    ExplicitWidth = 68
  end
  object Label3: TLabel
    Left = 8
    Top = 167
    Width = 103
    Height = 13
    Caption = 'Buffer Length (bayt):'
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
  end
  object ProjectNameEdit: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 114
    Width = 687
    Height = 21
    Align = alTop
    TabOrder = 2
  end
  object SaveProjectBtn: TButton
    Left = 560
    Top = 184
    Width = 125
    Height = 25
    Anchors = [akRight, akBottom]
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
  end
  object DeleteFromDestBtn: TCheckBox
    Left = 8
    Top = 141
    Width = 369
    Height = 17
    Caption = 
      'Delete files in destination folder if they do not exist in sourc' +
      'e folder'
    TabOrder = 4
  end
  object BufferEdit: TJvSpinEdit
    Left = 117
    Top = 164
    Width = 100
    Height = 21
    CheckOptions = [coCheckOnExit, coCropBeyondLimit]
    CheckMaxValue = False
    Alignment = taCenter
    ButtonKind = bkClassic
    MinValue = 1.000000000000000000
    Value = 8192.000000000000000000
    TabOrder = 5
  end
end
