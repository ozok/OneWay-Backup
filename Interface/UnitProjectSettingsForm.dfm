object ProjectSettingsForm: TProjectSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project Settings'
  ClientHeight = 269
  ClientWidth = 693
  Color = 13353918
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
    269)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TsLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Source Folder:'
    ExplicitWidth = 70
  end
  object Label2: TsLabel
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Destination folder:'
    ExplicitWidth = 89
  end
  object Label4: TsLabel
    AlignWithMargins = True
    Left = 3
    Top = 95
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Project Name:'
    ExplicitWidth = 68
  end
  object Label3: TsLabel
    Left = 8
    Top = 213
    Width = 103
    Height = 13
    Caption = 'Buffer Length (bayt):'
  end
  object Label5: TsLabel
    AlignWithMargins = True
    Left = 3
    Top = 141
    Width = 687
    Height = 13
    Align = alTop
    Caption = 'Ignore these file types (separate with ; example: .mp3;.jpeg)'
    ExplicitWidth = 296
  end
  object ProjectNameEdit: TsEdit
    AlignWithMargins = True
    Left = 3
    Top = 114
    Width = 687
    Height = 21
    Align = alTop
    Color = 15921906
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object SaveProjectBtn: TsButton
    Left = 560
    Top = 236
    Width = 125
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 5
    OnClick = SaveProjectBtnClick
  end
  object DeleteFromDestBtn: TsCheckBox
    Left = 8
    Top = 187
    Width = 349
    Height = 19
    Caption = 
      'Delete files in destination folder if they do not exist in sourc' +
      'e folder'
    TabOrder = 4
    ImgChecked = 0
    ImgUnchecked = 0
  end
  object IgnoreTypesEdit: TsEdit
    AlignWithMargins = True
    Left = 3
    Top = 160
    Width = 687
    Height = 21
    Align = alTop
    Color = 15921906
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object SourceDirEdit: TsDirectoryEdit
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 687
    Height = 21
    Align = alTop
    AutoSize = False
    Color = 15921906
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 0
    Text = ''
    CheckOnExit = True
    GlyphMode.Blend = 0
    GlyphMode.Grayed = False
    Root = 'rfDesktop'
    ExplicitLeft = 8
    ExplicitTop = 14
    ExplicitWidth = 693
  end
  object DestDirEdit: TsDirectoryEdit
    AlignWithMargins = True
    Left = 3
    Top = 68
    Width = 687
    Height = 21
    Align = alTop
    AutoSize = False
    Color = 15921906
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 1
    Text = ''
    CheckOnExit = True
    GlyphMode.Blend = 0
    GlyphMode.Grayed = False
    Root = 'rfDesktop'
    ExplicitLeft = 8
    ExplicitTop = 60
  end
  object BufferEdit: TsSpinEdit
    Left = 117
    Top = 210
    Width = 121
    Height = 21
    Alignment = taCenter
    Color = 15921906
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 6
    Text = '8192'
    MaxValue = 0
    MinValue = 0
    Value = 8192
  end
  object sSkinProvider1: TsSkinProvider
    AddedTitle.Font.Charset = DEFAULT_CHARSET
    AddedTitle.Font.Color = clNone
    AddedTitle.Font.Height = -11
    AddedTitle.Font.Name = 'Tahoma'
    AddedTitle.Font.Style = []
    FormHeader.AdditionalHeight = 0
    SkinData.SkinSection = 'FORM'
    TitleButtons = <>
    Left = 368
    Top = 152
  end
end
