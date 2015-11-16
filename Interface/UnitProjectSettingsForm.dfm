object ProjectSettingsForm: TProjectSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project Settings'
  ClientHeight = 274
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
    274)
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
    Color = 16119285
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object SaveProjectBtn: TsButton
    Left = 540
    Top = 241
    Width = 145
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 5
    OnClick = SaveProjectBtnClick
  end
  object DeleteFromDestBtn: TsCheckBox
    Left = 8
    Top = 187
    Width = 347
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
    Color = 16119285
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
    Color = 16119285
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
  end
  object DestDirEdit: TsDirectoryEdit
    AlignWithMargins = True
    Left = 3
    Top = 68
    Width = 687
    Height = 21
    Align = alTop
    AutoSize = False
    Color = 16119285
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
  end
  object BufferEdit: TsSpinEdit
    Left = 117
    Top = 210
    Width = 121
    Height = 21
    Alignment = taCenter
    Color = 16119285
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
  object CompareMethodList: TsComboBox
    Left = 540
    Top = 210
    Width = 145
    Height = 21
    Anchors = [akTop, akRight]
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'File Compare Method:'
    VerticalAlignment = taAlignTop
    Style = csDropDownList
    Color = 16119285
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 16119285
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    ParentFont = False
    TabOrder = 7
    Text = 'Full File Search'
    Items.Strings = (
      'Full File Search'
      'MD5'
      'Compare Sizes')
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
    Left = 376
    Top = 128
  end
end
