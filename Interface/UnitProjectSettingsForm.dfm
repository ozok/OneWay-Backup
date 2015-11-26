object ProjectSettingsForm: TProjectSettingsForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Project Settings'
  ClientHeight = 303
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
    303)
  PixelsPerInch = 96
  TextHeight = 13
  object ProjectNameEdit: TsEdit
    AlignWithMargins = True
    Left = 8
    Top = 114
    Width = 677
    Height = 21
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    BoundLabel.Active = True
    BoundLabel.Caption = 'Project Name:'
    BoundLabel.Layout = sclTopLeft
  end
  object SaveProjectBtn: TsButton
    Left = 540
    Top = 270
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
    Width = 342
    Height = 20
    Caption = 
      'Delete files in destination folder if they do not exist in sourc' +
      'e folder'
    TabOrder = 4
    ImgChecked = 0
    ImgUnchecked = 0
  end
  object IgnoreTypesEdit: TsEdit
    AlignWithMargins = True
    Left = 8
    Top = 160
    Width = 677
    Height = 21
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    BoundLabel.Active = True
    BoundLabel.Caption = 'Ignore these file types (separate with ; example: .mp3;.jpeg)'
    BoundLabel.Layout = sclTopLeft
  end
  object SourceDirEdit: TsDirectoryEdit
    AlignWithMargins = True
    Left = 8
    Top = 22
    Width = 593
    Height = 21
    AutoSize = False
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
    BoundLabel.Active = True
    BoundLabel.Caption = 'Source folder:'
    BoundLabel.Layout = sclTopLeft
    GlyphMode.Blend = 0
    GlyphMode.Grayed = False
    Root = 'rfDesktop'
  end
  object DestDirEdit: TsDirectoryEdit
    AlignWithMargins = True
    Left = 8
    Top = 68
    Width = 593
    Height = 21
    AutoSize = False
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
    BoundLabel.Active = True
    BoundLabel.Caption = 'Destination folder:'
    BoundLabel.Layout = sclTopLeft
    GlyphMode.Blend = 0
    GlyphMode.Grayed = False
    Root = 'rfDesktop'
  end
  object BufferEdit: TsSpinEdit
    Left = 117
    Top = 213
    Width = 121
    Height = 21
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    NumbersOnly = True
    ParentFont = False
    TabOrder = 6
    Text = '8192'
    BoundLabel.Active = True
    BoundLabel.Caption = 'Buffer length (bytes):'
    MaxValue = 0
    MinValue = 0
    Value = 8192
  end
  object CompareMethodList: TsComboBox
    Left = 117
    Top = 240
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'File Compare Method:'
    VerticalAlignment = taAlignTop
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 7
    Text = 'Full File Search'
    Items.Strings = (
      'Full File Search'
      'MD5'
      'Compare Sizes')
  end
  object SwapFoldersBTn: TsButton
    Left = 607
    Top = 22
    Width = 78
    Height = 67
    Anchors = [akTop, akRight]
    Caption = 'Switch Source with Destination'
    TabOrder = 8
    OnClick = SwapFoldersBTnClick
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
