object LogForm: TLogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 478
  ClientWidth = 1008
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LogFilePathLabel: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 462
    Width = 1002
    Height = 13
    Cursor = crHandPoint
    Align = alBottom
    Caption = 'log file link'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = LogFilePathLabelClick
    ExplicitWidth = 49
  end
  object LogList: TListView
    Left = 0
    Top = 66
    Width = 1008
    Height = 393
    Align = alClient
    Columns = <
      item
        Caption = '#'
      end
      item
        Caption = 'Date'
        Width = 110
      end
      item
        Alignment = taCenter
        Caption = 'Type'
      end
      item
        Caption = 'Source File/Message'
        Width = 300
      end
      item
        Alignment = taCenter
        Caption = 'Operation'
        Width = 100
      end
      item
        Caption = 'Destination'
        Width = 300
      end
      item
        Alignment = taCenter
        Caption = 'Reason'
        Width = 140
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GridLines = True
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawSubItem = LogListCustomDrawSubItem
    OnData = LogListData
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 66
    Align = alTop
    AutoSize = True
    TabOrder = 1
    object CopiedFileLabel: TLabel
      Left = 1
      Top = 1
      Width = 1006
      Height = 16
      Align = alTop
      Caption = 'CopiedFileLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 89
    end
    object DeletedFileLabel: TLabel
      Left = 1
      Top = 17
      Width = 1006
      Height = 16
      Align = alTop
      Caption = 'DeletedFileLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 93
    end
    object SkippedFileLabel: TLabel
      Left = 1
      Top = 33
      Width = 1006
      Height = 16
      Align = alTop
      Caption = 'SkippedFileLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 95
    end
    object ErrorLabel: TLabel
      Left = 1
      Top = 49
      Width = 1006
      Height = 16
      Align = alTop
      Caption = 'ErrorLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 59
    end
  end
end
