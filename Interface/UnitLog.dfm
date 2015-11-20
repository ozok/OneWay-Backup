object LogForm: TLogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 594
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LogFilePathLabel: TsLabel
    AlignWithMargins = True
    Left = 3
    Top = 578
    Width = 1002
    Height = 13
    Cursor = crHandPoint
    Align = alBottom
    Caption = 'log file link'
    Color = clBtnFace
    ParentColor = False
    ParentFont = False
    OnClick = LogFilePathLabelClick
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ExplicitWidth = 49
  end
  object LogList: TsListView
    Left = 0
    Top = 0
    Width = 1008
    Height = 575
    Align = alClient
    Columns = <
      item
        Width = 125
      end
      item
      end>
    GridLines = True
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnData = LogListData
    ExplicitHeight = 594
  end
end
