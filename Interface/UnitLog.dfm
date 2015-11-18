object LogForm: TLogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Preview Result'
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
  PixelsPerInch = 96
  TextHeight = 13
  object LogList: TsListView
    Left = 0
    Top = 0
    Width = 1008
    Height = 594
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
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 992
    ExplicitHeight = 529
  end
end
