object LogsForm: TLogsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Logs'
  ClientHeight = 403
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 0
    Height = 403
    ExplicitLeft = 144
    ExplicitTop = -8
  end
  object LogsList: TListBox
    Left = 0
    Top = 0
    Width = 201
    Height = 403
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = LogsListClick
  end
  object ContentList: TListView
    Left = 204
    Top = 0
    Width = 590
    Height = 403
    Align = alClient
    Columns = <
      item
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnData = ContentListData
    ExplicitLeft = 207
  end
  object Info: TJvComputerInfoEx
    Left = 464
    Top = 184
  end
end
