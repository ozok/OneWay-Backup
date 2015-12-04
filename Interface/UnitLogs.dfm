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
    Left = 185
    Top = 0
    Height = 403
    ExplicitLeft = 144
    ExplicitTop = -8
  end
  object ContentList: TListView
    Left = 188
    Top = 0
    Width = 606
    Height = 403
    Align = alClient
    Columns = <
      item
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ContentListData
    ExplicitLeft = 204
    ExplicitWidth = 590
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 403
    Align = alLeft
    TabOrder = 1
    ExplicitLeft = 19
    ExplicitTop = 8
    object LogsList: TListBox
      Left = 1
      Top = 26
      Width = 183
      Height = 376
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = LogsListClick
      ExplicitLeft = -16
      ExplicitTop = 1
      ExplicitWidth = 201
      ExplicitHeight = 401
    end
    object RefreshBtn: TButton
      Left = 1
      Top = 1
      Width = 183
      Height = 25
      Align = alTop
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = RefreshBtnClick
      ExplicitLeft = 56
      ExplicitTop = 192
      ExplicitWidth = 75
    end
  end
  object Info: TJvComputerInfoEx
    Left = 464
    Top = 184
  end
end
