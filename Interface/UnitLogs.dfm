object LogsForm: TLogsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Logs'
  ClientHeight = 430
  ClientWidth = 1047
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
    Height = 430
    ExplicitLeft = 144
    ExplicitTop = -8
    ExplicitHeight = 403
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 430
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 403
    object LogsList: TListBox
      Left = 1
      Top = 26
      Width = 183
      Height = 403
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = LogsListClick
      ExplicitHeight = 376
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
    end
  end
  object ContentList: TListView
    Left = 188
    Top = 0
    Width = 859
    Height = 430
    Align = alClient
    Columns = <
      item
        Width = 110
      end
      item
        Alignment = taCenter
      end
      item
        Width = 300
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 300
      end
      item
        Alignment = taCenter
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
    TabOrder = 1
    ViewStyle = vsReport
    OnData = ContentListData
    ExplicitLeft = -214
    ExplicitTop = -172
    ExplicitWidth = 1008
    ExplicitHeight = 575
  end
  object Info: TJvComputerInfoEx
    Left = 464
    Top = 184
  end
end
