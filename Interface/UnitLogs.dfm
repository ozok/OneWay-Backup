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
  OnClose = FormClose
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
    object LogsList: TListBox
      Left = 1
      Top = 76
      Width = 183
      Height = 353
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = LogsListClick
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
    object OpenLogBtn: TButton
      Left = 1
      Top = 26
      Width = 183
      Height = 25
      Align = alTop
      Caption = 'Open Selected Log'
      TabOrder = 2
      OnClick = OpenLogBtnClick
    end
    object OpenLogFolderBtn: TButton
      Left = 1
      Top = 51
      Width = 183
      Height = 25
      Align = alTop
      Caption = 'Open Log Folder'
      TabOrder = 3
      OnClick = OpenLogFolderBtnClick
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
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawSubItem = ContentListCustomDrawSubItem
    OnData = ContentListData
  end
  object Info: TJvComputerInfoEx
    Left = 464
    Top = 184
  end
end
