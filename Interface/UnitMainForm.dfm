object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'OneWay Backup'
  ClientHeight = 524
  ClientWidth = 1084
  Color = clBtnFace
  DoubleBuffered = True
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
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1084
    Height = 320
    Align = alClient
    Caption = 'TopPanel'
    TabOrder = 0
    object LeftPanel: TPanel
      Left = 241
      Top = 1
      Width = 842
      Height = 318
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 301
      ExplicitWidth = 782
      object JobsList: TListView
        Left = 1
        Top = 1
        Width = 840
        Height = 316
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Project Name'
            Width = 200
          end
          item
            Caption = 'Kaynak Klas'#246'r'
          end
          item
            Caption = 'Hedef Klas'#246'r'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitWidth = 780
      end
    end
    object RightPanel: TPanel
      Left = 1
      Top = 1
      Width = 240
      Height = 318
      Align = alLeft
      TabOrder = 1
      object RunJobsBtn: TButton
        Left = 1
        Top = 1
        Width = 238
        Height = 40
        Align = alTop
        Caption = 'Run Selected Jobs'
        TabOrder = 0
        OnClick = RunJobsBtnClick
        ExplicitWidth = 298
      end
      object AddNewProjectBtn: TButton
        Left = 1
        Top = 81
        Width = 238
        Height = 40
        Align = alTop
        Caption = 'Add A New Project'
        TabOrder = 1
        OnClick = AddNewProjectBtnClick
        ExplicitTop = 41
        ExplicitWidth = 298
      end
      object StopBtn: TButton
        Left = 1
        Top = 41
        Width = 238
        Height = 40
        Align = alTop
        Caption = 'Stop'
        Enabled = False
        TabOrder = 2
        OnClick = StopBtnClick
        ExplicitLeft = 3
        ExplicitTop = 25
      end
      object EditProjectBtn: TButton
        Left = 1
        Top = 121
        Width = 238
        Height = 40
        Align = alTop
        Caption = 'Edit Selected Project'
        TabOrder = 3
        OnClick = EditProjectBtnClick
        ExplicitLeft = -3
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 320
    Width = 1084
    Height = 204
    Align = alBottom
    TabOrder = 1
    object StateLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 170
      Width = 1076
      Height = 13
      Align = alBottom
      Caption = 'State:'
      ExplicitWidth = 30
    end
    object ProjectNameLabel: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 151
      Width = 1076
      Height = 13
      Align = alBottom
      Caption = 'Current Project:'
      ExplicitWidth = 78
    end
    object LogList: TListBox
      Left = 1
      Top = 1
      Width = 1082
      Height = 147
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object ProgressBar1: TProgressBar
      Left = 1
      Top = 186
      Width = 1082
      Height = 17
      Align = alBottom
      TabOrder = 1
    end
  end
  object SearchFile: TJvSearchFiles
    Options = [soAllowDuplicates, soSearchDirs, soSearchFiles, soIncludeSystemHiddenDirs, soIncludeSystemHiddenFiles]
    DirParams.MinSize = 0
    DirParams.MaxSize = 0
    FileParams.MinSize = 0
    FileParams.MaxSize = 0
    OnFindFile = SearchFileFindFile
    Left = 488
    Top = 160
  end
  object ProgressTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ProgressTimerTimer
    Left = 352
    Top = 200
  end
  object OperationThread: TIdThreadComponent
    Active = False
    Loop = False
    Priority = tpNormal
    StopMode = smTerminate
    OnRun = OperationThreadRun
    Left = 661
    Top = 209
  end
end
