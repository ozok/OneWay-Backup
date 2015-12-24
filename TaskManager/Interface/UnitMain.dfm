object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'OneWay Backup - Tasks'
  ClientHeight = 449
  ClientWidth = 559
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    559
    449)
  PixelsPerInch = 96
  TextHeight = 13
  object TasksList: TListView
    Left = 8
    Top = 8
    Width = 543
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
      end
      item
        Caption = 'Start'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
