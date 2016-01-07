object EmailConfForm: TEmailConfForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Settings'
  ClientHeight = 304
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 375
    Height = 304
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      object CheckUpdateBtn: TCheckBox
        Left = 20
        Top = 3
        Width = 165
        Height = 17
        Caption = 'Check updates on start up'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Logs'
      ImageIndex = 1
      DesignSize = (
        367
        276)
      object Label8: TLabel
        Left = 20
        Top = 6
        Width = 102
        Height = 13
        Caption = 'Log file name format:'
      end
      object LogFilePatterList: TComboBox
        Left = 128
        Top = 3
        Width = 236
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 0
        Text = 'YYYYMMDDHHMMSS'
        Items.Strings = (
          'YYYYMMDDHHMMSS'
          'YYYY-MM-DD-HH-MM-SS'
          'MMDDYYYYHHMMSS'
          'MM-DD-YYYY-HH-MM-SS'
          'DDMMYYYYHHMMSS'
          'DD-MM-YYYY-HH-MM-SS')
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Email Settings'
      DesignSize = (
        367
        276)
      object Label1: TLabel
        Left = 30
        Top = 11
        Width = 28
        Height = 13
        Caption = 'From:'
      end
      object Label2: TLabel
        Left = 42
        Top = 38
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object Label3: TLabel
        Left = 32
        Top = 75
        Width = 26
        Height = 13
        Caption = 'Host:'
      end
      object Label4: TLabel
        Left = 34
        Top = 100
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object Label5: TLabel
        Left = 8
        Top = 142
        Width = 55
        Height = 13
        Caption = 'User name:'
      end
      object Label6: TLabel
        Left = 8
        Top = 167
        Width = 50
        Height = 13
        Caption = 'Password:'
      end
      object Label7: TLabel
        Left = 21
        Top = 194
        Width = 37
        Height = 13
        Caption = 'Report:'
      end
      object sLabel1: TLabel
        Left = 64
        Top = 218
        Width = 251
        Height = 14
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Your password will be saved in plain text'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
      end
      object FromEdit: TEdit
        Left = 64
        Top = 8
        Width = 297
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object HostEdit: TEdit
        Left = 64
        Top = 70
        Width = 297
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object PassEdit: TEdit
        Left = 64
        Top = 164
        Width = 297
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PasswordChar = '*'
        TabOrder = 2
      end
      object PortEdit: TJvSpinEdit
        Left = 64
        Top = 97
        Width = 121
        Height = 21
        CheckMinValue = True
        ButtonKind = bkClassic
        Value = 25.000000000000000000
        TabOrder = 3
      end
      object ReportTypeList: TComboBox
        Left = 64
        Top = 191
        Width = 297
        Height = 21
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 4
        Text = 'HTML Zipped Attachment'
        Items.Strings = (
          'CSV Zipped Attachment'
          'HTML EMail Body'
          'HTML Zipped Attachment')
      end
      object SendTestBtn: TButton
        Left = 3
        Top = 248
        Width = 97
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Send Test Mail'
        TabOrder = 5
        OnClick = SendTestBtnClick
      end
      object ToEdit: TEdit
        Left = 64
        Top = 35
        Width = 297
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
      object UserNameEdit: TEdit
        Left = 64
        Top = 137
        Width = 297
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 7
      end
    end
  end
  object IdMessage1: TIdMessage
    AttachmentEncoding = 'UUE'
    BccList = <>
    CCList = <>
    Encoding = meDefault
    FromList = <
      item
      end>
    Recipients = <>
    ReplyTo = <>
    ConvertPreamble = True
    Left = 245
    Top = 100
  end
  object IdSMTP1: TIdSMTP
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    SASLMechanisms = <>
    Left = 325
    Top = 116
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    Destination = ':25'
    MaxLineAction = maException
    Port = 25
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 133
    Top = 104
  end
end
