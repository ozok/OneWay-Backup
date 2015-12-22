object EmailConfForm: TEmailConfForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Email Configuration'
  ClientHeight = 278
  ClientWidth = 369
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
  DesignSize = (
    369
    278)
  PixelsPerInch = 96
  TextHeight = 13
  object sLabel1: TLabel
    Left = 64
    Top = 218
    Width = 281
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '!!!Your password will be saved in plain text!!!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
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
  object FromEdit: TEdit
    Left = 64
    Top = 8
    Width = 291
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
  object ToEdit: TEdit
    Left = 64
    Top = 35
    Width = 291
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
  object HostEdit: TEdit
    Left = 64
    Top = 70
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object UserNameEdit: TEdit
    Left = 64
    Top = 137
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object PassEdit: TEdit
    Left = 64
    Top = 164
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 5
  end
  object SaveBtn: TButton
    Left = 286
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 6
    OnClick = SaveBtnClick
    ExplicitTop = 223
  end
  object CancelBtn: TButton
    Left = 205
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = CancelBtnClick
    ExplicitTop = 223
  end
  object SendTestBtn: TButton
    Left = 8
    Top = 245
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Send Test Mail'
    TabOrder = 8
    OnClick = SendTestBtnClick
    ExplicitTop = 223
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
    Width = 291
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 9
    Text = 'HTML'
    Items.Strings = (
      'CSV'
      'HTML'
      'HTML Attachment')
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
    Left = 269
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
    Left = 181
    Top = 104
  end
end
