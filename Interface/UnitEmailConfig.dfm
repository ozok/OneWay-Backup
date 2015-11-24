object EmailConfForm: TEmailConfForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Email Configuration'
  ClientHeight = 256
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
  OnShow = FormShow
  DesignSize = (
    369
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object sLabel1: TsLabel
    Left = 64
    Top = 191
    Width = 281
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '!!!Your password will be saved in plain text!!!'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
  end
  object FromEdit: TsEdit
    Left = 64
    Top = 8
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    BoundLabel.Active = True
    BoundLabel.Caption = 'From:'
  end
  object ToEdit: TsEdit
    Left = 64
    Top = 35
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    BoundLabel.Active = True
    BoundLabel.Caption = 'To:'
  end
  object HostEdit: TsEdit
    Left = 64
    Top = 70
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    BoundLabel.Active = True
    BoundLabel.Caption = 'Host:'
  end
  object UserNameEdit: TsEdit
    Left = 64
    Top = 137
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    BoundLabel.Active = True
    BoundLabel.Caption = 'User Name:'
  end
  object PassEdit: TsEdit
    Left = 64
    Top = 164
    Width = 291
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 4
    BoundLabel.Active = True
    BoundLabel.Caption = 'Password:'
  end
  object PortEdit: TsSpinEdit
    Left = 64
    Top = 97
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 5
    Text = '25'
    BoundLabel.Active = True
    BoundLabel.Caption = 'Port:'
    MaxValue = 0
    MinValue = 0
    Value = 25
  end
  object SaveBtn: TsButton
    Left = 286
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 6
    OnClick = SaveBtnClick
  end
  object CancelBtn: TsButton
    Left = 205
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = CancelBtnClick
  end
  object SendTestBtn: TsButton
    Left = 8
    Top = 223
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Send Test Mail'
    TabOrder = 8
    OnClick = SendTestBtnClick
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
    Left = 181
    Top = 68
  end
  object IdSMTP1: TIdSMTP
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    SASLMechanisms = <>
    Left = 117
    Top = 68
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
    Top = 136
  end
end
