object FormRadioChannel: TFormRadioChannel
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Radio Area'
  ClientHeight = 129
  ClientWidth = 330
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 25
  object LableRadioChannel: TLabel
    Left = 20
    Top = 15
    Width = 102
    Height = 20
    Alignment = taCenter
    Caption = 'Radio Channel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = 20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object BitBtnOk: TBitBtn
    Left = 114
    Top = 61
    Width = 100
    Height = 60
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    Spacing = 8
    TabOrder = 0
    OnClick = BitBtnOkClick
  end
  object EditRadioChannel: TEdit
    Left = 132
    Top = 12
    Width = 190
    Height = 28
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = 20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
end
