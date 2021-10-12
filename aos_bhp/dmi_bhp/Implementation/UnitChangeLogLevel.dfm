object FormChangeLogLevel: TFormChangeLogLevel
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Change log level'
  ClientHeight = 114
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 19
  object LabelLogLevel: TLabel
    Left = 8
    Top = 13
    Width = 63
    Height = 19
    Caption = 'Log level'
  end
  object ComboBoxLogLevel: TComboBox
    Left = 8
    Top = 38
    Width = 202
    Height = 27
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      'Do not log to N-JRU'
      'Normal'
      'Detailed'
      'Debug')
  end
  object BitBtnOk: TBitBtn
    Left = 216
    Top = 39
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object BitBtnCancel: TBitBtn
    Left = 216
    Top = 71
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
end
