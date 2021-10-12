object FormConfirmBrakeTest: TFormConfirmBrakeTest
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Start brake test'
  ClientHeight = 122
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 25
  object LabelStartBrakeTest: TLabel
    Left = 70
    Top = 8
    Width = 155
    Height = 25
    Caption = 'Start brake test?'
  end
  object BitBtnOk: TBitBtn
    Left = 5
    Top = 58
    Width = 100
    Height = 60
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = BitBtnOkClick
  end
  object BitBtnCancel: TBitBtn
    Left = 186
    Top = 54
    Width = 100
    Height = 60
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtnCancelClick
  end
end
