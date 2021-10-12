object FormConfirmDepartureTest: TFormConfirmDepartureTest
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Departure Test'
  ClientHeight = 118
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 25
  object LableConfirmDepTest: TLabel
    Left = 40
    Top = 8
    Width = 221
    Height = 25
    Caption = 'Confirm Departure Test'
  end
  object BitBtnOk: TBitBtn
    Left = 100
    Top = 54
    Width = 100
    Height = 60
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = BitBtnOkClick
  end
end
