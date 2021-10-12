object FormConfirmFreeRollingClear: TFormConfirmFreeRollingClear
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Confirm Free Rolling Clear'
  ClientHeight = 118
  ClientWidth = 443
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
  object LableConfirmFreeRollingClear: TLabel
    Left = 0
    Top = 0
    Width = 443
    Height = 25
    Align = alTop
    Alignment = taCenter
    Caption = 'Confirm that train is no longer in Free Rolling'
    ExplicitTop = 8
    ExplicitWidth = 473
  end
  object BitBtnOk: TBitBtn
    Left = 174
    Top = 50
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
