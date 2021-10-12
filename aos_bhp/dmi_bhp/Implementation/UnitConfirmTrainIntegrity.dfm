object FormConfirmTrainIntegrity: TFormConfirmTrainIntegrity
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Train Integrity'
  ClientHeight = 118
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 27
  object LabelConfirmTIMS: TLabel
    Left = 40
    Top = 8
    Width = 221
    Height = 27
    Caption = 'Confirm Train Integrity'
  end
  object BitBtnOK: TBitBtn
    Left = 8
    Top = 50
    Width = 100
    Height = 60
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtnOKClick
  end
  object BitBtnCancel: TBitBtn
    Left = 189
    Top = 50
    Width = 100
    Height = 60
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 1
    OnClick = BitBtnCancelClick
  end
end
