object FormConfirmDopplerRadarFailure: TFormConfirmDopplerRadarFailure
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Confirm Doppler radar failure'
  ClientHeight = 124
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDopplerRadarFailure: TLabel
    Left = 0
    Top = 20
    Width = 322
    Height = 24
    Alignment = taCenter
    AutoSize = False
    Caption = 'Confirm Doppler radar failure '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object BitBtnOk: TBitBtn
    Left = 111
    Top = 54
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
    OnClick = BitBtnOkClick
  end
end
