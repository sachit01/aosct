object FormCommLost: TFormCommLost
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Communication Lost'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBlack
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CommLostLabel: TLabel
    Left = 0
    Top = 120
    Width = 800
    Height = 27
    Alignment = taCenter
    Caption = 'Communication with ATP Lost'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
end
