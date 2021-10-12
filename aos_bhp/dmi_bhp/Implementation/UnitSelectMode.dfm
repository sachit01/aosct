object FormSelectMode: TFormSelectMode
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Select ATP Mode'
  ClientHeight = 472
  ClientWidth = 178
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
  object ButtonYard: TButton
    Left = 16
    Top = 16
    Width = 145
    Height = 100
    Caption = 'Yard'
    TabOrder = 0
    OnClick = ButtonYardClick
  end
  object ButtonConfiguration: TButton
    Left = 16
    Top = 127
    Width = 145
    Height = 100
    Caption = 'Configuration'
    TabOrder = 1
    OnClick = ButtonConfigurationClick
  end
  object ButtonPossession: TButton
    Left = 16
    Top = 238
    Width = 145
    Height = 100
    Caption = 'Possession'
    TabOrder = 2
    OnClick = ButtonPossessionClick
  end
  object ButtonShunting: TButton
    Left = 16
    Top = 354
    Width = 145
    Height = 100
    Caption = 'Shunting'
    TabOrder = 3
    OnClick = ButtonShuntingClick
  end
end
