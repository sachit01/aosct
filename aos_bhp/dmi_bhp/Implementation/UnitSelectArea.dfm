object FormSelectArea: TFormSelectArea
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Select area'
  ClientHeight = 112
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 25
  object ComboBoxArea: TComboBox
    Left = 8
    Top = 24
    Width = 290
    Height = 47
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = ComboBoxAreaChange
    Items.Strings = (
      '1')
  end
  object BitBtnOk: TBitBtn
    Left = 304
    Top = 24
    Width = 105
    Height = 73
    DoubleBuffered = True
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtnOkClick
  end
end
