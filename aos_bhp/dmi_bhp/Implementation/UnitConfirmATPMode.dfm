object FormConfirmNewATPMode: TFormConfirmNewATPMode
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Confirm new ATP Mode'
  ClientHeight = 83
  ClientWidth = 280
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
  object BitBtnOK: TBitBtn
    Left = 194
    Top = 15
    Width = 81
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
  object LabeledEditATPMode: TLabeledEdit
    Left = 8
    Top = 38
    Width = 169
    Height = 33
    EditLabel.Width = 142
    EditLabel.Height = 25
    EditLabel.Caption = 'New ATP Mode'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -21
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
end
