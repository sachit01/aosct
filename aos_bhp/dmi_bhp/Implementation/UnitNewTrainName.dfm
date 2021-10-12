object FormNewTrainName: TFormNewTrainName
  Left = 10
  Top = 10
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'New train name'
  ClientHeight = 112
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 25
  object BitBtnOk: TBitBtn
    Left = 255
    Top = 8
    Width = 105
    Height = 42
    DoubleBuffered = True
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = BitBtnOkClick
  end
  object EditTrainName: TEdit
    Left = 8
    Top = 17
    Width = 241
    Height = 33
    TabOrder = 0
    OnChange = EditTrainNameChange
    OnEnter = EditTrainNameEnter
    OnExit = EditTrainNameExit
  end
  object BitBtnCancel: TBitBtn
    Left = 255
    Top = 56
    Width = 105
    Height = 42
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = BitBtnCancelClick
  end
end
