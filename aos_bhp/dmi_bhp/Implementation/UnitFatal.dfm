object FormFatal: TFormFatal
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Please restart ATP'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -16
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  Touch.GestureManager = GestureManager
  OnCreate = FormCreate
  OnGesture = FormGesture
  PixelsPerInch = 96
  TextHeight = 20
  object FatalLabel1: TLabel
    Left = 0
    Top = 96
    Width = 128
    Height = 18
    Alignment = taCenter
    Caption = 'Safety monitor halt'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object FatalLabel2: TLabel
    Left = 0
    Top = 128
    Width = 130
    Height = 18
    Alignment = taCenter
    Caption = 'Please restart ATP'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object GestureManager: TGestureManager
    Left = 496
    Top = 16
    GestureData = <
      item
        Control = Owner
        Collection = <
          item
            GestureID = sgiLeft
          end
          item
            GestureID = sgiRight
          end>
      end>
  end
end
