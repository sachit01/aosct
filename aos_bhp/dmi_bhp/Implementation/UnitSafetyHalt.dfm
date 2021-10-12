inherited FormSafetyHalt: TFormSafetyHalt
  BorderIcons = []
  Caption = 'Please restart ATP'
  Color = clBlack
  Font.Color = clWhite
  Font.Height = -16
  Font.Name = 'MS Sans Serif'
  FormStyle = fsStayOnTop
  Scaled = False
  Touch.GestureManager = GestureManager
  OnGesture = FormGesture
  PixelsPerInch = 96
  TextHeight = 20
  object FatalLabel1: TLabel [0]
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
  object FatalLabel2: TLabel [1]
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
  inherited GestureManager: TGestureManager
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
