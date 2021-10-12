inherited FormUnreg: TFormUnreg
  Caption = 'Please restart ATP'
  Font.Name = 'MS Sans Serif'
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object UnregLabel1: TLabel [0]
    Left = 0
    Top = 104
    Width = 412
    Height = 27
    Alignment = taCenter
    Caption = 'Unregistration received from stationary'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -24
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
