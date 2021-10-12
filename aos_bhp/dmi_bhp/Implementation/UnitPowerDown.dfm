inherited FormPowerDown: TFormPowerDown
  Caption = 'ATP Power down'
  Color = clBlack
  Font.Name = 'MS Sans Serif'
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPowerDownMessage: TLabel [0]
    Left = 0
    Top = 47
    Width = 261
    Height = 22
    Alignment = taCenter
    Caption = 'ATP Power down in progress...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -19
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
