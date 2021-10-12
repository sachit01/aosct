inherited FormSleepingMode: TFormSleepingMode
  Caption = ''
  ExplicitWidth = 800
  ExplicitHeight = 600
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSleeping: TLabel [0]
    Left = 8
    Top = 72
    Width = 265
    Height = 30
    AutoSize = False
    Caption = 'ATP Sleeping '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -25
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
