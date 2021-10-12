inherited FormFullScreenMsg: TFormFullScreenMsg
  BorderIcons = []
  Font.Name = 'MS Sans Serif'
  FormStyle = fsStayOnTop
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMessage: TLabel [0]
    Left = 0
    Top = 47
    Width = 800
    Height = 41
    Alignment = taCenter
    AutoSize = False
    Caption = 'Message'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
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
  object TimerDelayedMessage: TTimer
    Enabled = False
    OnTimer = TimerDelayedMessageTimer
    Left = 8
    Top = 96
  end
end
