inherited FormLogin: TFormLogin
  Caption = 'FormLogin'
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object NameLabel: TLabel [0]
    Left = 155
    Top = 96
    Width = 55
    Height = 22
    Alignment = taRightJustify
    Caption = 'Name:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object PasswordLabel: TLabel [1]
    Left = 123
    Top = 136
    Width = 89
    Height = 22
    Alignment = taRightJustify
    Caption = 'Password:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  inherited Information: TMMIPanel
    TabOrder = 8
  end
  inherited TextListArea: TMMIPanel
    TabOrder = 9
  end
  inherited BitBtnMenu: TBitBtn
    TabOrder = 10
  end
  object NameEdit: TEdit [6]
    Left = 230
    Top = 96
    Width = 209
    Height = 30
    AutoSelect = False
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    MaxLength = 20
    ParentFont = False
    TabOrder = 0
    OnClick = NameEditClick
    OnKeyPress = NameEditKeyPress
  end
  object PasswordEdit: TEdit [7]
    Left = 230
    Top = 134
    Width = 209
    Height = 30
    AutoSelect = False
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    MaxLength = 20
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 1
    OnClick = PasswordEditClick
    OnKeyPress = PasswordEditKeyPress
  end
  object LoginButton: TColorButton [8]
    Left = 480
    Top = 96
    Width = 116
    Height = 44
    BevelSize = 5
    Caption = 'Login'
    Highlight = 16723245
    shadow = 6946816
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    Style = [bsCenter, bsShowFocus, bsShowKey]
    TabOrder = 2
    TabStop = True
    OnClick = LoginButtonClick
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
  object TimerLoginReenable: TTimer
    Enabled = False
    Interval = 15000
    OnTimer = TimerLoginReenableTimer
    Left = 480
    Top = 32
  end
end
