object FormViewLog: TFormViewLog
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Communication Lost'
  ClientHeight = 480
  ClientWidth = 638
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  Touch.GestureManager = GestureManager
  OnCreate = FormCreate
  OnGesture = FormGesture
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 638
    Height = 480
    Align = alClient
    TabOrder = 0
    object Memo: TMemo
      Left = 1
      Top = 1
      Width = 636
      Height = 440
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial monospaced for SAP'
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      Touch.GestureManager = GestureManager
      OnGesture = FormGesture
    end
    object BitBtnCancel: TBitBtn
      Left = 301
      Top = 441
      Width = 75
      Height = 38
      Align = alLeft
      Caption = 'Hide'
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = BitBtnCancelClick
    end
    object ButtonBottom: TButton
      Left = 76
      Top = 441
      Width = 75
      Height = 38
      Action = ActionBottom
      Align = alLeft
      TabOrder = 2
    end
    object ButtonTop: TButton
      Left = 1
      Top = 441
      Width = 75
      Height = 38
      Action = ActionTop
      Align = alLeft
      TabOrder = 1
    end
    object ButtonPrevPage: TButton
      Left = 151
      Top = 441
      Width = 75
      Height = 38
      Action = ActionPrevPage
      Align = alLeft
      TabOrder = 4
    end
    object ButtonNextPage: TButton
      Left = 226
      Top = 441
      Width = 75
      Height = 38
      Action = ActionNextPage
      Align = alLeft
      TabOrder = 5
    end
  end
  object GestureManager: TGestureManager
    Left = 552
    Top = 440
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
      end
      item
        Control = Memo
        Collection = <
          item
            GestureID = sgiLeft
          end
          item
            GestureID = sgiRight
          end>
      end>
  end
  object ActionManager: TActionManager
    Left = 488
    Top = 440
    StyleName = 'Platform Default'
    object ActionTop: TAction
      Caption = 'Top'
      OnExecute = ActionTopExecute
    end
    object ActionBottom: TAction
      Caption = 'Bottom'
      OnExecute = ActionBottomExecute
    end
    object ActionPrevPage: TAction
      Caption = 'Prev Page'
      OnExecute = ActionPrevPageExecute
    end
    object ActionNextPage: TAction
      Caption = 'Next Page'
      OnExecute = ActionNextPageExecute
    end
  end
end
