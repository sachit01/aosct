object FormMainLayer1: TFormMainLayer1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'FormMainLayer1'
  ClientHeight = 600
  ClientWidth = 800
  Color = 34607120
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Information: TMMIPanel
    Left = 425
    Top = 500
    Width = 375
    Height = 100
    Caption = 'Information'
    Color = 36712496
    TabOrder = 0
    PaletteToUse = 0
    object MMIPanelE14: TMMIPanel
      Left = 0
      Top = 0
      Width = 160
      Height = 50
      Color = 36712496
      TabOrder = 0
      PaletteToUse = 0
      object LabelTrainId: TLabel
        Left = 1
        Top = 1
        Width = 158
        Height = 48
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clOlive
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = True
        Layout = tlCenter
        ExplicitLeft = 0
        ExplicitTop = 9
        ExplicitWidth = 121
        ExplicitHeight = 31
      end
    end
    object MMIPanelE17: TMMIPanel
      Left = 120
      Top = 50
      Width = 93
      Height = 50
      Caption = 'MMIPanelE17'
      Color = 36712496
      TabOrder = 1
      PaletteToUse = 0
      object TimeField: TLabel
        Left = 0
        Top = 9
        Width = 89
        Height = 31
        Alignment = taCenter
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clOlive
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        Transparent = True
        OnClick = TimeFieldClick
      end
    end
    object MMIPanelE18: TMMIPanel
      Left = 213
      Top = 50
      Width = 164
      Height = 50
      Color = 36712496
      TabOrder = 2
      PaletteToUse = 0
      object LabelATPMode: TLabel
        Left = 0
        Top = 9
        Width = 161
        Height = 31
        Alignment = taCenter
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clGray
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        Transparent = True
      end
    end
    object MMIPanelE19: TMMIPanel
      Left = 320
      Top = 0
      Width = 53
      Height = 50
      Color = 36712496
      TabOrder = 3
      PaletteToUse = 0
      object RadioImage: TImage
        Left = 0
        Top = 5
        Width = 50
        Height = 40
        Align = alCustom
        AutoSize = True
        Transparent = True
      end
    end
    object MMIPanelE16: TMMIPanel
      Left = 0
      Top = 50
      Width = 120
      Height = 50
      Caption = 'MMIPanelE10'
      Color = 36712496
      TabOrder = 4
      PaletteToUse = 0
      object LabelETA: TLabel
        Left = 0
        Top = 9
        Width = 121
        Height = 31
        Alignment = taCenter
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clOlive
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        Transparent = True
        OnClick = LabelETAClick
      end
    end
    object MMIPanelE15: TMMIPanel
      Left = 160
      Top = 0
      Width = 160
      Height = 50
      Color = 36712496
      TabOrder = 5
      PaletteToUse = 0
      object LabelLocationName: TLabel
        Left = 1
        Top = 1
        Width = 158
        Height = 48
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clOlive
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = True
        Layout = tlCenter
        ExplicitLeft = 0
        ExplicitTop = 13
        ExplicitWidth = 112
        ExplicitHeight = 31
      end
    end
  end
  object TextListArea: TMMIPanel
    Left = 75
    Top = 425
    Width = 350
    Height = 125
    Color = 36712496
    TabOrder = 1
    PaletteToUse = 0
    object UpArrow: TColorButton
      Left = 305
      Top = 6
      Width = 40
      Height = 40
      BevelSize = 5
      Highlight = clBlack
      shadow = clBlack
      Picture.Data = {
        07544269746D617056020000424D560200000000000076000000280000001E00
        00001E0000000100040000000000E00100000000000000000000100000001000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888800000000000000000000088
        8800888888000000000000000000088888008888888000000000000000008888
        8800888888880000000000000008888888008888888880000000000000888888
        8800888888888800000000000888888888008888888888800000000088888888
        8800888888888888000000088888888888008888888888888000008888888888
        8800888888888888880008888888888888008888888888888880888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800}
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 0
      TabStop = True
      OnClick = UpArrowClick
    end
    object DownArrow: TColorButton
      Left = 304
      Top = 81
      Width = 40
      Height = 40
      BevelSize = 5
      Highlight = clBlack
      shadow = clBlack
      Picture.Data = {
        07544269746D617056020000424D560200000000000076000000280000001E00
        00001E0000000100040000000000E00100000000000000000000100000001000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888088888888888888008888888888888800088888888888
        8800888888888888800000888888888888008888888888880000000888888888
        8800888888888880000000008888888888008888888888000000000008888888
        8800888888888000000000000088888888008888888800000000000000088888
        8800888888800000000000000000888888008888880000000000000000000888
        8800888880000000000000000000008888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800888888888888888888888888888888008888888888888888888888888888
        8800}
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 1
      TabStop = True
      OnClick = DownArrowClick
    end
    object DBGridEvents: TDBGrid
      Left = 1
      Top = 2
      Width = 298
      Height = 122
      BorderStyle = bsNone
      Color = 34607120
      DataSource = DataSourceEvents
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Options = [dgRowSelect]
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'Time'
          Width = 46
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Text'
          Width = 250
          Visible = True
        end>
    end
  end
  object BitBtnMenu: TBitBtn
    Left = 0
    Top = 0
    Width = 75
    Height = 41
    Caption = 'Menu'
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Glyph.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888800008888888888888888888800008888888888888888888800008888
      8880888888888888000088888880088888888888000088888880008888888888
      0000888888800008888888880000888888800000888888880000888888800000
      0888888800008888888000000088888800008888888000000088888800008888
      8880000008888888000088888880000088888888000088888880000888888888
      0000888888800088888888880000888888800888888888880000888888808888
      8888888800008888888888888888888800008888888888888888888800008888
      88888888888888880000}
    ModalResult = 3
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 2
    OnClick = BitBtnMenuClick
  end
  object AreaF: TMMIPanel
    Left = 745
    Top = 0
    Width = 53
    Height = 425
    Caption = 'AreaF'
    Color = 36712496
    TabOrder = 3
    PaletteToUse = 0
    object ColorButtonConfiguration: TColorButton
      Left = 3
      Top = 3
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'C'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 0
      TabStop = True
      OnClick = ColorButtonConfigurationClick
    end
    object ColorButtonYard: TColorButton
      Left = 3
      Top = 211
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'Y'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 1
      TabStop = True
      OnClick = ColorButtonYardClick
    end
    object ColorButtonHandlingDone: TColorButton
      Left = 3
      Top = 107
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'R'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 2
      TabStop = True
      OnClick = ColorButtonHandlingDoneClick
    end
    object ColorButtonLogOut: TColorButton
      Left = 3
      Top = 370
      Width = 50
      Height = 50
      BevelSize = 3
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Picture.Data = {
        07544269746D617016030000424D160300000000000076000000280000002000
        00002A0000000100040000000000A00200000000000000000000100000001000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00CCCCCCCCCCCCBBBBBBBBBBBBBBBCCCCCCCCCCCCCCBBB77BBBBBBBBBBBBCC
        CCCCCCCCCCCBB77787BBBBBBBBBBBBCCCCCCCCCCBBB7788887BBBBBBBBBBBCCC
        CCCCC444777888888744444444444CCCCCCCC4778888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4788888888887BBBBBBBBBB4CCCCCCCC4788888888887BBBBBB0BBB4CCC
        CCCCC4788888888887BBBBB00BBB4CCCCCCCC4788888888887BBBB000BBB4CCC
        CCCCC4788888808807BBB00000000000000CC4788888880087BB000000000000
        000CC4788888888887BBB00000000000000CC4788888888887BBBB000BBB4CCC
        CCCCC4788888888887BBBBB00BBB4CCCCCCCC4788888888887BBBBBB0BBB4CCC
        CCCCC4788888777787BBBBBBBBBB4CCCCCCCC47877777BB787BBBBBBBBBB4CCC
        CCCCC4787BBBBBB787BBBBBBBBBB4CCCCCCCC4787BBBBBB787BBBBBBBBBB4CCC
        CCCCC4787BBBBBB787BBBBBBBBBB4CCCCCCCC4787BBBBBB787BBBBBBBBBB4CCC
        CCCCC4787BBBBBB787BBBBBBBBBB4CCCCCCCC47877777BB787BBBBBBBBBB4CCC
        CCCCC4788888777787BBBBBBBBBB4CCCCCCCC4788888888887BBBBBBBBBB4CCC
        CCCCC4778888888887BBBBBBBBBB4CCCCCCCC444777888888744444444444CCC
        CCCCCCCCCCC7788887CCCCCCCCCCCCCCCCCCCCCCCCCCC77787CCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCC77CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCC}
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 4
      TabStop = True
      OnClick = ColorButtonLogOutClick
    end
    object ColorButtonPossession: TColorButton
      Left = 3
      Top = 55
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'P'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 5
      TabStop = True
      OnClick = ColorButtonPossessionClick
    end
    object ColorButtonShunting: TColorButton
      Left = 3
      Top = 159
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'S'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 6
      TabStop = True
      OnClick = ColorButtonShuntingClick
    end
    object ColorButtonAbortSetup: TColorButton
      Left = 3
      Top = 263
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'X'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 3
      TabStop = True
      OnClick = ColorButtonAbortSetupClick
    end
    object ColorButtonFreeRolling: TColorButton
      Left = 3
      Top = 313
      Width = 50
      Height = 50
      BevelSize = 3
      Caption = 'F'
      Color = clBlack
      Highlight = clSilver
      shadow = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -37
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Style = [bsCenter, bsShowFocus, bsShowKey]
      TabOrder = 7
      TabStop = True
      OnClick = ColorButtonFreeRollingClick
    end
  end
  object TimerClock: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerClockTimer
    Left = 616
    Top = 8
  end
  object DataSourceEvents: TDataSource
    DataSet = DataModuleDMI.ClientDataSetDMIEvents
    Left = 584
    Top = 8
  end
  object PopupMenu: TPopupMenu
    Images = ImageListPopup
    Left = 520
    Top = 8
    object N4: TMenuItem
      Caption = '-'
    end
    object MenuItemShowStartuphistory: TMenuItem
      Caption = 'Show Startup history'
      OnClick = MenuItemShowStartuphistoryClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItemClearDMIEventlog: TMenuItem
      Caption = 'Clear DMI Eventlog'
      OnClick = MenuItemClearDMIEventlogClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItemChangeTrainId: TMenuItem
      Caption = 'Change Train id'
      OnClick = MenuItemChangeTrainIdClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object MenuItemShowVehicleData: TMenuItem
      Caption = 'Show Vehicle data'
      OnClick = MenuItemShowVehicleDataClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object MenuItemShowRadioArea: TMenuItem
      Caption = 'Show Radio channel'
      OnClick = MenuItemShowRadioAreaClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object MenuItemCancelRegArea: TMenuItem
      Caption = 'Cancel Reg Area'
      OnClick = MenuItemCancelRegAreaClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
  end
  object ImageListPopup: TImageList
    Height = 30
    Left = 552
    Top = 8
  end
  object GestureManager: TGestureManager
    Left = 648
    Top = 8
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
