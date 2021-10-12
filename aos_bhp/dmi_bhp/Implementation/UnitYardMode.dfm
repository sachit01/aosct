object FormYardMode: TFormYardMode
  Left = 226
  Top = 114
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'MMI'
  ClientHeight = 479
  ClientWidth = 640
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000130000000000000000000
    0000001313131313130000000000000000000031313131313100000000000000
    0000000000031000000000000000000000000000000130000000000000000000
    0000000313131313100000000000000000001310013131300313000000000000
    0013000013131313000013000000000003000001313131313000001000000000
    1000001313131313130000030000000300000131313131313100000010000001
    0000031313131313131000003000003000000131313131313130000001000010
    0000031313131313131000000300000300000131313131313130000010000001
    0000031313131313130000003000000030000031313131313100000100000000
    0100000313131313100000300000000000310000313131310000310000000000
    0000313003131310013100000000000000000001313131313000000000000000
    000000000000000000000000000000000000000000000000000000000000000B
    00B00B00B00B00B00B0000000000000B00B00B00B00B00B00B0000000000000B
    0B0B0B00B0B0B0B00B0000000000000B0B0B0B00B0B0B0B00B000AAA0000000B
    0B0B0B00B0B0B0B00B00AAAAA000000B0B0B0B00B0B0B0B00B00AAAAA000000B
    B000BB00BB000BB00B00AAAAA000000BB000BB00BB000BB00B000AAA00000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MMIareaB: TMMIareaB
    Left = 64
    Top = 8
    Width = 160
    Height = 170
    PaletteToUse = 0
    WarningColor = 33575167
    BackColor = 34607120
    ScaleTicmark = 1
    TargetSpeed = 0
    IndicatePermittedSpeed = False
    IndicateTargetSpeed = False
    PermittedDrivingDirection = MMIUndefined
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -27
    Font.Name = 'Helvetica'
    Font.Style = [fsBold]
  end
  object TimeField: TLabel
    Left = 525
    Top = 11
    Width = 65
    Height = 25
    Alignment = taRightJustify
    AutoSize = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clOlive
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    Transparent = True
    OnClick = TimeFieldClick
  end
  object Label1: TLabel
    Left = 624
    Top = 264
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object LabelSpeed: TLabel
    Left = 40
    Top = 145
    Width = 18
    Height = 36
    Alignment = taRightJustify
    Caption = '0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object EButton: TMMIPanel
    Left = 2
    Top = 0
    Width = 63
    Height = 147
    Caption = 'EButton'
    Color = 36712496
    TabOrder = 0
    PaletteToUse = 0
    object Brake: TSpeedButton
      Left = 0
      Top = 3
      Width = 59
      Height = 34
      Enabled = False
      Flat = True
      NumGlyphs = 4
      Visible = False
      OnClick = BrakeClick
    end
    object BrakeIntervention: TSpeedButton
      Left = 0
      Top = 37
      Width = 59
      Height = 34
      AllowAllUp = True
      Flat = True
      Margin = 4
      NumGlyphs = 4
      Visible = False
      OnClick = BrakeInterventionClick
    end
    object IntegrityAlert: TSpeedButton
      Left = 0
      Top = 72
      Width = 59
      Height = 34
      Flat = True
      Visible = False
    end
    object EAlert: TSpeedButton
      Left = 0
      Top = 107
      Width = 59
      Height = 34
      AllowAllUp = True
      Flat = True
      Margin = 4
      NumGlyphs = 2
      Visible = False
    end
  end
  object AreaF: TMMIPanel
    Left = 591
    Top = 0
    Width = 49
    Height = 178
    Caption = 'AreaF'
    Color = 36712496
    TabOrder = 1
    PaletteToUse = 0
    object ConfigurationReq: TColorButton
      Left = 5
      Top = 2
      Width = 40
      Height = 40
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
      OnClick = ConfigurationReqClick
    end
    object MMIPanelE12: TMMIPanel
      Left = 0
      Top = 110
      Width = 46
      Height = 48
      Color = 36712496
      TabOrder = 1
      PaletteToUse = 0
      object RadioImage: TImage
        Left = 9
        Top = 3
        Width = 36
        Height = 34
        Transparent = True
      end
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 184
    Width = 640
    Height = 295
    ActivePage = TabSheetMsg
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    MultiLine = True
    ParentFont = False
    TabOrder = 2
    TabPosition = tpRight
    OnChange = PageControlChange
    object TabSheetMsg: TTabSheet
      Caption = 'Msg'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ImageIndex = 3
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoMsg: TMemo
        Left = 0
        Top = 0
        Width = 558
        Height = 287
        Align = alClient
        Color = clSilver
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheetATP1: TTabSheet
      Caption = 'ATP1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelLastATP1Command: TLabel
        Left = 8
        Top = 16
        Width = 8
        Height = 32
        Visible = False
      end
      object MemoATP1Response: TMemo
        Left = 0
        Top = 0
        Width = 558
        Height = 287
        Align = alClient
        Color = clSilver
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheetATP2: TTabSheet
      Caption = 'ATP2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelLastATP2Command: TLabel
        Left = 8
        Top = 16
        Width = 8
        Height = 32
        Visible = False
      end
      object MemoATP2Response: TMemo
        Left = 0
        Top = 0
        Width = 558
        Height = 287
        Align = alClient
        Color = clSilver
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheetATO: TTabSheet
      Caption = 'ATO'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelLastATOCommand: TLabel
        Left = 8
        Top = 16
        Width = 8
        Height = 32
        Visible = False
      end
      object MemoATOResponse: TMemo
        Left = 0
        Top = 0
        Width = 558
        Height = 287
        Align = alClient
        Color = clSilver
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object PanelCommand: TPanel
    Left = 227
    Top = 72
    Width = 370
    Height = 110
    Color = clSilver
    ParentBackground = False
    TabOrder = 3
    Visible = False
    object LabelCommand: TLabel
      Left = 8
      Top = 5
      Width = 205
      Height = 19
      Caption = 'ATP1 Yard mode command'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ComboBoxCommand: TComboBox
      Left = 8
      Top = 31
      Width = 350
      Height = 28
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object BitBtnGo: TBitBtn
      Left = 8
      Top = 69
      Width = 75
      Height = 33
      Caption = '&Go'
      DoubleBuffered = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Kind = bkRetry
      NumGlyphs = 2
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 1
      OnClick = BitBtnGoClick
    end
    object BitBtnClear: TBitBtn
      Left = 89
      Top = 69
      Width = 75
      Height = 33
      Caption = '&Clear'
      DoubleBuffered = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333000000000
        3333333777777777F3333330F777777033333337F3F3F3F7F3333330F0808070
        33333337F7F7F7F7F3333330F080707033333337F7F7F7F7F3333330F0808070
        33333337F7F7F7F7F3333330F080707033333337F7F7F7F7F3333330F0808070
        333333F7F7F7F7F7F3F33030F080707030333737F7F7F7F7F7333300F0808070
        03333377F7F7F7F773333330F080707033333337F7F7F7F7F333333070707070
        33333337F7F7F7F7FF3333000000000003333377777777777F33330F88877777
        0333337FFFFFFFFF7F3333000000000003333377777777777333333330777033
        3333333337FFF7F3333333333000003333333333377777333333}
      ModalResult = 4
      NumGlyphs = 2
      ParentDoubleBuffered = False
      ParentFont = False
      TabOrder = 2
      OnClick = BitBtnClearClick
    end
  end
  object MMIPanelE21: TMMIPanel
    Left = 230
    Top = 0
    Width = 48
    Height = 70
    ParentColor = True
    TabOrder = 4
    PaletteToUse = 0
    object SystemIconChangeMode: TImage
      Left = 2
      Top = 3
      Width = 45
      Height = 64
      AutoSize = True
      Enabled = False
      Transparent = True
      Visible = False
    end
  end
  object MMIPanelE22: TMMIPanel
    Left = 278
    Top = 0
    Width = 48
    Height = 70
    ParentColor = True
    TabOrder = 5
    PaletteToUse = 0
    object SystemIconATOMode: TImage
      Left = 5
      Top = 6
      Width = 44
      Height = 65
      Stretch = True
      Transparent = True
    end
  end
  object TimerDisplay: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerDisplayTimer
    Left = 479
    Top = 8
  end
  object TcpClientATP1: TTcpClient
    BlockMode = bmNonBlocking
    RemoteHost = 'localhost'
    RemotePort = '55150'
    Left = 344
    Top = 8
  end
  object TcpClientATP2: TTcpClient
    BlockMode = bmNonBlocking
    RemoteHost = 'localhost'
    Left = 376
    Top = 8
  end
  object TcpClientATO: TTcpClient
    BlockMode = bmNonBlocking
    RemoteHost = 'localhost'
    Left = 408
    Top = 8
  end
  object TimerResponse: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerResponseTimer
    Left = 448
    Top = 8
  end
end
