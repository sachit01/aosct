object FormMMIFrame: TFormMMIFrame
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DMI'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMMIFrame: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    Color = 34607120
    ParentBackground = False
    TabOrder = 0
  end
  object TcpClientNJRU: TTcpClient
    BlockMode = bmNonBlocking
    Left = 728
    Top = 24
  end
  object TimerPending: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = TimerPendingTimer
    Left = 728
    Top = 80
  end
  object ExchangeTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ExchangeTimerTimer
    Left = 427
    Top = 19
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 504
    Top = 16
  end
  object IdTCPServerRemoteDMIInterface: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    MaxConnections = 1
    OnConnect = IdTCPServerRemoteDMIInterfaceConnect
    OnDisconnect = IdTCPServerRemoteDMIInterfaceDisconnect
    OnExecute = IdTCPServerRemoteDMIInterfaceExecute
    Left = 688
    Top = 168
  end
  object IdTCPServerRemoteDMIInterfaceInternal: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    MaxConnections = 1
    OnConnect = IdTCPServerRemoteDMIInterfaceInternalConnect
    OnDisconnect = IdTCPServerRemoteDMIInterfaceInternalDisconnect
    OnExecute = IdTCPServerRemoteDMIInterfaceInternalExecute
    Left = 688
    Top = 240
  end
end
