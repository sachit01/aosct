object FormStartUpHistory: TFormStartUpHistory
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Startup history'
  ClientHeight = 304
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GridPanel: TGridPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 304
    Align = alClient
    Caption = 'GridPanel'
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = ListView
        Row = 0
      end
      item
        Column = 0
        Control = BitBtnClose
        Row = 1
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 24.000000000000000000
      end>
    TabOrder = 0
    object ListView: TListView
      Left = 1
      Top = 1
      Width = 615
      Height = 278
      Align = alClient
      Columns = <
        item
          Caption = 'Time'
          Width = 140
        end
        item
          Caption = 'Text'
          Width = 420
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
    end
    object BitBtnClose: TBitBtn
      Left = 541
      Top = 279
      Width = 75
      Height = 24
      Align = alRight
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
end
