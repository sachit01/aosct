object FormTrainComposition: TFormTrainComposition
  Left = 0
  Top = 0
  Anchors = []
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Train Composition'
  ClientHeight = 465
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PrintScale = poNone
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BevelCarList: TBevel
    Left = 0
    Top = 0
    Width = 521
    Height = 337
  end
  object BevelTrainInfo: TBevel
    Left = 0
    Top = 343
    Width = 521
    Height = 75
  end
  object LabelTrainLen: TLabel
    Left = 8
    Top = 352
    Width = 60
    Height = 13
    Alignment = taCenter
    Caption = 'Train Length'
    Transparent = False
    Layout = tlCenter
  end
  object LabelTrainWeight: TLabel
    Left = 8
    Top = 392
    Width = 61
    Height = 13
    Caption = 'Train Weight'
  end
  object LabelTrainName: TLabel
    Left = 216
    Top = 352
    Width = 54
    Height = 13
    Caption = 'Train Name'
  end
  object LabelTrainWeightUnit: TLabel
    Left = 176
    Top = 392
    Width = 3
    Height = 13
  end
  object LabelTrainLenUnit: TLabel
    Left = 176
    Top = 352
    Width = 3
    Height = 13
  end
  object DBGridTrainComp: TDBGrid
    Left = 0
    Top = 0
    Width = 473
    Height = 337
    ParentCustomHint = False
    BiDiMode = bdLeftToRight
    BorderStyle = bsNone
    Ctl3D = True
    DataSource = DataSourceTrainComp
    FixedColor = clWindow
    GradientEndColor = clGradientActiveCaption
    GradientStartColor = clGradientActiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [dgTitles, dgColLines, dgRowLines, dgRowSelect]
    ParentBiDiMode = False
    ParentCtl3D = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clYellow
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'SrNbr'
        ReadOnly = True
        Title.Alignment = taCenter
        Title.Caption = 'S. No'
        Title.Color = clWindowText
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = []
        Width = 32
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'VehicleTypeName'
        ReadOnly = True
        Title.Alignment = taCenter
        Title.Caption = 'Vehicle Type'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = []
        Width = 145
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'VehicleName'
        ReadOnly = True
        Title.Alignment = taCenter
        Title.Caption = 'Vehicle Name'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = []
        Width = 145
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'VehicleNodeId'
        ReadOnly = True
        Title.Alignment = taCenter
        Title.Caption = 'Road Number'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = []
        Width = 145
        Visible = True
      end>
  end
  object DownArrow: TColorButton
    Left = 471
    Top = 294
    Width = 42
    Height = 43
    Highlight = clBlack
    shadow = clBlack
    Picture.Data = {
      07544269746D617056020000424D560200000000000076000000280000001E00
      00001E0000000100040000000000E00100000000000000000000100000000000
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
  object UpArrow: TColorButton
    Left = 471
    Top = 0
    Width = 42
    Height = 43
    Highlight = clBlack
    shadow = clBlack
    Picture.Data = {
      07544269746D617056020000424D560200000000000076000000280000001E00
      00001E0000000100040000000000E00100000000000000000000100000000000
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
    TabOrder = 2
    TabStop = True
    OnClick = UpArrowClick
  end
  object BitBtnClose: TBitBtn
    Left = 428
    Top = 424
    Width = 92
    Height = 41
    DoubleBuffered = True
    Kind = bkClose
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object EditTrainLen: TEdit
    Left = 94
    Top = 349
    Width = 78
    Height = 21
    BevelOuter = bvNone
    Color = clBtnFace
    NumbersOnly = True
    ReadOnly = True
    TabOrder = 4
  end
  object EditTrainWeight: TEdit
    Left = 94
    Top = 389
    Width = 78
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
  object EditTrainName: TEdit
    Left = 284
    Top = 349
    Width = 102
    Height = 21
    BevelOuter = bvNone
    Color = clBtnFace
    NumbersOnly = True
    ReadOnly = True
    TabOrder = 6
  end
  object DataSourceTrainComp: TDataSource
    DataSet = DataModuleDMI.ClientDataSetDMITrainComp
    Top = 432
  end
end
