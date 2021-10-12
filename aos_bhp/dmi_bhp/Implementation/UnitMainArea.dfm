inherited FormMainArea: TFormMainArea
  Left = 226
  Top = 114
  Caption = 'DMI'
  Font.Name = 'MS Sans Serif'
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
  Scaled = False
  OnGesture = FormGesture
  ExplicitWidth = 800
  ExplicitHeight = 600
  PixelsPerInch = 96
  TextHeight = 13
  object MMIareaA: TMMIareaA [0]
    Left = 0
    Top = 40
    Width = 75
    Height = 310
    PaletteToUse = 0
    BackColor = 34607120
    WarningColor = 33575167
    OrangeColor = 33575136
    TimeToIntervention = 0
    DistanceToTarget = 0
    PredictedStopPosition = 0
    PredictedSpeedAtTarget = 0
    PredSpeedTolerance = 3
  end
  object MMIareaD: TMMIareaD [1]
    Left = 425
    Top = 0
    Width = 320
    Height = 350
    CurrentOdometerPosition = 1
    BackColor = 34607120
    GradPlusColor = clGreen
    GradMinusColor = clOlive
    ShowBCA = True
    TrainLength = 0
    PaletteToUse = 0
    TrainLengthSecondLayerText = 'Driver id: xxxxxxxxx  Length: xx '
  end
  inherited MMIareaB: TMMIareaB
    WarningColor = 33575136
  end
  object LabeledEditTrailing: TLabeledEdit [4]
    Left = 95
    Top = 396
    Width = 70
    Height = 16
    BorderStyle = bsNone
    Color = 34607120
    EditLabel.Width = 34
    EditLabel.Height = 14
    EditLabel.BiDiMode = bdLeftToRight
    EditLabel.Caption = 'Trailing'
    EditLabel.Color = 34607120
    EditLabel.Font.Charset = ANSI_CHARSET
    EditLabel.Font.Color = clGray
    EditLabel.Font.Height = -11
    EditLabel.Font.Name = 'Arial'
    EditLabel.Font.Style = []
    EditLabel.ParentBiDiMode = False
    EditLabel.ParentColor = False
    EditLabel.ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 10
    Text = '12345,6'
    Visible = False
    OnClick = LabeledEditTrackPosClick
  end
  object LabeledEditLeading: TLabeledEdit [5]
    Left = 349
    Top = 396
    Width = 70
    Height = 16
    BorderStyle = bsNone
    Color = 34607120
    EditLabel.Width = 38
    EditLabel.Height = 14
    EditLabel.Caption = 'Leading'
    EditLabel.Color = 34607120
    EditLabel.Font.Charset = ANSI_CHARSET
    EditLabel.Font.Color = clGray
    EditLabel.Font.Height = -11
    EditLabel.Font.Name = 'Arial'
    EditLabel.Font.Style = []
    EditLabel.ParentColor = False
    EditLabel.ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 9
    Text = '12345,6'
    Visible = False
    OnClick = LabeledEditTrackPosClick
  end
  inherited TextListArea: TMMIPanel
    Color = 34607120
    ParentBackground = False
    inherited DBGridEvents: TDBGrid
      Font.Name = 'MS Sans Serif'
      Options = []
      ParentColor = True
      TitleFont.Name = 'MS Sans Serif'
    end
  end
  inherited MMIPanelE12: TMMIPanel
    inherited LabeledEditBrakeTestNotification: TLabeledEdit
      EditLabel.ExplicitWidth = 158
    end
  end
  inherited MMIPanelE13: TMMIPanel
    inherited LabeledEditBTMTestNotification: TLabeledEdit
      EditLabel.ExplicitWidth = 153
    end
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
