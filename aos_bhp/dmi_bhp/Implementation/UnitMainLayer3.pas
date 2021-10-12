unit UnitMainLayer3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitMainLayer2, GestureMgr, ImgList, Menus, DB, ExtCtrls, StdCtrls,
  Buttons, Grids, DBGrids, ColorButton2, MMIPanel, MMIareaB, MMIareaC;

type
  TFormMainLayer3 = class(TFormMainLayer2)
    MMIareaB: TMMIareaB;
    MMIareaC: TMMIareaC;
    MMIPanelSystemArea1: TMMIPanel;
    MMIPanelE24: TMMIPanel;
    SystemIconIdle: TImage;
    MMIPanelE23: TMMIPanel;
    SystemIconStopTrain: TImage;
    MMIPanelE22: TMMIPanel;
    SystemIconTimeout: TImage;
    MMIPanelE21: TMMIPanel;
    SystemIconOdoInvalid: TImage;
    MMIPanelE20: TMMIPanel;
    SystemIconFreeRolling: TImage;
    MMIPanelE25: TMMIPanel;
    SystemIconLevelCrossing: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabeledEditTrackPosClick(Sender: TObject);
    procedure TimerFlashTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { Private declarations }
    BitmapIdle: TBitmap;

  protected
    odoMeterInvalid : Boolean;

  public
    { Public declarations }
    procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormMainLayer3: TFormMainLayer3;

implementation

uses UnitMMIFrame, UnitDMIDataModule, MMIStd, MMITypes, UnitViewLog;

{$R *.dfm}

procedure TFormMainLayer3.FormCreate(Sender: TObject);
var
  ImagePanelWidth: Integer;
  ImagePanelHeight: Integer;

begin

  MMIareaB.ActualSpeed := 0;
  MMIareaB.InBCA := False;
  MMIareaB.IndicatePermittedSpeed := False;
  MMIareaB.IndicateTargetSpeed := False;
  MMIareaB.Intervention := False;
  MMIareaB.PermittedDrivingDirection := MMINone;
  MMIareaB.PermittedSpeed := 0;
  MMIareaB.ShowPermittedDrivingDirection := False;
  MMIareaB.TargetSpeed := 0;
  MMIareaB.Warning := False;

  // Init area B, speedometer area.
  MMIareaB.MaxScale := 100;
  MMIareaB.MaxValue := 1200;
  MMIareaB.ScaleTicmark := 2;
  MMIareaB.ScaleDivision := 10;
  MMIareaB.ScaleFigures := 20;
  MMIareaB.FontSize := 15;
  MMIareaB.RadiusFigures := 63;

  MMIareaB.ArrowStyle := dasStandard;

          { Now possible to override ArrowStyle project-setting }
  MMIareaB.ArrowStyle := TDirectionArrowStyle
      (FormMMIFrame.InitFile.ReadInteger('MMI', 'DirectionArrowStyle',
      Ord(MMIareaB.ArrowStyle)));

          { Now possible to override speedometer project-setting }
  MMIareaB.MaxScale := FormMMIFrame.InitFile.ReadInteger('MMI',
      'SpeedMaxScale', MMIareaB.MaxScale);
  MMIareaB.MaxValue := MMIareaB.MaxScale * 10;

  MMIareaB.Width := GetXpos2 - GetXpos1;
  MMIareaB.Height := GetYpos1;
  MMIareaB.Left := GetXpos1;
  MMIareaB.FontSize := round(20 * GetXFactor);

  MMIareaC.Width := GetXpos2 - GetXpos1;
  MMIareaC.Height := GetYpos2 - GetYpos1;
  MMIareaC.Top := GetYpos1;
  MMIareaC.Left := GetXpos1;

  MMIPanelSystemArea1.Width := GetXpos3 - GetXpos2;
  MMIPanelSystemArea1.Height := (GetYpos3 - GetYpos1) div 2;
  MMIPanelSystemArea1.Top := GetYpos1;
  MMIPanelSystemArea1.Left := GetXpos2;

  AdjustControl(MMIPanelE20);
  AdjustControl(MMIPanelE21);
  AdjustControl(MMIPanelE22);
  AdjustControl(MMIPanelE23);
  AdjustControl(MMIPanelE24);
  AdjustControl(MMIPanelE25);

  BitmapIdle := TBitmap.Create;
  LoadBitmapFromResource(BitmapIdle, FormMMIFrame.HiRes, 'IDLE',
      'HIRES_IDLE');
  SystemIconIdle.Picture.Bitmap := BitmapIdle;

        // Found it better to centre image within its panel
        // instead of stretching to the panel size
        // Bo H / 2012-02-22
        // Image may look distorted when stretched

  AdjustImage(SystemIconIdle);

  ImagePanelWidth := MMIPanelE20.Width;
  ImagePanelHeight := MMIPanelE20.Height;

  CentreImage(SystemIconFreeRolling, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconStopTrain, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconTimeout, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconOdoInvalid, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconIdle, ImagePanelWidth, ImagePanelHeight);
  CentreImage(SystemIconLevelCrossing,ImagePanelWidth,ImagePanelHeight);
  odoMeterInvalid := false;

  inherited;

end;

procedure TFormMainLayer3.FormDestroy(Sender: TObject);
begin
  inherited;
  BitmapIdle.Free;
end;

procedure TFormMainLayer3.FormShow(Sender: TObject);
begin
  inherited;

  MMIAreaB.Show;
  MMIAreaC.Show;

end;

{ *********************************************************
* LabeledEditTrackPosClick
* Description: Handle touch or click on Track/Pos
********************************************************* }
procedure TFormMainLayer3.LabeledEditTrackPosClick(Sender: TObject);
var
  ThisLabeledEdit: TLabeledEdit;
begin
  inherited;
  ThisLabeledEdit := TLabeledEdit(Sender);

  if (ThisLabeledEdit.Font.Color = ThisLabeledEdit.Color) then
  begin { Foreground color is equal to background -> Not visible but still clickable }
          { Make visible again }
    ThisLabeledEdit.Font.Color := clGray;
    ThisLabeledEdit.EditLabel.Font.Color := clGray;
  end
  else
  begin
          { Make foreground color equal to background -> Not visible but still clickable }
    ThisLabeledEdit.Font.Color := ThisLabeledEdit.Color;
    ThisLabeledEdit.EditLabel.Font.Color := ThisLabeledEdit.EditLabel.Color;
  end;

      { Disable/Enable to get rid of focus and text-cursor }
  ThisLabeledEdit.Enabled := False;
  ThisLabeledEdit.Enabled := true;
end;

{ *********************************************************
* TimerFlashTimer
* Description: Handle flashing controls
********************************************************* }
procedure TFormMainLayer3.TimerFlashTimer(Sender: TObject);
begin
  inherited;
  case FlashTick of
    1:
      begin
        if SystemIconStopTrain.Enabled then // StopTrain
          FlashControl(SystemIconStopTrain);
      end;
    3:
      begin
        if SystemIconStopTrain.Enabled then // StopTrain
          FlashControl(SystemIconStopTrain);
      end;
  else
  end;

end;

{ *********************************************************
* Function:    UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormMainLayer3.UpdateDMI(Refresh : Boolean);
var
  LocoStatus : LongWord;
  LocoStatusChanged : Boolean;

  InterfaceFlags : Byte;
  InterfaceFlagsChanged : Boolean;

  DriverInfo : TDriverInfoMT;
  DriverInfoChanged : Boolean;

  SpeedAndDistance : TSpeed_DistanceMT;
  SpeedAndDistanceChanged : Boolean;

  dmiSpeed: LongInt;

  AdditionalStatus1 : Byte;
  AdditionalStatus1Changed : Boolean;

begin
  inherited;

    {Loco Status}
  LocoStatus := DataModuleDMI.GetLocoStatus(LocoStatusChanged);
  if Refresh or LocoStatusChanged then
  begin
        // Free rolling
    if LongBool(LocoStatus and LS_FreeRolling) then
      SystemIconFreeRolling.Show
    else
      SystemIconFreeRolling.Hide;

      // MA timeout.
    if LongBool(LocoStatus and LS_MA_TimeOut) then
      SystemIconTimeout.Show
    else
      SystemIconTimeout.Hide;

        // Train idling ?
    if LongBool(LocoStatus and LS_TrainIdling) then
      SystemIconIdle.Show
    else
      SystemIconIdle.Hide;
  end;

  InterfaceFlags := DataModuleDMI.GetInterfaceFlags(InterfaceFlagsChanged);
  if Refresh or InterfaceFlagsChanged then
  begin
    if (bytebool(InterfaceFlags and ODOMETER_INVALID)) then
          // Odometer invalid
    begin
      SystemIconOdoInvalid.Visible := true;
      OdometerInvalid := true;
    end
    else
    begin
      SystemIconOdoInvalid.Visible := false;
      OdometerInvalid := false;
    end;


    if (bytebool(InterfaceFlags and STOP_TRAIN_REQUEST)) then
    begin // Stop Train icon
      SystemIconStopTrain.Enabled := true;
    end
    else
    begin
      SystemIconStopTrain.Enabled := false;
      SystemIconStopTrain.Hide;
    end;
  end;

  // Additional status 1
  AdditionalStatus1 := DataModuleDMI.GetAdditionalStatus1(AdditionalStatus1Changed);
  if Refresh or AdditionalStatus1Changed then
  begin
   {Approach Area for Level Crossing}
    if (ByteBool(AdditionalStatus1 AND IN_APPROACH_AREA_FOR_LEVEL_CROSSING)) then
    begin
       SystemIconLevelCrossing.Enabled := true;
       SystemIconLevelCrossing.Show;
    end
    else
       SystemIconLevelCrossing.Visible := false;
  end;

  DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);
  if Refresh or DriverInfoChanged then
  begin
    MMIAreaB.Dimmed:= odoMeterInvalid;
    MMIAreaC.Dimmed:= odoMeterInvalid;

    MMIAreaB.PermittedDrivingDirection:=TPermittedDirectionEnum(DriverInfo.Permitteddrivingdirection);
    MMIAreaB.PermittedSpeed:=10*DriverInfo.Pspeed ;
    MMIAreaB.TargetSpeed:= 10*DriverInfo.Tspeed ;
    MMIareaB.InBCA:= ByteBool(DriverInfo.StatusD11 and INBCA) ;
    MMIareaB.Warning:=ByteBool(DriverInfo.StatusD11 and ATPWARNING) ;

       // Intervention indication
    if ByteBool(DriverInfo.StatusD11 and ATPINTERVENTION) then
    begin
      MMIAreaB.Intervention:= True;
    end
    else
    begin
      MMIAreaB.Intervention:= False;
    end;

    MMIareaB.ShowPermittedDrivingDirection:=ByteBool(DriverInfo.StatusD12 and INDPERMDIR) ;
    MMIareaB.IndicatePermittedSpeed:=ByteBool(DriverInfo.StatusD12 and INDPERMSPEED) ;
    MMIareaB.IndicateTargetSpeed:=ByteBool(DriverInfo.StatusD12 and INDTARGETSPEED) ;
    MMIAreaB.RefreshArea ;
  end;

  SpeedAndDistance := DataModuleDMI.GetSpeedAndDistance(SpeedAndDistanceChanged);
  if Refresh or SpeedAndDistanceChanged then
  begin

    if SpeedAndDistance.CurrentSpeed > 0 then
    begin
      FormViewLog.Hide;
    end;

    { Always show 1km/h between 0 and 1km/h! (konedlu 2003-04-28) }
    if (SpeedAndDistance.CurrentSpeed > 0) and (SpeedAndDistance.CurrentSpeed < 27) then   // 27cm/s is ~1km/h.
      dmiSpeed := 10 // 10 * 0.1 km/h.
    else
      dmiSpeed := CmPerSec_ToKmPerHour(SpeedAndDistance.CurrentSpeed); //Gives 0.1 km/h.

    MMIareaB.ActualSpeed := dmiSpeed;
    MMIAreaC.VActual := Round(dmiSpeed / 10);
    if (SpeedAndDistance.CurrentOdometer > 0) then
      MMIareaB.CurrentOdometerPosition:=SpeedAndDistance.CurrentOdometer;
    MMIareaB.RefreshArea;
  end;

end;

end.
