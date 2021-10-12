(*****************************************************************************
*           © COPYRIGHT Bombardier Transportation AB, SWEDEN 2011.           *
*           ======================================================           *
*                                                                            *
*    The copyright to the computer program herein is the                     *
*    property of Bombardier Transportation AB, Sweden. All rights reserved.  *
*    The program may be used and/or copied only with the                     *
*    written permission from Bombardier Transportation AB,                   *
*    or in accordance                                                        *
*    with the terms and conditions stipulated in the                         *
*    agreement/contract under which the program has been                     *
*    supplied.                                                               *
*                                                                            *
*                                                                            *
*    NAME:  UnitMainArea.pas                                                 *
*                                                                            *
*    PROJECT:  LKAB, InterFlow TrainBorne                                    *
*                                                                            *
*    Ver    Author           Date    Reason                                  *
*    ---    ------           ------  ------                                  *
*           Bo H              140507  Show LogOut-button also when in undefined ATO mode (ATOEnable = 0)
*           Bo H              150126  Init_Demo and separate MaxSpeedScale override.
*
*    DESCRIPTION:  Mainarea that incorporates area a,b,c,d,e,f and service   *
*                                                                            *
*    INTERFACE:                                                              *
******************************************************************************)
unit UnitMainArea;

interface

{ ****************************************************************************
* UNIT USES                                                                 *
**************************************************************************** }
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, MMIareaD, MMIareaB, MMIareaA, ColorButton2,
  UnitFlash, MMITypes, IniFiles, MMIareaC, MMIPanel, AnsiStrings, Gestures,
  UnitMainLayer3, DB, Grids, DBGrids, Menus, ImgList, GestureMgr;

{ ****************************************************************************
* UNIT CONST DECLARATIONS                                                   *
**************************************************************************** }
const


  StartLocoTextDelay = 7000; { ms delay before StartLoco - message displayed }

{ ****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
**************************************************************************** }
type

  TSpeedBitmapArray = array [TCeilingSpeedReason] of TBitmap;
  TAdapSpeedBitmapArray = array [TAdapCeilingSpeedReason] of TBitmap;

  TFormMainArea = class(TFormMainLayer3)
    MMIareaA: TMMIareaA;
    MMIareaD: TMMIareaD;
    LabeledEditLeading: TLabeledEdit;
    LabeledEditTrailing: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
    procedure TimerFlashTimer(Sender: TObject);

  private
    { Private declarations }

    FSpeedReason: TSpeedBitmapArray;
    FAdapSpeedReason: TAdapSpeedBitmapArray;

  protected
    { Protected declarations }
    procedure MMIInit;
    procedure Init_BHPB();
    function getThePalette: hPalette;

  public
    { Public declarations }

    // Palette256: hPalette;

    FID: string;
    FPassword: string;

    property SpeedReason: TSpeedBitmapArray read FSpeedReason;
    property AdapSpeedReason: TAdapSpeedBitmapArray read FAdapSpeedReason;

    procedure updateCarAndTrainNames();
    procedure UpdateDMI(Refresh : Boolean);

  published
    { Published declarations }

  end;

{ ****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
**************************************************************************** }
var
  FormMainArea: TFormMainArea;


implementation

{$R *.DFM}

{ ****************************************************************************
* USES                                                                      *
**************************************************************************** }
uses
  UnitExchange, MMIStd, UnitCommLost, UnitUnregForm, UnitAtpMessages, UnitTrainVsTrack,
  UnitLogin, UnitMMIFrame, UnitPowerDown,
  UnitViewLog, UnitNewTrainName, UnitReReg, UnitDMIDataModule,
  UnitManualConfiguration, UnitConfirmBrakeTest;

{ ****************************************************************************
* CONST DECLARATIONS                                                        *
**************************************************************************** }
{ ****************************************************************************
* TYPE DECLARATIONS                                                         *
**************************************************************************** }

{ ****************************************************************************
* VAR DECLARATIONS                                                          *
**************************************************************************** }

{ ****************************************************************************
* FORWARD DECLARATIONS                                                      *
**************************************************************************** }

{ ****************************************************************************
* FUNCTIONS AND PROCEDURES                                                  *
**************************************************************************** }

{ *********************************************************
* Function:    FormMainArea.FormCreate
* Description: Create and init objects for the main form
********************************************************* }
procedure TFormMainArea.FormCreate(Sender: TObject);
begin

{  Palette256 := getThePalette;
  MMIareaA.PaletteToUse := Palette256;
  MMIareaB.PaletteToUse := Palette256;
  MMIareaC.PaletteToUse := Palette256;
  MMIareaD.PaletteToUse := Palette256;
}
  MMIareaA.InBCA := False;

  try
    MMIInit;
          { Initialize the mmi areas. }
    Init_BHPB();

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    FSpeedReason[ssrUndefined] := TBitmap.Create;
    FSpeedReason[ssrPointStraight] := TBitmap.Create;
    FSpeedReason[ssrPointCurve] := TBitmap.Create;
    FSpeedReason[ssrPointPassed] := TBitmap.Create;
    FSpeedReason[ssrLocation] := TBitmap.Create;
    FSpeedReason[ssrTSR] := TBitmap.Create;
    FSpeedReason[ssrRestrictiveSection] := TBitmap.Create;
    FSpeedReason[ssrOther] := TBitmap.Create;
    FSpeedReason[ssrEndOfMa] := TBitmap.Create;
    FSpeedReason[ssrCondTarget] := TBitmap.Create;
    FSpeedReason[ssrPantoShiftToNone] := TBitmap.Create;
    FSpeedReason[ssrPantoShiftToRoof] := TBitmap.Create;
    FSpeedReason[ssrPantoShiftToSide] := TBitmap.Create;

    FAdapSpeedReason[ssrLevelCrossing] := TBitmap.Create;

    FSpeedReason[ssrUndefined].LoadFromResourceName(Hinstance, 'SSRUNDEFINED');
    FSpeedReason[ssrPointStraight].LoadFromResourceName(Hinstance,
      'SSRPOINTSTRAIGHT');
    FSpeedReason[ssrPointCurve].LoadFromResourceName(Hinstance,
      'SSRPOINTCURVE');
    FSpeedReason[ssrPointPassed].LoadFromResourceName(Hinstance,
      'SSRPOINTPASSED');
    FSpeedReason[ssrLocation].LoadFromResourceName(Hinstance, 'SSRLOCATION');
    FSpeedReason[ssrTSR].LoadFromResourceName(Hinstance, 'SSRMENATWORK');
    FSpeedReason[ssrRestrictiveSection].LoadFromResourceName(Hinstance, 'SSRMENATWORK');
    FSpeedReason[ssrOther].LoadFromResourceName(Hinstance, 'SSROTHER');
    FSpeedReason[ssrEndOfMa].LoadFromResourceName(Hinstance, 'SSRENDOFMA');
    FSpeedReason[ssrCondTarget].LoadFromResourceName(Hinstance,
      'SSRCONDTARGET');
    FSpeedReason[ssrPantoShiftToNone].LoadFromResourceName(Hinstance,
      'SSRPantoShiftToNone');
    FSpeedReason[ssrPantoShiftToRoof].LoadFromResourceName(Hinstance,
      'SSRPantoShiftToRoof');
    FSpeedReason[ssrPantoShiftToSide].LoadFromResourceName(Hinstance,
      'SSRPantoShiftToSide');
    FAdapSpeedReason[ssrLevelCrossing].LoadFromResourceName(Hinstance,
      'LEVEL_CROSSING_ANN');


        // Adjust for screen size
    MMIareaA.Width := GetXpos1;
    MMIareaA.Height := GetYpos1-BitBtnMenu.Height;
    MMIareaA.XFactor := GetXFactor;

    MMIareaD.Width := GetXpos3 - GetXpos2;
    MMIareaD.Height := GetYpos1;
    MMIareaD.Left := GetXpos2;

    AdjustLabeledEdit(LabeledEditTrailing);
    AdjustLabeledEdit(LabeledEditLeading);

    LabeledEditLeading.EditLabel.Caption := InterfaceString(ifLeading);
    LabeledEditTrailing.EditLabel.Caption := InterfaceString(ifTrailing);

    inherited; // Call the parent Create method

    FormMMIFrame.LogEventStr(LogLevelNormal, AnsiString('MAIN'),
      AnsiString('Ver:') + AnsiString(GetVersion(Application.ExeName)) +
      AnsiString(' starting up..'));

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorFormCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FormCreate:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate;
    end;
  end;
end;


{ *********************************************************
* Function:    FormMainArea.getThePalette
* Description: Get a palette to be used by components
********************************************************* }
function TFormMainArea.getThePalette: hPalette;
const
  PaletteDataSize = Sizeof(TLogPalette) + (255 * Sizeof(TPaletteEntry));
var
  Resource: hRsrc;
  Memory: hGlobal;
  InfoHeader: PBitmapInfoHeader;
  Palette: PLogPalette;
  i: Integer;
  BMI: PBitmapInfo;

begin
  Resource := FindResource(Hinstance, PChar('256BMP'), rt_Bitmap);
  Memory := LoadResource(Hinstance, Resource);
  InfoHeader := PBitmapInfoHeader(LockResource(Memory));
  GetMem(Palette, PaletteDataSize);
  BMI := PBitmapInfo(InfoHeader);
  with Palette^ do
  begin
    palVersion := $300;
    palNumEntries := 256;
    for i := 0 to 255 do
    begin
{$R-}
      palPalEntry[i].peRed := BMI^.bmiColors[i].rgbRed;
      palPalEntry[i].peGreen := BMI^.bmiColors[i].rgbGreen;
      palPalEntry[i].peBlue := BMI^.bmiColors[i].rgbBlue;
      palPalEntry[i].peFlags := 0;
{$R+}
    end;
  end;
  Result := CreatePalette(Palette^);
  FreeMem(Palette, PaletteDataSize);
  UnlockResource(Memory);
  FreeResource(Memory);
end;

{ *********************************************************
* Function:    FormMainArea.MMIInit
* Description: Initiates the MMI Applications
********************************************************* }
procedure TFormMainArea.MMIInit;
begin
  try

    Top := 0;
    Left := 0;

    MMIareaA.Hide;

    MMIareaD.CurrentOdometerPosition := 0;
    MMIareaD.Dimmed := False;
    MMIareaD.EraseAllLists;
    MMIareaD.ShowBCA := False;
    MMIareaD.TrainLength := 0;
    MMIareaD.TrainLengthSecondLayerText := '     ';

    if Exchange.Portopen then
      Show;

    if Assigned(FormAtpMessages) then
      FormAtpMessages.Hide;

  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorMMIInit);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in MMIInit:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;

procedure TFormMainArea.TimerClockTimer(Sender: TObject);
begin
  inherited;

  if ((ClockTick mod 2) = 0) then
    MMIareaD.OneSecTic;

end;

{ *********************************************************
* TimerFlashTimer
* Description: Handle flashing controls
********************************************************* }
procedure TFormMainArea.TimerFlashTimer(Sender: TObject);
begin
  inherited;
end;

{ *********************************************************
* Procedure:   TFormMainArea.Init_BHPB
* Description: Initiates the MMI Areas to BHPB
*              settings.
********************************************************* }
procedure TFormMainArea.Init_BHPB();
begin
  // Init area A, brake area.
  MMIareaA.LogScale := False;

  // Init area D, planning area.
  MMIareaD.MaxScaleDistance := 20000;
  MMIareaD.Maxdistance := 5000;
  MMIareaD.Rescaleable := true;
end;

{ *********************************************************
* Function:    FormMainArea.FormDestroy
* Description: Cleans up and exits the program
********************************************************* }
procedure TFormMainArea.FormDestroy(Sender: TObject);
begin
  try
    FSpeedReason[ssrUndefined].Free;
    FSpeedReason[ssrPointStraight].Free;
    FSpeedReason[ssrPointCurve].Free;
    FSpeedReason[ssrPointPassed].Free;
    FSpeedReason[ssrLocation].Free;
    FSpeedReason[ssrOther].Free;
    FSpeedReason[ssrEndOfMa].Free;
    FSpeedReason[ssrCondTarget].Free;
    FSpeedReason[ssrPantoShiftToNone].Free;
    FSpeedReason[ssrPantoShiftToRoof].Free;
    FSpeedReason[ssrPantoShiftToSide].Free;
    FAdapSpeedReason[ssrLevelCrossing].Free;
    // DeleteObject(Palette256);
  except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('MAIN', MS_SW_ERROR, MS_MAIN,
        MainLogErrorFormDestroy);
      if FormMMIFrame.TestMode then
        MessageBox(Handle, PWideChar('Error in FormDestroy:' + E.Message),
          PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);

    end;
  end;
end;


procedure TFormMainArea.FormShow(Sender: TObject);
begin
  inherited;

  MMIAreaA.Show;
  MMIAreaD.Show;

  UpdateDMI(true);


end;

{ *********************************************************
* updateCarAndTrainNames
* Description: Update the list of cars
********************************************************* }
procedure TFormMainArea.updateCarAndTrainNames();
var
  i: Integer;
  TrainId : String;
  TrainIdChanged : Boolean;

begin

  TrainId := DataModuleDMI.GetTrainId(TrainIdChanged);
    // First add name of locomotive.
  if TrainId = '' then
  begin
    MMIareaD.TrainLengthSecondLayerText := '[Loco]#';
  end
  else
  begin
    MMIareaD.TrainLengthSecondLayerText := '[' + TrainId + ']#';
  end;

    // Then add all car names.
  with FormMMIFrame do
  begin
    for i := 1 to MainCarList.NumberofCars do
    begin
      with MMIareaD do
      begin
        TrainLengthSecondLayerText := TrainLengthSecondLayerText +
          string(MainCarList.Car[i].ID);
        if i <> MainCarList.NumberofCars then
          TrainLengthSecondLayerText := TrainLengthSecondLayerText + '#';
      end;
    end;
  end;
end;

{ *********************************************************
* Function:    UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormMainArea.UpdateDMI(Refresh: Boolean);
var

  TrainId: string;
  TrainIdChanged: Boolean;

  TrainLength: Word;
  TrainLengthChanged: Boolean;

  DriverInfo: TDriverInfoMT;
  DriverInfoChanged: Boolean;

  SpeedAndDistance: TSpeed_DistanceMT;
  SpeedAndDistanceChanged: Boolean;

  ATPModeChanged : Boolean;

  MAMarginAdjust : LongInt;

begin
  inherited;

    { Train Id }
  TrainId := DataModuleDMI.GetTrainId(TrainIdChanged);
  if Refresh or TrainIdChanged then
  begin
    updateCarAndTrainNames;
  end;

      { Train Length }
  TrainLength := DataModuleDMI.GetTrainLength(TrainLengthChanged);
  if Refresh or TrainLengthChanged then
  begin
    MMIareaD.TrainLength := TrainLength;
  end;

  DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);
  if Refresh or DriverInfoChanged then
  begin

          // Setting direction
    if (DataModuleDMI.GetATPMode(ATPModeChanged) = amLocation) then
    begin
      if (DriverInfo.ActualDrivingDirection = addReverse) then
        MMIareaD.Direction := -1
      else
        MMIareaD.Direction := 1;
    end
    else
    begin
      if (DriverInfo.PermittedDrivingDirection = pddReverse) then
        MMIareaD.Direction := -1
      else
        MMIareaD.Direction := 1;
    end;

    MMIareaD.ShowBCA := ByteBool(DriverInfo.StatusD12 and INDREMDISTTOBCA);
    if ByteBool(DriverInfo.StatusD12 and INDREMDISTTOBCA) then
      MMIareaD.AddBCAStartToList(DriverInfo.RemainingDistanceToBCA,
        MMIareaD.CurrentOdometerPosition);
    MMIareaD.Dimmed := ByteBool(DriverInfo.StatusD11 and InBCA) or
      odoMeterInvalid;

    MMIareaA.TimeToIntervention := DriverInfo.TimeToIntervention;
    MMIareaA.DistanceToTarget := DriverInfo.RemainingDistanceToTargetPoint;

    MMIareaA.PredictedStopPosition :=
      DriverInfo.PredictedDistanceToStandStillLocation;

    MMIareaA.PredictedSpeedAtTarget := 0; // Not used

    MMIareaA.InBCA := ByteBool(DriverInfo.StatusD11 and InBCA);

    MMIareaA.Warning := ByteBool(DriverInfo.StatusD11 and ATPWARNING);

    if DriverInfo.Tspeed > 0 then
    begin
      { No margin for speedtarget }
      MMIareaA.MAMargin := 0;
    end
    else
    begin

      { Adjust MA margin (cm) to be shown relative to distance to SB target }
      MAMarginAdjust := 100 * (DataModuleDMI.GetDistanceToPrimaryTarget - DriverInfo.RemainingDistanceToTargetPoint);
      { Adjust only if adjustment is positive }
      if (MAMarginAdjust > 0) then
      begin
        if (DriverInfo.MAMargin > MAMarginAdjust) then
          MMIareaA.MAMargin := DriverInfo.MAMargin - MAMarginAdjust
        else
          { Margin can not be less than 0}
          MMIareaA.MAMargin := 0;
      end
      else
      begin
        { No adjustment }
        MMIareaA.MAMargin := DriverInfo.MAMargin;
      end;



    end;

       // Intervention indication
    if ByteBool(DriverInfo.StatusD11 and ATPINTERVENTION) then
    begin
      MMIareaA.Intervention := true;
    end
    else
    begin
      MMIareaA.Intervention := False;
    end;

    MMIareaA.ShowDistanceToTarget :=
      ByteBool(DriverInfo.StatusD12 and INDREMDISTTOTARGETPOINT);
    MMIareaA.ShowPredictedStopPosition :=
      ByteBool(DriverInfo.StatusD12 and INDPREDDISTTOSSLOC);
    MMIareaA.ShowPredictedSpeedAtTarget := False;
    MMIareaA.ShowTimeToIntervention :=
      ByteBool(DriverInfo.StatusD12 and INDTIMETOINT);

    MMIareaA.RefreshArea;
    MMIareaD.Refresh;
  end;

  SpeedAndDistance := DataModuleDMI.GetSpeedAndDistance
    (SpeedAndDistanceChanged);
  if Refresh or SpeedAndDistanceChanged then
  begin
    MMIareaD.CurrentOdometerPosition := SpeedAndDistance.CurrentOdometer;
    if (FormMMIFrame.TSPos > 0) then
    begin

      if (LabeledEditTrailing.Font.Color = LabeledEditTrailing.Color) then
      begin
            { Not visible but property Visible shall be true to make it click-able }
        LabeledEditTrailing.Visible := true;
        LabeledEditTrailing.Text := '';
      end
      else
      begin
        if SpeedAndDistance.TrailingTrackSection > 0 then
        begin { Valid trackSection to display }
          LabeledEditTrailing.Visible := true;
          LabeledEditTrailing.Text :=
            IntToStr(SpeedAndDistance.TrailingTrackSection) + ':' +
            FloatToStrF(SpeedAndDistance.TrailingPositionCm / 100.0,
            ffFixed, 8, 1);
        end
        else
        begin
          LabeledEditTrailing.Text := '';
        end;
      end;

      if (LabeledEditLeading.Font.Color = LabeledEditLeading.Color) then
      begin
            { Not visible but property Visible shall be true to make it click-able }
        LabeledEditLeading.Visible := true;
        LabeledEditLeading.Text := '';
      end
      else
      begin
        if SpeedAndDistance.LeadingTrackSection > 0 then
        begin { Valid trackSection to display }
          LabeledEditLeading.Visible := true;
          LabeledEditLeading.Text :=
            IntToStr(SpeedAndDistance.LeadingTrackSection) + ':' +
            FloatToStrF(SpeedAndDistance.LeadingPositionCm / 100.0,
            ffFixed, 8, 1);
        end
        else
        begin
          LabeledEditLeading.Text := '';
        end;
      end;
    end;
  end;
end;

initialization

{ ****************************************************************************
* FINALIZATION PART                                                         *
**************************************************************************** }
finalization

{ ****************************************************************************
* EXPORTS DECLARATIONS                                                     *
**************************************************************************** }

{ ****************************************************************************
* RESOURCE STRING DECLARATIONS                                              *
**************************************************************************** }
end.

