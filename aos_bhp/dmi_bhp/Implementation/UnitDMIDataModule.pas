unit UnitDMIDataModule;

interface

uses
  SysUtils, Classes, MidasLib, DB, DBClient, MMITypes;

type

  TTypicalConfigBlockStorage = Record
    TypicalConfigName: AnsiString;
    VehicleTypeName: AnsiString;
    CarsinTypicalConfig: Byte;
  End;

  TDataModuleDMI = class(TDataModule)
    ClientDataSetDMIEvents: TClientDataSet;
    ClientDataSetDMITrainComp: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleClean();
    procedure ClientDataSetDMIEventsAfterScroll(DataSet: TDataSet);
    procedure ClientDataSetDMITrainCompAfterScroll(DataSet: TDataSet);

  private
    { Private declarations }
    TrainId: String;
    TrainIdChanged: Boolean;

    { Radio Area }
    RadioArea: String;
    RadioAreaChanged: Boolean;

    TrainLength: Word;
    TrainLengthChanged: Boolean;

    ATPMode: T_ATPmodes;
    ATPModeChanged: Boolean;

    ATPState: T_ATPStates;
    ATPStateChanged: Boolean;

    ATPSubState: Byte; // Service or Config SubState
    ATPSubStateChanged: Boolean;

    ConfigSubState: TConfigSubState;
    ConfigSubStateChanged: Boolean;

    ATPVerificationState: T_ATPVerificationStates;
    ATPVerificationStateChanged: Boolean;

    ATOMode: T_ATOModes;
    ATOModeChanged: Boolean;

    ATOSwitch: T_ATOSwitch;
    ATOSwitchChanged: Boolean;

    LocoStatus: LongWord;
    LocoStatusChanged: Boolean;

    InterfaceFlags: Byte;
    InterfaceFlagsChanged: Boolean;

    AdditionalStatus1: Byte;
    AdditionalStatus1Changed: Boolean;

    AdditionalStatus2: Byte;
    AdditionalStatus2Changed: Boolean;

    AllowedButtons: Byte;
    AllowedButtonsChanged: Boolean;

    BrakeTestStatus: T_BrakeTestStatus;
    BrakeTestStatusChanged: Boolean;

    RemainingTimeToBrakeTest: Word;
    RemainingTimeToBrakeTestChanged: Boolean;

    RemainingTimeToBTMTest: Word;
    RemainingTimeToBTMTestChanged: Boolean;

    { Driver info }
    DriverInfo: TDriverInfoMT;
    DriverInfoChanged: Boolean;

    { Speed and distance }
    SpeedAndDistance: TSpeed_DistanceMT;
    SpeedAndDistanceChanged: Boolean;

    DistanceToPrimaryTarget : LongInt;
    PrimaryTargetOdoPos : LongInt;

    { Re-registration }
    ReRegOptions: Byte;
    ReRegOptionsChanged: Boolean;

    { Config }
    ConfigOptions: Byte;
    ConfigOptionsChanged: Boolean;

    { Confirm Mode Change }
    ConfirmModeChange: Byte;
    ConfirmModeChanged: Boolean;

    AdditionalAllowedToInfo: Byte;
    AdditionalAllowedToInfoChanged: Boolean;

    AdditionalConfirmationInfo: Byte;
    AdditionalConfirmationInfoChanged: Boolean;

    AdditionalConfirmationInfo2 : Byte;
    AdditionalConfirmationInfo2Changed : Boolean;

    PlatformStatusInfo : Byte;
    PlatformStatusInfoChanged : Boolean;

    AdaptationTrainStatus: Byte;
    AdaptationTrainStatusChanged: Boolean;

    TIMSAvailable: Boolean;
    TIMSOK: Boolean;
    LocomotiveConnected: TLocomotiveConnected; // reported from ATP

    { VehicleType }
    VehicleTypesStorage: array [1 .. MAX_VEHICLE_TYPE_BLOCKS] of AnsiString;
    VehicleTypeChanged: Boolean;

    { TypicalConfigType }
    TypicalConfigStorage: array [1 .. MAX_TYPICAL_CONFIG_BLOCKS]
      of TTypicalConfigBlockStorage;
    TypicalConfigChanged: Boolean;

    { TrainVsTrackDirWanted Message Received }
    TrainVsTrackDirWantedReceived: Boolean;
    TrainVsTrackDirWantedChanged: Boolean;

    { Number of vehicle with loco }
    NumberOfVehiclesConnected: Integer;
    VehicleDataReceived: Boolean;

    { Location Data }
    LocationName: String;
    LocationNameChanged: Boolean;
    LocationType: Integer;
    LocationTypeChanged: Boolean;

    { Train Weight }
    TrainWeight: LongWord;
    TrainWeightChanged: Boolean;

    { Estimated Time of Arriavel }
    ETA: UInt32;
    ETAChanged: Boolean;

  public
    { Public declarations }
    procedure AddEvent(TimeStamp: TDateTime; Text: String);
    function GetTrainId(var Changed: Boolean): String;
    procedure SetTrainId(NewTrainId: String);
    function GetRadioArea(var Changed: Boolean): String;
    procedure SetRadioArea(NewRadioArea: String);
    function GetTrainLength(var Changed: Boolean): Word;
    procedure SetTrainLength(NewTrainLength: Word);
    function GetATPMode(var Changed: Boolean): T_ATPmodes;
    procedure SetATPMode(NewATPMode: T_ATPmodes);
    function GetATPState(var Changed: Boolean): T_ATPStates;
    procedure SetATPState(NewATPState: T_ATPStates);
    function GetATPSubState(var Changed: Boolean): Byte;
    procedure SetATPSubState(NewATPSubState: Byte);
    function GetConfigSubState(var Changed: Boolean): TConfigSubState;
    procedure SetConfigSubState(NewConfigSubState: TConfigSubState);
    function GetATPVerificationState(var Changed: Boolean)
      : T_ATPVerificationStates;
    procedure SetATPVerificationState(NewATPVerificationState
      : T_ATPVerificationStates);
    function GetATOMode(var Changed: Boolean): T_ATOModes;
    procedure SetATOMode(NewATOMode: T_ATOModes);
    function GetATOSwitch(var Changed: Boolean): T_ATOSwitch;
    procedure SetATOSwitch(NewATOSwitch: T_ATOSwitch);
    function GetLocoStatus(var Changed: Boolean): LongWord;
    procedure SetLocoStatus(NewLocoStatus: LongWord);
    function GetInterfaceFlags(var Changed: Boolean): Byte;
    procedure SetInterfaceFlags(NewInterfaceFlags: Byte);
    function GetAllowedButtons(var Changed: Boolean): Byte;
    procedure SetAllowedButtons(NewAllowedButtons: Byte);
    function GetAdditionalStatus1(var Changed: Boolean): Byte;
    procedure SetAdditionalStatus1(NewAdditionalStatus1: Byte);
    function GetAdditionalStatus2(var Changed: Boolean): Byte;
    procedure SetAdditionalStatus2(NewAdditionalStatus2: Byte);
    function GetBrakeTestStatus(var Changed: Boolean): T_BrakeTestStatus;
    procedure SetBrakeTestStatus(NewBrakeTestStatus: T_BrakeTestStatus);
    function GetRemainingTimeToBrakeTest(var Changed: Boolean): Word;
    procedure SetRemainingTimeToBrakeTest(NewRemainingTimeToBrakeTest: Word);
    function GetRemainingTimeToBTMTest(var Changed: Boolean): Word;
    procedure SetRemainingTimeToBTMTest(NewRemainingTimeToBTMTest: Word);
    function GetDriverInfo(var Changed: Boolean): TDriverInfoMT;
    procedure SetDriverInfo(NewDriverInfo: TDriverInfoMT);
    function GetSpeedAndDistance(var Changed: Boolean): TSpeed_DistanceMT;
    procedure SetSpeedAndDistance(NewSpeedAndDistance: TSpeed_DistanceMT);
    function GetDistanceToPrimaryTarget : LongInt;
    procedure SetDistanceToPrimaryTarget(NewDistanceToPrimaryTarget : LongInt);
    function GetPrimaryTargetOdoPos : LongInt;
    procedure SetPrimaryTargetOdoPos(NewPrimaryTargetOdoPos : LongInt);

    function GetRadioAvailable: Boolean;
    function GetReRegOptions(var Changed: Boolean): Byte;
    procedure SetReRegOptions(NewReRegOptions: Byte);
    function GetConfigOptions(var Changed: Boolean): Byte;
    procedure SetConfigOptions(NewConfigOptions: Byte);
    function GetConfirmModeChange(var Changed: Boolean): Byte;
    procedure SetConfirmModeChange(NewConfirmModeChange: Byte);
    function GetTIMSAvailable: Boolean;
    function GetTIMSOK: Boolean;
    function GetLocoVsTrainDir: TLocomotiveConnected;
    procedure SetLocoVsTrainDir(Dir: TLocomotiveConnected);
    procedure ClearChanged;

    procedure SetTypicalConfig(TypicalConfigType: Byte;
      TypicalConfigName: AnsiString; VehicleTypeName: AnsiString;
      NumberofCars: Byte);
    function GetTypicalConfigVehicleTypeName(TypicalConfigType: Byte): AnsiString;
    function GetTypicalConfigName(TypicalConfigType: Byte): AnsiString;
    function GetTypicalConfigCarsCount(TypicalConfigType:Byte):Byte;
    function GetTypicalConfigType(TypicalConfigTypeName: AnsiString): Byte;
    procedure ClearTypicalConfigStorage();
    function  VehicleTypeNameExists(TypicalConfigVehicleTypeName:AnsiString): Boolean;
    procedure SetVehicleType(VehicleType: Byte; VehicleTypeName: AnsiString);
    function GetVehicleType(VehicleTypeName: AnsiString): Byte;
    function GetVehicleTypeName(VehicleType: Byte): AnsiString;
    procedure SetVehicleData(SrNumber: Word; NrOfVehicle: Integer;
      VehicleType: Byte; VehicleNodeId: Word; VehicleName: AnsiString);
    procedure ClearVehicleData();
    function GetTrainVsTrackDirWantedReceived(var Changed: Boolean): Boolean;
    procedure SetTrainVsTrackDirWantedReceived(Flag: Boolean);
    procedure SetNbOfVehiclesConnected(Count: Integer);
    function GetNbOfVehiclesConnected(): Integer;
    function GetAdditionalAllowedToInfo(var Changed: Boolean): Byte;
    procedure SetAdditionalAllowedToInfo(NewAdditionalAllowedToInfo: Byte);
    function GetAdditionalConfirmationInfo(var Changed: Boolean): Byte;
    procedure SetAdditionalConfirmationInfo(NewAditionalConfirmationInfo: Byte);
    function GetAdaptationTrainStatus(var Changed: Boolean): Byte;
    procedure SetAdaptationTrainStatus(NewAdaptationTrainStatus: Byte);
    function GetVehicleDataReceived(): Boolean;
    procedure SetLocationName(NewLocationName: String);
    function GetLocationName(var Changed: Boolean): String;
    procedure SetLocationType(var NewLocType: Integer);
    function GetLocationType(var Changed: Boolean): Integer;
    procedure SetTrainWeight(var NewTrainWeight: LongWord);
    function GetTrainWeight(var Changed: Boolean): LongWord;
    procedure SetETA(var NewETA: UInt32);
    function GetETA(var Changed: Boolean): UInt32;

    procedure SetAdditionalConfirmationInfo2(var NewAdditionalConfirmationInfo2: Byte);
    function GetAdditionalConfirmationInfo2(var Changed: Boolean): Byte;

    procedure SetPlatformStatusInfo( var NewPlatformStatusInfo : Byte);
    function GetPlatformStatusInfo(var Changed: Boolean): Byte;

  end;

var
  DataModuleDMI: TDataModuleDMI;

implementation

uses UnitMainLayer1, Windows, UnitMainArea, UnitMMIFrame, UnitNoMAControlled,
  UnitLogin, UnitManualConfiguration, UnitReReg, UnitTrainComposition,
  UnitAutomaticConfiguration, UnitTrainVsTrack, UnitFullScreenMsg,
  UnitPowerDown, UnitSafetyHalt, UnitUnregForm;

{$R *.dfm}

{ *********************************************************
  *    ClientDataSetDMIEventsAfterScroll
  * Description: Fix to remove the scroll-bar on the grid after scrolling
  ********************************************************* }
procedure TDataModuleDMI.ClientDataSetDMIEventsAfterScroll(DataSet: TDataSet);
begin
  ShowScrollBar(FormMainArea.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormNoMAControlled.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormLogin.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormManualConfiguration.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormAutomaticConfiguration.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormReRegistration.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormTrainVsTrack.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormFullScreenMsg.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormPowerDown.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormSafetyHalt.DBGridEvents.Handle, SB_VERT, False);
  ShowScrollBar(FormUnreg.DBGridEvents.Handle, SB_VERT, False);
end;

procedure TDataModuleDMI.ClientDataSetDMITrainCompAfterScroll
  (DataSet: TDataSet);
begin
  ShowScrollBar(FormTrainComposition.DBGridTrainComp.Handle, SB_VERT, False);
end;

{ *********************************************************
  *     DataModuleCreate
  * Description: Setup dataset and init variables
  ********************************************************* }
procedure TDataModuleDMI.DataModuleCreate(Sender: TObject);
begin

  { Create event-log storage }
  with ClientDataSetDMIEvents do
  begin
    FieldDefs.Add('Time', ftDateTime);
    FieldDefs.Add('Text', ftString, 50);
    CreateDataSet;
    LogChanges := False;
  end;

  { Create Train Composition storage }
  with ClientDataSetDMITrainComp do
  begin
    FieldDefs.Add('SrNbr', ftInteger);
    FieldDefs.Add('NrOfVehicle', ftInteger);
    FieldDefs.Add('VehicleType', ftInteger);
    FieldDefs.Add('VehicleTypeName', ftString, VEHICLE_TYPE_NAME_MAX_CHARS);
    FieldDefs.Add('VehicleNodeId', ftInteger);
    FieldDefs.Add('VehicleName', ftString, VEHICLE_NAME_MAX_CHARS);

    CreateDataSet;
    LogChanges := False;
  end;

  DataModuleClean;

end;

{ *********************************************************
  *     DataModuleClean
  * Description: Clean the Data Module
  ********************************************************* }
procedure TDataModuleDMI.DataModuleClean();
var
  VehicleType: Byte;
begin

  ClientDataSetDMITrainComp.EmptyDataSet;
  ClientDataSetDMIEvents.EmptyDataSet;

  // Reset vehicle type
  for VehicleType := 1 to MAX_VEHICLE_TYPE_BLOCKS do
    VehicleTypesStorage[VehicleType] := AnsiString('');

  //Reset TypicalConfigType
  ClearTypicalConfigStorage;

  { Init TrainId }
  TrainId := '';
  TrainLength := 0;

  { Init ATP Mode }
  ATPMode := amUndefined;
  ATPState := asUndefined;
  ATPSubState := 0;
  ConfigSubState := cssUndefined;
  ATPVerificationState := avsUndefined;

  { Init ATO Mode }
  ATOMode := atomUndefined;
  ATOSwitch := ATOSwitch_Undefined;

  { Init other statuses }
  LocoStatus := 0;
  InterfaceFlags := 0;
  AdditionalStatus1 := 0;
  AdditionalStatus2 := 0;

  AdditionalAllowedToInfo := 0;
  AdditionalConfirmationInfo := 0;
  AdaptationTrainStatus := 0;

  { Init allowed buttons }
  AllowedButtons := 0;

  { Init brake / BTM - test }
  BrakeTestStatus := btsUndefined;
  RemainingTimeToBrakeTest := 0;
  RemainingTimeToBTMTest := 0;

  { Init driver info }
  DriverInfo.PermittedDrivingDirection := pddUndefined;
  DriverInfo.Pspeed := 0;
  DriverInfo.Tspeed := 0;
  DriverInfo.TimeToIntervention := 0;
  DriverInfo.RemainingDistanceToTargetPoint := 0;
  DriverInfo.RemainingDistanceToBCA := 0;
  DriverInfo.PredictedDistanceToStandStillLocation := 0;
  DriverInfo.Spare := 0;
  DriverInfo.StatusD11 := 0;
  DriverInfo.StatusD12 := 0;
  DriverInfo.ActualDrivingDirection := addUndefined;
  DriverInfo.MAMargin := 0;

  { Speed and distance }
  SpeedAndDistance.CurrentSpeed := 0;
  SpeedAndDistance.CurrentOdometer := 0;
  SpeedAndDistance.leadingTrackSection := 0;
  SpeedAndDistance.leadingPositionCm := 0;
  SpeedAndDistance.trailingTrackSection := 0;
  SpeedAndDistance.trailingPositionCm := 0;

  DistanceToPrimaryTarget := 0;
  PrimaryTargetOdoPos := 0;

  ReRegOptions := 0;
  ConfigOptions := 0;
  TIMSAvailable := False;
  TIMSOK := False;
  LocomotiveConnected := lcUndefined;
  ClearChanged;
  TrainVsTrackDirWantedReceived := False;
  NumberOfVehiclesConnected := 0;

  { Train Composition Data }
  TrainWeight := 0;
  ETA := 0;

  { Location Data }
  LocationName := '';
  LocationType := 0;

  VehicleDataReceived := False;
end;

{ *********************************************************
  *    AddEvent
  * Description:Add a DMI-event to the event-storage
  ********************************************************* }
procedure TDataModuleDMI.AddEvent(TimeStamp: TDateTime; Text: String);
begin
  if ClientDataSetDMIEvents.RecordCount > 0 then
    ClientDataSetDMIEvents.First;

  ClientDataSetDMIEvents.Insert;
  ClientDataSetDMIEvents.FieldValues['Time'] := TimeStamp;
  ClientDataSetDMIEvents.FieldValues['Text'] := Text;
  ClientDataSetDMIEvents.Post();

  { Fix to hide the scroll-bar }
  ShowScrollBar(FormMainArea.DBGridEvents.Handle, SB_VERT, False);

end;

{ *********************************************************
  *    SetVehicleData
  * Description: Add Vehicle Data recieved from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetVehicleData(SrNumber: Word; NrOfVehicle: Integer;
  VehicleType: Byte; VehicleNodeId: Word; VehicleName: AnsiString);
var
  VehicleTypeName: AnsiString;

begin

  if (VehicleType = 0) or ((VehicleType > MAX_VEHICLE_TYPE_BLOCKS) and
    (VehicleType < LOCO_TYPE_RANGE_START)) then
    VehicleTypeName := AnsiString('Undefined:' + IntToStr(VehicleType))
  else if ((VehicleType > 0) and (VehicleType <= MAX_VEHICLE_TYPE_BLOCKS)) then
  begin
    VehicleTypeName := GetVehicleTypeName(VehicleType);
    if (VehicleTypeName = '') then
      VehicleTypeName := AnsiString('Undefined:' + IntToStr(VehicleType));
  end
  else if ((VehicleType >= LOCO_TYPE_RANGE_START) and
    (VehicleType <= LOCO_TYPE_RANGE_END)) then
    VehicleTypeName := 'LOCO:' + IntToStr(VehicleType)
  else
    VehicleTypeName := 'VEHICLE:' + IntToStr(VehicleType);

  ClientDataSetDMITrainComp.Append;
  ClientDataSetDMITrainComp.FieldValues['SrNbr'] := SrNumber;
  ClientDataSetDMITrainComp.FieldValues['NrOfVehicle'] := NrOfVehicle;
  ClientDataSetDMITrainComp.FieldValues['VehicleType'] := VehicleType;
  ClientDataSetDMITrainComp.FieldValues['VehicleTypeName'] := VehicleTypeName;
  ClientDataSetDMITrainComp.FieldValues['VehicleNodeId'] := VehicleNodeId;
  ClientDataSetDMITrainComp.FieldValues['VehicleName'] := VehicleName;
  ClientDataSetDMITrainComp.Post();
  VehicleDataReceived := true;
end;

{ *********************************************************
  *    ClearVehicleData
  * Description: Add Vehicle Data recieved from ATP
  ********************************************************* }
procedure TDataModuleDMI.ClearVehicleData();
begin

  if ClientDataSetDMITrainComp.RecordCount > 0 then
  begin
    ClientDataSetDMITrainComp.EmptyDataSet;
  end;
end;

{ *********************************************************
  *    GetTrainId
  * Description:Access-function for TrainId
  ********************************************************* }
function TDataModuleDMI.GetTrainId(var Changed: Boolean): String;
begin
  Changed := TrainIdChanged;
  Result := TrainId;
end;

{ *********************************************************
  *    SetTrainId
  * Description:Set new TrainId
  ********************************************************* }
procedure TDataModuleDMI.SetTrainId(NewTrainId: String);
begin
  if TrainId <> NewTrainId then
  begin
    TrainId := NewTrainId;
    TrainIdChanged := true;
  end;
end;

{ *********************************************************
  *    SetRadioArea
  * Description:Set new Radio Area
  ********************************************************* }
procedure TDataModuleDMI.SetRadioArea(NewRadioArea: String);
begin
  if RadioArea <> NewRadioArea then
  begin
    RadioArea := NewRadioArea;
    RadioAreaChanged := true;
  end;
end;

{ *********************************************************
  *    GetRadioArea
  * Description:Access-function for Radio Area
  ********************************************************* }
function TDataModuleDMI.GetRadioArea(var Changed: Boolean): String;
begin
  Changed := RadioAreaChanged;
  Result := RadioArea;
end;

{ *********************************************************
  *    GetATPMode
  * Description:Access-function for ATP-mode
  ********************************************************* }
function TDataModuleDMI.GetATPMode(var Changed: Boolean): T_ATPmodes;
begin
  Changed := ATPModeChanged;
  Result := ATPMode;
end;

{ *********************************************************
  *    SetATPMode
  * Description:Set new ATPMode
  ********************************************************* }
procedure TDataModuleDMI.SetATPMode(NewATPMode: T_ATPmodes);
begin
  if ATPMode <> NewATPMode then
  begin
    ATPMode := NewATPMode;
    ATPModeChanged := true;
  end;
end;

{ *********************************************************
  * GetATOMode
  * Description:Access-function for ATO-mode
  ********************************************************* }
function TDataModuleDMI.GetATOMode(var Changed: Boolean): T_ATOModes;
begin
  Changed := ATOModeChanged;
  Result := ATOMode;
end;

{ *********************************************************
  * SetATOMode
  * Description:Set new ATOMode
  ********************************************************* }
procedure TDataModuleDMI.SetATOMode(NewATOMode: T_ATOModes);
begin
  if ATOMode <> NewATOMode then
  begin
    ATOMode := NewATOMode;
    ATOModeChanged := true;
  end;
end;

{ *********************************************************
  * GetATOSwitch
  * Description:Access-function for ATO-switch position
  ********************************************************* }
function TDataModuleDMI.GetATOSwitch(var Changed: Boolean): T_ATOSwitch;
begin
  Changed := ATOSwitchChanged;
  Result := ATOSwitch;
end;

{ *********************************************************
  * SetATOSwitch
  * Description:Set new ATO-switch position
  ********************************************************* }
procedure TDataModuleDMI.SetATOSwitch(NewATOSwitch: T_ATOSwitch);
begin
  if ATOSwitch <> NewATOSwitch then
  begin
    ATOSwitch := NewATOSwitch;
    ATOSwitchChanged := true;
  end;
end;

{ *********************************************************
  * GetAdditionalStatus1
  * Description:Get byte with allowed buttons bits
  ********************************************************* }
function TDataModuleDMI.GetAdditionalStatus1(var Changed: Boolean): Byte;
begin
  Changed := AdditionalStatus1Changed;
  Result := AdditionalStatus1;
end;

{ *********************************************************
  * SetAdditionalStatus1
  * Description:Set new allowed buttons
  ********************************************************* }
procedure TDataModuleDMI.SetAdditionalStatus1(NewAdditionalStatus1: Byte);
begin
  if AdditionalStatus1 <> NewAdditionalStatus1 then
  begin
    AdditionalStatus1 := NewAdditionalStatus1;
    AdditionalStatus1Changed := true;
  end;

end;

{ *********************************************************
  * GetAdditionalStatus2
  * Description:Get byte with allowed buttons bits
  ********************************************************* }
function TDataModuleDMI.GetAdditionalStatus2(var Changed: Boolean): Byte;
begin
  Changed := AdditionalStatus2Changed;
  Result := AdditionalStatus2;
end;

{ *********************************************************
  * SetAdditionalStatus1
  * Description:Set new allowed buttons
  ********************************************************* }
procedure TDataModuleDMI.SetAdditionalStatus2(NewAdditionalStatus2: Byte);
begin
  if AdditionalStatus2 <> NewAdditionalStatus2 then
  begin
    AdditionalStatus2 := NewAdditionalStatus2;
    AdditionalStatus2Changed := true;
  end;

end;

{ *********************************************************
  * GetLocoStatus
  * Description:Get locomotive (train) status
  ********************************************************* }
function TDataModuleDMI.GetLocoStatus(var Changed: Boolean): LongWord;
begin
  Changed := LocoStatusChanged;
  Result := LocoStatus;
end;

{ *********************************************************
  * SetLocoStatus
  * Description:Set new LocoStatus
  ********************************************************* }
procedure TDataModuleDMI.SetLocoStatus(NewLocoStatus: LongWord);
begin
  if LocoStatus <> NewLocoStatus then
  begin
    LocoStatus := NewLocoStatus;
    LocoStatusChanged := true;
  end;

end;

{ *********************************************************
  * GetInterfaceFlags
  * Description:Get byte with InterfaceFlags
  ********************************************************* }
function TDataModuleDMI.GetInterfaceFlags(var Changed: Boolean): Byte;
begin
  Changed := InterfaceFlagsChanged;
  Result := InterfaceFlags;
end;

{ *********************************************************
  * SetInterfaceFlags
  * Description:Set new InterfaceFlags
  ********************************************************* }
procedure TDataModuleDMI.SetInterfaceFlags(NewInterfaceFlags: Byte);
begin
  if InterfaceFlags <> NewInterfaceFlags then
  begin
    InterfaceFlags := NewInterfaceFlags;
    InterfaceFlagsChanged := true;

    if bytebool(Byte(InterfaceFlags) and TIMS_AVAILABLE) then
      TIMSAvailable := true
    else
      TIMSAvailable := False;

    if bytebool(Byte(InterfaceFlags) and TIMS_OK) then
      TIMSOK := true
    else
      TIMSOK := False;

  end;

end;

{ *********************************************************
  * GetAllowedButtons
  * Description:Get byte with allowed buttons bits
  ********************************************************* }
function TDataModuleDMI.GetAllowedButtons(var Changed: Boolean): Byte;
begin
  Changed := AllowedButtonsChanged;
  Result := AllowedButtons;
end;

{ *********************************************************
  * SetAllowedButtons
  * Description:Set new allowed buttons
  ********************************************************* }
procedure TDataModuleDMI.SetAllowedButtons(NewAllowedButtons: Byte);
begin
  if AllowedButtons <> NewAllowedButtons then
  begin
    AllowedButtons := NewAllowedButtons;
    AllowedButtonsChanged := true;
  end;

end;

{ *********************************************************
  * GetBrakeTestStatus
  * Description:Get status of brake-test
  ********************************************************* }
function TDataModuleDMI.GetBrakeTestStatus(var Changed: Boolean)
  : T_BrakeTestStatus;
begin
  Changed := BrakeTestStatusChanged;
  Result := BrakeTestStatus;
end;

{ *********************************************************
  * SetBrakeTestStatus
  * Description:Set status of brake-test
  ********************************************************* }
procedure TDataModuleDMI.SetBrakeTestStatus(NewBrakeTestStatus
  : T_BrakeTestStatus);
begin
  if BrakeTestStatus <> NewBrakeTestStatus then
  begin
    BrakeTestStatus := NewBrakeTestStatus;
    BrakeTestStatusChanged := true;
  end;
end;

{ *********************************************************
  * GetRemainingTimeToBrakeTest
  * Description:Get time left to mandatory brake-test
  ********************************************************* }
function TDataModuleDMI.GetRemainingTimeToBrakeTest(var Changed: Boolean): Word;
begin
  Changed := RemainingTimeToBrakeTestChanged;
  Result := RemainingTimeToBrakeTest;
end;

{ *********************************************************
  * SetRemainingTimeToBrakeTest
  * Description:Set time left to mandatory brake-test
  ********************************************************* }
procedure TDataModuleDMI.SetRemainingTimeToBrakeTest
  (NewRemainingTimeToBrakeTest: Word);
begin
  if RemainingTimeToBrakeTest <> NewRemainingTimeToBrakeTest then
  begin
    RemainingTimeToBrakeTest := NewRemainingTimeToBrakeTest;
    RemainingTimeToBrakeTestChanged := true;
  end;

end;

{ *********************************************************
  * GetRemainingTimeToBTMTest
  * Description:Get time left to mandatory BTM-test
  ********************************************************* }
function TDataModuleDMI.GetRemainingTimeToBTMTest(var Changed: Boolean): Word;
begin
  Changed := RemainingTimeToBTMTestChanged;
  Result := RemainingTimeToBTMTest;
end;

{ *********************************************************
  * SetRemainingTimeToBTMTest
  * Description:Set time left to mandatory BTM-test
  ********************************************************* }
procedure TDataModuleDMI.SetRemainingTimeToBTMTest
  (NewRemainingTimeToBTMTest: Word);
begin
  if RemainingTimeToBTMTest <> NewRemainingTimeToBTMTest then
  begin
    RemainingTimeToBTMTest := NewRemainingTimeToBTMTest;
    RemainingTimeToBTMTestChanged := true;
  end;

end;

{ *********************************************************
  * GetDriverInfo
  * Description:Get the driver info last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetDriverInfo(var Changed: Boolean): TDriverInfoMT;
begin
  Changed := DriverInfoChanged;
  Result := DriverInfo;
end;

{ *********************************************************
  * SetDriverInfo
  * Description:Set the driver info received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetDriverInfo(NewDriverInfo: TDriverInfoMT);
begin
  DriverInfo := NewDriverInfo;
  DriverInfoChanged := true;
end;

{ *********************************************************
  * GetSpeedAndDistance
  * Description:Get the SpeedAndDistance last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetSpeedAndDistance(var Changed: Boolean)
  : TSpeed_DistanceMT;
begin
  Changed := SpeedAndDistanceChanged;
  Result := SpeedAndDistance;

end;

{ *********************************************************
  * SetSpeedAndDistance
  * Description:Set the SpeedAndDistance received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetSpeedAndDistance(NewSpeedAndDistance
  : TSpeed_DistanceMT);
begin
  SpeedAndDistance := NewSpeedAndDistance;
  SpeedAndDistanceChanged := true;

end;

{ *********************************************************
  * GetDistanceToPrimaryTarget
  * Description:Get the distance to primary target (m)
  * Only valid when in BCA (otherwise 0)
  ********************************************************* }
function TDataModuleDMI.GetDistanceToPrimaryTarget : LongInt;
begin

  Result := DistanceToPrimaryTarget;

end;

{ *********************************************************
  * SetDistanceToPrimaryTarget
  * Description:Set the distance to primary target (m)
  * Only valid when in BCA (otherwise set to 0)
  ********************************************************* }
procedure TDataModuleDMI.SetDistanceToPrimaryTarget(NewDistanceToPrimaryTarget : LongInt);
begin

  DistanceToPrimaryTarget := NewDistanceToPrimaryTarget;

end;

{ *********************************************************
  * GetPrimaryTargetOdoPos
  * Description:Get the pos of the primary target (m)
  ********************************************************* }
function TDataModuleDMI.GetPrimaryTargetOdoPos : LongInt;
begin

  Result := PrimaryTargetOdoPos;

end;

{ *********************************************************
  * SetPrimaryTargetOdoPos
  * Description:Set the pos of primary target (m)
  ********************************************************* }
procedure TDataModuleDMI.SetPrimaryTargetOdoPos(NewPrimaryTargetOdoPos : LongInt);
begin

  PrimaryTargetOdoPos := NewPrimaryTargetOdoPos;

end;

{ *********************************************************
  * GetRadioAvailabld
  * Description:Check the RadioAvailable-bit in the last received
  * DriverInfo and return its status
  ********************************************************* }
function TDataModuleDMI.GetRadioAvailable: Boolean;
var
  RadioAvail: Boolean;
begin
  if bytebool(DriverInfo.StatusD11 and RADIOAVAILABLE) then
    RadioAvail := true
  else
    RadioAvail := False;
  Result := RadioAvail;
end;

{ *********************************************************
  * GetReRegOptions
  * Description: Get the ReRegOptions last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetReRegOptions(var Changed: Boolean): Byte;
begin
  Changed := ReRegOptionsChanged;
  Result := ReRegOptions;
end;

{ *********************************************************
  * SetReRegOptions
  * Description: Get the ReRegOptions received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetReRegOptions(NewReRegOptions: Byte);
begin
  if ReRegOptions <> NewReRegOptions then
  begin
    ReRegOptions := NewReRegOptions;
    ReRegOptionsChanged := true;

    if bytebool(ReRegOptions and CARS_CONNECTED_AT_A_END) then
      LocomotiveConnected := lcAEnd
    else
      LocomotiveConnected := lcBEnd;

  end;
end;

{ *********************************************************
  * GetConfigOptions
  * Description: Get the ConfigOptions last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetConfigOptions(var Changed: Boolean): Byte;
begin
  Changed := ConfigOptionsChanged;
  Result := ConfigOptions;

end;

{ *********************************************************
  * SetConfigOptions
  * Description: Set the ConfigOptions received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetConfigOptions(NewConfigOptions: Byte);
begin
  if ConfigOptions <> NewConfigOptions then
  begin
    ConfigOptions := NewConfigOptions;
    ConfigOptionsChanged := true;
  end;

end;

{ *********************************************************
  * GetTrainLength
  * Description: Get the TrainLength last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetTrainLength(var Changed: Boolean): Word;
begin
  Changed := TrainLengthChanged;
  Result := TrainLength;

end;

{ *********************************************************
  * SetTrainLength
  * Description: Set the TrainLength received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetTrainLength(NewTrainLength: Word);
begin

  if TrainLength <> NewTrainLength then
  begin
    TrainLength := NewTrainLength;
    TrainLengthChanged := true;
    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('COMM'),
      'TrainLength:' + IntToStr(TrainLength));
  end;

end;

{ *********************************************************
  * GetATPState
  * Description: Get the ATPState last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetATPState(var Changed: Boolean): T_ATPStates;
begin
  Changed := ATPStateChanged;
  Result := ATPState;
end;

{ *********************************************************
  * SetATPState
  * Description: Set the ATPState received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetATPState(NewATPState: T_ATPStates);
begin

  if ATPState <> NewATPState then
  begin
    ATPState := NewATPState;
    ATPStateChanged := true;
    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('COMM'),
      'ATPState:' + IntToStr(ord(ATPState)));
  end;

end;

{ *********************************************************
  * GetATPSubState
  * Description: Get the ATPSubState last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetATPSubState(var Changed: Boolean): Byte;
begin
  Changed := ATPSubStateChanged;
  Result := ATPSubState;

end;

{ *********************************************************
  * SetATPSubState
  * Description: Set the ATPSubState received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetATPSubState(NewATPSubState: Byte);
begin

  if ATPSubState <> NewATPSubState then
  begin
    ATPSubState := NewATPSubState;
    ATPSubStateChanged := true;

    // if (ATPMode = amTrainconfiguration) then
    ConfigSubState := TConfigSubState(ATPSubState);

    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('COMM'),
      'ATPSubState:' + IntToStr(ATPSubState));
  end;

end;

{ *********************************************************
  * GetConfigSubState
  * Description: Get the ConfigSubState last received from ATP
  * Note. ConfigSubstate and ATPSubState occupythe same byte in the message from ATP
  ********************************************************* }
function TDataModuleDMI.GetConfigSubState(var Changed: Boolean)
  : TConfigSubState;
begin
  Changed := ConfigSubStateChanged;
  Result := ConfigSubState;

end;

{ *********************************************************
  * SetConfigSubState
  * Description: Set the ConfigSubState received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetConfigSubState(NewConfigSubState: TConfigSubState);
begin

  if ConfigSubState <> NewConfigSubState then
  begin
    ConfigSubState := NewConfigSubState;
    ConfigSubStateChanged := true;
    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('COMM'),
      'ConfigSubState:' + IntToStr(ord(ConfigSubState)));
  end;

end;

{ *********************************************************
  * GetATPVerificationState
  * Description: Get the ATPVerification state last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetATPVerificationState(var Changed: Boolean)
  : T_ATPVerificationStates;
begin
  Changed := ATPVerificationStateChanged;
  Result := ATPVerificationState;

end;

{ *********************************************************
  * SetATPVerificationState
  * Description: Set the ATPVerification state received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetATPVerificationState(NewATPVerificationState
  : T_ATPVerificationStates);
begin

  if ATPVerificationState <> NewATPVerificationState then
  begin
    ATPVerificationState := NewATPVerificationState;
    ATPVerificationStateChanged := true;
    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('COMM'),
      'ATPVerificationState:' + IntToStr(ord(ATPVerificationState)));
  end;

end;

{ *********************************************************
  * GetTIMSAvailable
  * Description: Get the TIMSAvailable flag received from ATP
  ********************************************************* }
function TDataModuleDMI.GetTIMSAvailable: Boolean;
begin
  Result := TIMSAvailable;

end;

{ *********************************************************
  * GetTIMSOk
  * Description: Get the TIMSOK flag received from ATP
  ********************************************************* }
function TDataModuleDMI.GetTIMSOK: Boolean;
begin
  Result := TIMSOK;

end;

function TDataModuleDMI.GetLocoVsTrainDir: TLocomotiveConnected;
begin
  Result := LocomotiveConnected;
end;

procedure TDataModuleDMI.SetLocoVsTrainDir(Dir: TLocomotiveConnected);
begin
  LocomotiveConnected := Dir;

end;

{ *********************************************************
  * GetConfirmModeChange
  * Description: Get the Confirm Mode Change option last received from ATP
  ********************************************************* }
function TDataModuleDMI.GetConfirmModeChange(var Changed: Boolean): Byte;
begin
  Changed := ConfirmModeChanged;
  Result := ConfirmModeChange;

end;

{ *********************************************************
  * SetConfirmModeChange
  * Description: Set the Confirm Mode Change received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetConfirmModeChange(NewConfirmModeChange: Byte);
begin
  if ConfirmModeChange <> NewConfirmModeChange then
  begin
    ConfirmModeChange := NewConfirmModeChange;
    ConfirmModeChanged := true;
  end;

end;

{ *********************************************************
  * SetVehicleType
  * Description: Set the Vehicle Type received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetVehicleType(VehicleType: Byte;
  VehicleTypeName: AnsiString);
begin
  if (VehicleTypesStorage[VehicleType] <> VehicleTypeName) then
  begin
    VehicleTypesStorage[VehicleType] := VehicleTypeName;
    VehicleTypeChanged := true;
  end;
end;

{ *********************************************************
  * SetTypicalConfigType
  * Description: Set the TypicalConfig Type received from ATP
  ********************************************************* }
procedure TDataModuleDMI.SetTypicalConfig(TypicalConfigType: Byte;
  TypicalConfigName: AnsiString; VehicleTypeName: AnsiString;
  NumberofCars: Byte);
begin

  if ((TypicalConfigType > 0) and (TypicalConfigType <= 5)) then
  TypicalConfigStorage[TypicalConfigType].TypicalConfigName := TypicalConfigName;
  TypicalConfigStorage[TypicalConfigType].VehicleTypeName := VehicleTypeName;
  TypicalConfigStorage[TypicalConfigType].CarsinTypicalConfig := NumberofCars;
  TypicalConfigChanged := true;

end;

{ *********************************************************
  * GetVehicleType
  * Description: search vehicle type by name
  ********************************************************* }
function TDataModuleDMI.GetVehicleType(VehicleTypeName: AnsiString): Byte;
var
  VehicleType: Byte;
  Found: Boolean;
begin
  Found := False;
  For VehicleType := 1 To MAX_VEHICLE_TYPE_BLOCKS do
  begin
    if VehicleTypesStorage[VehicleType] = VehicleTypeName then
    begin
      Found := true;
      break;
    end;
  end;

  if (Found) then
    Result := VehicleType
  else
    Result := 0;
end;

{ *********************************************************
  * GetTypicalConfigType
  * Description: search TypicalConfigtype by name
  ********************************************************* }
function TDataModuleDMI.GetTypicalConfigType(TypicalConfigTypeName: AnsiString): Byte;
var
TypicalConfigType: Byte;
Found :Boolean;
begin
Found := False;
for TypicalConfigType := 1 to MAX_TYPICAL_CONFIG_BLOCKS do
  begin
    if TypicalConfigStorage[TypicalConfigType].TypicalConfigName = TypicalConfigTypeName then
    begin
      Found:= True;
      break;
    end;
  end;

  if(Found) then
  Result:= TypicalConfigType
  else
  Result := 0;
end;

{ *********************************************************
  * GetTypicalConfigCarsCount
  * Description: Get TypicalConfig VehicleType Count
  ********************************************************* }
function TDataModuleDMI.GetTypicalConfigCarsCount(TypicalConfigType
  : Byte): Byte;
var
  TypicalConfigCarsCount: Byte;
  Found: Boolean;

begin
  Found := False;
  begin
    TypicalConfigCarsCount := TypicalConfigStorage[TypicalConfigType]
      .CarsinTypicalConfig;

    if (TypicalConfigCarsCount > 0) then
      Found := true;

    if (Found) then
      Result := TypicalConfigCarsCount
    else
      Result := 0;

  end;
end;

{ *********************************************************
  * GetTypicalConfigVehicleTypeName
  * Description: Get TypicalConfig VehicleType Names
  ********************************************************* }
function TDataModuleDMI.GetTypicalConfigVehicleTypeName(TypicalConfigType: Byte)
  : AnsiString;
var
  TypicalConfigVehicleName: AnsiString;
  Found: Boolean;

begin
  Found := False;
  begin
    TypicalConfigVehicleName := TypicalConfigStorage[TypicalConfigType]
      .VehicleTypeName;
    if (TypicalConfigVehicleName <> '') then
      Found := true;

    if (Found) then
      Result := TypicalConfigVehicleName
    else
      Result := '';

  end;
end;

{ *********************************************************
  * GetTypicalConfigName
  * Description: Get TypicalConfig type by Name
  ********************************************************* }
function TDataModuleDMI.GetTypicalConfigName(TypicalConfigType: Byte)
  : AnsiString;
var
  TypicalConfigName: AnsiString;
  Found: Boolean;

begin
  Found := False;
  begin
    TypicalConfigName := TypicalConfigStorage[TypicalConfigType]
      .TypicalConfigName;

    if (TypicalConfigName <> '') then
      Found := true;

    if (Found) then
      Result := TypicalConfigName
    else
      Result := '';
  end;
end;

{ *********************************************************
  * VehicleTypeNameExists
  * Description: Check if the VehicleTypeName Exists
  ********************************************************* }
function TDataModuleDMI.VehicleTypeNameExists(TypicalConfigVehicleTypeName
  : AnsiString): Boolean;
var
  VehicleTypeStored: AnsiString;
  VehicleType: Byte;
  Found: Boolean;
begin
  Found := False;
  VehicleType := 1;
  while (Not Found) AND (VehicleType <= MAX_VEHICLE_TYPE_BLOCKS) do
  begin
    VehicleTypeStored := GetVehicleTypeName(VehicleType);
    if (TypicalConfigVehicleTypeName = VehicleTypeStored) then
      Found := true
    else
      Inc(VehicleType);
  end;
  Result := Found;
end;

{ *********************************************************
  * ClearTypicalConfigStorage
  * Description: Get vehicle type
  ********************************************************* }
procedure TDataModuleDMI.ClearTypicalConfigStorage();
var
  TypicalConfigType: Byte;
begin
  for TypicalConfigType := 1 to MAX_TYPICAL_CONFIG_BLOCKS do
  begin
    TypicalConfigStorage[TypicalConfigType].TypicalConfigName := AnsiString('');
    TypicalConfigStorage[TypicalConfigType].VehicleTypeName := AnsiString('');
    TypicalConfigStorage[TypicalConfigType].CarsinTypicalConfig := 0;
  end;
end;

{ *********************************************************
  * GetVehicleTypeName
  * Description: Get vehicle type
  ********************************************************* }
function TDataModuleDMI.GetVehicleTypeName(VehicleType: Byte): AnsiString;
begin
  if (VehicleType > 0) AND (VehicleType <= 20) then
    Result := VehicleTypesStorage[VehicleType]
  else
    Result := '';
end;

{ *********************************************************
  * GetTrainVsTrackDirWantedReceived
  * Description: Get if TrainVsTrackDirWanted message recieved
  ********************************************************* }
function TDataModuleDMI.GetTrainVsTrackDirWantedReceived
  (var Changed: Boolean): Boolean;
begin
  Changed := TrainVsTrackDirWantedChanged;
  Result := TrainVsTrackDirWantedReceived;
end;

procedure TDataModuleDMI.SetTrainVsTrackDirWantedReceived(Flag: Boolean);
begin
  TrainVsTrackDirWantedChanged := true;
  TrainVsTrackDirWantedReceived := Flag;
end;

{ *********************************************************
  * SetNbOfVehiclesConnected
  * Description: Set the Number of vehicle connected with loco
  ********************************************************* }
procedure TDataModuleDMI.SetNbOfVehiclesConnected(Count: Integer);
begin
  NumberOfVehiclesConnected := Count;
end;

{ *********************************************************
  * GetNbOfVehiclesConnected
  * Description: get Total number of vehicle connected with loco
  ********************************************************* }
function TDataModuleDMI.GetNbOfVehiclesConnected(): Integer;
begin
  Result := NumberOfVehiclesConnected;
end;

{ *********************************************************
  * GetAdditionalAllowedToInfo
  * Description:Get byte with additional allowed info (data 12)
  ********************************************************* }
function TDataModuleDMI.GetAdditionalAllowedToInfo(var Changed: Boolean): Byte;
begin
  Changed := AdditionalAllowedToInfoChanged;
  Result := AdditionalAllowedToInfo;
end;

{ *********************************************************
  * SetAdditionalAllowedToInfo
  * Description:Set byte with additional allowed info (data 12)
  ********************************************************* }
procedure TDataModuleDMI.SetAdditionalAllowedToInfo
  (NewAdditionalAllowedToInfo: Byte);
begin
  if AdditionalAllowedToInfo <> NewAdditionalAllowedToInfo then
  begin
    AdditionalAllowedToInfo := NewAdditionalAllowedToInfo;
    AdditionalAllowedToInfoChanged := true;
  end;

end;

{ *********************************************************
  * GetVehicleDataReceived
  * Description: Indicate if vehicle data has recieved from ATP
  ********************************************************* }
function TDataModuleDMI.GetVehicleDataReceived(): Boolean;
begin
  Result := VehicleDataReceived;
end;

{ *********************************************************
  * GetAdditionalConfirmationInfo
  * Description:Get byte with additional confirmation info (data 21)
  ********************************************************* }
function TDataModuleDMI.GetAdditionalConfirmationInfo
  (var Changed: Boolean): Byte;
begin
  Changed := AdditionalConfirmationInfoChanged;
  Result := AdditionalConfirmationInfo;
end;

{ *********************************************************
  * SetAdditionalAllowedToInfo
  * Description:Set byte with additional confirmation info (data 21)
  ********************************************************* }
procedure TDataModuleDMI.SetAdditionalConfirmationInfo
  (NewAditionalConfirmationInfo: Byte);
begin
  if AdditionalConfirmationInfo <> NewAditionalConfirmationInfo then
  begin
    AdditionalConfirmationInfo := NewAditionalConfirmationInfo;
    AdditionalConfirmationInfoChanged := true;
  end;

end;


{ *********************************************************
  * SetAdditionalConfirmationInfo2
  * Description:Set byte with additional confirmation odometer info (data 23)
  ********************************************************* }
procedure TDataModuleDMI.SetAdditionalConfirmationInfo2
  (var NewAdditionalConfirmationInfo2: Byte);
begin
  if AdditionalConfirmationInfo2 <>
    NewAdditionalConfirmationInfo2 then
  begin
    AdditionalConfirmationInfo2 := NewAdditionalConfirmationInfo2;
    AdditionalConfirmationInfo2Changed := true;
  end;

end;

{ *********************************************************
  * SetPlatformStatusInfo
  * Description:Set byte with Platform Status info (data 24)
  ********************************************************* }
procedure TDataModuleDMI.SetPlatformStatusInfo(var NewPlatformStatusInfo: Byte);
begin
  if PlatformStatusInfo <> NewPlatformStatusInfo then
  begin
    PlatformStatusInfo := NewPlatformStatusInfo;
    PlatformStatusInfoChanged := true;
  end;

end;

{ *********************************************************
  * SetAdaptationTrainStatus
  * Description:Set byte with adatptation Train Status
  ********************************************************* }
procedure TDataModuleDMI.SetAdaptationTrainStatus
  (NewAdaptationTrainStatus: Byte);
begin
  if AdaptationTrainStatus <> NewAdaptationTrainStatus then
  begin
    AdaptationTrainStatus := NewAdaptationTrainStatus;
    AdaptationTrainStatusChanged := true;
  end;

end;


{ *********************************************************
  * GetAdditionalConfirmationInfo2
  * Description:Get byte with additional confirmtion Info 2 (Data 23)
  ********************************************************* }
function TDataModuleDMI.GetAdditionalConfirmationInfo2
  (var Changed: Boolean): Byte;
begin
  Changed := AdditionalConfirmationInfo2Changed;
  Result := AdditionalConfirmationInfo2;
end;


{ *********************************************************
  * GetPlatformStatusInfo
  * Description:Get byte with Platform Status bits(Data 24)
  ********************************************************* }
function TDataModuleDMI.GetPlatformStatusInfo(var Changed: Boolean): Byte;
begin
  Changed := PlatformStatusInfoChanged;
  Result := PlatformStatusInfo;
end;


{ *********************************************************
  * GetAdaptationTrainStatus
  * Description:Get byte with adaptation Train Status (data 22)
  ********************************************************* }
function TDataModuleDMI.GetAdaptationTrainStatus(var Changed: Boolean): Byte;
begin
  Changed := AdaptationTrainStatusChanged;
  Result := AdaptationTrainStatus;
end;

{ *********************************************************
  * SetLocationName
  * Description:Set Location Name
  ********************************************************* }
procedure TDataModuleDMI.SetLocationName(NewLocationName: String);
begin
  if NewLocationName <> LocationName then
  begin
    LocationName := NewLocationName;
    LocationNameChanged := true;
  end;
end;

{ *********************************************************
  * GetLocationName
  * Description: Get Location Name
  ********************************************************* }
function TDataModuleDMI.GetLocationName(var Changed: Boolean): String;
begin
  Changed := LocationNameChanged;
  Result := LocationName;
end;

{ *********************************************************
  * SetLocationType
  * Description:Set Location Type
  ********************************************************* }
procedure TDataModuleDMI.SetLocationType(var NewLocType: Integer);
begin
  if NewLocType <> LocationType then
  begin
    LocationType := NewLocType;
    LocationTypeChanged := true;
  end;
end;

{ *********************************************************
  * GetLocationType
  * Description: Get Location Name
  ********************************************************* }
function TDataModuleDMI.GetLocationType(var Changed: Boolean): Integer;
begin
  Changed := LocationTypeChanged;
  Result := LocationType;
end;

{ *********************************************************
  * SetTrainWeight
  * Description:Set Train Weight
  ********************************************************* }
procedure TDataModuleDMI.SetTrainWeight(var NewTrainWeight: LongWord);
begin
  if NewTrainWeight <> TrainWeight then
  begin
    TrainWeight := NewTrainWeight;
    TrainWeightChanged := true;
  end;
end;

{ *********************************************************
  * GetTrainWeight
  * Description: Get Train Weight
  ********************************************************* }
function TDataModuleDMI.GetTrainWeight(var Changed: Boolean): LongWord;
begin
  Changed := TrainWeightChanged;
  Result := TrainWeight;
end;

{ *********************************************************
  * SetETA
  * Description:Set ETA
  ********************************************************* }
procedure TDataModuleDMI.SetETA(var NewETA: UInt32);
begin
  if NewETA <> ETA then
  begin
    ETA := NewETA;
    ETAChanged := true;
  end;
end;

{ *********************************************************
  * GetETA
  * Description: Get ETA
  ********************************************************* }
function TDataModuleDMI.GetETA(var Changed: Boolean): UInt32;
begin
  Changed := ETAChanged;
  Result := ETA;
end;

{ *********************************************************
  *     ClearChanged
  * Description: Clear all flags indicating changed data
  ********************************************************* }
procedure TDataModuleDMI.ClearChanged;
begin
  TrainIdChanged := False;
  ATPModeChanged := False;
  ATPStateChanged := False;
  ATPSubStateChanged := False;
  ConfigSubStateChanged := False;
  ATPVerificationStateChanged := False;
  ATOModeChanged := False;
  ATOSwitchChanged := False;
  LocoStatusChanged := False;
  InterfaceFlagsChanged := False;
  AdditionalStatus1Changed := False;
  AdditionalStatus2Changed := False;
  AllowedButtonsChanged := False;
  DriverInfoChanged := False;
  BrakeTestStatusChanged := False;
  RemainingTimeToBrakeTestChanged := False;
  RemainingTimeToBTMTestChanged := False;
  SpeedAndDistanceChanged := False;
  ReRegOptionsChanged := False;
  ConfigOptionsChanged := False;
  TrainLengthChanged := False;
  ConfirmModeChanged := False;
  AdditionalAllowedToInfoChanged := False;
  AdditionalConfirmationInfoChanged := False;
  PlatformStatusInfoChanged := False;
  AdditionalConfirmationInfo2Changed := False;
  LocationNameChanged := False;
  LocationTypeChanged := False;
  RadioAreaChanged := False;
  TrainWeightChanged := False;
  ETAChanged := False;
  TrainVsTrackDirWantedChanged := False;
end;

end.
