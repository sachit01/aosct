(**************************************************************************
           © COPYRIGHT Bombardier Transportation (Signal)  AB, SWEDEN 1998.
           ================================================================

    The copyright to the computer program herein is the property of
    Bombardier Transportation (Signal) AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the written permission
    from Bombardier Transportation (Signal) AB and in accordance
    with the terms and conditions stipulated in the agreement/contract
    under which the program has been supplied.


    NAME :  Mainarea.pas

    PROJECT :  LKAB

    Ver    Author           Date      Reason
    ---    ------           ------    ------
           Bo H             110210    MMICL entry names now without '_'

           Bo H             110301    Delphi XE
                                      Uses Windows (because of Beep)
                                      ATPNotification(..);
                                      str: AnsiString;  // Delphi XE, string -> AnsiString
                                      TExchange.Train_Config_Data
                                      NewTrainName : AnsiString;

           Bo Hermansson    110310    Delphi XE:StrLen(pchar()) -> Length()
                                      Variables with name Length renamed in order
                                      to avoid conflict with SysUtils::Length


          Bo Hermansson     110329    Redesigned for LK

          Bo H              111011    CarNameData replaced by CarNameList etc
          Bo H              120111    Message MMITime removed
          Bo H              120201    Display 'Undef text' when TextType out of range
          Bo H              120207    Unit renamed from SerUnit to UnitExchange
          Bo H              120314    Log level debug
          Bo H              121029    OdoMeterInvalid
          Bo H              131029    Use TranslateString() to translate strings
                                      arriving from AOS as TextMessage or ATPNotification
          Bo H              150409    Allow a translated
                                      ATPNotification-string to grow longer than
                                      the original MAX SIZE
                                      See NCR arn_006#3527
          Bo H              20170823  Allow up to 350 cars and 20 char carnames

    DESCRIPTION :  Mainarea that incorporates area a,b,c,d,e,f

    INTERFACE :
*********************************************************)

unit UnitExchange;

interface

{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
Uses
    Windows, SysUtils, Classes, MMITypes,MMIAreaB, MMIAreaC;

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
Type
TExchange = Class
  private
    { Private declarations }
    FportOpen: boolean ;

    Procedure ATP_Modes_states(Data: PATPModes_StatesMT) ;
    Procedure Driver_info(Data:PDriverInfoMT);
    Procedure Speed_distance(Data:PSpeed_DistanceMT;DataLength :Word);
    Procedure Train_Config_Data(Data:PTrnConfigDataMT;DataLength :Word);
    Procedure Gradient_Data(Data:TGradientDataMT;NoOFGradTarg:Integer);
    Procedure Gradient_DataList(Data: PGradientDataList);
    Procedure Manual_Config_Selected;
    Procedure ReReg_Selected(Data:PReRegSelectedMT);
    Procedure Ceiling_Speed(Item:TCeilingSpeedMT);
    Procedure Ceiling_SpeedList(Data:PCeilingSpeedList);
    Procedure Time(Data:PTimeMT);
    Procedure TrainVSTrackDirWanted;
    Procedure LocoVSTrainDir(Data:PLocoVSTrnDirMT);
    Procedure Erase_PlanningArea ;
    Procedure Text_message(Data:PTextMessageMT);
    Procedure StartUpHistory(Data:PStartUpHistoryMT);
    Procedure PredefTextMsg(data: PPredefinedTextMsgMT);
    Procedure TrainName_Data (Data:PTrainNameMT);
    Procedure RadioArea_Data (Data:PRadioAreaMT);
    Procedure ATPNotification(Data: PAtpNotificationMT);
    Procedure Version(Data: PVersionMT);
    Procedure AreaRequest(Data: PAreaRequestMT);
    Function  MessageTypeString(MT : TMessageType):String ;
    Procedure VehicleTypes(Data: PAtpVehicleTypes);
    Procedure TypicalConfig(Data :PTypicalConfigTypes);
    Procedure VehicleData(Data: PAtpVehicleData);
    Procedure LocationData(Data: PAtpLocationData);
    Procedure ETARequest(Data: PETAMT);
    Procedure TrainWeight(Data: PTTrainWeightMT);
    Procedure getATP_ToMMI_Data( Var MessageType: Byte;
                                 var DataLength :Word ;
                                 Data : Pointer) ;

    Function putMMI_ToATP_Data( MsgDataLength : Word ;
                             MsgData:Pointer) : Integer ;

    Procedure HandleRemoteDMIRegArea(AreaId : String; Var ResponseString : String);
    Procedure HandleRemoteDMILogin(UserName : String;Pwd : String; Var ResponseString : String);
    Procedure HandleRemoteDMISelect(Operation : String; Var ResponseString : String);
    Procedure HandleRemoteGetATPInfo(Var ResponseString : String);
    Procedure HandleRemoteMinimize(Var ResponseString : String);
    Procedure HandleRemoteRestore(Var ResponseString : String);
    Procedure HandleRemoteDMIClose(Var ResponseString : String);
    Procedure HandleRemoteCommand(Command : String; ArgList : TStringList; Var ResponseString:String);

  Protected
    { Protected declarations }
  public
    { Public declarations }
    FComOK : Longbool ;
    StartUpAlreadySent: boolean;
    Function Init( Port : integer ; Debug : Integer) : integer;
    Function Portopen : Boolean ;
    Procedure MMICLExit ;
    Procedure SendMMIStatusToATP ;
    Procedure SendMMIButton ( Buttons : TButtonStatus ) ;
    procedure SendTrainLoaded( TrainLoaded : Byte);
    Procedure SendConfirmation (Answer : Boolean) ;
    Procedure SendDriverIDandPassword ;
    Procedure SendLocoVSTrainDir (Dir : TLocomotiveConnected);
    Procedure SendTrainVSTrackDir (Dir : TTrackDirection) ;

    Procedure SendStartUp;

    Procedure SendRegistrationArea(AreaId : Byte);
    Procedure SendNewTrainName(TrainName : String);
    procedure SendVehicleData(Msg: PVehicleDataMsg);

    Function PollPort : Boolean ;
    Procedure RemoteCommand(CommandString : String; Var ResponseString : String);
  End ;

{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}
Var
  Exchange :TExchange ;

implementation

{****************************************************************************
* USES                                                                      *
****************************************************************************}
uses
  UnitMainArea, MMIAreaD, Graphics, Dialogs, Forms,
  MMIStd, UnitManualConfiguration, UnitAtpMessages,
  UnitTrainVsTrack, UnitLogin, UnitStartupHistory, UnitViewLog,
  UnitConfirmATPMode, UnitSelectArea,
  UnitReReg, UnitMMIFrame, UnitDMIDataModule, UnitFullScreenMsg, UnitAutomaticConfiguration, UnitConfirmLoadedStatus;

{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}

{****************************************************************************
* TYPE DECLARATIONS                                                         *
****************************************************************************}

{****************************************************************************
* VAR DECLARATIONS                                                          *
****************************************************************************}

{****************************************************************************
* FORWARD DECLARATIONS                                                      *
****************************************************************************}

{****************************************************************************
* FUNCTIONS AND PROCEDURES                                                  *
****************************************************************************}
  procedure MMICL_getNextMsgDataFromATP(MsgDataLength : Pointer;
                                     MsgData : Pchar);
                                     cdecl;
                                     external 'mmicl.dll'
                                     name 'MMICL_getNextMsgDataFromATP';

  Function MMICL_putMMI_ToATP_Data( MsgDataLength :Word ;
                             MsgData:Pointer) : integer ;
                             cdecl;
                             external 'mmicl.dll'
                             name 'MMICL_putMessageDataToATP';

  procedure MMICL_getErrors ( VitalMsgErrCount : Pointer;
                              recErrCount : Pointer);
                              cdecl;
                              external 'mmicl.dll'
                              name 'MMICL_getErrors';

  function MMICL_init( Port : integer ;
                       Debug : integer) : integer;
                       cdecl; external 'mmicl.dll' name 'MMICL_init';

  procedure MMICL_exit; cdecl; external 'mmicl.dll' name 'MMICL_exit';


{****************************************************************************
* FUNCTIONS                                                                 *
****************************************************************************}

{*********************************************************
* Function:    TExchange.Init
* Description: Initializes the selected comport by way
               the of the DLL MMICL.
*********************************************************}
function TExchange.Init(Port : integer ; Debug : Integer ) : Integer ;
begin
   Result:= MMICL_init( Port, Debug) ;
   if result = 0 then
   begin
     FportOpen := True ;
     FComOK := LongBool(15) ;

   end
   else
   begin
     FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,SER_INIT_ERR);
     if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error Init, Port:'+IntToStr(Port)),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
   end;

   startupAlreadySent := false;

End;

{*********************************************************
* Function:    TExchange.Portopen
* Description: Returns the status of the Fportopen
               variable.
*********************************************************}
Function TExchange.Portopen : Boolean ;
Begin
  Result:= FportOpen ;
End;

{*********************************************************
* Function:    TExchange.MMICL_exit
* Description: Shutsdown the MMICL thread and frees
               the Comport that was assigned to MMICL
*********************************************************}
procedure TExchange.MMICLexit ;
begin

  if FPortOpen then
    MMICL_exit;
  FPortOpen := False ;

end;

{*********************************************************
* Function:    TExchange.PollPort
* Description: Collects a message from MMICL and passes it
               to ser getATP_ToMMI_Data
               Function set to true, if message collected
*********************************************************}
Function TExchange.PollPort : Boolean ;
Type
  TInData = Packed Record
                  MT : Byte;
                  Data : Array [0..(MMI_DATA_MAX_SIZE-1-1)] of Byte; {Index starts at 0, Excluding MT }
            End;
Var
  InData : TInData;     { As received from ATP }
  MessageType : Byte;   { 0..127, MSBit masked away }

  DataLength : Word;    { Length of InData }
  MessageLength : Word; { Length of message-type dependent part of data }

  VEMessageCount,EMessageCount : Integer ;

Begin

  Result := false;       { Return value false when no message received }
  Try

    MMICL_getErrors(addr(VEMessageCount),addr(EMessageCount)) ;

    InData.MT:= 0;       { It is necessary to clear MessageType inb order to detect
                           if any message has arrived }

    MMICL_getNextMsgDataFromATP(@DataLength, @InData) ;


    While InData.MT  <> 0 do begin

      FormMMIFrame.CheckVersion();

      MessageType:= InData.MT and $7F ; { not interrested in if vital MT }
      MessageLength := DataLength - 1;  { MessageType is first byte of data }

      getATP_ToMMI_Data(MessageType, MessageLength , @InData.Data);

      InData.MT:= 0;
      MMICL_getNextMsgDataFromATP(@DataLength, @InData) ;

      Result := true;

    End;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,SER_POLL_ERR);
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in PollPort:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    TExchange.MessageTypeString
* Description: Translates MessageType to a readable string
*********************************************************}
Function TExchange.MessageTypeString(MT : TMessageType):String ;
Begin

  case MT  of
    mtATPModes_States :
        Result := 'ATP-mode and states';
    mtDriverInfo :
        Result := 'Driver info';
    mtSpeed_Distance :
        Result := 'Speed & distance';
    mtTrnConfigData :
        Result := 'TrainConfigData';
    mtManualConfigSelected :
        Result := 'ManualConfigSelected';
    mtVehicleData :
        Result := 'VehicleData';
    mtTrnVSTrackDirWanted :
        Result := 'TrainVsTrackDirWanted';
    mtMMIStatus :
        Result := 'MMI_Status';
    mtBoolConfirmation :
        Result := 'Confirmation';
    mtDriverID_Password :
        Result := 'DriverIDandPassword';
    mtLocVSTrnDir :
        Result := 'LocoVsTrainDir';
    mtMMIToATP_Data :
        Result := 'MMI_ToATP_Data';
    mtTrnVSTrackDir :
        Result := 'TrainVsTrackDir';
    mtErasePlanningArea :
        Result := 'ErasePlanningArea';
    mtTextMessage :
        Result := 'TextMessage';
    mtCeilingSpeedList :
        Result := 'CeilingSpeedList';
    mtGradientDataList :
        Result := 'GradientDataList';
    mtMMIStartUp :
        Result := 'MMIStartup';
    mtReRegSelected :
        Result := 'ReRegSelected';
    mtTrainName :
        Result := 'TrainName';
    mtPredefinedTextMsg :
        Result := 'PredefinedTextMessage';
    mtAtpNotification :
        Result := 'AtpNotification';
    mtStartUpHistory :
        Result := 'StartUpHistory';
    mtVersion :
        Result := 'Version';
    mtRegistrationArea :
        Result := 'RegistrationArea';
    mtAreaRequest :
        Result := 'AreaRequest';
    mtTime :
        Result := 'Time';
    mtTypicalConfig:
        Result := 'TypicalConfig';
    mtVehicleTypes :
        Result := 'VehicleTypes';
    mtRadioArea  :
        Result := 'RadioArea';
    mtLocationData :
        Result := 'LocationData';
    mtETARequest:
        Result := 'ETA';
    mtTrainWeight:
        Result := 'Train Weight';
    mTTrainLoaded :
        Result := 'Train Loaded';
  else
        Result := 'Unknown';
  end;

End;

{*********************************************************
* Function:    TExchange.putATP_ToMMI_Data
* Description: Pass the outgoing message to the MMICL DLL
*********************************************************}
Function TExchange.putMMI_ToATP_Data ( MsgDataLength : Word ;
                                    MsgData:Pointer) : Integer ;
var
  pByte : ^Byte;
  MessageTypeByte : Byte;
begin

        { MessageType is the first byte in the message }
        { Most significant bit may be set if acknowledged message }
  pByte := MsgData;
  MessageTypeByte := pByte^ and $7F;

  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('MT:' + IntToStr(MessageTypeByte) + '=' + MessageTypeString(TMessageType(MessageTypeByte)) +' sent'));

  Result := MMICL_putMMI_ToATP_Data ( MsgDataLength, MsgData);
end;


{*********************************************************
* Function:    TExchange.getATP_ToMMI_Data
* Description: Calls the aproriate routin to handle the
               received message
*********************************************************}
Procedure TExchange.getATP_ToMMI_Data (Var MessageType: Byte;var DataLength :Word ; Data : Pointer) ;
Begin
  Try
    //**********************************************
    // Normal Messages
    //**********************************************
    Case TMessageType(MessageType) of

    mtATPModes_States:       ATP_Modes_states(PATPModes_StatesMT(Data)) ;
    mtErasePlanningArea:     Erase_PlanningArea ;
    mtDriverInfo:            Driver_info(PDriverInfoMT(Data));
    mtSpeed_Distance:        Speed_distance(PSpeed_DistanceMT(Data), DataLength);
    mtTrnConfigData:         Train_Config_Data(PTrnConfigDataMT(Data), DataLength);
    mtManualConfigSelected:  Manual_Config_Selected ;
    mtTime:                  Time(PTimeMT(Data));
    mtVehicleData:           VehicleData(PAtpVehicleData(Data));
    mtTrnVSTrackDirWanted:   TrainVSTrackDirWanted ;
    mtLocVSTrnDir:           LocoVSTrainDir(PLocoVSTrnDirMT(Data));
    mtTextMessage:           Text_message(PTextMessageMT(Data));
    mtStartUpHistory:        StartUpHistory(PStartUpHistoryMT(Data));
    mtPredefinedTextMsg:     PredefTextMsg(PPredefinedTextMsgMT(Data));
    mtCeilingSpeedList:      Ceiling_SpeedList(PCeilingSpeedList(Data));
    mtGradientDataList:      Gradient_DataList(PGradientDataList(Data));
    mtReRegSelected:         ReReg_Selected(PReRegSelectedMT(Data));
    mtTrainName:             TrainName_Data (PTrainNameMT(Data));
    mtRadioArea:             RadioArea_Data (PRadioAreaMT(Data));
    mtAtpNotification:       ATPNotification(PAtpNotificationMT(Data));
    mtVersion:               Version(PVersionMT(Data));
    mtAreaRequest:           AreaRequest(PAreaRequestMT(Data));
    mtVehicleTypes:          VehicleTypes(PAtpVehicleTypes(Data));
    mtTypicalConfig:         TypicalConfig(PTypicalConfigTypes(Data));
    mtLocationData:          LocationData(PAtpLocationData(Data));
    mtETARequest:            ETARequest(PETAMT(Data));
    mtTrainWeight:           TrainWeight(PTTrainWeightMT(Data));

    else If FormMMIFrame.TestMode then
      MessageBox(FormMainArea.Handle,PWideChar('Unknown message type in getATP_ToMMI_Data'),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    End ;

    FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('Len:' + IntToStr(DataLength) + ' MT:' + IntToStr(MessageType) + '=' + MessageTypeString(TMessageType(MessageType)) +' received'));

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,SER_DETECT_ERR);
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in getATP_ToMMI_Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

  { *********************************************************
  * Function:    TExchange.Ceiling_SpeedList
  * Description: Takes care of the CeilingSpeedList messages.
  ********************************************************* }
procedure TExchange.Ceiling_SpeedList(Data: PCeilingSpeedList);
var
  i                   : Integer;
  NrOfSpeedDataBlocks : Integer;
begin
  try

    FormMainArea.MMIAreaD.EraseAllLists;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR, MS_SER, Ord(mtCeilingSpeedList));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Ceiling_SpeedList, EraseAllLists:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;

  NrOfSpeedDataBlocks := Data.NrOfSpeedDataBlocks;
  if (NrOfSpeedDataBlocks > CEILING_SPEED_MAX) then
  begin

    if FormMMIFrame.TestMode then
    begin
      MessageBox(FormMainArea.Handle,PWideChar('Error in Ceiling_SpeedList, illegal no of datablocks:' + IntToStr(NrOfSpeedDataBlocks)) ,PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
        {Illegal no of data-blcoks}
    NrOfSpeedDataBlocks := 0;

  end;

  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('NrOfSpeedDataBlocks:') + AnsiString(IntToStr(Data.NrOfSpeedDataBlocks)));

  try

    for i := 1 to NrOfSpeedDataBlocks do
    begin
      Ceiling_Speed(Data.Speed[i]);
    end;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR, MS_SER, Ord(mtCeilingSpeedList));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Ceiling_SpeedList:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;

  try

    FormMainArea.MMIAreaD.Refresh;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR, MS_SER, Ord(mtCeilingSpeedList));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Ceiling_SpeedList, Refresh:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    TExchange.Gradient_DataList
* Description: Takes care of the GradientDataList messages.
*********************************************************}

procedure TExchange.Gradient_DataList(Data: PGradientDataList);
var
  i: integer;

begin

  try

    for i := 1 to Data.NrOfGradientDataBlocks do
    begin
      Gradient_Data(Data.Gradient[i],Data.NrOfGradientDataBlocks);
    end;

    FormMainArea.MMIAreaD.Refresh;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR, MS_SER, Ord(mtGradientDataList));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Gradient_DataList:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;

end;

{ **
{*********************************************************
* Function:    TExchange.ATP_Modes_states
* Description: Sets the mainarea mode and state variable
               and updates the MMI
*********************************************************}
Procedure TExchange.ATP_Modes_states(Data:PATPModes_StatesMT);
begin
  try
      FormMMIFrame.LogEventStr(LogLevelDebug , 'COMM', 'ATP_Modes_states,Mode:'+
                        IntToStr(Ord(Data^.ATPMode)) + ', State:' +
                        IntToStr(Ord(Data^.ATPState)) + ', CnfModeSubState:' +
                        IntToStr(Ord(Data^.ATPSubState)) + ', VerState:' +
                        IntToStr(Ord(Data^.ATPVerification)));
      FormMMIFrame.LogEventStr(LogLevelDebug , 'COMM', 'ATP_Modes_states,Data 11-14:'+
                        IntToStr(Data^.AdditionalStatus1) + ',' +
                        IntToStr(Data^.AdditionalStatus2) + ',' +
                        IntToStr(Data^.ConfirmModeChange) + ',' +
                        IntToStr(Data^.AllowedButtons));

      if bytebool(Byte(Data^.InterfaceFlags) and CONFIGURATION_REJECTED) then
      begin // Config rejected
        FormManualConfiguration.RetryInfo.Show;
      end;

      DataModuleDMI.SetATPMode(Data^.ATPMode);
      DataModuleDMI.SetATPState(Data^.ATPState);
      DataModuleDMI.SetATPSubState(Data^.ATPSubState);
      DataModuleDMI.SetATPVerificationState(Data^.ATPVerification);
      DataModuleDMI.SetATOMode(Data^.ATOMode);
      DataModuleDMI.SetATOSwitch(Data^.ATOSwitch);
      DataModuleDMI.SetAdditionalStatus1(Data^.AdditionalStatus1);
      DataModuleDMI.SetAdditionalStatus2(Data^.AdditionalStatus2);
      DataModuleDMI.SetLocoStatus(SwapLongWord(Data^.LocoStatus));
      DataModuleDMI.SetInterfaceFlags(Data^.InterfaceFlags);
      DataModuleDMI.SetAllowedButtons(Data^.AllowedButtons);
      DataModuleDMI.SetBrakeTestStatus(Data^.BrakeTestStatus);
      DataModuleDMI.SetRemainingTimeToBrakeTest(Swap(Data^.RemainingTimeToBrakeTest));
      DataModuleDMI.SetRemainingTimeToBTMTest(Swap(Data^.RemainingTimeToBTMTest));
      DataModuleDMI.SetConfirmModeChange(Data^.ConfirmModeChange);
      DataModuleDMI.SetAdditionalAllowedToInfo(Data^.AdditionalAllowedToInfo);
      DataModuleDMI.SetAdditionalConfirmationInfo(Data^.AdditionalConfirmationInfo1);
      DataModuleDMI.SetAdaptationTrainStatus(Data^.AdaptationTrainStatus);
      DataModuleDMI.SetAdditionalConfirmationInfo2(Data^.AdditionalConfirmationInfo2);
      DataModuleDMI.SetPlatformStatusInfo(Data^.PlatformStatusInfo);
      FormMMIFrame.SetMMI_ATPModeAndState;
      FormMMIFrame.UpdateDMI;

  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR, MS_SER, Ord(mtATPModes_States));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in ATP_Modes_states:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{ **
{*********************************************************
* Function:    TExchange.Driver_info
* Description: Handles the driver info message
*********************************************************}
procedure TExchange.Driver_info(Data:PDriverInfoMT);
var
  DriverInfo : TDriverInfoMT;
begin

  DriverInfo.PermittedDrivingDirection := Data.PermittedDrivingDirection;
  DriverInfo.Pspeed := Data.Pspeed;
  DriverInfo.Tspeed := Data.Tspeed;
  DriverInfo.TimeToIntervention := Data.TimeToIntervention;
  DriverInfo.RemainingDistanceToTargetPoint := Swap(Data.RemainingDistanceToTargetPoint);
  DriverInfo.RemainingDistanceToBCA := Swap(Data.RemainingDistanceToBCA);
  DriverInfo.PredictedDistanceToStandStillLocation := Swap(Data.PredictedDistanceToStandStillLocation);
  DriverInfo.StatusD11 := Data.StatusD11;
  DriverInfo.StatusD12 := Data.StatusD12;
  DriverInfo.ActualDrivingDirection := Data.ActualDrivingDirection;
  DriverInfo.MAMargin := Swap(Data.MAMargin);
  DriverInfo.Brakeability := Swap(Data.Brakeability);
  DriverInfo.BrakeDelayEB := Swap(Data.BrakeDelayEB);
  DriverInfo.BrakeDelaySB := Swap(Data.BrakeDelaySB);

  DataModuleDMI.SetDriverInfo(DriverInfo);

  FormMMIFrame.UpdateDMI;

End;

{*********************************************************
* Function:    TExchange.Speed_distance
* Description: Handles the speed and distance message
*********************************************************}
procedure TExchange.Speed_distance(Data:PSpeed_DistanceMT;DataLength :Word);
var
  SpeedAndDistance : TSpeed_DistanceMT;
  DistanceToPrimaryTarget: LongInt;
  PrimaryTargetOdoPos : LongInt;
begin

  SpeedAndDistance.CurrentSpeed := Swap(Data^.CurrentSpeed);
  SpeedAndDistance.CurrentTrackGradient := Swap(Data^.CurrentTrackGradient);
  SpeedAndDistance.leadingTrackSection := Swap(Data^.leadingTrackSection);
  SpeedAndDistance.leadingPositionCm := SwapLongWord(Data^.leadingPositionCm);
  SpeedAndDistance.trailingTrackSection := Swap(Data^.trailingTrackSection);
  SpeedAndDistance.trailingPositionCm := SwapLongWord(Data^.trailingPositionCm);
  SpeedAndDistance.CurrentOdometer := SwapLongInt(Data^.CurrentOdometer);
  SpeedAndDistance.CurrentEffectiveGradient := Swap(Data^.CurrentEffectiveGradient);
  DataModuleDMI.SetSpeedAndDistance(SpeedAndDistance);

           { Calculate and store distance to primary target }
  PrimaryTargetOdoPos := DataModuleDMI.GetPrimaryTargetOdoPos;
  if (PrimaryTargetOdoPos <> 0) then
  begin

    if PrimaryTargetOdoPos > SpeedAndDistance.CurrentOdometer then
      DistanceToPrimaryTarget := PrimaryTargetOdoPos - SpeedAndDistance.CurrentOdometer
    else
      DistanceToPrimaryTarget := SpeedAndDistance.CurrentOdometer
              - PrimaryTargetOdoPos;
    DataModuleDMI.SetDistanceToPrimaryTarget(DistanceToPrimaryTarget);

  end;

  FormMMIFrame.UpdateDMI;

end;

{*********************************************************
* Function:    TExchange.Train_Config_Data
* Description: Handles the Train Config message
*********************************************************}
Procedure TExchange.Train_Config_Data(Data:PTrnConfigDataMT;DataLength :Word);
Var
  I: integer;
  NewTrainName : AnsiString;  // Delphi XE
Begin
  Try

              // Use AnsiString as a buffer to create the new Train name
    NewTrainName:='';
    for i := 0 to (TRAIN_NAME_MAX_CHARS - 1) do
    begin
      if Data^.TrainName[i] = #0 then break;
        // TrainID.Caption := TrainID.Caption + Data^.TrainName[I];
      NewTrainName := NewTrainName + Data^.TrainName[I];
    end;

            // Convert AnsiString to UnicodeString
    DataModuleDMI.SetTrainId(String(NewTrainName));
    DataModuleDMI.SetTrainLength(Swap(Data^.TrainLength));
    DataModuleDMI.SetConfigOptions(Data^.Options);
    FormMMIFrame.UpdateDMI();

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTrnConfigData));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Train_Config_Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;

End;

{*********************************************************
* Function:    TExchange.Gradient_Data
* Description: Handles the Gradient data message
*********************************************************}
Procedure TExchange.Gradient_Data(Data:TGradientDataMT;NoOFGradTarg:Integer);
Begin
  Try
    with FormMainArea do Begin

        FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('EXC'), AnsiString('Gradient(' +
              IntToStr(SwapLongInt(Data.Odometerposition)) + ',' +
              IntToStr(Data.NewGradient) + ')'));

        MMIareaD.AddGradientToList( Integer(Data.NewGradient),
                                 SwapLongInt(Data.Odometerposition),
                                 LONGINTMAX*MMIAreaD.direction,
                                 MMIareaD.CurrentOdometerPosition,
                                 NoOFGradTarg)
    End
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtGradientDataList));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Gradient_Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

 {**************************************************
  * Function:    TExchange.Ceiling_Speed
  * Description: Handles a single Ceiling speed item
  ********************************************************* }
Procedure TExchange.Ceiling_Speed(Item: TCeilingSpeedMT);
var
  DummyChanged: Boolean;
  SpeedAndDistance: TSpeed_DistanceMT;
  DistanceToPrimaryTarget: LongInt;
  OdoPos: LongInt;
begin

  with FormMainArea do
  begin

    FormMMIFrame.LogEventStr(LogLevelDetail, AnsiString('EXC'),
      AnsiString('Ceiling_Speed(' + IntToStr(SwapLongInt(Item.Odometerposition))
      + ',' + IntToStr(Item.NewSpeed) + ',' + IntToStr(Ord(Item.Reason)
      ) + ')'));

    if Item.Reason = 254 then
    begin
      MMIAreaD.AddCeilingSpeedToList(Item.NewSpeed,
        MMIAreaD.CurrentOdometerPosition, MMIAreaD.direction * LONGINTMAX,
        MMIAreaD.CurrentOdometerPosition);
    end
    else if Item.Reason = 255 then // Handling of change reason 'other'
    begin
      Item.Reason := Ord(ssrOther);
      MMIAreaD.AddAnnouncementsToList
        (SpeedReason[TCeilingSpeedReason(Item.Reason)],
        FSpeedreasonString[Ord(Item.Reason)],
        SwapLongInt(Item.Odometerposition), MMIAreaD.CurrentOdometerPosition);
      MMIAreaD.AddCeilingSpeedToList(Item.NewSpeed,
        SwapLongInt(Item.Odometerposition), LONGINTMAX * MMIAreaD.direction,
        MMIAreaD.CurrentOdometerPosition);
    end
    else
    begin
      if (Item.Reason < Ord(ssrMax)) then
      begin
        MMIAreaD.AddAnnouncementsToList
          (SpeedReason[TCeilingSpeedReason(Item.Reason)],
          FSpeedreasonString[Item.Reason], SwapLongInt(Item.Odometerposition),
          MMIAreaD.CurrentOdometerPosition);
        MMIAreaD.AddCeilingSpeedToList(Item.NewSpeed,
          SwapLongInt(Item.Odometerposition), LONGINTMAX * MMIAreaD.direction,
          MMIAreaD.CurrentOdometerPosition);
        if (Item.Reason = Ord(ssrEndOfMa)) then
        begin
           { Calculate and store distance to primary target }
          SpeedAndDistance := DataModuleDMI.GetSpeedAndDistance(DummyChanged);
          OdoPos := SwapLongInt(Item.Odometerposition);
          if OdoPos > SpeedAndDistance.CurrentOdometer then
            DistanceToPrimaryTarget := OdoPos - SpeedAndDistance.CurrentOdometer
          else
            DistanceToPrimaryTarget := SpeedAndDistance.CurrentOdometer
              - OdoPos;
          DataModuleDMI.SetDistanceToPrimaryTarget(DistanceToPrimaryTarget);
          { store pos of primary target }
          DataModuleDMI.SetPrimaryTargetOdoPos(OdoPos);
        end;

      end
      else if ((Item.Reason >= START_OF_ADAP_SPEED_REASONS) and
        (Item.Reason <= END_OF_ADAP_SPEED_REASONS)) then
      begin
        MMIAreaD.AddAnnouncementsToList
          (AdapSpeedReason[TAdapCeilingSpeedReason(Item.Reason -
          START_OF_ADAP_SPEED_REASONS)],
          FAdapSpeedReasonString[Ord(ssrLevelCrossing)],
          SwapLongInt(Item.Odometerposition), MMIAreaD.CurrentOdometerPosition);
        MMIAreaD.AddCeilingSpeedToList((Item.NewSpeed),
          SwapLongInt(Item.Odometerposition), LONGINTMAX * MMIAreaD.direction,
          MMIAreaD.CurrentOdometerPosition);
      end
      else
      begin
         // Reason out of range
        FormMMIFrame.WriteMMIStatus('COMM', MS_SW_ERROR, MS_SER,
          Ord(mtCeilingSpeedList));
        if FormMMIFrame.TestMode then
          MessageBox(FormMainArea.Handle,
            PWideChar('Error in Ceiling_Speed. Reason out of range:' +
            IntToStr(Ord(Item.Reason))), PWideChar(ClassName),
            MB_OK or MB_ICONEXCLAMATION);
      end;
    end;
  end;
end;

{ *********{*********************************************************
* Function:    TExchange.Manual_Config_Selected
* Description:Handles the manual config message
*********************************************************}
Procedure TExchange.Manual_Config_Selected;
Begin
  Try
    DataModuleDMI.SetConfigSubState(cssManual);
    FormMMIFrame.SetMMI_ATPModeAndState;
    FormMMIFrame.UpdateDMI;

    FormMMIFrame.LogEventStr(LogLevelDetail, 'COMM', 'Manual_Config_selected');

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtManualConfigSelected));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Manual_Config_Selected:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    TExchange.ReReg_Selected
* Description:Handles the ReRegSelected message
*********************************************************}
Procedure TExchange.ReReg_Selected(Data:PReRegSelectedMT);
Begin
  Try



    DataModuleDMI.SetConfigSubState(cssReReg);

    DataModuleDMI.SetReRegOptions(Data.Options);
    FormMMIFrame.SetMMI_ATPModeAndState;
    FormMMIFrame.UpdateDMI();


  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtReRegSelected));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in ReReg_Selected:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    TExchange.Time
* Description: Handles the Time message
*********************************************************}
Procedure TExchange.Time(Data:PTimeMT);
var
 RecvdTimeMostSig4Byte :UInt32 ;
 RecvdTimeLeastSig4Byte :UInt32 ;
Begin                      //Set Realtime clock
  Try
    RecvdTimeMostSig4Byte := SwapLongWord(Data.TimeMostSig4Byte);
    RecvdTimeLeastSig4Byte := SwapLongWord(Data.TimeLeastSig4Byte);
    FormMMIFrame.SetMMITime(UnixToDelphiTime(RecvdTimeLeastSig4Byte));
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTime));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Time:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    TExchange.TrainName_Data
* Description: Handles the Train name data message
*********************************************************}
procedure TExchange.TrainName_Data(Data:PTrainNameMT);
var
  I: integer;
  NewTrainName : AnsiString;
begin
  try
    NewTrainName:='';
    for i := 1 To TRAIN_NAME_MAX_CHARS do
    begin
      if Data.trainName[i] = #0 then break;
      NewTrainName:= NewTrainName + Data.TrainName[I];
    end;
    DataModuleDMI.SetTrainId(String(NewTrainName));
    FormMMIFrame.UpdateDMI();
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTrainName));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in TrainName_Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    TExchange.RadioArea_Data
* Description: Handles the Radio Area data message
*********************************************************}
procedure TExchange.RadioArea_Data(Data:PRadioAreaMT);
var
  I: integer;
  NewRadioArea : AnsiString;
begin
  try
    NewRadioArea := '';
    for i := 1 To RADIO_AREA_MAX_CHARS do
    begin
      if Data.RadioArea[i] = #0 then break;
      NewRadioArea:= NewRadioArea + Data.RadioArea[I];
    end;
    DataModuleDMI.SetRadioArea(String(NewRadioArea));
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTrainName));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in TrainName_Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;


{*********************************************************
* Function:    TExchange.ATPNotification
* Description: Handles the ATP notification mesage
*********************************************************}
Procedure TExchange.ATPNotification(Data: PAtpNotificationMT);
Var
  i: integer;
  str: AnsiString;  // Delphi XE, string -> AnsiString
  translatedStr : AnsiString;
begin
      Beep;
      str := '';
      For i := 1 To ATP_NOTIFICATION_MAX_CHARS do begin
        if Data.Text[i] = #0 then break;
        str:= str + Data.Text[I];
      end;
        // Allow the translated string to grow as much as 3 times as the original max size,
        // See NCR arn_006#3527 / Bo H
      translatedStr := FormMMIFrame.TranslateString('ATPNotification','#',str,3*ATP_NOTIFICATION_MAX_CHARS);
      FormAtpMessages.AddMessage(String(translatedStr),Data^.mode);
end;


{*********************************************************
* Function:    TExchange.TrainVSTrackDirWanted
* Description: Handles the Train VS Track message
*********************************************************}
Procedure TExchange.TrainVSTrackDirWanted ;
Begin
  Try
      DataModuleDMI.SetTrainVsTrackDirWantedReceived(true);
      DataModuleDMI.SetATPMode(amTrainregistration);
      DataModuleDMI.SetATPState(asActiveState);
      FormTrainVsTrack.LocomotiveConnected := DataModuleDMI.GetLocoVsTrainDir;
      FormFullScreenMsg.Hide;
      FormTrainVsTrack.Show;
      FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('Track vs Train Wanted received'));
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTrnVSTrackDirWanted));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in TrainVsTrackDirWanted:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
End;

{*********************************************************
* Function:    TExchange.LocoVSTrainDir
* Description: Handles the loco VS Train Direction message
*********************************************************}
Procedure TExchange.LocoVSTrainDir(Data: PLocoVSTrnDirMT);
Var
  Dir : TLocomotiveConnected;
Begin

    // TLocomotiveConnected is which end of loco cars are connected,
    // but ATP sends what end of loco that goes first in train

  Dir := Data.LocoVSTrainDir ;

  if Dir <> lcUndefined then
  begin
    if Dir = lcAEnd then
      Dir := lcBEnd
    else
      Dir := lcAEnd;
  end;

  DataModuleDMI.SetLocoVsTrainDir(Dir);
  FormMMIFrame.UpdateDMI;

  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('LocoVsTrain:' + IntToStr(Ord(Data.LocoVSTrainDir)) + ' received'));

End;

{*********************************************************
* Function:    TExchange.Erase_PlanningArea
* Description: Handles the Erases planning area
*********************************************************}
Procedure TExchange.Erase_PlanningArea ;
Begin
  Try
   FormMainArea.MMIareaD.EraseAllLists ;

   FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('Erase_PlanningArea'));

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtErasePlanningArea));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Erase_PlanningArea:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
  FormMainArea.MMIAreaD.Refresh;

End;


{*********************************************************
* Function:    TExchange.Text_message
* Description: Handles the text message
*********************************************************}
Procedure TExchange.Text_message(Data:PTextMessageMT);
Var
  TextEntry : TTextEntry;
  i   : integer;
  str : AnsiString;
begin
  Try
    TextEntry.Time := Now;
    TextEntry.Confirm := PTextMessageMT(Data)^.ConfirmationRequested;
    TextEntry.Error := false;
    TextEntry.Valid := true;
    str := '';
    for i:= 1 to TEXT_MESSAGE_MAX_CHARS do
      str := str + PTextMessageMT(Data)^.Text[I];

    TextEntry.Text := FormMMIFrame.TranslateString('TextMessage','$#$',str,TEXT_MESSAGE_MAX_CHARS_TRANSLATED);

    DataModuleDMI.AddEvent(Now, String(TextEntry.Text));

  Except
    on E: Exception do
    begin
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Text_message:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
end;
{*********************************************************
* Function:    TExchange.StartUpHistory
* Description: Append the start-up history
*********************************************************}
procedure TExchange.StartUpHistory(Data:PStartUpHistoryMT);
begin
  try

    FormStartUpHistory.AppendHistory(Data);

  except
    on E: Exception do
    begin
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in StartUpHistory:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    TExchange.PredefTextMsg
* Description: Handles the predefined text message
*********************************************************}
Procedure TExchange.PredefTextMsg(Data: PPredefinedTextMsgMT);
Var
  TextEntry : TTextEntry;
begin
  Try
    TextEntry.Time := Now;
    TextEntry.Confirm := Data^.confirmationRequested;
    TextEntry.Error := false;
    TextEntry.Valid := true;
    TextEntry.Text := AnsiString(getPredefinedMsg(data^.messageSpecifier));
    DataModuleDMI.AddEvent(Now, TextEntry.Text);

  Except
    on E: Exception do
    begin
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in PredefTextMsg:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  End;
end;


Procedure TExchange.SendStartUp;
Type
  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TMMIStartUpMT;
                End;

var
  OutData : TOutDataType ;
  MessageDataLength : Word ;
  MessageType : Byte ;

begin
  if not startupAlreadySent then
  begin
    startupAlreadySent := true;
    try
                { Message to be acknowledged }
      MessageType := byte(ord(mtMMIStartUp)) or $80 ;
                { Leading MessageType }

      MessageDataLength := sizeof(MessageType) + SizeOf(TMMIStartUpMT);

      OutData.MT:=MessageType;
      OutData.Data.MMIStatus:= Swap(FormMMIFrame.ReadMMIStatus);
      OutData.Data.CompatibilityVersion := COMPATIBILITY_VERSION;
        {***********************************************************************
          Send this message twice. If the MMI has been shut down and
          restarted and no other vital message then this one was sent
          before shutdown, this message will be discarded by the ATP.
          This is because the vital message numher will be the same as
          last vital message received by ATP. See the MMICL documentation.
          (konedlu 2002-11-19)
        ***********************************************************************}
      putMMI_ToATP_Data(MessageDataLength, @OutData) ; //once
      putMMI_ToATP_Data(MessageDataLength, @OutData) ; //twice!

    except
      on E: Exception do
      begin
        FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtMMIStartUp));
        if FormMMIFrame.TestMode then
          MessageBox(FormMainArea.Handle,PWideChar('Error in SendStartUp:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      end;
    end;
  end;
end;

{*********************************************************
* Function:    TExchange.SendMMIStatusToATP
* Description:
*********************************************************}
Procedure TExchange.SendMMIStatusToATP ;
Type

  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TMMIStatusMT;
                End;

Var

  OutData : TOutDataType;
  MessageDataLength : Word ;
  MessageType : Byte ;

Begin

  MessageType := ord(mtMMIStatus) ;

  MessageDataLength := sizeof(MessageType) + SizeOf(TMMIStatusMT);

  OutData.MT:=MessageType;
  OutData.Data.MMIStatus:= Swap(FormMMIFrame.ReadMMIStatus) ;

  putMMI_ToATP_Data(MessageDataLength, @OutData) ;

End;

{*********************************************************
* Function:    TExchange.SendMMIButton
* Description:
*********************************************************}
Procedure TExchange.SendMMIButton ( Buttons : TButtonStatus ) ;
Type

  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TMMIToATP_DataMT;
                End;

Var
  OutData : TOutDataType;
  MessageType : Byte ;
  MessageDataLength : Word ;

Begin
          { Message to be acknowledged }
  MessageType := byte(ord(mtMMIToATP_Data)) or $80 ;
          { MessageType as leading byte }
  MessageDataLength := sizeof(MessageType) + SizeOf(TMMIToATP_DataMT);
  OutData.MT:=MessageType;
  OutData.Data.ButtonStatus:= Buttons ;
  FormMMIFrame.LogEventStr(LogLevelDebug, AnsiString('COMM'), AnsiString('SendMMIButton:' + IntToStr(Ord(Buttons)) + ' sent'));
  putMMI_ToATP_Data(MessageDataLength, @OutData) ;

End;

{*********************************************************
* Function:    TExchange.SendTrainLoaded
* Description:
*********************************************************}
Procedure TExchange.SendTrainLoaded( TrainLoaded : Byte ) ;
 type
  TOutDataType = packed record
    MT: Byte;
    Data: TTrainLoadedMT;
  end;

 var
  OutData: TOutDataType;
  MessageType: Byte;
  MessageDataLength: Word;

Begin

   MessageType := Byte(Ord(mtTrainLoaded)) or $80; // vital message

  MessageDataLength := SizeOf(MessageType) + SizeOf(TTrainLoadedMT);

  OutData.MT := MessageType;
  OutData.Data.TrainLoaded := TrainLoaded;
  putMMI_ToATP_Data(MessageDataLength, @OutData);

End;

{*********************************************************
* Function:    TExchange.SendConfirmation
* Description:
*********************************************************}
Procedure TExchange.SendConfirmation (Answer : Boolean) ;
Type

  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TConfirmationMT;
                End;

Var

  OutData : TOutDataType;
  MessageType : Byte ;
  MessageDataLength : Word ;

Begin

  MessageType := byte(ord(mtBoolConfirmation)) or $80 ;  // vital message

  MessageDataLength := sizeof(MessageType) + SizeOf(TConfirmationMT) ;

  OutData.MT:=MessageType;
  if Answer then
    OutData.Data.Confirmation:= 1
  else
    OutData.Data.Confirmation:= 2;

  putMMI_ToATP_Data(MessageDataLength, @OutData) ;

End;


{*********************************************************
* Function:    TExchange.SendDriverIDandPassword
* Description:
*********************************************************}
Procedure TExchange.SendDriverIDandPassword ;
Type
  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TDriverID_PasswordMT;
                End;

Var
  OutData : TOutDataType;
  MessageType : Byte ;
  MessageDataLength : Word ;
  I,TempLength : Integer ;
  DriverID, DriverPassword : AnsiString;

Begin

  MessageType := byte(ord(mtDriverID_Password)) or $80 ;  // vital message
  MessageDataLength := sizeof(MessageType) +  SizeOf(TDriverID_PasswordMT);
  OutData.MT:=MessageType;

  DriverID := FormLogin.ReadID;
  TempLength := Length(DriverID);

  for I := 1 to DRIVER_MAX_CHARS  do
  begin
    if I > TempLength
    then  { Fill trailing bytes with blanks }
      OutData.Data.DriverID[I] := #0
    else
      OutData.Data.DriverID[I] := DriverID[I] ;
  end;

  DriverPassword := FormLogin.ReadPassword;
  TempLength := Length(DriverPassword);

  for I := 1 to PASSWORD_MAX_CHARS  do
  begin
    if I > TempLength
    then  { Fill trailing bytes with blanks }
      OutData.Data.Password[I] := #0
    else
      OutData.Data.Password[I] := DriverPassword[I] ;
  end;

  putMMI_ToATP_Data(MessageDataLength, @OutData) ;

End;

{********************************************************
 * Function:    TExchange.SendLocoVSTrainDir
 * Description:
 ********************************************************* }
procedure TExchange.SendLocoVSTrainDir(Dir: TLocomotiveConnected);
type
  TOutDataType = packed record
    MT: Byte;
    Data: TLocoVSTrnDirMT;
  end;

var
  OutData: TOutDataType;
  MessageType: Byte;
  MessageDataLength: Word;

begin

  MessageType := Byte(Ord(mtLocVSTrnDir)) or $80; // vital message

  MessageDataLength := SizeOf(MessageType) + SizeOf(TLocoVSTrnDirMT);

    // From MMI, TLocomotiveConnected is which end of loco cars are connected,
    // but ATP wants to have what end of loco that goes first in train

  if Dir <> lcUndefined then
    if Dir = lcAEnd then
      Dir := lcBEnd
    else
      Dir := lcAEnd;

  OutData.MT := MessageType;
  OutData.Data.LocoVSTrainDir := Dir;
  putMMI_ToATP_Data(MessageDataLength, @OutData);

end;

{*********************************************************
* Function:    TExchange.SendVehicleData
* Description: Send Vehicle Data To ATP
**********************************************************}
procedure TExchange.SendVehicleData(Msg: PVehicleDataMsg);
var
  MessageLength: Word;
  NrOfDataBlocks : Word;

begin

  NrOfDataBlocks := Swap(Msg.NewVehicleData.NrOfVehicleDataBlocks);
  Msg.MT := Byte(Ord(mtVehicleData)) or $80; // Acknowledged message

  MessageLength := SizeOf(Msg.MT) + SizeOf(NrOfDataBlocks) +
      (NrOfDataBlocks * SizeOf(TAtpVehicleDataBlock));
  putMMI_ToATP_Data(MessageLength, Pointer(Msg));

end;

{ *****************************************************************
* Function:    TExchange.SendTrainVSTrackDir
* Description:
*******************************************************************}
Procedure TExchange.SendTrainVSTrackDir (dir : TTrackDirection) ;
Type

  TOutDataType= Packed Record
                  MT : Byte;
                  Data : TTrnVSTrackDirMT;
                End;


Var

  OutData : TOutDataType ;
  MessageType : Byte ;
  MessageDataLength : Word ;

Begin

  MessageType := byte(ord(mtTrnVSTrackDir)) or $80 ;  // vital message
  MessageDataLength := sizeof(MessageType) + SizeOf(TTrnVSTrackDirMT);
  OutData.Data.TrackDirection:= Dir ;
  OutData.MT:=MessageType;
  putMMI_ToATP_Data(MessageDataLength,@OutData) ;

End;


{ *****************************************************************
  * Function:    TExchange.Version
  * Description: Receive compatibility version from ATP
  ******************************************************************* }
Procedure TExchange.Version(Data: PVersionMT);
begin

  FormMMIFrame.VerifyVersion(Data.CompatibilityVersion);

end;
{ *****************************************************************
* Function:    TExchange.AreaRequest
* Description: Receive available areas from ATP
*******************************************************************}
Procedure TExchange.AreaRequest(Data: PAreaRequestMT);
var
  i: Byte;
begin

  { Do not disturb an already open form }
  if Not FormSelectArea.Visible then
  begin
    FormSelectArea.ClearAreas;

    if (Data.AreaIdCount > 0) AND (Data.AreaIdCount <= MAX_AREA_ID_COUNT) then
    begin
      for i := 1 to Data.AreaIdCount do
      begin
        FormSelectArea.AddArea(Data.AreaId[i]);
      end;
      FormSelectArea.Show;
    end
    else
    begin
      if FormMMIFrame.TestMode then
      MessageBox(FormMainArea.Handle,
        PWideChar('Error in AreaRequest:Illegal number of Area Id´s!'),
        PWideChar(ClassName), MB_OK or MB_ICONEXCLAMATION);
    end;
  end;

end;



{********************************************************
 * Function:    TExchange.SendRegistrationARea
 * Description:
 ********************************************************* }
procedure TExchange.SendRegistrationArea(AreaId : Byte);
type
  TOutDataType = packed record
    MT: Byte;
    Data: TRegistrationAreaMT;
  end;

var
  OutData: TOutDataType;
  MessageType: Byte;
  MessageDataLength: Word;

begin

  MessageType := Byte(Ord(mtRegistrationArea)) or $80; // vital message

  MessageDataLength := SizeOf(MessageType) + SizeOf(TRegistrationAreaMT);

  OutData.MT := MessageType;
  OutData.Data.AreaId := AreaId;
  putMMI_ToATP_Data(MessageDataLength, @OutData);

end;

{********************************************************
 * Function:    TExchange.SendNewTrainName
 * Description:
 ********************************************************* }
Procedure TExchange.SendNewTrainName(TrainName : String);
type
  TOutDataType = packed record
    MT: Byte;
    Data: TTrainNameDataMT;
  end;

var
  OutData: TOutDataType;
  MessageType: Byte;
  MessageDataLength: Word;
  i : Byte;

begin

  MessageType := Byte(Ord(mtTrainName)) or $80; // vital message

  MessageDataLength := SizeOf(MessageType) + SizeOf(TTrainNameDataMT);

  OutData.MT := MessageType;

  for i := 1 to TRAIN_NAME_MAX_CHARS  do
  begin
    if (i > Length(TrainName))
    then  { Fill trailing bytes with blanks }
      OutData.Data.TrainName[i] := #0
    else
      OutData.Data.TrainName[i] := AnsiChar(TrainName[i]);
  end;

  putMMI_ToATP_Data(MessageDataLength, @OutData);

end;

{*******************************************************
  * Function:    TExchange.TypicalConfigTypes
  * Description: Takes care of the TypicalConfigType messages.
  ********************************************************* }
procedure TExchange.TypicalConfig(Data: PTypicalConfigTypes);
var
  TypicalConfigTypeCounter: Byte;
  TypicalConfigBlockCount: Byte;
  i: Byte;
  storedIndex : Byte;
  TypicalConfigstr: AnsiString;
  VehicleTypeNamestr: AnsiString;
  NumberofCars: Byte;
begin

  DataModuleDMI.ClearTypicalConfigStorage;
  storedIndex := 1;
  TypicalConfigBlockCount := Data.NrofTypicalConfigBlocks;
  if (TypicalConfigBlockCount > 0) and
    (TypicalConfigBlockCount <= MAX_TYPICAL_CONFIG_BLOCKS) then
  begin
    // Clear the TypicalConfigTypes
    for TypicalConfigTypeCounter := 1 to TypicalConfigBlockCount do
    begin
      TypicalConfigstr := '';

      for i := 1 to TYPICAL_CONFIG_NAME_MAX_CHARS do
      begin
        if Data.TypicalConfigBlocks[TypicalConfigTypeCounter].TypicalConfigName
          [i] = #0 then
          break;
        TypicalConfigstr := TypicalConfigstr + Data.TypicalConfigBlocks
          [TypicalConfigTypeCounter].TypicalConfigName[i];
      end;
      VehicleTypeNamestr := '';

      for i := 1 to VEHICLE_TYPE_NAME_MAX_CHARS do
      begin
        if Data.TypicalConfigBlocks[TypicalConfigTypeCounter].VehicleTypeName
          [i] = #0 then
          break;
        VehicleTypeNamestr := VehicleTypeNamestr + Data.TypicalConfigBlocks
          [TypicalConfigTypeCounter].VehicleTypeName[i];
      end;
      NumberofCars := Data.TypicalConfigBlocks[TypicalConfigTypeCounter]
        .CarsinTypicalConfig;
      // Store the TypicalConfigName and VehicleTypeName in DataModule
      DataModuleDMI.SetTypicalConfig(storedIndex, TypicalConfigstr,
        VehicleTypeNamestr, NumberofCars);
       Inc(storedIndex);
     end;
  end;
end;

{ *********************************************************
  * Function:    TExchange.VehicleTypes
  * Description: Takes care of the VehicleTypes messages.
  ********************************************************* }
procedure TExchange.VehicleTypes(Data: PAtpVehicleTypes);
var
  VehicleTypeCounter: Byte;
  VehicleTypeBlockCount: Byte;
  i: Byte;
  VehicleType: Byte;
  str: AnsiString; // Delphi XE, string -> AnsiString
begin
  VehicleTypeBlockCount := Data.NrOfVehicleTypesBlocks;
  if (VehicleTypeBlockCount > 0) and
    (VehicleTypeBlockCount <= MAX_VEHICLE_TYPE_BLOCKS) then
  begin
    for VehicleTypeCounter := 1 to VehicleTypeBlockCount do
    begin
      VehicleType := Data.VehicleTypeBlocks[VehicleTypeCounter].VehicleType;
      str := '';
      for i := 1 to VEHICLE_TYPE_NAME_MAX_CHARS do
      begin
        if Data.VehicleTypeBlocks[VehicleTypeCounter].VehicleTypeName[i] = #0 then
          break;
        str := str + Data.VehicleTypeBlocks[VehicleTypeCounter].VehicleTypeName[i];
      end;
        // Store Vechile Type and Vehicle Type Name in Data Module
      DataModuleDMI.SetVehicleType(VehicleType, string(str));

    end;
  end;
end;


{ *********************************************************
  * Function:    TExchange.VehicleData
  * Description: Takes care of the VehicleData messages.
  ********************************************************* }
procedure TExchange.VehicleData(Data: PAtpVehicleData);
var
  VehicleDataCounter: Integer;
  VehicleDataBlockCount: Word;
  VehicleNameIndex: Byte;
  VehicleType : Byte;
  str: AnsiString;  // Delphi XE, string -> AnsiString
  NrOfVehicleInBlock : Word;
  VehicleNodeId : Word;
  VehicleCount : Integer;
begin

  VehicleCount := 0;
  DataModuleDMI.ClearVehicleData();
  VehicleDataBlockCount := Swap(Data.NrOfVehicleDataBlocks);
  if (VehicleDataBlockCount > 0) and
    (VehicleDataBlockCount <= MAX_VEHICLE_DATA_BLOCKS) then
  begin
    for VehicleDataCounter := 1 to VehicleDataBlockCount do
    begin
      NrOfVehicleInBlock := Swap(Data.VehicleDataBlocks[VehicleDataCounter].NrOfVehicleInBlock);
      VehicleCount := VehicleCount + NrOfVehicleInBlock;
      VehicleType := Data.VehicleDataBlocks[VehicleDataCounter].VehicleType;
      VehicleNodeId := Swap(Data.VehicleDataBlocks[VehicleDataCounter].VehicleNodeId);
      str := '';
      for VehicleNameIndex := 1 to VEHICLE_NAME_MAX_CHARS do
      begin
        if Data.VehicleDataBlocks[VehicleDataCounter].VehicleName
          [VehicleNameIndex] = #0 then
          break;
        str := str + Data.VehicleDataBlocks[VehicleDataCounter].VehicleName
          [VehicleNameIndex];
      end;
        // Store Vechile Type and Vechile Type Name in Data Module
      DataModuleDMI.SetVehicleData(VehicleDataCounter, NrOfVehicleInBlock,
        VehicleType, VehicleNodeId, AnsiString(str));
    end;
  end;
  VehicleCount := VehicleCount - 1; // ATP sends loco in vehicle data
  DataModuleDMI.SetNbOfVehiclesConnected(VehicleCount);
end;


{*********************************************************
* Function:    TExchange.LocationData
* Description: Handles the Location data message
*********************************************************}
procedure TExchange.LocationData(Data: PAtpLocationData);
var
  I: integer;
  NewLocationName : AnsiString;
begin
  try
    NewLocationName:='';
    for I := 1 To TRAIN_NAME_MAX_CHARS do
    begin
      if Data.LocationName[I] = #0 then break;
      NewLocationName:= NewLocationName + Data.LocationName[I];
    end;
    DataModuleDMI.SetLocationName(String(NewLocationName));
    DataModuleDMI.SetLocationType(Data.LocationType);
    FormMMIFrame.UpdateDMI();
  except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtLocationData));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Location Data:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;


{*********************************************************
* Function:    TExchange.ETARequest
* Description: Handles the ETA
*********************************************************}
procedure TExchange.ETARequest(Data: PETAMT);
var
 RecvdETAMostSig4Byte : UInt32;
  RecvdETALeastSig4Byte : UInt32;
begin
  Try
    RecvdETAMostSig4Byte := SwaplongWord(Data.ETAMostSig4Byte); //4 most significant byte are not used
    RecvdETALeastSig4Byte := SwaplongWord(Data.ETALeastSig4Byte);
    DataModuleDMI.SetETA(RecvdETALeastSig4Byte);
    FormMMIFrame.UpdateDMI;
  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMM',MS_SW_ERROR,MS_SER,Ord(mtTime));
      if FormMMIFrame.TestMode then
        MessageBox(FormMainArea.Handle,PWideChar('Error in Time:' + E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
    end;
  end;
end;

{*********************************************************
* Function:    TExchange.TrainWeight
* Description: Handles the Train Weight
*********************************************************}
Procedure TExchange.TrainWeight(Data: PTTrainWeightMT);
var
TrainWeight : LongWord;
begin
    TrainWeight := SwapLongWord(Data.TrainWeight);
    DataModuleDMI.SetTrainWeight(TrainWeight);
end;

{*********************************************************
* Function:    TExchange.HandleRemoteDMILogin
* Description: Handles login from Remote DMI Interface
*********************************************************}
Procedure TExchange.HandleRemoteDMILogin(UserName : String;Pwd : String; Var ResponseString : String);
var
  ATPVerificationState : T_ATPVerificationStates;
  ATPVerificationStateChanged : Boolean;

begin

  ATPVerificationState := DataModuleDMI.GetATPVerificationState(ATPVerificationStateChanged);


  if ATPVerificationState = avsLoggedOnState then
    ResponseString := 'Ok'
  else
  begin

    if (FormLogin.Visible AND FormLogin.LoginButton.Visible AND FormLogin.LoginButton.Enabled ) then
    begin
      FormLogin.NameEdit.Text := UserName;
      FormLogin.PasswordEdit.Text := Pwd;
      FormLogin.LoginButtonClick(FormLogin.LoginButton);
      ResponseString := 'Retry';
    end
    else
      ResponseString := 'Retry';
  end;
end;
{*********************************************************
* Function:    TExchange.HandleRemoteDMIRegArea
* Description: Handles selection of Reg area from remote
*********************************************************}
Procedure TExchange.HandleRemoteDMIRegArea(AreaId : String; Var ResponseString : String);
var
  AreaNo : Word;

begin

  AreaNo :=StrToIntDef(AreaId, 0);
  if AreaNo > 0
  then
  begin
    if FormSelectArea.Visible then
    begin
      SendRegistrationArea(AreaNo);
      FormSelectArea.Hide;
      ResponseString := 'Ok';
    end
    else
    begin
      ResponseString := 'Retry';
    end;
  end
  else
  begin
    ResponseString := 'Fail';
  end;
end;
{*********************************************************
* Function:    TExchange.HandleRemoteDMISelect
* Description: Handles select from Remote DMI Interface
*********************************************************}
Procedure TExchange.HandleRemoteDMISelect(Operation : String; Var ResponseString : String);
var
  DriverInfo : TDriverInfoMT;
  DriverInfoChanged : Boolean;

  AdditionalStatus1 : Byte;
  AdditionalStatus1Changed : Boolean;

  AllowedButtons : Byte;
  AllowedButtonsChanged : Boolean;

  ATPMode        : T_ATPmodes;
  ATPModeChanged : Boolean;

  ConfigSubState: TConfigSubState ;
  ConfigSubStateChanged : Boolean;

  AdditionalConfirmationInfo : Byte;
  AdditionalConfirmationInfoChanged : Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

  ConfirmModeChange : Byte;
  ConfirmModeChanged : Boolean;

begin
  if (Operation = 'RELEASESB') then

   begin
    DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);
    if ByteBool(DriverInfo.StatusD11 and FLASHSBBRAKEBUTTON) then
    begin
      Exchange.SendMMIButton(bsBrakeRelease);
      ResponseString := 'Ok';
    end
    else
    begin
      if ByteBool(DriverInfo.StatusD11 and SERVICEBRAKEAREAPPLIED) then
        ResponseString := 'Retry'
      else
          // SB is already released
        ResponseString := 'Ok';
    end;
  end
  else if (Operation = 'RELEASEEB') then
  begin
    DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);
    if ByteBool(DriverInfo.StatusD11 and FLASHEBBRAKEBUTTON) then
    begin
      Exchange.SendMMIButton(bsBrakeRelease);
      ResponseString := 'Ok';
    end
    else
    begin
      if ByteBool(DriverInfo.StatusD11 and EMERGENCYBRAKEAREAPPLIED) then
        ResponseString := 'Retry'
      else
          // SB is already released
        ResponseString := 'Ok';
    end;

  end
  else if (Operation = 'BRAKETEST') then
  begin
    AdditionalStatus1 := DataModuleDMI.GetAdditionalStatus1(AdditionalStatus1Changed);
           {Only when Brake Test is possible}
    if ByteBool(AdditionalStatus1 AND AS_BrakeTestNotification) OR ByteBool(AdditionalStatus1 AND AS_BrakeTestMandatory) then
    begin
      Exchange.SendMMIButton(bsStartBrakeTest);
           {May take a while respond with retry immediately }
      ResponseString := 'Retry';
    end
    else
    begin
      ResponseString := 'Ok';
    end;
  end
  else if (Operation = 'CONFIG') then
  begin

    {ATP Mode}
    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);

    if ATPMode = amTrainconfiguration then
      ResponseString := 'Ok'
    else
    begin

      {Area F buttons}
      AllowedButtons := DataModuleDMI.GetAllowedButtons(AllowedButtonsChanged);
      if bytebool(AllowedButtons and ALLOWED_TO_ENTER_CONFIG_MODE) then
      begin
        Exchange.SendMMIButton(bsTrainConfig);
           {May take a while ... respond with retry immediately }
        ResponseString := 'Retry';
      end
      else
      begin
        ResponseString := 'Fail';
      end;
    end;
  end
  else if (Operation = 'ACCEPTCARSONBSIDE') OR (Operation = 'ACCEPTAUTOCONFIG') then
  begin

    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration) then
    begin

      if (ConfigSubstate = cssAutomatic) AND (FormAutomaticConfiguration.Visible) then
      begin
        ResponseString := 'Ok';
        Exchange.SendMMIButton(bsAcceptAutomaticConfiguration);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end

  else if (Operation = 'ACCEPTREREG') then
  begin

    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration) then
    begin

      if (ConfigSubState = cssReReg) AND (FormReRegistration.Visible) then
      begin
        ResponseString := 'Ok';
        SendConfirmation(True);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end
  else if (Operation = 'REJECTREREG') then
  begin

    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration) then
    begin

      if (ConfigSubState = cssReReg) AND (FormReRegistration.Visible) then
      begin
        ResponseString := 'Ok';
        SendConfirmation(false);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end
  else if (Operation = 'HANDLINGDONE') then
  begin

    AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
    if ByteBool(AdditionalAllowedToInfo and ALLOWED_TO_SHOW_HANDLING_DONE) then
    begin
      ResponseString := 'Ok';
      SendMMIButton(bsHandlingDone);
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end

  else if (Operation = 'CONFIRMDEPARTURE') then
  begin

    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo(AdditionalConfirmationInfoChanged);

    if (ATPMode = amTrainconfiguration) then
    begin

       { Departure Test confirmation }
      if (ByteBool(AdditionalConfirmationInfo and CONFIRM_DEPARTURE_TEST)) then
      begin
        ResponseString := 'Ok';
        Exchange.SendMMIButton(bsConfirmDeparture);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end
  else if (Operation = 'CONFIRMNEWMODE') then
  begin

    ConfirmModeChange := DataModuleDMI.GetConfirmModeChange(ConfirmModeChanged);

       { New mode confirmation }
    if (ConfirmModeChange > 0) then
    begin
      ResponseString := 'Ok';
      Exchange.SendMMIButton(FormConfirmNewATPMode.thisButton);
    end
    else
    begin
      ResponseString := 'Retry';
    end;

  end
  else if (Operation = 'LOADED') then
  begin
      ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration) then
    begin
      if (ConfigSubstate = cssAutomatic) AND (FormAutomaticConfiguration.Visible) then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainLoaded(1);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end;
  end
  else if (Operation = 'NOTLOADED') then
  begin
    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration) then
    begin
      if (ConfigSubstate = cssAutomatic) AND (FormAutomaticConfiguration.Visible) then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainLoaded(0);
      end
      else
      begin
        ResponseString := 'Retry';
      end;
    end;
  end
  else if (Operation = 'CONFIRMLOADCHANGE') then
  begin
    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);

    if (ATPMode = amTrainconfiguration)  then
    begin
      if (ConfigSubstate = cssAutomatic) AND (FormAutomaticConfiguration.Visible) AND (FormConfirmLoadedStatus.Visible) then
      begin
         ResponseString := 'Ok';
         Exchange.SendMMIButton(bsConfirmChangeOfTrainLoadedStatus);
       end
       else
      begin
        ResponseString := 'Retry';
      end;
    end;
  end
  else if (Operation = 'CANCELLOADCHANGE') then
  begin
    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
    ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);
    AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);

    if (ATPMode = amTrainconfiguration)  then
    begin
      if (ConfigSubstate = cssAutomatic) AND (FormAutomaticConfiguration.Visible) AND (FormConfirmLoadedStatus.Visible)  then
      begin
         ResponseString := 'Ok';
         Exchange.SendMMIButton(bsCancelChangeOfTrainLoadedStatus);
       end
       else
      begin
        ResponseString := 'Retry';
      end;
    end;
  end
  else if (Pos('ORIENTATION',Operation) = 1) then {Command starts with Orientation}
  begin

    ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);

    if (ATPMode = amTrainRegistration) then
    begin

      if Operation = 'ORIENTATION1FORW' then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainVSTrackDir(tdForwardDrivingForward) ;
      end
      else if Operation = 'ORIENTATION1REV' then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainVSTrackDir(tdForwardDrivingReverse) ;
      end
      else if Operation = 'ORIENTATION0FORW' then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainVSTrackDir(tdReverseDrivingForward) ;
      end
      else if Operation = 'ORIENTATION0REV' then
      begin
        ResponseString := 'Ok';
        Exchange.SendTrainVSTrackDir(tdReverseDrivingReverse) ;
      end
      else
      begin
        ResponseString := 'Fail';
      end;
    end
    else if (ATPMode = amTrainconfiguration) then
    begin   { Retry in case ATPMode has not yet changed to Registration }
      ResponseString := 'Retry';
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end

  else
    ResponseString := 'Fail';
end;
{*********************************************************
* Function:    TExchange.HandleRemoteDMIATPInfo
* Description: Retrieves ATP information to send on the Remote Interface
*********************************************************}
Procedure TExchange.HandleRemoteGetATPInfo(Var ResponseString : String);
var
  ATPMode        : T_ATPmodes;
  ATPModeChanged : Boolean;

  ConfigSubState: TConfigSubState ;
  ConfigSubStateChanged : Boolean;

  ATPVerificationState : T_ATPVerificationStates;
  ATPVerificationStateChanged : Boolean;

  InterfaceFlags : Byte;
  InterfaceFlagsChanged : Boolean;

  LocoStatus : LongWord;
  LocoStatusChanged : Boolean;

  AdditionalStatus1 : Byte;
  AdditionalStatus1Changed : Boolean;

  AdditionalStatus2 : Byte;
  AdditionalStatus2Changed : Boolean;

  AdditionalStatus : Word;

  ConfirmModeChange : Byte;
  ConfirmModeChanged : Boolean;

  AllowedButtons : Byte;
  AllowedButtonsChanged : Boolean;

  BrakeTestStatus : T_BrakeTestStatus;
  BrakeTestStatusChanged : Boolean;

  AdditionalAllowedToInfo : Byte;
  AdditionalAllowedToInfoChanged : Boolean;

  AdditionalConfirmationInfo : Byte;
  AdditionalConfirmationInfoChanged : Boolean;

  AdaptationTrainStatus : Byte;
  AdaptationTrainStatusChanged: Boolean;

  DriverInfo : TDriverInfoMT;
  DriverInfoChanged : Boolean;

  SpeedAndDistance : TSpeed_DistanceMT;
  SpeedAndDistanceChanged : Boolean;

  TrainLength : Word;
  TrainLengthChanged : Boolean;

  FieldStr  : String;
begin
  ResponseString := 'Ok;ATPInfo';
    //Field 1, ATPMode
  ATPMode := DataModuleDMI.GetATPMode(ATPModeChanged);
  FieldStr := IntToStr(Ord(ATPMode));
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 2, Config mode substate
  ConfigSubState := DataModuleDMI.GetConfigSubState(ConfigSubStateChanged);
  FieldStr := IntToStr(Ord(ConfigSubState));
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 3, Driver verification state
  ATPVerificationState := DataModuleDMI.GetATPVerificationState(ATPVerificationStateChanged);
  FieldStr := IntToStr(Ord(ATPVerificationState));
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 4, Interface flags
  InterfaceFlags := DataModuleDMI.GetInterfaceFlags(InterfaceFlagsChanged);
  FieldStr := IntToStr(InterfaceFlags);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 5, Core train status
  LocoStatus := DataModuleDMI.GetLocoStatus(LocoStatusChanged);
  FieldStr := IntToStr(LocoStatus);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 6, ATO Mode (Future)
  ResponseString := ResponseString + ';' + IntToStr(Ord(atomManual));

    //Field 7, Additional status
  AdditionalStatus1 := DataModuleDMI.GetAdditionalStatus1(AdditionalStatus1Changed);
  AdditionalStatus2 := DataModuleDMI.GetAdditionalStatus1(AdditionalStatus2Changed);
  AdditionalStatus := (AdditionalStatus1 * $10000) + AdditionalStatus2;
  FieldStr := IntToStr(AdditionalStatus);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 8, Confirm mode change
  ConfirmModeChange   :=  DataModuleDMI.GetConfirmModeChange(ConfirmModeChanged);
  FieldStr := IntToStr(ConfirmModeChange);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 9, Allowed to
  AllowedButtons := DataModuleDMI.GetAllowedButtons(AllowedButtonsChanged);
  FieldStr := IntToStr(AllowedButtons);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 10, Brake test status
  BrakeTestStatus := DataModuleDMI.GetBrakeTestStatus(BrakeTestStatusChanged);
  FieldStr := IntToStr(Ord(BrakeTestStatus));
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 11, Additional allowed to
  AdditionalAllowedToInfo :=  DataModuleDMI.GetAdditionalAllowedToInfo(AdditionalAllowedToInfoChanged);
  FieldStr := IntToStr(AdditionalAllowedToInfo);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 12, Additional confirm
  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo(AdditionalConfirmationInfoChanged);
  FieldStr := IntToStr(AdditionalConfirmationInfo);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 13, Adaptation train status
  AdaptationTrainStatus := DataModuleDMI.GetAdaptationTrainStatus(AdaptationTrainStatusChanged);
  FieldStr := IntToStr(AdaptationTrainStatus);
  ResponseString := ResponseString + ';' + FieldStr;

  DriverInfo := DataModuleDMI.GetDriverInfo(DriverInfoChanged);

    //Field 14, Permitted driving direction
  FieldStr := IntToStr(Ord(DriverInfo.PermittedDrivingDirection));
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 15, Permitted speed
  FieldStr := IntToStr(DriverInfo.Pspeed);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 16, Target speed
  FieldStr := IntToStr(DriverInfo.Tspeed);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 17, Target speed
  FieldStr := IntToStr(DriverInfo.TimeToIntervention);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 18, Remaining distance to target
{  FieldStr := IntToStr(DriverInfo.RemainingDistanceToTargetPoint);}
  FieldStr := IntToStr(DataModuleDMI.GetDistanceToPrimaryTarget);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 19, Remaining distance to BCA
  FieldStr := IntToStr(DriverInfo.RemainingDistanceToBCA);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 20, Brake related indications
  FieldStr := IntToStr(DriverInfo.StatusD11);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 21, Supervision related indications
  FieldStr := IntToStr(DriverInfo.StatusD12);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 22, MA Margin
  FieldStr := IntToStr(DriverInfo.MAMargin);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 23, Current speed
  SpeedAndDistance := DataModuleDMI.GetSpeedAndDistance(SpeedAndDistanceChanged);
  FieldStr := IntToStr(SpeedAndDistance.CurrentSpeed);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 24, Leading track
  FieldStr := IntToStr(SpeedAndDistance.leadingTrackSection);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 25, Leading position
  FieldStr := IntToStr(SpeedAndDistance.leadingPositionCm);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 26, Trailing track
  FieldStr := IntToStr(SpeedAndDistance.trailingTrackSection);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 27, Trailing position
  FieldStr := IntToStr(SpeedAndDistance.trailingPositionCm);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 28, Train length
  TrainLength := DataModuleDMI.GetTrainLength(TrainLengthChanged);
  FieldStr := IntToStr(TrainLength);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 29, Track gradient
  FieldStr := IntToStr(SpeedAndDistance.CurrentTrackGradient);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 30, Effective gradient
  FieldStr := IntToStr(SpeedAndDistance.CurrentEffectiveGradient);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 31, Brakeability
  FieldStr := IntToStr(DriverInfo.Brakeability);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 32, Brake delay, EB
  FieldStr := IntToStr(DriverInfo.BrakeDelayEB);
  ResponseString := ResponseString + ';' + FieldStr;

    //Field 33, Brake delay, SB
  FieldStr := IntToStr(DriverInfo.BrakeDelaySB);
  ResponseString := ResponseString + ';' + FieldStr;

end;
{*********************************************************
* Function:    TExchange.HandleRemoteMinimize
* Description: Minimize application window
*********************************************************}
Procedure TExchange.HandleRemoteMinimize(Var ResponseString : String);
begin
  Application.Minimize;
  // Do not send response on Minimize / Restore as they are used as
  // AOS internal commands response shall not be sent back to the test-environment
  ResponseString := '';
end;
{*********************************************************
* Function:    TExchange.HandleRemoteRestore
* Description: Restore application window
*********************************************************}
Procedure TExchange.HandleRemoteRestore(Var ResponseString : String);
begin
  Application.Restore;
  // Do not send response on Minimize / Restore as they are used as
  // AOS internal commands response shall not be sent back to the test-environment
  ResponseString := '';
end;
{*********************************************************
* Function:    TExchange.HandleRemoteDMIATPInfo
* Description: Retrieves ATP information to send on the Remote Interface
*********************************************************}
Procedure TExchange.HandleRemoteDMIClose(Var ResponseString : String);
begin
  Application.Terminate;
  ResponseString := 'Ok'; // Will not be sent back because app is closing
end;
{*********************************************************
* Function:    TExchange.HandleRemoteCommand
* Description: Handles command arriving on the Remote DMI Interface
*********************************************************}
Procedure TExchange.HandleRemoteCommand(Command : String; ArgList : TStringList; Var ResponseString:String);
var
  Operation : String;
  AreaId    : String;
begin
  if (Command = 'DMISELECT')
  then
  begin
    if (ArgList.Count > 1) then
    begin
      Operation := ArgList[1];
      HandleRemoteDMISelect(Operation, ResponseString);
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end
  else if (Command = 'DMILOGIN') then
  begin
    if (ArgList.Count > 2) then
    begin
      HandleRemoteDMILogin(ArgList[1], ArgList[2], ResponseString);
    end
    else
      ResponseString := 'Fail';
  end
  else if (Command = 'GETATPINFO') then
  begin
    HandleRemoteGetATPInfo(ResponseString);
  end
  else if (Command = 'MINIMIZE') then
  begin
    HandleRemoteMinimize(ResponseString);
  end
  else if (Command = 'RESTORE') then
  begin
    HandleRemoteRestore(ResponseString);
  end
  else if (Command = 'DMICLOSE') then
  begin
    HandleRemoteDMIClose(ResponseString);
  end
  {DMIREGAREA}
  else if (Command = 'DMIREGAREA')
  then
  begin
    if (ArgList.Count > 1) then
    begin
      AreaId := ArgList[1];
      HandleRemoteDMIRegArea(AreaId, ResponseString);
    end
    else
    begin
      ResponseString := 'Fail';
    end;
  end
  else
    ResponseString := 'Fail';
end;
{*********************************************************
* Function:    TExchange.RemoteCommand
* Description: Handles remote commands arriving on the Remote DMI Interface
* Commands are NOT case-sensitive so all characters are translated to upper-case
*********************************************************}
Procedure TExchange.RemoteCommand(CommandString : String; Var ResponseString : String);
var
  ArgList: TStringList;
  Command: String;
begin

  ArgList := TStringList.Create;
  Split(' ', UpperCase(CommandString), ArgList) ;

  if (ArgList.Count > 0) then
  begin
    Command := ArgList[0];
    HandleRemoteCommand(Command, ArgList, ResponseString);
  end
  else
  begin
    ResponseString:='Fail';
  end;

  ArgList.Free;
end;

{****************************************************************************
* INITIALIZATION PART                                                       *
****************************************************************************}
initialization

{****************************************************************************
* FINALIZATION PART                                                         *
****************************************************************************}
finalization

{****************************************************************************
* EXPORTS DECLARATIONS                                                     *
****************************************************************************}

{****************************************************************************
* RESOURCE STRING DECLARATIONS                                              *
****************************************************************************}
end.


