
(*************************************************************************
           © COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011.
           ===============================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation (Signal) Sweden AB.
    All rights reserved.
    The program may be used and/or copied only with the
    written permission from Bombardier Transportation (Signal) Sweden AB,
    and in accordance with the terms and conditions stipulated in the
    agreement/contract under which the program has been supplied.


    NAME :  MMITypes.pas

    PROJECT :  LKAB, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------

    2.3.5  Bo Hermansson    110310  Delphi XE
                                    String -> AnsiString :
                                    FFilename
                                    TrainName, CarName etc
                                    redeclared to Array of AnsiChar
                                    instead of Array of Char.
                                    AnsiChar is guaranteed to be 8 bit
                                    Char is 'Wide' in Delphi 2010 and later

    2.6.2   Bo Hermansson   110830  Constants for Train/Car name, password size

    2.7.0   Bo Hermansson   110929  New protocol for LK

    2.7.3   Bo Hermansson   111018  Renamed "Static Speed" to "Ceiling Speed"

    2.7.5   Bo Hermansson   111025  FirstTimeInactive state removed etc

    2.7.11  Bo Hermansson   111101  Removed mtShutdownMMI
    2.7.24  Bo Hermansson   111130  mtStartUpHistory
    2.7.35  Bo Hermansson   120117  Added Text-types
    2.7.38  Bo Hermansson   120201  Removed ErrorTxtTypeToStr, added Text-types
    2.7.45  Bo Hermansson   120314  Removed obsolete message-types
    2.8.10  Bo Hermansson   121015  Display leading/trailing pos [Test]
    2.8.14  Bo Hermansson   121024  Obsolete types/strings removed
    2.8.15  Bo Hermansson   121029  ODOMETER_INVALID
    2.8.21  Bo Hermansson   121116  Max length of text-messages 20->25
    2.8.23  Bo Hermansson   130320  New and spare [ERROR]TextType´s for RollAwayProtection etc.
    2.8.26  Bo Hermansson   130919  New texts, ShortMonthNames, Leading/Trailing.
    2.9.1   Bo Hermansson   131029  TEXT_MESSAGE_MAX_CHARS
    3.0.4   Bo Hermansson   140411  CompatibilityVersion introduced
    3.2.2   Bo Hermansson   150402  TT_LCSError
    4.0.0   Bo Hermansson   170428  Changed ATPMode, LocoStatus to align with ATP / BHP
    4.1.0   Bo Hermansson   170822  Supporting up to 350 cars (previously 27)
                                    and MAX_PACKAGE_SIZE 512->8500 bytes.
                                    Changed related byte-size variables to word-size.
                                    Changed TRAIN/CAR_NAME_MAX_CHARS 10->20
    4.1.2   Bo Hermansson   171012  Support BrakeTest and Standstill
    4.1.7   Bo Hermansson   171025  Now dispays up to 25 characters per message

    DESCRIPTION :  Type declarations for the MMI program

 Type declarations grouped according:
 1. Mode, State Enums
 2. MessageType Enums
 3. Various Enums in messages and other
 4. Pointer declarations
 5. Messagetype declarations
 6. Constants
************************************************************************)
unit MMITypes;

interface
{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
uses
  Sysutils, Classes;

const

  LONGINTMAX = $7FFFFFFF;

  COMPATIBILITY_VERSION = 2;

  COMPATIBILITY_VERSION_READ_TIMEOUT = 5;
  { Version should arrive from ATP within
    n seconds after that MMIStartup sent to ATP }

  CAR_NAME_MAX_CHARS    = 20;   // Max characters in car name
  TRAIN_NAME_MAX_CHARS  = 20;   // Max characters in train name
  RADIO_AREA_MAX_CHARS  = 20;   // Max Characters in radio area
  PASSWORD_MAX_CHARS    = 20;   // Max characters in password
  DRIVER_MAX_CHARS      = 20;   // Max characters in driver id
  CAR_BLOCK_LIST_MAX    = 8;    // Max no of car blocks (count, car id)

                                // Max characters in ATP Notfication Message
  ATP_NOTIFICATION_MAX_CHARS = 100;

  TEXT_MESSAGE_MAX_CHARS = 20;  // Max no of characters to receive from ATP
                                // Max no of characters to display (after translation)
  TEXT_MESSAGE_MAX_CHARS_TRANSLATED = 35;

  CAR_LIST_MAX          = 350;
  CEILING_SPEED_MAX     = 51;
  GRADIENT_MAX          = 52;

  MAX_AREA_ID_COUNT     = 10;

  LF = #10;

  MMI_MAX_PACKAGE_SIZE  = 8787;
  MMI_HEADER_SIZE       = 5;					{ MS,HT,MN,DL(MSB),DL(LSB) }
  MMI_TRAILER_SIZE      = 4;					{ 4-byte CRC }
  MMI_DATA_MAX_SIZE     = (MMI_MAX_PACKAGE_SIZE - MMI_HEADER_SIZE - MMI_TRAILER_SIZE);

      { Log levels }

  LogLevelNormal = 1;
  LogLevelDetail = 2;
  LogLevelDebug = 3;

  VEHICLE_TYPE_NAME_MAX_CHARS  = 20;   // Max characters in vehicle type name
  MAX_VEHICLE_TYPE_BLOCKS = 20;   //Max blocks in Vehicletype message
  VEHICLE_NAME_MAX_CHARS  = 20;  //Max characters in vehicle name
  MAX_VEHICLE_DATA_BLOCKS = 351; //Max Number of Vehicle data
  LOCO_TYPE_RANGE_START = 128; //Loco type will start from 128
  LOCO_TYPE_RANGE_END = 254;   //Loco type range is 128 to 254
  VEHICLE_TYPE_UNDEFINED = 255; //Vechicle type 255 is undefined
  TYPICAL_CONFIG_NAME_MAX_CHARS = 20; //Max Characters in Typical Config type name
  MAX_TYPICAL_CONFIG_BLOCKS    =  5; //Max Number of Typical Configurations in message
  MAX_CARS_IN_EACH_TYPICAL_CONFIG = 150; //Max Number of Cars in each Typical Configuration
  START_OF_ADAP_SPEED_REASONS = 129; //Starting count for the speed change reason for adaptation
  END_OF_ADAP_SPEED_REASONS  = 253;  //End count for the speed change reason for adaptation
{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
Type

THostSettings =
              Record
                Host : AnsiString;
                Port : Integer;
              End;

T_Orientation = (orUndefined,
                 orFixed,
                 orNotFixed);

//*****************************************************
// Mode, State and Status enums
//*****************************************************
T_ATPmodes = (
      amUndefined,                    { ATP Modes }
      amPowerup,
      amTrainconfiguration,
      amTrainregistration,
      amBalisSearchMode,
      amNormal,
      amShunting,
      amLocation,
      amYard,
      amUnregistered,
      amPoweringDown,
      amSafetyHalt,
      amSleeping,
      amStaffResponsible,
      amShuntingRoute,
      amPossession,
      amSplit,
      amJoin,
      amSafeBrakeToStop);

T_ATPStates = (
          asUndefined,                    {ATP States }
	        asBasicsSystemStartUp,
	        asApplicationStartUp,
	        asInactive,
	        asActivationInitiation,
	        asActivationTest,
	        asActiveState,
	        asFatalFailureState,
          asSystemRestart,
          asPowerDown);

T_ATPServiceSubStates = (
          asssUndefined);       { ATP Service Sub States }

T_ATPVerificationStates = (
          avsUndefined,     {ATP Verification Sub States }
          avsNoActionState,
          avsInputState,
          avsVerificationState,
          avsRedoinputState,
          avsLoggedOnState);

T_ATPLocomotiveStatus = LongWord ;

T_ATOModes = (atomUndefined,
            atomManual,
            atomSupervised,
            atomAutomatic,
            atomRemote
            );
T_ATOSwitch = (
            ATOSwitch_Undefined,
            ATOSwitch_Manual,
            ATOSwitch_SupervisedAutomatic,
            ATOSwitch_Automatic,
            ATOSwitch_Illegal
            );

T_ComATOLCSStatus = (
            ComATOLCS_NotOk,
            ComATOLCStoATO_Ok,
            ComATOLCSfromATO_Ok,
            ComATOLCS_Ok
            );

T_BrakeTestStatus = (btsUndefined,
                    btsInProgress,
                    btsAborted,
                    btsFailed,
                    btsSuccess);

//*****************************************************
// Message type enum
//*****************************************************
TMessageType = (mtUndefined,             // 0      Message Types
                mtATPModes_States,       // 1      as defined in  MMI
                mtSpare2,                // 2      External interface spec
                mtDriverInfo,            // 3
                mtSpare4 ,               // 4
                mtSpeed_Distance,        // 5
                mtSpare6,                // 6
                mtSpare7,                // 7
                mtTrnConfigData,         // 8
                mtSpare9,                // 9
                mtSpare10,              // 10
                mtSpare11,               // 11
                mtSpare12,               // 12
                mtManualConfigSelected,  // 13
                mtSpare14,               // 14
                mtSpare15,               // 15
                mtSpare16,               // 16
                mtSpare17,               // 17
                mtTime,                  // 18
                mtVehicleData,           // 19
                mtTrnVSTrackDirWanted,   // 20
                mtSpare21,               // 21
                mtMMIStatus,             // 22
                mtSpare23,               // 23
                mtSpare24,               // 24
                mtSpare25,               // 25
                mtBoolConfirmation,      // 26 Renamed not  to interfere with delphi
                mtDriverID_Password,     // 27
                mtSpare28,          // 28
                mtSpare29,               // 29
                mtSpare30,               // 30
                mtSpare31,               // 31
                mtSpare32,               // 32
                mtLocVSTrnDir,           // 33
                mtMMIToATP_Data,         // 34
                mtSpare35,               // 35
                mtSubmenuButton,         // 36
                mtSpare37,               // 37
                mtSpare38,               // 38
                mtTrnVSTrackDir,         // 39
                mtErasePlanningArea,     // 40
                mtSpare41,               // 41
                mtTextMessage,           // 42
                mtSpare43,               // 43
                mtSpare44,               // 44
                mtSpare45,               // 45
                mtCeilingSpeedList,      // 46
                mtGradientDataList,      // 47
                mtMMIStartUp,            // 48
                mtRegistrationArea,      // 49
                mtSpare50,               // 50
                mtReRegSelected,         // 51
                mtTrainName,             // 52
                mtRadioArea,             // 53
                mtLocationData,          // 54
                mtTrainLoaded,           // 55
                mtSpare56,               // 56
                mtPredefinedTextMsg,     // 57
                mtAtpNotification,       // 58
                mtTypicalConfig,         // 59
                mtStartUpHistory,        // 60
                mtVersion,               // 61
                mtAreaRequest,           // 62
                mtVehicleTypes,          // 63
                mtETARequest,            // 64
                mtTrainWeight            // 65
                );


//*****************************************************
// Message specific Enum declarations and other enum
//*****************************************************
TLogin = (lLoginName,lLoginPassword) ;

                                  { Train Configuration substates }
TConfigSubState = (cssUndefined,
               cssManual,
               cssReReg,
               cssRestartConfig,
               cssAutomatic);

                                  { PowerUp substates }
TPowerUpSubState = (pssUndefined,
               pssSelectMode);



TCarPlatformStatus =(cpsPlatformDownUnloaded,               // Car Platform status, used
                     cpsPlatformDownLoaded,            // in CarStatusData message
                     cpsPlatformGoingup,
                     cpsPlatformUp,
                     cpsPlatformGoingDown) ;

TCeilingSpeedReason = (ssrUndefined,             // Different reasons for ceiling
                      ssrPointStraight,          // speed changes in D area
                      ssrPointCurve,             // used in Ceilingspeed message
                      ssrPointPassed,            // make sure to update
                      ssrLocation,               // TCeilingReasonStr if fiddle here
                      ssrTSR,
                      ssrRestrictiveSection,
                      ssrEndOfMa,
                      ssrCondTarget,
                      ssrPantoShiftToNone,
                      ssrPantoShiftToRoof,
                      ssrPantoShiftToSide,
                      ssrOther,
                      ssrMax);

 //Adaptation Ceiling Speed reason reserved from 129 - 253

TAdapCeilingSpeedReason = (ssrLevelCrossing);


TButtonStatus = ( bsUndefined,
                  bsBrakeRelease,
                  bsTIMSInhibit,
                  bsTIMSResume,
                  bsTrainConfig,
                  Spare5,
                  bsHandlingDone,
                  bsEnterYardMode,
                  bsManualIntegrity,
                  bsConfirmIntegrity,
                  bsCancelIntegrity,
                  bsRetryConfig,
                  bsAbortSetup,
                  bsLogOut,
                  bsPossession,
                  bsShunting,
                  bsSpare16,
                  bsStartBrakeTest,
                  bsAbortBrakeTest,
                  bsStartBTMTest,
                  bsAbortLastCarBrakePressureTest,
                  bsConfirmYard,
                  bsConfirmShuntingRoute,
                  bsConfirmSR,
                  bsConfirmJoin,
                  bsConfirmSleep,
                  bsConfirmSplit,
                  bsConfirmDeparture,
                  bsAcceptAutomaticConfiguration,
                  bsRequestFreeRolling,
                  bsConfirmFreeRollingCleared,
                  bsConfirmStaffRespMAAccept,
                  bsConfirmShuntRouteMAAccept,
                  bsConfirmJoinMAAccept,
                  bsCancelRegArea,
                  bsConfirmChangeOfTrainLoadedStatus,
                  bsCancelChangeOfTrainLoadedStatus,
                  bsConfirmAbortLastCarBrakePressureTest,
                  bsCancelAbortLastCarBrakePressureTest,
                  bsConfirmTachometer1Failure,
                  bsConfirmTachometer2Failure,
                  bsConfirmDopplerFailure);

TSubmenuButton = (smbMainmenu,
                  smbTimeAndDate,
                  smbParameters,
                  smbMMIDiagnostics,
                  smbIOAccess,
                  smbTICAccess,
                  smbExit);


TCarBrakeCmd = (cbcRelease, cbcApply); //konedlu 2002-09-09

TTrackDirection =( tdUndefined,
                   tdForwardDrivingForward,
                   tdReverseDrivingForward,
                   tdForwardDrivingReverse,
                   tdReverseDrivingReverse );

TPermittedDrivingDirection =(pddUndefined,
                             pddNone,
                             pddForward,
                             pddReverse,
                             pddBoth);

TActualDrivingDirection =(addUndefined,
                             addNone,
                             addForward,
                             addReverse);

TLocomotiveConnected = (lcUndefined,lcAEnd, lcBEnd) ;

TTachoConfiguration = (tcUndefined,        //konedlu 2002-09-10
                       tcThreePhase,
                       tcTwoPhase,
                       tcTwoPlusOnePhase);

TCarBlockType = Record
                  Name : String;
                  ImagePath : String;
                  Other: Boolean;
                End;

TCarBlockTypes = Array [1..MAX_VEHICLE_TYPE_BLOCKS] of TCarBlockType;


TCarBlockColumns = (CarBlockColumnCount,
                    CarBlockColumnIcon,
                    CarBlockColumnTypeOfCar);

TCustomImage =  Record
                  ImagePath : String;
                  HiResImagePath : String;
                End;



//*****************************************************
// Misc
//*****************************************************

TCarStatus = Record
               NodeAddress:    Word;
               Weight:         Longint;
               TICStatus:      Word;
               PlatformStatus: TCarPlatformStatus;
               VehicleType:    byte;
             end;

TCarEntry = record
              ID : AnsiString;
              Information : TCarStatus ;
            End;

TCarBlockEntry = record
                  Count: Word;
                  ID : AnsiString;
                  VehicleType:    byte;
                 end;

TCarBlockListRecord = array [1..CAR_BLOCK_LIST_MAX] of TCarBlockEntry ;

TCarBlocksRecord = Record
             NumberofCarBlocks : integer ;
             CarBlocks : TCarBlockListRecord ;
             End;


TCarListRecord = array [1..CAR_LIST_MAX] of TCarEntry ;

TCarRecord = Record
             NumberofCars : Word ;
             Car : TCarListRecord ;
             End;

{TErrorSet = set of TERRORGrade ;}

{*******************************}
{* Configuration mode substate *}
{*******************************}

// TextType, texts that should be displayed on the MMI
// taken from COM and adapted to delphi original name: texttype
TTexttypeEnum =(
    UndefText ,      // The enumeration starts with
    EmergencyBrakeError ,
    ATP_WheelSizeError,
    TT_RcondError,
    TachometerError,
    BaliseAntennaError,
    SpeedometerError,
    BaliseError,
    ATP_SW_Error,
    DX_Error,
    VDX_Error,
    MMI_Error,
    TargetPassed,
    MA_TimeOut,
    TT_SensorError,
    TT_LastCar,
    TT_EMSButton,
    TT_Pantograph,
    TT_ATO_SW_Error,
    TT_RollAwayProtection,
    TT_ReversingProtection,
    TT_FlashLamp,
    TT_Config,
    TT_EBApplied,
    TT_LCSError,
    TT_Spare5,
    TT_Spare6,
    TT_Spare7,
    TT_Spare8,
    TT_Spare9,
    TT_Spare10,
    TT_Spare11,
    TT_Spare12,
    TT_Spare13,
    TT_Spare14,
    TT_Spare15,
    TexttypeEnumMax) ;

TErrorSwitchEnum = (eseDefault,eseIni) ;

TErrorEnumToStr = Array [TTexttypeEnum, TErrorSwitchEnum] of String ;

TIfaceEnum =( ifATPModeUndef,
              ifATPModePowerUp,
              ifATPModeConfiguration,
              ifATPModeRegistration,
              ifATPModeBaliseSearch,
              ifATPModeNormal,
              ifATPMOdeShunting,
              ifATPModeLocation,
              ifATPModeYard,
              ifATPModeUnregistered,
              ifATPModePoweringDown,
              ifATPModeSafetyHalt,
              ifATPModeSleeping,
              ifATPModeStaffResponsible,
              ifATPModeShuntingRoute,
              ifATPModePossession,
              ifATPModeSplit,
              ifATPModeJoin,
              ifATPModeSafeBrakeToStop,
              ifATPStU,
              ifATPVsstU,
              ifATPBssys_str,
              ifATPApplStr,
              ifATPTst,
              ifATPStartLoco,
              ifATPDefTrn,
              ifATPRegtTrn,
              ifMMIClk,
              ATPManCnfWait,
              ifName,
              ifPsswd,
              ifLocDir,
              ifManTrnCnf,
              ifTrnReg,
              ifTRnVsTrk,
              ifLoginVer,
              ifLoginFALSE,
              ifLoginBtn,
              ifYardModeBtn,
              ifAcceptBtn,
              ifReEnterBtn,
              ifDelBtn,
              ifCloseBtn,
              ifCommLost,
              ifFatal1,
              ifFatal2,
              ifUnreg1,
              ifRetryConfig,
              ifTrain,
              ifRetryConfigCount,
              ifATPPowerDown,
              ifSystemRestart,
              ifNone,
              ifCount,
              ifTypeOfCar,
              ifStartupHistory,
              ifTime,
              ifText,
              ifLastCarNotDetected,
              ifTIMSNotAvailable,
              ifTIMSAvailable,
              ifWithoutTIMS,
              ifLeading,
              ifTrailing,
              ifConfigurationFBtn,
              ifYardModeFBtn,
              ifReadyFBtn,
              ifClearTXTBtn,
              ifUnknownTextId,
              ifAbortSetupFBtn,
              ifSleepingMsg,
              ifCarsConnBSide,
              ifLoaded,
              ifNotLoaded,
              ifMax) ;

TSwitchEnum = (ifsDefault,ifsIni) ;
TIfaceEnumToStr = Array [TIfaceEnum ,TSwitchEnum] of String ;

TSpeedReasonEnumToStr = Array [TCeilingSpeedReason,TSwitchEnum] of string ;
TAdapSpeedReasonEnumStr = Array [TAdapCeilingSpeedReason,TSwitchEnum] of string;
//*****************************************************
// Array Pointer declarations
//*****************************************************
TTrainName = Array [0..(TRAIN_NAME_MAX_CHARS-1)] of AnsiChar ;
TCarName = Array [0..(CAR_NAME_MAX_CHARS -1)] of AnsiChar ;
TMMIByteArray = Array [0..$FFFF] of Byte ;
PMMIByteArray = ^TMMIByteArray ;

        {Types used when swapping bytes for Motorola - Intel compatibility}
  TWordType = packed record
    case integer of
    1:
      (Byte0 : byte;   //LSB
       Byte1 : byte);  //MSB
    2:
      (Total : word);
    end;

  TLongWordType = packed record
    case integer of
    1:
      (Byte0 : byte;
       Byte1 : byte;
       Byte2 : byte;
       Byte3 : byte);
    2:
      (Total : LongWord);
  end;

  TSmallIntType = packed record
    case integer of
    1:
      (Byte0 : byte;   //LSB
       Byte1 : byte);  //MSB
    2:
      (Total : SmallInt);
    end;

  TCardinalType = packed record
    case integer of
    1:
      (byte0 : byte;
       byte1 : byte;
       byte2 : byte;
       byte3 : byte);
    2:
      (Total : cardinal);
    end;

  TLongintType = packed record
    case integer of
    1:
      (byte0 : byte;
       byte1 : byte;
       byte2 : byte;
       byte3 : byte);
    2:
      (Total : Longint);
    end;



//*****************************************************
// Message types declarations
//*****************************************************
TATPModes_StatesMT = Packed Record      //1
                      ATPMode : T_ATPmodes ;
                      ATPState : T_ATPStates ;
                      ATPSubState : Byte ; // Service or Config SubState
                      ATPVerification : T_ATPVerificationStates;
                      InterfaceFlags : Byte;
                      LocoStatus : LongWord ;
                      ATOMode : T_ATOModes;
                      ATOSwitch : T_ATOSwitch;
                      AdditionalStatus1 : Byte;
                      AdditionalStatus2 : Byte;
                      ConfirmModeChange : Byte;
                      AllowedButtons    : Byte;
                      BrakeTestStatus   : T_BrakeTestStatus;
                      RemainingTimeToBrakeTest : Word;
                      RemainingTimeToBTMTest : Word;
                      AdditionalAllowedToInfo : Byte;
                      AdditionalConfirmationInfo1 : Byte;
                      AdaptationTrainStatus : Byte;
                      AdditionalConfirmationInfo2 : Byte;
                      PlatformStatusInfo : Byte;

                   End;
PATPModes_StatesMT = ^TATPModes_StatesMT ;

TDriverInfoMT = Packed Record           // 3
                PermittedDrivingDirection : TPermittedDrivingDirection ;
                Pspeed :Byte ;
                Tspeed :Byte ;
                TimeToIntervention : Byte ;
                RemainingDistanceToTargetPoint : Word ;
                RemainingDistanceToBCA : Word ;
                PredictedDistanceToStandStillLocation :Word ;
                Spare :Byte ;
                StatusD11 : Byte ;
                StatusD12 : Byte ;
                ActualDrivingDirection : TActualDrivingDirection;
                MAMargin : Word ;
                Brakeability : Word;
                BrakeDelayEB : Word;
                BrakeDelaySB : Word;
                End ;
PDriverInfoMT = ^TDriverInfoMT ;

TSpeed_DistanceMT = Packed Record       // 5
                    CurrentSpeed : Word ;
                    CurrentTrackGradient : Int16;
                    leadingTrackSection : Word;
                    leadingPositionCm : LongInt;
                    trailingTrackSection : Word;
                    trailingPositionCm : LongInt;
                    CurrentOdometer : LongInt ;
                    CurrentEffectiveGradient : Int16;
                    End;
PSpeed_DistanceMT = ^TSpeed_DistanceMT ;



TTACSpeed_DistanceMT = Packed Record    // 6   1385
                       Speed : TWordType ;
                       SpeedLastPulseTime : TCardinalType ;
                       SpeedServiceAvailable : Byte ;
                       OdometerServiceAvailable : Byte ;
                       Odometer : TLongintType ;
                       OdometerLastPulseTime : TCardinalType ;
                       DrivingDirection  : Boolean ;
                       SlipSlide : Boolean ;
                       End;
PTACSpeed_DistanceMT =^TTACSpeed_DistanceMT ;

TCarListCompletedMT = Packed Record     // 7
                        Status : Byte ;
                      End;
PCarListCompletedMT = ^TCarListCompletedMT ;

TTrnConfigDataMT = Packed Record        // 8
                    TrainName : TTrainName ;
                    TrainLength: Word ;
                    Options : Byte ;
                    baliseToTrainFront : Word;
                    baliseToTrainEnd : Word;
                   End;

PTrnConfigDataMT = ^TTrnConfigDataMT ;

TCarStatusAnswerMT = Packed Record      // 9
                     CarNumber : Word ;
                     StatusBitfieldInput1 : Byte ;
                     StatusBitfieldInput2 : Byte ;
                     StatusBitfieldInput3 : Byte ;
                     StatusBitfieldInput4 : Byte ;
                     R02 : Word ;
                     End;

PCarStatusAnswerMT = ^TCarStatusAnswerMT ;

TGradientDataMT = Packed Record         // 11
                    Odometerposition : LongInt ;
                    NewGradient : Shortint ;
                  End;
PGradientDataMT = ^TGradientDataMT ;

TGradientDataList = Packed Record
                      NrOfGradientDataBlocks : Byte;
                      Gradient : Array [1..GRADIENT_MAX] of TGradientDataMT;
                    End;

PGradientDataList = ^TGradientDataList;

TLCarStatusAnswerMT = Packed Record     // 12
                      StatusBitfieldInput: Byte ;
                      End;
PLCarStatusAnswerMT =^TLCarStatusAnswerMT ;

TCeilingSpeedMT = Packed Record          // 14
                 OdometerPosition : LongInt ;
                 NewSpeed : Byte ;
                 Reason : Byte ;
                 End;
PCeilingSpeedMT = ^TCeilingSpeedMT;

TCeilingSpeedList =  Packed Record
                      NrOfSpeedDataBlocks : Byte;
                      Speed : Array [1..CEILING_SPEED_MAX] of TCeilingSpeedMT;
                    End;

PCeilingSpeedList = ^TCeilingSpeedList;

TTICAvailableAnswerMT = Packed Record   // 17
                   Status :Byte ;
                   End;
PTICAvailableAnswerMT= ^TTICAvailableAnswerMT ;

TTimeMT = Packed Record                 // 18
             TimeMostSig4Byte :UInt32 ;
             TimeLeastSig4Byte :UInt32 ;
             End;
PTimeMT = ^TTimeMT ;

TCarNameDataMT =  Packed Record          // 19
                    CarNumber : Word ;
                    CarName : Array [1..CAR_NAME_MAX_CHARS] of AnsiChar ;
                  End;

TCarNameListMT =  Packed Record          // 19
                    NrOfDataBlocks : Word;
                    Entry : Array [1..CAR_LIST_MAX] of TCarNameDataMT;
                  End;

PCarNameListMT = ^TCarNameListMT ;

TCarNameListMsg =
                Packed Record
                  MT : Byte;
                  NamesToSend : TCarNameListMT;
                End;

PCarNameListMsg = ^TCarNameListMsg ;


TTrainNameDataMT = Packed Record
               TrainName : Array [1..TRAIN_NAME_MAX_CHARS] of AnsiChar ;
               end;
PTrainNameMT = ^TTrainNameDataMT ;

{Radi Area}
TRadioAreaDataMT = Packed Record
               RadioArea : Array [1..RADIO_AREA_MAX_CHARS] of AnsiChar ;
               end;
PRadioAreaMT = ^TRadioAreaDataMT ;


TMMITimeMT = Packed Record              // 21
          Time : LongInt ;
          End;
PMMITimeMT = ^TMMITimeMT ;

TMMIStatusMT = Packed Record            // 22
             MMIStatus : Word;
             End;
PMMIStatusMT = ^TMMIStatusMT ;

TMMIStartUpMT = Packed Record
              MMIStatus : Word;
              CompatibilityVersion : Byte;
             End;
PMMIStartUpMT = ^TMMIStartUpMT ;

TCarListDataBlock = Packed Record
                      CarId : Word;
                      VehicleType : Byte;
                    End;

TCarListAnswerMT =  Packed Record
                      AcceptStatus : Byte;
                      NrOfCars : Word;
                      DataBlock : Array[1..CAR_LIST_MAX] Of TCarListDataBlock;
                    End;

PCarListAnswerMT = ^TCarListAnswerMT;           // 23

TCarListAnswerMsg =  Packed Record
                      MT : Byte;
                      Msg : TCarListAnswerMT;
                    End;

PCarListAnswerMsg = ^TCarListAnswerMsg;           // 23


TConfirmationMT = Packed Record          // 26
                  Confirmation : Byte ;
                  End;
PConfirmationMT = ^TConfirmationMT ;

TDriverID_PasswordMT = Packed Record     // 27
                       DriverID: array [1..DRIVER_MAX_CHARS] of AnsiChar ;
                       Password : array [1..PASSWORD_MAX_CHARS] of AnsiChar ;
                       End;
PDriverID_PasswordMT = ^TDriverID_PasswordMT ;

TGetCarStatusMT = Packed Record          // 28
                  CarNumber : Word ;
                  End;
PGetCarStatusMT = ^TGetCarStatusMT ;


TLocoVSTrnDirMT = Packed Record           // 33
                 LocoVSTrainDir : TLocomotiveConnected ;
                 End;
PLocoVSTrnDirMT = ^TLocoVSTrnDirMT ;

TMMIToATP_DataMT = Packed Record         // 34
                   ButtonStatus : TButtonStatus ;
                   End;
PMMIToATP_DataMT = ^TMMIToATP_DataMT ;

TSubmenuButtonMT = Packed Record         // 36
                   Button : TSubmenuButton
                   End;
PSubmenuButtonMT = ^TSubmenuButtonMT ;


TTrnVSTrackDirMT = Packed Record         // 39
                   TrackDirection : TTrackDirection ;
                   End;
PTrnVSTrackDirMT = ^TTrnVSTrackDirMT ;

TTextMessageMT = Packed Record           // 42
                 ConfirmationRequested : Boolean ;
                 Text : array [1..20] of AnsiChar ;
                 End;
PTextMessageMT = ^TTextMessageMT ;

TRegistrationAreaMT = Packed Record      // 49
                 AreaId : Byte;
                 End;
PRegistrationAreaMT = ^TRegistrationAreaMT;

TTrainLoadedMT = Packed Record      // 55
                 TrainLoaded : Byte;
                 End;
PTrainLoadedMT = ^TTrainLoadedMT;


TAtpVehicleTypeBlock = Packed Record
   VehicleType : Byte;
   VehicleTypeName : array [1..VEHICLE_TYPE_NAME_MAX_CHARS] of AnsiChar ;
end;

TAtpVehicleTypes = Packed Record
   NrOfVehicleTypesBlocks : Byte;
   VehicleTypeBlocks : array [1..MAX_VEHICLE_TYPE_BLOCKS] of TAtpVehicleTypeBlock;
end;

PAtpVehicleTypes = ^TAtpVehicleTypes;


TTypicalConfigBlock = Packed Record
                      TypicalConfigName : array[1..TYPICAL_CONFIG_NAME_MAX_CHARS] of AnsiChar;
                      VehicleTypeName : array[1..VEHICLE_TYPE_NAME_MAX_CHARS] of AnsiChar;
                      CarsinTypicalConfig : Byte;
                      End;

TTypicalConfigMT = Packed Record    //59
                    NrofTypicalConfigBlocks : Byte;
                    TypicalConfigBlocks : array[1..MAX_TYPICAL_CONFIG_BLOCKS] of TTypicalConfigBlock;
                    End;

PTypicalConfigTypes = ^TTypicalConfigMT;

TStartUpHistoryMT = Packed Record           // 60
                 TextLength : Word;
                 Text : array [1..MMI_DATA_MAX_SIZE] of AnsiChar ;
                 End;
PStartUpHistoryMT = ^TStartUpHistoryMT ;

TVersionMT =  Packed Record           // 61
                      CompatibilityVersion : Byte;
                    End;

PVersionMT = ^TVersionMT;           // 61

TAreaRequestMT =   Packed Record     // 62
                  AreaIdCount : Byte;
                  AreaId : Array [1..MAX_AREA_ID_COUNT] Of Byte;
                  End;

PAreaRequestMT = ^TAreaRequestMT;


TCarStatusDataMT = Packed Record        // 44
                   CarNumber : Word ;
                   LoadWeight : Word ;
                   TICStatus : Word ;
                   PlatformStatus: TCarPlatformStatus ;
                   End;
PCarStatusDataMT = ^TCarStatusDataMT ;

TCarStatusList =    Packed Record
                      AcceptStatus : Byte;
                      NrOfCarStatusDataBlocks : Word;
                      Status : Array [1..CAR_LIST_MAX] of TCarStatusDataMT;
                    End;

PCarStatusList = ^TCarStatusList;

PLogDataMT = PMMIByteArray;

TLogVersionMT = Packed Record
                projectNbr: byte;
                versionNbr: byte;
                newLogFile: boolean;
                end;
PLogVersionMT = ^TLogVersionMT;


TCarBrakeCmdMT = Packed Record
                 carNumber: Word;
                 command: TCarBrakeCmd;
                 end;
PCarBrakeCmdMT = ^TCarBrakeCmdMT;

TPredefinedTextMsgMT = Packed Record   //konedlu 2002-09-23
                       confirmationRequested: Boolean;
                       messageSpecifier: Byte;
                       end;
PPredefinedTextMsgMT = ^TPredefinedTextMsgMT;

TAtpNotificationMT = Packed Record   // sigjkg 2004.06.17
                     mode: byte;
                     Text : array [1..ATP_NOTIFICATION_MAX_CHARS] of AnsiChar ;
                     end;
PAtpNotificationMT = ^TAtpNotificationMT;

TReRegSelectedMT = Packed Record       // 5
                        Options : Byte ;
                      End;
PReRegSelectedMT = ^TReRegSelectedMT ;





TAtpVehicleDataBlock = Packed Record
   NrOfVehicleInBlock : Word;
   VehicleType : Byte;
   VehicleNodeId : Word;
   VehicleName : array [1..VEHICLE_NAME_MAX_CHARS] of AnsiChar ;
end;

TAtpVehicleData = Packed Record
   NrOfVehicleDataBlocks : Word;
   VehicleDataBlocks : array [1..MAX_VEHICLE_DATA_BLOCKS] of TAtpVehicleDataBlock;
end;

PAtpVehicleData = ^TAtpVehicleData;

TVehicleDataMsg =
                Packed Record
                  MT : Byte;
                  NewVehicleData : TAtpVehicleData;
                End;

PVehicleDataMsg = ^TVehicleDataMsg ;

TAtpLocationData = Packed Record
   LocationName : array [1..VEHICLE_NAME_MAX_CHARS] of AnsiChar ;
   LocationType : Integer;
End;

PAtpLocationData = ^TAtpLocationData;

TETAMT = Packed Record              // 64
          ETAMostSig4Byte : UInt32;
          ETALeastSig4Byte : UInt32;
         End;
PETAMT = ^TETAMT ;

TTrainWeightMT = Packed Record           // 65
                 TrainWeight : LongWord;
                 End;
PTTrainWeightMT = ^TTrainWeightMT ;


//*****************************************************
// Text area list entries
//*****************************************************


TTextEntry =  Packed Record
                Time : TDateTime ;
                Confirm : Boolean ;
                Error : Boolean ;
                Valid : Boolean ;
                Text : String[TEXT_MESSAGE_MAX_CHARS_TRANSLATED] ;
              End;
PTextEntry = ^TTextEntry;

{*****************************************************************************
* CONST DECLARATIONS                                                         *
*****************************************************************************}
Const

    { Interface flags }

  RECONFIG_TIMS_REQUIRED              : Byte = $01;
  CARS_CONNECTED_AT_A_END             : Byte = $02;
  ACCEPT_WITHOUT_TIMS                 : Byte = $01;
  RADIO_ACTIVE                        : Byte = $01;
  TIMS_AVAILABLE                      : Byte = $02;
  TIMS_OK                             : Byte = $04;
  ODOMETER_INVALID                    : Byte = $08;
  STOP_TRAIN_REQUEST                  : Byte = $10;
  CONFIGURATION_REJECTED              : Byte = $20;
  TRAIN_INTEGRITY_GRANTED_BY_DRIVER   : Byte = $40;
  TRAIN_INTEGRITY_CONFIRMED           : Byte = $80;

  //Data 11, Additional Status bit
  TRAIN_LOADED                        : Byte = $40;
  IN_APPROACH_AREA_FOR_LEVEL_CROSSING : Byte = $80;

  // Data 13 , Confirm
  CONFIRM_YARD                               = $01;
  CONFIRM_SHUNTING_ROUTE                     = $02;
  CONFIRM_STAFF_RESPONSIBLE                  = $03;
  CONFIRM_JOIN                               = $04;
  CONFIRM_SLEEP                              = $05;
  CONFIRM_SPLIT                              = $06;

  // Data 14 , Allowed buttons
  ALLOWED_TO_LOGIN                    : Byte = $01;
  ALLOWED_TO_ENTER_YARD_MODE          : Byte = $02;
  ALLOWED_TO_ENTER_POSSESSION_MODE    : Byte = $04;
  ALLOWED_TO_ENTER_SHUNTING_MODE      : Byte = $08;
  ALLOWED_TO_ENTER_CONFIG_MODE        : Byte = $10;
  ALLOWED_TO_CHANGE_TRAIN_NAME        : Byte = $20;
  ALLOWED_TO_ABORT_SETUP              : Byte = $40;
  ALLOWED_TO_LOGOUT                   : Byte = $80;

  //Data 20, Additional Allowed Buttons
  ALLOWED_TO_SHOW_TRAIN_COMP          : Byte = $01;
  ALLOWED_TO_SHOW_FREE_ROLLING        : Byte = $02;
  ALLOWED_TO_SHOW_HANDLING_DONE       : Byte = $04;
  ALLOWED_TO_SHOW_CANCEL_REG_AREA     : Byte = $08;
  ALLOWED_TO_SHOW_RADIO_AREA          : Byte = $10;
  ALLOWED_TO_SELECT_CARS_CONNECTED_ON_A_B_SIDE : Byte = $20;
  ALLOWED_TO_INHIBIT_TRAIN_INTEGRITY  : Byte = $40;
  ALLOWED_TO_RESUME_TRAIN_INTEGRITY   : Byte = $80;

  //Data 21, Additional Confirm bit information
  CONFIRM_DEPARTURE_TEST              = $01;
  CONFIRM_STAFF_RESP_MA               : Byte = $02;
  CONFIRM_SHUNITNG_ROUTE_MA           : Byte = $04;
  CONFIRM_ABORT_LAST_CAR_BRAKE_TEST   : Byte = $08;
  CONFIRM_FREE_ROLLING_CLEARED        : Byte = $10;
  CONFIRM_JOIN_MA                     : Byte = $20;
  CONFIRM_CONFIRM_TIMS_INTEGRITY      : Byte = $40;
  CONFIRM_TRAIN_LOADED_STATUS_CHANGE  : Byte = $80;

  //Data 23, Additional Confirm odometer bit information
   CONFIRM_TACHOMETER1_FAILURE        :Byte  = $01;
   CONFIRM_TACHOMETER2_FAILURE        :Byte  = $02;
   CONFIRM_DOPPLER_FAILURE            :Byte  = $04;

   //Data 24, Additional Platform Indications
   TACHOMETER1_FAILURE                 :Byte = $01;
   TACHOMETER2_FAILURE                 :Byte = $02;
   DOPPPLER_FAILURE                    :Byte = $04;


  // Secsperday declared in sysutils
   SecsPerHour            = 60 * 60 ;
   SecsPerMinute          = 60 ;
   UnixOffset : TDateTime = 25569 ;     // offset between delphi start time 1899 12/31
                                       // and Unix time 1970 1/1 (days between)

   MAX_EVENT_LOG_BUFFER = 4000;  //(konedlu 2002-05-21)


  { MMI Status, Module }
   MS_SW_ERROR          : Word = $0004 ;

   MS_MAIN              : Word = $0010 ;
   MS_SER               : Word = $0020 ;
   MS_YARDMODE          : Word = $0030 ;
   MS_ATRN_CNF          : Word = $0040 ;
   MS_MTRN_CNF          : Word = $0050 ;
   MS_COMM_LOST         : Word = $0060 ;
   MS_LHANDLING         : Word = $0070 ;
   MS_PDOWN             : Word = $0080 ;
   MS_FULLSCREEN        : Word = $0090 ;
   MS_FATAL_ERROR       : Word = $00A0 ;
   MS_RE_CNF            : Word = $00B0 ;
   MS_LOGIN             : Word = $00C0 ;
   MS_KEYB              : Word = $00D0 ;
   MS_NUMKEYB           : Word = $00E0 ;
   MS_UNREG             : Word = $00F0 ;
   MS_Undefined         : Word = $0F00 ;

  {Error in MMI Status}
  SER_INIT_ERR  :Byte= $71 ;
  SER_EXIT_ERR  :Byte= $72 ;
  SER_POLL_ERR  :Byte= $73 ;
  SER_DETECT_ERR:Byte= $74 ;



  { Driver info}
  INBCA :                  Byte = $01 ;
  ATPWARNING:              Byte = $02 ;
  ATPINTERVENTION:         Byte = $04 ;
  FLASHSBBRAKEBUTTON:      Byte = $08 ;
  RADIOAVAILABLE:          Byte = $10 ;
  SERVICEBRAKEAREAPPLIED : Byte = $20 ;
  EMERGENCYBRAKEAREAPPLIED: Byte = $40;
  FLASHEBBRAKEBUTTON:      Byte = $80;
  {Driver info}
  INDPERMDIR:              Byte = $01 ;
  INDPERMSPEED:            Byte = $02 ;
  INDTARGETSPEED:          Byte = $04 ;
  INDREMDISTTOTARGETPOINT: Byte = $08 ;
  INDREMDISTTOBCA:         Byte = $10 ;
  INDPREDDISTTOSSLOC:      Byte = $20 ;
  INDPREDSPEEDATBRKTRGT:   Byte = $40 ;
  INDTIMETOINT:            Byte = $80 ;


  {TCarListCompleStatus}
   Cars_connected :        Byte = $00 ;
   No_cars_connected :     Byte = $FF ;


  {Car Status Data }
  TIC_FrontRightDerail: Word = $0001 ;
  TIC_FrontLeftDerail:  Word = $0002 ;
  TIC_RearRightDerail:  Word = $0004 ;
  TIC_RearLeftDerail:   Word = $0008 ;
  TIC_CarDumpBottom:    Word = $0010 ;
  TIC_CarDumpTop:       Word = $0020 ;
  TIC_CarDumpClosed:    Word = $0040 ;
  TIC_LoadWeightBad:    Word = $0100 ;
  TIC_DerailInhibit:    Word = $0200 ;
  TIC_Derail:           Word = $0400 ;
  TIC_PlatformError:    Word = $0800 ;

  { LocomotiveStatus }
   LS_SafetyHalt:                 LongWord = $00001 ;
   LS_EmergencyAlertFromDriver:   LongWord = $00002 ;
   LS_TIMS_IntegrityBroken:       LongWord = $00004 ;
   LS_BrakingEvent:               LongWord = $00008 ;
   LS_HandlingDone:               LongWord = $00010 ;
   LS_TrainIdling:                LongWord = $00020 ;
   LS_TIMS_DisablebyDriver:       LongWord = $00040 ;
   LS_MA_TimeOut:                 LongWord = $00080 ;
   LS_ATP_Reset:                  LongWord = $00100 ;
   LS_ATP_Needs_Reset:            LongWord = $00200 ;
   LS_ATPIntervention:            LongWord = $00400 ;
   LS_BrakeReleaseWanted:         LongWord = $00800 ; { Brake release wanted from dispatcher }
   LS_ManualTIMS_Confirmation:    LongWord = $01000 ;
   LS_SlipDetected:               LongWord = $02000 ;
   LS_FreeRolling:                LongWord = $04000 ;
   LS_EmergencyAlertActive:       LongWord = $08000 ;
   LS_ATO_AttentionNeeeded:       LongWord = $10000 ;
   LS_ATO_NotReadyToDrive:        LongWord = $20000 ;
   LS_ATO_SafeForBoarding:        LongWord = $40000 ;

   {Additional status-bits 1}
   AS_ATO_ENABLE:           Byte = $01;
   AS_DrivingForward:       Byte = $02;
   AS_StandStillEvent:      Byte = $04;
   AS_BrakeTestPossible:    Byte = $08;
   AS_BrakeTestNotification:Byte = $10;
   AS_BrakeTestMandatory:   Byte = $20;

   {Additional status-bits 2}

   AS2_ATOLCSStatus:        Byte = $03;
   AS2_LastCarBrakeTest:    Byte = $04;
   AS2_BTMTestPossible:     Byte = $08;
   AS2_BTMTestNotification: Byte = $10;
   AS2_BTMTestMandatory:    Byte = $20;
   AS2_BrakeSystemPB:       Byte = $40; //Pneumatic Brakin System
   AS2_BrakeSystemECPB:     Byte = $80; //Electronically controlled brake system
   AS2_BrakeSystemType3:    Byte = $C0;

   {Vehicle types}                   //konedlu 2002-10-01
   vtUndefinedCar      = 0;
   vtAutoSideTiltCar   = 1;
   vtTicCar            = 2;
   vtBottomDumpCar     = 3;
   vtPassengerCar      = 4;
   vtGeneralCar        = 5;
   vtInterflowCar      = 101;
   vtInterflowAtoCar   = 102;
   vtInterflowMotorCar = 103;


      { WriteMMIStatus/Log error codes }
  MainLogErrorFormCreate  = 1;
  MainLogErrorFormShow    = 2;
  MainLogErrorMMIInit     = 3;
  MainLogErrorFormDestroy = 4;
  MainLogErrorSetRadioActive= 5;
  MainLogErrorSetMMITime  = 6;
  MainLogErrorUpdateTime  = 7;
  MainLogErrorTIMEREvent  = 8;
  MainLogErrorFlashControl= 9;
  MainLogErrorFlashBrakeIntervention = 10;
  MainLogErrorTimeFieldClick= 11;
  MainLogErrorSetMMI_ATPModeAndState=12;
  MainLogErrorLocoStatus = 13;
  MainLogErrorSETasPowerDown = 14;
  MainLogErrorSETasSystemRestart = 15;
  MainLogErrorSETUndefined  = 16;
  MainLogErrorSETasBasicsSystemStartUp = 17;
  MainLogErrorSETasInactive = 18;
  MainLogErrorSETavsNoActionState = 19;
  MainLogErrorSETavsInputState = 20;
  MainLogErrorSETavsverificationState = 21;
  MainLogErrorSETavsRedoinputState = 22;
  MainLogErrorSETasActivationTest = 23;
  MainLogErrorSETcssAutomatic = 24;
  MainLogErrorSETcssManual = 25;
  MainLogErrorSETcssReConfig = 26;
  MainLogErrorSETamTrainregistration = 27;
  MainLogErrorSETamFullATP = 28;
  MainLogErrorSETamManualLocation = 30;
  MainLogErrorSETamManualLocationUnload = 31;
  MainLogErrorSETFatalFailure = 32;
  MainLogErrorSetUnregistered = 33;
  MainLogErrorSETamYardMode = 34;


//**************************************************************************
// Interface strings
//**************************************************************************

TInterfaceString :TIfaceEnumToStr =
                (('Undef','ATPModeUndef'),
                 ('PowerUp','ATPModePowerUp'),
                 ('Config','ATPModeConfiguration'),
                 ('Reg','ATPModeRegistration'),
                 ('B Search','ATPModeBaliseSearch'),
                 ('Normal','ATPModeNormal'),
                 ('Shunting','ATPModeShunting'),
                 ('Location','ATPModeLocation'),
                 ('Yard','ATPModeYard'),
                 ('Unreg','ATPModeUnregistered'),
                 ('PowerDown','ATPModePoweringDown'),
                 ('SafetyHalt','ATPModeSafetyHalt'),
                 ('Sleep','ATPModeSleeping'),
                 ('Staff R','ATPModeStaffResponsible'),
                 ('Shunting Rt','ATPModeShuntingRoute'),
                 ('Possession','ATPModePossession'),
                 ('Split','ATPModeSplit'),
                 ('Join','ATPModeJoin'),
                 ('SafeBrkToStop','ATPModeSafeBrakeToStop'),
                 ('ATP-tillstånd ej definierat','ATPStU'),
                 ('ATP inloggning i ej definierat tillstånd','ATPVsstU') ,
                 ('ATP system startar, vänta ...','ATPBssys_str'),
                 ('ATP applikationen startar, vänta ...','ATPApplStr'),
                 ('ATP testar delsystem, vänta ...','ATPTst'),
                 ('Var vänlig starta upp loket ...','ATPStartLoco'),
                 ('ATP undersöker tågkonfigurationen, vänta ...','ATPDefTrn'),
                 ('ATP registrerar tåget hos TCC, vänta ...','ATPRegtTrn'),
                 ('MMI-klockan uppdaterad','MMIClk'),
                 ('Press C for Mannual Configuration...','ATPManCnfWait'),
                 ('Namn','Name'),
                 ('Passord','Psswd'),
                 ('Vagnarna anslutna','LocDir'),
                 ('Manuell tågkonfiguration','ManTrnCnf'),
                 ('Tågregistrering','TrnReg'),
                 ('Tågets orientering i layouten','TRnVsTrk'),
                 ('Verifierar lösenord och id, vänta ....','LoginVer'),
                 ('Fel ID eller lösenord ! Försök igen','LoginFALSE'),
                 ('Login','LoginBtn'),
                 ('Yard Mode','YardModeBtn'),
                 ('Acceptera','AcceptBtn'),
                 ('UnderKänn','ReEnterBtn'),
                 ('Del','Delbtn'),
                 ('&Close','CloseBtn'),
                 ('Kommunikation med ATP nere !','CommLost'),
                 ('Safety Monitor Halt','Fatal1'),
                 ('Please Restart ATP','Fatal2'),
                 ('Unregistration received','Unreg1'),
                 ('Rejected, retry!','RetryConfig'),
                 ('Train', 'Train'),
                 ('Retry with no of cars in the range 0..', 'RetryConfigCount'),
                 ('ATP Power down in progress...', 'ATPPowerDown'),
                 ('System restart in progress...', 'SystemRestart'),
                 ('None', 'None'),
                 ('Count', 'Count'),
                 ('Type of car', 'TypeOfCar'),
                 ('Startup history', 'StartupHistory'),
                 ('Time', 'Time'),
                 ('Text', 'Text'),
                 ('Last car not detected!','LastCarNotDetected'),
                 ('TIMS not available!','TIMSNotAvailable'),
                 ('TIMS Available!','TIMSAvailable'),
                 ('without TIMS','WithoutTIMS'),
                 ('Leading','Leading'),
                 ('Trailing','Trailing'),
                 ('C','ConfigurationFBtn'),
                 ('Y','YardModeFBtn'),
                 ('R','ReadyFBtn'),
                 ('C','ClearTXTBtn'),
                 ('Unknown text:', 'UnknownTextId'),
                 ('X','AbortSetup'),
                 ('ATP Sleeping','SleepingMsg'),
                 ('Cars connected B Side?','CarsConnBSide'),
                 ('Loaded', 'Loaded'),
                 ('Not loaded','NotLoaded'),
                 ('',''));

TSpeedReasonStr: TSpeedReasonEnumToStr =
               (('Undefined','srUndef'),
                ('Point Straight','srPointStraight'),
                ('Point Curve','srPointCurve'),
                ('Point Passed','srPointPassed'),
                ('Location','srLocation'),
                ('Men at work','srTSR'),
                ('Restrictive Section','ssrRestrictiveSection'),
                ('End of MA','srEndOfMa'),
                ('Conditional target','srCondTarget'),
                ('Panto shift to none','srPantoShiftToNone'),
                ('Panto shift to roof','srPantoShiftToRoof'),
                ('Panto shift to side','srPantoShiftToSide'),
                ('Other','srOther'),
                ('',''));

TAdapSpeedReasonStr: TAdapSpeedReasonEnumStr = (('Approach Area for Level Crossing','srLevelCrossing'));

TErrorString :TErrorEnumToStr =
              (('UndefText ',           'UndefText'),
               ('EmergencyBrake Error', 'EmergencyBrakeError'),
               ('ATP WheelSize Error ', 'ATP_WheelSizeError'),
               ('Radio Com Error ',     'TT_RcondError'),
               ('Tachometer Error',     'TachometerError'),
               ('BaliseAntenna Error',  'BaliseAntennaError'),
               ('Speedometer Error',    'SpeedometerError'),
               ('Balise Error',         'BaliseError'),
               ('ATP SW Error ',        'ATP_SW_Error'),
               ('DX Error',             'DX_Error'),
               ('VDX Error',            'VDX_Error'),
               ('MMI Error ',           'MMI_Error'),
               ('Target Passed',        'TargetPassed'),
               ('MA TimeOut',           'MA_TimeOut'),
               ('Sensor Error',         'TT_SensorError'),
               ('LastCar Error',        'TT_LastCar'),
               ('EMSButton',            'TT_EMSButton'),
               ('Pantograph Error',     'TT_Pantograph'),
               ('ATO/Pantograph Error', 'TT_ATO_SW_Error'),
               ('RollAway!',            'TT_RollAwayProtection'),
               ('Reversing!',           'TT_ReversingProtection'),
               ('Last Car FlashLamp Err', 'TT_FlashLamp'),
               ('Err config param',     'TT_Config'),
               ('Emergency brake detected', 'TT_EBApplied'),
               ('LCS Error',            'TT_LCSError'),
               ('Spare5',               'TT_Spare5'),
               ('Spare6',               'TT_Spare6'),
               ('Spare7',               'TT_Spare7'),
               ('Spare8',               'TT_Spare8'),
               ('Spare9',               'TT_Spare9'),
               ('Spare10',               'TT_Spare10'),
               ('Spare11',               'TT_Spare11'),
               ('Spare12',               'TT_Spare12'),
               ('Spare13',               'TT_Spare13'),
               ('Spare14',               'TT_Spare14'),
               ('Spare15',               'TT_Spare15'),
               ('',''));


{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}
var
  CarBlockTypes : TCarBlockTypes;
  CarBlockTypeOther : Integer; { Index of 'Other' car type }
  CarBlockTypeStandard : Integer;

  ImageCarsConnectedA : TCustomImage;
  ImageCarsConnectedB : TCustomImage;

  ImageLocoVSTrackAForward  : TCustomImage;
  ImageLocoVSTrackAReverse  : TCustomImage;
  ImageLocoVSTrackBForward  : TCustomImage;
  ImageLocoVSTrackBReverse  : TCustomImage;
  ImageTrainVSTrackAForward : TCustomImage;
  ImageTrainVSTrackAReverse : TCustomImage;
  ImageTrainVSTrackBForward : TCustomImage;
  ImageTrainVSTrackBReverse : TCustomImage;
  ImageTrainVSTrackAForwardCarAtA : TCustomImage;
  ImageTrainVSTrackAReverseCarAtA : TCustomImage;
  ImageTrainVSTrackBForwardCarAtA : TCustomImage;
  ImageTrainVSTrackBReverseCarAtA : TCustomImage;
  ImageConfiguration : TCustomImage;
  ImageYardMode      : TCustomImage;
  ImageReady         : TCustomImage;
  ImageClear         : TCustomImage;
  ImageATOUndefined  : TCustomImage;
  ImageATOManual     : TCustomImage;
  ImageATOSupervised : TCustomImage;
  ImageATOAutomatic  : TCustomImage;
  ImageATORemote     : TCustomImage;
  ImageTrainLoaded   : TCustomImage;
  ImageBrakeSystemECPB   : TCustomImage;
  ImageBrakeSystemPB   : TCustomImage;
  ImageLCSCommDisconnected: TCustomImage;
  ImageOdometerFailure  : TCustomImage;
  FInterfaceString    : TStringList ;
  FPredefTextMsgString: TStringList;
  FSpeedreasonString  : TStringList ;
  FAdapSpeedReasonString : TStringList;

implementation
{****************************************************************************
* USES                                                                      *
****************************************************************************}
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

{****************************************************************************
* INITIALIZATION PART                                                       *
****************************************************************************}
initialization

  CarBlockTypeStandard := 1;

  FInterfaceString   := TStringList.Create;
  FSpeedreasonString := TStringList.Create;
  FPredefTextMsgString:= TStringList.Create;
  FAdapSpeedReasonString:= TStringList.Create;

{****************************************************************************
* FINALIZATION PART                                                         *
****************************************************************************}
finalization
  FInterfaceString.Clear ;
  FInterfaceString.Free ;
  FSpeedReasonString.Clear ;
  FSpeedReasonString.Free ;
  FAdapSpeedReasonString.Clear;
  FAdapSpeedReasonString.Free;

{****************************************************************************
* EXPORTS DECLARATIONS                                                     *
****************************************************************************}

{****************************************************************************
* RESOURCE STRING DECLARATIONS                                              *
****************************************************************************}
end.
