#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoConsts.h %
*
*  %version:       5 %
*
*  %created_by:    akushwah %
*
*  %date_created:  2017-07-10 15:29 %
*
*  DESCRIPTION: 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-04-21    Antbäck     File created
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2016-10-16    Marlundg    New simulation type
*
*******************************************************************************/

/******************************************************************************
* DEFINES
******************************************************************************/
#define RECEIVED_OUTPUT_SIZE  (OutputEnd - 1)	// Number of outputs from AOS
#define INPUT_SIZE            (InputEnd -1)		// NUmber of inputs to AOS
#define ANALOG_INPUT_SIZE     (AnalogInputEnd -1)		// NUmber of inputs to AOS
#define MAX_CHARS_FROM_LCS  100 

#define SIZE_OF_UNIT_EXT_STATUS 10
#define CHECK_ATP_TIMER 5000 // The frequency at which AOSPC checks for activity from ATP (ms)

/******************************************************************************
* ENUMS
******************************************************************************/

enum ATxStatus
{
    ATxStopped,
    ATxReadyToRun,
    ATxRunning,
    ATxNoProcess
};

typedef enum
{
  ATPModeUndefined = 0,
  ATPModePowerUp,
  ATPModeConfiguration,
  ATPModeRegistration,
  ATPModeBaliseSearch,
  ATPModeNormal,
  ATPModeShunting,
  ATPModeLocation,
  ATPModeYard,
  ATPModeUnregistered,
  ATPModePoweringDown,
  ATPModeSafetyHalt,
  ATPModeSleeping,
  ATPModeStaffResponsible,
  ATPModeShuntingRoute,
  ATPModePossession,
  ATPModeSplit,
  ATPModeJoin,
  ATPModeSafeBrakeToStop,
} ATPModeEnum;

typedef enum
{
   ConfigModeSubStateUndefined = 0,
   ConfigModeSubStateManual,
   ConfigModeSubStateReRegistration,
   ConfigModeSubStateRestartConfiguration,
   ConfigModeSubStateAutomaticConfiguration
} ConfigModeSubState;

enum TBSystemStatus
{
    TBSystemPendingStartUp,
    TBSystemStartUp,
    TBSystemRunning,
    TBSystemStopped,
    TBSystemQuitCmd
};

enum Inputs
{
  Input1 = 1,
  Input2,
  Input3,
  Input4,
  Input5,
  Input6,
  Input7,
  Input8,
  Input9,
  Input10,
  Input11,
  Input12,
  Input13,
  Input14,
  Input15,
  Input16,
  Input17,
  Input18,
  Input19,
  Input20,
  Input21,
  Input22,
  Input23,
  Input24,
  InputEnd
};

enum AnalogInputs
{
  AnalogInput1 = 1,
  AnalogInput2,
  AnalogInput3,
  AnalogInput4,
  AnalogInput5,
  AnalogInput6,
  AnalogInput7,
  AnalogInput8,
  AnalogInputEnd
};

enum Outputs
{
  Output1 = 1,
  Output2,
  Output3,
  Output4,
  Output5,
  Output6,
  Output7,
  Output8,
  Output9,
  Output10,
  Output11,
  Output12,
  OutputEnd
};

typedef enum {
    SelCtrlDriver           = 0,
    SelCtrlLCS              = 1,
    SelCtrlRailDriver       = 2,
    SelCtrlLCSRailDriver    = 3,
} EnumSelectedController; 

typedef enum {
    ATOSwitchUndef = 0,
    ATOSwitchMan   = 1,
    ATOSwitchSupv  = 2,
    ATOSwitchAuto  = 3
} EnumATOMODE; 

typedef enum {
    DDUndef   = 0,
    DDNeutral = 1,
    DDForward = 2,
    DDReverse = 3,
    DDBoth = 4
} EnumDriveDir;

typedef enum {
    SimulationSil = 1,
    SimulationHil,
    SimulationEmd,
    SimulationVSim
} EnumSimulationMode;

typedef enum {
   WindowStateMin = 0,
   WindowStateNormal,
   WindowStateMax
} EnumWindowState;

typedef enum {
  TcoNoFb = 0,
  TcoOnlyFb = 1,
  TcoOrderAndFb = 2
} EnumTcoFb;

typedef enum
{
   StartTrainCommandRegArea = 0,
   StartTrainCommandLogin,
   StartTrainCommandRegistrationTypeSelection,
   StartTrainCommandConfigSelection,
   StartTrainCommandAcceptReReg,
   StartTrainCommandSetBaliseID,
   StartTrainCommandAcceptAutoConfig,
   StartTrainCommandConfirmDeparture,
   StartTrainCommandOrientation,
   StartTrainCommandBrakeTest,
   StartTrainCommandAutoControl,
   StartTrainCommandReleaseSB,
   StartTrainCommandSBFlashes,
   StartTrainCommandFailReceivedFromDMI,
   StartTrainCommandComplete
} StartTrainCommands;

typedef enum
{
   StartTrainArgTimeout = 1,
   StartTrainArgRegArea,
   StartTrainArgBaliseId,
   StartTrainArgUser,
   StartTrainArgPwd,
   StartTrainArgLoaded,
   StartTrainArgOrientation,
   StartTrainArgAutoConfigCars
} StartTrainArguments;

/******************************************************************************
* STRUCTS
******************************************************************************/
typedef struct {
    bool        dataValid;
    bool        IncAcc;
    bool        IncRet;
    bool        Coast;
} StructLCSSpeedCmd; 

typedef struct {
    bool        dataValid;
    int         value;
} StructLCSAbsAccCmd; 

typedef struct {
    bool            dataValid;
    EnumDriveDir    value;
} StructLCSDrivDirCmd; 

typedef struct
{
    union
    {
        enum ATxStatus      ATx;
        enum TBSystemStatus TBSystem;
    }Status;

    unsigned char   ExtStatus[SIZE_OF_UNIT_EXT_STATUS];

} SimulatedUnitStatus;

/* Time type (milliseconds) used in the platform. */
typedef DWORD         PlatformTime;