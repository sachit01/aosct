(****************************************************************************
           © COPYRIGHT Adtranz Signal AB, SWEDEN 1998.
           ===========================================

    The copyright to the computer program herein is the
    property of Adtranz AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Adtranz AB, or in accordance
    with the terms and conditions stipulated in the
    agreement/contract under which the program has been
    supplied.


    NAME :  MMI.pas

    PROJECT :  Esmeralda, InterFlow TrainBorne

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Daniel Persson   980126  First version

    DESCRIPTION :  Main unit that connects all the "little"
                units to form the MMI

    INTERFACE :
****************************************************************************)
program Dmi;


{****************************************************************************
* PROGRAM USES                                                              *
****************************************************************************}
{$R 'MMIAreaB.res' 'Resource\MMIAreaB\MMIAreaB.rc'}
{$R 'MMIAreaD.res' 'Resource\MMIAreaD\MMIAreaD.rc'}
{$R 'MMIResources.res' 'Resource\MMI\MMIResources.rc'}

uses
  Forms,
  UnitFullScreenMsg in 'UnitFullScreenMsg.pas' {FormFullScreenMsg},
  UnitFlash in 'UnitFlash.pas' {FormFlash},
  MMIStd in 'MMIStd.pas',
  MMITypes in 'MMITypes.pas',
  UnitExchange in 'UnitExchange.pas',
  MMIareaA in 'MMIareaA.pas',
  MMIareaB in 'MMIareaB.pas',
  MMIareaC in 'MMIareaC.pas',
  MMIareaD in 'MMIareaD.pas',
  UnitTrainVsTrack in 'UnitTrainVsTrack.pas' {FormTrainVsTrack},
  KeyboardUnitNumeric in 'KeyboardUnitNumeric.pas' {OSDKeyboardNumericWinControl},
  UnitViewLog in 'UnitViewLog.pas' {FormViewLog},
  UnitMMIFrame in 'UnitMMIFrame.pas' {FormMMIFrame},
  KeyboardUnit in 'KeyboardUnit.pas' {OSDKeyboardWinControl},
  UnitUnregForm in 'UnitUnregForm.pas' {FormUnreg},
  UnitStartupHistory in 'UnitStartupHistory.pas' {FormStartUpHistory},
  UnitChangeLogLevel in 'UnitChangeLogLevel.pas' {FormChangeLogLevel},
  UnitCommLost in 'UnitCommLost.pas' {FormCommLost},
  UnitSelectArea in 'UnitSelectArea.pas' {FormSelectArea},
  UnitMainLayer1 in 'UnitMainLayer1.pas' {FormMainLayer1},
  UnitDMIDataModule in 'UnitDMIDataModule.pas' {DataModuleDMI: TDataModule},
  UnitNewTrainName in 'UnitNewTrainName.pas' {FormNewTrainName},
  UnitMainLayer2 in 'UnitMainLayer2.pas' {FormMainLayer2},
  UnitMainArea in 'UnitMainArea.pas' {FormMainArea},
  UnitMainLayer3 in 'UnitMainLayer3.pas' {FormMainLayer3},
  UnitNoMAControlled in 'UnitNoMAControlled.pas' {FormNoMAControlled},
  UnitLogin in 'UnitLogin.pas' {FormLogin},
  UnitManualConfiguration in 'UnitManualConfiguration.pas' {FormManualConfiguration},
  UnitReReg in 'UnitReReg.pas' {FormReRegistration},
  UnitAtpMessages in 'UnitAtpMessages.pas' {FormAtpMessages},
  UnitPowerDown in 'UnitPowerDown.pas' {FormPowerDown},
  UnitSafetyHalt in 'UnitSafetyHalt.pas' {FormSafetyHalt},
  UnitTrainComposition in 'UnitTrainComposition.pas' {FormTrainComposition},
  UnitConfirmSleepModeExit in 'UnitConfirmSleepModeExit.pas' {FormConfirmExitSleepingMode},
  UnitConfirmCancelRegArea in 'UnitConfirmCancelRegArea.pas' {FormConfirmCancelRegArea},
  UnitConfirmFreeRollingClear in 'UnitConfirmFreeRollingClear.pas' {FormConfirmFreeRollingClear},
  UnitSleepingModeForm in 'UnitSleepingModeForm.pas' {FormSleepingMode},
  UnitConfirmRejectSetup in 'UnitConfirmRejectSetup.pas' {FormConfirmRejectSetup},
  UnitConfirmATPMode in 'UnitConfirmATPMode.pas' {FormConfirmNewATPMode},
  UnitConfirmMAAccept in 'UnitConfirmMAAccept.pas' {FormConfirmMAAccept},
  UnitAutomaticConfiguration in 'UnitAutomaticConfiguration.pas' {FormAutomaticConfiguration},
  UnitRadioChannel in 'UnitRadioChannel.pas' {FormRadioChannel},
  UnitConfirmDepartureTest in 'UnitConfirmDepartureTest.pas' {FormConfirmDepartureTest},
  UnitConfirmBrakeTest in 'UnitConfirmBrakeTest.pas' {FormConfirmBrakeTest},
  UnitConfirmTrainIntegrity in 'UnitConfirmTrainIntegrity.pas' {FormConfirmTrainIntegrity},
  UnitConfirmLoadedStatus in 'UnitConfirmLoadedStatus.pas' {FormConfirmLoadedStatus},
  UnitAbortLastCarBrakeTest in 'UnitAbortLastCarBrakeTest.pas' {FormAbortLastCarBrakeTest},
  UnitConfirmAbortLastCarBrakeTest in 'UnitConfirmAbortLastCarBrakeTest.pas' {FormConfirmAbortLastCarBrake},
  UnitConfirmTachometer1Failure in 'UnitConfirmTachometer1Failure.pas' {FormConfirmTachometer1Failure},
  UnitConfirmTachometer2Failure in 'UnitConfirmTachometer2Failure.pas' {FormConfirmTachometer2Failure},
  UnitConfirmDopplerRadarFailure in 'UnitConfirmDopplerRadarFailure.pas' {FormConfirmDopplerRadarFailure};

{$R *.RES}

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
begin
  Application.Initialize;
  Application.Title := 'DMI';
  Application.CreateForm(TFormMMIFrame, FormMMIFrame);
  Application.CreateForm(TFormFullScreenMsg, FormFullScreenMsg);
  Application.CreateForm(TOSDKeyboardNumericWinControl, OSDKeyboardNumericWinControl);
  Application.CreateForm(TFormPowerDown, FormPowerDown);
  Application.CreateForm(TFormReRegistration, FormReRegistration);
  Application.CreateForm(TFormFlash, FormFlash);
  Application.CreateForm(TFormTrainVsTrack, FormTrainVsTrack);
  Application.CreateForm(TFormViewLog, FormViewLog);
  Application.CreateForm(TFormCommLost, FormCommLost);
  Application.CreateForm(TOSDKeyboardWinControl, OSDKeyboardWinControl);
  Application.CreateForm(TFormUnreg, FormUnreg);
  Application.CreateForm(TFormStartUpHistory, FormStartUpHistory);
  Application.CreateForm(TFormChangeLogLevel, FormChangeLogLevel);
  Application.CreateForm(TFormConfirmMAAccept, FormConfirmMAAccept);
  Application.CreateForm(TFormConfirmExitSleepingMode, FormConfirmExitSleepingMode);
  Application.CreateForm(TFormSelectArea, FormSelectArea);
  Application.CreateForm(TDataModuleDMI, DataModuleDMI);
  Application.CreateForm(TFormConfirmCancelRegArea, FormConfirmCancelRegArea);
  Application.CreateForm(TFormNewTrainName, FormNewTrainName);
  Application.CreateForm(TFormMainArea, FormMainArea);
  Application.CreateForm(TFormNoMAControlled, FormNoMAControlled);
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormManualConfiguration, FormManualConfiguration);
  Application.CreateForm(TFormPowerDown, FormPowerDown);
  Application.CreateForm(TFormSafetyHalt, FormSafetyHalt);
  Application.CreateForm(TFormTrainComposition, FormTrainComposition);
  Application.CreateForm(TFormConfirmFreeRollingClear, FormConfirmFreeRollingClear);
  Application.CreateForm(TFormSleepingMode, FormSleepingMode);
  Application.CreateForm(TFormConfirmRejectSetup, FormConfirmRejectSetup);
  Application.CreateForm(TFormConfirmDepartureTest, FormConfirmDepartureTest);
  Application.CreateForm(TFormConfirmNewATPMode, FormConfirmNewATPMode);
  Application.CreateForm(TFormConfirmMAAccept, FormConfirmMAAccept);
  Application.CreateForm(TFormAutomaticConfiguration, FormAutomaticConfiguration);
  Application.CreateForm(TFormRadioChannel, FormRadioChannel);
  Application.CreateForm(TFormAtpMessages, FormAtpMessages);
  Application.CreateForm(TFormConfirmBrakeTest, FormConfirmBrakeTest);
  Application.CreateForm(TFormConfirmTrainIntegrity, FormConfirmTrainIntegrity);
  Application.CreateForm(TFormConfirmLoadedStatus, FormConfirmLoadedStatus);
  Application.CreateForm(TFormAbortLastCarBrakeTest, FormAbortLastCarBrakeTest);
  Application.CreateForm(TFormConfirmAbortLastCarBrake, FormConfirmAbortLastCarBrake);
  Application.CreateForm(TFormConfirmTachometer1Failure, FormConfirmTachometer1Failure);
  Application.CreateForm(TFormConfirmTachometer2Failure, FormConfirmTachometer2Failure);
  Application.CreateForm(TFormConfirmDopplerRadarFailure, FormConfirmDopplerRadarFailure);
  Application.Run;
end.
