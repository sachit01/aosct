#ifndef DMIEventCodes_hpp
#define DMIEventCodes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file defines the DMI event ID's which will be used as index of text message
* to display the text on DMI
* Below mentioned event codes need to update with respective of language INI file of DMI
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-16   spandita    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * Do not send to driver ID 0
    */
    static const ATC::DMIEventCode noDmi = 0U;

    /**
    *Mode Control Event ID's 1--35
    */
    static const ATC::DMIEventCode modFirstBaliseError = 1U;
    static const ATC::DMIEventCode modReRegInvalidQSetup = 2U;
    static const ATC::DMIEventCode modReRegTsetUpNotValid = 3U;
    static const ATC::DMIEventCode modReRefInvalidMA = 4U;
    static const ATC::DMIEventCode modPwrUpModFailure = 5U;
    static const ATC::DMIEventCode modSafetyHaltEB = 6U;
    static const ATC::DMIEventCode modChangeToUnreg = 7U;
    static const ATC::DMIEventCode modUnknownModeState = 8U;
    static const ATC::DMIEventCode modOdometerInvalid = 9U;
    static const ATC::DMIEventCode modUnconditionalMA = 10U;
    static const ATC::DMIEventCode modSBToStopInvalidTSetup = 11U;
    static const ATC::DMIEventCode modSBToStopInvalidPrevMod = 12U;
    static const ATC::DMIEventCode modNegativeAck = 13U;
    static const ATC::DMIEventCode modThirdBal = 14U;
    static const ATC::DMIEventCode modEmergencyAlertBS = 15U;
    static const ATC::DMIEventCode nullPointerAccess = 16U;
    static const ATC::DMIEventCode tccConnectionLost = 17U;
    static const ATC::DMIEventCode sbInBothCabinActive = 18U;
    // 19U
    static const ATC::DMIEventCode maxAllowedRunTime = 20U;
    static const ATC::DMIEventCode notReadyToDrive = 21U;
    static const ATC::DMIEventCode exceededMAWaitForSecBal = 22U;
    static const ATC::DMIEventCode secBaliseFoundInBS = 23U;
    static const ATC::DMIEventCode unregMsg = 24U;
    // 25U
    static const ATC::DMIEventCode sbInSafeBrake = 26U;
    static const ATC::DMIEventCode eaActiveFromTCC = 27U;
    static const ATC::DMIEventCode unknownStateInShunting = 28U;
    static const ATC::DMIEventCode unknownStateInConfig = 29U;
    static const ATC::DMIEventCode maTimeout = 30U;
    static const ATC::DMIEventCode stopTrainRequested = 31U;
    static const ATC::DMIEventCode modEmergencyAlertRegistrationMode = 32U;

    /**
    * Tracks DMI events ID's 36--49
    */
    static const ATC::DMIEventCode tracksTrkListFull = 36U;
    static const ATC::DMIEventCode tracksBalListFull = 37U;
    static const ATC::DMIEventCode tracksInconsistentTrkandBal = 38U;
    static const ATC::DMIEventCode tracksSameBalIdFound = 39U;
    static const ATC::DMIEventCode tracksSameTrkIdFound = 40U;
    static const ATC::DMIEventCode tracksInvalidPosInTrk = 41U;
    static const ATC::DMIEventCode tracksInvalidTrack = 42U;
    static const ATC::DMIEventCode tracksInvalidTrkDir = 43U;
    static const ATC::DMIEventCode tracksPosLimitFailureInTrk = 44U;
    static const ATC::DMIEventCode tracksInternalFailure = 45U;
    static const ATC::DMIEventCode tracksBalRemovalFailure = 46U;
    
    /**
    * Targets DMI events ID's 50--65
    */
    static const ATC::DMIEventCode targetsInconsistentTrgt = 50U;
    static const ATC::DMIEventCode targetsTrgtListFull = 51U;
    static const ATC::DMIEventCode targetsSameTrgtId = 52U;
    static const ATC::DMIEventCode targetsInternalFailure = 53U;

    /**
    * Tsetup DMI events ID's 66--79
    */
    static const ATC::DMIEventCode tSetupAtpVehicleIndexNotInRange = 60U;
    static const ATC::DMIEventCode tSetupAtpNotValid = 61U;
    static const ATC::DMIEventCode tSetupAtpModFailure = 62U;


    /**
    * Position DMI events ID's 80--99
    */
    static const ATC::DMIEventCode posUnknownBal = 80U;
    static const ATC::DMIEventCode posOutsideBalWindow = 81U;
    static const ATC::DMIEventCode posExceededRefRange = 82U;
    static const ATC::DMIEventCode posSecondAndFirstBalIdentical = 83U;
    static const ATC::DMIEventCode posMissedBalErr = 84U;
    static const ATC::DMIEventCode posFrontOrRearEndOutOfBounderies = 85U;
    static const ATC::DMIEventCode posThirdBalError = 86U;
    static const ATC::DMIEventCode posYardOrShuntingModeNotAllowed = 87U;
    static const ATC::DMIEventCode invalidBaliseInBaliseSearch = 88U;
    static const ATC::DMIEventCode secBaliseFoundBeforeMA = 89U;
    static const ATC::DMIEventCode invalidBalInPos = 90U;

    /**
    * BTM & LOCO IO DMI events ID's 100--119
    */
    static const ATC::DMIEventCode btmMalfunctioning             = 100U;
    static const ATC::DMIEventCode btmIncorrectBal               = 101U;
    static const ATC::DMIEventCode btmRoutineTestInProg          = 102U;
    static const ATC::DMIEventCode btmRoutineTestMandatory       = 103U;
    static const ATC::DMIEventCode locoIOError                   = 104U;
    static const ATC::DMIEventCode btmRoutineTestSucceded        = 105U;
    static const ATC::DMIEventCode btmRoutineTestFailed          = 106U;
    static const ATC::DMIEventCode btmMandatoryRoutineTestFailed = 107U;
    static const ATC::DMIEventCode btmFailed                     = 108U;
    static const ATC::DMIEventCode btmBaliseDetectedAtStandstill = 109U;
    static const ATC::DMIEventCode btmReadingCouldBeFaulty       = 110U;
    static const ATC::DMIEventCode btmAvailabilityIsGreen        = 111U;
    static const ATC::DMIEventCode btmSporadicErrorOccurred      = 112U;
    static const ATC::DMIEventCode isolationSwitchNotinRunMode   = 113U;
    static const ATC::DMIEventCode sleepingSignalActive          = 114U;
    static const ATC::DMIEventCode sleepingSignalInActive        = 115U;
    static const ATC::DMIEventCode ambiguousTravelDir            = 116U;
    static const ATC::DMIEventCode vitalDriverFailure            = 117U;
    static const ATC::DMIEventCode opcFailed                     = 118U;

    /**
    * Decode DMI events ID's 120--138
    */
    static const ATC::DMIEventCode decodeTachErr = 120U;
    static const ATC::DMIEventCode decodeStandStill = 121U;
    static const ATC::DMIEventCode decodeBalQueNotEmpty = 122U;
    static const ATC::DMIEventCode redBaliseDetected = 123U;

    /**
    * Odometry  DMI events ID's 139--159
    */
    static const ATC::DMIEventCode NominalSpeedDifferenceFailureOccured = 139U;
    static const ATC::DMIEventCode odoBalWindowError = 140U;
    static const ATC::DMIEventCode odoFailure = 141U;
    static const ATC::DMIEventCode noConfigRespRecv = 142U;
    static const ATC::DMIEventCode noDataTelFailure = 143U;
    static const ATC::DMIEventCode odoConfigFail = 144U;
    static const ATC::DMIEventCode odoInvalidState = 145U;
    static const ATC::DMIEventCode odoVersionMismatch = 146U;
    static const ATC::DMIEventCode invalidDataTelegram = 147U;
    static const ATC::DMIEventCode tachometer1Failure = 148U;
    static const ATC::DMIEventCode tachometer2Failure = 149U;
    static const ATC::DMIEventCode dopplerFailure = 150U;
    static const ATC::DMIEventCode tachometer1NormalOperation = 151U;
    static const ATC::DMIEventCode tachometer2NormalOperation = 152U;
    static const ATC::DMIEventCode dopplerNormalOperation = 153U;
    static const ATC::DMIEventCode tachometer1FailureConfirmed = 154U;
    static const ATC::DMIEventCode tachometer2FailureConfirmed = 155U;
    static const ATC::DMIEventCode dopplerFailureConfirmed = 156U;
    static const ATC::DMIEventCode slipDetected = 157U;
    static const ATC::DMIEventCode slideDetected = 158U;
    static const ATC::DMIEventCode dopplerNeedsMaintenance = 159U;

    /**
    * Brake,Brake Calculation & Supervise  DMI events ID's 160--189
    */
    static const ATC::DMIEventCode brakeEbSupv = 160U;
    static const ATC::DMIEventCode brakeEbRelaySupv = 161U;
    static const ATC::DMIEventCode supTargetListErr = 162U;
    static const ATC::DMIEventCode brakeCalInternalFailure = 163U;
    static const ATC::DMIEventCode brakeTestInProgress = 164U;
    static const ATC::DMIEventCode brakeTestFailed = 165U;
    static const ATC::DMIEventCode brakeTestSuccessful = 166U;
    static const ATC::DMIEventCode unableToStartBrakeTest = 167U;
    static const ATC::DMIEventCode internalEbFbFailure = 168U;
    static const ATC::DMIEventCode brakeTestTimedOut = 169U;
    static const ATC::DMIEventCode cabinDeactivated = 170U;
    static const ATC::DMIEventCode additionalBrakeOrders = 171U;
    static const ATC::DMIEventCode emergencyAlertActive = 172U;
    static const ATC::DMIEventCode brakeTestAbortedByDriver = 173U;
    static const ATC::DMIEventCode brakeTestAborted = 174U;
    static const ATC::DMIEventCode atpNeedsReset = 175U;
    static const ATC::DMIEventCode wrongOBRDProtocolVersionRecvd = 176U;
    static const ATC::DMIEventCode ebEventInSBSupv = 177U;
    static const ATC::DMIEventCode sbOnCeilingSpeed = 178U;
    static const ATC::DMIEventCode serviceBrakeATPmode = 179U;
    static const ATC::DMIEventCode serviceBrakeTargetSupervision = 180U;
    static const ATC::DMIEventCode serviceBrakeStandstillSupervision = 181U;
    static const ATC::DMIEventCode emergencyBrakeCeilingSpeed = 182U;
    static const ATC::DMIEventCode emergencyBrakeTargetSupervision = 183U;
    static const ATC::DMIEventCode emergencyBrakeStandstillSupervision = 184U;
    static const ATC::DMIEventCode firstWarningTargetSupervision = 185U;
    static const ATC::DMIEventCode secondWarningTargetSupervision = 186U;
    static const ATC::DMIEventCode secondWarningCeilingSpeedSupervision = 187U;
    static const ATC::DMIEventCode firstWarningCeilingSpeedSupervision = 188U;
    static const ATC::DMIEventCode locationSupvError = 189U;

    /**
    * Cross Compare  DMI events ID's 190--200
    */
    static const ATC::DMIEventCode crossCompMisMatchErr = 190U;
    static const ATC::DMIEventCode crossCompDataWriteErr = 191U;
    static const ATC::DMIEventCode crossCompBuffFull = 192U;

    /**
    * Radio channel/handler DMI events ID's 200--219
    */

    /**
    * Message Handler  DMI events ID's 220--240
    */
    static const ATC::DMIEventCode msgHdlrFirstBalNotFoundInMA = 220U;
    static const ATC::DMIEventCode msgHdlrInvalidCarCnt = 221U;
    static const ATC::DMIEventCode msgHdlrUnregistrationMsgReceived = 222U;
    static const ATC::DMIEventCode msgHdlrTooLowLambdaReceived = 223U;
    static const ATC::DMIEventCode msgHdlrTrainSetupRejectedByAOS = 224U;
    static const ATC::DMIEventCode msgHdlrInvalidConfigDataByTCC = 225U;
    static const ATC::DMIEventCode msgHdlrInvalidMaInReposition = 226U;
    static const ATC::DMIEventCode msgHdlrCriticalState = 227U;
    static const ATC::DMIEventCode msgHdlrQueueFullError = 228U;
    static const ATC::DMIEventCode msgHdlrValidationOutgoingMessageFailed = 229U;
    static const ATC::DMIEventCode msgHdlrNoValidPositionreport = 230U;
    static const ATC::DMIEventCode msgHdlrParserError = 231U;
    static const ATC::DMIEventCode uncondDiscardedSafeBrakeToStopInBaliseSearch = 232U;
    static const ATC::DMIEventCode msgHdlrInvalidDataInMessage = 233U;
    static const ATC::DMIEventCode msgHdlrInvalidMessage = 234U;
    static const ATC::DMIEventCode msgHdlrInvalidMaInReregistration = 235U;
    static const ATC::DMIEventCode msgHdlrTooManyBalisesInPossessionAck = 236U;

    /**
    * DMI channel/handler DMI events ID's 240--259
    */
    static const ATC::DMIEventCode dmiUnDefMsg = 240U;

    /**
    * TIMS related DMI events ID's 260--279
    */
    static const ATC::DMIEventCode timsIntegrityBrokenStillSupervised = 260U;

    /**
    * Reverse Supervision and Rollaway related DMI events ID's 290--299
    */
    static const ATC::DMIEventCode revSupvError = 290U;
    static const ATC::DMIEventCode revEBMarginError = 291U;
    static const ATC::DMIEventCode rollAwaySupvError = 292U;
    static const ATC::DMIEventCode rollAwayEBMarginError = 293U;   
      
  }
}

#endif
