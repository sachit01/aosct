#ifndef Abstract_Mode_Control_EventIDs_hpp
#define Abstract_Mode_Control_EventIDs_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
*
* The Unique Event Ids used by the Mode Control Component while creating events
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-07-24    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
   namespace Kernel
   {
      /**
      * Event Ids to report Errors to event handler
      * By keeping them in the namespace we will not get Lint-warnings from
      * constructors accessing them.
      */
      static const uint16_t eventIdSBinPowerUp = 0x01U;
      static const uint16_t eventIdFFinPowerUp = 0x02U;
      static const uint16_t eventIdSBinConfig = 0x03U;
      static const uint16_t eventIdUnknownStateInConfig = 0x04U;
      static const uint16_t eventIdSBinReg = 0x05U;
      static const uint16_t eventIdSBinBALFirstBalise = 0x07U;
      static const uint16_t eventIdUnknownStateInBAL = 0x08U;
      static const uint16_t eventIdSBinPowerDown = 0x09U;
      static const uint16_t eventIdSBinSafeBrake = 0x0AU;
      static const uint16_t eventIdStandstillInUnregistered = 0x0BU;
      static const uint16_t eventIdSafetyHaltEvent = 0x0CU;
      static const uint16_t eventIdBaliseNotFoundinBS = 0x0DU;
      static const uint16_t eventIdExceededMAWaitForSecBal = 0x0EU;
      static const uint16_t eventIdSBinBothCabinActive = 0x0FU;
      static const uint16_t eventIdSBinNoCabinActive = 0x10U;
      static const uint16_t eventIdTimeForMandatoryBrakeTestSB = 0x11U;
      // 12U
      static const uint16_t eventIdStandStillInStaffResp = 0x13U;
      static const uint16_t eventIdSBinLocMod = 0x14U;
      static const uint16_t eventIdStandStillShuntingRouteConfirmation = 0x15U;
      static const uint16_t eventIdUnknownStateInShuntRoute = 0x16U;
      static const uint16_t eventIdIncorrectQsetupAndPositionCombination = 0x17U;
      static const uint16_t eventIdInValidMAReceivedInReRegistration = 0x18U;
      static const uint16_t eventIdSBInOdometerInvalidStatus = 0x19U;
      static const uint16_t eventIdStandstillInIdleTrainState = 0x1AU;
      static const uint16_t eventIdSecondBaliseFoundinBS = 0x1BU;
      static const uint16_t eventIdOdometerInvalid = 0x1CU;
      static const uint16_t eventIdTimeForMandatoryBrakeTestSS = 0x1DU;
      static const uint16_t eventIdInvalidTSetupInSafeBrkToStp = 0x1EU;
      static const uint16_t eventIdSafetyHaltInSBSInvalidPrevMod = 0x1FU;
      static const uint16_t eventIdStandStillInJoin = 0x20U;
      static const uint16_t eventIdStandStillSleepInactiveAndMoving = 0x21U;
      static const uint16_t eventIdLogSleepInactiveAndMoving = 0x22U;
      static const uint16_t eventIdNotifyRejectedAck = 0x23U;
      static const uint16_t eventIdWaitForTCCResp = 0x24U;
      static const uint16_t eventIdTCClost = 0x25U;
      static const uint16_t eventIdModAckRcv = 0x26U;
      static const uint16_t eventIdModTCCTimeout = 0x27U;
      static const uint16_t eventIdSecBaliseNotFoundInBS = 0x28U;
      static const uint16_t eventIdSecBaliseFoundInBS = 0x29U;
      static const uint16_t eventIdEmergencyAlertInBS = 0x2AU;
      static const uint16_t eventIdATPNeedsReset = 0x2BU;
      static const uint16_t eventIdSleepSigInactiveInSleeping = 0x2CU;
      static const uint16_t eventIdSleepingSigActiveIllegalMode = 0x2DU;
      static const uint16_t eventIdPowerUpSeqStarted = 0x2EU;
      static const uint16_t eventIdMaxAllowedRunTime = 0x2FU;
      static const uint16_t eventIdSafetyHaltMaxAllowedRunTime = 0x30U;
      static const uint16_t eventIdPowerDownSeqStarted = 0x31U;
      static const uint16_t eventIdDriverNotAuth = 0x32U;
      static const uint16_t eventIdDriverLogout = 0x33U;
      static const uint16_t eventIdDriverLoginNotAuthorized = 0x34U;
      static const uint16_t eventIdEaFromTCC = 0x35U;
      static const uint16_t eventIdStandStillInSplit = 0x36U;
      static const uint16_t eventIdInCorrectRouteTypeInBS = 0x37U;
      static const uint16_t eventIdUnconditionalInBS = 0x38U;
      static const uint16_t eventIdModAckTimeout = 0x39U;
      static const uint16_t eventIdNotReadyToDrive = 0x3AU;
      static const uint16_t eventIdUnRegMsg = 0x3BU;
      static const uint16_t eventIdTCClostInStandstill = 0x3CU;
      static const uint16_t eventIdSleepingSignalInactive = 0x3DU;
      static const uint16_t eventIdDispatcherVersionMismatch = 0x3FU;
      static const uint16_t eventIdWaitForDriverAck = 0x40U;
      static const uint16_t eventIdMATimeout = 0x41U;
      static const uint16_t eventIdModeChanged = 0x42U;
      static const uint16_t eventIdStopTrainRequested = 0x43U;
      static const uint16_t eventIdEmergencyAlertInRegistrationMode = 0x44U;
   }
}
#endif
