#ifndef DMIBHPEventCodes_hpp
#define DMIBHPEventCodes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file defines the DMI event ID's for BHP components which will be used as index of text message
* to display the text on DMI
* Below mentioned event codes need to update with respective of language INI file of DMI
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-31  spandita    Created
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
    * ATP-BHP related DMI events ID's 340--400
    */

    static const ATC::DMIEventCode tSetupOutOfRange = 340U;
    static const ATC::DMIEventCode tSetupVehicleIndexNotInRange = 341U;
    static const ATC::DMIEventCode tSetupNotValid = 342U;
    static const ATC::DMIEventCode externalEbFbFailure = 343U;
    static const ATC::DMIEventCode internalTcoFbFailure = 344U;
    static const ATC::DMIEventCode externalTcoFbFailure = 345U;
    static const ATC::DMIEventCode brakeTcoSupv = 346U;
    static const ATC::DMIEventCode brakeTcoRelaySupv = 347U;
    static const ATC::DMIEventCode locoIONotValid = 348U;
    static const ATC::DMIEventCode percentageOfEcpbCarsWithOperationalBrakesTooLow = 349U;
    static const ATC::DMIEventCode leaderCommLoss = 350U;
    static const ATC::DMIEventCode airBrakeCommLoss = 351U;
    static const ATC::DMIEventCode leaderCommEstablished = 352U;
    static const ATC::DMIEventCode airBrakeCommEstablished = 353U;
    static const ATC::DMIEventCode extRevDistExceededEvent = 354U;
    static const ATC::DMIEventCode extRevEBMarginExceededEvent = 355U;
    static const ATC::DMIEventCode extRevEmergencyBrakeCeilingSpeedEvent = 356U;
    static const ATC::DMIEventCode extRevServiceBrakeCeilingSpeedEvent = 357U;
    static const ATC::DMIEventCode brakeHandlingEvent = 358U;
    static const ATC::DMIEventCode errorReadEBCutOut = 359U;
    static const ATC::DMIEventCode nullPtr = 360U;
    static const ATC::DMIEventCode faultyLigConfig = 361U;
    static const ATC::DMIEventCode emergencyBrakesCutOut = 362U;
    static const ATC::DMIEventCode validationOutgoingMessageFailed = 363U;
    static const ATC::DMIEventCode invalidMessageType = 364U;
    static const ATC::DMIEventCode parserError = 365U;
    static const ATC::DMIEventCode missedMessageFromLCS = 366U;
    static const ATC::DMIEventCode parserNotImplemented = 367U;
    static const ATC::DMIEventCode lostConnectionWithLCSandECPB = 368U;
    static const ATC::DMIEventCode checkingLastCarBrakePressure = 369U;
    static const ATC::DMIEventCode lastCarBrakePressurePassed = 370U;
    static const ATC::DMIEventCode lastCarBrakePressureFailed = 371U;
    static const ATC::DMIEventCode ecpbReportedChangedRunMode = 372U;
    static const ATC::DMIEventCode lastCarBrakePressureTestAborted = 373U;
    static const ATC::DMIEventCode lastCarBrakePressureIsPending = 374U;
    static const ATC::DMIEventCode brakeSystemUpdated = 375U;

    /**Add here Event Codes for BHP*/
  }
}
#endif
