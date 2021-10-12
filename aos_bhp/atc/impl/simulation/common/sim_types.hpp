#ifndef SIMTypes_hpp
#define SIMTypes_hpp
/*******************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
 * 
 * We reserve all rights in this file and in the information 
 * contained therein. Reproduction, use or disclosure to third 
 * parties without written authority is strictly forbidden.
 *
 *
 * DESCRIPTION: 
 *
 * The SIM Types declares common types used by Simulator components
*******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-18    akushwah    Created
* 2016-05-19    adgupta     Added VIOH Sim related Values
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_types.h>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/

namespace ATC
{
  namespace Sim
  {
    /** length of the header footer of message received from AOSPC
    */
    static const uint8_t headerFooterLen =5U;

    /** protocol version of Simulation movement Messages
    */
    static const uint8_t simMovementProtocolVer = 1U;

    /** protocol version of Simulation Integrity Messages
    */
    static const uint8_t simIntegrityProtocolVer = 1U;

    /** protocol version of Simulated VIOH Input Messages
    */
    static const uint8_t simViohInputProtocolVer = 1U;

    /** protocol version of Simulated VIOH Output Messages
    */
    static const uint8_t simViohOutputProtocolVer = 1U;

    /** protocol version of Simulation Registration Balise ID Messages
    */
    static const uint8_t simRegBaliseIDProtocolVer = 1U;

    /** protocol version of Simulation ATP Ready Messages
    */
    static const uint8_t simATPReadyProtocolVer = 1U;

    /** Length of SimulatedMovement message from AOSPC
    */
    static const uint8_t simMovementDataLen = 9U;

    /** Length of SimulatedMovement message from AOSPC
    */
    static const uint8_t simIntegrityDataLen = 3U;

    /** Length of Simulated Input message from AOSPC 
    */
    static const uint8_t simInputDataLen = 25U;

    /** Length of Simulated Input NID message type
    */
    static const uint8_t simInputNidMsgType = 1U;

    /** Length of SimulatedMovement NID message type
    */
    static const uint8_t simMovementNidMsgType = 10U;

    /** Length of Simulated Integrity NID message type
    */
    static const uint8_t simIntegrityNidMsgType = 11U;

    /** Registration balise ID message type
    */
    static const uint8_t simBaliseIDNidMsgType = 20U;

    /** Length of Simulated Registration balise id from AOSPC
    */
    static const uint8_t simOPCRegBaliseDataLen = 3U;

    /** ATP ready signal message type
    */
    static const uint8_t simATPReadyIDNidMsgType = 21U;

    /** Length of Simulated ATP Ready message from OPC sim
    */
    static const uint8_t simOPCATPReadyDataLen = 1U;

    /** Length of Simulated Output NID message type
    */
    static const uint8_t simOutputNidMsgType = 128U;

    /** Length of Simulated Output message from AOSPC 
    */
    static const uint8_t simOutputDataLen = 5U;

    /** Static configuration telegramType value
    */
    static const uint8_t staticConfigTelType = 1U;

    /** Dynamic configuration telegramType value
    */
    static const uint8_t dynConfigTelType = 2U;

    /** configuration Response telegramType value
    */
    static const uint8_t configResTelType = 3U;

    /** Odometer Measurement Data telegramType value
    */
    static const uint8_t odoMeasDataTelType = 4U;

    /** SDP SW version (major)
    */
    static const uint8_t sdpMajorVer = 2U;

    /** SDP SW version (middle)
    */
    static const uint8_t sdpMidVer = 2U;

    /** SDP SW version (minor)
    */
    static const uint8_t sdpMinorVer = 5U;

     /** No Calibration Ongoing
    */
    static const uint8_t noCalibrationOngoing = 1U;
      
    /** 1: Interface version supported 
    */
    static const uint8_t interfaceVerSupported     = 1U;    
    /** 1: Full Service
    */
    static const uint8_t fullService               = 1U;   
    /** 0: No maintance or driver warning
    */
    static const uint8_t noMaintanceDriverWarning  = 0U;   
    /** 1: Direction known 
    */
    static const uint8_t directionKnown            = 1U;   
    /** 0:unplausible
    */
    static const uint8_t unplausible               = 0U;   

    /** No Slip/Slide Status from tachometer */
    static const uint8_t noSlipSlideStatusTacho    = 0U;

    /** Slip Status from tachometer */
    static const uint8_t slipStatusTacho           = 1U;

    /** Slide Status from tachometer */
    static const uint8_t slideStatusTacho          = 2U;

    /** 0: Zero filtered velocity of tachometer 1
    */
    static const int16_t filteredVelTacho1         = 0;
    /** 0: Zero filtered velocity of tachometer 2
    */
    static const int16_t filteredVelTacho2         = 0;
    /** 0: Zero filtered velocity of Doppler
    */
    static const int16_t filteredVelDoppler        = 0;
    /** 0: Measured distance using only tachometer 1
    */
    static const int32_t measureDistTacho1         = 0;
    /** 0: Measured distance using only tachometer 2
    */
    static const int32_t measureDistTacho2         = 0;
    /** 0: Measured distance using only Doppler
    */
    static const int32_t measureDistDoppler        = 0;


    /**configuration status
    */
    static const uint8_t configNOK                 = 0U;
    static const uint8_t invalidStaticConfigTelRec = 1U;
    static const uint8_t validStatWaitDyn          = 2U;
    static const uint8_t invalidDynConfigTelRec    = 3U;
    static const uint8_t configOK                  = 4U;
    static const uint8_t unexpectedStatic          = 5U;
    static const uint8_t unexpectedDyn             = 6U;

    /** tacho Distance & Status
    */
    static const uint8_t measuredDistTacho1         = 0U;
    static const uint8_t resultConcludedCaliTacho1  = 0U;
    static const uint8_t measuredDistTacho2         = 0U;
    static const uint8_t resultConcludedCaliTacho2  = 0U;
    static const uint8_t measuredDistDoppler        = 0U;
    static const uint8_t resultConcludedCaliDoppler = 0U;
    
  }
}

#endif
