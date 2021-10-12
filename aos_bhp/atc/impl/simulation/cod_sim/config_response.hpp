#ifndef ConfigResponse_hpp
#define ConfigResponse_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This header file contains the structure for the response of the Odometer 
*  Configuration Static/Dynamic Telegram.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-03    akushwah    Created
* 2016-05-10    akushwah    Added the Doxygen tag
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
//Config response to the User Application 
struct ConfigResponse
{
  uint8_t     telegramType;         //!< 3= config response
  uint8_t     qVersion;             //!< Interface version
  uint8_t     sdpVerMajor;          //!< Major SDP SW Version
  uint8_t     sdpVerMid;            //!< Middle SDP SW Version
  uint8_t     sdpVerMinor;          //!< Minor SDP SW Version
  uint8_t     configStatus;         //!< Configuration Status
  uint8_t     calStatus;            //!< Callibration Status
  uint32_t    tDvTrain;             //!< TimeStamp
  uint32_t    calTachoDistance1;    //!< Measured distance (Tacho 1)
  uint8_t     tachoCalResultStatus1;//!< Calibration result status (Tacho 1)
  uint32_t    calTachoDistance2;    //!< Measured distance (Tacho 2)
  uint8_t     tachoCalResultStatus2;//!< Calibration result status (Tacho 2)
  uint32_t    calDopplerDistance;   //!< Measured Doppler radar distance
  uint8_t     dopplerCalResultStatus;//!< Calibration result status (Doppler radar)
};

#endif
