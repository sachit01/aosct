/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation, 2015
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: Comms
*
* %name: comms_odo_measurement.h
* %version: 1 %
* %created_by: siguro %
* %date_created: 2015-08-12 09:16 %
* %Creation date of original object: Fri Nov 17 13:56:51 2000 %
*
* Description: SDP Odo Measurement telegram structure to ATP_CU
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version Date   Sign     Change description
*
* 1       150811 siguro   Created for first release in R4. Task arn_068#918.
*
*******************************************************************************/
#ifndef _COMMS_ODO_MEASURE_H
#define _COMMS_ODO_MEASURE_H

/*******************************************************************************
* Includes
*******************************************************************************/

/*******************************************************************************
* Global typedefs
*******************************************************************************/

// Odometer measurement data telegram to User Application (55 bytes)
typedef struct
{
  uint8_t             telegramType;
  uint8_t             qVersion;
  uint8_t             qOdoSafe;
  uint16_t            qControl;
  uint8_t             qDirErr;  
  uint32_t            tDvTrain;
  uint32_t            prodTime;
  int16_t             aTrain;
  int16_t             vMax;            
  int16_t             vNom;
  int16_t             vMin;
  int32_t             dMax;
  int32_t             dNom;
  int32_t             dMin;           
  uint8_t             tRadarPlausible; 
  uint8_t             slipSlideStatus1;
  uint8_t             slipSlideStatus2;
  int16_t             vTacho1;
  int16_t             vTacho2;
  int16_t             vDoppler;
  int32_t             dTacho1;
  int32_t             dTacho2;
  int32_t             dDoppler;
} OutputPacketType1;

#endif
 
/*************************** end of file **************************************/