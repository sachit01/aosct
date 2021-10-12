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
* %name: comms_dyn_conf.h
* %version: 1 %
* %created_by: siguro %
* %date_created: 2015-08-12 09:16 %
* %Creation date of original object: Fri Nov 17 13:56:51 2000 %
*
* Description: ATP CU telegram for dynamic configuration message
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
#ifndef _COMMS_DYN_CONF_H
#define _COMMS_DYN_CONF_H

/*******************************************************************************
* Includes
*******************************************************************************/

/*******************************************************************************
* Global typedefs
*******************************************************************************/

// Dynamic configuration data from User Application (31 bytes, 248 bits)
typedef struct
{
  uint8_t                configTgmType;
  uint8_t                qVersion;
  uint16_t               aMaxAcc;
  uint16_t               aMaxDec;
  uint8_t                gCur;
  uint8_t                qGdir;
  uint8_t                gradType;
  uint16_t               tachoWheelSize1;
  uint8_t                tachoWheelSizeError1;
  uint16_t               tachoWheelSize2;
  uint8_t                tachoWheelSizeError2;
  uint32_t               dopplerPulse;
  uint8_t                dopplerPrecision;
  uint8_t                calibrationFlag;
  uint64_t               calibrationSyncTime; //Start or stop time for calibration (ns)
  uint8_t                appliedBrakeLevel;
  uint8_t                pzbLzbDtrainDirection;
} DynamicConfigType;

#endif
 
/*************************** end of file **************************************/