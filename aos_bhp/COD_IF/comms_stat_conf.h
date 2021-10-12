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
* %name: comms_stat_conf.h
* %version: 3 %
* %created_by: siguro %
* %date_created: 2015-09-07 14:03 %
* %Creation date of original object: Fri Nov 17 13:56:51 2000 %
*
* Description: ATP CU interface for static configuration message
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version Date   Sign     Change description
*
* 3       150907 siguro   Updated after Lint. Task arn_068#978.
* 2       150901 siguro   Removed BTM parameter from static configuration. Task arn_068#977.
* 1       150811 siguro   Created for first release in R4. Task arn_068#918.
*
*******************************************************************************/
#ifndef _COMMS_STAT_CONF_H
#define _COMMS_STAT_CONF_H

/*******************************************************************************
* Includes
*******************************************************************************/

//#include "vss_a_scalar_types.h"

/*******************************************************************************
* Global typedefs
*******************************************************************************/

// Static configuration data from ATP CU (58 bytes, 464 bits)

typedef struct
{
  uint8_t                configTgmType;        
  uint8_t                qVersion;
  uint8_t                sensorConfig;
  uint8_t                mContrTraction;
  uint8_t                maxPulseCounterFrequency;
  uint8_t                tachoDirection1;
  uint16_t               tachoPulse1;          // Input pulse count from tachometer, number of pulses per 10 revolutions.
  uint8_t                tachoDirection2;
  uint16_t               tachoPulse2;
  uint16_t               vLowDoppler;
  uint16_t               vHighDoppler;
  uint16_t               aMaxAccDoppler;
  uint8_t                ch1OdoMeasInterval;
  uint8_t                ch1FffisOdoMsgInterval;
  uint8_t                ch1FffisParamMsgInterval;
  uint8_t                ch1FffisStatusMsgInterval;
  uint8_t                ch1PzbLzbMeasInterval;
  uint8_t                ch2OdoMeasInterval;
  uint8_t                ch2FffisOdoMsgInterval;
  uint8_t                ch2FffisParamMsgInterval;
  uint8_t                ch2FffisStatusMsgInterval;
  uint8_t                ch2PzbLzbMeasInterval;
  uint8_t                ch3OdoMeasInterval;
  uint8_t                ch3FffisOdoMsgInterval;
  uint8_t                ch3FffisParamMsgInterval;
  uint8_t                ch3FffisStatusMsgInterval;
  uint8_t                ch3PzbLzbMeasInterval;
  uint8_t                ch4OdoMeasInterval;
  uint8_t                ch4FffisOdoMsgInterval;
  uint8_t                ch4FffisParamMsgInterval;
  uint8_t                ch4FffisStatusMsgInterval;
  uint8_t                ch4PzbLzbMeasInterval;
  uint8_t                ch5OdoMeasInterval;
  uint8_t                ch5FffisOdoMsgInterval;
  uint8_t                ch5FffisParamMsgInterval;
  uint8_t                ch5FffisStatusMsgInterval;
  uint8_t                ch5PzbLzbMeasInterval;
  uint8_t                ch6OdoMeasInterval;
  uint8_t                ch6FffisOdoMsgInterval;
  uint8_t                ch6FffisParamMsgInterval;
  uint8_t                ch6FffisStatusMsgInterval;
  uint8_t                ch6PzbLzbMeasInterval;
  uint8_t                ch7OdoMeasInterval;
  uint8_t                ch7FffisOdoMsgInterval;
  uint8_t                ch7FffisParamMsgInterval;
  uint8_t                ch7FffisStatusMsgInterval;
  uint8_t                ch7PzbLzbMeasInterval;
  uint8_t                ch8OdoMeasInterval;
  uint8_t                ch8FffisOdoMsgInterval;
  uint8_t                ch8FffisParamMsgInterval;
  uint8_t                ch8FffisStatusMsgInterval;
  uint8_t                ch8PzbLzbMeasInterval;
  uint8_t                powerSuppliedSensors;
} StaticConfigType;

#endif
 
/*************************** end of file **************************************/
