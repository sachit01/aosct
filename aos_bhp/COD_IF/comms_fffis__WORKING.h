/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation, 2016
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: Comms
*
* %name: comms_fffis.h
* %version: 1 %
* %created_by: siguro %
* %date_created: 2016-04-15 12:46 %
* %Creation date of original object: Fri Nov 17 13:56:51 2013 %
*
* Description: SDP FFFIS types used for COMMS internal storing of message data
*              before packing to VFW output interfases.
*              And SDP FFFIS packed output telegram sizes.
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date   Sign     Change description
* 1        160415 siguro   Created. Task arn_068#1375.
*
*******************************************************************************/
#ifndef _COMMS_FFFIS_H
#define _COMMS_FFFIS_H

/*******************************************************************************
* Includes
*******************************************************************************/

/*******************************************************************************
* Global typedefs
*******************************************************************************/
namespace SDP_Comms_Names
{
  const uint8_t   NUM_FFFIS_MESSAGE_OUTPUT_BYTES(30U);            // 30 bytes, 240 bits, FFFIS Odometer Message
  const uint8_t   NUM_FFFIS_STATUS_MESSAGE_OUTPUT_BYTES(9U);      // 9 bytes,  72 bits,  FFFIS Odometer Status Message
  const uint8_t   NUM_FFFIS_PARAMETER_MESSAGE_OUTPUT_BYTES(23U);  // 23 bytes, 184 bits, FFFIS Odometer Parameter Message

  const uint8_t   VERIFY_BIT_OFFSET_1U(1U);  // 1 bit offset at last byte of packed data
  const uint8_t   VERIFY_BIT_OFFSET_2U(2U);  // 2 bits offset at last byte of packed data

  // COMMS internal storage of FFFIS Odometer msg (STM_8)
  typedef struct
  {                               // (preceeded by 8 + 8 + 8 bit at telegram)
    uint8_t  nidPacket;           // 8  bit at telegram
    uint16_t lPacket;             // 13 bit at telegram
    uint32_t tOdo;                // 32 bit at telegram
    int16_t  vMax;                // 16 bit at telegram
    int16_t  vNom;                // 16 bit at telegram
    int16_t  vMin;                // 16 bit at telegram
    int32_t  dMax;                // 32 bit at telegram
    int32_t  dNom;                // 32 bit at telegram
    int32_t  dMin;                // 32 bit at telegram
    uint8_t  dRes;                // 8 bit at telegram
    uint8_t  qSafedir;            // 1 bit at telegram
    uint8_t  qNomOdo;             // 1 bit at telegram
    uint8_t  mKetcs;              // 3 bit at telegram (plus 6 bit padding)
  } FfffisOdoPacket;

  // COMMS internal storage of FFFIS Odometer Status msg (STM_202) 
  typedef struct
  {                               // (preceeded by 8 + 8 + 8 bit at telegram)
    uint8_t  nidPacket;           // 8 bit at telegram
    uint16_t lPacket;             // 13 bit at telegram
    uint8_t  qSensorStatus;       // 8 bit at telegram
    uint8_t  qSlipSlide;          // 4 bit at telegram
    uint8_t  tRadarPlausible;     // 8 bit at telegram (plus 7 bits padding)
  } FfffisStatusPacket;

  // COMMS internal storage of FFFIS Odometer Parameter msg (STM_1 and STM_9)
  typedef struct
  {                               // (preceeded by 8 + 8 + 8 bit at telegram)
    uint8_t  nidPacket1; //STM_1  // 8 bit at telegram
    uint16_t lPacket1;            // 13 bit at telegram      
    uint8_t  n58VerMajor;         // 8 bit at telegram
    uint8_t  n58VerMid;           // 8 bit at telegram
    uint8_t  n58VerMinor;         // 8 bit at telegram
    uint8_t  n35VerMajor;         // 8 bit at telegram
    uint8_t  n35VerMid;           // 8 bit at telegram
    uint8_t  n35VerMinor;         // 8 bit at telegram
    uint8_t  nSrsVerMajor;        // 8 bit at telegram
    uint8_t  nSrsVerMinor;        // 8 bit at telegram

    uint8_t  nidPacket9; //STM_9  // 8 bit at telegram
    uint16_t lPacket2;            // 13 bit at telegram
    uint8_t  tOdoCycle;           // 8 bit at telegram
    uint8_t  tOdoMaxProd;         // 8 bit at telegram
    uint8_t  qVabs;               // 8 bit at telegram
    uint8_t  qVrel;               // 8 bit at telegram
    uint8_t  qDabs;               // 8 bit at telegram
    uint8_t  qDrel;               // 8 bit at telegram (plus 6 bit padding)
  } FfffisParameterPacket;
}
#endif

/*************************** end of file **************************************/