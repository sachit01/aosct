#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OBRDSimTypes.h %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2018-10-12 18:40 %
*
*  DESCRIPTION:     Declaration of types and constants used in OBRDSim
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-12    Marlundg    File created
*
*******************************************************************************/

namespace OBRDSimDLL {
  
#define UPDATE_IF_DIFFERENT(a, b) { if (a != (b)) {a = (b);}}

  /*******************************************************/
  /* Constants                                           */
  /*******************************************************/

  // Max last car brake pressure
  const unsigned char max = 255;  // 1-254 are valid values, 255 = Measurement not available

  // Max number of tracks to be handled
  const int maxTracks = 1000;

  // Max number of paths from LCS
  const unsigned short pathArraySize = 200;

  // Size of stored front positions from LCS (every 500ms)
  const unsigned short frontPosArraySize = 10;

  // Packet definitions
  const unsigned short RNID_PROTOCOL_PACKET = 1;
  const unsigned short RL_PROTOCOL_PACKET = 6;

  const unsigned short RNID_STATUS_PACKET = 10;
  const unsigned short RL_STATUS_PACKET = 19;

  const unsigned short RNID_REJECT_PACKET = 2;
  const unsigned short RL_REJECT_PACKET = 7;

  /*******************************************************/
  /* Enums                                               */
  /*******************************************************/

  // Simulator states
  typedef enum
  {
    OBRDStateNotConnected = 0,
    OBRDStateConnectToAtp,
    OBRDStateProtocolVerification,
    OBRDStateNormal
  } OBRDStateEnum;

  // Error injections
  typedef enum {
    FINoError = 0,
    FIFreezePOS = 1,
    FIFailedToReadGPS = 2,
    FICRCFailure = 3,
    FIIgnoreRejection = 4,
    FIPosErrorOffset = 5,
    FIOverrideBrakePressure = 6
  } EnumFaultInjection;

  value struct posItem
  {
    unsigned short trackId;
    unsigned long position;
    unsigned long long timeStamp;
  };

  value struct pathItem
  {
    unsigned short trackId;
    unsigned long length;
    bool locoTowardsLeg1;
  };
  
  unsigned long long getUTCTime();
}
