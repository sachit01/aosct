/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class implements the application- and project-specific adaptations
*  for BasicIP
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-11-03    nsyed    Created
* 2016-11-16    adgupta  Updated after design
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "basic_ip.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace Dispatcher
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  BasicIP::BasicIP() : AbstractBasicIP(&connectionCCB[0], connectionMax)
  {
    // Construct the connection block as all reset
    memset(&connectionCCB[0], 0, (connectionMax * sizeof(ConnectionControlBlock)));
  }

  /******************************************************************************
  * instance
  ******************************************************************************/
  BasicIP& BasicIP::instance(void)
  {
    static BasicIP theOnlyBasicIpInstance;

    return theOnlyBasicIpInstance;
  }

  /******************************************************************************
  * connectionIdStr
  ******************************************************************************/
  const char_t * BasicIP::connectionIdStr(const uint8_t connectionId)
  {
    const char_t *idStr;
    switch (connectionId)
    {
    case connectionConsole:
      idStr = "Console";
      break;

    case connectionNJRU:
      idStr = "NJRU";
      break;

    case connectionIOSim:
      idStr = "IOSim";
      break;

    case connectionSpeedSim:
      idStr = "SpeedSim";
      break;

    case connectionOPCSim:
      idStr = "OPCSim";
      break;

    case connectionLIG:
      idStr = "LIG";
      break;

    case connectionDMI1:
      idStr = "DMI1";
      break;
    
    case connectionDMI2:
      idStr = "DMI2";
      break;
    
    case connectionTCC1:
      idStr = "TCC1";
      break;
    
    case connectionTCC2:
      idStr = "TCC2";
      break;

    case connectionTCC3:
      idStr = "TCC3";
      break;

    case connectionOPCClient:
      idStr = "OPCCli";
      break;

    case connectionOPCServer:
      idStr = "OPCServ";
      break;

    case connectionOPCAppStatusServer:
      idStr = "OPCAppSta";
      break;

    case connectionOPCProfibusClient:
      idStr = "OPCProfi";
      break;

    case connectionOBRD:
      idStr = "OBRDServ";
      break;

    case connectionATP:
      idStr = "ATP";
      break;

    default:
      idStr = "Unknown";
      break;
    }
    return idStr;
  }

  /******************************************************************************
  * getMaxConnection
  ******************************************************************************/
  uint8_t BasicIP::getMaxConnections()
  {
    return connectionMax;
  }
}
