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
* 2016-03-13    bhermans    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-06-27    adgupta     Implementation of BasicIP functionality
* 2016-09-08    adgupta     Preliminary implementation of bip console call
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    bhermans    Improved bip command
* 2016-09-26    spandita    Added case statement for other OPC connection ID
* 2016-09-28    adgupta     Added case statement for BTM connection ID
* 2016-10-05    spandita    Removed Extra break
* 2016-12-16    saprasad    Added case statement for AIF connection ID
*
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
namespace ATP
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
  *
  ******************************************************************************/
  BasicIP& BasicIP::instance(void)
  {
    static BasicIP theOnlyBasicIpInstance;

    return theOnlyBasicIpInstance;
  }

  /******************************************************************************
  * connectionIdStr
  *
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

    case connectionRU:
      idStr = "RU";
      break;

#ifdef _SIL
    case connectionSimTCC1:
      idStr = "TCC1";
      break;

    case connectionSimTCC2:
      idStr = "TCC2";
      break;

    case connectionSimTCC3:
      idStr = "TCC3";
      break;

    case connectionSimMMI1:
      idStr = "DMI1";
      break;

    case connectionSimMMI2:
      idStr = "DMI2";
      break;

    case connectionSimOdometryConfig:
      idStr = "OdoCnf";
      break;

    case connectionSimOdometryConfigResponse:
      idStr = "OdoCnfResp";
      break;

    case connectionSimOdometryMeas:
      idStr = "OdoMeas";
      break;

    case connectionSimCODConfig:
      idStr = "CODCnf";
      break;

    case connectionSimCODConfigResponse:
      idStr = "CODCnfResp";
      break;

    case connectionSimCODMeas:
      idStr = "CODMeas";
      break;

    case connectionSimCODSim:
      idStr = "CODSim";
      break;

    case connectionSimVIOHSim:
      idStr = "VIOHSim";
      break;

    case connectionSimOPCSimBTMCommand:
      idStr = "OPCSimCmd";
      break;

    case connectionSimOPCSimBTMTelegram:
      idStr = "OPCSimTel";
      break;

    case connectionSimBTMHandlerBTMCommand:
      idStr = "BTMCmd";
      break;

    case connectionSimBTMhandlerTelegram:
      idStr = "BTMTel";
      break;

    case connectionSimBTMhandlerAppStatus:
      idStr = "BTMApp";
      break;

    case connectionSimBTMhandlerToOpcAppStatus:
      idStr = "BTMToOPC";
      break;

    case connectionSimBTMhandlerClockSync:
      idStr = "BTMClock";
      break;

    case connectionSimLCS:
      idStr = "LCS";
      break;

    case connectionSimBTMhandlerTigrisOffset:
      idStr = "TigOff";
      break;

    case connectionSimBTMhandlerToOpcTigrisOffset:
      idStr = "OPCTigOff";
      break;

    case connectionSimOPCSim:
      idStr = "OPCSim";
      break;

    case connectionSimOBRDSim:
      idStr = "OBRDSim";
      break;
#endif

    case connectionAnalyzerIF:
      idStr = "AIF";
      break;

    default:
      idStr = "Unknown";
      break;
    }
    return idStr;
  }
  /******************************************************************************
  * getMaxConnection
  *
  ******************************************************************************/
  uint8_t BasicIP::getMaxConnections()
  {
    return connectionMax;
  }

}
