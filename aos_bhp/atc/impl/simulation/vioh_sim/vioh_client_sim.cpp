/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is a fake/simulated implementation that replaces the real implementation
*  of the VIOH Client. Most functions in this simulation will be stubbed to return
*  a success while apart from a few functions which re-direct to access functions
*  of VIOH Sim class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-28    adgupta    Created
* 2016-06-13    adgupta    Added VOU function implementations simulation.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"
#include <vfw_sync.h>
#include "vio_types.h"
#include <cstring>
#include "vioh_client.hpp"
#include "vioh_sim.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

namespace
{
  bool gpioRegisterSvdCalled = false;
  uint8_t relayCounter = 0U;

  bool_t vioActiveOrder = ATC::falseVfw;
  bool_t vioFeedback = ATC::falseVfw;
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace VIOHnames
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  VIOHClient::VIOHClient() :myclientId(0U),
    mytaskType(enTTVITALSWIT),
    mytaskCycle(0U)
  {

  }

  /******************************************************************************
  * Constructor
  ******************************************************************************/

  VIOHClient::VIOHClient(const uint32_t clientId,
    const VIOH_taskType taskType,
    const uint32_t taskCycle)
    :
    myclientId(clientId),
    mytaskType(taskType),
    mytaskCycle(taskCycle)
  {

  }  // VIOHClient::VIOHClient

  /******************************************************************************
  * VIOHClientInit
  ******************************************************************************/
  void VIOHClient::VIOHClientInit(const uint32_t /*clientID*/)
  {

  }

  /******************************************************************************
  * Update
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::Update()
  {
    if (vioActiveOrder != vioFeedback)
    {
      if (relayCounter > 2U)
      {
        relayCounter = 0U;
        vioFeedback = vioActiveOrder;
      }
      else
      {
        ++relayCounter;
      }
    }

    return enCRT_OK;
  }

  /******************************************************************************
  * GetSwVersion
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GetSwVersion(char* const pClientVersion,
    char* const pServerVersion)
  {
    const char_t clientVersionString[] = "1.8.00";
    const char_t serverVersionString[] = "1.8.00";

    VIOH_clientResultType result = enCRT_OKNU;

    if (gpioRegisterSvdCalled)
    {
      memcpy(pClientVersion, clientVersionString, sizeof(clientVersionString));
      memcpy(pServerVersion, serverVersionString, sizeof(serverVersionString));

      result = enCRT_OK;
    }

    return result;
  }

  /******************************************************************************
  * Status
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::Status(VIOH_clientStatusType* const pStatus)
  {
    *pStatus = VIOHnames::enCST_RUNOK;

    return enCRT_OK;
  }

  /******************************************************************************
  * CrossCompare
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::CrossCompare()
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * VIURegister
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIURegister(const VIOH_listType * const pInputList)
  {
    return ATC::Sim::VIOHSim::corePtr()->VIURegister(pInputList);
  }

  /******************************************************************************
  * VIURegisterResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIURegisterResult(VIOH_confRespType * const pResult)
  {
    *pResult = VIOHnames::enCFG_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * VIUGetState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIUGetState(const uint32_t Id,
    bool_t * const pbState,
    VIOH_healthStateType* const pHealthState)
  {
    return ATC::Sim::VIOHSim::corePtr()->VIUGetState(Id, pbState, pHealthState);
  }

  /******************************************************************************
  * VIUGetDeviceState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIUGetDeviceState(VIOH_healthStateType * const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * VIUGetRevisionId
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIUGetRevisionId(uint32_t* const /*pRevisionId*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * VIUGetHWConf
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VIUGetHWConf(uint8_t* const /*pChannelId*/,
    VIOH_InputVoltageType* const /*pVoltageRange*/,
    VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * VOURegister
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOURegister(const VIOH_listType * const /*pUsedOutp*/,
    const VIOH_listType * const /*pSwOutp*/,
    const VIOH_listType * const /*pInitOutp*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * VOURegisterSingleOutput
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOURegisterSingleOutput(const uint32_t /*Id*/,
    const bool_t /*bSwitchable*/,
    const bool_t /*bInitVal*/ )
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * VOURegisterResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOURegisterResult(VIOH_confRespType* const pResult)
  {
    *pResult = enCFG_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * VOUSetOutput
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOUSetOutput(const uint32_t  Id,
    const bool_t    bIsActive,
    const bool_t    bIsSync
  )
  {
    return ATC::Sim::VIOHSim::corePtr()->VOUSetOutput(Id, bIsActive, bIsSync);
  }

  /******************************************************************************
  * VOUGetState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOUGetState(const uint32_t Id,
    bool_t * const pbIsActive,
    VIOH_healthStateType* const pHealthState)
  {
    const VIOH_clientResultType result = ATC::Sim::VIOHSim::corePtr()->VOUGetState(Id, pbIsActive, pHealthState);
    
    // Maybe VIOHSim should simulate this?
    if ((*pbIsActive != ATC::falseVfw) && (vioFeedback == ATC::falseVfw))
    {
      *pbIsActive = ATC::falseVfw;
    }

    return result;
  }

  /******************************************************************************
  * VOUGetRevisionId
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOUGetRevisionId(uint32_t* const /*pRevisionId*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * VOUGetHWConf
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOUGetHWConf(uint8_t* const /*pNumOutputs*/,
    uint8_t* const /*pVOUConfType*/,
    VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * VOUGetDeviceState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::VOUGetDeviceState(VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * AIOURegisterSync
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOURegisterSync(const VIOH_listType * const /*pInputs*/,
    const VIOH_listType * const /*pOutputs*/,
    const uint16_t* const /*pLimitLow*/,
    const uint16_t* const /*pLimitHigh*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * AIOURegisterResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOURegisterResult(VIOH_confRespType* const pResult)
  {
    *pResult = VIOHnames::enCFG_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * AIOUGetState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOUGetState(const uint8_t Id,
    uint16_t *const pValue,
    VIOH_healthStateType* const pHealthState)
  {
    return ATC::Sim::VIOHSim::corePtr()->AIOUGetState(Id, pValue, pHealthState);
  }

  /******************************************************************************
  * AIOUSetOutput
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOUSetOutput(const uint8_t /*Id*/,
    const uint16_t /*value*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * AIOUGetDeviceState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOUGetDeviceState(VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * AIOUGetRevisionId
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::AIOUGetRevisionId(uint32_t* const /*pRevisionId*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * OSURegisterSerial
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSURegisterSerial(uint8_t const /*channel*/,
    uint32_t const /*baudrate*/,
    uint8_t const /*charLen*/,
    uint8_t const /*stopbit*/,
    uint8_t const /*parity*/,
    uint8_t const /*duplex*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * OSURegisterSerialResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSURegisterSerialResult(uint8_t const /*channel*/,
    VIOH_confRespType* const /*result*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUReadSerial
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUReadSerial(uint32_t const /*channel*/,
    uint8_t* const /*pBuffer*/,
    uint32_t const /*buffersize*/,
    bool_t const /*bLast*/,
    uint32_t* const /*pReadLength*/,
    uint16_t* const /*pRecvTimeBegin*/,
    uint64_t* const /*pRecvTimeBeginConv*/,
    uint16_t* const /*pRecvTimeEnd*/,
    uint64_t* const /*pRecvTimeEndConv*/,
    VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUGetSerialRXState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUGetSerialRXState(uint32_t const /*channel*/,
    uint16_t* const /*pParityErrCount*/,
    uint16_t* const /*pFramingErrCount*/,
    bool_t* const /*pbRxOverrun*/,
    bool_t* const /*pbRxResynch*/,
    VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUWriteSerial
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUWriteSerial(uint32_t const /*channel*/,
    uint8_t* const /*pBuffer*/,
    uint32_t const /*writeLength*/,
    VIOH_healthStateType* const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUSetBreakSerial
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUSetBreakSerial(uint32_t const /*channel*/,
    bool_t const /*bOn*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUGetDeviceState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUGetDeviceState(VIOH_healthStateType* const healthState)
  {
    *healthState = VIOHnames::VIORes_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * OSUGetRevisionId
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::OSUGetRevisionId(uint32_t* const /*pRevisionId*/,
    uint16_t* const /*FPGArevisionId*/)
  {
    return enCRT_OK;
  }


  /******************************************************************************
  * GPIORegisterSVD
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIORegisterSVD()
  {
    gpioRegisterSvdCalled = true;

    return enCRT_OK;
  }

  /******************************************************************************
  * GPIORegisterDisplay
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIORegisterDisplay(const bool_t /*bShared*/,
    const VIOH_displayTextType * const /*pInitialText*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * GPIORegisterSVDResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIORegisterSVDResult(VIOH_confRespType* const pResult)
  {
    *pResult = VIOHnames::enCFG_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * GPIORegisterDisplayResult
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIORegisterDisplayResult(VIOH_confRespType* const pResult)
  {
    *pResult = VIOHnames::enCFG_OK;
    return enCRT_OK;
  }

  /******************************************************************************
  * GPIODisplayIsBusy
  ******************************************************************************/
  bool_t VIOHClient::GPIODisplayIsBusy()
  {
    return true;
  }

  /******************************************************************************
  * GPIOTriggerSVD
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIOTriggerSVD(const bool_t on)
  {
    VIOH_clientResultType value = enCRT_NOREG;

    if (gpioRegisterSvdCalled)
    {
      if (vioActiveOrder != on)
      {
        vioActiveOrder = on;
        relayCounter = 0U;
        vioFeedback = (vioActiveOrder == ATC::falseVfw) ? ATC::trueVfw : ATC::falseVfw;
      }

      value = enCRT_OK;
    }

    return value;
  }


  /******************************************************************************
  * GPIOTriggerSafe
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIOTriggerSafe(const bool_t on)
  {
    VIOH_clientResultType value = enCRT_NOREG;

    if (gpioRegisterSvdCalled)
    {
      if (vioActiveOrder != on)
      {
        vioActiveOrder = on;
        relayCounter = 255U; // Safe will trigger immediately...
        vioFeedback = (vioActiveOrder == 0) ? ATC::trueVfw : ATC::falseVfw;
      }

      value = enCRT_OK;
    }

    return value;
  }


  /******************************************************************************
  * GPIOSetDisplay
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIOSetDisplay(const VIOH_displayTextType * const /*pText*/,
    const VIOH_brightnessType /*brightness*/,
    const bool_t /*bBlinking*/,
    const bool_t /*bOverwrite*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * GPIOGetSVDState
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIOGetSVDState(bool_t * const pbState,
    bool_t * const pbFeedback,
    VIOH_healthStateType * const pHealthState)
  {
    *pHealthState = VIOHnames::VIORes_OK;

    if (vioActiveOrder != vioFeedback)
    {
        *pHealthState = VIORes_PSVDU_TIMEOUT;
    }

    *pbFeedback = vioFeedback;
    *pbState = vioActiveOrder;

    return enCRT_OK;
  }

  /******************************************************************************
  * GPIOGetRevisionId
  ******************************************************************************/
  VIOH_clientResultType VIOHClient::GPIOGetRevisionId(uint32_t* const /*SVDrevisionId*/,
    uint32_t* const /*DDRevisionId*/)
  {
    return enCRT_OK;
  }

  /******************************************************************************
  * Destructor
  ******************************************************************************/
  VIOHClient::~VIOHClient()
  {

  }

  /******************************************************************************
  * VIOHClientInit2SyncDeact
  ******************************************************************************/
  void VIOHClient::VIOHClientInit2SyncDeact()
  {

  }
} // namespace VIOHnames


  /*************************** end of file **************************************/

