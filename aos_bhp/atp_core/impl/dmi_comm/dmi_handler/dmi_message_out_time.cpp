/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements the creator for the Time DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 09-06-2017    adgupta     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_handler.hpp"
#include "atc_util.hpp"
#include "dmi_message_out_time.hpp"
#include "abstract_message_handler.hpp"

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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutTime constructor
    ******************************************************************************/
    DMIMessageOutTime::DMIMessageOutTime() :AbstractDMIMessageOut(MTypeTime)
    {
      unixTime = 0U;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutTime::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message : Time message");

        if (unixTime != 0U)
        {
          if (AbstractDMIHandler::corePtr()->getDMICompatibilityVersionAccepted())
          {
            if (assembleDMIMessageData())
            {
              dmiDataProcessState = DMIDataValidated;
            }
          }
        }
      }

      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutTime::invalidate()
    {
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutTime::collectData()
    {
      //Collect data from TCC
      bool updateDMITime = Kernel::AbstractMessageHandler::corePtr()->getUpdateDMITime();

      if (updateDMITime)
      {
        uint32_t newTime = Kernel::AbstractMessageHandler::corePtr()->getTimeNew();
        uint32_t reqTime = Kernel::AbstractMessageHandler::corePtr()->getTimeAtReq();
        uint32_t sysTime;
        ATC::getUTCTime(sysTime);

        bool okToUpdateTime = false;

        // Below logic makes sure that the system time is updated on ATP before it is sent to DMI
        bool isNewTimeAheadOfSysTime = ((static_cast<int64_t>(newTime) - static_cast<int64_t>(reqTime))>=0);
        if (isNewTimeAheadOfSysTime)
        {
          if ((static_cast<int64_t>(newTime) - static_cast<int64_t>(sysTime)) <= 0)
          {
            okToUpdateTime = true;
          }
        }
        else
        {
          if (sysTime < reqTime)
          {
            okToUpdateTime = true;
          }
        }

        if (okToUpdateTime)
        {
          unixTime = sysTime;

          //Reset the Flag.
          Kernel::AbstractMessageHandler::corePtr()->setUpdateDMITime(false);

          dmiDataProcessState = DMIDataAvailable;
        }
      }
    }

    /******************************************************************************
    * DMIMessageOutTime::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutTime::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      //Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      //Get MSB for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      vfwPutU64(&buffer, unixTime);

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

  }
}
