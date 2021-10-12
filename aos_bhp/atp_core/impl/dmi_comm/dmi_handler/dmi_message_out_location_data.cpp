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
* This file implements the creator for the outgoing Location Data DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-05-15    skothiya    Created
* *******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_location_data.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_targets.hpp"
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
    * DMIMessageOutLocationData constructor
    ******************************************************************************/
    DMIMessageOutLocationData::DMIMessageOutLocationData() :AbstractDMIMessageOut(MTypeLocationData)
    {
      memset(&locationName[0], 0, sizeof(locationName));
      locationType = 0U;

    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutLocationData::validate()
    {

      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Location Data");
        if (assembleDMIMessageData())
        {
          dmiDataProcessState = DMIDataValidated;
        }
      }
      return(DMIDataValidated == dmiDataProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageOutLocationData::invalidate()
    {
      //clear all data
      memset(&locationName[0], 0, sizeof(locationName));
      locationType = static_cast<uint8_t>(UndefinedLocationType);
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutLocationData::collectData()
    {
      const ATPMode curMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      bool locationNameValid = false;
      if (curMode == ATPModeLocation)
      {
        if (Kernel::LocationMode::locationStart == Kernel::AbstractModeControl::corePtr()->getLocationModeState())
        {
          DS::BaseTarget* bTarget = DS::AbstractTargets::corePtr()->getLocationEndTarget();

          if(bTarget != static_cast<DS::BaseTarget*>(NULL))
          {
            const DS::PrimaryTarget* const locTarget =
              ATC::dynamicCast<DS::BaseTarget*, DS::PrimaryTarget*>(bTarget, __FILE__, __LINE__);

            locationNameValid = locTarget->getLocationName(&locationName[0]);
            locationType = static_cast<uint8_t>(locTarget->getLocationType());
          }

        }

        if (locationNameValid)
        {
          dmiDataProcessState = DMIDataAvailable;
        }
        else
        {
          trace->write(ATC::veryDetailedTrace, "DMI Handler: Invalid Location Name");
          writeToLog(ATC::VeryDetailedLog, "DMI Handler: Invalid Location Name", __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * DMIMessageOutLocationData::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutLocationData::assembleDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));

      //Header Type
      messageData.headerType = dmiHeaderTypeAckMsg;
      //Message Number
      messageData.msgNumber = AbstractDMIHandler::corePtr()->getNextMessageNumber();

      //Set most significant bit for the acknowledged DMIMessageType 
      messageData.dmiData.msgType = static_cast<uint8_t>(static_cast<uint8_t>(messageType) | 0x80U);

      // Assemble data and write in network order
      vfwCpyFromRawBuffer(&buffer, &locationName[0], static_cast<uint32_t>(maxLocationNameLength));
      vfwPutU8(&buffer, locationType);
      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
