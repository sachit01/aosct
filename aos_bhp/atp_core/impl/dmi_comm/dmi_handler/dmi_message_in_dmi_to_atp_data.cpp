/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each DMI messageType (DMI->AOS) has an associated parser class inherited from AbstractDMIMessageIn.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
* 2016-10-12    arastogi    Fixed comparison when cheking for invalid button.
* 2017-04-11    skothiya    updated for implementation of cabin handling and authorization
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_in_dmi_to_atp_data.hpp"
#include <vfw_checkpoints.h>
#include <cstdio>
#include "abstract_dmi_handler_event_ids.hpp"

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
      * DMIMessageInDMIToATPData constructor
      ******************************************************************************/
      DMIMessageInDMIToATPData::DMIMessageInDMIToATPData() :
        AbstractDMIMessageIn(MTypeMMIToATPData),
        driverInteractions(ATC::Event::createLogEvent(atpDMIHandlerId, ATC::CoreContainer,
          eventIdDriverInteractions, 0U, "Driver Interactions, DMI Button:", true))
      {
         buttonStatus = DMIButtonUndefined;
      }

      /******************************************************************************
      * validate
      ******************************************************************************/
      bool DMIMessageInDMIToATPData::validate()
      {
         trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Incoming Message :DMI To ATP Data");
         writeToLog(ATC::DetailedLog, "DMI Handler: Validating DMI Incoming Message :DMI To ATP Data", __FILE__, __LINE__);

         // Parse, validate and publish data
         if (DMIDataIncomingAvailable == dmiDataInProcessState)
         {
            if (parseDMIMessageData())
            {
               //prepare the dynamic text to be send while reporting event.
               driverInteractions.setDynamicText(static_cast<uint32_t>(buttonStatus));
               ATC::AbstractEventHandler::corePtr()->reportEvent(driverInteractions, __FILE__, __LINE__);

               dmiDataInProcessState = DMIDataIncomingValidated;
            }
         }

         return (DMIDataIncomingValidated == dmiDataInProcessState);
      }

      /******************************************************************************
      * invalidate
      ******************************************************************************/
      void DMIMessageInDMIToATPData::invalidate()
      {
         buttonStatus = DMIButtonUndefined;
         dmiDataInProcessState = DMINoDataIncomingAvailable;
      }

      /******************************************************************************
      * getDMIbuttonStatus
      ******************************************************************************/
      DMIButtonStatus DMIMessageInDMIToATPData::getDMIButtonStatus() const
      {
         DMIButtonStatus tempButtonStatus;

         if (DMIDataIncomingValidated == dmiDataInProcessState)
         {
            static uint32_t cp = 0U; // Must be initialized to 0
            vfwVisitCheckPoint(&cp, "DH_getDMIButtonStatus");

            tempButtonStatus = buttonStatus;
         }
         else
         {
            tempButtonStatus = DMIButtonUndefined;

         }

         return tempButtonStatus;
      }

      /******************************************************************************
      * DMIMessageInDMIToATPData::parseMessageData
      ******************************************************************************/
      bool DMIMessageInDMIToATPData::parseDMIMessageData()
      {
         bool parseDataValid = true;

         VFW_Buffer buffer;

         // Initialize buffer to first byte of Application level message
         vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
         vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

         uint8_t tempButtonStatus = vfwGetU8(&buffer);

         // Check if button is valid
         if (validateAllowedButtonStatus(static_cast<DMIButtonStatus>(tempButtonStatus)))
         {
            buttonStatus = static_cast<DMIButtonStatus>(tempButtonStatus);
         }
         else
         {
            trace->write(ATC::detailedTrace, "DMI Handler: Invalid DMI Button Status");
            writeToLog(ATC::DetailedLog, "DMI Handler: Invalid DMI Button Status", __FILE__, __LINE__);
            parseDataValid = false;
         }

         // Write the Trace regarding Parsing of Data
         traceParseData(parseDataValid);

         return parseDataValid;
      }

      /******************************************************************************
      * DMIMessageInDMIToATPData::validateAllowedButtonStatus
      ******************************************************************************/
      bool DMIMessageInDMIToATPData::validateAllowedButtonStatus(const DMIButtonStatus button) const
      {
      const bool allowedButton = ((button != DMIButtonUndefined) && 
            (button < DMIButtonMaxCount));
         return allowedButton;
      }
   }
}
