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
* This file implements the creator for the outgoing Loco Vs Train Dir DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_out_loco_vs_train_dir.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_tsetup.hpp"

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
    * DMIMessageOutLocoVsTrainDir constructor
    ******************************************************************************/
    DMIMessageOutLocoVsTrainDir::DMIMessageOutLocoVsTrainDir() : AbstractDMIMessageOut(MTypeLocoVsTrainDir)
    {
      locoVsTrainDirOut = DMIATPLocoVsTrainDirUndefined;
     
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageOutLocoVsTrainDir::validate()
    {
      // Assemble, validate and publish data
      if (DMIDataAvailable == dmiDataProcessState)
      {
        trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Message :Loco Vs Train Direction");

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
    void DMIMessageOutLocoVsTrainDir::invalidate()
    {
      //Clear all data
      locoVsTrainDirOut = DMIATPLocoVsTrainDirUndefined;
      dmiDataProcessState = DMINoDataAvailable;
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void DMIMessageOutLocoVsTrainDir::collectData()
    {
      //Fetch the bit for Car Connected from train setup
      dmiDataProcessState = DMINoDataAvailable;
      ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      Kernel::TrainConfigModeState trainConfigModeState = Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();

      if ((ATPModeConfiguration == currentMode) &&
         (Kernel::TrainConfigMode::trainConfigTSetupAccepted == trainConfigModeState))
      {
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          if ((pTrainSetup->orientation & trainLocoOrientation) == 0U)
          {
            locoVsTrainDirOut = DMIATPcarsConnectedAtBEnd;
          }
          else
          {
            locoVsTrainDirOut = DMIATPcarsConnectedAtAEnd;
          }
          dmiDataProcessState = DMIDataAvailable;
        }
      }
      else if ((ATPModeConfiguration == currentMode) &&
        (Kernel::TrainConfigMode::trainConfigSendReRegDataToDMI == trainConfigModeState))
      {
        // Need to send LocoVsTrainDir during Re-registration to be accepted by driver
        DS::TrainSetup prelTrainSetup;
        if (DS::AbstractTSetup::corePtr()->getPreliminaryTrainSetup(prelTrainSetup))
        {
          if ((prelTrainSetup.orientation & trainLocoOrientation) == 0U)
          {
            locoVsTrainDirOut = DMIATPcarsConnectedAtBEnd;
          }
          else
          {
            locoVsTrainDirOut = DMIATPcarsConnectedAtAEnd;
          }
          dmiDataProcessState = DMIDataAvailable;
        }
      }
      else
      {
        // No reason to send message
      } 
    }

    /******************************************************************************
    * DMIMessageOutLocoVsTrainDir::assembleDMIMessageData
    ******************************************************************************/
    bool DMIMessageOutLocoVsTrainDir::assembleDMIMessageData()
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

      // Assemble data and write in network order
      vfwPutU8(&buffer, static_cast<uint8_t>(locoVsTrainDirOut));

      // Total length of message
      messageData.msgLen = static_cast<uint16_t>(vfwGetValidSize(&buffer))
        + static_cast<uint16_t>(sizeof(messageData.dmiData.msgType));

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }
  }
}
