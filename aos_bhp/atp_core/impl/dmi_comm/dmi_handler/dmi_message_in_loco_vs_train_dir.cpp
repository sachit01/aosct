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
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"
#include "abstract_dmi_handler.hpp"
#include "dmi_message_in_loco_vs_train_dir.hpp"
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
    * DMIMessageInLocoVsTrainDir constructor
    ******************************************************************************/
    DMIMessageInLocoVsTrainDir::DMIMessageInLocoVsTrainDir() : AbstractDMIMessageIn(MTypeLocoVsTrainDir)
    {
      locoVsTrainDir = DMIATPLocoVsTrainDirUndefined;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInLocoVsTrainDir::validate()
    {
      trace->write(ATC::briefTrace, "DMI Handler: Validating DMI Incoming Message :Loco Vs Train Direction");
      writeToLog(ATC::DetailedLog, "DMI Handler: Validating DMI Incoming Message :Loco Vs Train Direction", __FILE__, __LINE__);

      // Parse, validate and publish data
      if (DMIDataIncomingAvailable == dmiDataInProcessState)
      {
        if (parseDMIMessageData())
        {
          if (validateMode())
          {          
            AbstractDMIHandler::corePtr()->setLocoVsTrainDirData(locoVsTrainDir);
            dmiDataInProcessState = DMIDataIncomingValidated;
          }
        }
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageInLocoVsTrainDir::invalidate()
    {
      locoVsTrainDir = DMIATPLocoVsTrainDirUndefined;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

     /******************************************************************************
    * getLocoVsTrainDir
    ******************************************************************************/
    LocoVsTrainDirection DMIMessageInLocoVsTrainDir::getLocoVsTrainDir() const
    {
      LocoVsTrainDirection tempLocoVsTrainDir;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        tempLocoVsTrainDir = locoVsTrainDir;
      }
      else
      {
        tempLocoVsTrainDir = DMIATPLocoVsTrainDirUndefined; 
      }

      return tempLocoVsTrainDir;
    }

    /******************************************************************************
    * DMIMessageInLocoVsTrainDir::parseMessageData
    ******************************************************************************/
    bool DMIMessageInLocoVsTrainDir::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      uint8_t tempLocoVsTrainDir = vfwGetU8(&buffer);

      if (!(tempLocoVsTrainDir <= static_cast<uint8_t>(DMIATPcarsConnectedAtAEnd)))
      {
        trace->write(ATC::detailedTrace, "DMI Handler: Invalid Loco Vs Train Direction");
        writeToLog(ATC::DetailedLog, "DMI Handler: Invalid Loco Vs Train Direction", __FILE__, __LINE__);
        parseDataValid = false;
      }
      else
      {
        locoVsTrainDir = static_cast<LocoVsTrainDirection>(tempLocoVsTrainDir);
      }

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageInLocoVsTrainDir::validateMode
    ******************************************************************************/
    bool DMIMessageInLocoVsTrainDir::validateMode() const
    {
      bool modeValid = false;

      if (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeConfiguration)
      {
        modeValid = true;
      }

      //Write the Trace regarding Mode 
      traceValidateMode(modeValid);

      return modeValid;
    }

  }
}
