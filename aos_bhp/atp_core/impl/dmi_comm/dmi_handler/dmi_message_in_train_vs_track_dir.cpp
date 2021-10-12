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
#include "dmi_message_in_train_vs_track_dir.hpp"
#include "abstract_mode_control.hpp"

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
    * DMIMessageInTrainVsTrackDir constructor
    ******************************************************************************/
    DMIMessageInTrainVsTrackDir::DMIMessageInTrainVsTrackDir() : AbstractDMIMessageIn(MTypeTrainVsTrackDir)
    {
      trainVsTrackDir = DMIATPTrainVsTrackDirectionUndefined;
    }

    /******************************************************************************
    * validate
    ******************************************************************************/
    bool DMIMessageInTrainVsTrackDir::validate()
    {
      trace->write(ATC::briefTrace, "Validating DMI Incoming Message :Train Vs Track Direction");

      // Parse, validate and publish data
      if (DMIDataIncomingAvailable == dmiDataInProcessState)
      {
        if (parseDMIMessageData())
        {
          if (validateMode())
          {
            dmiDataInProcessState = DMIDataIncomingValidated;
          }
        }
      }

      return (DMIDataIncomingValidated == dmiDataInProcessState);
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void DMIMessageInTrainVsTrackDir::invalidate()
    {
      trainVsTrackDir = DMIATPTrainVsTrackDirectionUndefined;
      dmiDataInProcessState = DMINoDataIncomingAvailable;
    }

    /******************************************************************************
    * getTrainVsTrackDirection
    ******************************************************************************/
    TrainVsTrackDirection DMIMessageInTrainVsTrackDir::getTrainVsTrackDirection() const
    {
      TrainVsTrackDirection tempTrainVsTrackDir;

      if (DMIDataIncomingValidated == dmiDataInProcessState)
      {
        tempTrainVsTrackDir = trainVsTrackDir;
      }
      else
      {
        tempTrainVsTrackDir = DMIATPTrainVsTrackDirectionUndefined;
      }

      return tempTrainVsTrackDir;
    }

    /******************************************************************************
    * DMIMessageInTrainVsTrackDir::parseMessageData
    ******************************************************************************/
    bool DMIMessageInTrainVsTrackDir::parseDMIMessageData()
    {
      bool parseDataValid = true;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &messageData.dmiData.msgData[0], sizeof(messageData.dmiData.msgData));
      vfwSetReadBuffer(&buffer, sizeof(messageData.dmiData.msgData));

      uint8_t tempTrainVsTrackDir = vfwGetU8(&buffer);

      if (!(tempTrainVsTrackDir <= static_cast<uint8_t>(DMIATPTrainVsTrackDirectionReverseTrackTravelReverse)))
      {
        trace->write(ATC::detailedTrace, "DMI Handler: Invalid Train Vs Track Direction");
        writeToLog(ATC::DetailedLog, "DMI Handler: Invalid Train Vs Track Direction", __FILE__, __LINE__);
        parseDataValid = false;
      }
      else
      {
        trainVsTrackDir = static_cast<TrainVsTrackDirection>(tempTrainVsTrackDir);
      }

      //Write the Trace regarding Parsing of Data
      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * DMIMessageInTrainVsTrackDir::validateMode
    ******************************************************************************/
    bool DMIMessageInTrainVsTrackDir::validateMode() const
    {
      bool modeValid = false;

      if (Kernel::AbstractModeControl::corePtr()->getCurrentMode() == ATPModeRegistration)
      {
        modeValid = true;
      }

      //Write the Trace regarding Mode 
      traceValidateMode(modeValid);

      return modeValid;
    }

  }
}
