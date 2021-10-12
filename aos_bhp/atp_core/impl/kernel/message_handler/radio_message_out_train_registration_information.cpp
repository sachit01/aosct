/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->TCC) has an associated creator class inherited from AbstractRadioMessageOut.
* This file implements the creator for the TrainRegistrationInformation message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
* 2016-10-03    arastogi    Modified the api to get first balise.
* 2017-03-21    spandita    Corrected the collect API
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_mode_control.hpp"
#include "abstract_decode.hpp"
#include "abstract_targets.hpp"
#include "abstract_message_handler.hpp"
#include "abstract_radio_message_common.hpp"
#include "radio_message_out_train_registration_information.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_message_handler_event_ids.hpp"

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
  namespace Kernel
  {

    /******************************************************************************
    * RadioMessageOutTrainRegistrationInformation constructor
    ******************************************************************************/
    RadioMessageOutTrainRegistrationInformation::RadioMessageOutTrainRegistrationInformation() :
      AbstractRadioMessageOut(MTypeTrainRegistrationInformation),
      // creating different set of objects for different type of events
      noBaliseInfoAvailable(ATC::Event::createSafeBrakeSBEvent(atpMessageHandlerId, ATC::CoreContainer,
        eventIdNoBaliseInfoAvailable, ATC::NoSB, 0U, "No Balise Info Available"))
    {
      implemented = true;

      trainRegistrationInformation.baliseId = 0U;
      trainRegistrationInformation.direction = 0U;
      orientationDataFetchedFlag = false;

    }

    /******************************************************************************
    * RadioMessageOutTrainRegistrationInformation::collectData
    ******************************************************************************/
    void RadioMessageOutTrainRegistrationInformation::collectData()
    {
      if (AbstractModeControl::corePtr()->getCurrentMode() == ATPModeBaliseSearch)
      {
        if (AbstractModeControl::corePtr()->getBaliseSearchModeState() == BaliseSearchMode::baliseSearchWaitMA)
        {
          const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

          // Get Loco-orientation, Travel-direction and Track-orientation
          if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
          {
            TravelDir travelDirection = DS::AbstractTargets::corePtr()->getSupposedTravelDir();
            uint8_t drivingDirection = 0U;

            switch (travelDirection)
            {
            case DirReverse:
              // Driving direction is '1' if reversing
              drivingDirection = 1U;
              break;

            case DirForward:
              // Driving direction is '0' if forwarding, use default value.
              break;

            case DirUndefined:
              trace->write(ATC::briefTrace, "TravelDirection is DirUndefined, Forward is used.");
              break;

            case DirNone:
              trace->write(ATC::briefTrace, "TravelDirection is DirNone, Forward is used.");
              break;

            case DirBoth:
              trace->write(ATC::briefTrace, "TravelDirection is DirBoth, Forward is used.");
              break;

            default:
              // Unknown driving direction, use default value.
              trace->write(ATC::briefTrace, "Unknown TravelDirection, Forward is used.");
              break;
            }

            OdoDir odoDir = AbstractModeControl::corePtr()->getOdoDirInNewRegistration();
            uint8_t orientationInTrack = 0U;

            if (OdoUndefined == odoDir)
            {
              // Unknown orientation, use default value.
              trace->write(ATC::briefTrace, "Unknown orientation in track");
            }
            else
            {
              // Orientation in track is '1' if Odometer direction is set to negative.
              orientationInTrack = (OdoNegative == odoDir) ? 1U : 0U;
            }

            // Bit2 = LocoOrientation, Bit1 = Orientation in Track, Bit0 = DrivingDirection 
            trainRegistrationInformation.direction = (pTrainSetup->orientation | static_cast<uint8_t>(orientationInTrack << 1U) | drivingDirection);
          }
          else
          {
            trace->write(ATC::briefTrace, "Unable to fetch Train-Setup");
          }

          if (!orientationDataFetchedFlag)
          {
            Pos::AbstractDecode::BaliseInfo baliseInfo;
            // Fetch Balise ID 
            if (Pos::AbstractPosition::corePtr()->getFirstBaliseInfo(baliseInfo))
            {
              trainRegistrationInformation.baliseId = baliseInfo.nidBG;
              dataProcessState = DataAvailable;
              orientationDataFetchedFlag = true;
            }
            else
            {
              // Unable to fetch the balise information
              ATC::AbstractEventHandler::corePtr()->reportEvent(noBaliseInfoAvailable, __FILE__, __LINE__);
            }
          }        
        }
        else
        {
          orientationDataFetchedFlag = false;
        }
      }
    }

    /******************************************************************************
    * RadioMessageOutTrainRegistrationInformation::validate
    ******************************************************************************/
    bool RadioMessageOutTrainRegistrationInformation::validate()
    {
      // assemble, validate and publish data
      if (DataAvailable == dataProcessState)
      {
        trace->write(ATC::briefTrace, "Validating TrainRegistrationInformation");

        if (assembleMessageData())
        {
          dataProcessState = DataValidated;
        }
      }

      return (DataValidated == dataProcessState);
    }

    /******************************************************************************
    * RadioMessageOutTrainRegistrationInformation::assembleMessageData
    ******************************************************************************/
    bool RadioMessageOutTrainRegistrationInformation::assembleMessageData()
    {
      bool assembleDataValid = true;

      if (!validateB_DIRECTION(trainRegistrationInformation.direction))
      {
        trace->write(ATC::detailedTrace, "B_DIRECTION invalid");
        assembleDataValid = false;
      }

      if (assembleDataValid)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, &messageData.message.data[0], sizeof(messageData.message.data));

        vfwPutU8(&buffer, static_cast<uint8_t>(messageType));
        vfwPutU16(&buffer, trainRegistrationInformation.baliseId);
        vfwPutU8(&buffer, trainRegistrationInformation.direction);

        //Add M_END_OF_MESSAGE
        vfwPutU8(&buffer, M_END_OF_MESSAGE);

        // Total length of message
        messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      traceAssembleData(assembleDataValid);

      return assembleDataValid;
    }

    /******************************************************************************
    * RadioMessageOutTrainRegistrationInformation::invalidate
    ******************************************************************************/
    void RadioMessageOutTrainRegistrationInformation::invalidate()
    {
      dataProcessState = NoDataAvailable;
    }
  }
}
