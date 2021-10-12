/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->LCS) has an associated creator class inherited from AbstractLCSMessageOut.
* This file implements the creator for the Path Message towards LCS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-25    nsyed    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_math.hpp"
#include <vfw_buffer.h>
#include "lcs_message_common.hpp"
#include "lcs_message_out_path.hpp"
#include "radio_message_types.hpp"
#include "message_handler.hpp"

#ifdef WIN32
void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
#include <vfw_time.h>
#endif

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
  namespace TG
  {
    /******************************************************************************
    * LCSMessageOutPath constructor
    ******************************************************************************/
    LCSMessageOutPath::LCSMessageOutPath() : AbstractLCSMessageOut(LCSMTypePath, 1U, true)
    {
      lcsPath.numberOfTracks = 0U;
      memset(&lcsPath.trackId[0], 0x00, sizeof(lcsPath.trackId));
      lcsPath.speedAtBeginningOfPath = 0U;
      lcsPath.numberOfSpeedChanges = 0U;
      memset(&lcsPath.speedChangePos[0], 0x00, sizeof(lcsPath.speedChangePos));
      lcsPath.nextTargetTrackAndPos.track = 0U;
      lcsPath.nextTargetTrackAndPos.position = 0U;
      lcsPath.nextTargetRTA = 0U;
      lcsPath.adsMapVersion = 0x0000U;
    }

    /******************************************************************************
    * LCSMessageOutPath::collectData
    ******************************************************************************/
    void LCSMessageOutPath::collectData()
    {
      const Kernel::Path* const pathMessage = Kernel::AbstractMessageHandler::corePtr()->getPath();

      if (pathMessage != static_cast<Kernel::Path*>(NULL))
      {
        lcsPath.numberOfTracks = pathMessage->numberOfTracks;

        for (uint8_t trackIdx = 0U; trackIdx < pathMessage->numberOfTracks; ++trackIdx)
        {
          lcsPath.trackId[trackIdx] = pathMessage->trackIdList[trackIdx];
        }

        // Speed at beginning of Path, Convert unit from cm/sec to 0.1km/h
        lcsPath.speedAtBeginningOfPath = static_cast<uint16_t>(ATC::ATCMath::convCmpsTo100mph(pathMessage->speedAtBeginningOfPath));

        lcsPath.numberOfSpeedChanges = static_cast<uint8_t>(pathMessage->numberOfSpeedChanges);

        for (uint8_t speedChangeIdx = 0U; speedChangeIdx < lcsPath.numberOfSpeedChanges; speedChangeIdx++)
        {
          SpeedChangePositionType& lcsSpeedChangePos = lcsPath.speedChangePos[speedChangeIdx];
          const Kernel::SpeedChangePositionStruct& pathMessageChangePos = pathMessage->speedChangePosition[speedChangeIdx];
          lcsSpeedChangePos.trackAndPosition = pathMessageChangePos.trackAndPosition;

          // New speed , Convert unit from cm/sec to 0.1km/h
          lcsSpeedChangePos.newSpeed = static_cast<uint16_t>(ATC::ATCMath::convCmpsTo100mph(pathMessageChangePos.newSpeed));
        }

        lcsPath.nextTargetTrackAndPos = pathMessage->pathNextTarget;
        lcsPath.nextTargetRTA = static_cast<uint32_t>(pathMessage->etaRequestStruct.nextTargetArrivalTime);

        // ADS Map Version is received from TCC in Radio Message Path 
        uint8_t majorVersion = 0U;
        uint8_t minorVersion = 0U;

        if (Kernel::MessageHandler::instance().getStaticConfigurationVersionInPath(majorVersion, minorVersion))
        {
          // Shift major version by 8 bit to add minor version in ADS map 
          lcsPath.adsMapVersion = static_cast<uint16_t>((static_cast<uint16_t>(majorVersion) << 8U)) | minorVersion;
        }
        else
        {
          lcsPath.adsMapVersion = 0x0000U;
        }

        setDataProcessState(DataAvailable);
      }
    }

    /******************************************************************************
    * LCSMessageOutPath::validate
    ******************************************************************************/
    bool LCSMessageOutPath::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::briefTrace, "Validating Data in the Path message to LCS");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }
      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMessageOutPath::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutPath::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      if (getDataProcessState() == DataAvailable)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

        vfwPutU16(&buffer, lcsPath.numberOfTracks);
        for (uint16_t trackIdx = 0U; trackIdx < lcsPath.numberOfTracks; trackIdx++)
        {
          vfwPutU16(&buffer, lcsPath.trackId[trackIdx]);
        }

        vfwPutU16(&buffer, lcsPath.speedAtBeginningOfPath);
        vfwPutU8(&buffer, lcsPath.numberOfSpeedChanges);

        for (uint8_t speedChangeIdx = 0U; speedChangeIdx < lcsPath.numberOfSpeedChanges; speedChangeIdx++)
        {
          vfwPutU16(&buffer, lcsPath.speedChangePos[speedChangeIdx].trackAndPosition.track);
          vfwPutU32(&buffer, lcsPath.speedChangePos[speedChangeIdx].trackAndPosition.position);
          vfwPutU16(&buffer, lcsPath.speedChangePos[speedChangeIdx].newSpeed);
        }

        vfwPutU16(&buffer, lcsPath.nextTargetTrackAndPos.track);
        vfwPutU32(&buffer, lcsPath.nextTargetTrackAndPos.position);
        vfwPutU32(&buffer, lcsPath.nextTargetRTA);
        vfwPutU16(&buffer, lcsPath.adsMapVersion);

        traceAssembleData(parseDataValid);

        // Total length of Application-message
        appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMessageOutPath::invalidate
    ******************************************************************************/
    void LCSMessageOutPath::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }

  }
}
