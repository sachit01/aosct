/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AbstractMessageHandler class
* which contains the core functionality of the MessageHandler
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-11-15    rquensel    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_types.hpp"
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
    * Constructor
    ******************************************************************************/
    ProtocolVersion::ProtocolVersion()
    {
      majorVersion = 0U;
      minorVersion = 0U;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    ProtocolVersion::ProtocolVersion(const uint8_t maj, const uint8_t min)
    {
      majorVersion = maj;
      minorVersion = min;
    }

    /******************************************************************************
    * operator==
    ******************************************************************************/
    bool ProtocolVersion::operator==(const ProtocolVersion& other) const
    {
      return (majorVersion == other.majorVersion) && (minorVersion == other.minorVersion);
    }

    /******************************************************************************
    * operator==
    ******************************************************************************/
    bool TrackData::operator==(const TrackData& other) const
    {
      return((track == other.track) &&
        (length == other.length) &&
        (trvDir == other.trvDir) &&
        (odoDir == other.odoDir) &&
        (previousTrack == other.previousTrack));
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TrackDataTrackIdComp::TrackDataTrackIdComp() :
      trackIdToFind(0U)
    {
    }
     
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TrackDataTrackIdComp::TrackDataTrackIdComp(const uint16_t trackId) :
      trackIdToFind(trackId)
    {
    }

    /******************************************************************************
    * operator()
    ******************************************************************************/
    bool TrackDataTrackIdComp::operator() (const TrackData &t) const
    {
      return t.track == trackIdToFind;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BaliseData::BaliseData()
    {
      baliseId = 0U;
      baliseTrackAndPosition.track = 0U;
      baliseTrackAndPosition.position = 0U;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BaliseData::BaliseData(const uint16_t balId, const TrackAndPos& trackAndPosition)
    {
      baliseId = balId;
      baliseTrackAndPosition = trackAndPosition;
    }

    /******************************************************************************
    * operator==
    ******************************************************************************/
    bool BaliseData::operator==(const BaliseData& other) const
    {
      return((baliseTrackAndPosition.track == other.baliseTrackAndPosition.track) &&
        (baliseTrackAndPosition.position == other.baliseTrackAndPosition.position) &&
        (baliseId == other.baliseId));
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BaliseDataBaliseIdComp::BaliseDataBaliseIdComp() :
      baliseIdToFind(0U)
    {
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BaliseDataBaliseIdComp::BaliseDataBaliseIdComp(const uint16_t baliseId) :
      baliseIdToFind(baliseId)
    {
    }

    /******************************************************************************
    * operator()
    ******************************************************************************/
    bool BaliseDataBaliseIdComp::operator() (const BaliseData &b) const
    {
      return b.baliseId == baliseIdToFind;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    PossessionAcknowledge::PossessionAcknowledge()
    {
      possAcknowledge = RequestNotAcknowledged;
      allowedSpeedInPossession = 0U;
      baliseIdVec.clear();
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    VehTypeComp::VehTypeComp(const uint8_t vehType) :
      vehTypeToFind(vehType)
    {
    }

    /******************************************************************************
    * operator()
    ******************************************************************************/
    bool VehTypeComp::operator() (const ATP::Kernel::VehicleTypeData &vehTypeData) const
    {
      return vehTypeData.vehicleType == vehTypeToFind;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    ShuntingAcknowledge::ShuntingAcknowledge()
    {
      shuntingAcknowledge = RequestNotAcknowledged;
      allowedSpeedInShunting = 0U;
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    YardAcknowledge::YardAcknowledge()
    {
      yardAcknowledge = RequestNotAcknowledged;
      allowedSpeedInYard = 0U;
    }
   
    /******************************************************************************
    * invalidate()
    ******************************************************************************/
    void Path::invalidate()
    {
      speedAtBeginningOfPath = 0U;
      pathNextTarget.position = 0U;
      pathNextTarget.track = 0U;
      memset(&trackIdList[0], 0, sizeof(trackIdList));
      numberOfTracks = 0U;
      numberOfSpeedChanges = 0U;
      etaRequestStruct.nextTargetArrivalTime = 0U;
      memset(&speedChangePosition[0], 0, sizeof(speedChangePosition));
    }
}
}
