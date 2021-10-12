/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the parser for the MovementAuthority message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_types_bhp.hpp"
#include "radio_message_in_movement_authority_bhp.hpp"
#include "abstract_tracks.hpp"
#include "abstract_targets.hpp"
#include "targets.hpp"
#include "track_data_item_target_bhp.hpp"

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
      * constructor
      ******************************************************************************/
      RadioMessageInMovementAuthorityBHP::RadioMessageInMovementAuthorityBHP() : RadioMessageInMovementAuthority()
      {
          //do nothing
      }

      /******************************************************************************
      * invalidate
      ******************************************************************************/
      void RadioMessageInMovementAuthorityBHP::invalidate()
      {
         RadioMessageInMovementAuthority::invalidate();
      }

      /******************************************************************************
      * publishAdhesion
      ******************************************************************************/
      void RadioMessageInMovementAuthorityBHP::publishAdhesion() const
      {
        // Adaptation: Publish BHP specific adhesion value
        DS::AbstractTargets::corePtr()->setAdhesionValue(adhesionValueBHP);
      }

      /******************************************************************************
      * validateQ_TRACK_DATA_TYPE
      ******************************************************************************/
      bool RadioMessageInMovementAuthorityBHP::validateQ_TRACK_DATA_TYPE(const uint8_t val) const
      {
        return RadioMessageInMovementAuthority::validateQ_TRACK_DATA_TYPE(val)
          || (val == static_cast<uint8_t>(TrackDataTypeLevelCrossing));
      }

      /******************************************************************************
      * RadioMessageInMovementAuthorityBHP::publishTrackDataItemTarget
      ******************************************************************************/
      void RadioMessageInMovementAuthorityBHP::publishTrackDataItemTarget() const
      {
        // Add Track Data Item 
        for (uint8_t i = 0U; i < maData.trackDataItemVec.size(); i++)
        {
          TrackDataType trackDataType = maData.trackDataItemVec[i].trackDataType;
          TrackAndPos trackPosition;
          trackPosition.track = maData.trackDataItemVec[i].trackAndPosition.track;
          trackPosition.position = maData.trackDataItemVec[i].trackAndPosition.position;

          OdoPosition odoPosition;
          bool isOdoPositionOK = DS::AbstractTracks::corePtr()->getOdoPos(trackPosition, odoPosition);

          if (isOdoPositionOK)
          {
            if ((TrackDataTypeSafetyMarginChange == trackDataType) || (TrackDataTypeAdhesionChange == trackDataType) ||
              (TrackDataTypeAcousticSignal == trackDataType) || (TrackDataTypeLevelCrossing == static_cast<TrackDataBHPType>(trackDataType)))
            {
              uint16_t nValue = maData.trackDataItemVec[i].optionalValue;
              DS::TrackDataItemTargetBHP TrackDataItemTarget(static_cast<uint8_t>(trackDataType), trackPosition,
                dirAndOrientation2TravelDir(maHead.trainDirection),
                odoPosition, nValue);
              DS::Targets::instance().addTargetBHP(TrackDataItemTarget);
            }
            else
            {
              DS::TrackDataItemTargetBHP TrackDataItemTarget(static_cast<uint8_t>(trackDataType), trackPosition,
                dirAndOrientation2TravelDir(maHead.trainDirection),
                odoPosition);
              DS::Targets::instance().addTargetBHP(TrackDataItemTarget);
            }
          }
        }
      }
   }
}
