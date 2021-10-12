/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the creator for the Position Report message.
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
#include "radio_message_out_position_report_bhp.hpp"
#include "brake.hpp"
#include "targets.hpp"

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
    RadioMessageOutPositionReportBHP::RadioMessageOutPositionReportBHP(const uint16_t chId) : RadioMessageOutPositionReport(chId)
    {
      bhpbTrainStatus = 0U;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageOutPositionReportBHP::invalidate()
    {
      bhpbTrainStatus = 0U;
      RadioMessageOutPositionReport::invalidate();
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void RadioMessageOutPositionReportBHP::collectData()
    {
      //Collect the data from Core Part
      RadioMessageOutPositionReport::collectData();
      
      // collect data for Rapid loss of brake pressure detected 
      // Bit 1, Rapid loss of brake pressure detected
      bhpbTrainStatus |= (Supv::Brake::instance().getRapidLossInBrakePressureDetected()? bhpTrainStatusRapidLossOfBrakePressure: 0U);

    }

    /******************************************************************************
    * assembleAdditionalBlocks
    ******************************************************************************/
    void RadioMessageOutPositionReportBHP::assembleAdditionalBlocks(VFW_Buffer &buffer)
    {
      vfwPutU8(&buffer, BTypeBHPBTrainStatus);
      vfwPutU16(&buffer, static_cast<uint16_t>(sizeof(bhpbTrainStatus)));
      vfwPutU32(&buffer, bhpbTrainStatus);

      //set BHPB_SET_APPROACH_SPEED block
      uint16_t approachingSpeedLevelCrossing = 0U;
      if (DS::Targets::instance().getApproachingLevelCrossing(approachingSpeedLevelCrossing) && (0U != approachingSpeedLevelCrossing) )
      {
        vfwPutU8(&buffer, BTypeBHPBSetApproachSpeed);
        vfwPutU16(&buffer, static_cast<uint16_t>(sizeof(approachingSpeedLevelCrossing)));
        vfwPutU16(&buffer, approachingSpeedLevelCrossing);
      }

    }
  }
}
