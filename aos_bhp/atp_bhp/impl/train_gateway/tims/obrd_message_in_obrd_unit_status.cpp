/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageInUnitStatus class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    sunilk    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "obrd_message_in_obrd_unit_status.hpp"
#include <stdio.h>

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
    *constructor
    ******************************************************************************/
    OBRDMessageInUnitStatus::OBRDMessageInUnitStatus(ATC::TraceInterface* const trace_) :
      OBRDMessageIn(trace_, obrdMTypeInUnitStatus)
    {
    }

    /******************************************************************************
    *validate
    ******************************************************************************/
    bool OBRDMessageInUnitStatus::validate(OBRDDataPacket* const messageData)
    {
      bool success = false;

      invalidate();
      if (parseMessageData(messageData))
      {
        success = true;
      }

      return success;
    }

    /******************************************************************************
    * parseMessageData
    ******************************************************************************/
    bool OBRDMessageInUnitStatus::parseMessageData(OBRDDataPacket* const messageData)
    {
      bool success = true;
      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      vfwInitBuffer(&buffer, &(messageData->messageData[0]), messageData->dataLength);
      vfwSetReadBuffer(&buffer, messageData->dataLength);
      obrdUnitStatusReport.unitTrackAndPos.track = vfwGetU16(&buffer);
      obrdUnitStatusReport.unitTrackAndPos.position = vfwGetU32(&buffer);
      obrdUnitStatusReport.timeOfMeasurement = vfwGetU64(&buffer);
      obrdUnitStatusReport.lastCarBrakePressure = vfwGetU8(&buffer);

      if ( (obrdUnitStatusReport.lastCarBrakePressure != 255U)
        && (obrdUnitStatusReport.lastCarBrakePressure > maxBrakePressure) )
      {
        trace->write(ATC::briefTrace, "Invalid value for RN_BPLASTCAR");
        success = false;
      }

      //ToDo: To Remove after integration testing
      char_t buf[100];
      const int32_t ret = snprintf(&buf[0], sizeof(buf),
        "OBRD Unit Status message:: track: %d, position:%d, timestamp: %lld, Brake Pressure: %d ",
        obrdUnitStatusReport.unitTrackAndPos.track,
        obrdUnitStatusReport.unitTrackAndPos.position,
        obrdUnitStatusReport.timeOfMeasurement,
        obrdUnitStatusReport.lastCarBrakePressure);

      if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buf)))
      {
        trace->write(ATC::briefTrace, &buf[0]);
      }

      return success;
    }

    /******************************************************************************
    * OBRDMessageInUnitStatus::invalidate
    ******************************************************************************/
    void OBRDMessageInUnitStatus::invalidate()
    {
      obrdUnitStatusReport.unitTrackAndPos.track = 0U;
      obrdUnitStatusReport.unitTrackAndPos.position = 0U;
      obrdUnitStatusReport.lastCarBrakePressure = 0U;
      obrdUnitStatusReport.timeOfMeasurement = 0U;
    }

    /******************************************************************************
    * OBRDMessageInUnitStatus::getStatusReport
    ******************************************************************************/
    void OBRDMessageInUnitStatus::getStatusReport(OBRDUnitStatusReport &report) const
    {
      report = obrdUnitStatusReport;
    }
  }
}

