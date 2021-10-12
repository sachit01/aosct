/******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the OBRDDataPacket and OBRDMessage classes.
******************************************************************************/

/******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-11-05    csundin     Created
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstring>
#include "obrd_message_common.hpp"
#include "lcs_message_in_train_status.hpp"

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
    /**************************************************************************
    * OBRDDataPacket::OBRDDataPacket
    **************************************************************************/
    OBRDDataPacket::OBRDDataPacket()
    {
      packetType = 0U;
      packetSize = 0U;
      dataLength = 0U;
      memset(&messageData[0], 0, sizeof(messageData));
    }

    /**************************************************************************
    * OBRDMessage::OBRDMessage
    **************************************************************************/
    OBRDMessage::OBRDMessage()
    {
      siteId = 0U;
      locoId = 0U;
      tSender = 0U;
      memset(&receiverId[0], 0, sizeof(receiverId));
      memset(&senderId[0], 0, sizeof(senderId));
    }

    /**************************************************************************
    * OBRDUnitStatusReport::OBRDUnitStatusReport
    **************************************************************************/
    OBRDUnitStatusReport::OBRDUnitStatusReport()
    {
      unitTrackAndPos.track = 0U;
      unitTrackAndPos.position = 0U;
      timeOfMeasurement = 0U;
      lastCarBrakePressure = lastCarBrakePressureNotAsserted;
    }
  }
}
