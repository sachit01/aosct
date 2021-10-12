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
* This file implements the creator for the Movement Authority Message towards LCS.
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

#include "lcs_message_out_ma.hpp"
#include "message_handler.hpp"

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
    * LCSMovementAuthority constructor
    ******************************************************************************/
    LCSMessageOutMovementAuthority::LCSMessageOutMovementAuthority() : AbstractLCSMessageOut(LCSMTypeMovementAuthority, 1U, true)
    {
      lcsMovementAuthority.endOfMATrackId = 0U;
      lcsMovementAuthority.endOfMAPos = 0U;
      lcsMovementAuthority.maDir = MADirForward;
      lcsMovementAuthority.maMargin = 0U;
    }

    /******************************************************************************
    * LCSMovementAuthority::collectData
    ******************************************************************************/
    void LCSMessageOutMovementAuthority::collectData()
    {
      MAHead head;
      bool retVal;
      
      retVal = Kernel::MessageHandler::corePtr()->getMAHead(head);

      if (retVal)
      {
        lcsMovementAuthority.endOfMATrackId = head.endOfMATrackAndPos.track;
        lcsMovementAuthority.endOfMAPos = head.endOfMATrackAndPos.position;

        TravelDir travelDir = Kernel::dirAndOrientation2TravelDir(head.trainDirection);
        
        lcsMovementAuthority.maDir = (DirReverse == travelDir) ? MADirReverse : MADirForward;
        
        // MA Margin
        lcsMovementAuthority.maMargin = head.maMarginCm;
        
        setDataProcessState(DataAvailable);
      }
    }

    /******************************************************************************
    * LCSMovementAuthority::validate
    ******************************************************************************/
    bool LCSMessageOutMovementAuthority::validate(EmpMsg* const mData, uint16_t& length)
    {
      // Parse, validate and publish data
      if (getDataProcessState() == DataAvailable)
      {
        getTracer().write(ATC::briefTrace, "Validating Data in the MA message to LCS");

        if (assembleMessageData(mData, length))
        {
          setDataProcessState(DataValidated);
        }
      }
      return (getDataProcessState() == DataValidated);
    }

    /******************************************************************************
    * LCSMovementAuthority::assembleMessageData
    ******************************************************************************/
    bool LCSMessageOutMovementAuthority::assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const
    {
      bool parseDataValid = true;

      if (getDataProcessState() == DataAvailable)
      {
        VFW_Buffer buffer;

        // Initialize buffer to first byte of Application level message
        vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageData->getEMPMessageMaxBodyLen());

        vfwPutU16(&buffer, lcsMovementAuthority.endOfMATrackId);
        vfwPutU32(&buffer, lcsMovementAuthority.endOfMAPos);
        vfwPutU8(&buffer, static_cast<uint8_t>(lcsMovementAuthority.maDir));
        vfwPutU16(&buffer, lcsMovementAuthority.maMargin);

        traceAssembleData(parseDataValid);

        // Total length of Application-message
        appDataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
      }

      return parseDataValid;
    }

    /******************************************************************************
    *  LCSMovementAuthority::invalidate
    ******************************************************************************/
    void LCSMessageOutMovementAuthority::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }
  }
}
