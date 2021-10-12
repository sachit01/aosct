/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (LCS->AOS) has an associated parser class inherited from AbstractLCSMessageIn.
* This file implements the parser for the TrainStatus message.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "lcs_message_in_rcl_status.hpp"
#include "lcs_message_common.hpp"

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
    * LCSMessageInRclStatus constructor, Note: Update version (2nd parameter) if message is updated
    ******************************************************************************/
    LCSMessageInRclStatus::LCSMessageInRclStatus() : AbstractLCSMessageIn(LCSMTypeRCLStatusMessage, 1U, true)
    {
      handlingDone = HandlingDoneUndefined;
    }

    /******************************************************************************
    * LCSMessageInRclStatus::getHandlingDone
    ******************************************************************************/
    bool LCSMessageInRclStatus::getHandlingDone(HandlingDoneType & status) const
    {
      const bool dataIsValidated = (DataValidated == getDataProcessState());
      if (dataIsValidated)
      {
        status = handlingDone;
      }

      return dataIsValidated;
    }

    /******************************************************************************
    * LCSMessageInRclStatus::validate
    ******************************************************************************/
    bool LCSMessageInRclStatus::validate(EmpMsg* const mData)
    {
      getTracer().write(ATC::briefTrace, "Validating TrainStatus");

      // Parse, validate and publish data
      if (parseMessageData(mData))
      {
        setDataProcessState(DataValidated);
      }

      return (DataValidated == getDataProcessState());
    }

    /******************************************************************************
    * LCSMessageInRclStatus::parseMessageData
    ******************************************************************************/
    bool LCSMessageInRclStatus::parseMessageData(EmpMsg* const messageData)
    {
      bool parseDataValid = true;

      uint8_t tmpValU8;

      VFW_Buffer buffer;

      // Initialize buffer to first byte of Application level message
      const uint32_t messageDataLen = messageData->getEMPMessageMaxBodyLen();
      vfwInitBuffer(&buffer, messageData->getEMPBodyBuffer(), messageDataLen);
      vfwSetReadBuffer(&buffer, messageDataLen);

      // Read & Validate handing Done 
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateRclStatus(tmpValU8))
      {
        getTracer().write(ATC::detailedTrace, "RclStatus Invaild");
        parseDataValid = false;
      }
      else
      {
        getTracer().write(ATC::veryDetailedMessageTrace, "RclStatus:handlingDone:", static_cast<uint32_t>(handlingDone));
        handlingDone = static_cast<HandlingDoneType>(tmpValU8);
      }

      traceParseData(parseDataValid);

      return parseDataValid;
    }

    /******************************************************************************
    * LCSMessageInRclStatus::invalidate
    ******************************************************************************/
    void LCSMessageInRclStatus::invalidate()
    {
      setDataProcessState(NoDataAvailable);
    }
  }
}

