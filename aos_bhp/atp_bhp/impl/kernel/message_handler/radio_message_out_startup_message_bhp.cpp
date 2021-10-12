/****************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the creator for the StartUpMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-01-21    csundin     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message_types_bhp.hpp"
#include "radio_message_out_startup_message_bhp.hpp"
#include "abstract_tsetup.hpp"

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
    RadioMessageOutStartUpMessageBHP::RadioMessageOutStartUpMessageBHP() :
      RadioMessageOutStartUpMessage()
    {
      bhpbLoadStatus = 0U;
    }

    /******************************************************************************
    * invalidate
    ******************************************************************************/
    void RadioMessageOutStartUpMessageBHP::invalidate()
    {
      bhpbLoadStatus = 0U;

      RadioMessageOutStartUpMessage::invalidate();
    }

    /******************************************************************************
    * collectData
    ******************************************************************************/
    void RadioMessageOutStartUpMessageBHP::collectData()
    {
      RadioMessageOutStartUpMessage::collectData();

      bhpbLoadStatus = (DS::AbstractTSetup::corePtr()->getTrainLoadStatus() == TrainIsEmpty) ? 1U : 0U;
    }

    /******************************************************************************
    * assembleAdditionalBlocks
    ******************************************************************************/
    void RadioMessageOutStartUpMessageBHP::assembleAdditionalBlocks(VFW_Buffer& buffer)
    {
      vfwPutU8(&buffer, BTypeBHPBLoadStatus);
      vfwPutU16(&buffer, static_cast<uint16_t>(sizeof(bhpbLoadStatus)));
      vfwPutU8(&buffer, bhpbLoadStatus);
    }
  }
}
