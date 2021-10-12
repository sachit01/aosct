/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-11-13    bhermans     Created
* 2016-03-01    bhermans     AdaptedRadioChannel removed
* 2016-03-03    bhermans     Introduced namespace RadioCom
* 2016-04-19    lantback     Use ATC::ProcComponent, init to return bool
* 2016-06-15    akushwah     Radio Handler Implementation
* 2016-06-17    akushwah     Incorporated Review Comment
* 2016-07-25    akushwah     Removed the third Radio Channel
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>
#include "radio_handler.hpp"
#include "channel_config.hpp"
#include "abstract_application_base.hpp"
#include "abstract_cross_compare.hpp"
      
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
  namespace RadioCom
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    RadioHandler::RadioHandler() : AbstractRadioHandler(),
      radioChannel1((vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel1DispToATPA : ATC::radioChannel1DispToATPB,
      (vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel1ATPAToDisp : ATC::radioChannel1ATPBToDisp,
        radioChannelId1, "RCH1"),
      radioChannel2((vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel2DispToATPA : ATC::radioChannel2DispToATPB,
      (vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel2ATPAToDisp : ATC::radioChannel2ATPBToDisp,
        radioChannelId2, "RCH2"),
      radioChannel3((vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel3DispToATPA : ATC::radioChannel3DispToATPB,
      (vfwGetSide() == VFW_A_SIDE) ? ATC::radioChannel3ATPAToDisp : ATC::radioChannel3ATPBToDisp,
        radioChannelId3, "RCH3"),
      chnlStatus1(false), chnlStatus2(false), chnlStatus3(false), crossCompareInitialized(false)
    {
    }

    /******************************************************************************
    * Function: instance()
    *-----------------------------------------------------------------------------
    * Returns singleton instance
    *
    ******************************************************************************/
    RadioHandler& RadioHandler::instance(void)
    {
      static RadioHandler theOnlyRadioHandlerInstance;

      return theOnlyRadioHandlerInstance;
    }

    void RadioHandler::initCrossCompare() const
    {
      AbstractRadioHandler::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&chnlStatus1));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&chnlStatus2));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&chnlStatus3));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&crossCompareInitialized));
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void RadioHandler::preInit(void)
    {
      radioChannel1.preInit();
      radioChannel2.preInit();
      radioChannel3.preInit();
    }

    /******************************************************************************
    * init
    *-----------------------------------------------------------------------------
    * creates one Radio Channel instance per simultaneously connected TCC
    * Calls the init for the Radio Channels created
    ******************************************************************************/
    bool RadioHandler::init(void)
    {

      if (!chnlStatus1)
      {
        // Initialize the Radio Channel for TCC1
        chnlStatus1 = radioChannel1.init();

        if (chnlStatus1)
        {
          // Push Radio Channel1 in the vector   
          radioChannels.push_back(&radioChannel1);

          ATC::AbstractApplicationBase::corePtr()->addComponent(&radioChannel1);
         
        }
      }

      if (!chnlStatus2)
      {
        // Initialize the Radio Channel for TCC2
        chnlStatus2 = radioChannel2.init();

        if (chnlStatus2)
        {
          // Push Radio Channel2 in the vector   
          radioChannels.push_back(&radioChannel2);

          ATC::AbstractApplicationBase::corePtr()->addComponent(&radioChannel2);
        }
      }

      if (!chnlStatus3)
      {
        // Initialize the Radio Channel for TCC3
        chnlStatus3 = radioChannel3.init();

        if (chnlStatus3)
        {
          // Push Radio Channel2 in the vector   
          radioChannels.push_back(&radioChannel3);

          ATC::AbstractApplicationBase::corePtr()->addComponent(&radioChannel3);
        }
      }

      if (!crossCompareInitialized)
      {
        initCrossCompare();
        crossCompareInitialized = true;
      }

      // All channels must be properly initialized
      return (chnlStatus1 && chnlStatus2 && chnlStatus3);
    }
  }
}
