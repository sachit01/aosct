/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2002
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without express authority is strictly forbidden.
*
* Module name: BTM Handler
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Date    Sign      Change description
*
* 170221  rquensel  Created
*******************************************************************************/

/*******************************************************************************
* Lint directives
*******************************************************************************/
//lint -esym(1714,CrossCompareFolder::addObject) Needed as glue towards the SPL library
//lint -esym(1714,CrossCompareHandler::instance) Needed as glue towards the SPL library
//lint -esym(1914,CrossCompare::CrossCompare) Needed as glue towards the SPL library

/*******************************************************************************
* Includes
*******************************************************************************/
#include "cc_wrapper.hpp"

/*******************************************************************************
* Declarations and Definitions
*******************************************************************************/

/******************************************************************************
* CrossCompareHandler constructor
******************************************************************************/
CrossCompareHandler::CrossCompareHandler()
  :
  p_currentLowFrequencyCrossCompare_(static_cast<CrossCompare*>(NULL)),
  p_lowFrequencyFolder_(static_cast<CrossCompareFolder*>(NULL)),
  p_mediumFrequencyFolder_(static_cast<CrossCompareFolder*>(NULL)),
  p_highFrequencyFolder_(static_cast<CrossCompareFolder*>(NULL))
{
  static CrossCompareFolder low;

  p_lowFrequencyFolder_ = &low;
}


/******************************************************************************
* CrossCompareHandler destructor
******************************************************************************/
CrossCompareHandler::~CrossCompareHandler()
{
  p_currentLowFrequencyCrossCompare_ = static_cast<CrossCompare*>(NULL);
  p_lowFrequencyFolder_ = static_cast<CrossCompareFolder*>(NULL);
  p_mediumFrequencyFolder_ = static_cast<CrossCompareFolder*>(NULL);
  p_highFrequencyFolder_ = static_cast<CrossCompareFolder*>(NULL);
}


/******************************************************************************
* CrossCompareHandler instance
******************************************************************************/
CrossCompareHandler&
CrossCompareHandler::instance()
{
  static CrossCompareHandler crossCompareHandler_;

  return crossCompareHandler_;
}


/******************************************************************************
* CrossCompareFolder addObject
******************************************************************************/
//lint -e{1762} Cannot be made const, due to external interface
void //lint !e1960 Needed as glue towards the SPL library
CrossCompareFolder::addObject(CrossCompare* const mv)
{
  ATP::Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new ATP::IO::CrossCompareSPL(mv));
}



/******************************************************************************
* CrossCompareFolder constructor
******************************************************************************/
CrossCompareFolder::CrossCompareFolder()
{
}


/******************************************************************************
* CrossCompareFolder destructor
******************************************************************************/
CrossCompareFolder::~CrossCompareFolder()
{
}


/******************************************************************************
* CrossCompare constructor
******************************************************************************/
CrossCompare::CrossCompare() :
  next_(static_cast<CrossCompare*>(NULL))
{
}


/******************************************************************************
* CrossCompare destructor
******************************************************************************/
CrossCompare::~CrossCompare()
{
  next_ = static_cast<CrossCompare*>(NULL);
}


namespace ATP
{
  namespace IO
  {
    /******************************************************************************
    * CrossCompareSPL constructor
    ******************************************************************************/
    CrossCompareSPL::CrossCompareSPL(CrossCompare* const mv)
      :
      CrossCompareObject(),
      splCC_(mv)
    {
    }

    /******************************************************************************
    * CrossCompareSPL writeDataBinary
    ******************************************************************************/
    void CrossCompareSPL::writeDataBinary(VFW_Buffer * const ownBuffer) const
    {
      splCC_->writeDataBinary(ownBuffer);
    }

    /******************************************************************************
    * CrossCompareSPL getMaxDataSize
    ******************************************************************************/
    uint32_t CrossCompareSPL::getMaxDataSize() const
    {
      return splCC_->getMaxDataSize();
    }

  }
}




/*************************** end of file **************************************/

