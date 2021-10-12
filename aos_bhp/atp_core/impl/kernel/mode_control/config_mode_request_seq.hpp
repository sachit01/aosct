#ifndef ConfigModeRequestSeq_hpp
#define ConfigModeRequestSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for Config Mode Requested DMI buttons
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-12-18    spandita    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "mode_request_seq.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * The class ConfigModeRequestSeq defines config DMI buttons sequence.
    *
    */
    class ConfigModeRequestSeq : public ModeRequestSeq
    {
    public:

      /**
      * The intial state of ConfigModeRequestSeq
      */
      static const ModeRequestSeqState configDmiButtonPressed = 1U;

      /**
      * The configDmiButtonConfirmed state of ConfigModeRequestSeq
      */
      static const ModeRequestSeqState configDmiButtonConfirmed = 2U;

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data 
      */
      virtual void run(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      ConfigModeRequestSeq();

    protected:

      /**
      * Function to validate the config buttons in ATP modes
      *
      * @return true  if allowed in the respective mode.
      */
      virtual bool validateModes();

      /**
      * Function to run the configDmiButtonPressed state of ConfigModeRequestSeq.
      *
      */
      virtual void runConfigButtonPressed();
    
      /**
      * Function to run the configDmiButtonConfirmed state of ConfigModeRequestSeq.
      * @param[in] commonData   reference to common data 
      */
      virtual void runConfigDmiButtonConfirmed(CommonDataForModes &commonData);

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      *
      */
      virtual ~ConfigModeRequestSeq(void) {};

    private:
     
    };
  }
}

#endif
