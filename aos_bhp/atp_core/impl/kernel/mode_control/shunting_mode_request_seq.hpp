#ifndef ShuntingModeRequestSeq_hpp
#define ShuntingModeRequestSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for Shunting Requested DMI buttons
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
    * The class ShuntModeRequestSeq defines shunting DMI buttons sequence.
    *
    */
    class ShuntModeRequestSeq : public ModeRequestSeq
    {
    public:

      /**
      * The intial state of ShuntModeRequestSeq
      */
      static const ModeRequestSeqState shuntIsDmiButtonPressed = 1U;

      /**
      * The state when to send the Shunting Request of ShuntModeRequestSeq
      */
      static const ModeRequestSeqState shuntDmiButtonSendReq = 2U;

      /**
      * The wait for shunting ack state of ShuntModeRequestSeq
      */
      static const ModeRequestSeqState shuntDmiButtonWaitForAck = 3U;

      /**
      * The shuntDmiButtonConfirmed state of ShuntModeRequestSeq
      */
      static const ModeRequestSeqState shuntDmiButtonConfirmed = 4U;

      /**
      * The TCC timeout state of ShuntModeRequestSeq
      */
      static const ModeRequestSeqState shuntDmiButtonTCCTimeout = 5U;

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data 
      */
      virtual void run(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      ShuntModeRequestSeq();

    protected:

      /**
      * Function to run the shuntIsDmiButtonPressed state of ShuntModeRequestSeq.
      *
      */
      virtual void runShuntButtonPressed();

      /**
      * Function to run the shuntDmiButtonWaitForAck state of ShuntModeRequestSeq.
      *
      */
      virtual void runShuntDmiButtonWaitForAck();

      /**
      * Function to run the shuntDmiButtonTCCTimeout state of ShuntModeRequestSeq.
      *
      */
      virtual void runShuntDmiButtonTCCTimeout();

      /**
      * Function to run the shuntDmiButtonConfirmed state of ShuntModeRequestSeq.
      * @param[in] commonData   reference to common data 
      */
      virtual void runShuntDmiButtonConfirmed(CommonDataForModes &commonData);

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      *
      */
      virtual ~ShuntModeRequestSeq(void) {};

      /**
      * Function to validate the shunting buttons in ATP modes
      *
      * @return true  if allowed in the respective mode.
      */
      virtual bool validateModes();

    private:
      
    };
  }
}

#endif

