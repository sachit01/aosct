#ifndef PossessionModeRequestSeq_hpp
#define PossessionModeRequestSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for Possession Mode Requested DMI buttons
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
    * The class PosModeRequestSeq defines possession DMI buttons sequence.
    *
    */

    class PosModeRequestSeq : public ModeRequestSeq
    {
    public:

      /**
      * The intial state of PosModeRequestSeq
      */
      static const ModeRequestSeqState posIsDmiButtonPressed = 1U;

      /**
      * The state when to send the PossesionMode Request of PosModeRequestSeq
      */
      static const ModeRequestSeqState posDmiButtonSendReq = 2U;

      /**
      * The wait for possession ack state of PosModeRequestSeq
      */
      static const ModeRequestSeqState posDmiButtonWaitForAck = 3U;

      /**
      * The posDmiButtonConfirmed state of PosModeRequestSeq
      */
      static const ModeRequestSeqState posDmiButtonConfirmed = 4U;

      /**
      * The TCC timeout state of PosModeRequestSeq
      */
      static const ModeRequestSeqState posDmiButtonTCCTimeout = 5U;

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data 
      */
      virtual void run(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      PosModeRequestSeq();

    protected:

      /**
      * Function to run the posIsDmiButtonPressed state of PosModeRequestSeq.
      *
      */
      virtual void runPosButtonPressed();

      /**
      * Function to run the posDmiButtonWaitForAck state of PosModeRequestSeq.
      *
      */
      virtual void runPosDmiButtonWaitForAck();

      /**
      * Function to run the posDmiButtonTCCTimeout state of PosModeRequestSeq.
      *
      */
      virtual void runPosDmiButtonTCCTimeout();

      /**
      * Function to run the posDmiButtonConfirmed state of PosModeRequestSeq.
      * @param[in] commonData   reference to common data 
      */
      virtual void runPosDmiButtonConfirmed(CommonDataForModes &commonData);

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      *
      */
      virtual ~PosModeRequestSeq(void) {};

      /**
      * Function to validate the possession buttons in ATP modes
      *
      * @return true  if allowed in the respective mode.
      */
      virtual bool validateModes();

   private:

    };
  }
}

#endif
