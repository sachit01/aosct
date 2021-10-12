#ifndef YardModeRequestSeq_hpp
#define YardModeRequestSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for Yard Mode Requested DMI buttons
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
    * The class YardModeRequestSeq defines yard mode DMI buttons sequence.
    *
    */
    class YardModeRequestSeq : public ModeRequestSeq
    {
    public:

      /**
      * The intial state of YardModeRequestSeq
      */
      static const ModeRequestSeqState yardWaitDmiButtonPress = 1U;

      /**
      * The state when to send the Yard Request of YardModeRequestSeq
      */
      static const ModeRequestSeqState yardDmiButtonSendReq = 2U;

      /**
      * The wait for yard mode ack state of YardModeRequestSeq
      */
      static const ModeRequestSeqState YardWaitForTCCAck = 3U;

      /**
      * The yardConfirmed state of YardModeRequestSeq
      */
      static const ModeRequestSeqState yardConfirmed = 4U;

      /**
      * The TCC timeout state of YardDmiAckManualConfirm
      */
      static const ModeRequestSeqState YardDmiAckManualConfirm = 5U;

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data 
      */
      virtual void run(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      YardModeRequestSeq();

      /**
      * Get the status of allowed yard mode DMI buttons.
      *
      */
      virtual bool isDMIButtonNeeded();

    protected:

      /**
      * Function to run the yardWaitDmiButtonPress state of YardModeRequestSeq.
      *
      */
      virtual void runYardWaitDmiButtonPress();

      /**
      * Function to run the YardWaitForTCCAck state of YardModeRequestSeq.
      *
      */
      virtual void runYardWaitForTCCAck();

      /**
      * Function to run the YardDmiAckManualConfirm state of YardModeRequestSeq.
      */
      virtual void runYardDmiAckManualConfirm();

      /**
      * Function to run the yardConfirmed state of YardModeRequestSeq.
      * @param[in] commonData   reference to common data 
      */
      virtual void runYardConfirmed(CommonDataForModes &commonData);

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      *
      */
      virtual ~YardModeRequestSeq(void) {};

      /**
      * Function to validate the yard buttons in ATP modes
      *
      * @return true  if allowed in the respective mode.
      */
      virtual bool validateModes();

    private:

    };
  }
}

#endif
