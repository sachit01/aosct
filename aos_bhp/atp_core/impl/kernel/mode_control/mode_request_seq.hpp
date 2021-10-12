#ifndef ModeRequestSeq_hpp
#define ModeRequestSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence for Mode Requested DMI buttons
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
#include "event.hpp"
#include "atp_types.hpp"
#include "abstract_mode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /******************************************************************************
    * GLOBAL DECLARATIONS
    ******************************************************************************/
    typedef uint8_t ModeRequestSeqState;
   
    /**
    * The class ModeRequestSeq defines base class for DMI buttons sequence.
    *
    */
    class ModeRequestSeq
    {
    public:

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data 
      */
      virtual void run(CommonDataForModes &commonData) = 0;

      /**
      * Constructor.
      *
      */
      ModeRequestSeq();

      /**
      * Getter for the state of the for mode requested buttons
      *
      * @return state value of seqState variable.
      */
      virtual ModeRequestSeqState getModeReqSeqState() const;

      /**
      * Getter to to check if allowed to enter into  mode by requested buttons
      *
      * @return true if allowed to enter.
      */
     virtual bool isDMIButtonNeeded();

     /**
     * Function for sending out the cross compare data.
     *
     */
     virtual void initCrossCompare();

    protected:
      
      /**
      * Function to validate the DMI buttons in ATP modes
      *
      * @return true  if allowed in the respective mode.
      */
      virtual bool validateModes();

      /**
      * Check if an acknowledge for a mode change request from TCC has timed-out.
      *
      * @return true  if a time-out has occurred
      */
      virtual bool isAckTimeout();

      /**
      * Destructor.
      * Virtual destructor for class with virtual functions.
      */
      virtual ~ModeRequestSeq(void);

      /**
      * The current state of ModeRequestSeq
      */
      ModeRequestSeqState seqState;

      /**
      * Event for driver to notify regarding negative ack for mode change
      */
      const ATC::Event modeAckRejectNotify;

      /**
      * Event when waiting for TCC response on request for mode transition 
      */
      const ATC::Event waitForModAckStandStill;

      /**
      * Event when waiting for driver to ack mode transition
      */
      const ATC::Event waitForModAckDriver;

      /**
      * Event for mode ack received.
      */
      const ATC::Event modAckReceived;

      /**
      * Event for TCC timeout error.
      */
      const ATC::Event modTCCNotAvail;

      /**
      * Event if a request-mode-change-Ack timeout error.
      */
      const ATC::Event modAckTimeout;

      /**
      * Trace interface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Number of previously sent position reports
      */
      uint32_t lastNumPositionMessages;

      /**
      * Max number of polls(or actually default reports to be send) to wait for an Ack 
      * before resetting the state of the sequence, and wait for another trigger to request mode change.
      */
      static const uint32_t maxNrPollsWaitForAck = 5U; 

    private:

    };
  }
}

#endif
