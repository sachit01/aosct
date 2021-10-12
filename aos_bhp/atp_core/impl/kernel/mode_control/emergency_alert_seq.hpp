#ifndef EmergencyAlertSeq_hpp
#define EmergencyAlertSeq_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the sequence to process emergency alert.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-01    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "event.hpp"
#include "abstract_mode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    typedef uint8_t EmergencyAlertState;

    /**
    * The inactive state of EmergencyAlertSeq
    */
    static const EmergencyAlertState emergencyAlertInactive = 1U;

    /**
    * The class EmergencyAlertSeq defines the emergency alert sequence.
    *
    */
    class EmergencyAlertSeq
    {
    public:

      /**
      * The state of EmergencyAlertSeq when emergency alert is active
      */
      static const EmergencyAlertState emergencyAlertActive = 2U;

      /**
      * Main run function of the sequence.
      * @param[in] commonData   reference to common data
      */
      virtual void run(CommonDataForModes &commonData);

      /**
      * Constructor.
      *
      */
      EmergencyAlertSeq();

      /**
      * Getter for the state of the EmergencyAlertSeq
      *
      * @return EmergencyAlertState value of seqState variable.
      */
      EmergencyAlertState getState() const;

      /**
      * Function for sending out the cross compare data.
      *
      */
      void initCrossCompare() const;

    private:

      /**
      * Function to run the emergencyAlertInactive state of EmergencyAlertSeq.
      *
      */
      virtual void runEmergencyAlertInactive();

      /**
      * Function to run the runEmergencyAlertSeq.
      *
      */
      virtual void runEmergencyAlertSeq(CommonDataForModes &commonData);

      /**
      * Destructor.
      * Virtual destructor for pure abstract class.
      *
      */
      virtual ~EmergencyAlertSeq(void);

      /**
      * The current state of EmergencyAlertSeq
      */
      EmergencyAlertState seqState;

      /**
      * Traceinterface to be used
      */
      ATC::TraceInterface *trace;

      /**
      * Event to apply SB after emergency alert
      */
      const ATC::Event emergencyAlertActiveEvent;
    };
  }
}

#endif
