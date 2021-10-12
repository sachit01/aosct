#ifndef TIMS_hpp
#define TIMS_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the TIMS class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-29    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_tims.hpp"
#include "obrd_message_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /**
    * The class TIMS instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class TIMS : public AbstractTIMS
    {
    public:

      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static TIMS& instance();

      /**
      * Initializes the component.
      *
      * @return true when initialization completed
      */
      virtual bool init();

      /**
      * Implements the preInit function.
      *
      */
      virtual void preInit();

      /**
      * Processes incoming messages from OBRD BOS
      */
      virtual void runIn();

      /**
      * Processes the TIMS logic
      */
      virtual void run();

      /**
      * Processes outgoing messages to OBRD BOS
      */
      virtual void runOut();

      /**
      * Checks if the brake pressure in the last car received in OBRD report has dropped
      * below the configurable limit (see ATP::Config::getOBRDTestBrakePressure())
      *
      * @param[out] pressureValid    indicates whether 'pressureDropped' is valid
      * @param[out] pressureDropped  if 'pressureValid' is true, indicates whether the brake pressure has dropped
      */
      void checkLastCarBPDrop(bool& pressureValid, bool& pressureDropped) const;

    protected:

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Implements the virtual getTimsAvailable function.
      *
      * @return true if TIMS is available.
      */
      virtual void updateTimsAvailable();

      /**
      * Checks and updates train integrity.
      */
      virtual void updateTimsStatus();

      /**
      * Indicates when the most recent automated integrity confirmation was received.
      *
      * @return the time (seconds since epoch) when the most recent automated integrity
      * confirmation was received or 0 if no confirmation has been received
      */
      virtual uint64_t getAutomatedReportTime();

    private:

      /**
      * Constructor
      */
      TIMS();

      /**
      * Checks whether ECBP is in use.
      */
      void checkECPBInput();

      /**
      * Checks for new reports from OBRD.
      */
      void checkOBRDInput();

      /**
      * Updates train integrity based on ECPB.
      */
      void updateECBPStatus();

      /**
      * Updates train integrity based on OBRD.
      */
      void updateOBRDStatus();

      /**
      * Check if brake pressure sensors 1 and/or 2 reports values that indicate train integrity.
      *
      * @return true if train integrity is ok.
      */
      bool checkBrakePressureLoco() const;

      /**
      * Check if the brake pressure in the last received OBRD report indicates train integrity.
      *
      * @param[out] pressureValid  indicates whether 'pressureOk' is valid
      * @param[out] pressureOk     if 'pressureValid' is true, indicates whether the brake pressure indicates integrity ok
      */
      void checkBrakePressureLastCar(bool& pressureValid, bool& pressureOk) const;

      /**
      * Check if the position in the last received OBRD report indicates train integrity.
      *
      * @param[out] positionValid  indicates whether 'positionOk' is valid
      * @param[out] positionOk     if 'positionValid' is true, indicates whether train integrity is ok
      */
      void checkObrdPosition(bool& positionValid, bool& positionOk) const;

      /**
      * Log event for reporting OBRD positions outside the train extent.
      */
      ATC::Event invalidObrdEvent;

      /**
      * The OBRD Message Handler
      */
      OBRDMessageHandler obrdMessageHandler;

      /**
      * The most recent valid status report received from OBRD.
      */
      OBRDUnitStatusReport obrdStatusReport;

      /**
      * The time at which @ref obrdStatusReport was received. (Reference time [ms])
      */
      int64_t obrdStatusReportTime;

      /**
      * Indicates whether an OBRD report with valid position has been received during this cycle.
      */
      bool validObrdReportReceived;

      /**
      * Indicates whether brake pressure has been received from OBRD during this cycle.
      */
      bool obrdBrakePressureReceived;

      /**
      * The time at which ECPB train integrity was last confirmed (LCS time stamp; UTC, seconds since epoch).
      */
      uint64_t ecpbConfirmedTime;

      /**
      * The time at which ECPB train integrity was last received. (Reference time [ms])
      */
      int64_t ecpbStatusReportTime;

      /**
      * Indicates whether an ECPB report has been received during this cycle.
      */
      bool ecpbReportReceived;

      /**
      * Indicates whether ECPB is in use.
      */
      bool ecpbIsUsed;
    };
  }
}

#endif
