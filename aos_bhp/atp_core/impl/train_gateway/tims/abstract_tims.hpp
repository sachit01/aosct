#ifndef AbstractTIMS_hpp
#define AbstractTIMS_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines AbstractTIMS class which contains the core functionality
* of the TIMS
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
#include "atc_base.hpp"
#include "atp_types.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_position.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    /** Train Integrity Management System Status
    */
    enum TIMSStatus
    {
      /** TIMS Status: Unknown */
      TIMSStatusNotAvailable = 0,
      /** TIMS Status: Intact */
      TIMSStatusIntact = 1,
      /** TIMS Status: Broken */
      TIMSStatusBroken = 2
    };

    /** Train Integrity Management System Supervision
    */
    enum TIMSSupervision
    {
      /** TIMS is not supervised */
      TIMSNotSupervised = 0,
      /** TIMS is supervised */
      TIMSSupervised = 1,
      /** TIMS is inhibited by driver */
      TIMSInhibited = 2
    };

    class AbstractTIMS;
    /**
    * Static variable to store the single instance of AbstractTIMS
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractTIMS* coreTIMSInstancePtr = static_cast<AbstractTIMS*>(NULL);

    /**
    * The class AbstractTIMS implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractTIMS : public ATC::ProcComponent
    {
    public:

      /**
      * Implements the virtual init function.
      */
      virtual bool init();

      /**
      * Implements the virtual runIn function.
      */
      virtual void runIn();

      /**
      * Implements the virtual run function.
      */
      virtual void run();

      /**
      * Implements the virtual runOut function.
      */
      virtual void runOut();

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractTIMS* corePtr();

      /**
      * Returns TIMS Availability.
      *
      * @return true if TIMS is available.
      */
      bool getTimsAvailable() const;

      /**
      * Indicates whether train integrity is confirmed by an automated integrity report.
      *
      * @return true if train integrity is confirmed by an automated integrity report.
      */
      bool getTimsConfirmed() const;

      /**
      * The rear end to be reported to TCC.
      * If the rear end is the last car end, and if we're not reversing,
      * the TIMS confirmed last car position will be returned.
      *
      * @return the rear end position
      */
      TrackAndPos getRearPosition() const;

      /**
      * Returns TIMS Status.
      *
      * @return the current status of TIMS.
      */
      TIMSStatus getTimsStatus() const;

      /**
      * Returns the TIMS Supervision state.
      *
      * @return true if TIMS is being supervised
      */
      TIMSSupervision getTimsSupervision() const;

      /**
      * Indicates whether confirmation of a manual integrity report is needed.
      *
      * @return true if confirmation of a manual integrity report is needed
      */
      virtual bool isManualConfirmationNeeded();

      /**
      * Updates the position report. Must be called whenever a position report is sent to TCC.
      */
      virtual void setPositionReport(
        const Pos::PosAccuracyState posAccuracy,
        const TrackAndPos frontTrackAndPos,
        const TrackAndPos rearTrackAndPos);

      /**
      * Indicates whether @ref getRearPositionOdo would return a valid value.
      *
      * @return true if the rear end position is valid
      */
      virtual bool isRearPositionValid() const;

      /**
      * If @ref isRearPositionValid returns true then this function returns
      * the rear end position.
      *
      * If the rear end is the last car end, and if we're not reversing,
      * the TIMS confirmed last car position will be returned.
      *
      * @return the rear end position
      */
      virtual OdoPosition getRearPositionOdo() const;

      /**
      * If @ref isRearPositionValid returns true then this function returns
      * the rear end position beyond which it is safe to delete tracks and balises.
      *
      * @return the safe rear end position
      */
      virtual OdoPosition getSafePositionToDeleteTrack() const;

      /**
      * Returns the front half of the balise window.
      *
      * @return the front half of the balise window
      */
      virtual uint32_t getFrontBaliseWindow() const;

      /**
      * Returns the rear half of the balise window.
      *
      * @return the rear half of the balise window
      */
      virtual uint32_t getRearBaliseWindow() const;

      /**
      * Interface to call  Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

    protected:

      /**
      * Constructor
      */
      AbstractTIMS();

      /**
      * Update TIMS availability.
      * This function should be implemented in adaptation part.
      */
      virtual void updateTimsAvailable();

      /**
      * Update TIMS supervision state.
      */
      virtual void updateTimsSupervision();

      /**
      * Update TIMS status.
      * This function should be implemented in adaptation part.
      */
      virtual void updateTimsStatus();

      /**
      * Indicates when the most recent automated integrity confirmation was received.
      * This function should be implemented in adaptation part.
      *
      * @return the time (seconds since epoch) when the most recent automated integrity
      * confirmation was received or 0 if no confirmation has been received
      */
      virtual uint64_t getAutomatedReportTime();

      /**
      * Set the TIMS available flag
      *
      * @param[in] isTimsAvailable  Tims available flag
      */
      void setTimsAvailable(const bool isTimsAvailable);

      /**
      * Set the TIMS confirmed flag
      *
      * @param[in] isTimsConfirmed  Tims confirmed flag
      */
      void setTimsConfirmed(const bool isTimsConfirmed);

      /**
      * Set the TIMS status value
      *
      * @param[in] status  Tims status value
      */
      void setTimsStatus(const TIMSStatus status);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Log event for reporting that integrity is broken.
      */
      ATC::Event timsBrokenEvent;

      /**
      * Log event for reporting that integrity is intact.
      */
      ATC::Event timsIntactEvent;

      /**
      * A sample of OdoPosition and its properties at a given time.
      */
      struct PositionSample
      {
        uint64_t millis;              //!< UTC, milliseconds since epoch
        bool valid;                   //!< Indicates whether this sample is valid at time 'millis'
        OdoPosition lastCarPosition;  //!< Odometer position at time 'millis'
        uint32_t lastCarBaliseWindow; //!< The balise window for the last car end of the train at time 'millis'
      };

      /**
      * Updates the position of the last car of the train or clears it, depending on ATP mode.
      */
      void updateOrClearLastCarPosition();

      /**
      * Updates the position of the last car of the train.
      */
      void updateLastCarPosition();

      /**
      * Calculates and updates the position of the last car of the train based on the current
      * integrity supervision system.
      *
      * May only be called when TIMS is supervised and the train is not reversing.
      *
      * @param tSensor time stamp for the integrity confirmation (UTC, seconds since epoch)
      */
      void updateLastCarPositionSupervised(const uint64_t tSensor);

      /**
      * Calculates and updates the position of the last car of the train based on the latest
      * manual integrity confirmation.
      *
      * May only be called when TIMS is inhibited and the train is not reversing.
      */
      void updateLastCarPositionManual();

      /**
      * Handles any DMI input that arrived in this cycle.
      */
      void handleDmiActions();

      /**
      * Updates @ref positionHistory by adding current locomotive position.
      */
      void updatePositionHistory();

      /**
      * Clear @ref positionHistory.
      */
      void clearPositionHistory();

      /**
      * Retrieves the last car position as it was at the given time.
      *
      * @param[in]  millis  the time for which the position shall be retrieved (UTC, milliseconds since epoch)
      * @param[out] sample  the last car position as it was at 'millis'
      *
      * @return true if a position with accuracy PosKnown was found in @ref positionHistory and copied to 'sample'
      */
      bool getLastCarPositionAtTime(const uint64_t millis, PositionSample& sample) const;

      /**
      * Data from the position report most recently sent to TCC.
      */
      struct PositionReport
      {
        bool posKnown;           //!< Indicates whether the position accuracy state was Known
        OdoPosition frontPos;    //!< The train front position
        OdoPosition rearPos;     //!< The train rear position
      };

      /**
      * Data from the position report most recently sent to TCC.
      */
      PositionReport positionReport;

      /**
      * Data for odo-position.
      */
      struct OdoPositionData
      {
        OdoPosition  odoValue; //!< Odometer position
        bool valid;            //!< Indicates whether the position is valid
      };

    private:

      /** Indicates whether TIMS is available */
      bool timsAvailable;

      /** Indicates whether train integrity is confirmed by TIMS */
      bool timsConfirmed;

      /** TIMS Status value */
      TIMSStatus timsStatus;

      /** TIMS supervision state */
      TIMSSupervision timsSupervision;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Variable to store the previous state of TIMS Status
      */
      TIMSStatus prevTIMSStatus;

      /**
      * Indicates whether the Inhibit TIMS button was pressed.
      */
      bool inhibitButtonPressed;

      /**
      * Indicates whether the Resume TIMS button was pressed.
      */
      bool resumeButtonPressed;

      /**
      * Indicates whether Join MA is received.
      */
      bool isRouteTypeJoin;

      /**
      * If not zero, the time when a manual integrity report was received. (Reference time [ms])
      */
      int64_t manualConfirmationStartTime;

      /**
      * Indicates whether a manual integrity confirmation has been received in this cycle.
      */
      bool manualConfirmationReceived;

      /**
      * The maximum number of samples that can be stored in @ref positionHistory
      */
      static const uint16_t maxHistorySize = 300U;

      /**
      * The number of samples that are stored in @ref positionHistory
      */
      uint16_t historySize;

      /**
      * Samples of locomotive position values.
      *
      * Only positions with accuracy PosKnown will be stored here.
      */
      PositionSample positionHistory[maxHistorySize];

      /**
      * Index of the newest sample in @ref positionHistory
      */
      uint16_t latestPositionIndex;

      /**
      * The position of the unconnected end of the last car
      */
      OdoPositionData lastCarPosition;

      /**
      * The rear half of the balise window
      */
      uint32_t lastCarBaliseWindow;

      /**
      * The direction of the most revent movement
      */
      TravelDir lastMovementDir;
    };
  }
}

#endif
