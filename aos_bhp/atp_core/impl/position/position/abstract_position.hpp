#ifndef AbstractPosition_hpp
#define AbstractPosition_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines AbstractPosition class which contains the core position logic
* used by the AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-07    lantback    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Make abstract constructor protected
* 2016-08-10    spandita    Updated member and methods to implement the
*                           functionality of position
* 2016-08-22    spandita    Updated the code with review comments
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    added interfaces for balises and offset correction.
* 2016-10-03    arastogi    Added functions to get safe front and rear position.
* 2016-10-24    arastogi    Added variable for direction of movement at last balise.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"
#include "abstract_decode.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Pos
  {
    /* Define position accuracy constants */
    enum PosAccuracyState
    {
      /** Position unknown */
      PosUnknown = 0,
      /** Position approximate, that is not yet established by enough balise reads */
      PosApprox = 1,
      /** Position Known, established by at least one matching balise */
      PosKnown = 2,
      /** Position is doubtful, that is last read balise does not match available track data */
      PosDoubtfull = 3
    };

    class AbstractPosition;
    /**
    * Static variable to store the single instance of AbstractPosition
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractPosition* corePositionInstancePtr = static_cast<AbstractPosition*>(NULL);

    /**
    * The class AbstractPosition implements the core logic for position calculations
    * used by the AOS.
    *
    */
    class AbstractPosition : public ATC::ProcComponent
    {
    public:

      /**
      * Implement the main execution of AbstractPosition
      */
      virtual void run(void);

      /**
      * Implement the initializer of AbstractPosition
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Return current leading position based on MA direction
      *
      * @return Track and position of leading train end
      */
      const TrackAndPos& getLeadingPos(void) const;

      /**
      * Return current leading position based on MA direction
      *
      * @return Odometer of leading train end
      */
      OdoPosition getLeadingPosOdo(void) const;
      
      /**
      * Return the safe leading position
      *
      * @return Odometer of leading train end including the uncertainty
      */
      OdoPosition getSafeLeadingPosOdo(void) const;

      /**
      * Return actual leading position (the end that could potentially collide when moving)
      *
      * @return Odometer of leading train end
      */
      OdoPosition getActualLeadingPosOdo(void) const;
      
      /**
      * Return current trailing position based on MA direction
      *
      * @return Odometer of trailing train end
      */
      OdoPosition getTrailingPosOdo(void) const;

      /**
      * Return safe trailing position
      *
      * @return Odometer of trailing train end including uncertainty
      */
      OdoPosition getSafeTrailingPosOdo(void) const;

      /**
      * Return actual trailing position (the end that could NOT potentially collide when moving)
      *
      * @return Odometer of trailing train end
      */
      OdoPosition getActualTrailingPosOdo(void) const;

      /**
      * Returns the odometry position of the unconnected end of the outermost locomotive.
      *
      * @return the odometry position of the unconnected end of the outermost locomotive
      */
      OdoPosition getLocoEndPosOdo(void) const;

      /**
      * Clears the balises encountered from Decode in modes where balise information is not needed.
      */
      void clearEncounteredBalises(void) const;

      /**
      * Return current antenna position
      *
      * @return Track and position of balise antenna;
      */
      const TrackAndPos& getCurrAntennaPos(void) const;
      /**
      * Return current antenna position (odometer)
      *
      * @return Odometer of balise antenna
      */
      OdoPosition getCurrAntennaPosOdo(void) const;

      /**
      * Get current position accuracy state
      *
      * @return Current position accuracy state
      */
      PosAccuracyState getAccuracyState(void) const;

      /**
      * Get the last detected balise information
      *
      * @param[in] baliseInfo Reference to the balise information
      * @return true if a balise was received this cycle, false otherwise
      */
      bool getBaliseInfo(AbstractDecode::BaliseInfo& baliseInfo) const;

      /**
      * Get the first detected balise information in Balise Search mode
      *
      * @param[in] baliseInfo Reference to the balise information
      * @return true if first balise is received in balise search mode,
      *         false otherwise
      */
      bool getFirstBaliseInfo(AbstractDecode::BaliseInfo& baliseInfo) const;

      /**
      * To check if a given balise was an expected balise
      *
      * @param[in] baliseInfo Reference to the balise information
      * @return true if the balise is the expected balise, false otherwise
      */
      bool isPassedBaliseExpected(const AbstractDecode::BaliseInfo& baliseInfo) const;

      /**
      * Return current odometer offset correction value.
      * This value is calculated when a balise is detected.
      * In other situations it equals zero.
      *
      * @return Odometer offset correction
      */
      OdoPosition getOdometerOffsetCorrection(void) const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractPosition* corePtr();

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

    protected:
      /**
      *  Constructor, setup class variables
      */
      AbstractPosition();

      /**
      * Return leading position for given direction
      *
      * @param[in] direction  Direction
      *
      * @return Odometer of leading train end
      */
      OdoPosition getLeadingPosForDirectionOdo(TravelDir const direction) const;
      
      /**
      * Return trailing position for given direction
      *
      * @param[in] direction  Direction
      *
      * @return Odometer of trailing train end
      */
      OdoPosition getTrailingPosForDirectionOdo(TravelDir const direction) const;

    private:
      /**
      * A value that cannot occur as a balise id.
      */
      static const uint16_t invalidBaliseId = 0xFFFFU;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Current accuracy state of position
      */
      PosAccuracyState  accuracyState;
      /**
      * Current Front track and position of train
      */
      TrackAndPos  currentPosNominalLeading;
      /**
      * Current track and position of antenna
      */
      TrackAndPos  currPosAntenna;
      /**
      * Current odo position
      */
      OdoPosition      currOdoPos;
      /**
      * Last cycle odo offset
      */
      OdoPosition      prevOdoPos;

      /**
      * Last cycle track and position of antenna
      */
      TrackAndPos  prevPosAntenna;

      /**
      * Current odo offset
      */
      OdoPosition      odoOffset;

      /**
      * bool for first normal mode balise found
      */
      bool         isFirstBaliseFound;
      /**
      * number of missed balise
      */
      uint32_t     numOfMissedBalise;

      /**
      * Information of first balise in balise search mode
      */
      AbstractDecode::BaliseInfo firstBaliseInfo;

      /**
      * Information of most recent balise received
      */
      AbstractDecode::BaliseInfo lastDetectedBaliseInfo;

      /**
      * The direction of movement over last balise.
      */
      TravelDir dirAtLastBalise;

      /**
      * Information of previous expected balise in direction of movement.
      */
      AbstractDecode::BaliseInfo prevBaliseInfo;

      /**
      * Information of next expected balise in direction of movement.
      */
      AbstractDecode::BaliseInfo nextBaliseInfo;

      /**
      * The direction of movement over last balise.
      */
      TravelDir dirAtPrevBalise;

      /**
      * To indicate if a balise is received this cycle.
      */
      bool isBaliseReceived;

      /**
      * Flag for if second balise get detected before MA.
      */
      bool secBalFoundBeforeMA;

      /**
      * The time delay in reading the balise
      *
      */
      static const uint16_t baliseReadTimeDelay = 800U;

      /**
      * First missed balise event
      */
      const ATC::Event missedFirstBalise;

      /**
      * Missed balise Error
      */
      const ATC::Event missedBaliseFound;

      /**
      * Yard or Shunting mode not allowed when detecting a dangerForShunting balise
      */
      const ATC::Event yardOrShuntingModeNotAllowed;

      /**
      * Outside balise window Error
      */
      const ATC::Event outSideBaliseFound;

      /**
      * Unexpected Balise found Error
      */
      const ATC::Event unKnownBaliseFound;

      /**
      * Invalid Atp Mode
      */
      const ATC::Event unDefATPMode;

      /**
      * Out of range Error
      */
      const ATC::Event outOfLimitBoundaries;

      /**
      * rear or front end Out of limit
      */
      const ATC::Event rearOrFrontEndoutOfLimit;

      /**
      * Wrong number of balises in balise search
      */
      const ATC::Event invalidBaliseInBalisesearch;

      /**Identical balise found Error
      */
      const ATC::Event identicalBaliseFoundError;

      /**
      * Event for if second balise found before MA
      */
      const ATC::Event secBaliseFoundBeforeMA;

      /**
      * Event for if third balise found before second balise validation
      */
      const ATC::Event thirdBalFound;

      /**
      * Event for invalid balise in possession mode
      */
      const ATC::Event invalidBalInPossession;

      /**
      * Event for balise detected in non-relevant Modes.
      */
      const ATC::Event baliseDetectedIrrelevantMode;

      /**
      * Event for balise passage with calibration data
      */
      const ATC::Event baliseDetected;

      /**
      * gets the Balise Air Gap value
      * @return value of the BaliseAirGap
      */
      virtual uint16_t getBaliseAirGap() const = 0;

      /**
      * Normal mode Operation
      *
      */
      void readBalises(void);

      /**
      * Check for missing balise between current balise and previous
      * Balise present in balise list
      * @param[in]  Current balise from decode
      */
      void checkMissedBaliseOnDetection(const AbstractDecode::BaliseInfo& newBaliseInfo);

      /**
      * Check for missed balise if odometer crosses the threshold to detect a balise
      *
      */
      void checkMissedBaliseOnMovement(void);

      /**
      * Take the necessary actions for a missed balise
      */
      void reportMissedBalise(const uint16_t missedBaliseId);

      /**
      * Find the next expected balise from balise list.
      *
      */
      void findNextExpectedBalise(void);

      /**
      * Check if update is needed for prev and next expected balise.
      *
      */
      void updatePrevAndNextBaliseInfo(void);

      /**
      * update the current Rear and Front
      * Position of train
      */
      void updateCurrentPos(void);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      * Calculate Odo difference between reference Odo with actual Odo.
      *
      * @param[in]  refOdo  reference odo value
      * @param[in]  Odo Odo value
      *
      * @return value  Difference between the reference Odo with actual Odo.
      */
      void updateCurrentOdoPos(const OdoPosition offset);

      /**
      * Handling the Balise Search Mode
      */
      void handleBaliseSearch();

      /**
      * Handling the SBTS
      */
      void handleSafeBrakeToStop();

      /**
      * Handling the Registration mode
      */
      void handleRegistration();

      /**
      * Handling the Registration mode
      */
      void handlePossession();

      /**
      * Recalculate the odo offset.
      * The antenna track and position is fixed.
      * Offset is adjusted based on new antenna odo and stored value.
      */
      void recalcOffset();
    };
  }
}
#endif
