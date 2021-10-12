#ifndef AbstractSupervise_hpp
#define AbstractSupervise_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the core implementation of the supervise component
*  Supervise component, supervises the most restrictive speed. Supervise indicates
*  permitted speed, target speed and the remaining distance to the target point to
*  the driver. The component turns on/off an buzzer when certain zones are passed.
*  It also requests the brakes in the case of over speeding
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date      Name    Changes
* ---------------------------------------------------------------------------
* 2016-09-09    Hidaji      Created
* 2016-10-05    Hidaji      Implementation of functions
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "brake_calculations.hpp"
#include "abstract_targets.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Supv
  {
    class AbstractSupervise;
    /**
    * Static variable to store the single instance of AbstractTracks
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractSupervise* coreSuperviseInstancePtr = static_cast<AbstractSupervise*>(NULL);
    /**
    * The class AbstractSupervise implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractSupervise : public ATC::ProcComponent
    {

    public:

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
      */
      virtual void run(void);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractSupervise* corePtr();

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get buzzer
      *
      * @returns buzzer type
      */
      BuzzerType getBuzzerRequest(void) const;

      /** Get ATP warning
      *
      * @returns boolean value for existence of atp warning
      */
      bool getAtpWarning(void) const;

      /** Get ATP Intervention
      *
      * @returns boolean value for existence of atp intervention
      */
      bool getAtpIntervention(void) const;

      /** Get InBCA
      *
      * @returns boolean value true if the train is in brake curve area
      */
      bool getInBCA(void) const;

      /** Get in release speed area
      *
      * @returns boolean value true if the train is in release speed area
      */
      bool getInReleaseSpeedArea(void) const;

      /**
      * Returns true if Emergency Alert shall be activated
      *
      * @return true if Emergency Alert shall be activated
      */
      bool isEmAlertRequested() const;

      /** Get distance to brake curve area
      *
      * @returns distance to the brake curve area
      */
      int32_t getDistanceToBCA(void) const;

      /** Get distance to supervised target
      *
      * @returns distance to supervised target
      */
      int32_t getDistanceToTarget(void) const;

      /** Get speed at supervised target
      *
      * @returns speed of supervised target
      */
      uint32_t getSpeedAtTarget(void) const;

      /** Get time to intervention
      *
      * @returns time until intervention
      */
      uint32_t getTimeToIntervention(void) const;

      /** Get predicted distance to stand still
      *
      * @returns predicted distance to stand still
      */
      int32_t getPredDistToStandStill(void) const;

      /** Get permitted speed
      *
      * @returns permitted speed
      */
      uint32_t getPermittedSpeed(void) const;

      /**
      * Interface to call different level of Console Command
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
      AbstractSupervise();

      /** Calculate the Supervise data used by DMI
      *
      */
      void calcDMIDisplayData(void);

      /**
      *
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Supervise reversing.
      *
      * This will check if train is reversing and set boolean values.
      * It will supervise that the train cannot reverse more than the
      * defined values.
      */
      virtual void revSupervision();

    private:

      /**
      * Non empty target list does not have a valid target
      */
      const ATC::Event errorInTargetList;

      /**
      * An event to report for service brake on ceiling speed supervision
      */
      const ATC::Event serviceBrakeCeilingSpeed;

      /**
      * Keep Standstill event active as long as the ceiling speed is zero
      */
      const ATC::Event standstillCeilingSpeedZero;

      /**
      * Keep Standstill event active as long as no target is available
      */
      const ATC::Event standstillNoTarget;

      /**
      * Apply service brake on some ATP modes
      */
      const ATC::Event invalidAtpMode;

      /**
      An event to report for service brake on target supervision
      */
      const ATC::Event serviceBrakeTargetSupervision;

      /**
      * An event to report for service brake on Standstill supervision
      */
      const ATC::Event serviceBrakeStandstillSupervision;

      /**
      * An event to report for emergency brake on Standstill supervision
      */
      const ATC::Event emergencyBrakeStandstillSupervision;

      /**
      * An event to report for service brake on TIMS Broken.
      */
      const ATC::Event serviceBrakeTIMSBroken;

      /**
      * An event to report for emergency brake on ceiling speed supervision
      */
      const ATC::Event emergencyBrakeCeilingSpeed;

      /**
      * An event to report for emergency brake on target supervision
      */
      const ATC::Event emergencyBrakeTargetSupervision;

      /**
      * An event to report when current speed exceeds first warning curve passed
      */
      const ATC::Event firstWarningSpeedExceed;

      /**
      * An event to report when current speed exceeds second warning curve passed
      */
      const ATC::Event secondWarningSpeedExceed;

      /**
      * An event to report when current speed exceeds speed limit + Warning limit
      */
      const ATC::Event speedWarningLimitExceed;

      /**
      * An event to report when current speed exceeds ceiling speed
      */
      const ATC::Event ceilingSpeedLimitExceed;

      /** Maximum Reverse Supervision Error
      */
      const ATC::Event revSupvError;

      /** Maximum Reverse Emergency break Margin Error
      */
      const ATC::Event revEBMarginError;

      /** Maximum RollAway Supervision Error
      */
      const ATC::Event rollAwaySupvError;

      /** Maximum RollAway Emergency break Margin Error
      */
      const ATC::Event rollAwayEBMarginError;

      /** Location supervision error
      */
      const ATC::Event locationSupvError;

      /**
      * Target supervision zones
      */
      enum ZoneStatus
      {
        NoZone = 0,
        ZoneA,
        ZoneB,
        ZoneC,
        ZoneD,
        ZoneE
      };

      /**
      * Speed Limits
      */
      enum SpeedLimitStatus
      {
        NoSpeedLimit = 0,
        EBSpeedLimit,
        SBSpeedLimit,
        WarningSpeedLimit,
        CeilingSpeedLimit
      };

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      *
      * Flag indicating if the train is in the release speed area
      */
      bool inReleaseSpeedArea;

      /**
      * Flag for requesting Emergency Alert
      */
      bool requestEmAlert;

      /**
      *
      * Keeping track of current target supervision zone
      */
      ZoneStatus currentZone;

      /**
      *
      * Keeping track of current Speed Limits
      */
      SpeedLimitStatus currentSpeedLimit;

      /**
      *
      * Indicates the existence of the buzzer and its type
      */
      BuzzerType buzzer;

      /**
      *
      * Indicates an atp warning if the speed passes the buzzer limit
      */
      bool atpWarning;

      /**
      *
      * Indicates an atp intervention if the speed passes the service brake limit
      */
      bool atpIntervention;

      /**
      *
      * Indicates an if the train is inside the brake curve area
      */
      bool inBCA;

      /**
      *
      * Distance to the brake curve area.
      * This is a positive value, and is zero if sb has passed
      */
      int32_t distanceToBCA;

      /**
      *
      * Distance to the supervised target
      */
      int32_t distanceToTarget;

      /**
      *
      * Odometer of the mrt
      */
      int32_t mrtOdo;

      /**
      *
      * Target speed at the supervised target
      */
      uint32_t speedAtTarget;

      /**
      *
      * time to intervention
      */
      uint32_t timeToIntervention;

      /**
      *
      * predicted distance to stand still
      */
      int32_t predDistToStandStill;

      /**
      *
      * first warning speed
      */
      uint32_t firstWarningSpeed;

      /**
      *
      * second warning speed
      */
      uint32_t secondWarningSpeed;

      /**
      *
      * service brake speed
      */
      uint32_t sbSpeed;

      /**
      *
      * emergency brake speed
      */
      uint32_t ebSpeed;

      /**
      *
      * value to keep the previous state of targetList empty, so that supervise can act if it is changed
      */
      bool prevTargetListEmpty;

      /**
      *
      * keep the previous target Id, so supervise can determine if it is changed
      */
      uint32_t targetId;

      /**
      * Odo position at the time Standstill event is active.
      */
      OdoPosition standstillOdoPos;

      /**
      * Odo position at the time rollaway supervision starts.
      */
      OdoPosition rollAwayStartPos;

      /**
      * Flag to trigger standstill supervision.
      */
      bool triggerStandstillSup;

      /**
      * Flag to reset reference position at stand-still
      */
      bool resetOfStandStillRefPosition;

      /**
      * Flag to reset reference position at rev-supervision
      */
      bool resetOfReverseRefPosition;

      /**
      * Flag to keep track to report event to TCC only once for Warning limit
      */
      bool warningLimitEventReportedToTCC;

      /**
      * Flag to keep track that SB speed has exceeded and brake need 
      *to be applied till speed is below First warning curve speed
      */
      bool speedExceedSBSpeed;

      /**
      * The distance the train has reversed
      */
      uint32_t reverseDistance;

      /**
      * Verifies the current MA with respect to the worst brakeability at the worst gradient.
      */
      void verifyBrakeabilityVsGradient();

      /** supervise ceiling speed
      *
      * Implements the superviseCeilingSpeed() function.
      * The function supervise the ceiling speed by calling the speed limit determining functions and
      * comparing the current speed with speed limits.
      *
      * @param[in]  currentSpeed       Is the current speed of the train
      * @param[in]  calcCeilingSpeed   Is the calculated ceiling speed from target calculation
      */
      void superviseCeilingSpeed(const uint32_t currentSpeed, const uint32_t calcCeilingSpeed);

      /** handle modes
      *
      * Implements the handleModes() function.
      * The method will decide what type of supervision is performed for each mode
      *
      * @param[out] superviseCeiling      Will indicate whether ceiling speed supervision shall be performed
      * @param[out] superviseTarget       Will indicate whether target supervision shall be performed
      * @param[out] superviseStandstill   Will indicate whether standstill supervision shall be performed
      * @param[out] superviseReleaseSpeed Will indicate whether release speed supervision shall be performed
      * @param[out] superviseLocation     Will indicate whether location mode supervision shall be performed
      */
      void handleModes(bool& superviseCeiling, bool& superviseTarget, bool& superviseStandstill, bool& superviseReleaseSpeed, bool& superviseLocation) const;

      /** handle brake curve area
      *
      * Implements the calcDistanceToBCA() function.
      * The function determines the distance to BCA and also indicates inBCA for the supervised target.
      * @param[in]  currLeadPos         is the current odometer of the train lead
      * @param[in]  nextRestTargetIt    is the iterator to the next restrictive target
      */
      void calcDistanceToBCA(const int32_t currLeadPos, DS::SupervisedTargetIterator nextRestTargetIt);

      /** find the closest supervise-able target
      *
      * Implements the getFirstSupTarget() function.
      * The function finds the closest target that needs to be supervised
      * @param[in]  tDir                is the travel direction
      * @param[out] newTarget           Reference to flag indicating a new target
      * @return returns the pointer of the next supervisable target
      */
      const DS::SupervisedTarget* getFirstSupTarget(const TravelDir tDir, bool &newTarget);

      /** supervise targets
      *
      * Implements the calcBrakeCurveSpeedAtCurPosition() function.
      * the function is responsible to calculate the brake curve speeds for the first warning, second warning service brake and emergency 
      * brake curves.
      * @param[in]  currLeadPos           is the current odometer of the train Lead
      * @param[in]  tDir                  is the travel direction
      * @param[in]  gradient              is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration          is the effective deceleration Unit: cm / s^2
      * @param[in]  calcCeilingSpeed      is the ceiling Speed calculated by target calculation
      * @param[in]  target                is the target
      * @param[in]  superviseReleaseSpeed indicates if mode dependent supervise release speed flag is set
      */
      void calcBrakeCurveSpeedAtCurPosition(const int32_t currLeadPos, const int32_t gradient, const int32_t deceleration, const TravelDir tDir,
        const uint32_t calcCeilingSpeed, const DS::SupervisedTarget &target, const bool superviseReleaseSpeed);

      /** supervise targets
      *
      * Implements the superviseTargets() function.
      * the function is responsible to supervise the targets see SCDS for more information
      * @param[in]  currentSpeed          is the current speed of the train cm/s
      * @param[in]  currfrontPos         is the current odometer of the train lead
      * @param[in]  tDir                  is the travel direction
      * @param[in]  gradient              is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration          is the effective deceleration Unit: cm / s^2
      * @param[in]  calcCeilingSpeed      is the ceiling Speed calculated by target calculation
      * @param[in]  target                is the target
      * @param[in]  newTarget             indicates if the supervision is on a new target
      * @param[in]  superviseReleaseSpeed indicates if mode dependent supervise release speed flag is set
      */
      void superviseTargets(const uint32_t currentSpeed, const int32_t currfrontPos, const TravelDir tDir,
        const int32_t gradient, const int32_t deceleration, const uint32_t calcCeilingSpeed, const DS::SupervisedTarget &target,
        const bool newTarget, const bool superviseReleaseSpeed);

      /**
      * Supervise Train when Standstill event is active.
      */
      void superviseTrainAtStandstill();

      /**
      * Supervise TIMS.
      */
      virtual void superviseTims() const;

      /** calculate time to intervention
      *
      * Implements the calcTimeToIntervention() function.
      * the function calculates time until intervention shall happen
      * @param[in]  targetPtr                  Pointer to the most restrictive target
      * @param[in]  currSpeed                  is the current Train speed (va). Unit: cm / s
      * @param[in]  currAcceleration           is the current acceleration value (rCurr). Unit: cm / s^2
      * @param[in]  startPointOdo              Odometer value from where we calculate. Unit: cm
      * @param[in]  tDir                       is the travel direction
      * @param[in]  gradient                   is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration               is the effective deceleration (rsb). Unit: cm / s^2
      */
      void calcTimeToIntervention(DS::SupervisedTarget* const targetPtr, const uint32_t currSpeed, const int32_t currAcceleration, const int32_t startPointOdo,
        const TravelDir tDir, const int32_t gradient, const int32_t deceleration);
      

      /** Find restrictive targets
      *
      * Find the most restrictive and next restrictive targets.
      *
      * @param[in]  mrt                        Reference to the MRT target iterator.
      * @param[in]  nextRestTarget             Reference to the next restrictive target iterator.
      */
      void findRestrictiveTarget(DS::SupervisedTargetIterator& mrt, DS::SupervisedTargetIterator& nextRestTarget) const;

      /**
      * Supervise roll away.
      *
      * This will check if train is rolling away and set boolean values.
      * It will supervise that the train cannot roll away more than the
      * defined values.
      */
      void rollAwaySupervision();

      /**
      * Supervision of train rear end with respect to location borders.
      */
      void locationSupervision() const;
    };
  }
}

#endif
