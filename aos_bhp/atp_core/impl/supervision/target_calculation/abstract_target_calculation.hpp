#ifndef AbstractTargetCalculation_hpp
#define AbstractTargetCalculation_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the core implementation of the TargetCalculation
*  TargetCalculation is responsible for calculating the most restrictive ceiling speed
*  and the Permitted Speed for each target. The component provides supervise component
*  with all the information it needs to effectively supervise only the closest target
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-17    Hidaji      Created
* 2016-09-15    Hidaji      Added brake_calculations.hpp to include files
* 2016-09-30    Hidaji      First implementation of functions
* 2016-10-05    Hidaji      Added maxTrainSpeed to updateGBC
* 2016-12-05    rquensel    Added one attribute to be cross compared
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atc_math.hpp"
#include "abstract_targets.hpp"
#include "abstract_tsetup.hpp"
#include "brake_calculations.hpp"
#include "abstract_mode_control.hpp"
#include "base_target.hpp"
#include <vector>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Supv
  {
    class AbstractTargetCalculation;
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
    static AbstractTargetCalculation* coreTargetCalculationInstancePtr = static_cast<AbstractTargetCalculation*>(NULL);

    /**
    * The class AbstractTargetCalculation implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractTargetCalculation : public ATC::ProcComponent
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
      static AbstractTargetCalculation* corePtr();

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get calculated ceiling speed
      *
      * @returns ceiling speed
      */
      uint32_t getCalcCeilingSpeed(void) const;

      /** Get Most Restrictive Target
      * @returns pointer of Most Restrictive Target
      */
      DS::SupervisedTarget* getMostRestrictiveTarget() const;

      /** Select Ceiling Speed
      *
      * Implements the calcModeDependentCeilingSpeed() function.
      * determines the Mode dependent Ceiling Speed.
      *
      * @param[in]  currCeilSpeed           Current ceiling speed.
      * return  maxSpeed                    Maximum allowed train speed based current ceil speed, atp mode and train setup
      */
      virtual uint32_t calcModeDependentCeilingSpeed(const uint32_t currCeilSpeed) const;

      /**
      * Get odometer value where the sb speed for primary target equals release speed.
      *
      * @returns releaseSpSBPos
      */
      int32_t getReleaseSpSBPos() const;

      /**
      * Get odometer value where the eb speed for primary target equals release speed.
      *
      * @returns releaseSpEBPos
      */
      int32_t getReleaseSpEBPos() const;

    protected:

      /**
      * Constructor
      */
      AbstractTargetCalculation();

    private:

      struct SupGradInfo
      {
        OdoPosition odoPos;
        DS::BaseTarget* parentTarget;
        TravelDir tDir;
        int32_t gradient;
      };

      /**
      *
      * Indicates the odometer value where the sb speed for primary target equals release speed.
      */
      OdoPosition releaseSpSBPos;

      /**
      *
      * Indicates the odometer value where the eb speed for primary target equals release speed.
      */
      OdoPosition releaseSpEBPos;

      /**
      *
      * Indicates the calculated ceiling speed.
      */
      uint32_t calcCeilingSpeed;

      /**
      *
      * Indicates the old MA end.
      */
      OdoPosition maStartPos;

      /**
      *
      * boolean value which is true if the last Ma is Ma from scratch
      */
      bool maStartFromScratch;

      /**
      * Internal flag to make sure we only call initCrossCompare() the first time init is called
      */
      bool initialized;

      /**
      * Event when exceeding container size
      */
      const ATC::Event contSizeExceeded;

      /**
      * Event when ma head is invalid but new ma is received
      */
      const ATC::Event maHeadInvalid;

      /**
      * Last TDI before the end of MA
      */
      DS::BaseTarget* lastSmTDI;

      /**
      * Delta of the max SM and SM at end of MA
      */
      uint16_t smDelta;

      /** Add supervised gradient targets
      *
      * Implements the addSuperviseGradientTargets() function.
      * The function adds supervised gradient targets
      *  @param[in]  trainLength           train length
      */
      void addSuperviseGradientTargets(const uint32_t trainLength) const;

      /** Add supervised targets for ceiling speed and primary targets
      */
      void addSupvSpeedAndPrimaryTargets(void);

      /** update gradient and ceiling speed for targets
      * The function updates the gradient of all non gradient targets,
      * updates the ceiling speed for all targets.
      * @param[in]  maxTrainSpeed          Maximum train speed from train setup
      *
      */
      void updateGradientAndCeilingSpeed(const uint32_t maxTrainSpeed);

      /** Caluclate Decelaration speed and set the curve speeds (FW,SW,SB and EB) for the given target
      * 
      * @param sTarget      Target to calculate curve speeds for
      * @param stDir        Travel Direction
      * @param gradient     Gradient for target
      * @param deceleration current deceleration
      * @param nextTarget   next target
      *
      * @return uint32_t
      */
      uint32_t calcDecSpeedAndSetCurveSpeeds(DS::SupervisedTarget* const sTarget, const ATP::TravelDir stDir, const int32_t gradient, const int32_t deceleration, const DS::SupervisedTarget* const nextTarget) const;

      /** Calculate curve speeds (FW,SW,SB and EB) for all the targets
      *
      * The function will update the First Warning(FW), Second Warning (SW), 
      * Service Brake (SB) and Emergency Brake (EB) speeds of all new targets
      * that are affected by a change in Ma or change of status in the conditional targets
      *
      */
      void updateCurveSpeedsForTargets();

      /** calculate the update to curve speed based on the deceleration curve value
      *
      * @param[in]  currSpeed           is the current curve speed at the target cm/s
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  currSpeed           is the deceleration curve speed at the target cm/s
      * @param[in]  delay               is the total time delays Unit: 0.1 s
      * @param[in]  ceilSpeed           is the ceiling speed at target cm/s
      * @return     updated curve speed at the target
      */
      uint32_t calcCurveSpeedUpdate(const uint32_t currSpeed,const int32_t gradient,
              const int32_t deceleration, const uint32_t decSpeed, const uint32_t delay, const uint32_t ceilSpeed) const;

      /**
      *
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      * get section length for the supervised gradient targets
      * @param[in]   trainLen    Train length
      * @returns     section length for the supervised gradient targets
      */
      uint32_t getSectionLength(const uint32_t trainLen) const;

      /** Find the first gradient and the position of its first supervised target (in this Ma)
      * Implements the findGradAndSupTargToProcess() function.
      * @param[in]  gradientTargets[]          pointer of array for all the gradient target
      * @param[in]  gradTargCount              number of gradients in the gradientTargets
      * @param[out]  firstGradTargIndex        The index the first gradient target in gradientTargets array that needs to be processed. This target might have already been partially prcessed
      * @param[out]  firstSupTargOdo           Odometer position of first supervised target related to firstGradTargIndex that shall be added.
                                               firstGradTargIndex might already have some supervised targets that were created from previous Ma.
      * @param[in]  trainLength                Train length
      * @returns     true if a target is found
      */
      bool findGradAndSupTargToProcess(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount,
        uint8_t &firstGradTargIndex, OdoPosition &firstSupTargOdo, const uint32_t trainLength) const;

      /** Creates the all possible supervise gradient targets and stores them in supGradTargets array
      * Implements the createSuperviseGradTargets() function.
      * @param[in]  gradientTargets[]          pointer of array for all the gradient target
      * @param[in]  gradTargCount              number of gradients in the gradientTargets
      * @param[out] supGradTargets[]           pointer of array for supervised gradient targets that have been prepared to be added
      * @param[out] supGradTargCount           number of supervised gradient targets in the supGradTargets
      * @param[in]  firstGradTargIndex         The index the first gradient target in gradientTargets array that needs to be processed. This target might have already been partially prcessed
      * @param[in]  firstSupTargOdo            Odometer position of first supervised target related to firstGradTargIndex that shall be added.
                                               firstGradTargIndex might already have some supervised targets that were created from previous Ma.
      * @param[in]  trainLength                Train length
      */
      void createSuperviseGradTargets(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount,
        SupGradInfo supGradTargets[], uint8_t &supGradTargCount, const uint8_t firstGradTargIndex, const OdoPosition firstSupTargOdo,
        const uint32_t trainLength) const;

      /** Calculate Gradient for the target and add the target to the supervised target list
      * Implements the calcGradient() function.
      * @param[in]  gradientTargets[]          pointer of array for all the gradient target
      * @param[in]  gradTargCount              number of gradients in the gradientTargets
      * @param[in]  supGradTargets[]           pointer of array for supervised gradient targets that have been prepared to be added
      * @param[in]  supGradTargCount           number of supervised gradient targets in the supGradTargets
      * @param[in]  trainLength                Train length
      */
      void calcGradient(DS::GradientTarget* const gradientTargets[], const uint8_t gradTargCount, SupGradInfo supGradTargets[],
        const uint8_t supGradTargCount, const uint32_t trainLength) const;

      //Vectors for fw, sw, sb and EB ceiling speeds. The vector contains pointers to parent target for supervised target.
      //The vectors are used to keep track of the fw, sw, sb and eb supervised targets that have been processed.
      //When the CS increase target is processed, the minimum of all the CS in the vectors is used to calculate the increase in speed.
      std::vector<DS::BaseTarget*> fwVector;
      std::vector<DS::BaseTarget*> swVector;
      std::vector<DS::BaseTarget*> sbVector;
      std::vector<DS::BaseTarget*> ebVector;

      /**
      * Maximum size Vectors for fw, sw, sb and EB ceiling speeds.
      */
      static const uint32_t maxSizeCurveVectors = maxNumberOfSpeedTargets;

      /** Get the minimum of max speed in train setup, and the minimum ceiling speed for all targets in the vector.
      * @param[in]  speedVector          reference to the vector of targets to calculate the minimum
      * @returns    minimum value if train setup is valid, otherwise max value of uint32_t
      */
      uint32_t getMinCSinVector(std::vector<DS::BaseTarget*>& speedVector) const;

      /** find the next CS target that reduces the ceiling speed ahead of the odometer position.
      * @param[in]  speedVector          start iterator to find next CS target
      * @param[in]  startOdo             the odometer position to find the next CS target
      * @param[in]  speedVector          the CS target should have speed lower than this.
      * @returns    iterator to the CS target. If no CS target is found, iterator to Primary target is returned.
      */
      DS::MaTargetIterator findNextCSTarget(DS::MaTargetIterator startItr,
          const OdoPosition startOdo, uint32_t currFWSpeed) const;

      /** Update the fw, sw, sb and eb ceiling speeds based on the current CS
      * @param[in]  currentCS       Ceiling speed to use for fw, sw, sb and eb ceiling speed calculation.
      */
      void updateCeilingSpeeds(const uint32_t currentCS) const;

      /** remove the CS target from the cs vectors
      * @param[in]  speedVector       Vector to remove the target from.
      * @param[in]  csTargetId        Id of the CS target to remove from the vector.
      */
      void removeCsFromVector(std::vector<DS::BaseTarget*>& speedVector, const uint32_t csTargetId) const;
    };
  }
}

#endif

