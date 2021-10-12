#ifndef TargetCalculation_hpp
#define TargetCalculation_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the adaptation implementation of the TargetCalculation
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
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_target_calculation.hpp"
#include "lcs_message_out_warning_curve.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Supv
  {
    /**
    * The class TargetCalculation instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class TargetCalculation : public AbstractTargetCalculation
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
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static TargetCalculation& instance();

      /** get LCS Warning Curve Message 
      * This method will send the calculated samples for permitted speed curve
      * @param[in]  lcsWarningCurve          LCS Warning Curve Message reference
      * @returns       return true if message is available
      */
      bool getWarningCurveMessage(TG::LCSWarningCurve &lcsWarningCurveMessage);

      /** Select Mode Dependent Ceiling Speed
      *
      * Implements the calcModeDependentCeilingSpeed() function.
      * determines the Mode dependent Ceiling Speed.
      *
      * @param[in]  currCeilSpeed           Current ceiling speed.
      * return  maxSpeed                    Maximum allowed train speed based current ceil speed, atp mode and train setup
      */
      virtual uint32_t calcModeDependentCeilingSpeed(const uint32_t currCeilSpeed) const;

    protected:

    private:

      /**
      *  Sample point exceed limit event
      */
      const ATC::Event lcsWarningCurvePointsLimit;
      
      /**
      *  Null Point access error
      */
      const ATC::Event nullPtrErr;

      /**
      * Container to store the last processed supervised target odometer position
      * This will store the last sample odometer position for which sample has been collected for LCS warning curve message
      */
      OdoPosition lastSampleOdoPos;

      /**
      * Id of the last MRT till which the curve is sent.
      */
      uint32_t lastProcessedMRTId;


      /**
      * Contain for the sample points for permitted speed
      */
      TG::LCSWarningCurve samplePointStorage;

      /**
      * Container to store sample point index.
      * This will store the ample point index for LCS warning curve message
      */
      uint8_t samplePointIdx;

      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      TargetCalculation();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      TargetCalculation(const TargetCalculation&);

      /** store the sample values in LCS Warning Curve Message
      *
      * The function stores the sample.
      * @param[in]  sampleOdo           is the odometer point of the sample to be stored
      * @param[in]  sampleSpeed         is the sample Speed cm/s
      * @returns                        true is the sample was valid and stored
      */
      bool storeSample(const int32_t sampleOdo, const uint32_t sampleSpeed);

      /** calculate Samples for Target start to target end
      *
      * calculate Samples for Target start to target end
      * @param[in]  currLeadPos         is the current odometer of the train lead
      * @param[in]  targetSbOdo         is the service brake odometer point of the target
      * @param[in]  tDir                is the travel direction
      * @param[in]  gradient            is the effective gradient Unit: cm / s^2
      * @param[in]  deceleration        is the effective deceleration Unit: cm / s^2
      * @param[in]  targetSpeed         is the target speed: cm/s
      * @param[in]  samplePermittedSpeed  is the sample Speed cm/s
      * @returns true if samples calculated for target
      */
      bool calSamplesForTarget(const int32_t currLeadPos, const int32_t targetOdo, const TravelDir tDir, const int32_t gradient,
        const int32_t deceleration, const uint32_t targetSpeed, uint32_t sampleSpeed);

      /** Calculate the samples  of permitted speed curve till most restrictive target
      *  @returns       return true if samples successfully collected
      */
      bool calcPermSpeedCurvePoints();

      /** Clear sample points
      */
      void clearSamplePoints();

      /**
      *
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

      /**
      *
      * reset last processed target position
      */
      void resetLastProcessedTargetPosition();

    };
  }
}
#endif
