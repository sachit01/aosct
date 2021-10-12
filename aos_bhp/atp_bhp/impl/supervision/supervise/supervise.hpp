#ifndef Supervise_hpp
#define Supervise_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This is the adaptation implementation of the supervise component
*  Supervise component, supervises the most restrictive speed. Supervise indicates
*  permitted speed, target speed and the remaining distance to the target point to
*  the driver. The component turns on/off an alarm when certain zones are passed.
*  It also requests the brakes in the case of over speeding
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-09    Hidaji      Created
* 2016-04-09    pparthib    Implementation of Extended Reverse Supervision
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_supervise.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Supv
  {    
        /**
        * The class Supervise instantiates the abstract class and implements
        * the interfaces needed for both inherited classes and component.
        *
        */
        class Supervise : public AbstractSupervise
        {
        public:
          /**
          * Singleton instance.
          * Only one instance of this class is allowed.
          * @return the one and only instance.
          *
          * NOTE: Singleton handling shall only be used in Adaptation, not Core!
          */
          static Supervise& instance();

        protected:
          /**
          * Initializes cross-comparison of vital data. Called by the init function.
          */
          virtual void initCrossCompare() const;

          /**
          * Supervises reversing so that the train cannot reverse further or faster than
          * defined limits.
          */
          virtual void revSupervision();


        private:
          /**
          * Constructor. Declared as private in order to prevent illegal use.
          */
          Supervise();

          /**
          * Copy constructor. Declared as private in order to prevent illegal use.
          */
          Supervise(const Supervise&);

          /**
          * Assignment operator. Declared as private in order to prevent illegal use.
          */
          Supervise& operator = (const Supervise&);

          /**
          * Supervises TIMS.
          */
          virtual void superviseTims() const;

          /**
          * Checks if extended reversing supervision shall be enabled or disabled.
          *
          * @return true if extended reversing supervision is enabled
          */
          bool isExtRevSupervisionEnabled();

          /**
          * Calculates and updates @ref accumulatedReverseDistance.
          */
          void calculateAccumulatedReverseDistance();

          /**
          * Checks whether we are moving against the direction of the current MA.
          *
          * @return true if we are moving against the direction of the current MA.
          */
          bool isMovingAgainstMA() const;

          /**
          * Calculates the distance travelled since we last received an MA from scratch.
          *
          * @return the distance travelled (cm)
          */
          uint32_t getDistanceFromScratchMA() const;

          /**
          * Returns the maximum extended reversing distance.
          *
          * @return the distance in cm
          */
          uint32_t getMaxExtndReverseDistance() const;

          /**
          * Returns the extended reversing ceiling speed.
          *
          * @return the ceiling speed in cm/s
          */
          uint16_t getExtndReverseCeilingSpeed() const;

          /**
          * Monitors the extended reversing distance.
          */
          void superviseExtRevDistance();

          /**
          * Monitors the extended reversing ceiling speed.
          */
          void superviseExtRevCeilingSpeed() const;

          /**
          * Service brake event for extended reversing distance supervision.
          */
          const ATC::Event extRevSupvError;

          /**
          * Emergency brake event for extended reversing distance supervision.
          */
          const ATC::Event extRevEBMarginError;

          /**
          * Emergency brake event for extended reversing ceiling speed supervision.
          */
          const ATC::Event extRevSupvEmergencyBrakeCeilingSpeed;

          /**
          * Service brake event for extended reversing ceiling speed supervision.
          */
          const ATC::Event extRevSupvServiceBrakeCeilingSpeed;

          /**
          * Indicates whether extended reversing supervision is enabled.
          */
          bool extRevSupvEnabled;

          /**
          * The "accumulated reverse distance" (never negative).
          */
          uint32_t accumulatedReverseDistance;

          /**
          * Odometer position when MA from Scratch is received.
          * Only valid if @ref posAtMAFromScratchValid is true.
          */
          int32_t posAtMAFromScratch;

          /**
          * Indicates whether @ref posAtMAFromScratch is valid.
          */
          bool posAtMAFromScratchValid;
        };
      }
    }

#endif
