#ifndef Targets_hpp
#define Targets_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the adaptation class Targets.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_targets.hpp"
#include "radio_message_types_bhp.hpp"
#include "track_data_item_target_bhp.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DS
  {
    /**
    * The class Targets instantiates the abstract class and implements 
    * the interfaces needed for both inherited classes and component.
    *
    */
    class Targets : public AbstractTargets
    {
    public:
      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static Targets& instance(void);

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * Add TrackDataItemTargetBHP
      *
      * @param[in]    t Reference to target to be added
      */
      void addTargetBHP(TrackDataItemTargetBHP const &t);

      /** Set approaching level crossing
      * @param[in] status        boolean value, set true if train front has crossed level crossing approach area
      set false if train front has crossed the level crossing distance
      */
      void setApproachingLevelCrossing(const bool status);

      /** Getter function to get approach speed for level crossing
      *@param[out] approachSpeed        reference to get approach speed to follow for level crossing.
      *@return true value of approach speed for level crossing
      *        false either train front is not crossed level crossing approach area or after passed the level crossing distance (received from TCC)
      */
      bool getApproachingLevelCrossing(uint16_t & approachSpeed) const;

      /** Set approach speed for level crossing
      * @param[in] speed        approach speed to follow for level crossing.
      */
      void setApproachingSpeedLevelCrossing(const uint16_t speed);

      /**
      * Remove all targets
      * Immediate removal of all targets in list, no checks performed.
      */
      virtual void removeAll(void);

    protected:
      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * Maximum Number of TrackDataItemBHP Target
      */
      static const uint32_t maxNumberOfTrackDataItemTargetBHP = 50U;

      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      Targets();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Targets(const Targets&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Targets& operator = (const Targets&);

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /**
      * Boolean to indicate train is approaching level crossing
      */
      bool approachingLevelCrossing;

      /**
      * Approach speed for level crossing
      */
      uint16_t approachingSpeedLevelCrossing;

    };
  }
}
#endif
