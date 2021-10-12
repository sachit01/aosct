#ifndef TSetup_hpp
#define TSetup_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration of the adaptation class TSetup.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-18    bhermans    Created
* 2016-08-13    saprasad    Added event name,function prototypes for adaptation part
*                           Fixed review comments
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-20    arastogi    Removed the run function
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_tsetup.hpp"
#include "brakeability_bhp.hpp"

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
    class TSetup : public AbstractTSetup
    {
    public:

      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static TSetup& instance(void);

      /**
      * Declare destructor for the TSetup storage.
      */
      ~TSetup();

      /**
      * Extends the virtual run function.
      *
      */
      virtual void run(void);

      /**
      * Remove train setup
      */
      virtual void removeTrainSetup();

      /**
      * Set the brake data
      *
      * @param[in]  trainDynamicWeightLoaded Dynamic weight for loaded locomotive plus cars.
      * @param[in]  trainDynamicWeightEmpty Dynamic weight for empty locomotive plus cars.
      * @param[in]  locomotiveBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  locomotiveBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the locomotive
      * @param[in]  carsBrakeWeightLoadedBrakeSystem Array for the three loaded brake weights for the cars
      * @param[in]  carsBrakeWeightEmptyBrakeSystem Array for the three loaded brake weights for the cars
      */
      virtual void setBrakeData(
        const int32_t    trainDynamicWeightLoaded,
        const int32_t    trainDynamicWeightEmpty,
        const int32_t    locomotiveBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    locomotiveBrakeWeightEmptyBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightLoadedBrakeSystem[maxBrakeSystems],
        const int32_t    carsBrakeWeightEmptyBrakeSystem[maxBrakeSystems]);

      /**
      * Sets the Maximum Consecutive cars length in the Train
      *
      * @param[in] maxCarLength for the maximum consecutive car length in the train
      */
      void setMaxConsectiveCarLength(uint32_t maxCarLength);

      /**
      * gets the  Maximum Consecutive cars length in the Train
      *
      * @return length(in cm) of the maximum consecutive cars in the train
      */
      virtual uint32_t getMaxConsecutiveCarLength();

    protected:

      /**
      * Initializes the cross compare module. Called by the init.
      */
      virtual void initCrossCompare() const;

      /**
      * Init called only once.
      */
      virtual void initOnce();

      /**
      * getBrakeabilityObject, this returns the brakeability object.
      */
      virtual const Brakeability& getBrakeabilityObject() const;

      /**
      * Get the brake system translated to text.
      *
      *@param[in] brakeSystem New brake system enum to get string for.
      *@param[in] buffer the Brake system string is copied to the buffer
      */
      virtual void getBrakeSystemAsText(const BrakeSystemType brakeSystem, char_t* const buffer) const;

      /**
      * Update the brake system type
      *
      * @param[in]  newBrakeSystemType  the new brake system type
      */
      void updateBrakeSystemType(const BrakeSystemType newBrakeSystemType);

    private:

      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      TSetup();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      TSetup(const TSetup&);
      
      /** 
      * TSetupTests function used for testing of tsetup function in bhp
      */

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      TSetup& operator = (const TSetup&);

      /**
      * Handle brake system in use
      */
      void handleBrakeBrakeSystemInUse();

      /**
      * An safety halt event to reportevent when Data is not valid in tsetup
      */
      const ATC::Event eventDataValidTSetup;

      /**
      * An safety halt event to reportevent when out of index in tsetup  
      */
      const ATC::Event eventOutOfIndexTSetup;

      /**
      * An safety halt event to reportevent when out of range value
      */
      const ATC::Event eventOutOfRangeTSetup;

      /**
      * Stand still event when change in ECPB operating mode
      */
      const ATC::Event standstillEcpbReportedChangedRunMode;

      /**
      * Brake Handling Event when change in brake system in use
      */
      const ATC::Event brakeHandlingEvent;

      /**
      * Brake Handling Event when change in brake system in use
      */
      const ATC::Event ecpbReportedTooLowPercentageOfWorkingBrakes;

      /**
      * Stand still when ecpb reported too low working brake percentage
      */
      const ATC::Event standStillTooLowPercentageOfWorkingBrakes;

      /**
      * Brake Handling event when lambda is less then configured minimum lambda
      * calculated after receiving changed working brake percentage from ECPB
      */
      const ATC::Event sbDueToLowLambdaPercentage;

      /**
      * Stand still event when when lambda is less then configured minimum lambda
      * calculated after receiving changed working brake percentage from ECPB
      */
      const ATC::Event standStillDueToLowLambdaPercentage;

      /**
      * Brake Handling Event when change in ECPB operating mode
      */
      const ATC::Event sbEcpbReportedChangedRunMode;

      /**
      * Event to notify the Brake system is updated 
      */
      const ATC::Event newBrakeSystemUpdated;


      /** The Maximum consecutive cars length in the train */
      uint32_t maxConsecutiveCarsLength;

      /** 
      * Brakeability for the BHP adaptation
      */
      BrakeabilityBhp brakeAbilityBhp;

    };
  }
}
#endif
