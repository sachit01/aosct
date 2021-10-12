#ifndef AbstractTargets_hpp
#define AbstractTargets_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the behaviour of the abstract class to handle targets
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
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-21    lantback    Make abstract constructor protected
* 2016-07-27    akushwah    Initial Implementation
* 2016-08-25    bhidaji     Added changes needed for TargetCalculation
* 2016-08-26    bhidaji     in The method removePassed changed rearPos to frontPos
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Added Console call
* 2016-09-29    bhidaji     Changed travelDirection to supposed travel direction
*                           Added getSupposedTravelDir(), getTargetListChanged(), isTargetListEmpty()
*                           Added setConditionalTargetSupervise()
* 2016-10-12    arastogi    Disabled the trace in TargetErrorHandler report()
* 2016-10-24    spandita    Added set function for keep target variable
* 2017-06-20    skothiya    Updated to handle the train states, removePassed renamed with handlePassedTargets
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "gradient_target.hpp"
#include "primary_target.hpp"
#include "speed_target.hpp"
#include "conditional_target.hpp"
#include "track_data_item_target.hpp"
#include "event.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#include "supervised_target.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * Helper class for reporting target errors as trace messages
    *
    */
    class TargetErrorHandler
    {
    public:
      //lint -esym(1714,ATP::DS::TargetErrorHandler::report) Lint is wrong, this *is* used
      /**
      * Reports the given error message as a trace.
      *
      * @param[in] str  error string.
      *
      */
      static void report(const char_t * const str)
      {
        //TODO this is wrong implementation.. also it prints out too much data

        /*The below code is added just to remove the warning
        *warning C4100 : 'str' : unreferenced formal parameter
        * The code needs to removed once trace API is available
        */
        ATC::TraceInterface trace("Abstract Target", 9U, str);
        trace.write(9U, str);
      }
    };

    /**
    * Main target list
    * Pointer to BaseTarget type declaration
    */
    typedef BaseTarget* maTargetPtr;

    /**
    * The type of container used for the target objects
    */
    typedef ATC::GPList<maTargetPtr, maxNumberOfTotalTargets, TargetErrorHandler> MaTargetList;

    /**
    * Target objects list const iterator
    */
    typedef MaTargetList::const_iterator ConstMaTargetIterator;

    /**
    * Target objects list iterator
    */
    typedef MaTargetList::iterator MaTargetIterator;

    /**
    * Target objects list reverse iterator
    */
    typedef MaTargetList::reverse_iterator MaTargetRevIterator;

    /**
    * Supervised target list
    * Pointer to SupervisedTarget type 
    */
    typedef SupervisedTarget* supervisedTargetPtr;

    /**
    * The type of container used for the supervised target objects
    */
    typedef ATC::GPList<supervisedTargetPtr, maxNumberOfSupvTotalTargets, TargetErrorHandler> SupervisedTargetList;

    /**
    * SupervisedTarget objects list const iterator
    */
    typedef SupervisedTargetList::const_iterator ConstSupervisedTargetIterator;

    /**
    * SupervisedTarget objects list iterator
    */
    typedef SupervisedTargetList::iterator SupervisedTargetIterator;

    /**
    * SupervisedTarget objects list reverse iterator
    */
    typedef SupervisedTargetList::reverse_iterator SupervisedTargetRevIterator;

    class AbstractTargets;
    /**
    * Static variable to store the single instance of AbstractTargets
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractTargets* coreTargetsInstancePtr = static_cast<AbstractTargets*>(0);

    /**
    * The class AbstractTargets implements the interface defined by the ProcComponent class.
    *
    */
    class AbstractTargets : public ATC::ProcComponent
    {
    public:

      /**
      * Implements the virtual run function.
      * (If needed)
      */
      virtual void run(void);

      /**
      * Implements the virtual init function.
      * (If needed)
      */
      virtual bool init(void);

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /*************************************************/
      /* Interface functions to manipulate target list */
      /*************************************************/

      /**
      * Add PrimaryTarget
      *
      * @param[in]    t Reference to target to be added
      */
      void addTarget(PrimaryTarget const &t);

      /**
      * Add SpeedTarget
      *
      * @param[in]    t Reference to target to be added
      */
      void addTarget(SpeedTarget const &t);

      /**
      * Add GradientTarget
      *
      * @param[in]    t Reference to target to be added
      */
      void addTarget(GradientTarget const &t);

      /**
      * Add TrackDataItemTarget
      *
      * @param[in]    t Reference to target to be added
      */
      void addTarget(TrackDataItemTarget const &t);

      /**
      * Add SupervisedTarget
      *
      * @param[in]    t Reference to target to be added
      */
      void addTarget(const SupervisedTarget &t);

      /**
      * Deletes the given target
      *
      * @param[in]    bTarget pointer to the target to delete
      * @return       true if successful, false if failed
      */
      bool delTarget(const BaseTarget* const bTarget);

      /**
      * Delete base target
      *
      * @param[in]  it     reference of target iterator to be deleted
      * @return            true if successful, false if failed
      */
      bool delTarget(MaTargetIterator &it);

      /**
      * Delete supervised target
      *
      * @param[in]  it     reference of supervised target iterator to be deleted
      * @return            true if successful, false if failed
      */
      bool delTarget(SupervisedTargetIterator &it);

      /**
      * Remove all targets
      * Immediate removal of all targets in list, no checks performed.
      */
      virtual void removeAll(void);

      /**
      * Remove all supervised targets
      * Immediate removal of all supervised targets in list, no checks performed.
      */
      void removeAllSupervisedTargets(void);

      /**
      * Delete the targets in the given range except the primary targets
      *
      * @param[in] startOdoPos    Odo start position
      * @param[in] endOdoPos      Odo End position
      *
      */
      void delTargetsInRange(const OdoPosition startOdoPos, const OdoPosition endOdoPos);

      /**
      * Set the supposed travel direction
      *
      * Method used to set or change the defined travel direction. A call to this method will set the internal
      * travel direction status and sort the targets accordingly.
      * In Location, when travel direction changes, this method will exchange the data in current Ceiling speed
      * gradient, with the opposite values.
      *
      * @param[in]  stDir    Travel direction
      */
      void setSupposedTravelDir(const TravelDir stDir);

      /**
      * Set the adhesion value
      *
      * @param[in]  adhesionV    adhesion Value
      */
      void setAdhesionValue(const uint8_t adhesionV);

      /**
      * Get the adhesion value
      *
      * @return adhesion value.
      */
      uint8_t getAdhesionValue(void) const;

      /**
      * Get travel direction of stored targets
      *
      * Get a target iterator pointing at the first element.
      *
      * @return the supposed travel direction.
      */
      TravelDir getSupposedTravelDir(void) const;

      /**
      * Get target list changed
      *
      * Get a boolean indicating a new target or a change in conditional target status
      * or normal target deleted in location.
      *
      * @return Target list changed.
      */
      bool getTargetListChanged(void) const;

      /**
      * Get target list reversed
      *
      * Get a boolean indicating the target list is reversed in location
      *
      * @return Target list reversed
      */
      bool isTargetListReversed(void) const;

      /**
      * Get status of isTargetDeletedInList 
      *
      * Get a boolean indicating a deletion of target in target list.
      *
      * @return Target list changed.
      */
      bool isTargetDelInList(void) const;

      /**
      * Get target iterator
      *
      * Get a target iterator pointing at the first element.
      *
      * @return Pointer pointing to first element of the list.
      */
      MaTargetIterator getMATargetIter(void);

      /**
      * Get target iterator
      *
      * Get a target iterator pointing at the first element.
      *
      * @return Pointer pointing to first element of the list.
      */
      ConstMaTargetIterator getConstMATargetIter() const;

      /**
      * Get target iterator end pointer
      *
      * Get a target iterator pointing at one element after last element.
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      * @return Pointer pointing to logical last element of the list.
      */
      MaTargetIterator getMATargetIterEnd(void);

      /**
      * Get target constant iterator to end pointer
      *
      * Get a target iterator pointing at one element after last element.
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      * @return Constant Pointer pointing to logical last element of the list.
      */
      ConstMaTargetIterator getConstMATargetIterEnd() const;

      /**
      * Get supervised target iterator
      *
      * Get a supervised target iterator pointing at the first element.
      *
      * @return Pointer pointing to first element of the supervised target list.
      */
      SupervisedTargetIterator getSupervisedTargetIter(void);

      /**
      * Get Supervised target iterator end pointer
      *
      * Get a Supervised target iterator pointing at one element after last element.
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      * @return Pointer pointing to logical last element of the list.
      */
      SupervisedTargetIterator getSupervisedTargetIterEnd(void);

      /**
      * Get reverse iterator pointing to the last element of list
      *
      * Get a target iterator pointing at last element .
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      * @return Pointer pointing to last element of the list.
      */
      SupervisedTargetRevIterator getSupervisedRevTargetIter(void);

      /**
      * Get reverse Supervised target pointing to the theoretical element preceding the first element
      *
      * Get a Supervised target iterator pointing to the theoretical element preceding the first element.
      *
      * @return Pointer pointing to theoretical element preceding the first element.
      */
      SupervisedTargetRevIterator getSupervisedRevTargetIterEnd(void);

      /**
      * Is the supervised target list empty
      *
      * The function checks the target list and will return true if it is empty
      * @return boolean true if target is empty
      */
      bool isSupervisedTargetListEmpty() const;

      /**
      * Is the MA target list empty
      *
      * The function checks the MA target list and will return true if it is empty
      * @return boolean true if MA target list is empty
      */
      bool isMATargetListEmpty() const;

      /**
      * Is the Primary target reached
      *
      * Set to true when reached within MA margin of the primary target 
      * or when the normal primary target removed in Location mode.
      * Set to false again when any new Primary target created.
      *
      * @return boolean true if primary target is considered to be reached
      */
      bool isPrimaryTargetReached() const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractTargets* corePtr();

      /*************************/
      /* Data access functions */
      /*************************/

      /**
      * Get the primary target in a travel direction 
      *
      * @param[in] trvDir The direction of the primary target.
      * @return Pointer to primary target object. NULL if target list is empty
      */
      BaseTarget* getPrimaryTarget(TravelDir const trvDir);

      /**
      * Get the primary target 
      * @return Pointer to primary target object. NULL if target list is empty
      */
      BaseTarget* getPrimaryTarget(void);

      /**
      * Get the location Start primary target
      * @return Pointer to location start primary target object. NULL if target list is empty
      */
      BaseTarget* getLocationStartTarget();

      /**
      * Get the location end primary target
      * @return Pointer to location end primary target object. NULL if target list is empty
      */
      BaseTarget* getLocationEndTarget();

      /** Get current ceiling speed
      *
      * @return current ceiling speed [cm/s]
      */
      uint32_t getCurCeilingSpeed(void) const;

      /** Get current gradient
      *
      * @return current track gradient [0.1%]
      */
      int32_t getCurTrackGradient(void) const;

      /** Get current gradient
      *
      * @return current supervised gradient [0.1%]
      */
      int32_t getCurGradient(void) const;

      /** Set current ceiling speed
      *
      * @param[in] currentCeilingSpeed  current ceilingSpeed value
      */
      void setCurCeilingSpeed(const uint32_t currentCeilingSpeed);

      /** Set current gradient
      *
      * @param[in] currentTrackGradient  current gradient value
      */
      void setCurTrackGradient(const int32_t currentTrackGradient);

      /** Set current gradient
      *
      * @param[in] currentGradient  current gradient value
      */
      void setCurGradient(const int32_t currentGradient);

      /** Set Conditional Target Supervise
      *
      * @param[in] targetId         target id of the conditional target that had changed status
      * @param[in] supervise        boolean value to show if the target is supervised
      */
      void setConditionalTargetSupervise(const uint32_t targetId, const bool supervise);

      /** Getter function to check if train is inside free rolling target boundaries
      *
      * @return true if train is inside free rolling targets boundaries (between the free rolling start and end targets)
      * @return false if train is out of free rolling targets boundaries (outside the free rolling start and end targets)
      */
      bool getFreeRollingTargetActive() const;

      /** Getter function to check if train is within Odometer Invalid target boundaries
      * @return true if train is inside odometer invalid target boundaries (between the odometer invalid start and end targets)
      * @return false if train is out of odometer invalid targets boundaries (outside odometer invalid start and end targets)
      */
      bool getOdometerInvalidTargetActive() const;

      /** Set Odometer Invalid Target Active status
      * @param[in] status        boolean value, set true if train inside Odometer invalid start and end target and
                                 set false if train is outside  Odometer invalid start and end target
      */
      void setOdometerInvalidTargetActive(const bool status);

      /** Set Free Rolling Target Active status
      * @param[in] status        boolean value, set true if Train is inside Free Rolling Start and End targets
                                 And set false if train is outside Free Rolling Start and End targets
      */
      void setFreeRollingTargetActive(const bool status);


      /** Fetch all the gradient targets
      *
      * Implements the getGradientTargetList() function.
      * This function fetch the all gradient targets from the target list and will fill array of pointer
      * @param[in] gradTargArrPtr          pointer of array to store gradient targets pointer
      * @return number of gradient targets fetched
      */
      const uint8_t getGradientTargetList(GradientTarget* gradTargArrPtr[]) const;

      /**
       * Set train Safety Margin value
       * @param[in] value    Value to set for safety margin
      */
      void setSafetyMarginChangeTargetActive(const uint16_t value);

      /**
      * Invalid Gradient target Memory Allocation Error
      */
      const ATC::Event invalidGradientMemAllocation;

      /**
      * Invalid Primary target Memory Allocation Error
      */
      const ATC::Event invalidPrimaryMemAllocation;

      /**
      * Invalid Speed target Memory Allocation Error
      */
      const ATC::Event invalidSpeedMemAllocation;

      /**
      * Invalid Supervised target Memory Allocation Error
      */
      const ATC::Event invalidSupervisedMemAllocation;

      /**
      * Invalid Track Data target Memory Allocation Error
      */
      const ATC::Event invalidTrackDataMemAllocation;

      /**
      * Invalid Track Data target Memory Allocation Error
      */
      const ATC::Event invalidKeepTrackDataMemAllocation;

      /**
      * Invalid direction
      */
      const ATC::Event invalidDirection;

      /**
      * Remove gradient,track Data Item and ceiling targets in location mode
      */
      void removeTargetsAtStandStillInLocation();

      /**
      * Delete normal primary target in location mode
      */
      void removeNormalPrimaryTargetInLocation();

      /**
      * Delete supervised target of type SMSpeedRestrictionTarget
      */
      void removeSMSpeedRestrictionTarget();

      /**
      * Delete the all the target at standStill
      *
      * @param[in] status value to set
      */

      void setDelTargetAtStandStill(const bool status);

      /**
       * Get current train safety margin
       *
       * @return value of current train safety margin
       */
      uint16_t getSafetyMargin(void) const;

      /**
      * The maximum size for the cross compare data
      *
      * @return Returns the maximum data size for this item
      */
      uint32_t getWriteCrossCompareMaxSize() const;

      /**
      * Add attributes to cross compare
      * @param[in] buffer        The cross compare buffer to get store the attributes in
      */
      void writeCrossCompare(VFW_Buffer* const buffer) const;

      /** Get first warning  ceiling speed
      *
      * @return fw ceiling speed [cm/s]
      */
      uint32_t getCurrFwCeilingSpeed(void) const;

      /** Get second warning ceiling speed
      *
      * @return sw ceiling speed [cm/s]
      */
      uint32_t getCurrSwCeilingSpeed(void) const;

      /** Get service brake ceiling speed
      *
      * @return sb ceiling speed [cm/s]
      */
      uint32_t getCurrSbCeilingSpeed(void) const;

      /** Get emergency brake ceiling speed
      *
      * @return eb ceiling speed [cm/s]
      */
      uint32_t getCurrEbCeilingSpeed(void) const;

      /** Set fw ceiling speed
      *
      * @param[in] fwCS ceilingSpeed value
      */
      void setCurrFwCeilingSpeed(const uint32_t fwCS);

      /** Set sw ceiling speed
      *
      * @param[in] swCS ceilingSpeed value
      */
      void setCurrSwCeilingSpeed(const uint32_t swCS);

      /** Set sb ceiling speed
      *
      * @param[in] sbCS ceilingSpeed value
      */
      void setCurrSbCeilingSpeed(const uint32_t sbCS);

      /** Set eb ceiling speed
      *
      * @param[in] ebCS  ceilingSpeed value
      */
      void setCurrEbCeilingSpeed(const uint32_t ebCS);

      /** Set CS restriction due to SM change
      *
      * @param[in] smRestriction  restricted cs
      */
      void setSMSpeedRestriction(const uint32_t smRestriction);

      /** Get ceiling speed restriction due to SM change
      *
      * @return sm ceiling speed restriction [cm/s]
      */
      uint32_t getSMSpeedRestriction(void) const;

      /** Display all Ma targets on console
      *
      */
      void displayMaTarg();

      /** Display all supervised targets on console
      *
      */
      void displaySupTarg();

      /**
      * Delete related supervised targets of given MA target and pointers of related supervised targets from the list.
      *
      * @param[in]    bTarget pointer to the MA target.
      */
      void delAllRelatedSupvTarget(const BaseTarget* const bTarget);

      /**
      * Checks if the targetList can fit size number of elements in its remaining size.
      *
      * @param[in]    size number of elements to fit
      * @return       true if the elements fit
      */
      bool targetListFitsVolume(const size_t size) const;

      /**
      * Checks if the supervised targetList can fit size number of elements in its remaining size.
      *
      * @param[in]    size number of elements to fit
      * @return       true if the elements fit
      */
      bool supvTargetListFitsVolume(const uint32_t size) const;

      /**
      * remove passed targets
      *
      * As soon as the front of the train passes a target position the target can be removed.
      * Before removing a passed target, the method copies ceiling speed, brakeability, and Gradient
      * of the target to the values of current values, and then removes the target. The current values of
      * G/B/C shall always contain the data coming from initial MA or the last passed target
      */
      void removePassedTargets();

      /** Get free blocks available for speed targets
      *
      * @return number of free speed-target blocks available in memory-pool
      */
      uint32_t getFreeBlocksSpeed() const;

      /** Get free blocks available for Track Data Items
      *
      * @return number of free Track Data Items blocks available in memory-pool
      */
      uint32_t getFreeBlocksTDI() const;

      /** Get free blocks available for gradients targets
      *
      * @return number of free gradients target blocks available in memory-pool
      */
      uint32_t getFreeBlocksGrad() const;

    protected:
      /**
      * Constructor
      */
      AbstractTargets();

      /**
      * Inserts a target in the MA target list
      *
      * @param[in] target  The MA target to insert
      */
      void insertMaTargetInList(BaseTarget* const target);

      /**
      * Insert a target in the Supervised target list
      *
      * @param[in] target  The supervised target to insert
      */
      void insertSupvTargetInList(SupervisedTarget* const target);

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

      /**
      * Function for very detailed log of memory-pool counters
      *
      */
      virtual void veryDetailedLog(void) const;

      /**
      * Helper method to Write both to trace and NJRU/BDS
      *
      * @param[in]    traceLevel    The Trace level for this to be traced.
      * @param[in]    logLevel      The Log level for this to be logged.
      * @param[in]    text          The text log to be logged.
      */
      void traceLog(uint8_t const traceLevel, ATC::LogLevel const logLevel, const char_t* const text) const;

      /**
      * Target List Full Error
      */
      const ATC::Event targetListFullError;

      /**
      * memory for the Initialization not set properly
      */
      const ATC::Event initNotDone;

      /**
      * Base Target NULL pointer error
      */
      const ATC::Event baseTargetNULLError;

      /**
      * Unknown Direction of Target while adding it to List error
      */
      const ATC::Event unknownTargetDirectionError;

      /**
      * Inconsistency between target/track storage
      */
      const ATC::Event inconsistencyTargetTrack;

      /**
      * Same target ID error
      */
      const ATC::Event sameTargetIdError;

      /**
      * Event when exceeding container size
      */
      const ATC::Event contSizeExceeded;

      /**
      * Event supervised gradient at the position of previous supervised gradient
      */
      const ATC::Event twoSupGradAtSamePos;

      /**
      * Event for null pointer access
      */
      const ATC::Event nullPointerAcess;

      /**
      * Boolean to indicate train is withing Odometer Invalid target boundaries
      */
      bool odometerInvalidTargetActive;

      /**
      * Boolean to indicate Train is withing Free Rolling target boundaries
      */
      bool freeRollingTargetActive;

    private:

      /**
      * Manage Emergency Stop Active Signal Input
      */
      void manageEMSInput(void);

      /**
      * Flag to prevent multiple initialization.
      */
      bool coreInitDone;

      /**
      * Target List Object
      */
      MaTargetList maTargetList;

      /**
      * Supervised Target List Object
      */
      SupervisedTargetList supvTargetList;

      /**
      * Supposed Travel direction
      */
      TravelDir supposedTravelDir;

      /**
      * target list changed, indicates whether a new target is added and also a
      * conditional target had changed status or normal target deleted in location.
      * Removing a target except Normal target in Location mode does not affect this flag
      * the run function for target will reset this flag to false, so it is only valid for one cycle
      */
      bool targetListChanged;

      /**
      * target list reversed in Location mode
      */
      bool targetListReversed;

      /**
      * Indicates if primary target reached. 
      * Set to true when reached within MA margin of primary target.
      * Set to false when adding a primary target
      */
      bool reachedPrimaryTarget;

      /** Current ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t curCeilingSpeed;

      /** Current track gradient [0.1% cm/s^2]
      *
      * The current track gradient value from the MA.
      */
      int32_t curTrackGradient;

      /** Current supervised gradient [0.1% cm/s^2]
      *
      * The current gradient value, that affects the train from current position to the next target.
      */
      int32_t curGradient;

      /** Current ceiling speed restriction[cm/s]
      *
      * The ceiling speed restriction on the current CS.
      */
      uint32_t smSpeedRestriction;

      /**
      * Flag for target to delete
      */
      bool delTargetAtStandStill;

      /**
      * Current Safety Margin (cm).
      */
      uint16_t currentSafetyMargin;
      
      /**
      * Flag to indicate if any target has been deleted in target list.
      */
      bool isTargetDeletedInList;

      /**
      * The number of free primary-target blocks available in memory-pool from last cycle.
      */
      uint32_t oldFreeBlocksPrim;

      /**
      * The number of free gradient-target blocks available in memory-pool from last cycle.
      */
      uint32_t oldFreeBlocksGrad;

      /**
      * The number of free speed-target blocks available in memory-pool from last cycle.
      */
      uint32_t oldFreeBlocksSpeed;

      /**
      * The number of free TrackDataItems-target blocks available in memory-pool from last cycle.
      */
      uint32_t oldFreeBlocksTDI;

      /**
      * The number of free Supervised-target blocks available in memory-pool from last cycle.
      */
      uint32_t oldFreeBlocksSupervised;

      /**
      * The number of free primary-target blocks available in memory-pool.
      */
      uint32_t freeBlocksPrim;

      /**
      * The number of free gradient-target blocks available in memory-pool.
      */
      uint32_t freeBlocksGrad;

      /**
      * The number of free speed-target blocks available in memory-pool.
      */
      uint32_t freeBlocksSpeed;

      /**
      * The number of free TrackDataItems-target blocks available in memory-pool.
      */
      uint32_t freeBlocksTDI;

      /**
      * The number of free Supervised-target blocks available in memory-pool.
      */
      uint32_t freeBlocksSupervised;

      /** Current FW ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t curFWCeilingSpeed;

      /** Current SW ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t curSWCeilingSpeed;

      /** Current SB ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t curSBCeilingSpeed;

      /** Current EB ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t curEBCeilingSpeed;

      /** Adhesion value [percent]
      *
      * The adhesion value that is used to do brakeability calculations
      */
      uint8_t adhesionValue;

      /**
      * Insert Element in MA target List
      *
      * For internal use only
      *
      * @param[in] base  The target to add
      *
      * @return return true, if insertion of Target is successful
      */
      bool insertMaTargetInListInternal(BaseTarget * const base);

      /**
      * Insert Element in Supervised target List
      *
      * For internal use only
      *
      * @param[in] supvTarg  The target to add
      *
      * @return return true, if insertion of Target is successful
      */
      bool insertSupvTargetInListInternal(SupervisedTarget * const supvTarg);
    };
  }
}

#endif
