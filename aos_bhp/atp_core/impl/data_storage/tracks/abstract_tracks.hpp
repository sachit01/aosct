#ifndef AbstractTracks_hpp
#define AbstractTracks_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc defined.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-22    ljoars      Created
* 2016-05-13    arastogi    Modified to new design
* 2016-07-21    spandita    Updated and modified the declaration
* 2016-07-31    spandita    Updated the code with review comments and changed design
* 2016-08-08    spandita    Updated and optimized the code as per the review comment
* 2016-08-08    spandita    updated the event id for position mismatch
* 2016-08-08    spandita    Added the balise Iterator function
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-19    arastogi    Added update tracks function for balise search.
*                           Modified the getLastTrack function to check for empty tracklist.
*                           Added Console call
* 2016-10-05   arastogi      Added removePassed and removeNotCovered functions
*                            with odometer reading as input
****************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "track.hpp"
#include "balise.hpp"
#include "gp_list_iterator.hpp"
#include "gp_list.hpp"
#include "abstract_position.hpp"
#include "event.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {

    /**Class Errorhandler is being used for error code
    * of GP list */
    class ErrorHandler
    {
    public:

      static void report(const char_t *const str)
      {
        /**
        * Need Trace API for writing the error message
        *Currently it is not available
        *Cout operator Lint issue
        */

        /*The below code is added just to remove the warning
        *warning C4100 : 'str' : unreferenced formal parameter
        * The code needs to removed once trace API is available
        */
        ATC::TraceInterface trace("Abstract Tracks", 9U, str);
        trace.write(9U, str);
      }
    };

    class AbstractTracks;
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
    static AbstractTracks* coreTracksInstancePtr = static_cast<AbstractTracks*>(NULL);

    /**
    * The class AbstractTracks implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractTracks : public ATC::ProcComponent
    {
    public:
      /**
      * Constructor
      */
      AbstractTracks();

      /**
      * Maximum number of tracks that can be stored on-board
      */
      static const uint16_t maxNumberOfStoredTracks = 100U;

      /**
      * Maximum number of balises that can be stored on-board
      */
      static const uint16_t maxNumberOfStoredBalises = 200U;

      /**
      * Maximum Possession Balise Count
      */
      static const int8_t maxPossessionBaliseCount = 50;

      /**Type def for TrackListType */
      typedef ATC::GPList<Track *, maxNumberOfStoredTracks, ErrorHandler> TrackListType;

      /**Type def for TrackList Iterator Type */
      typedef TrackListType::iterator TrackListIteratorType;

      /**Type def for Track Iterator Type */
      typedef TrackListType::const_iterator ConstTrackListIteratorType;

      /**Type def for BaliseListType */
      typedef ATC::GPList<Balise*, maxNumberOfStoredBalises, ErrorHandler> BaliseListType;
      
      /**Type def for Balise List Iterator Type */
      typedef BaliseListType::iterator BaliseListIteratorType;

      /**Type def for Balise List Iterator Type */
      typedef BaliseListType::const_iterator ConstBaliseListIteratorType;

      /**
      * Implements the virtual init function.
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
      */
      virtual void run(void);

      /**
      * Interface to call different level of Console Command
      *
      * @param[in] argc  Number of arguments in the argument array argv
      * @param[in] argv  Arguments array
      *
      * @return true if the Call is successful.
      */
      virtual bool consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv);

      /** Add track
      *
      * Implements the addTrack(Track &) function.
      * Adds track entry to track list.
      *
      * @param[in]  track Reference to track to be added.
      * @return     true if successful, false if failed.
      */
      bool addTrack(const  Track &track);

      /** Add Balise
      * Implements the addBalise(Balise &) function.
      * Adds Balise ID to Balise list.
      *
      * @param[in]  balise Reference to balise to be added.
      * @return     true if successful, false if failed.
      */
      bool addBalise(const Balise& balise);

      /** Remove all passed tracks
      *
      * Implements the functionality to remove all passed tracks.
      * All tracks before and excluding 'safeTrailingOdo' are removed.
      * Also the balise objects for those tracks are removed
      *
      * @param[in]  safeTrailingOdo  Safe passed position to remove tracks
      * @return     true if successful, false if failed.
      */
      bool removePassed(const OdoPosition& safeTrailingOdo);

      /** Remove all tracks outside the range of position specified.
      *
      * Implements the functionality to remove tracks outside the 2 positions.
      * Check if the rear position is before front position in the travel direction.
      * All tracks before and excluding 'safeLeadingOdo' are removed.
      * All tracks after and excluding 'safeTrailingOdo' are removed.
      * Also the balise objects for those tracks are removed
      *
      * @param[in]  safeTrailingOdo   Safe trailing odometer position to remove tracks
      * @param[in]  safeLeadingOdo    Safe leading odometer position to remove tracks
      * @return     true if successful, false if failed.
      */
      bool removeNotCovered(const OdoPosition& safeTrailingOdo, const OdoPosition& safeLeadingOdo);

      /**
      * Removes all track and balise objects in list.
      * Immediate removal no checks performed.
      */
      void removeAll();

      /** Check if odometer position corresponds to a valid TrackAndPos value.
      *
      * @param[in]  odoPos    odometer reading to check
      * @return     true if the odometer reading is valid, false otherwise.
      */
      bool checkOdoPos(const OdoPosition odoPos) const;

      /** Check if track and position corresponds to a valid odometer value.
      *
      * @param[in]  trackAndPos    track and position to check
      * @return     true if the track and position is valid, false otherwise.
      */
      bool checkTrackAndPos(const TrackAndPos &trackAndPos) const;

      /** Get track and position value from odometer value.
      *
      * @param[in]  odoPos    odometer reading to translate
      * @return     translated track and position value if valid, null otherwise.
      */
      TrackAndPos calculateTrackAndPos(const OdoPosition odoPos) const;

      /** Get odometer value form the track and position value.
      *
      * @param[in]    trackAndPos   track and position value to translate
      * @param[out]   odoPos        calculated odoposition
      * @return     true if Successful.
      */
      bool getOdoPos(const TrackAndPos &trackAndPos, OdoPosition &odoPos) const;

      /** Get Balise object from balise id.
      *
      * @param[in]  baliseId    ID of the balise
      * @return     Balise object if balise id is in baliseList, null otherwise.
      */  
      const Balise* getBalise(const uint16_t baliseId) const;

      /** Get Balise object from track and pos.
      *
      * @param[in]  trackAndPos   track and pos of balise
      * @return     Balise object if a balise with track and pos exist in the balise list, null otherwise.
      */
      const Balise* getBalise(const TrackAndPos& trackAndPos) const;

      /** Get Track object from track id.
      *
      * @param[in]  trackId    ID of the track
      * @return     Track object if track id is in trackList, null otherwise.
      */
      const Track* getTrack(const uint16_t trackId) const;

      /** Get the last track in the trackList.
      *
      * @return   The last track, if the track list empty, NULL is returned.
      */
      const Track* getLastTrack() const;

      /** Get track iterator
      *
      * Get a track iterator pointing at the first element.
      *
      */
      ConstTrackListIteratorType getTracksIter() const;

      /** Get balise iterator
      *
      * Get a balise iterator pointing at the first element.
      *
      */

      ConstBaliseListIteratorType getBaliseIter() const;

      /** Get track iterator end pointer
      *
      * Get a track iterator pointing at one element after last element.
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      */
      ConstTrackListIteratorType getTracksIterEnd() const;

      /** Get balise iterator end pointer
      *
      * Get a balise iterator pointing at one element after last element.
      * The returned iterator shall only be used for comparative tests in loops,
      * not for access.
      *
      */
      ConstBaliseListIteratorType getBaliseIterEnd() const;

      /**
      * Get current travel direction
      *
      * @return travel direction.
      */
      TravelDir getTravelDirection() const;


      /** Set travel direction of stored tracks
      *
      * Method used to set or change the defined travel direction.
      * A call to this method will set the internal travel direction
      * status and sort the tracks accordingly.
      *
      * @param[in]  travelDir    Travel direction
      */
      void setTravelDirection(const TravelDir travelDir);

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractTracks* corePtr();

      /**
      * Add all attributes to cross compare
      *
      * @param[in] buffer        The cross compare buffer to get store the attributes in
      */
      void writeCrossCompare(VFW_Buffer* const buffer) const;

      /**
      * The maximum size for the cross compare data
      *
      * @return the maximum data size for this item
      */
      uint32_t getWriteCrossCompareMaxSize() const;

      /**
      * Check if balise is present in the possession balise list
      *
      * @param[in] id        Balise ID
      *
      * @return true if balise is present in the list.
      */

      bool isBaliseInPossessionList(const uint16_t id) const;

      /**
      * Add the balise ID to possession balise list
      *
      * @param[in] id    Balise ID
      *
      * @return true if balise added sucessfully.
      */

      bool addPossessionBalise(const uint16_t id);

      /**
      * Remove all the balises from possession balise list
      *
      */
      void clearPossessionBaliseList();

      /**
      * Is the track list empty
      *
      * The function checks the track list and will return true if it is empty
      * @return boolean true if tracks is empty
      */
      bool isTrackListEmpty() const;

      /**
      * Flip the train orientation in the tracks.
      * 
      * The function flips the train orientation in tracks and reverses the track list.
      * Odo values of balises are recalculated and balise list is reversed. 
      */
      void flipTrackOdoDir();

    private:

      /**
      * Current Possession list index
      */
      int8_t possessionListIndex;
      /**
      * Array for storage for Possession Balises
      */
      uint16_t possessionBaliseList[maxPossessionBaliseCount];

      /** Remove all tracks outside the range of position specified.
      *
      * Implements the functionality to remove tracks outside the 2 positions.
      * Check if the rear position is before front position in the travel direction.
      * All tracks before and excluding safeTrailingPos are removed.
      * All tracks after and excluding safeLeadingPos are removed.
      * Also the balise objects for those tracks are removed
      *
      * @param[in]  safeTrailingPos   Safe trailing position to remove tracks
      * @param[in]  safeLeadingPos    Safe leading position to remove tracks
      * @return     true if successful, false if failed.
      */
      bool removeNotCovered(const TrackAndPos& safeTrailingPos, const TrackAndPos& safeLeadingPos);

      /** Remove all passed tracks
      *
      * Implements the functionality to remove all passed tracks.
      * All tracks before and excluding safeTrailingPos are removed.
      * Also the balise objects for those tracks are removed
      *
      * @param[in]  safeTrailingPos  Safe passed position to remove tracks
      *
      * @return     true if successful, false if failed.
      */
      bool removePassed(const TrackAndPos& safeTrailingPos);

      /**
      * Delete balise from baliseList
      *
      * @param[in]  baliseId    id of the balise to delete
      * @return true if balise was found and deleted.
      */
      bool removeBalise(const uint16_t baliseId);

      /**
      * Delete track from trackList
      *
      * @param[in]  trackId    id of the track to delete
      * @return true if track was found and deleted.
      */
      bool removeTrack(const uint16_t trackId);

      /**
      * An event to report error
      */
      const ATC::Event invalidPosInTrack;

      /**
      * An event to report invalid incoming track when adding track.
      */
      const ATC::Event invalidIncomingTrack;

      /**
      * An event to report invalid incoming track direction when adding track.
      */
      const ATC::Event invalidIncomingTrackDir;

      /**
      * Event when update tracks is called with invalid start point.
      */
      const ATC::Event invalidUpdateStartPoint;

      /**
      * Track List Failure error
      */
      const ATC::Event fullTrackListFailure;

      /**
      * Balise List Failure error
      */
      const ATC::Event fullBaliseListFailure;

      /**
      * Failed to remove Existing balise
      */
      const ATC::Event removeExistingBaliseFailure;

      /**
      * Initialization not done error
      */
      const ATC::Event initNotDone;

      /**
      * Out of memory error
      */
      const ATC::Event outOfMemoryFailure;

      /**
      *occurs when limits of track and balise position are mismatch
      */
      const ATC::Event unDefLimitFailure;

      /**
      * Inconsistency between balise/track storage
      */
      const ATC::Event inconsistencyBaliseTrack;

      /**
      * When trying to add a balise with same id but different position
      */
      const ATC::Event modifyExistingBalise;

      /**
      * When trying to add a track with same id but different values
      */
      const ATC::Event modifyExistingTrack;

      /**
      * Flag to prevent multiple initialization.
      */
      bool initDone;

      /** Tracks list */
      TrackListType  trackList;

      /** Balise list */
      BaliseListType baliseList;

      /** Travel direction currently used */
      TravelDir travelDirection;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      void initCrossCompare() const;

    };
  }
}
#endif
