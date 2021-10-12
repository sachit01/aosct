/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc implemented.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-13   arastogi      Created
* 2016-07-21   spandita      Created and updated the function.
* 2016-07-31   spandita      Updated the code with review comments and changed design
* 2016-08-08   spandita      Updated and optimized the code as per the review comment
* 2016-08-08   spandita      Added log if track limit and balise position are mismatch
* 2016-08-08   spandita      Added the balise Iterator function
* 2016-08-22   spandita      Changed the function from get rear position to get current antennapos
* 2016-09-19   akushwah    Corrected Init function
* 2016-09-19   arastogi      Added update tracks functions for balise search.
*                            Modified the getLastTrack function to check for empty tracklist.
*                            Added Console call.
* 2016-10-05   arastogi      Added removePassed and removeNotCovered functions
*                            with odometer reading as input
* 2016-10-12   arastogi      Fixed null pointer problem when updating tracks odo
*                            Fixed the issue that balises should be removed only
*                            if track is removed.
* 2016-10-14    arastogi     Fixed calculating odometer for first added balise.
*                            Remove passed tracks should not delete the safe pos track.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_tracks.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
#include "abstract_tracks_event_ids.hpp"

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace DS
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    AbstractTracks::AbstractTracks() : ATC::ProcComponent(atpTracksId, "Tracks", "TRK"),
      // creating different set of objects for different type of events
      invalidPosInTrack(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdInvalidPosInTrack,
        ATC::NoSB, DMICom::tracksInvalidPosInTrk, "Invalid position in track ")),
      invalidIncomingTrack(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdInvalidIncomingTrack,
        ATC::NoSB, DMICom::tracksInvalidTrack, "Invalid incoming track when adding track")),
      invalidIncomingTrackDir(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdInvalidIncomingTrackDir,
        ATC::NoSB, DMICom::tracksInvalidTrkDir, "Invalid incoming track direction when adding track")),
      invalidUpdateStartPoint(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdInvalidUpdateStartPoint,
        ATC::NoSB, DMICom::tracksInternalFailure, "Invalid start point in update tracks")),
      fullTrackListFailure(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdTrackListFullError,
        ATC::NoEB, DMICom::tracksTrkListFull, "Track list full Error")),
      fullBaliseListFailure(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdBaliseListFullError,
        ATC::NoEB, DMICom::tracksBalListFull, "Balise list full Error")),
      removeExistingBaliseFailure(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdRemoveExistingBaliseFailure,
        ATC::NoEB, DMICom::tracksBalRemovalFailure, "Removing balise Error")),
      initNotDone(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdInitNotDone,
        ATC::NoEB, DMICom::tracksInternalFailure, "Initialization of Memory not done")),
      outOfMemoryFailure(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdOutOfMemory,
        ATC::NoSB, DMICom::tracksInternalFailure, "Out of Memory")),
      unDefLimitFailure(ATC::Event::createSafeBrakeSBEvent(atpTracksId, ATC::CoreContainer, eventIdPosUndef,
        ATC::NoSB, DMICom::tracksPosLimitFailureInTrk, "Position limits are mismatch")),
      inconsistencyBaliseTrack(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdInconsistencyBaliseTrack,
        ATC::NoEB, DMICom::tracksInconsistentTrkandBal, "Inconsistency between balise/track storage")),
      modifyExistingBalise(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdModifyExistingBalise,
        ATC::NoEB, DMICom::tracksSameBalIdFound, "Adding balise with same id but different position, Balise ID:", true)),
      modifyExistingTrack(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdModifyExistingTrack,
        ATC::NoEB, DMICom::tracksSameTrkIdFound, "Adding track with same id but different position")),
      initDone(false)
    {
      if (coreTracksInstancePtr != 0)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Tracks Constructor already instantiated");
      }
      // Setup single instance pointer for core access
      coreTracksInstancePtr = this;
      //reset the possession balise list
      clearPossessionBaliseList();
    }
    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTracks::init(void)
    {
      if (!initDone)
      {
        trackList.clear();
        baliseList.clear();
        bool initTrackDone = Track::initMemPoolSize(maxNumberOfStoredTracks);
        bool initBaliseDone = Balise::initMemPoolSize(maxNumberOfStoredBalises);

        if (initTrackDone && initBaliseDone)
        {
          initCrossCompare();
          initDone = true;
        }
        else
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(initNotDone, __FILE__,
            __LINE__);
          initDone = false;
        }
      }
      return initDone;
    }
    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTracks::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TRK_run");
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTracks* AbstractTracks::corePtr(void)
    {
      return coreTracksInstancePtr;
    }

    /******************************************************************************
    * Add Balise Function
    ******************************************************************************/
    bool AbstractTracks::addBalise(const Balise &balise)
    {
      bool isBaliseAdded = false;
      OdoPosition odoPos;
      if (getOdoPos(balise.getPosition(), odoPos))
      {
        //check for baliselist is empty
        if (baliseList.empty())
        {
          Balise* const balisePtr = new Balise(balise);
          // Check if allocation is Null
          if (balisePtr == static_cast<Balise*>(NULL))
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(outOfMemoryFailure, __FILE__,
              __LINE__);
          }
          else
          {
            // Add balise to balise list
            baliseList.pushBack(balisePtr);
            isBaliseAdded = true;
          }
        }
        else
        {
          //Get the Existing balise from balise list
          const Balise* const existingBalise = getBalise(balise.getBaliseId());

          if (static_cast<Balise*>(NULL) != existingBalise)
          {
            if (!removeBalise(existingBalise->getBaliseId()))
            {
              //Raise safety halt 
              ATC::AbstractEventHandler::corePtr()->reportEvent(removeExistingBaliseFailure, __FILE__,
                __LINE__);
            }
          }

          if (maxNumberOfStoredBalises > baliseList.size())
          {
            Balise* const balisePtr = new Balise(balise);
            // Check if allocation is Null
            if (balisePtr == static_cast<Balise*>(NULL))
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(outOfMemoryFailure, __FILE__,
                __LINE__);
            }
            else
            {
              const bool directionForward = (DirForward == getTravelDirection());
              const bool directionReverse = (DirReverse == getTravelDirection());

              if (directionForward || directionReverse)
              {
                BaliseListIteratorType baliseItr = baliseList.begin();
                const OdoPosition balisePosition = balise.getOdoPosition();

                for (; (baliseItr != baliseList.end()) && (!isBaliseAdded); ++baliseItr)
                {
                  const OdoPosition iterPosition = (*baliseItr)->getOdoPosition();

                  if ((directionForward && (iterPosition > balisePosition)) ||
                      (directionReverse && (iterPosition < balisePosition)))
                  {
                    static_cast<void>(baliseList.insert(baliseItr, balisePtr));
                    isBaliseAdded = true;
                  }
                }
              }

              if (!isBaliseAdded)
              {
                baliseList.pushBack(balisePtr);
                isBaliseAdded = true;
              }
            }
          }
          else // if (maxNumberOfStoredBalises > baliseList.size())
          {
            //Raise safety halt in case list is full
            ATC::AbstractEventHandler::corePtr()->reportEvent(fullBaliseListFailure, __FILE__,
              __LINE__);
          }
        }
      }

      return isBaliseAdded;
    }

    /******************************************************************************
    * Add track Function
    ******************************************************************************/
    bool AbstractTracks::addTrack(const Track &track)
    {
      bool isTrackAdd = false;
      const uint16_t trackId = track.getTrackId();
      const uint16_t incomingTrackId = track.getIncomingTrackId();
      bool isTrkListEmpty = trackList.empty();
      //Track pointer
      Track *trackPtr = new Track(track);

      //is out of memory?
      if (NULL != trackPtr)
      {
        //Check if track list is empty
        if (isTrkListEmpty)
        {
          isTrackAdd = true;
          trackList.pushBack(trackPtr); /**Add the track pointer to list */
          setTravelDirection(trackPtr->getTravelDirection());
        }
        else
        {
          /**check for remaining size of list */
          if (maxNumberOfStoredTracks > trackList.size())
          {
            //Get the Existing track from track list
            const Track* const pExistingTrack = getTrack(trackId);

            if (0U != incomingTrackId)
            {
              //Is travel direction of incoming track same ?
              if (getTravelDirection() == track.getTravelDirection())
              {
                isTrackAdd = true;

                //Check for track id presence in list
                if (pExistingTrack != static_cast<const DS::Track*>(NULL))
                {
                  isTrackAdd = removeTrack(pExistingTrack->getTrackId());
                }

                if (isTrackAdd)
                {
                  trackList.pushBack(trackPtr);
                  setTravelDirection(trackPtr->getTravelDirection());
                }
              }
              else
              {
                /**in case incoming track is in wrong direction */
                ATC::AbstractEventHandler::corePtr()->reportEvent(invalidIncomingTrackDir, __FILE__,
                  __LINE__);
              }
            }
            //if adding the track with incoming id as 0, the track should be in the track list.
            else
            {
              if(getTravelDirection() != trackPtr->getTravelDirection())
              {
                setTravelDirection(trackPtr->getTravelDirection());
                // a change of direction: remove all old tracks and balises before adding the new ones
                removeAll();
                trackList.pushBack(trackPtr);
                isTrackAdd = true;
              }
              else if (pExistingTrack != static_cast<const DS::Track*>(NULL))
              {
                if (*pExistingTrack == track)
                {
                  delete(trackPtr);
                  isTrackAdd = true;
                }
                else
                {
                  //iterate to that existing track and get the iterator 
                  TrackListIteratorType trackPointItr = trackList.begin();
                  bool isTrackFound = false;
                  /**Searching for target Point */
                  for (TrackListIteratorType trackListPtr = trackList.begin(); ((trackListPtr != trackList.end()) && (!isTrackFound));)
                  {
                    if (trackPtr->getTrackId() == (*trackListPtr)->getTrackId())
                    {
                      isTrackFound = true;
                      delete(*trackListPtr);
                      trackListPtr = trackList.erase(trackListPtr);
                      trackPointItr = trackListPtr;
                    }
                    else
                    {
                      ++trackListPtr;
                    }
                  }

                  if (isTrackFound)
                  {
                    isTrackAdd = true;
                    trackPointItr = trackList.insert(trackPointItr, trackPtr);

                  }
                  else
                  {
                    isTrackAdd = true;
                    trackList.pushBack(trackPtr);
                  }
                }
              }
              else
              {
                removeAll();
                //add the track
                isTrackAdd = true;
                trackList.pushBack(trackPtr);
              }
            }
          }
          else
          {
            //Raise safety halt in case  list is full
            ATC::AbstractEventHandler::corePtr()->reportEvent(fullTrackListFailure, __FILE__,
              __LINE__);
          }
        }
      }
      else
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(outOfMemoryFailure, __FILE__,
          __LINE__);
      }
      return isTrackAdd;
    }

    /******************************************************************************
    * Remove all Balises and Tracks
    ******************************************************************************/
    void AbstractTracks::removeAll()
    {
      for (TrackListIteratorType trackItr = trackList.begin(); trackItr != trackList.end(); ++trackItr)
      {
        delete *trackItr;
      }
      trackList.clear();

      for (BaliseListIteratorType baliseItr = baliseList.begin(); baliseItr != baliseList.end(); ++baliseItr)
      {
        delete *baliseItr;
      }
      baliseList.clear();
    }

    /******************************************************************************
    * Remove all Balise and Tracks before safeLeadingOdo and after safeTrailingOdo
    * excluding safeTrailingOdo and safeLeadingOdo
    ******************************************************************************/
    bool AbstractTracks::removeNotCovered(const OdoPosition &safeTrailingOdo, const OdoPosition& safeLeadingOdo)
    {
      bool retVal = false;

      const TrackAndPos safeTrailingPos = calculateTrackAndPos(safeTrailingOdo);
      const TrackAndPos safeLeadingPos = calculateTrackAndPos(safeLeadingOdo);

      //check if the positions are correct with respect to direction
      bool checkDir = (((safeTrailingOdo < safeLeadingOdo) && (travelDirection == DirForward)) ||
        ((safeTrailingOdo > safeLeadingOdo) && (travelDirection == DirReverse)));

      //check if odometer reading translates to valid track and pos.
      if (checkDir)
      {
        retVal = removeNotCovered(safeTrailingPos, safeLeadingPos);
      }

      return retVal;
    }

    /******************************************************************************
    * Remove all Balise and Tracks before safeLeadingPos and after safeTrailingPos
    * excluding safeLeadingPos and safeTrailingPos
    ******************************************************************************/
    bool AbstractTracks::removeNotCovered(const TrackAndPos &safeTrailingPos, const TrackAndPos &safeLeadingPos)
    {
      // Flags for removal status
      bool isRemovedRear = false;
      bool isRemovedFront = false;

      TrackListIteratorType interFrontTrackIterator = trackList.begin();

      // Any tracks to remove at rear?
      if (safeTrailingPos.track != 0U)
      {
        isRemovedRear = removePassed(safeTrailingPos);
      }

      // Any tracks to remove at front?
      if (safeLeadingPos.track != 0U)
      {
        isRemovedFront = true;

        TrackListIteratorType trackIterator = trackList.begin();
        for (; trackIterator != trackList.end(); ++trackIterator)
        {
          if (safeLeadingPos.track == ((*trackIterator)->getTrackId()))
          {
            interFrontTrackIterator = trackIterator;
            break;
          }
        }
       
        //Removing the Track from Front position
        ++interFrontTrackIterator;
        while (interFrontTrackIterator != trackList.end())
        {
          delete (*interFrontTrackIterator);
          interFrontTrackIterator = trackList.erase(interFrontTrackIterator);
        }
      }

      // Remove the balises
      for (BaliseListIteratorType baliseIterator = baliseList.begin(); baliseIterator != baliseList.end();)
      {
        // If the track has been deleted from tracklist.
        if (getTrack((*baliseIterator)->getPosition().track) == static_cast<const DS::Track*>(NULL))
        {
          delete(*baliseIterator);
          baliseIterator = baliseList.erase(baliseIterator);
        }
        else
        {
          ++baliseIterator;
        }
      }

      return (isRemovedRear || isRemovedFront);
    }

    /******************************************************************************
    * get Odo position from TandP
    ******************************************************************************/
    bool  AbstractTracks::getOdoPos(const TrackAndPos &trackAndPos, OdoPosition &odoPos) const
    {
      bool odoPosDone = false;  //Set to false. Change only when true
      ConstTrackListIteratorType trackLocalItr = trackList.begin();
      for (; trackLocalItr != trackList.end(); ++trackLocalItr)
      {
        if ((*trackLocalItr)->isTandPInTrack(trackAndPos))
        {
          if (OdoPositive == (*trackLocalItr)->getOdoDirection())
          {
            odoPos = (*trackLocalItr)->getOdoAt0Cm() + static_cast<int32_t>(trackAndPos.position);
          }
          else
          {
            odoPos = (*trackLocalItr)->getOdoAt0Cm() - static_cast<int32_t>(trackAndPos.position);
          }
          odoPosDone = true;
          break;
        }
      }

      return odoPosDone;
    }


    /******************************************************************************
    * isTrackListEmpty
    ******************************************************************************/
    bool AbstractTracks::isTrackListEmpty() const
    {
      return trackList.empty();
    }

    /******************************************************************************
    * get TandP from OdoPosition
    ******************************************************************************/
    TrackAndPos AbstractTracks::calculateTrackAndPos(const OdoPosition odoPos) const
    {
      TrackAndPos localTrackpos;
      localTrackpos.position = 0U;
      localTrackpos.track = 0U;
      ConstTrackListIteratorType trackLocalItr = trackList.begin();

      for (; trackLocalItr != trackList.end(); ++trackLocalItr)
      {
        if ((**trackLocalItr).isOdoPosInTrack(odoPos))
        {
          localTrackpos.track = (**trackLocalItr).getTrackId();
          if (OdoPositive == (**trackLocalItr).getOdoDirection())
          {
            localTrackpos.position = static_cast<uint32_t>(odoPos) - static_cast<uint32_t>((**trackLocalItr).getOdoAt0Cm());
          }
          else
          {
            localTrackpos.position = static_cast<uint32_t>((**trackLocalItr).getOdoAt0Cm()) - static_cast<uint32_t>((odoPos));
          }
          break;
        }
        else
        {
          //TBD
        }
      }
      return localTrackpos;
    }
    /******************************************************************************
    * Check if odometer position corresponds to a valid TrackAndPos value
    ******************************************************************************/
    bool AbstractTracks::checkOdoPos(const OdoPosition odoPos) const
    {
      bool isOdoposfound = false;
      for (ConstTrackListIteratorType trackLocalItr = trackList.begin(); trackLocalItr != trackList.end(); ++trackLocalItr)
      {
        if ((**trackLocalItr).isOdoPosInTrack(odoPos))
        {
          isOdoposfound = true;
          break;
        }
        else
        {
          isOdoposfound = false;

        }

      }
      return isOdoposfound;
    }
    /******************************************************************************
    * Check if track and position corresponds to a valid odometer value
    ******************************************************************************/
    bool AbstractTracks::checkTrackAndPos(const TrackAndPos& trackAndPos) const
    {
      bool isTrackAndPosfound = false;
      ConstTrackListIteratorType trackLocalItr = trackList.begin();
      for (; trackLocalItr != trackList.end(); ++trackLocalItr)
      {
        if ((**trackLocalItr).isTandPInTrack(trackAndPos))
        {
          isTrackAndPosfound = true;
          break;
        }
        else
        {
          isTrackAndPosfound = false;
        }
      }
      return isTrackAndPosfound;
    }

    /******************************************************************************
    * Get the last track in the trackList
    ******************************************************************************/
    const Track* AbstractTracks::getLastTrack() const
    {
      const Track* pTrack = static_cast<const Track*>(NULL);

      if (!trackList.empty())
      {
        pTrack = trackList.back();
      }

      return pTrack;
    }

    /******************************************************************************
    * Get Track object from track id
    ******************************************************************************/
    const Track* AbstractTracks::getTrack(const uint16_t trackId) const
    {
      const Track* pTrack = static_cast<const Track*>(NULL);

      ConstTrackListIteratorType trackLocalItr = trackList.begin();

      for (; trackLocalItr != trackList.end(); ++trackLocalItr)
      {
        if (trackId == (**trackLocalItr).getTrackId())
        {
          pTrack = (*trackLocalItr);
          break;
        }
      }
      return pTrack;
    }

    /******************************************************************************
    * Delete track from trackList
    ******************************************************************************/
    bool AbstractTracks::removeTrack(const uint16_t trackId)
    {
      bool removeDone = false;

      TrackListIteratorType trackLocalItr = trackList.begin();
      for (; trackLocalItr != trackList.end();)
      {
        if (trackId == (**trackLocalItr).getTrackId())
        {
          delete(*trackLocalItr);
          trackLocalItr = trackList.erase(trackLocalItr);
          removeDone = true;
          break;
        }
        else
        {
          ++trackLocalItr;
        }
      }

      return removeDone;
    }
    /******************************************************************************
    * Get Balise object from balise id.
    ******************************************************************************/
    const Balise* AbstractTracks::getBalise(const uint16_t baliseId) const
    {
      ConstBaliseListIteratorType baliseLocalItr = baliseList.begin();
      const DS::Balise* foundBalise = static_cast<const DS::Balise* const>(NULL);

      for (; baliseLocalItr != baliseList.end(); ++baliseLocalItr)
      {
        if (baliseId == (*baliseLocalItr)->getBaliseId())
        {
          foundBalise = (*baliseLocalItr);
          break;
        }
      }

      return foundBalise;
    }

    /******************************************************************************
    * Get Balise object from balise track and pos.
    ******************************************************************************/
    const Balise* AbstractTracks::getBalise(const TrackAndPos& trackAndPos) const
    {
      ConstBaliseListIteratorType baliseLocalItr = baliseList.begin();
      const DS::Balise* foundBalise = static_cast<const DS::Balise* const>(NULL);

      for (; baliseLocalItr != baliseList.end(); ++baliseLocalItr)
      {
        if (trackAndPos == (*baliseLocalItr)->getPosition())
        {
          foundBalise = (*baliseLocalItr);
          break;
        }
      }

      return foundBalise;
    }

    /******************************************************************************
    * Delete balise from baliseList
    ******************************************************************************/
    bool AbstractTracks::removeBalise(const uint16_t baliseId)
    {
      bool removeDone = false;

      BaliseListIteratorType baliseLocalItr = baliseList.begin();
      for (; baliseLocalItr != baliseList.end();)
      {
        if (baliseId == (**baliseLocalItr).getBaliseId())
        {
          delete(*baliseLocalItr);
          baliseLocalItr = baliseList.erase(baliseLocalItr);
          removeDone = true;
          break;
        }
        else
        {
          ++baliseLocalItr;
          removeDone = false;
        }
      }

      return removeDone;
    }


    /******************************************************************************
    * writeCrossCompare
    ******************************************************************************/
    void AbstractTracks::writeCrossCompare(VFW_Buffer* const buffer) const
    {
      ConstTrackListIteratorType iter = trackList.begin();
      ConstTrackListIteratorType iterEnd = trackList.end();

      // Write the tracks list
      while (iter != iterEnd)
      {
        (*iter)->writeCrossCompare(buffer);
        ++iter;
      }

      // Write the balise list
      for (ConstBaliseListIteratorType baliseLocalItr = baliseList.begin(); baliseLocalItr != baliseList.end(); ++baliseLocalItr)
      {
        (*baliseLocalItr)->writeCrossCompare(buffer);
      }

    }

    /******************************************************************************
    * getWriteCrossCompareMaxSize
    ******************************************************************************/
    uint32_t AbstractTracks::getWriteCrossCompareMaxSize() const
    {
      ConstTrackListIteratorType iter = trackList.begin();
      ConstTrackListIteratorType iterEnd = trackList.end();

      uint32_t ccMaxSize = 0U;

      // Get the sizes from the tracks list
      while (iter != iterEnd)
      {
        ccMaxSize += (*iter)->getWriteCrossCompareMaxSize();
        ++iter;
      }

      // Get the sizes from the balise list
      for (ConstBaliseListIteratorType baliseLocalItr = baliseList.begin(); baliseLocalItr != baliseList.end(); ++baliseLocalItr)
      {
        ccMaxSize += (*baliseLocalItr)->getWriteCrossCompareMaxSize();
      }
      return ccMaxSize;
    }

    /******************************************************************************
    * Get current travel direction
    ******************************************************************************/
    TravelDir AbstractTracks::getTravelDirection() const
    {
      return this->travelDirection;
    }

    /******************************************************************************
    * Get track iterator end pointer
    ******************************************************************************/
    AbstractTracks::ConstTrackListIteratorType AbstractTracks::getTracksIterEnd() const
    {
      return (AbstractTracks::trackList.end()); //logical end 
    }

    /******************************************************************************
    *  Get track iterator
    ******************************************************************************/
    AbstractTracks::ConstTrackListIteratorType AbstractTracks::getTracksIter() const
    {
      return(AbstractTracks::trackList.begin());
    }

    /******************************************************************************
    *  Get Balise iterator
    ******************************************************************************/
    AbstractTracks::ConstBaliseListIteratorType AbstractTracks::getBaliseIter() const
    {
      return(AbstractTracks::baliseList.begin());
    }

    /******************************************************************************
    * Get balise iterator end pointer
    ******************************************************************************/
    AbstractTracks::ConstBaliseListIteratorType AbstractTracks::getBaliseIterEnd() const
    {
      return (AbstractTracks::baliseList.end()); //logical end 
    }

    /******************************************************************************
    *  Remove all passed tracks before safeTrailingOdo (excluding safeTrailingOdo)
    ******************************************************************************/
    bool AbstractTracks::removePassed(const OdoPosition& safeTrailingOdo)
    {
      bool success = false;

      //check if odometer reading translates to valid track and pos.
      const TrackAndPos safeTrailingPos = calculateTrackAndPos(safeTrailingOdo);
      if (safeTrailingPos.track != 0U)
      {
        success = removePassed(safeTrailingPos);
      }

      return success;
    }

    /******************************************************************************
    *  Remove all passed tracks before safeTrailingPos (excluding safeTrailingPos)
    ******************************************************************************/
    bool AbstractTracks::removePassed(const TrackAndPos& safeTrailingPos)
    {
      bool success = false;

      //if track is in tracklist
      if (getTrack(safeTrailingPos.track) != static_cast<const DS::Track*>(NULL))
      {
        success = true;

        //iterate till before the safe rear track
        for (TrackListIteratorType trackIterator = trackList.begin(); trackIterator != trackList.end();)
        {
          if ((*trackIterator)->getTrackId() == safeTrailingPos.track)
          {
            break;
          }
          //delete the tracks
          delete(*trackIterator);
          trackIterator = trackList.erase(trackIterator);
        }

        //found position should be present
        for (BaliseListIteratorType baliseIterator = baliseList.begin(); baliseIterator != baliseList.end();)
        {
          //If the track has been deleted from tracklist.
          if (getTrack((*baliseIterator)->getPosition().track) == static_cast<const DS::Track*>(NULL))
          {
            delete(*baliseIterator);
            baliseIterator = baliseList.erase(baliseIterator);
          }
          else
          {
            //We can safely break here as the balise list is sorted and all balises after this should have
            //a track in track list.
            break;
          }
        }
      }

      return success;
    }

    /******************************************************************************
    *  Set travel direction of stored tracks
    ******************************************************************************/
    void AbstractTracks::setTravelDirection(const TravelDir travelDir)
    {
      this->travelDirection = travelDir;
    }

    /******************************************************************************
    *  Clear the possession balise list
    ******************************************************************************/
    void AbstractTracks::clearPossessionBaliseList()
    {
      //reset the possession balise list
      memset(&possessionBaliseList[0], 0, (sizeof(possessionBaliseList)));
      possessionListIndex = 0;
    }

    /******************************************************************************
    *  Check if the balise present in possession balise list
    ******************************************************************************/
    bool AbstractTracks::isBaliseInPossessionList(const uint16_t id) const
    {
      int8_t i = 0;
      bool retFlag = false;
      int8_t maxElement = possessionListIndex - 1;
      while ((i <= maxElement) &&
        (!retFlag))
      {
        const int8_t mid = i + ((maxElement - i) / 2);
        //if id is present at mid
        if (id == possessionBaliseList[mid])
        {
          retFlag = true;
        }
        //if ID is greater, ignore left half
        else if (id > possessionBaliseList[mid])
        {
          i = mid + 1;
        }
        else
        {
          //if ID is smaller, ignore right half
          maxElement = mid - 1;
        }
      }
      return retFlag;
    }

    /******************************************************************************
    *  Add the balise to possession balise list
    ******************************************************************************/
    bool AbstractTracks::addPossessionBalise(const uint16_t id)
    {
      bool retFlag = false;
      //Possession balise list is full?
      if (possessionListIndex < maxPossessionBaliseCount)
      {
        possessionBaliseList[possessionListIndex] = id;
        ++possessionListIndex;
        retFlag = true;
      }
      else
      {
        //Raise safety halt in case  list is full
        ATC::AbstractEventHandler::corePtr()->reportEvent(fullBaliseListFailure, __FILE__,
          __LINE__);
      }
      return retFlag;
    }
    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTracks::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidPosInTrack));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidIncomingTrack));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidIncomingTrackDir));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidUpdateStartPoint));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&fullTrackListFailure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&fullBaliseListFailure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&removeExistingBaliseFailure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&initNotDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&outOfMemoryFailure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unDefLimitFailure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&inconsistencyBaliseTrack));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&modifyExistingBalise));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&modifyExistingTrack));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt8(&possessionListIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));

      // For the Tracks list and balise list, handled in writeCrossCompare()
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <AbstractTracks>(this));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&travelDirection));
      //write the possession balise list
      for (int8_t i = 0; i < maxPossessionBaliseCount; ++i)
      {
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&possessionBaliseList[i]));
      }

    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractTracks::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;

      if (argc >= 1U)
      {
        char_t  buffer[512];

        // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
        if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
        {
          const char_t* const toWrite =
            "tracks        To list all tracks in track list\n"
            "balises       To list all balises in balise list\n"
            "posBal        To list all the possession balises in balise list";

          ATC::AbstractConsole::corePtr()->writeWithNewline(toWrite);
          retVal = false;
        }
        else if (ATC::isTextMatch(&argv[0][0], "tracks", sizeof("tracks")) && (argc == 1U))
        {
          if (!trackList.empty())
          {
            int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-10s%-10s%-10s%-10s%-10s%-10s",
              "TrackId", "LengthCm", "TrvlDir", "OdoDir", "OdoAt0", "inTid");

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }

            for (TrackListIteratorType trackLocalItr = trackList.begin(); trackLocalItr != trackList.end(); ++trackLocalItr)
            {
              Track* trk = (*trackLocalItr);
              ret = snprintf(&buffer[0], sizeof(buffer), "%-10u%-10u%-10u%-10u%-10d%-10u",
                trk->getTrackId(), trk->getTrackLength(), trk->getTravelDirection(),
                trk->getOdoDirection(), trk->getOdoAt0Cm(), trk->getIncomingTrackId());

              if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }
          }
          else
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("No Tracks in Track List");
          }
          retVal = true;
        }
        else if (ATC::isTextMatch(&argv[0][0], "balises", sizeof("balises")) && (argc == 1U))
        {
          if (!baliseList.empty())
          {

            int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-10s%-10s%-10s%-10s", "BaliseId", "TrackId", "Pos", "Odo");

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }


            for (BaliseListIteratorType baliseLocalItr = baliseList.begin(); baliseLocalItr != baliseList.end(); ++baliseLocalItr)
            {
              Balise* bal = (*baliseLocalItr);
              ret = snprintf(&buffer[0], sizeof(buffer), "%-10u%-10u%-10u%-10d",
                bal->getBaliseId(), bal->getPosition().track, bal->getPosition().position, bal->getOdoPosition());

              if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }
          }
          else
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("No Balise in Balise List");
          }
          retVal = true;
        }
        else if (ATC::isTextMatch(&argv[0][0], "posBal", sizeof("posBal")) && (argc == 1U))
        {
          if (0 != possessionListIndex)
          {
            int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-10s", "BaliseId");

            if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
            {
              ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
            }
            for (int8_t i = 0; i < possessionListIndex; ++i)
            {
              ret = snprintf(&buffer[0], sizeof(buffer), "%-10d", possessionBaliseList[i]);

              if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
              {
                ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
              }
            }
          }
          else
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline("No Balise in Possession Balise List");
          }
          retVal = true;
        }
        else
        {
          //Do Nothing
        }
      }

      return retVal;
    }

    /******************************************************************************
    * flipTrackOdoDir
    ******************************************************************************/
    void AbstractTracks::flipTrackOdoDir()
    {
      uint16_t prevId = 0U;

      for(TrackListType::reverse_iterator itr = trackList.rbegin(); itr != trackList.rend(); ++itr)
      {
        Track* trk = (*itr);
        trk->flipTrackOdoDir(prevId);
        prevId = trk->getTrackId();
      }

      trackList.reverse();

      for (BaliseListIteratorType baliseLocalItr = baliseList.begin(); baliseLocalItr != baliseList.end(); ++baliseLocalItr)
      {
        Balise* bal = (*baliseLocalItr);
        bal->recalcOdoPosition();
      }

      baliseList.reverse();
    }
  }
}

//lint +esym(586,snprintf)
