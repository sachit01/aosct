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
* 2016-05-13    arastogi    Created
* 2016-07-18    spandita    updated function as per the design
* 2016-07-31    spandita    created and updated the function for odo at limit
* 2016-08-04    spandita    Added function declaration for getting the previous track
                            limit
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "track.hpp"
#include "abstract_position.hpp"
#include "fixed_size_pool_manager.hpp"
#include "atc_math.hpp"
#include "abstract_tracks.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_tracks_event_ids.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  /*Initialize the static class member*/
  ATC::AOSMem::FixedSizeMemoryPool trackMemPool = ATC::AOSMem::FixedSizeMemoryPool();
}

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
    Track::Track(const uint16_t trackId,
      const TravelDir dir,
      const uint16_t pId,
      const uint32_t lCm,
      const OdoDir odoDirect) :
      trackListInternalError(ATC::Event::createSafetyHaltEvent(atpTracksId, ATC::CoreContainer, eventIdTrackListInternalError,
        ATC::NoEB, DMICom::tracksInternalFailure, "Unable to find any track in storage"))
    {
      tId = trackId;
      travelDirection = dir;
      incomingTrackId = pId;
      tLength = lCm;
      odoDirection = odoDirect;

      /*Calculate odometer value a 0cm */
      odoAt0Cm = calculateTrackOdo(pId, dir, odoDirect, lCm, trackId);
    }
    /******************************************************************************
    * Default Constructor
    ******************************************************************************/
    Track::Track()
    {
      tId = 0U;
      travelDirection = DirUndefined;
      incomingTrackId = 0U;
      tLength = 0U;
      odoDirection = OdoUndefined;
      odoAt0Cm = 0;

    }
    /******************************************************************************
    * new
    ******************************************************************************/
    void* Track::operator new(size_t const sz)
    {
      void *ret;
      if (sz == trackMemPool.poolBlockSize())
      {
        ret = trackMemPool.allocateBlock();
      }
      else
      {
        ret = static_cast<void*>(NULL);
      }
      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    void Track::operator delete(void* const ptr)
    {
      trackMemPool.deAllocateBlock(ptr);

    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool Track::initMemPoolSize(const uint32_t items)
    {
      bool memFlag = false;

      if (!trackMemPool.poolCreatedOk())
      {
        if (trackMemPool.createPool(sizeof(Track), static_cast<uint16_t>(items)))
        {
          memFlag = true;
        }
        else
        {
          //Do nothing
        }
      }
      else
      {
        memFlag = true;
      }
      return memFlag;
    }

    /******************************************************************************
    * getTrackId
    ******************************************************************************/
    uint16_t Track::getTrackId() const
    {
      return tId;
    }

    /******************************************************************************
    * getTrackLength
    ******************************************************************************/
    uint32_t Track::getTrackLength() const
    {
      return tLength;
    }

    /******************************************************************************
    * getTravelDirection
    ******************************************************************************/
    TravelDir Track::getTravelDirection() const
    {
      return travelDirection;
    }

    /******************************************************************************
    * getOdoDirection
    ******************************************************************************/
    OdoDir Track::getOdoDirection() const
    {
      return odoDirection;
    }

    /******************************************************************************
    * getIncomingTrackId
    ******************************************************************************/
    uint16_t Track::getIncomingTrackId() const
    {
      return incomingTrackId;
    }

    /******************************************************************************
    * getOdoAt0Cm
    ******************************************************************************/
    OdoPosition Track::getOdoAt0Cm() const
    {
      return odoAt0Cm;
    }

    /******************************************************************************
    * Assignment Operator Overloading
    ******************************************************************************/

    Track& Track::operator= (const Track &track)
    {
      if (this == &track)
      {
      }
      else
      {
        tId = track.tId;
        travelDirection = track.travelDirection;
        tLength = track.tLength;
        odoDirection = track.odoDirection;
        incomingTrackId = track.incomingTrackId;
        odoAt0Cm = track.odoAt0Cm;
      }

      return *this;
    }


    /******************************************************************************
    * isTandPInTrack
    ******************************************************************************/
    bool Track::isTandPInTrack(const TrackAndPos &tAP) const
    {
      bool isPosTrackOk;
      if ((tAP.track == tId) && (tAP.position <= tLength))
      {
        isPosTrackOk = true;
      }
      else
      {
        isPosTrackOk = false;
      }
      return isPosTrackOk;
    }

    /******************************************************************************
    * Des'tor
    ******************************************************************************/
    Track::~Track()
    {
    }

    /******************************************************************************
    * isOdoPosInTrack
    ******************************************************************************/
    bool Track::isOdoPosInTrack(const OdoPosition odoPos) const
    {
      bool isOdoPosTrackOk = false;
      OdoPosition odoPosatStart;
      OdoPosition odoPosatEnd;

      if (OdoPositive == getOdoDirection())
      {
        odoPosatStart = getOdoAt0Cm();
        odoPosatEnd = getOdoAt0Cm() + static_cast<int32_t>(getTrackLength());
        /**Check for Position Validity */
        if ((odoPosatStart <= odoPos) && (odoPosatEnd >= odoPos))
        {
          isOdoPosTrackOk = true;
        }
        else
        {
          isOdoPosTrackOk = false;
        }
      }
      else if (OdoNegative == getOdoDirection())
      {
        odoPosatStart = getOdoAt0Cm();
        odoPosatEnd = getOdoAt0Cm() - static_cast<int32_t>(getTrackLength());
        if ((odoPosatStart >= odoPos) && (odoPosatEnd <= odoPos))
        {
          isOdoPosTrackOk = true;
        }
        else
        {
          isOdoPosTrackOk = false;
        }
      }
      else
      {
        isOdoPosTrackOk = false;
      }
      return isOdoPosTrackOk;
    }


    /******************************************************************************
    * operator==
    ******************************************************************************/
    bool Track::operator==(const Track& other) const
    {
      bool retVal = false;
      if ((tId == other.tId) && (tLength == other.tLength)
        && (odoDirection == other.odoDirection) && (travelDirection == other.travelDirection))
      {
        retVal = true;
      }
      return retVal;
    }

    /******************************************************************************
    * writeCrossCompare
    ******************************************************************************/
    void Track::writeCrossCompare(VFW_Buffer * const buffer) const
    {
      vfwPutU16(buffer, tId);
      vfwPutU32(buffer, tLength);
      vfwPutU32(buffer, static_cast<uint32_t>(travelDirection));
      vfwPutU32(buffer, static_cast<uint32_t>(odoDirection));
      vfwPutU16(buffer, incomingTrackId);
      vfwPutU32(buffer, tLength);
      vfwPutI32(buffer, odoAt0Cm);
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSize
    ******************************************************************************/
    uint32_t Track::getWriteCrossCompareMaxSize() const
    {
      // For the sizes, see writeCrossCompare above...
      return 2U + 4U + 4U + 4U + 2U + 4U + 4U + 1U;
    }


    /******************************************************************************
    * Get the odo value at next track limit
    ******************************************************************************/
    OdoPosition Track::getOdoAtNexTrackLimit() const
    {
      OdoPosition calOdo = 0;
      if (DirForward == getTravelDirection())
      {
        if (OdoPositive == getOdoDirection())
        {
          calOdo = getOdoAt0Cm() + static_cast<int32_t>(getTrackLength());
        }
        else
        {
          calOdo = getOdoAt0Cm();
        }
      }
      else if (DirReverse == getTravelDirection())
      {
        if (OdoNegative == getOdoDirection())
        {
          calOdo = getOdoAt0Cm() - static_cast<int32_t>(getTrackLength());
        }
        else
        {
          calOdo = getOdoAt0Cm();
        }
      }
      else
      {
        //Should never come here
      }
      return calOdo;
    }

    /******************************************************************************
    * calculateTrackOdo
    ******************************************************************************/
    OdoPosition Track::calculateTrackOdo(
      const uint16_t prevTrack,
      const TravelDir trvlDir,
      const OdoDir odoDir,
      const uint32_t length,
      const uint16_t trackId) const
    {
      const bool isTrkListEmpty = AbstractTracks::corePtr()->isTrackListEmpty();
      OdoPosition odoVal = 0;

      //Check if track list is empty
      if (isTrkListEmpty)
      {
        //set the current odoAt0cm to current antenna ODO value.
        odoVal = Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo();
      }
      else
      {
        //if track is already in trackList
        const Track* const pExistingTrack = AbstractTracks::corePtr()->getTrack(trackId);
        //If same track and orientation and length are same
        // travel direction is not checked as it doesnot impact odo at 0 if already calculated.
        if ((static_cast<const Track*>(NULL) != pExistingTrack) &&
            (pExistingTrack->odoDirection == odoDir) &&
            (pExistingTrack->tLength == length))
        {
          odoVal = pExistingTrack->getOdoAt0Cm();
        }
        else
        {
          //MA extension case 
          if (0U != prevTrack)
          {
            //Get the last track
            const DS::Track* const pLastTrack = AbstractTracks::corePtr()->getLastTrack();
            //NULL check
            if (static_cast<const Track*>(NULL) != pLastTrack)
            {
              if (pLastTrack->getTrackId() == prevTrack)
              {
                //Get the last track Odo limit value
                odoVal = pLastTrack->getOdoAtNexTrackLimit();
              }
              else
              {
                bool isTrackFound = false;
                AbstractTracks::ConstTrackListIteratorType trackListPtr = DS::AbstractTracks::corePtr()->getTracksIter();
                AbstractTracks::ConstTrackListIteratorType endTrackPtr = DS::AbstractTracks::corePtr()->getTracksIterEnd();
                //iterate over the track list and find the track in track list
                for (; ((trackListPtr != endTrackPtr) && (!isTrackFound)); ++trackListPtr)
                {
                  if ((*trackListPtr)->getTrackId() == prevTrack)
                  {
                    isTrackFound = true;
                  }
                }

                if (isTrackFound)
                {
                  odoVal = (*trackListPtr)->getOdoAtNexTrackLimit();
                }
              }

              /**Calculation of Odoat0cm for current track */
              if (DirForward == trvlDir)
              {
                if (OdoPositive != odoDir)
                {
                  odoVal += static_cast<int32_t>(length);
                }
              }
              else if (DirReverse == trvlDir)
              {
                if (OdoNegative != odoDir)
                {
                  odoVal -= static_cast<int32_t>(length);
                }
              }
              else
              {
                //warning again --should never come here.
              }
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(trackListInternalError, __FILE__, __LINE__);
            }
          }
          else //if adding the track with incoming id as 0.
          {
            //set the current odoAt0cm to current antenna ODO value.
            odoVal = Pos::AbstractPosition::corePtr()->getCurrAntennaPosOdo();
          }

        }
      }
      return odoVal;
    }

    /******************************************************************************
    * Get the odo value at previous track limit
    ******************************************************************************/
    OdoPosition Track::getOdoAtPrevTrackLimit() const
    {
      OdoPosition calOdo = 0;
      if (DirForward == getTravelDirection())
      {
        if (OdoPositive == getOdoDirection())
        {
          calOdo = getOdoAt0Cm();
        }
        else
        {
          calOdo = getOdoAt0Cm() - static_cast<int32_t>(getTrackLength());
        }
      }
      else if (DirReverse == getTravelDirection())
      {
        if (OdoNegative == getOdoDirection())
        {
          calOdo = getOdoAt0Cm();
        }
        else
        {
          calOdo = getOdoAt0Cm() + static_cast<int32_t>(getTrackLength());
        }
      }
      else
      {
        //should never come here
      }
      return calOdo;
    }

    /******************************************************************************
    * flipTrackOdoDir
    ******************************************************************************/
    void Track::flipTrackOdoDir(const uint16_t inTrack)
    {
      incomingTrackId = inTrack;

      OdoDir newOdoDirection = odoDirection;

      if(odoDirection == OdoPositive)
      {
        newOdoDirection = OdoNegative;
      }
      else if(odoDirection == OdoNegative)
      {
        newOdoDirection = OdoPositive;
      }
      else
      {
        //Do nothing
      }

      /*Calculate odometer value a 0cm */
      odoAt0Cm = calculateTrackOdo(incomingTrackId, travelDirection , newOdoDirection, tLength, tId);

      //update the odo direction.
      odoDirection = newOdoDirection;
    }
  }
}
