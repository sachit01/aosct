#ifndef Track_hpp
#define Track_hpp
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
* 2016-05-13    arastogi    Created
* 2016-07-23    spandita    updated the fucntion declaration
* 2016-07-31    spandita    created and updated the function for odo at limit
* 2016-08-04    spandita    Added function declaration for getting the previous track
                            limit
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "abstract_event_handler.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DS
  {
    /**
    * The class Track defines the Track data.
    *
    */
    class Track
    {
    public:

      /** Constructor (explicit)
      *
      * @param[in] trackId   Track Id
      * @param[in] dir       Travel direction
      * @param[in] pId       Track Id of previous Track
      * @param[in] lCm       Length of track
      * @param[in] odoDirect Orientation in track
      */
      Track(const uint16_t trackId,
        const TravelDir dir,
        const uint16_t pId,
        const uint32_t lCm,
        const OdoDir odoDirect);

      /** Default Constructor
      */
      Track();

      /** Memory allocator
      *
      * Virtual memory allocator to allocate memory from memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void* operator new(size_t const sz);

      /** Memory allocator (replacement)
      *
      * Virtual memory allocator to allocate memory from memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void* operator new(size_t const sz, void* const ptr);

      /** Memory de-allocator
      *
      * Virtual memory de-allocator to de-allocate memory to memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void operator delete(void* const ptr);

      /** Assignment Operator Overloading
      *
      *@param[in] track Reference object
      */

      Track& operator= (const Track &track);

      /** Destructor
      *
      */
      ~Track();

      /** Initialize/allocate memory pool for dynamic memory substitution
      *
      * @param[in] items Number of items to allocate memory for
      * @return true if memory allocation succeeded
      */
      static bool initMemPoolSize(const uint32_t items);

      /** Available memory pool
      *
      * @return number of available and empty slots in allocated memory pool
      */
      uint32_t availableMemPool(void);

      /*************************/
      /* Data access functions */
      /*************************/

      /** Get Track Id
      *
      * @return Return tId
      */
      uint16_t getTrackId() const;

      /** Get Track Length
      *
      * @return Return tLength
      */
      uint32_t getTrackLength() const;

      /** Get travel direction
      *
      * @return Return travelDirection
      */
      TravelDir getTravelDirection() const;

      /** Get odometer direction
      *
      * @return Return odoDirection
      */
      OdoDir getOdoDirection() const;

      /** Get previous track id
      *
      * @return Return incomingTrackId
      */
      uint16_t getIncomingTrackId() const;

      /** Get odometer value at 0cm track location
      *
      * @return Return odoAt0Cm
      */
      OdoPosition getOdoAt0Cm() const;

      /** Get odometer value at next track limit
      *
      * @return Return end limit odo value
      */
      OdoPosition getOdoAtNexTrackLimit() const;

      /** Get odometer value at at previous track limit
      *
      * @return Return  start limit odo value
      */
      OdoPosition getOdoAtPrevTrackLimit() const;

      /** Check if location is in this track
      *
      * @param[in] tAP  the location to check
      *
      * @return Return  true if the location is in track else false
      */
      bool isTandPInTrack(const TrackAndPos &tAP) const;

      /** Check if odometer value is in this track
      *
      * @param[in] odoPos  the odometer value to check
      *
      * @return Return     true if the the odometer reading is in track else false
      */
      bool isOdoPosInTrack(const OdoPosition odoPos) const;

      /** Operator to compare track obj
      *
      * @param[in] other  The track to compare to
      *
      * @return Return     true if the the tracks are same false otherwise
      */
      bool operator==(const Track& other) const;

      /**
      * Add all attributes to cross compare
      *
      * @param[in] buffer        The cross compare buffer to get store the attributes in
      */
      void writeCrossCompare(VFW_Buffer* const buffer) const;

      /**
      * The maximum size for the cross compare data
      *
      * @return Returns the maximum data size for this item
      */
      uint32_t getWriteCrossCompareMaxSize() const;
      
      /**
      * Flip the train orientation in the track.
      * 
      * Recalculate the odo at 0 cm and update incoming track.
      * @param[in] inTrack  The incoming track.
      */
      void flipTrackOdoDir(const uint16_t inTrack);

    protected:

    private:

      /**
      * Calculate Odo at 0cm of the current track
      *
      * @param[in]  prevTrack        Previous track ID
      * @param[in]  trvlDir          Travel direction of track
      * @param[in]  odoDir           Orientation in track
      * @param[in]  length           Length of track
      * @param[in]  trackId          Current Track ID
      *
      * @return   Returns the ODO position of track
      */
      OdoPosition calculateTrackOdo(
        const uint16_t prevTrack,
        const TravelDir trvlDir,
        const OdoDir odoDir,
        const uint32_t length,
        const uint16_t trackId) const;

      /** Track ID */
      uint16_t tId;

      /** Track Length in Cm */
      uint32_t tLength;

      /** Travel Direction */
      TravelDir travelDirection;

      /** Odometer Direction
      *
      * Tells if the odometer reading is incrementing or decrementing
      *
      */
      OdoDir odoDirection;

      /** Previous Track id */
      uint16_t incomingTrackId;

      /** Calculated Odometer reading at leg 0 */
      OdoPosition odoAt0Cm;

      /**
      * Internal error
      */
      const ATC::Event trackListInternalError;
    };
  }
}
#endif
