#ifndef TrackDataItemTargetBHP_hpp
#define TrackDataItemTargetBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for TrackDataItemTargetBHP class to manage all adaptation (BHP specific) actions for 
*  Track Data Item class
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-04-10    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "track_data_item_target.hpp"


/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * The class TrackDataItemTargetBHP defines and implements the BHP specific functionality related to target data item target storage
    *
    */
    class TrackDataItemTargetBHP : public TrackDataItemTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tType   Target track data type
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] tOdo    Odometer position of target
      * @param[in] nValue    Optional parameter
      */
      TrackDataItemTargetBHP(const uint8_t tType,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition tOdo, const uint16_t nValue = 0U);

      /**
      * Initialize/allocate memory pool for dynamic memory substitution
      *
      * @param[in] items Number of items to allocate memory for
      * @return true if memory allocation succeed
      */
      static bool initMemPoolSizeBHP(const uint32_t items);

      /**
      * Memory allocator to allocate memory from memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      //lint -e{1511} The 'new' operator must be static and therefore cannot be declared virtual
      static void* operator new(size_t const sz);

      /**
      * Memory de-allocator to de-allocate memory to memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      //lint -e{1511} The 'delete' operator must be static and therefore cannot be declared virtual
      static void operator delete(void* const ptr);

      /**
      * Virtual destructor to deallocate assigned memory
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      virtual ~TrackDataItemTargetBHP();

      /** To handle the passed target
      *
      * @return true if target can be removed
      */
      virtual bool isPassedTargetHandled() const;

    private:

      /** Calculate Speed while approach level crossing
      *
      * @return approach speed for level crossing
      */
      const uint16_t calcApproachSpeed() const;

      /**
      * Constructor (default)
      */
      TrackDataItemTargetBHP();

    };
  }
}

#endif
