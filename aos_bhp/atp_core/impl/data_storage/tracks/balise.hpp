#ifndef Balise_hpp
#define Balise_hpp
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
* 2016-05-13    arastogi      Created
* 2016-07-21    spandita      Updated the declarations 
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "fixed_size_pool_manager.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DS
  {
    /**
    * The class Balise defines the Balise data
    *
    */
    class Balise
    {
    public:

      /** Constructor (explicit)
      *
      */
      Balise(const uint16_t bId,
        const uint16_t trackId,
        const uint32_t pos);

      /** Default Constructor 
      *
      */
      Balise();

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

      /** Destructor
      *
      */
      ~Balise();

      /** Initialize/allocate memory pool for dynamic memory substitution
      *
      * @param[in] items Number of items to allocate memory for
      * @return true if memory allocation succeeded
      */
      static bool initMemPoolSize(const uint32_t items);

      /** Assignment Operator Overaloading
      *
      * @param[in] balise reference object
      * @return this balise object
      */

      Balise& operator=(const Balise &balise);

      /** Comparison Operator Overloading
      *
      * @param[in] other reference object
      * @return true if same, false otherwise
      */
      bool operator==(const Balise &other) const;

      /*************************/
      /* Data access functions */
      /*************************/

      /** Get Balise Id
      *
      * @return Return baliseId
      */
      uint16_t getBaliseId() const;

      /** Get odometer value at balise
      *
      * @return Return odoPosition
      */
      OdoPosition getOdoPosition() const;

      /** Get track and position of balise
      *
      * @return Return position
      */
      const TrackAndPos& getPosition() const;

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
      * Recalculate the odo value of the balise.
      */
      void recalcOdoPosition();

    protected:
      
    private:

      /** Balise id */
      uint16_t baliseId;

      /** Location of the balise */
      TrackAndPos position;

      /** Calculated odometer reading at the balise */
      OdoPosition odoPosition;

      /** Memory pool for virtual dynamic memory */
      static ATC::AOSMem::FixedSizeMemoryPool baliseMemPool;

    };
  }
}
#endif
