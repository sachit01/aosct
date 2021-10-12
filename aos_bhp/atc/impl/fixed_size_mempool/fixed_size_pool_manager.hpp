#ifndef FixedSizePoolManager_hpp
#define FixedSizePoolManager_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2015
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  A memory-pool for fixed size objects with size of each block <= 65535 bytes.            
*  new is used by createPool() to allocate memory for the entire pool at init 
*  delete is used by ~FixedSizeMemoryPool() to release the memory (if necessary).
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-12-04    bhermans    Created
* 2016-03-16    lantback    Added namespace ATC
* 2017-01-26    rquensel    Made it use 4 byte aligned memory blocks
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  namespace AOSMem
  {
    class FixedSizeMemoryPool
    { 

    public:           

      /**
      * Memory pool constructor.
      * 
      * Init internal variables.
      * @remarks createPool() must also be called once before the memory pool can be used
      */
      FixedSizeMemoryPool();    

      /**
      * Memory pool destructor.
      * 
      * Frees the memory allocated by createPool()
      */
      ~FixedSizeMemoryPool();

      /**
      * Memory pool creation
      * 
      * Calculates and allocates the memory needed for the memory pool.
      *
      * @param[in]  sizeOfEachBlockInPool  The fixed size of each block
      * @param[in]  numOfBlocksInPool      The number of fixed size blocks managed by the pool
      * 
      * @return      true if create successful
      *              false if illegal sizeOfEachBlockInPool (must be >=4 AND <= 0xFFFF)
      *
      * @remarks Shall only be called at init. Calls new to allocate the memory.
      */
      bool createPool(const size_t sizeOfEachBlockInPool, const uint16_t numOfBlocksInPool);

      /**
      * Calculates the address of a memory-block at an index
      *
      * @param[in]   blockIndex    Index to the block
      *
      * @return      returns the address as a pointer to a byte.
      */
      uint8_t* addrFromIndex(const uint16_t blockIndex);

      /**
      * Calculates the index of a memory-block at an address
      * 
      * @param[in]   pBlock        Pointer to the block
      *
      * @return      returns the index of the memory block.
      */
      uint16_t indexFromAddr(const uint8_t* const pBlock) const;

      /**
      * Allocates a fixed-size memory-block
      * 
      * @return      returns a pointer to the allocated block
      */

      void* allocateBlock();
      /**
      * Deallocates a fixed-size memory-block
      * 
      * @param[in]   pBlock        Pointer to the block to deallocate
      */
      void deAllocateBlock(void* const pBlock);

      /**
      * Returns the size of a memory-block managed by the pool
      * 
      * @return      returns the size of any block managed by the pool
      *
      * @remarks     returns 0 if the memory pool is not yet created by createPool()
      */
      size_t poolBlockSize() const;


      /**
      * Returns the aligned size of a memory-block managed by the pool
      *
      * @return      returns the aligned size of any block managed by the pool
      *
      * @remarks     returns 0 if the memory pool is not yet created by createPool()
      */
      size_t poolAlignedBlockSize() const;


      /**
      * Pool is created? 
      * 
      * @return      returns the flag indicating if the memory pool has been successfully created.
      *
      */
      bool poolCreatedOk() const;

      /**
      * Get the number of free blocks in memory-pool.
      *
      * @return number of free blocks
      */
      uint16_t getNumberOfFreeBlocks() const;

    private:

      uint16_t numOfBlocks;     // Num of blocks
      size_t   sizeOfEachBlock; // Size of each block
      uint16_t numFreeBlocks;   // Num of remaining blocks
      uint16_t numInitialized;  // Num of initialized blocks
      uint8_t* memStart;        // Beginning of memory pool
      uint8_t* nextFreeBlock;   // Next free block
      bool     createdOk;       // Pool created Ok 

    };
  }
}
#endif
