/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2015
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  A memory-pool for fixed size objects with size of each block >= 4 and <= 65535 bytes.            
*  new is used by createPool() to allocate memory for the entire pool at init 
*  delete is used by ~FixedSizeMemoryPool() to release the memory (if necessary).
*
*  Based on "Fast Efficient Fixed-Size Memory Pool", Ben Kenwright, Newcastle University
*  Published in COMPUTATION TOOLS 2012
*              
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-12-04    bhermans     Created
* 2016-03-16    lantback    Added namespace ATC
* 2017-01-26    rquensel    Made it use 4 byte aligned memory blocks
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_util.hpp"
#include "fixed_size_pool_manager.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  template <class Type>
  Type checkPointer(Type const pointer)
  {
    if (pointer == NULL)
    {
      ATC::aosHalt(__FILE__, __LINE__, "NULL pointer dereferenced");
    }
    return pointer;
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/

namespace ATC
{
  namespace AOSMem
  {

    /**
    * Memory pool constructor.
    * 
    * Init internal variables.
    * @remarks createPool() must also be called once before the memory pool can be used
    */
    FixedSizeMemoryPool::FixedSizeMemoryPool():
      numOfBlocks(0U),
      sizeOfEachBlock(0U),
      numFreeBlocks(0U),
      numInitialized(0U),
      memStart(static_cast<uint8_t*>(NULL)),
      nextFreeBlock(static_cast<uint8_t*>(NULL)),
      createdOk(false)
    {
    }

    /**
    * Memory pool destructor.
    * 
    * Frees the memory allocated by createPool()
    */
    FixedSizeMemoryPool::~FixedSizeMemoryPool() 
    { 
      delete[] memStart;
      nextFreeBlock = static_cast<uint8_t*>(NULL); // no effect, other than to please lint
    }

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
    bool FixedSizeMemoryPool::createPool(const size_t sizeOfEachBlockInPool, const uint16_t numOfBlocksInPool)
    {
      if ((sizeOfEachBlockInPool >= 4U) && (sizeOfEachBlockInPool <= 0xFFFFU))
      {
        numOfBlocks     = numOfBlocksInPool;
        sizeOfEachBlock = sizeOfEachBlockInPool;
        memStart        = new uint8_t[poolAlignedBlockSize() * numOfBlocks ];
        if (memStart == NULL)
        {   // Memory allocation failed
          createdOk = false;
        }
        else
        {
          numFreeBlocks   = numOfBlocks;
          nextFreeBlock   = memStart;
          createdOk = true;  
        }
      }
      else
      {
        createdOk = false;
      }
      return createdOk;
    }

    /**
    * Calculates the address of a memory-block at an index
    *
    * @param[in]   blockIndex    Index to the block
    *
    * @return      returns the address as a pointer to a byte.
    */
    uint8_t* FixedSizeMemoryPool::addrFromIndex(const uint16_t blockIndex)
    {
      return &checkPointer(memStart)[blockIndex * poolAlignedBlockSize()];
    }

    /**
    * Calculates the index of a memory-block at an address
    * 
    * @param[in]   pBlock        Pointer to the block
    *
    * @return      returns the index of the memory block.
    */
    uint16_t FixedSizeMemoryPool::indexFromAddr(const uint8_t* const pBlock) const
    {
      //lint -e{946} -e{947} Pointer arithmetic is necessary here
      //lint -e{1960} Cast changes signedness - but we have to change sign here
      return static_cast<uint16_t>(((static_cast<uint32_t>(pBlock - checkPointer(memStart))) / poolAlignedBlockSize()));
    }
    /**
    * Allocates a fixed-size memory-block
    * 
    * @return      returns a pointer to the allocated block
    */
    void* FixedSizeMemoryPool::allocateBlock()
    { 
      if (numInitialized < numOfBlocks )
      {
        //lint -e{826} -e{927} Cast is unavoidable here
        uint16_t* p = reinterpret_cast<uint16_t*>(addrFromIndex( numInitialized ));
        *p = numInitialized + 1U;
        numInitialized++;
      }
      void* ret = static_cast<void*>(NULL);
      if ( numFreeBlocks > 0U )
      {
        //lint -e{925} Cast is unavoidable here
        ret = static_cast<void*>(nextFreeBlock);
        --numFreeBlocks;
        if (numFreeBlocks != 0U)
        {
          //lint -e{826} -e{927} Cast is unavoidable here
          nextFreeBlock = addrFromIndex( *(reinterpret_cast<uint16_t*>(nextFreeBlock)) );
        }
        else
        {
          nextFreeBlock = static_cast<uint8_t*>(NULL);
        }
      }
      return ret;
    }
    /**
    * Deallocates a fixed-size memory-block
    * 
    */
    void FixedSizeMemoryPool::deAllocateBlock(void* const pBlock)
    {
      //lint --e{925} Cast is unavoidable here

      if (nextFreeBlock != NULL)
      {
        *(reinterpret_cast<uint16_t*>(pBlock)) = indexFromAddr( nextFreeBlock );
        nextFreeBlock = static_cast<uint8_t*>(pBlock);
      }
      else
      {
        *(reinterpret_cast<uint16_t*>(pBlock)) = numOfBlocks;
        nextFreeBlock = static_cast<uint8_t*>(pBlock);
      }
      ++numFreeBlocks;
    }
    /**
    * Returns the size of a memory-block managed by the pool
    * 
    * @return      returns the size of any block managed by the pool
    *
    * @remarks     returns 0 if the memory pool is not yet created by createPool()
    */
    size_t FixedSizeMemoryPool::poolBlockSize() const
    {
      return sizeOfEachBlock;
    }

    /**
    * Returns the aligned size of a memory-block managed by the pool
    *
    * @return      returns the aligned size of any block managed by the pool
    *
    * @remarks     returns 0 if the memory pool is not yet created by createPool()
    */
    size_t FixedSizeMemoryPool::poolAlignedBlockSize() const
    {
      const size_t alignedSizeOfEachBlock = sizeOfEachBlock + (sizeOfEachBlock % 4U);
      return alignedSizeOfEachBlock;
    }

    /**
    * Pool is created? 
    * 
    * @return      returns the flag indicating if the memory pool has been successfully created.
    *
    */
    bool FixedSizeMemoryPool::poolCreatedOk() const
    {
      return createdOk;
    }

    /**
    * Get the number of free blocks in memory-pool.
    *
    * @return number of free blocks
    */
    uint16_t FixedSizeMemoryPool::getNumberOfFreeBlocks() const
    {
      return numFreeBlocks;
    }
  }
}
