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
* 2016-07-21    spandita    Updated and Created the functions
* 2016-07-31    spandita    Updated  functions with lint fix
********************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "balise.hpp"
#include "abstract_tracks.hpp"
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

    /*Initialize the static class member*/
    ATC::AOSMem::FixedSizeMemoryPool Balise::baliseMemPool = ATC::AOSMem::FixedSizeMemoryPool();

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    Balise::Balise(const uint16_t bId,
      const uint16_t trackId,
      const uint32_t pos)
    {
      baliseId = bId;
      position.track = trackId;
      position.position = pos;

      odoPosition = 0;
      static_cast<void>(AbstractTracks::corePtr()->getOdoPos(position, odoPosition));
    }

    /******************************************************************************
    * Default Constructor
    ******************************************************************************/
    Balise::Balise()
    {
      baliseId = 0U;
      position.track = 0U;
      position.position = 0U;
      odoPosition = 0;
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    void* Balise::operator new(size_t const sz)
    {
      void *ret;
      if(sz == baliseMemPool.poolBlockSize())
      {
        ret = baliseMemPool.allocateBlock();
      }
      else
      {
        ret = static_cast<void *>(NULL);
      }
      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    void Balise::operator delete(void* const ptr)
    {
      baliseMemPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool Balise::initMemPoolSize(const uint32_t items)
    {
      bool memFlag = false;

      if(!baliseMemPool.poolCreatedOk())
      {
        if(baliseMemPool.createPool(sizeof(Balise),static_cast<uint16_t>(items)))
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
    * getBaliseId
    ******************************************************************************/
    uint16_t Balise::getBaliseId() const
    {
      return baliseId;
    }

    /******************************************************************************
    * getPosition
    ******************************************************************************/
    const TrackAndPos& Balise::getPosition() const
    {
      return position;
    }

    /******************************************************************************
    * writeCrossCompare
    ******************************************************************************/
    void Balise::writeCrossCompare(VFW_Buffer * const buffer) const
    {
      vfwPutU16(buffer, baliseId);
      vfwPutU16(buffer, position.track);
      vfwPutU32(buffer, position.position);
      vfwPutI32(buffer, odoPosition);
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSize
    ******************************************************************************/
    uint32_t Balise::getWriteCrossCompareMaxSize() const
    {
      // For the sizes, see writeCrossCompare above
      return 2U + 2U + 4U + 4U;
    }

    /******************************************************************************
    * getOdoPosition
    ******************************************************************************/
    OdoPosition Balise::getOdoPosition()const
    {
      return odoPosition;
    }

    /******************************************************************************
    * Assignment Operator Overloading
    ******************************************************************************/
    Balise& Balise::operator= (const Balise &balise)
    {
      if(this == &balise)
      {
      }
      else
      {
        this-> baliseId = balise.baliseId;
        this-> position.track =balise.position.track;
        this->position.position = balise.position.position;
        this->odoPosition =balise.odoPosition;
      }

      return *this;
    }

    /******************************************************************************
    * Comparison Operator Overloading
    ******************************************************************************/
    bool Balise::operator== (const Balise &other) const
    {
      bool retVal = false;
      if((baliseId == other.baliseId) && (position.track == other.position.track)
          && (position.position == other.position.position))
      {
        retVal = true;
      }

      return retVal;
    }

    /******************************************************************************
    * Des'tor
    ******************************************************************************/
    Balise::~Balise()
    {
      //nothing
    }

    /******************************************************************************
    * recalcOdoPosition
    ******************************************************************************/
    void Balise::recalcOdoPosition()
    {
      static_cast<void>(AbstractTracks::corePtr()->getOdoPos(position, odoPosition));
    }
  }
}
