/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the abstract (core) Cross Compare component class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-05    rquensel    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_config_base.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_event_handler.hpp"
#include <vfw_checkpoints.h>
#include <vfw_crc.h>
#include "dmi_event_codes.hpp"
#include "atc_util.hpp"
#include <stdio.h>
#include "abstract_atp_application.hpp"
#include "abstract_cross_compare_event_ids.hpp"
#include "atc_math.hpp"

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
namespace
{
  /******************************************************************************
  * Maxmimum CrossCompareBuffer size
  ******************************************************************************/
  const uint32_t COMPARE_BUFFER_SIZE = 1000U;

  /******************************************************************************
  * Instance pointer to the abstract Cross Compare object
  ******************************************************************************/
  ATP::Support::AbstractCrossCompare* corePtr_ = static_cast<ATP::Support::AbstractCrossCompare*>(NULL);
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Support
  {
    /******************************************************************************
    * AbstractCrossCompare constructor
    ******************************************************************************/
    AbstractCrossCompare::AbstractCrossCompare()
      :
      ATC::IOComponent(atpCrossCompareId, "CrossCompare", "CC"),
      crossCompareErrorOccured_(false),
      crossComparePointInput(static_cast<VFW_CrossCompare>(NULL)),
      crossComparePointOutput(static_cast<VFW_CrossCompare>(NULL)),
      crossComparePointVersion(static_cast<VFW_CrossCompare>(NULL)),
      nextObjectToCompare(static_cast<CrossCompareObject*>(NULL)),
      crossCompareObjectList(static_cast<CrossCompareObject*>(NULL)),
      crossCompareOutputList(static_cast<CrossCompareOutput*>(NULL)),
      inputBufferPos_(0U),
      // Events
      eventMismatchSafetyHalt_(ATC::Event::createSafetyHaltEvent(ATP::atpCrossCompareId,
        ATC::CoreContainer, eventIdMismatchSafetyHalt, ATC::NoEB, DMICom::crossCompMisMatchErr, "Cross compare error")),
      eventCouldNotWriteSafetyHalt_(ATC::Event::createSafetyHaltEvent(ATP::atpCrossCompareId,
        ATC::CoreContainer, eventIdCouldNotWriteSafetyHalt, ATC::NoEB, DMICom::crossCompDataWriteErr, "Cross compare error", true)),
      eventCouldNotAddIOSafetyHalt_(ATC::Event::createSafetyHaltEvent(ATP::atpCrossCompareId,
        ATC::CoreContainer, eventIdCouldNotAddIOSafetyHalt, ATC::NoEB, DMICom::crossCompBuffFull, "IO compare buffer full")),
      isVersionSentForCrossCompare_(false),
      initHasFinished(false)
    {
      if (corePtr_ != static_cast<AbstractCrossCompare*>(NULL))
      {
        ATC::aosHalt(__FILE__, __LINE__, "Cross compare constructor already instantiated");
      }

      corePtr_ = this;

      for (uint16_t i = 0U; i < inputOutputCrossCompareSize; ++i)
      {
        inputOutputCrossCompareBuffer_[i] = 0U;
      }
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractCrossCompare::preInit()
    {
      // Setup the Cross Compare functionality in VFW
      crossComparePointInput = vfwAllocateCrossCompare();
      crossComparePointOutput = vfwAllocateCrossCompare();
      crossComparePointVersion = vfwAllocateCrossCompare();

      if ((crossComparePointInput != static_cast<VFW_CrossCompare>(NULL)) &&
          (crossComparePointOutput != static_cast<VFW_CrossCompare>(NULL)) &&
          (crossComparePointVersion != static_cast<VFW_CrossCompare>(NULL)))
      {
        vfwCrossCompareRegisterFunctions(crossComparePointInput,
          &writeCrossCompareDataCallbackRunIn, &receiveCrossCompareDataCallbackIn, COMPARE_BUFFER_SIZE);
        vfwCrossCompareRegisterFunctions(crossComparePointOutput,
          &writeCrossCompareDataCallbackRunOut, &receiveCrossCompareDataCallbackOut, COMPARE_BUFFER_SIZE);
        vfwCrossCompareRegisterFunctions(crossComparePointVersion,
          &writeCrossCompareDataCallbackVersionCheck, &receiveCrossCompareVersionCheck, maxVersionLength);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Failed to allocate vfw cross compare point");
      }
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractCrossCompare::init(void)
    {
      return true;
    }

    /******************************************************************************
    * initDone
    ******************************************************************************/
    void AbstractCrossCompare::initDone()
    {
      initHasFinished = true;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractCrossCompare * AbstractCrossCompare::corePtr()
    {
      return corePtr_;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void AbstractCrossCompare::runIn()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "CC_runIn");

      if (!isVersionSentForCrossCompare_)
      {
        isVersionSentForCrossCompare_ = true;
        vfwCrossCompare(crossComparePointVersion);
      }

      vfwCrossCompare(crossComparePointInput);
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void AbstractCrossCompare::runOut()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "CC_runOut");

      vfwCrossCompare(crossComparePointOutput);

      CrossCompareOutput* ccOutput = crossCompareOutputList;
      while (ccOutput != static_cast<CrossCompareOutput*>(NULL))
      {
        if (!crossCompareErrorOccured_)
        {
          // No cross compare error, write the cross compared data to the VFW channel
          ccOutput->commit();
        }
        else
        {
          ccOutput->clearBuffers();
        }
        ccOutput = ccOutput->getNext();
      }
    }

    /******************************************************************************
    * writeCrossCompareDataCallbackRunIn
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataCallbackRunIn(VFW_Buffer* const ownBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_writeCrossCompareDataCallbackRunIn_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->writeCrossCompareDataRunIn(ownBuffer); 

      vfwVisitCheckPoint(&endCp, "CC_writeCrossCompareDataCallbackRunIn_end");
    }

    /******************************************************************************
    * writeCrossCompareDataCallbackRunOut
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataCallbackRunOut(VFW_Buffer* const ownBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_writeCrossCompareDataCallbackRunOut_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->writeCrossCompareDataRunOut(ownBuffer);

      vfwVisitCheckPoint(&endCp, "CC_writeCrossCompareDataCallbackRunOut_end");
    }

    /******************************************************************************
    * writeCrossCompareDataCallbackVersionCheck
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataCallbackVersionCheck(VFW_Buffer* const ownBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_writeCrossCompareDataCallbackVersionCheck_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->writeCrossCompareVersionCheck(ownBuffer);

      vfwVisitCheckPoint(&endCp, "CC_writeCrossCompareDataCallbackVersionCheck_end");
    }

    /******************************************************************************
    * receiveCrossCompareDataCallbackIn
    ******************************************************************************/
    void AbstractCrossCompare::receiveCrossCompareDataCallbackIn(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_receiveCrossCompareDataCallbackIn_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->compareCrossCompareDataIn(ownBuffer, otherBuffer);

      vfwVisitCheckPoint(&endCp, "CC_receiveCrossCompareDataCallbackIn_end");
    }

    /******************************************************************************
    * receiveCrossCompareDataCallbackOut
    ******************************************************************************/
    void AbstractCrossCompare::receiveCrossCompareDataCallbackOut(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_receiveCrossCompareDataCallbackOut_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->compareCrossCompareDataOut(ownBuffer, otherBuffer);

      vfwVisitCheckPoint(&endCp, "CC_receiveCrossCompareDataCallbackOut_end");
    }

    /******************************************************************************
    * receiveCrossCompareVersionCheck
    ******************************************************************************/
    void AbstractCrossCompare::receiveCrossCompareVersionCheck(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer)
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&beginCp, "CC_receiveCrossCompareVersionCheck_begin");

      ATP::Support::AbstractCrossCompare::corePtr()->compareCrossCompareVersionCheck(ownBuffer, otherBuffer);

      vfwVisitCheckPoint(&endCp, "CC_receiveCrossCompareVersionCheck_end");
    }

    /******************************************************************************
    * writeCrossCompareDataRunOut
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataRunOut(VFW_Buffer* const ownBuffer)
    {
      uint8_t outputCrossCompareBuffer[inputOutputCrossCompareSize];

      VFW_Buffer buffer;

      //Initialize a simMovementTelegram buffer for VFW_Buffer usage
      vfwInitBuffer(&buffer, &outputCrossCompareBuffer[0], inputOutputCrossCompareSize);

      writeCrossCompareDataOutput_(&buffer);
      writeCrossCompareObjects(&buffer);

      const uint32_t bytesCompared = vfwGetValidSize(&buffer);

      if (bytesCompared > 0U)
      {
        // Now create CRC in blocks of maxCrcDataSize for the data and send the CRC instead of actual data
        vfwCalcBlockedCrc64(&buffer, ownBuffer, maxCrcDataSize);
      }
    }

    /******************************************************************************
    * writeCrossCompareVersionCheck
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareVersionCheck(VFW_Buffer* const ownBuffer) const
    {
      vfwPutString (ownBuffer, ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());

      ATC::AbstractConfigBase::basePtr()->writeCrossCompareVersions(ownBuffer);
    }

    /******************************************************************************
    * addCrossCompareInputData
    ******************************************************************************/
    void AbstractCrossCompare::addCrossCompareInputData(const uint8_t * const buffer, const uint16_t bufferSize)
    {
      const uint16_t newBufferPos = inputBufferPos_ + bufferSize;
      if (newBufferPos <= sizeof(inputOutputCrossCompareBuffer_))
      {
        //lint -e{826} The buffer is large enough (checked above)
        memmove(&inputOutputCrossCompareBuffer_[inputBufferPos_], buffer, bufferSize);
        inputBufferPos_ = newBufferPos;
      }
      else
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventCouldNotAddIOSafetyHalt_, __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * compareCrossCompareDataIn
    ******************************************************************************/
    void AbstractCrossCompare::compareCrossCompareDataIn(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer)
    {
      const uint32_t ownSize = vfwGetValidSize(ownBuffer);
      const uint32_t otherSize = vfwGetValidSize(otherBuffer);

      if (ownSize != otherSize)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventMismatchSafetyHalt_, __FILE__, __LINE__);
        crossCompareErrorOccured_ = true;
      }
      else
      {
        // Same length, get the actual byte buffers ...
        const uint8_t* ownByteBuffer = vfwGetStart(ownBuffer);
        const uint8_t* otherByteBuffer = vfwGetStart(otherBuffer);

        // Compare the char buffers received.
        const int32_t cmpResult = memcmp(ownByteBuffer, otherByteBuffer, ownSize);

        if (cmpResult != 0)
        {
          crossCompareErrorOccured_ = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventMismatchSafetyHalt_, __FILE__, __LINE__);

        }
      }

      vfwClearBuffer(ownBuffer);
      vfwClearBuffer(otherBuffer);
    }


    /******************************************************************************
    * compareCrossCompareDataOut
    ******************************************************************************/
    void AbstractCrossCompare::compareCrossCompareDataOut(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer)
    {
      const uint32_t ownSize = vfwGetValidSize(ownBuffer);
      const uint32_t otherSize = vfwGetValidSize(otherBuffer);
      
      if (ownSize != otherSize)
      {
        ATC::AbstractEventHandler::corePtr()->reportEvent(eventMismatchSafetyHalt_, __FILE__, __LINE__);

        char_t bufferInfo[300];
        const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo),
          "Buffer out size mismatch, our val = %d, other = %d", ownSize, otherSize);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
        {
          trace.write(1U, &bufferInfo[0]);
        }
        crossCompareErrorOccured_ = true;
      }
      else
      {
        // Same length, get the actual byte buffers ...
        const uint8_t* ownByteBuffer = vfwGetStart(ownBuffer);
        const uint8_t* otherByteBuffer = vfwGetStart(otherBuffer);

        // Compare the char buffers received.
        const int32_t cmpResult = memcmp(ownByteBuffer, otherByteBuffer, ownSize);

        if (cmpResult != 0)
        {
          crossCompareErrorOccured_ = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventMismatchSafetyHalt_, __FILE__
            , __LINE__);
        }
      }

      vfwClearBuffer(ownBuffer);
      vfwClearBuffer(otherBuffer);
    }

    /******************************************************************************
    * compareCrossCompareVersionCheck
    ******************************************************************************/
    void AbstractCrossCompare::compareCrossCompareVersionCheck(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer) const
    {
      const uint32_t ownSize = vfwGetValidSize(ownBuffer);
      const uint32_t otherSize = vfwGetValidSize(otherBuffer);

      if (ownSize != otherSize)
      {
        char_t bufferInfo[300];
        const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo),
          "Version size mismatch, our val = %d, other = %d", ownSize, otherSize);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
        {
          ATC::aosHalt(__FILE__, __LINE__, &bufferInfo[0]);
        }
        else
        {
          ATC::aosHalt(__FILE__, __LINE__, "Version size mismatch");
        }
      }
      else
      {
        char_t otherVersionString[maxVersionLength];

        const uint32_t otherVersionStringLen = vfwGetString(otherBuffer, &otherVersionString[0], sizeof(otherVersionString));
        const int32_t versionCompareResult = strncmp(&otherVersionString[0],
          ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString(), otherVersionStringLen);

        if (versionCompareResult != 0)
        {
          char_t bufferInfo[300];
          const char_t* const applicationVersionString = ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString();
          const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo),
            "ATP application version mismatch, our val = %s, other = %s", applicationVersionString, &otherVersionString[0]);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
          {
            ATC::aosHalt(__FILE__, __LINE__, &bufferInfo[0]);
          }
          else
          {
            ATC::aosHalt(__FILE__, __LINE__, "ATP application version mismatch");
          }
        }

        if (!ATC::AbstractConfigBase::basePtr()->crossCompareVersions(otherBuffer))
        {
          ATC::aosHalt(__FILE__, __LINE__, "ATP config version mismatch");
        }
      }

      vfwClearBuffer(ownBuffer);
      vfwClearBuffer(otherBuffer);
    }

    /******************************************************************************
    * addCrossCompareData
    ******************************************************************************/
    void AbstractCrossCompare::addCrossCompareData(CrossCompareObject* const crossCompare)
    {
      if (initHasFinished)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Calling addCrossCompareData is only allowed during initialization");
      }

      crossCompare->setNext(crossCompareObjectList);
      crossCompareObjectList = crossCompare;
    }

    /******************************************************************************
    * addCrossCompareOutputData
    ******************************************************************************/
    void AbstractCrossCompare::addCrossCompareOutputData(CrossCompareOutput* const crossCompare)
    {
      if (initHasFinished)
      {
        ATC::aosHalt(__FILE__, __LINE__, "Calling addCrossCompareOutputData is only allowed during initialization");
      }

      crossCompare->setNext(crossCompareOutputList);
      crossCompareOutputList = crossCompare;
    }

    /******************************************************************************
    * AbstractCrossCompare destructor
    ******************************************************************************/
    AbstractCrossCompare::~AbstractCrossCompare()
    {
      crossComparePointInput = static_cast<VFW_CrossCompare>(NULL);
      crossComparePointOutput = static_cast<VFW_CrossCompare>(NULL);
      crossComparePointVersion = static_cast<VFW_CrossCompare>(NULL);
      nextObjectToCompare = static_cast<CrossCompareObject*>(NULL);
      crossCompareObjectList = static_cast<CrossCompareObject*>(NULL);
      crossCompareOutputList = static_cast<CrossCompareOutput*>(NULL);
    }

    /******************************************************************************
    * writeCrossCompareDataRunIn
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataRunIn(VFW_Buffer* const ownBuffer)
    {
      // Do we have any input data to write?
      if (inputBufferPos_ > 0U)
      {
        VFW_Buffer buffer;

        //Initialize a simMovementTelegram buffer for VFW_Buffer usage
        vfwInitBuffer(&buffer, &inputOutputCrossCompareBuffer_[0], inputBufferPos_);
        vfwSetReadBuffer(&buffer, inputBufferPos_);

        // Now create CRC in blocks of maxCrcDataSize for the data and send the CRC instead of actual data
        vfwCalcBlockedCrc64(&buffer, ownBuffer, maxCrcDataSize);

        inputBufferPos_ = 0U;
      }
    }

    /******************************************************************************
    * writeCrossCompareDataOutput_
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareDataOutput_(VFW_Buffer* const ownBuffer)
    {
      CrossCompareOutput* ccOutput = crossCompareOutputList;
      while (ccOutput != static_cast<CrossCompareOutput*>(NULL))
      {
        ccOutput->writeDataBinary(ownBuffer);
        ccOutput = ccOutput->getNext();
      }
    }

    /******************************************************************************
    * writeCrossCompareObjects
    ******************************************************************************/
    void AbstractCrossCompare::writeCrossCompareObjects(VFW_Buffer* const ownBuffer)
    {
      if (nextObjectToCompare == static_cast<CrossCompareObject*>(NULL))
      {
        nextObjectToCompare = crossCompareObjectList;
      }

      if (nextObjectToCompare != static_cast<CrossCompareObject*>(NULL))
      {
        const uint32_t bufferMaxSize = vfwGetFreeSpace(ownBuffer);
        const uint32_t maxDataSize = nextObjectToCompare->getMaxDataSize();

        if (bufferMaxSize < maxDataSize)
        {
          char_t bufferInfo[30];
          const int32_t res = snprintf(&bufferInfo[0], sizeof(bufferInfo),
            " %d, %d", bufferMaxSize, maxDataSize);

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(bufferInfo)))
          {
            eventCouldNotWriteSafetyHalt_.setDynamicText(&bufferInfo[0]);
          }
          ATC::AbstractEventHandler::corePtr()->reportEvent(eventCouldNotWriteSafetyHalt_, __FILE__
            , __LINE__);
        }

        bool done = false;
        uint8_t numComparedObjects = 0U;
        while (!done)
        {
          const uint32_t objectSize = nextObjectToCompare->getMaxDataSize();

          if (numComparedObjects >= maxObjectsToComparePerCycle)
          {
            done = true;
          }
          else if (vfwGetFreeSpace(ownBuffer) >= objectSize)
          {
            const uint32_t offset1 = vfwGetValidSize(ownBuffer);
            nextObjectToCompare->writeDataBinary(ownBuffer);
            const uint32_t offset2 = vfwGetValidSize(ownBuffer);

            if ((offset2 - offset1) > objectSize)
            {
              ATC::aosHalt(__FILE__, __LINE__, "CrossCompareObject::getMaxDataSize() returned the wrong size");
            }

            nextObjectToCompare = nextObjectToCompare->getNext();
            done = (nextObjectToCompare == static_cast<CrossCompareObject*>(NULL));
            ++numComparedObjects;
          }
          else
          {
            done = true;
          }
        }
      }
    }
  }
}

//lint +esym(586,snprintf)
