#ifndef AbstractCrossCompare_hpp
#define AbstractCrossCompare_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* This file defines CrossCompare class which contains the core braking logic 
* used by the AOS. 
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-06    rquensel    Created
* 2017-01-27    rquensel    Added Cross Compare of Output
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_crosscompare.h>
#include "event.hpp"
#include "cross_compare_object.hpp"
#include "atc_base.hpp"
#include "cross_compare_output_channel.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Support
  {
    // Max buffer size for input/output data
    const uint16_t inputOutputCrossCompareSize = 32768U;

    /**
    * Implements the Core part of the CrossCompare component.
    */
    class AbstractCrossCompare : public ATC::IOComponent
    {
    public:

      /**
      * Implements the virtual preInit function.
      *
      */
      virtual void preInit();

      /**
      * Implements the virtual init function.
      *
      * This init function needs to be run before any other component can call this module.
      * \exception vfwInit() not yet called.
      * \exception vfwStart() already called.
      *
      * @return true
      */
      virtual bool init();

      /**
      * Must be called after initialization of the application has finished (and before any runIn,
      * run or runOut function is called).
      *
      * Tells AbstractCrossCompare that initialization has finished and that no more cross compare
      * objects can be added.
      */
      void initDone();

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractCrossCompare* corePtr();

      /**
      * Implements the virtual runIn function.
      *
      * The runIn function calls vfwCrossCompare for the input data. Vital Framework will then call
      * all the necessary callbacks in order to schedule the execution of Cross Compare.
      */
      virtual void runIn();

      /**
      * Implements the virtual runOut function.
      *
      * The run function calls vfwCrossCompare for the output data. Vital Framework will then call 
      * all the necessary callbacks in order to schedule the execution of Cross Compare.
      */
      virtual void runOut();

      /**
      * This function is used by the modules that have input data to be cross compared
      *
      * @param[in] buffer      Buffer to write input cross compare data to
      * @param[in] bufferSize  Number of bytes to add
      */
      void addCrossCompareInputData(const uint8_t* const buffer, const uint16_t bufferSize);

      /**
      * This function shall only be called from the init method. It will add the provided object to the list of objects to be cross compared.
      *
      * @param[in] crossCompare   The cross compare object to append to the list of cross compare items
      */
      void addCrossCompareData(CrossCompareObject* const crossCompare);

      /**
      * This function shall only be called from the init method. It will add the provided object to the list of objects to be cross compared.
      *
      * @param[in] crossCompare   The cross compare object to append to the list of cross compare items
      */
      void addCrossCompareOutputData(CrossCompareOutput* const crossCompare);

      /**
      * Destructor
      */
      ~AbstractCrossCompare();

    protected:
      /**
      * Constructor
      */
      AbstractCrossCompare();

    private:
      
      /**
      * Copy constructor, just disabled and not implemented.
      */
      AbstractCrossCompare(const AbstractCrossCompare& arg);

      /**
      * Assignment operator, just disabled and not implemented.
      */
      AbstractCrossCompare & operator= (const AbstractCrossCompare & other);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void writeCrossCompareDataCallbackRunIn(VFW_Buffer* const ownBuffer);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void writeCrossCompareDataCallbackRunOut(VFW_Buffer* const ownBuffer);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void writeCrossCompareDataCallbackVersionCheck(VFW_Buffer* const ownBuffer);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void receiveCrossCompareDataCallbackIn(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void receiveCrossCompareDataCallbackOut(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer);

      /**
      * A callback function that calls the corresponding non-static method. Needed to map a C-style callback to a C++ method call.
      */
      static void receiveCrossCompareVersionCheck(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer);

      /**
      * This function is called from the callback function provided to vital framework when the input data should be added to cross compare
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeCrossCompareDataRunIn(VFW_Buffer* const ownBuffer);

      /**
      * This function is called from the callback function provided to vital framework when the data should be added to cross compare
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeCrossCompareDataRunOut(VFW_Buffer* const ownBuffer);

      /**
      * This function is called from the callback function provided to vital framework when the data should be added to cross compare
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeCrossCompareVersionCheck(VFW_Buffer* const ownBuffer) const;

      /**
      * This function is called from the callback function provided to vital framework when the input data from the other CPU should be cross compared
      *
      * @param[in] ownBuffer     Our own buffer to cross compare
      * @param[in] otherBuffer   The buffer from the other CPU to cross compare
      */
      void compareCrossCompareDataIn(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer);

      /**
      * This function is called from the callback function provided to vital framework when the output data from the other CPU should be cross compared
      *
      * @param[in] ownBuffer     Our own buffer to cross compare
      * @param[in] otherBuffer   The buffer from the other CPU to cross compare
      */
      void compareCrossCompareDataOut(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer);

      /**
      * This function is called from the callback function provided to vital framework when the version data from the other CPU should be cross compared
      *
      * @param[in] ownBuffer     Our own buffer to cross compare
      * @param[in] otherBuffer   The buffer from the other CPU to cross compare
      */
      void compareCrossCompareVersionCheck(VFW_Buffer* const ownBuffer, VFW_Buffer* const otherBuffer) const;

      /**
      *
      * Helper function to write output data to cross compare
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeCrossCompareDataOutput_(VFW_Buffer* const ownBuffer);

      /**
      *
      * Helper function to write one or more objects to cross compare
      *
      * @param[in] ownBuffer   Buffer to write all cross compare data to
      */
      void writeCrossCompareObjects(VFW_Buffer* const ownBuffer);

      // Attributes

      /**
      * Maximum number of CrossCompareObject instances that can be compared each cycle.
      */
      static const uint8_t maxObjectsToComparePerCycle = 10U;

      /**
      *
      * Maximum CRC data size.
      */
      static const uint16_t maxCrcDataSize = 2048U;

      /**
      * Maximum length of the ATP version plus the configuration versions
      */
      static const uint8_t maxVersionLength = 64U;

      /**
      *
      * Internal flag to keep track of cross compare errors
      */
      bool crossCompareErrorOccured_;

      /**
      * Handle for performing cross comparison of input data.
      */
      VFW_CrossCompare crossComparePointInput;

      /**
      * Handle for performing cross comparison of output data and vital variables.
      */
      VFW_CrossCompare crossComparePointOutput;

      /**
      * Handle for performing cross comparison of version data.
      */
      VFW_CrossCompare crossComparePointVersion;

      /**
      * Points to the next CrossCompareObject to be cross compared.
      */
      const CrossCompareObject* nextObjectToCompare;

      /**
      * Points to the first CrossCompareObject in the list.
      */
      CrossCompareObject* crossCompareObjectList;

      /**
      * Points to the first CrossCompareOutput object in the list.
      */
      CrossCompareOutput* crossCompareOutputList;

      /**
      *
      * Internal attribute to store how many bytes we have to cross compare this cycle for input/output data.
      */
      uint16_t inputBufferPos_;

      /**
      *
      * Internal buffer to store input/output data to be cross compared.
      */
      uint8_t inputOutputCrossCompareBuffer_[inputOutputCrossCompareSize];

      /**
      * Internal SafetyHalt Event raised when the Cross Compare data for CPU-A and CPU-B does not match
      */
      const ATC::Event eventMismatchSafetyHalt_;

      /**
      * Internal SafetyHalt Event raised when the Cross Compare data could not be written
      */
      const ATC::Event eventCouldNotWriteSafetyHalt_;

      /**
      * Internal SafetyHalt Event raised when the Cross Compare input/output data could not fit the buffer
      */
      const ATC::Event eventCouldNotAddIOSafetyHalt_;

      /**
      * flag to check whether version is done
      */
      bool isVersionSentForCrossCompare_;

      /**
      * Flag that indicates whether initialization of the application has finished.
      */
      bool initHasFinished;
    };
  }
}
#endif
