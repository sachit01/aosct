#ifndef BTMStatusMessage_hpp
#define BTMStatusMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the BTM status message sent from OPCAgent to AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-31    arastogi    Created
* 2016-09-23    adgupta     Implementation for BTM Handler
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "btm_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace IO
  {

    /**
    * The class BtmStatusMessage defines the BTM status message telegram.
    *
    */
    class BtmStatusMessage
    {
    public:

      /**
      * Constructor.
      *
      */
      BtmStatusMessage();

      /**
      * Function to check if BTM test was completed successfully
      *
      * @return btmTestOk variable.
      */
      bool isBtmTestOk() const;

      /**
      * Function to check if telepowering switch off is requested
      *
      * @return telepoweringSwitchOffRequest variable.
      */
      uint8_t getTelepoweringStatus() const;

      /**
      * This function returns the status of the BTM overheating warning.
      *
      * @return - Returns the Over heating Warning Status
      */
      bool getBTMOverheatingWarningStatus() const;

      /**
      * This function returns if BTM antenna is present above a balise or not.
      *
      * @return - Returns if the BTM antenna is over the balise or not
      */
      bool isBalisePresent() const;
      
      /**
      * Function to unpack the BTM Status message.
      *
      * The data is unpacked from the structure.
      * Checks are performed to ensure the data extracted is valid.
      * If valid the class variables are updated in the BTM Handler.
      *
      * @param [in] btmStatus Pointer to BTM status data from OPC Agent
      * @return true if the unpacked message is valid, false otherwise.
      */
      void unpack(const GP_10ByteVitalSinkDataA& btmStatus);

      /**
      * Return the bsaCounter
      *
      * @return bsaCounter.
      */
      uint8_t getBsaCounter() const;

      /**
      * Return the baliseServiceAvailable
      *
      * @return baliseServiceAvailable.
      */
      uint8_t getBaliseServiceAvailable() const;

      /**
      * Return the Preliminary Availability Test Status
      *
      * @return preliminaryAvailabilityTestStatus.
      */
      uint8_t getPreliminaryAvailabilityTestStatus() const;

      /**
      * Return true if initialized
      *
      * @return true if we have had contact and read at least one packet.
      */
      bool isInitialized() const;

      /**
      * Get the version number for command message
      *
      *@return - returns the version number for the status message
      */
      uint8_t getVersion() const;

      /**
      * Get the telegram enable bits
      *
      *@return - returns the version number for the status message
      */
      uint8_t getEnabledTelegramFormat() const;

      /**
      * Get the BTM Options
      *
      *@return - returns the BTM options
      */
      uint8_t getBtmOptions() const;

      /**
      * Initializes the cross compare module.
      */
      void initCrossCompare() const;

    private:

      /**
      * Copy Constructor.
      *
      * Disabled, do not implement it
      */
      BtmStatusMessage(const BtmStatusMessage& btmStatusMessage);

      //Please refer BTM IFS 3NSS010889D0108 v3.2 sec 5.2

      /**
      * Status from received message if BTM test(start-up or Routine test) was successful.
      */
      bool btmTestOk;

      /**
      * Status from received message if BTM service is available.
      * 0:Available
      * 1:Routine test in progress
      * 2:sporadic failure
      * 3:permanent failure
      */
      uint8_t baliseServiceAvailable;

      /**
      * The counter value from received message indicating if BSA has changed.
      */
      uint8_t bsaCounter;

      /**
      * Status of the antenna telepowering from the received message.
      */
      uint8_t telepoweringStatus;

      /**
      * The BTM protocol reported by the received message.
      */
      uint8_t usedProtocol;


      /* Status and options */

      /**
      * Indicates if antenna power off is requested by BTM. BTM overheating warning.
      */
      bool telepoweringSwitchOffRequest;
      
      /**
      * Status from received message if balise is present below antenna.
      * Intended to be used when train is in a stand still
      */
      bool balisePresent;

      /**
       * Preliminary Availability Test Status
       * 0 : Not implemented : The test is not implemented.
       * 1 : Disabled : The test is not enabled in BTM command.
       * 2 : Unavailable : The test is not available, e.g., due to the antenna being switched off or an on - going routine test.
       * 3 : Test in progress : This is the initial state when the test becomes active after being disabled or unavailable.
       * The BTM is not expected to remain in this state for more than 1 s before going to one of the states 4 to 6.
       * 4 : Green / Success : The test indicates that the BTM + antenna are likely to give full performance when the train starts rolling.
       * 5 : Red / Error : The test indicates that the BTM + antenna are not likely to give full performance when the train starts rolling.
       * 6 : Yellow / Warning : It cannot be determined whether the BTM + antenna are likely to give full performance when the train starts rolling.
       * 7 : Unused.
       */
      uint8_t preliminaryAvailabilityTestStatus;

      /**
      * BTM options
      */
      uint8_t btmOptions;

      /**
      * BTM enabled telegram format
      */
      uint8_t enabledTelegramFormat;
    };
  }
}

#endif
