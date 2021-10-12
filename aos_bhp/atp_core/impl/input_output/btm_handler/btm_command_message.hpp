#ifndef BTMCommandMessage_hpp
#define BTMCommandMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class defines the BTM command message sent from AOS to OPCAgent.
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
    * The class BtmCommandMessage defines the BTM command message telegram.
    *
    */
    class BtmCommandMessage
    {
    public:

      /**
      * Constructor.
      *
      */
      BtmCommandMessage();

      /**
      * Function to set the power on/off for the antenna
      *
      * @param[in] powerOn To indicate the power state of the antenna
      */
      void setTelePoweringCmd(const bool powerOn);

      /**
      * Get the Telepowering State
      *
      * @return - returns the telepowering State
      */
      uint8_t getTelePoweringCmd() const;

      /**
      * Get the Routine Test number
      *
      * @return - returns the Routine Test number set in the Command message
      */
      uint8_t getRoutineTest() const;

      /**
      * This sets the BTM options of the command message
      *
      * @param[in] options - BTM options to be set. bit fields should be set appropriately
      * as per the BTM IFS(3NSS010889D0108) v2.3
      */
      void setBtmOptions(const uint8_t options);

      /**
      * Get the BTM options set in the command message
      *
      * @return - returns the BTM Options of Command message
      */
      uint8_t getBtmOptions() const;
      
      /**
      * Function to send the command to perform routine test
      *
      * When the routine test number is incremented by 1 and sent, routine test is 
      * automatically performed.
      */
      void setPerformRoutineTest();

      /**
      * Function to pack the BTM Command message.
      *
      * The data is collected and packed into the structure.
      * Checks are performed to ensure the data put in the structure is valid.
      *
      * @param [in] btmCmd Pointer to the buffer to send BTM command
      * @return  rue, if there is any change in the BTM command message
      */
      bool pack(GP_10ByteVitalSourceDataA& btmCmd);

      /**
      * Initializes the cross compare module.
      */
      void initCrossCompare() const;

    protected:

    private:
 
      /**
      * Preliminary Availability Test Enable
      */
      static const uint8_t preliminaryAvailabilityTestEnable = 0x1U;

      /**
      * 0:Tele-powering OFF
      * 1:Tele-powering ON
      * 2:Reserved
      * 3:Unused
      */
      uint8_t telePoweringCmd;

      /** Routine test. Number to be incremented to run the routine test.*/
      uint8_t routineTest;
            
      /**
      * BTM Options field of the command message
      */
      uint8_t btmOptions;

      /**
      * Flag to indicate whether any data has been changed in BTM command Message or not
      */
      bool isBtmCmdDataChanged;

    };
  }
}

#endif
