/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the BtmCommandMessage class.
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

#include "btm_command_message.hpp"
#include "abstract_cross_compare.hpp"

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
  namespace IO
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    BtmCommandMessage::BtmCommandMessage()
      :
      telePoweringCmd(0U),
      routineTest(0U),
      btmOptions(0U),
      isBtmCmdDataChanged(true)
    {
    }

    /******************************************************************************
    * setTelePoweringCmd
    ******************************************************************************/
    void BtmCommandMessage::setTelePoweringCmd(const bool powerOn)
    {
       uint8_t prevTelePoweringCmd = telePoweringCmd;

       if (powerOn)
       {
          telePoweringCmd = 1U;
       }
       else
       {
          telePoweringCmd = 0U;
       }

       if (telePoweringCmd != prevTelePoweringCmd)
       {
          isBtmCmdDataChanged = true;
       }
    }

    /******************************************************************************
    * getTelePoweringCmd
    ******************************************************************************/
    uint8_t BtmCommandMessage::getTelePoweringCmd() const
    {
      return telePoweringCmd;      
    }

    /******************************************************************************
    * getRoutineTest
    ******************************************************************************/
    uint8_t BtmCommandMessage::getRoutineTest() const
    {
      return routineTest;
    }

    /******************************************************************************
    * setBtmOptions
    ******************************************************************************/
    void BtmCommandMessage::setBtmOptions(const uint8_t options)
    {
      if (btmOptions != options)
      {
        btmOptions = options;
        isBtmCmdDataChanged = true;
      }
    }

    /******************************************************************************
    * getBtmOptions
    ******************************************************************************/
    uint8_t BtmCommandMessage::getBtmOptions() const
    {
      return btmOptions;
    }

    /******************************************************************************
    * isBaliseServiceAvailable
    ******************************************************************************/
    void BtmCommandMessage::setPerformRoutineTest()
    {
      ++routineTest;
      isBtmCmdDataChanged = true;
    }

    /******************************************************************************
    * pack
    ******************************************************************************/
    bool BtmCommandMessage::pack(GP_10ByteVitalSourceDataA& btmCmd)
    {
      const bool retValue = isBtmCmdDataChanged;

      if (isBtmCmdDataChanged)
      {
        isBtmCmdDataChanged = false;

        //Packing of data from the member variables.
        VFW_Buffer btmCommandBuffer;

        vfwInitBuffer(&btmCommandBuffer, &btmCmd.safetyData[0], sizeof(btmCmd.safetyData));

        const uint8_t firstByte =
          // 2 Unused, set to 0, bit 7, 6
          static_cast<uint8_t>(preliminaryAvailabilityTestEnable << 5) | // Preliminary Availability Test Enable, bit 5
          // Activate IF K, set to 0, bit 4 (Active only if Ebicab 700 or Ebicab 900 are used)
          // IF K BTM ID, set to 0. Bit 3, 2
          telePoweringCmd;  // bit 1, 0

        vfwPutU8(&btmCommandBuffer, firstByte);
        // Enable Telegram Format
        vfwPutU8(&btmCommandBuffer, telegramFormat);
        // Routine Test
        vfwPutU8(&btmCommandBuffer, routineTest);
        // ATP CU Protocol Version, the highest version of the interface protocol that the ATP CU supports.
        vfwPutU8(&btmCommandBuffer, btmProtocolVersion);
        // BTM Options
        vfwPutU8(&btmCommandBuffer, btmOptions);
        // 4 bits Euroloop code + 4 bits unused
        vfwPutU8(&btmCommandBuffer, 0U);
        // 32 bits Unused, set to 0.
        vfwPutU32(&btmCommandBuffer, 0U);
      }

      return retValue;
    }
    
    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void BtmCommandMessage::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&isBtmCmdDataChanged));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&telePoweringCmd));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&routineTest));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&btmOptions));
    }

  }
}
