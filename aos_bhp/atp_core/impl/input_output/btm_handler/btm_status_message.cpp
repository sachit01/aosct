/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This implements the BtmStatusMessage class.
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
#include "btm_status_message.hpp"
#include "atc_bit_unpacker.hpp"
#include "abstract_log_handler.hpp"
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
    BtmStatusMessage::BtmStatusMessage()
      :
      btmTestOk(false),
      baliseServiceAvailable(1U),
      bsaCounter(0U),
      telepoweringStatus(false),
      usedProtocol(0U),
      telepoweringSwitchOffRequest(false),
      balisePresent(false),
      preliminaryAvailabilityTestStatus(0U),
      btmOptions(0U),
      enabledTelegramFormat(0U)
    {
    }

    /******************************************************************************
    * isBtmTestOk
    ******************************************************************************/
    bool BtmStatusMessage::isBtmTestOk() const
    {
      return btmTestOk;
    }

    /******************************************************************************
    * getTelepoweringStatus
    ******************************************************************************/
    uint8_t BtmStatusMessage::getTelepoweringStatus() const
    {
      return telepoweringStatus;
    }

    /******************************************************************************
    * getBTMOverheatingWarningStatus
    ******************************************************************************/
    bool BtmStatusMessage::getBTMOverheatingWarningStatus() const
    {
      return telepoweringSwitchOffRequest;
    }

    /******************************************************************************
    * isBTMAntennaOverBalise
    ******************************************************************************/
    bool BtmStatusMessage::isBalisePresent() const
    {
      return balisePresent;
    }

    /******************************************************************************
    * unpack
    ******************************************************************************/
    void BtmStatusMessage::unpack(const GP_10ByteVitalSinkDataA& btmStatus)
    {
      ATC::BitUnpacker btmStatusUnpacker(&(btmStatus.safetyData[0]), sizeof(btmStatus.safetyData));

      // The ATP shall supervise the BTM Status variable Enabled Telegram Format defined in the Interface 'B' Specification
      enabledTelegramFormat = btmStatusUnpacker.unpack8(8U);

      const uint8_t newSafetyMode = btmStatusUnpacker.unpack8(8U);

      // Skip 2 unused bits + 3 Euroloop Service Available
      btmStatusUnpacker.skip(5U);

      baliseServiceAvailable = btmStatusUnpacker.unpack8(3U);
      bsaCounter = btmStatusUnpacker.unpack8(8U);

      // Preliminary Availability Test Status 3 bits
      preliminaryAvailabilityTestStatus = btmStatusUnpacker.unpack8(3U);

      // IF K Active and Used IF K BTM ID, 1 + 2 bits
      btmStatusUnpacker.skip(3U);

      telepoweringStatus = btmStatusUnpacker.unpack8(2U);

      // Next 8 bits are "Status and Options"
      telepoweringSwitchOffRequest = (btmStatusUnpacker.unpack8(1U) == 1U);
      balisePresent = (btmStatusUnpacker.unpack8(1U) == 1U);

      btmOptions = btmStatusUnpacker.unpack8(4U);

      // Status and Options, Bit 1 and 0, Spare
      btmStatusUnpacker.skip(2U);

      //BTM Protocol version
      usedProtocol = btmStatusUnpacker.unpack8(8U);

      //Euroloop Code, not used here
      btmStatusUnpacker.skip(4U);

      //Spare 20 bits
      btmStatusUnpacker.skip(20U);

      btmTestOk = ((newSafetyMode & 1U) == 0U);
    }

    /******************************************************************************
    * getBsaCounter
    ******************************************************************************/
    uint8_t BtmStatusMessage::getBsaCounter() const
    {
      return bsaCounter;
    }

    /******************************************************************************
    * getBaliseServiceAvailable
    ******************************************************************************/
    uint8_t BtmStatusMessage::getBaliseServiceAvailable() const
    {
      return baliseServiceAvailable;
    }

    /******************************************************************************
    * getPreliminaryAvailabilityTestStatus
    ******************************************************************************/
    uint8_t BtmStatusMessage::getPreliminaryAvailabilityTestStatus() const
    {
      return preliminaryAvailabilityTestStatus;
    }


    /******************************************************************************
    * isInitialized
    ******************************************************************************/
    bool BtmStatusMessage::isInitialized() const
    {
      return (usedProtocol == btmProtocolVersion);
    }


    /******************************************************************************
    * getVersion
    ******************************************************************************/
    uint8_t BtmStatusMessage::getVersion() const
    {
      return usedProtocol;
    }

    /******************************************************************************
    * getEnabledTelegramFormat
    ******************************************************************************/
    uint8_t BtmStatusMessage::getEnabledTelegramFormat() const
    {
      return enabledTelegramFormat;
    }

    /******************************************************************************
    * getBtmOptions
    ******************************************************************************/
    uint8_t BtmStatusMessage::getBtmOptions() const
    {
      return btmOptions;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void BtmStatusMessage::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&btmTestOk));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&baliseServiceAvailable));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&bsaCounter));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&telepoweringStatus));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&usedProtocol));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&telepoweringSwitchOffRequest));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&balisePresent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&preliminaryAvailabilityTestStatus));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&btmOptions));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&enabledTelegramFormat));
    }
  }
}
