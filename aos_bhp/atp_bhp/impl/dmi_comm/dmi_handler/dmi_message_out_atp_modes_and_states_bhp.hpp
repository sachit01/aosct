#ifndef DMIMessageOutATPModesAndStatesBHP_hpp
#define DMIMessageOutATPModesAndStatesBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  One creator per message-type.
*  The DMIMessageOutATPModesAndStatusBHP creator is responsible for collecting
*  ATP Modes and States from other components and validation and creation of
*  the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-07-11    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_atp_modes_and_states.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
   /**
   * Bit value for additional status bits 1 ,In approach area for level crossing (Data 11 bit 7)
   */
   static const uint8_t additionalStatusInApproachAreaForLevelCrossing = 0x80U;

   /**
   * Bit Value for Last Car Brake Test in progress (Data 12 bit-2)
   */
   static const uint8_t additionalLastCarBrkTestinProgress = 0x04U;

   /**
   * Pneumatic brake system in use (Data 12, bit 6 =1,bit 7 = 0)
   */
   static const uint8_t additionalStatusPneumaticBrakeSystem = 0x40U;

   /**
   * ECPB brake system in use (Data 12, bit 6 = 0 ,bit 7 = 1)
   */
   static const uint8_t additionalStatusECPBBrakeSystem = 0x80U;

   /**
   * BrakeSystem3 brake system in use (Data 12, bit 6 = 1,bit 7 = 1)
   */
   static const uint8_t additionalStatusBrakeSystem3 = 0xC0U;

    /**
    * Bit Value for Radio Channel Enable ( Data 20 bit -4)
    */
    static const uint8_t radioChannelEnable = 0x10U;

    /**
    * Bit Value for confirm Abort last car brake Pressure Test in progress (Data 21  bit -3)
    */
    static const uint8_t confirmAbortLastCarBrakeTestInProgress = 0x08U;

    /**
    * Bit Value for confirmation of change of Train Loaded status (Data 21 bit 7)
    */
    static const uint8_t confirmTrainLoadedStatusChange = 0x80U;

    /**
    * Bit Value for Rapid loss of brake pressure detected (Data 22 bit -0)
    */
    static const uint8_t rapidLossOfBrakePressureDetected = 0x01U;

    /**
    * Bit Value for Safe for boarding is active  ( Data 22 bit -1)
    */
    static const uint8_t safeForBoardingIsActive = 0x02U;

    /**
    * DMIMessageOutATPModesAndStatusBHP is a creator for the outgoing ATPModesAndStatus message
    */
    class DMIMessageOutATPModesAndStatusBHP : public DMIMessageOutATPModesAndStatus
    {
    public:

      /**
      * Constructor for the creator of the outgoing ATPModesAndStatus message
      */
      DMIMessageOutATPModesAndStatusBHP();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    private:
      /**
      * Collect Brake System in Use
      */
      void collectBrakeSystemInUse();

      /**
      * Evaluate and get if the last car brake-test is in progress
      */
      bool getLastCarBrkTestInProgress() const;


    };
  }
}
#endif
