#ifndef RadioMessageInTrainSetupBHP_hpp
#define RadioMessageInTrainSetupBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming messages are inherited from AbstractRadioMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "radio_message_in_train_setup.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * RadioMessageInTrainSetupBHP is a parser for the incoming TrainSetup message
    */
    class RadioMessageInTrainSetupBHP : public RadioMessageInTrainSetup
    {

    public:

      /**
      * Constructor for RadioMessageInTrainSetupBHP which is a parser for the incoming TrainSetup Msg
      */
      RadioMessageInTrainSetupBHP();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Validates the extracted data
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate();


    protected:
      /**
      * Parses the blocks related to adaptation in Train Setup
      *
      * @param[in] buffer            The buffer to be parsed
      * @param[in] adapBlockType     BlockType for which buffer needs to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      virtual bool parseAdditionalBlocks(VFW_Buffer* const buffer, const uint8_t adapBlockType);

      /**
      * Function for detailed log of incoming message
      *
      */
      virtual void detailedLog(void) const;

      /**
      * Function to calculate the Brake parameters
      *
      * @param[in] currentVehicleTypeData   Current VehicleType Data to check VehicleType
      * @param[in] numOfVeh                 The number of cars with the current vehicle type
      */
      virtual void calculateBrakeParameters(const VehicleTypeData* const currentVehicleTypeData, const uint16_t numOfVeh);

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      virtual bool publishData();

    private:

      /**
      * The storage of BHPBConfigVersion which is a result of a successful parse of the incoming BHPB_CONFIG_VERSION.
      */
      BHPBConfigVersion bhpConfigVersion;

      /**
      * Size of Application Data in BHPB_CONFIG_VERSION block
      *
      */
      static const uint8_t bhpbConfigVersionInTSetupBlockSize = 2U;

      /**
      * Store the Current Consecutive cars in Train setup
      */
      uint32_t currentCarsInOrder;

      /**
      * Store the Previous Consecutive cars in Train setup
      */
      uint32_t previousCarsInOrder;

    };
  }
}
#endif
