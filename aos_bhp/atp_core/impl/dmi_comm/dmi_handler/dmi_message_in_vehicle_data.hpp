#ifndef DMIMessageInVehicleData_hpp
#define DMIMessageInVehicleData_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming DMI messages are inherited from AbstractDMIMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming DMI data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-30    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageInVehicleData is a "placeholder" in the array of parsers representing vehicle data received from DMI
    */
    class DMIMessageInVehicleData : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI Vehicle Data
      */
      DMIMessageInVehicleData();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI Vehicle Data
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /** Access-function to know whether manual train setup is confirmed by driver in DMI
      *
      *  @return true if manualTrainSetupConfirmed is true
      */
      bool getManualTrainSetupConfirmed(void) const;


    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * Number of Vehicle Data Blocks
      */
      uint16_t noOfVehicleDatablocks;

      /**
      * Vehicle Data List received from DMI
      */
      VehicleDataBlock vehicleData[maxVehicleCount];

      /**
      * Flag to indicate whether Manual TrainSetup is Confirmed from DMI or not
      */
      bool manualTrainSetupConfirmed;

    };
  }
}
#endif
