#ifndef DMIMessageInTrainLoaded_hpp
#define DMIMessageInTrainLoaded_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming messages are inherited from AbstractDMIMessageIn.
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
* 2018-12-05    csundin     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageInTrainLoaded is a parser for the incoming Train Loaded message
    */
    class DMIMessageInTrainLoaded : public AbstractDMIMessageIn
    {
    public:
      /**
      * Constructor for DMIMessageInTrainLoaded which is a parser for the incoming Train Loaded message
      */
      DMIMessageInTrainLoaded();

      /**
       * Validates the extracted data
       *
       * @return true if data is valid as a result of a valid incoming message
       */
      virtual bool validate();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Access-function for train loaded status requested by driver
      *
      * @param[out] trainLoadedStatusRequested is the status requested by driver
      *
      * @return true if driver requested a change of train Loaded status
      */
      bool getTrainLoadedStatusRequestedByDriver(TrainLoaded & trainLoadedStatusRequested) const;


    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * The train loaded status requested by the driver
      */
      TrainLoaded trainLoadedRequested;
    };
  }
}

#endif // DMIMessageInTrainLoaded_hpp
