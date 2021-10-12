#ifndef RadioMessageInAreaRequest_hpp
#define RadioMessageInAreaRequest_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
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
* 2017-02-28    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_radio_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInAreaRequest is a parser for the incoming AreaRequest message
    */
    class RadioMessageInAreaRequest : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInAreaRequest which is a parser for the incoming AreaRequest message
      */
      RadioMessageInAreaRequest();

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
      * Access-function for any published Region Area Identification 
      *
      *  @param[out] tccArea   The published list of Region Area
      *
      *  @return true if any region Area identification is available
      */
      virtual bool getAvailableAreasFromTCC(TCCAreas &tccArea) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData();

      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * Vector for Region area identification
      */
      TCCAreas tccAreaData;

    };
  }
}
#endif
