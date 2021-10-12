#ifndef DMIMessageInRegistrationArea_hpp
#define DMIMessageInRegistrationArea_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
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
* 19-04-2017    adgupta     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "radio_message.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageInRegistrationArea is a parser for the incoming Registration Area message
    */
    class DMIMessageInRegistrationArea : public AbstractDMIMessageIn
    {
    public:
      /**
      * Constructor for DMIMessageInRegistrationArea which is a parser for the incoming Registration Area message
      */
      DMIMessageInRegistrationArea();

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
      * Access-function for Registration area
      *
      * @param[out] area Registration Area
      *
      * @return true if Registration Area is available
      */
      bool getRegistrationArea(uint8_t &area) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * ID of the Registration Area
      */
      RegistrationArea regArea;
    };

  }
}
#endif
