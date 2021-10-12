#ifndef DMIMessageInDriverIDandPassword_hpp
#define DMIMessageInDriverIDandPassword_hpp
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
* 2016-09-13    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
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
    * DMIMessageInDriverIDandPassword is a parser for the incoming DriverID and Password message
    */
    class DMIMessageInDriverIDandPassword : public AbstractDMIMessageIn
    {
    public:
      /**
      * Constructor for DMIMessageInDriverIDandPassword which is a parser for the incoming DriverID and Password message
      */
      DMIMessageInDriverIDandPassword();

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
      * Access-function for any published DriverId And Password
      *
      * @param[out] driverIdAndPass current DriverId and password
      *
      * @return true if DriverId and password is available
      */
      bool getDriverIdAndPassword(DriverIdAndPassword &driverIdAndPass) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * DriverID and Password
      */
      DriverIdAndPassword driverIDAndPassword;
    };

  }
}
#endif
