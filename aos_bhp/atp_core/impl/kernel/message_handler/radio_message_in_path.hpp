#ifndef RadioMessageInPath_hpp
#define RadioMessageInPath_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
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
#include "atc_bit_checker.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * RadioMessageInPath is a parser for the incoming DriverLogonStatus message
    */
    class RadioMessageInPath : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInPath which is a parser for the incoming DriverLogonStatus message
      */
      RadioMessageInPath();

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
      * Access-function for any published Path
      *
      *  @return the Path message if any Path message is published, NULL otherwise
      */
      virtual const Path* getPath() const;

    private:

      /**
      * Helper class to see if a NID_TRACK is used more than once
      *
      */
      ATC::BitChecker<uint16_t, 65535U> nidTracksUsed;

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
      * The storage of CommandMessage which is a result of a successful parse of the incoming Path message.
      */
      Path pathMessageData;

      /**
      * Checks the consistency of the track Id for next target
      *
      * @return true if valid track Id for next target is present in TRACKS block
      */
      bool validateTracks();

      /**
      * Checks the consistency of tracks Id of Speed Change Position block 
      *
      * @return true if valid track Id for SPEED_CHANGE_POSITION block is present in TRACKS block
      */
      bool validateSpeedChangePosition() const;
    };
  }
}
#endif
