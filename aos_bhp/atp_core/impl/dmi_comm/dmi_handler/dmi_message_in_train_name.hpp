#ifndef DMIMessageInTrainName_hpp
#define DMIMessageInTrainName_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
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
* 2017-12-11    akushwah    Created
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
    * DMIMessageInTrainName is a parser for the incoming Train Name message
    */
    class DMIMessageInTrainName : public AbstractDMIMessageIn
    {
    public:
      /**
      * Constructor for DMIMessageInTrainName which is a parser for the incoming Train Name message
      */
      DMIMessageInTrainName();

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
      * Access-function for train name changed by driver
      *
      * @param[out] trainName changed by driver
      *
      * @return true if driver requested a change of train name
      */
      bool getChangedTrainName(char_t* const trainName) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseDMIMessageData();

      /**
      * Changed Train Name 
      * NULL-terminated name of train 
      */
      char_t changedTrainName[trainNameMaxLength + 1U];
    };

  }
}
#endif
