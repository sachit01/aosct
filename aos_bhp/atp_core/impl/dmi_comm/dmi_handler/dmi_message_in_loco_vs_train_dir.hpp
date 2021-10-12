#ifndef DMIMessageInLocoVsTrainDir_hpp
#define DMIMessageInLocoVsTrainDir_hpp
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
    * DMIMessageInLocoVsTrainDir is a "placeholder" in the parsers representing a DMI LocoVsTrainDir messageType
    */
    class DMIMessageInLocoVsTrainDir : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI LocoVsTrainDir messageType message
      */
      DMIMessageInLocoVsTrainDir();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI LocoVsTrainDir messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /**
      *Access-function for any published LocoVsTrainDir
      *
      * @return LocoVsTrainDir
      */
      LocoVsTrainDirection getLocoVsTrainDir() const;

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
      * Loco Vs Train Direction
      */
      LocoVsTrainDirection locoVsTrainDir;

    };
  }
}
#endif
