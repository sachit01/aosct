#ifndef DMIMessageInTrainVsTrackDir_hpp
#define DMIMessageInTrainVsTrackDir_hpp
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
    * DMIMessageInTrainVsTrackDir is a "placeholder" in the parsers representing a DMI TrainVsTrackDir messageType
    */
    class DMIMessageInTrainVsTrackDir : public AbstractDMIMessageIn
    {
    public:

      /**
      * Constructor for a parser of a DMI TrainVsTrackDir messageType message
      */
      DMIMessageInTrainVsTrackDir();

      /**
      * Validates the extracted data
      *
      * @return true if the parser is implemented for DMI TrainVsTrackDir messageType message
      */
      virtual bool validate();

      /**
      * Invalidates the extracted data
      *
      */
      virtual void invalidate();

      /**
      * Access-function for any published TrainVsTrackDirection
      *
      *  @return TrainVsTrackDir
      */
      TrainVsTrackDirection getTrainVsTrackDirection() const;

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
      * MMI to ATP Train Vs Track Direction Data
      */
      TrainVsTrackDirection trainVsTrackDir;

    };
  }
}
#endif
