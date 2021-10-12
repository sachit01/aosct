#ifndef RadioMessageInMovementAuthorityBHP_hpp
#define RadioMessageInMovementAuthorityBHP_hpp
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

#include "radio_message_in_movement_authority.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * RadioMessageInMovementAuthority is a parser for the incoming EmAlert message
    */
    class RadioMessageInMovementAuthorityBHP : public RadioMessageInMovementAuthority
    {

    public:

      /**
      * Constructor for RadioMessageInMovementAuthorityBHP which is a parser for the incoming Movement Authority message
      */
      RadioMessageInMovementAuthorityBHP();

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

    protected:

      /**
      * Validates the Q_TRACK_DATA_TYPE parameter
      *
      * @param[in] val Value to be validated
      *
      * @return True if value is within limits
      */
      virtual bool validateQ_TRACK_DATA_TYPE(const uint8_t val) const;

      /**
      * Publishes BHP specific N_ADHESION data
      */
      virtual void publishAdhesion() const;

      /**
      * Publishes the Track Data Item target data
      */
      virtual void publishTrackDataItemTarget() const;

    private:

      /**
      * BHP specific value for adhesion
      */
      static const uint8_t adhesionValueBHP = 100U;
    };
  }
}
#endif
