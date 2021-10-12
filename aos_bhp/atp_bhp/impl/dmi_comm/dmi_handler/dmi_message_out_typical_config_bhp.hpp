#ifndef DMIMessageOutTypicalConfigBHP_hpp
#define DMIMessageOutTypicalConfigBHP_hpp

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creator for this outgoing message inherits from AbstractDMIMessageOut.
*  One creator per message-type.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-12-14    csundin     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"
#include "config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutTypicalConfigBHP is a creator for the outgoing typical config DMIMessage.
    */
    class DMIMessageOutTypicalConfigBHP : public AbstractDMIMessageOut
    {

    public:
      /**
      * The maximum number of configurations supported by this message.
      */
      static const uint8_t maxNoOfConfigs = 5U;

      /**
      * Constructor for a creator of a outgoing typical config DMIMessage.
      */
      DMIMessageOutTypicalConfigBHP();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /** Number of valid typical configs in @ref configs */
      uint8_t noOfConfigs;

      /** Typical configurations */
      BHPTypicalConfig configs[maxNoOfConfigs];
    };
  }
}

#endif
