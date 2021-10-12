#ifndef DMIMessageOutTime_hpp
#define DMIMessageOutTime_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This is the message for the Time message to sent to the DMI to the DMI Time.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 08-06-2017    adgupta     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutTime is a creator for the out time DMIMessage
    */
    class DMIMessageOutTime : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of Out time message
      */
      DMIMessageOutTime();

      /**
       * Validates the collected input data and creates the outgoing message in network byte-order
       *
       * @return true if data is valid and resulted in a valid outgoing message
       */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and invalidates the out time(shall be called once per ATP execution-cycle)
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

      /**
      * Unix Time in seconds after January 1 1970
      */
      uint64_t unixTime;
    };
  }
}
#endif
