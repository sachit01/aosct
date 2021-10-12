#ifndef LCSMessageInRclStatus_hpp
#define LCSMessageInRclStatus_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The parsers for incoming messages are inherited from AbstractLCSMessageIn.
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
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_lcs_message_in.hpp"
#include "lcs_message_common.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /**
    * LCSMessageInRclStatus is a parser for the incoming Rcl Status message
    */
    class LCSMessageInRclStatus : public AbstractLCSMessageIn
    {
    public:
      /**
      * Constructor for LCSMessageInRclStatus which is a parser for the Rcl Status message
      */
      LCSMessageInRclStatus();

      /**
      * Validates the extracted data
      *
      * @param[in] mData   The incoming message data to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(EmpMsg* const mData);

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Access-function for any published Rcl Status
      *
      *  @param[out] status   The handling done
      *
      *  @return true if any Rcl Status message is published
      */
      bool getHandlingDone(HandlingDoneType & status) const;
     
    private:

      /**
      * Parses the extracted data
      *
      * @param[in] messageData   The incoming message data to be parsed
      *
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData(EmpMsg* const messageData);

      /**
      * The storage of LCSRclStatus which is a result of a successful parse of the incoming Rcl Status message.
      * The status information may be accessed with the access-function during one ATP execution cycle
      * until invalidated by a call to invalidate().
      */
      HandlingDoneType handlingDone;

    };
  }
}
#endif
