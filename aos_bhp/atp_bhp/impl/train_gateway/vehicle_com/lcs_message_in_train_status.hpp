#ifndef LCSMessageInTrainStatus_hpp
#define LCSMessageInTrainStatus_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
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
* 2016-11-24    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_lcs_message_in.hpp"
#include "lcs_message_common.hpp"
#include "abstract_event_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /**
    * Value set when the Last Car Brake Pressure is not asserted by LCS
    */
    static const uint8_t lastCarBrakePressureNotAsserted = 255U;

    /**
    * Value set when the Total Train Weight is not asserted by LCS
    */
    static const uint32_t totalTrainWeightTonsNotAsserted = 0U;

    /**
    * LCSMessageInTrainStatus is a parser for the incoming TrainStatus message
    */
    class LCSMessageInTrainStatus : public AbstractLCSMessageIn
    {
    public:
      /**
      * Constructor for LCSMessageInTrainStatus which is a parser for the incoming TrainStatus message
      */
      LCSMessageInTrainStatus();

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
      * Logs the given message to RU. Assumes that validate() has been called
      * successfully for this message.
      *
      * Only logs the message if the state information has changed or if a certain time
      * has passed since the last call. (See configuration item "RuLogDynValuePeriod")
      */
      virtual void logToRU(const EmpMsg* const mData) const;

      /**
      * Access-function for any published TrainStatus
      *
      *  @param[out] status   The TrainStatus
      *
      *  @return true if any TrainStatus is published
      */
      bool getTrainStatus(LCSTrainStatusType & status) const;
     
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
      * The storage of LCSTrainStatus which is a result of a successful parse of the incoming TrainStatus message.
      * The status information may be accessed with the access-function during one ATP execution cycle
      * until invalidated by a call to invalidate().
      */
      LCSTrainStatusType trainStatus;

    };
  }
}
#endif
