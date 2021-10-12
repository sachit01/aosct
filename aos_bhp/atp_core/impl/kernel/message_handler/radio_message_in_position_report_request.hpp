#ifndef RadioMessageInPositionReportRequest_hpp
#define RadioMessageInPositionReportRequest_hpp
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
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vector>

#include "radio_message.hpp"
#include "abstract_radio_message_common.hpp"
#include "abstract_radio_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the parsed information of an arriving PositionReportRequest
    *
    */
    struct PositionReportRequest
    {
      bool             waitingTimeReceived;    //!< Estimated waiting time received
      WaitingTime      waitingTime;            //!< Estimated waiting time to next MA. Only applicable if train in idle
      bool             initiateConfigReceived; //!< Initiate configuration received
      InitiateConfig   initiateConfig;         //!< If present AOS is requested to initiate configuration to resume operation
    };
    
    /**
    * RadioMessageInPositionReportRequest is a parser for the incoming PositionReportRequest message
    */
    class RadioMessageInPositionReportRequest : public AbstractRadioMessageIn
    {
    public:
      /**
      * Constructor for RadioMessageInPositionReportRequest which is a parser for the incoming PositionReportRequest message
      */
      RadioMessageInPositionReportRequest();

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
      * Access-function for any published initiate config 
      *
      *  @param[out] initiateConfigValue   The published config reason
      *
      *  @return true if any initiate config reason is available
      */
      virtual bool getInitiateConfig(InitiateConfigReason &initiateConfigValue) const;

    protected:
      /**
      * Function for detailed log of incoming message
      *
      */
      virtual void detailedLog(void) const;

    private:

      /**
      * Parses the extracted data
      *
      * @return true if data is valid with respect to parsing
      */

      bool parseMessageData();
      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      virtual bool publishData() const;

      /**
      * The storage of PositionReportRequest which is a result of a successful parse of the 
      * incoming PositionReportRequest message.
      */
      PositionReportRequest positionReportRequest;

    };
  }
}
#endif
