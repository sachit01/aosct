#ifndef RadioMessage_hpp
#define RadioMessage_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*
*  DESCRIPTION:
*   The RadioMessage class is used by the RadioChannel and RadioHandler to represent one 
*   incoming or outgoing message.              
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-01-18    bhermans    Created
* 2016-04-19    lantback    Include atp_types, atp_base
* 2016-06-09    akushwah    Radio Channel implementation
* 2016-06-16    akushwah    updated the Structure for RadioMessageToSend & RadioMessageReceived
* 2016-06-17    akushwah    added doxygen markup for structure variables 
* 2016-08-04    adgupta     addedLeadIn LeadOut values
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_sync.h>
#include <cstring>
#include "atp_types.hpp"
#include "event.hpp"
#include "channel_config.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  /** The RadioMessage class represents one 
  *  incoming or outgoing message.
  *  The attributes are represented in host byte order 
  *  with the exception of the variable data part which is 
  *  represented in network byte order.
  */
  class RadioMessage
  {
  public:

    /** Constructor 
    *  Initialize data length to 0
    *  in case object is copied 
    */
    RadioMessage()
    {
      dataLength = 0;
      memset(data, 0, ATC::maxRadioMessageSize);
    }

    /** Copy constructor 
    *  is only copying the data being used
    */
    RadioMessage(const RadioMessage & msg)
    {
      id      = msg.id;
      siteId  = msg.siteId;
      regionId = msg.regionId;
      tSender = msg.tSender;
      tRef    = msg.tRef;
      dataLength = msg.dataLength;
      memset(data, 0, ATC::maxRadioMessageSize);
      memmove(data, msg.data, msg.dataLength);
    }

    /** Assignment operator
    *  is only copying the data being used
    */
    RadioMessage& operator = (const RadioMessage& msg)
    {
      id = msg.id;
      siteId = msg.siteId;
      regionId = msg.regionId;
      tSender = msg.tSender;
      tRef = msg.tRef;
      dataLength = msg.dataLength;
      memset(data, 0, ATC::maxRadioMessageSize);
      memmove(data, msg.data, msg.dataLength);

      return *this;
    }


    /** Size of header includes
    *  STX, ID, SITE ID,REGION ID, LEN, T_SENDER, T_REF  
    */
    static const uint8_t    headerSize = 11U;

    /** Identity of locomotive
    *  Host byte order
    */
    uint16_t id;        

    /** Identity of site
    *  Host byte order
    */
    uint8_t  siteId;   

    /** Identity of Region
    *  Host byte order
    */
    uint8_t  regionId;

    /** Timestamp from sender
    *  Host byte order
    */
    uint16_t tSender;   

    /** Previous timestamp
    *  Host byte order
    */
    uint16_t tRef;       

    /** Length of the variable part of the telegram
    *  Host byte order
    */
    uint16_t dataLength;  

    /** Variable part of the telegram
    *  The bytes shall appear in network order
    */
    uint8_t  data[ATC::maxRadioMessageSize];
  };

  /** The message to send and its destination channel id
  */
  struct RadioMessageToSend
  {
    RadioMessage message;    //!< Radio Message which needs to be send
    uint16_t     channelId;  //!< Radio Channel Id on which the Message will be send 
  };

  /** The message received and its source channel id
  */
  struct RadioMessageReceived
  {
    RadioMessage message;    //!< Radio Message which will be received 
    uint16_t     channelId;  //!< Radio Channel Id from which the Message was received
  };

  /** Registration Area received from the DMI
  */
  struct RegistrationArea
  {
    bool    isvalid;       //!< Is the registration Area valid
    uint8_t regAreaNidId;  //!< NID Id of the Registration Area
  };
}

#endif
