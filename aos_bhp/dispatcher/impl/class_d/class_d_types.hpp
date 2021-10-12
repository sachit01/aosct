#ifndef ClassDTypes_hpp
#define ClassDTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
* Defines the Class D protocol Library types used in the implementation.
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-06    adgupta     Created
* 2017-02-06    spandita    updated the enums with the new design
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ClassD
{

  /**
  * Type definition of the Error Type used to log the error
  **/
  enum ErrorType
  {
    NoError,                        //<! All ok. No error occurred.
    CommIdIncorrect,                //<! Communication Id/message number not correct
    MissingSTX,                     //<! Received Message missing STX
    MissingETX,                     //<! Received Message missing ETX
    IncorrectMessageFormat,         //<! Received Message has incorrect format
    NotDataMsg,                     //<! Received message is not data message
    MessageIncomplete               //<! Received message was not completely read
  };

  /**
  * Type definition of Message Type to be sent/received by Class D.
  * P.S.- Only DataMsg will be used as of now.
  */
  enum MessageType
  {
    DataMsg = 1,          //<! Data message
    Ack = 2,              //<! Acknowledge message
    NAK = 3,              //<! Negative acknowledgment
    KeepAlive = 4,        //<! Keep alive message
    TestMsg = 30,          //<! Test message
    TestEchoReq = 31,      //<! Tech Echo Request
    TestEcoResponse = 32,  //<! Test Echo response
    EchoRequest = 40,      //<! Echo request message
    EchoResponse = 41      //<! Echo response
  };

}

#endif
