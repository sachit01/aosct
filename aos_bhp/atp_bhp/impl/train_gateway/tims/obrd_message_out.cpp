/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageOut class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    sunilk    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "obrd_message_out.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace TG
  {

    /******************************************************************************
    * OBRDMessageOut Constructor
    ******************************************************************************/
    OBRDMessageOut::OBRDMessageOut(ATC::TraceInterface* const trace_, const OBRDMessageType mType) :
      trace(trace_),
      messageType(mType)
    {
    }

    /******************************************************************************
    * OBRDMessageIn destructor
    ******************************************************************************/
    OBRDMessageOut::~OBRDMessageOut()
    {
      trace = static_cast<ATC::TraceInterface*>(NULL);
    }
  }
}

