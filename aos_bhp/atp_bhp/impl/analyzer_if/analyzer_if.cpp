/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the methods of the AnalyzerIF class
*  which contains the adaptation functionality of the AnalyzerIF
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-14    saprasad     Created
* 2016-01-11    saprasad     Implemented the adaptation part for Analyzer IF
* 2016-01-19    saprasad     Incorporated review comments for Analyzer IF
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <vfw_string.h>

#include "analyzer_if.hpp"
#include "config.hpp"
#include "basic_ip.hpp"

#include "abstract_application_base.hpp"

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

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

  /******************************************************************************
   * Constructor
   ******************************************************************************/
  AnalyzerIF::AnalyzerIF() :AbstractAnalyzerIF()
  {

  }

  /******************************************************************************
  * instance
  ******************************************************************************/
  AnalyzerIF& AnalyzerIF::instance()
  {
    static AnalyzerIF theOnlyAnalyzerIFInstance;
    return theOnlyAnalyzerIFInstance;
  }

  /******************************************************************************
  * writeAIFUnitData
  ******************************************************************************/
  void AnalyzerIF::writeAIFUnitData() const
  {
    OutputBuffer outputBuffer;
    uint32_t lenOfStrToAppend = 0U;

    // Append unit Data AIF HeaderStart to outputBuffer
    int32_t retVal = snprintf(&outputBuffer.charBuffer[0], maxOutputBufferLen, "[UnitDataStart]\r\n");

    if ((retVal > 0)  &&  (static_cast<size_t>(retVal) < maxOutputBufferLen))
    {
      lenOfStrToAppend += static_cast<size_t>(retVal);
    }

    uint32_t bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
    // Append Name (ATP/ATO) 
    retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "Name %s\r\n",
      ATC::AbstractApplicationBase::corePtr()->getApplicationName());

    if ((retVal > 0)  && (static_cast<size_t>(retVal) < bytesLeft))
    {
      lenOfStrToAppend += static_cast<uint32_t>(retVal);
      bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
    }

    // Append Version information 
    retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "Version %s\r\n",
      ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());

    if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
    {
      lenOfStrToAppend += static_cast<uint32_t>(retVal);
      bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
    }

    // Append ProtocolVer number to  outputBuffer buffer 
    retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "ProtocolVer %u\r\n",
      protocolVersionAIF);

    if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
    {
      lenOfStrToAppend += static_cast<uint32_t>(retVal);
      bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
    }

    // Append unit Data AIF HeaderEnd to  outputBuffer buffer 
    retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "[UnitDataEnd]\r\n");

    if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
    {
      lenOfStrToAppend += static_cast<uint32_t>(retVal);
      // Sending Unit Data to AOS Analyzer 
      writeToAOSAnalyzer(outputBuffer, lenOfStrToAppend);
    }
  }

  /******************************************************************************
  * getConnectionID
  ******************************************************************************/
  uint8_t AnalyzerIF::getConnectionID(void) const
  {
     return BasicIP::connectionAnalyzerIF;
  }
}

//lint +esym(586,snprintf)
