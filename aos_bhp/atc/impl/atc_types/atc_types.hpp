#ifndef ATCTypes_hpp
#define ATCTypes_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION: 
*
* The ATC Types declares common types used by AOS components
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    bhermans    Created
* 2016-04-22    lantback    Added component type definitions
* 2016-05-24    adgupta     Added end of trail(ETX) definition
* 2016-06-07    akushwah    Added the SIL,HIL,VSIM and simEnvironment
* 2016-06-13    akushwah    Removed the SIL,HIL,VSIM and simEnvironment & corrected values of vfw_TRUE & vfw_FALSE
* 2016-06-22    jeneman     Added typedef for config item id numbers
* 2016-07-18    akushwah    Added enum for Log Level
* 2016-08-04    adgupta     Added LEAD(0x7E) for integration with TCC Sim
* 2016-08-10    akushwah    Added Config IDs for N-JRU & BDS 
* 2016-09-08    spandita    Added Component ID of OPC Sim
* 2016-09-16    nsyed       Added Config IDs for BDS & Console Port
* 2016-09-26    bhidaji     Added atcMathId
* 2016-09-28    bhidaji     Added comment on atcMathId
* 2016-01-12    saprasad    Added atcAnalyzerId for Analyzer IF Component
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_types.h>
#include "vfwChannel.h"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
typedef char char_t;

namespace ATC
{
  static const bool_t trueVfw  = 1;
  static const bool_t falseVfw = 0;

  const int32_t int32Max = 0x7FFFFFFF;
  const int32_t int32Min = static_cast<int32_t>(0x80000000);
  const int16_t int16Max = 32767;
  const int16_t int16Min = -32768;
  const uint16_t uint16Max = 0xFFFFU;
  const uint32_t uint32Max = 0xFFFFFFFFU;

  static const uint8_t STX       = 0x02U;
  static const uint8_t ETX       = 0x01U;

  //Added for integration with TCC Sim. Needs to be confirmed by TCC team before finalizing this usage.
  static const uint8_t LEAD     = 0x7EU;

  /**
  * The type definition for component id numbers.
  *
  * The id numbers shall be allocated in the following ranges:
  *   0         - Undefined/Illegal
  *   1-50      - ATC components
  *   51-151    - Core components
  *   152-252   - Adaptation components
  */
  typedef uint16_t ComponentIDType;

  //Maximum component ID Type
  static const ComponentIDType maxComponentID = 252U;

  //Minimum component ID Type
  static const ComponentIDType minComponentID = 1U;
  
  //Maximum Event Number
  static const uint16_t maxEventNr = 0xFFF;

  /** Max length of console arguments from console */
  static const uint8_t  maxArgumentStringLengthLenFromConsole = 30U;

  /** Maximum number of arguments acceptable by console */
  static const int8_t  maxNUmberOfArgumentsFromConsole = 5U;

  typedef char_t ArgumentString[maxArgumentStringLengthLenFromConsole];
  typedef ArgumentString ConsoleArguments[maxNUmberOfArgumentsFromConsole];

  /** Max length of source file names, i.e. __FILE__ */
  static const size_t maxSourceFilePathLength = 200U;

  /** 
  * Declaration of ATC component ID's 
  */
  const ComponentIDType atcEventHandlerId       = 1U;   //! Event handler component ID
  const ComponentIDType atcLogHandlerId         = 2U;   //! Log handler component ID
  const ComponentIDType atcConfigId             = 3U;   //! Config component ID
  const ComponentIDType atcConsoleId            = 4U;   //! Console component ID
  const ComponentIDType atcBasicIpId            = 5U;   //! BasicIP component ID
  const ComponentIDType atcCODSimId             = 6U;   //! COD Sim component ID
  const ComponentIDType atcVIOHSimId            = 7U;   //! VIOH Sim component ID
  const ComponentIDType atcOPCSimId             = 8U;   //! OPC Sim component ID
  const ComponentIDType atcMathId               = 9U;   //! ID for Math functions, Math is not a component
  const ComponentIDType atcAnalyzerId           = 10U;  //! ID for Analyzer ID

  
  /**
  * The type definition for DMI event codes.
  *
  */
  typedef uint16_t DMIEventCode;


  /**
  * The containers for the components.
  *
  */
  enum ComponentContainers
  {
    CommonContainer,
    CoreContainer,
    AdaptationContainer
  };

  /**
  * The Software Blocks enclosing the Components.
  */
  enum ComponentBlock
  {
    ATPBlock,
    ATOBlock,
    DispatcherBlock,
    DMIBlock
  };

  /**
  * The authorities which are allowed to release EB.
  * Ordered in the increasing authority level.
  *
  */
  enum EBReleaser
  {
    NoEB,
    DispatcherEB,
    DriverEB
  };

  /**
  * The authorities which are allowed to release SB.
  * Ordered in the increasing authority level.
  *
  */
  enum SBReleaser
  {
    NoSB,
    ATOSB,
    DispatcherSB,
    DriverSB
  };

  /**
  * Different Level of Log 
  */
  enum LogLevel
  {
    /** Brief Level */
    BriefLog = 1,
    /** Detailed Level */
    DetailedLog = 2,
    /** Very Detailed Level */
    VeryDetailedLog = 3,
    /** VFW Channel Info */
    ChannelLog = 4  // Should have the highest level as VFW channel logging will produce a lot of information
  };

  /**
  * Trace Levels
  */
  const uint8_t briefMessageTrace = 1U;       //<! Brief trace of Messages sent/received 
  const uint8_t detailedMessageTrace = 2U;    //<! Detailed trace Messages sent/received
  const uint8_t veryDetailedMessageTrace = 3U;//<! Very detailed trace of messages sent/received
  const uint8_t briefTrace = 5U;              //<! Brief trace of general info
  const uint8_t detailedTrace = 6U;           //<! Detailed trace
  const uint8_t veryDetailedTrace = 7U;       //<! Very detailed trace

  /**
  * Channel statistics structure
  */
  struct ChannelStats
  {
    char_t channelname[VFW_CH_NAME_MAX_Z];
    char_t channelType[6];   //Read/Write
    uint32_t numMsgCnt;     //Number of messages sent/received
    uint32_t numMsgBytesCnt;//Number of 
  };

#ifdef GENERATE_TCC_EVENTLIST_XML_PATH
  /**
  * File- and Script-names for the generation of Event-list in XML.
  */
  static const char_t * registerEventCsvFile = GENERATE_TCC_EVENTLIST_XML_PATH "AOSEvent.csv";
  static const char_t * registerEventXmlFile = GENERATE_TCC_EVENTLIST_XML_PATH "aos_events.xml";
  static const char_t * registerEventXlsFile = GENERATE_TCC_EVENTLIST_XML_PATH "AOSEvents.xlsx";
  static const char_t * registerEventPythonToXmlScriptFile = GENERATE_TCC_EVENTLIST_XML_PATH "csv2xml.py";
  static const char_t * registerEventPythonToXlsxScriptFile = GENERATE_TCC_EVENTLIST_XML_PATH "csv2xlsx.py";
  static const char_t * registerEventPythonToXmlSchemaFile = GENERATE_TCC_EVENTLIST_XML_PATH "rcs_cmm.xsd";

#ifdef WIN32
  static const char_t * registerEventPythonCommand = "py";
  static const char_t * registerEventDeleteCommand = "del";
  static const char_t * registerEventXmlLintFile = GENERATE_TCC_EVENTLIST_XML_PATH "xmllint\\xmllint.exe";
#else
  static const char_t * registerEventPythonCommand = "python";
  static const char_t * registerEventDeleteCommand = "rm";
  static const char_t * registerEventXmlLintFile = "xmllint";
#endif

#endif

}

#endif
