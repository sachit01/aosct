#ifndef TraceInterface_hpp
#define TraceInterface_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
* To print the Trace commands on Console each of the component will have
* a trace object of TraceInterface. This class defines the required 
* infrastructure and details of the same.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-01    adgupta     Created
* 2016-09-21    arastogi    Changed include file from atp_types to atc_types
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  static const uint8_t  maxTraceNameLen = 20U;   //!< Maximum length of Trace Name
  static const uint16_t maxHelpLen      = 256U;  //!< Maximum length of Help string


  /**
  * The class TraceInterface implements the interface defined by the ComponentBase class.
  *
  */
  class TraceInterface
  {
  public:

    /**
    * Constructor for explicit creation of the Trace object by any component.
    *
    * @param[in] nameTrc - Name of the Trace interface
    * @param[in] lev - Level of the trace created
    * @param[in] helpStr - Default help string
    */
    TraceInterface(const char_t* const nameTrc, const uint8_t lev, const char_t* const helpStr);

    /**
    * Returns the pointer to help array of Trace object
    *
    * @return - Returns the help string for trace
    */
    const char_t* traceHelp() const;

    /**
    * Returns the pointer to the Name of Trace object.
    *
    * @return - Returns the Trace Name
    */
    const char_t* trcName() const;

    /**
    * Sets the trace details i.e. - level and enable values.
    *
    * @param[in] lev - Level of trace to be set
    * @param[in] enable - Trace to be enabled/disabled
    */
    void setTraceDetails(uint8_t lev, bool enable);

    /**
    * Gets the trace details i.e. - level and isEnabled values.
    *
    * @param[out] lev - Current level of trace
    * @param[out] isEnabled - Current enable of trace
    */
    void getTraceDetails(uint8_t &lev, bool &isEnabled) const;

    /**
    * Write string for the level in Trace
    *
    * @param[in] lev -     level for the trace write
    * @param[in] str -     string to be written via trace
    * @param[in] newLine - bool value to denote if a newline is to be appended after Logging
    */
    void write(const uint8_t lev, const char_t* const str, const bool newLine = true) const;

    /**
    * Overloaded write function for writing string with a uint32 value
    *
    * @param[in] lev -     level for the trace write
    * @param[in] str -     string to be written via trace
    * @param[in] val -     uint value to be written via trace
    * @param[in] newLine - bool value to denote if a newline is to be appended after Logging
    */
    void write(const uint8_t lev, const char_t* const str, const uint32_t val, const bool newLine = true) const;

    /**
    * Overloaded write function for writing string with a uint16 value
    *
    * @param[in] lev -     level for the trace write
    * @param[in] str -     string to be written via trace
    * @param[in] val -     int value to be written via trace
    * @param[in] newLine - bool value to denote if a newline is to be appended after Logging
    */
    void write(const uint8_t lev, const char_t* const str, const int32_t val, const bool newLine = true) const;

  protected:

  private:

    /**
    * Default Constructor
    *
    */
    TraceInterface();

    /**
    * The level of trace(1-9)
    *
    **/
    uint8_t level;

    /**
    * Name of the Trace
    *
    **/
    char_t traceName[maxTraceNameLen];

    /**
    * Data to be echoed when asked about help
    *
    **/
    char_t help[maxHelpLen];

    /**
    * Is trace Enbaled or not
    *
    **/
    bool traceEnabled;

  };
}

#endif
