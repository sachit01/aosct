#ifndef LogHandler_hpp
#define LogHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The Log handler is the BHP adaptation of the abstract Log handler class. The main purpose at 
*  this time is to instantiate the abstract log handler for use within the ATP user process.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-04    akushwah    Created
* 2016-07-18    akushwah    Updated after review
* 2016-08-09    akushwah    Initial Implementation
* 2016-09-19    akushwah    Corrected Init function
* 2016-11-03    adgupta     Updated after Log Handler Redesign
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_log_handler.hpp"
#include "atc_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  /**
  * The class LogHandler instantiates the abstract class and implements 
  * the interfaces needed for both inherited classes and component.
  *
  */
  class LogHandler : public ATC::AbstractLogHandler
  {
  public:

    static const InterfaceId Ifc_LCS  = Ifc_NumOfCoreIds;       //!< Identifies an RU log as an LCS message
    static const InterfaceId Ifc_OBRD = Ifc_NumOfCoreIds + 1U;  //!< Identifies an RU log as an OBRD message

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static LogHandler& instance(void);

  protected:

    /**
    * Adds the application Id, as a string, to the given buffer.
    *
    * @param[out] buffer  The destination buffer
    */
    virtual void addApplicationIdToBuffer(LineBuffer& buffer) const;

    /**
    * Adds the position and speed of the train, as a string, to the given buffer.
    *
    * @param[out] buffer  The destination buffer
    */
    virtual void addPositionAndSpeedToBuffer(LineBuffer& buffer) const;

    /**
    * Translate interface Id to a string
    *
    * @param[in]  ifc  Id of interface
    *
    * @return the interface name for the given interface id.
    */
    virtual const char_t* interfaceIdToString(InterfaceId const ifc);

  private:

    /**
    * Maximum Size of TCC Buffer to store Event Code
    */
    static const uint16_t maximumTCCBufferSize = 15U;

    /** 
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    LogHandler();

    /** 
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    LogHandler(const LogHandler&);

    /** 
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    LogHandler& operator = (const LogHandler&);

    /**
    * writeToBDS()
    * This Function will write the text and data to to BDS which are stored in the buffer.
    *
    * @param[in] str  combined String of text and Value after concatenation.
    *
    */
    void writeToBDS(char_t * const str);
  };
}
#endif
