#ifndef AnalyzerIF_hpp
#define AnalyzerIF_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the AnalyzerIF class
* which contains the adapted functionality of the AnalyzerIF
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-14   saprasad     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_analyzer_if.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{

  /**
  * The class AnalyzerIF instantiate the abstract class and implements
  * the interfaces needed for both inherited classes and component.
  *
  */
  class AnalyzerIF : public ATC::AbstractAnalyzerIF
  {
  public:
    
    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    * @return the one and only instance.
    *
    * NOTE: Singleton handling shall only be used in Adaptation, not Core!
    */
    static AnalyzerIF& instance();

  protected:

    /**
    * This function creates and send the UnitData message consisting of application name, version and protocol version.
    * The message is then sent to AOS analyzer.
    *
    * @return void
    */
    virtual void writeAIFUnitData() const;

    /**
    * Implements the getConnectionID function.
    * @return the connection ID for Analyzer IF
    */
    virtual uint8_t getConnectionID() const;

  private:

    /**
    * protocolVersionAIF : Protocol version information of AOS Analyzer IF
    *
    */
    static const uint8_t protocolVersionAIF = 1U;

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    AnalyzerIF();

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    AnalyzerIF(const AnalyzerIF&);

    /** <
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    AnalyzerIF& operator = (const AnalyzerIF&);
  };

}
#endif
