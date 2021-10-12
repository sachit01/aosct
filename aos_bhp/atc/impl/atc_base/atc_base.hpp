#ifndef ATCBase_hpp
#define ATCBase_hpp
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
*  Defines base-classes for the all components within ATC/ATP/ATO.
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-15    bhermans    Created
* 2016-04-19    lantback    Reworked for a combined platform approach
* 2016-04-22    lantback    Added component id to constructors
* 2016-04-26    lantback    Added input argument spec's
* 2016-06-07    akushwah    updated input arguments to remove Linting Errors
* 2016-07-06    adgupta     Added Trace object in the Base component
* 2016-07-12    adgupta     Implemented Trace and virtual console call
* 2016-09-13    bhermans    void argc,argv to reduce warnings
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstring>
#include "atc_types.hpp"
#include "trace_interface.hpp"
#include <gp_list.hpp>

/******************************************************************************
* DECLARATIONS    
******************************************************************************/

namespace ATC
{

  /** Max component name string length */
  static const uint32_t  maxComponentNameLength = 30U;

  /** Max component short name string length */
  static const uint32_t  maxComponentShortNameLength = 7U;   


  /**
  * The class BaseComponent details the Core interface that an component need to implement.
  */
  class BaseComponent
  {

  public:
    /**
    * Virtual destructor
    * 
    * Implement destructor also in derived class to make sure that the derived destructor is called when 
    * an object inherited from BaseComponent, but pointed to by a base-class ptr, is destroyed.
    *
    * @remarks Shall be implemented in the derived class if additional resources need to be released by the derived destructor.
    * Only important in the simulated PC environment. (In the target environment the system will be restarted anyway)
    */
    virtual ~BaseComponent(void){};


    /**
    * Component initialization.
    * 
    * Called by the application at init.
    *
    * @return Return true when initialization done.
    *
    * @remarks Must be implemented in the derived class.
    */
    virtual bool init(void) = 0;

    /**
    * Get component id.
    *
    * @return Component id number.
    */
    ComponentIDType getId() const;

    /**
    * Get component name.
    *
    * @return Component name.
    */
    const char_t* getName() const;

    /**
    * Get component short name.
    *
    * @return Component short name.
    */
    const char_t* getShortName() const;

    /**
    * Get component trace.
    *
    * @return Component trace.
    */
    TraceInterface* getTrace();

    /**
    * Console Call
    *
    * The virtual function implementing Console Call for Console component.
    * This returns false unless handled by individual component.
    *
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    /**
    * Pre Initialization.
    *
    * This function is called before the vfwStart. It is responsible to setup the
    * vfw objects before the main loop starts executing. The function is called once
    * for each component and should not depend on any other component.
    *
    */
    virtual void preInit();

  protected:
    /**
    * Constructor
    *
    * The BaseComponent constructor is hidden as protected to avoid creating objects of the base 
    * class.
    *
    * @param[in]    id            Identity number of component.
    * @param[in]    compName      Text string with name of component
    * @param[in]    shortCompName Text string with short name of component
    */
    BaseComponent(const ComponentIDType id, const char_t* const compName, const char_t* const shortCompName);

    /**
    * writeToLog
    *
    * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
    *
    * @param[in]    level         The Log level for this to be logged.
    * @param[in]    text          The text log to be logged.
    * @param[in]    filepath      Pointer to the path of the file from where this is logged.
    * @param[in]    line          Line number from where this is logged.
    */
    void writeToLog(LogLevel const level, const char_t* const text,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), const int32_t line = 0) const;

    /**
    * writeToLog
    *
    * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
    *
    * @param[in]    level         The Log level for this to be logged.
    * @param[in]    text          The text log to be logged.
    * @param[in]    val           The value(unsigned integer) to be logged.
    * @param[in]    filepath      Pointer to the path of the file from where this is logged.
    * @param[in]    line          Line number from where this is logged.
    */
    void writeToLog(LogLevel const level, const char_t* const text, const uint32_t val,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), const int32_t line = 0) const;

    /**
    * writeToLog
    *
    * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
    *
    * @param[in]    level         The Log level for this to be logged.
    * @param[in]    text          The text log to be logged.
    * @param[in]    val           The value(signed integer) to be logged.
    * @param[in]    filepath      Pointer to the path of the file from where this is logged.
    * @param[in]    line          Line number from where this is logged.
    */
    void writeToLog(LogLevel const level, const char_t* const text, const int32_t val,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), const int32_t line = 0) const;

    /**
    * writeToLog
    *
    * Writes the Log onto the External interfaces viz.- N-JRU and BDS.
    *
    * @param[in]    level         The Log level for this to be logged.
    * @param[in]    text          The text log to be logged.
    * @param[in]    val           The value(boolean) to be logged.
    * @param[in]    filepath      Pointer to the path of the file from where this is logged.
    * @param[in]    line          Line number from where this is logged.
    */
    void writeToLog(LogLevel const level, const char_t* const text, const bool val,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), const int32_t line = 0) const;

    /**
    * Trace object to log Trace on Console
    */
    TraceInterface trace;

  private:
    /**
    * Default Constructor
    * Declare constructor as private in order to prevent illegal use.
    *
    * @remarks Shall not be implemented!
    */
    BaseComponent();

    /** 
    * Declare copy-constructor as private in order to prevent illegal use.
    *
    * @remarks Shall not be implemented!
    */
    BaseComponent(const BaseComponent&);

    /** 
    * Declare assignment-operator as private in order to prevent illegal use.
    *
    * @remarks Shall not be implemented!
    */
    BaseComponent& operator = (const BaseComponent&);

    /**
    * Component id, derived class 
    */
    ComponentIDType   compId;

    /**
    * Component name, derived class 
    */
    char_t name[maxComponentNameLength];

    /**
    * Component short name, derived class 
    */
    char_t shortName[maxComponentShortNameLength];
  };

  /**
  * The class ProcComponent details the Core interface of an processing component. 
  *
  * The class ProcComponent is the base for what is defined as processing components. A processing
  * component performs all actions in one single execution call.
  *
  * The defined interface is used by Core scheduling.
  */
  class ProcComponent : public BaseComponent
  {
  public:
    /**
    * Component logic.
    *
    * Called periodically by the application
    * @remarks Must be implemented in the derived class.     
    */
    virtual void run(void) = 0;

  protected:
    /**
    * Constructor
    *
    * The ProcComponent constructor is hidden as protected to avoid creating objects of the base 
    * class.
    */
    ProcComponent(ComponentIDType id, const char_t* compName, const char_t* shortCompName);

  private:
    /**
    * Default Constructor
    * Declare constructor as private in order to prevent illegal use.
    */
    ProcComponent();

    /** 
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    ProcComponent(const ProcComponent&);

    /** 
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    ProcComponent& operator = (const ProcComponent&);
  };

  /**
  * The class IOComponent details the Core interface of an input/output component. 
  *
  * The class IOComponent is the base for what is defined as interface components. An interface
  * component specifies a two phase execution by use of runIn() / runOut() methods. The runIn() 
  * method shall perform data capture of new data on input devices. The runOut() method shall 
  * perform preparation of data to be sent to output devices.
  *
  * The defined interface is used by Core scheduling.
  */
  class IOComponent : public BaseComponent
  {
  public:

    /**
    * Component logic for providing input to other modules.
    *
    * Called periodically by the application
    * @remarks Must be implemented in the derived class.     
    */

    virtual void runIn(void) = 0;
    /**
    * Component logic for taking care of output from other modules.
    *
    * Called periodically by the application
    * @remarks Must be implemented in the derived class.     
    */
    virtual void runOut(void) = 0;

  protected:
    /**
    * Constructor
    *
    * The ProcComponent constructor is hidden as protected to avoid creating objects of the base 
    * class.
    */
    IOComponent(ComponentIDType id, const char_t* compName, const char_t* shortCompName);

  private:
    /**
    * Default Constructor
    * Declare constructor as private in order to prevent illegal use.
    */
    IOComponent();

    /** 
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    IOComponent(const IOComponent&);

    /** 
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    IOComponent& operator = (const IOComponent&);
  };
}

#endif
