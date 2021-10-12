#ifndef AbstractConsole_hpp
#define AbstractConsole_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This abstract class implements the core functionality of Console component.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-01    adgupta     Created
* 2016-07-09    adgupta     Implementation of Console functionality
* 2016-08-10    akushwah    Implementated isNumeric() And isSingleDigit()
* 2016-09-08    adgupta     Updated Console call for write() and to have headers
* 2016-10-06    adgupta     Updated(increased) outputbuffer size
* 2016-11-07    saprasad    Added the prototype for writeVersion(pure virtual function) 
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "abstract_application_base.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  /** Pointer to Trace object type declaration
  */
  typedef TraceInterface* TraceInterfacePtr; 

  /** The type of container used for the Trace Interface objects. TODO: Templates to be used.
  */
  typedef std::vector<TraceInterfacePtr> TracePtrVector;

  /** The type of iterator used for the Trace Interface objects. TODO: Templates to be used.
  */
  typedef std::vector<TraceInterfacePtr>::iterator TracePtrIterator;

  /** Maximum size of the output buffer (to hold largest printout('tsetup' of 350 cars)) */
  static const uint32_t maxOutputBufferSize         = 32768U;
  /** Maximum size of the input buffer */
  static const uint8_t  maxInputBufferSize          = 100U;

  /** Maximum size of the Trace list */
  static const uint8_t  maxTraceListCnt             = 100U;
  /** Maximum size of the Prompt */
  static const uint8_t  maxPromptSize               = 6U;


  class AbstractConsole;
  /** 
  * Static variable to store the single instance of AbstractConsole
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only 
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractConsole* coreConsoleInstancePtr = static_cast<AbstractConsole*>(NULL);


  /**
  * The class AbstractConsole implements the interface defined by the ComponentBase class.
  *
  */
  class AbstractConsole : public ProcComponent
  {
  public:
    /**
    * Implements the virtual init function.
    * (If needed)
    *
    * @return Returns true when initialization completed
    */
    virtual bool init(void);

    /**
    * Implements the virtual run function.
    * (If needed)
    */
    virtual void run(void);

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractConsole* corePtr();

    /**
    * Writes out the string to the Console Output
    *
    * @param[in] str - String to be written on the Console. This MUST be NULL terminated
    */
    void write(const char_t* const str);

    /**
    * Writes out the string to the Console Output with a newline character
    *
    * @param[in] str - String to be written on the Console. This MUST be NULL terminated
    */
    void writeWithNewline(const char_t* const str);
    

    /**
    * Add Trace object to the Trace object List
    *
    * @param[in] obj - object to be added to the object list of trace
    */
    void addTraceObj(TraceInterface* const obj);

    /**
    * This functions parses the arguments searches for the "help", "trace" or any other Console 
    * component specific command calls and handles it. Returns true if completeley handled 
    * else returns false. returning false will let other componets handle the call. help always returns false.
    *
    * @param[in] argc - Number of arguments in the argument array argv
    *
    * @param[in] argv - Arguments array
    *
    * @return - returns true if handled successfully, except "help"(it always returns false)
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);


  protected:
    /**
    * Constructor
    *
    * @param[in] promptInput - The prompt to be displayed on Console
    */
    AbstractConsole(const char_t* const promptInput);

    /**
    * get the iterator of the component to loop while calling consoleCall
    *
    * @return - Iterator to the component vector in ATP application
    */
    virtual ATC::CompPtrIter getComponentIter() = 0;

    /**
    * get the last iterator of the component to loop while calling consoleCall
    *
    * @return - Iterator to the end of component vector in ATP application
    */
    virtual ATC::CompPtrIter getComponentIterEnd() = 0;

    /**
    * writeVersion.
    * This function will display the ATP/ATO/Dispatcher version .
    * This function should be implemented in adaptation part.
    */
    virtual void writeVersion() = 0;

    /**
    * Get to know if the Console call handled is a Distributed Console Call.
    * i.e.- Does it require more than one Components to handle the call(like help).
    *
    * @return - Is the Console call Distributed or not
    */
    virtual bool isDistributedConsoleCall(const char_t* const callString);

  private:

    /**
    * Singleton instance.
    * Declare constructor as private in order to prevent illegal use.
    */
    AbstractConsole();

    /**
    * Buffer to used to write to the Console output via Basic IP
    *
    */    
    char_t outputBuffer[maxOutputBufferSize];

    /**
    * Buffer to be used while reading from console via Basic IP
    *
    */
    char_t inputBuffer[maxInputBufferSize];

    /**
    * List of trace objects from all the components
    *
    */
    TracePtrVector traceObjList;

    /**
    * Prompt to be displayed at console after connection and after each 
    * execution.
    *
    */
    char_t prompt[maxPromptSize];

    /**
    * Flag to prevent multiple initialization.
    */
    bool initDone;

    /**
    * Is console connected?
    */
    bool connected;

    /**
    * Output buffer count. How much is the buffer filled.
    */
    uint32_t outputBuffCount;

    /**
    * Writes the prompt to output buffer without a newline
    */
    void writePrompt();

  };
}

#endif
