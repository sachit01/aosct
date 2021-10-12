#ifndef AbstractLogHandler_hpp
#define AbstractLogHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  The LogHandler-component shall provide the following:
*  . Provide generic interface for distributed logging to allow components to call LogHandler to log information.
*  . Forward logged information to different storage units based on Config parameter settings and/or type of information.
*  . Provide buffering mechanisms of some logged information to optimize performance.
*  . Provide any buffering mechanisms necessary because of the design of the interface to the destination storage.
*  . Console commands to show statistics, set/show log-levels
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-07-07    akushwah    Created
* 2016-07-18    akushwah    Updated after review 
* 2016-08-09    akushwah    Initial Implementation
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-20    akushwah    Added the addHeaderToNJRUBuffer function
* 2016-10-25    nsyed       Change line parameter from uint32_t to int32_t
* 2016-11-03    adgupta     Updated after Log Handler Redesign
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_application_base.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATC
{
  static const uint32_t maxJRUBufferSize    = 100U;    //!< Maximum size of the JRU buffer

  /** In Signed Int32 Value, Max number of digits when represented as string including null termination */
  static const int8_t sizeOfDigitsInSignedValue = 12;

  /** In Unsigned Int32 Value, Max number of digits when represented as string including null termination */
  static const uint8_t sizeOfDigitsInUnSignedValue = 11U;

  /** Size of Null Termination */
  static const uint8_t nullTerminationLength = 1U;

  /** Maximum size of the N-JRU buffer */
  static const uint32_t maxNJRUBufferSize = 53248U;

  /** Maximum size of the RU buffer */
  static const uint32_t maxRUBufferSize = 32768U;

  /** Maximum number of (binary) bytes to send in a fragment, to RU */
  static const uint32_t maxRUFragmentSize = 450U;

  /** Detailed Level Value */
  static const uint8_t detailedLevel = 2U;

  class AbstractLogHandler;
  /** 
  * Static variable to store the single instance of AbstractLogHandler
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only 
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractLogHandler* coreLogHandlerInstancePtr = static_cast<AbstractLogHandler*>(NULL);

  /**
  * The class AbstractLogHandler implements the interface defined by the ComponentBase class.
  *
  */
  class AbstractLogHandler : public ProcComponent
  {
  public:

    /**
    * Enumerates the interfaces supported by \ref logRU.
    */
    typedef uint8_t InterfaceId;

    static const InterfaceId Ifc_DMI          = 0U;  //!< Identifies an RU log as a DMI message
    static const InterfaceId Ifc_IO           = 1U;  //!< Identifies an RU log as an IO signal
    static const InterfaceId Ifc_NumOfCoreIds = 2U;  //!< The number of RU interfaces supported in ATC

    /**
    * Specifies whether the log data refers to input or output, see \ref logRU.
    */
    enum InterfaceDir
    {
      Ifc_In,  //!< Identifies an RU log as an input message or signal
      Ifc_Out  //!< Identifies an RU log as an output message or signal
    };

    /**
    * Implements the virtual init function.
    *
    * @return Returns true when initialization completed
    */
    virtual bool init(void);

    /**
    * Implements the virtual run function.
    * 
    */
    virtual void run(void);

    /**
    * Logs the given event to N-JRU (and to BDS, if \ref bdsLevel is at least "detailedLevel").
    *
    * @param[in] eventToLog Reference to the event being logged.
    * @param[in] filepath   Pointer to the path of the file from where the event is logged.
    * @param[in] line       Line number from where the event is logged.
    *
    */
    void writeEventToLogger(const Event& eventToLog, const char_t* const filepath, int32_t const line);

    /**
    * Logs the given text string to N-JRU (if the given level is at most \ref njruLevel)
    * and to BDS (if the given level is at most \ref bdsLevel).
    *
    * @param[in] level         Level at which the logging will be done.
    * @param[in] text          Pointer to the text, which needs to be logged.
    * @param[in] compName      Pointer to component name text.
    * @param[in] filepath      Pointer to the path of the file from where this is logged.
    * @param[in] line          Line number from where this is logged.
    *
    */
    void writeToLog(LogLevel const level, const char_t* const text, const char_t* const compName,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0);

    /**
    * Logs the given text string and integer value to N-JRU (if the given level is at most \ref njruLevel)
    * and to BDS (if the given level is at most \ref bdsLevel).
    *
    * @param[in] level         Level at which the logging will be done.
    * @param[in] text          Pointer to the text,which needs to be logged.
    * @param[in] val           value(unsigned integer) to be logged along with Text
    * @param[in] compName      Pointer to component name text.
    * @param[in] filepath      Pointer to the path of the file from where this is logged.
    * @param[in] line          Line number from where this is logged.
    *
    */
    void writeToLog(LogLevel const level, const char_t* const text, const uint32_t val, const char_t* const compName,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0);

    /**
    * Logs the given text string and integer value to N-JRU (if the given level is at most \ref njruLevel)
    * and to BDS (if the given level is at most \ref bdsLevel).
    *
    * @param[in] level         Level at which the logging will be done.
    * @param[in] text          Pointer to the text,which needs to be logged.
    * @param[in] val           value(signed integer) to be logged along with text.
    * @param[in] compName      Pointer to component name text.
    * @param[in] filepath      Pointer to the path of the file from where this is logged.
    * @param[in] line          Line number from where this is logged.
    *
    */
    void writeToLog(LogLevel const level, const char_t* const text, const int32_t val, const char_t* const compName,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0);

    /**
    * Logs the given text string and boolean value to N-JRU (if the given level is at most \ref njruLevel)
    * and to BDS (if the given level is at most \ref bdsLevel).
    *
    * @param[in] level         Level at which the logging will be done.
    * @param[in] text          Pointer to the text,which needs to be logged.
    * @param[in] val           value(boolean) to be logged along with text.
    * @param[in] compName      Pointer to component name text.
    * @param[in] filepath      Pointer to the path of the file from where this is logged.
    * @param[in] line          Line number from where this is logged.
    *
    */
    void writeToLog(LogLevel const level, const char_t* const text, const bool val, const char_t* const compName,
      const char_t* const filepath = static_cast<const char_t* const>(NULL), int32_t const line = 0);

    /**
    * Logs a message in binary format to RU.
    *
    * @param[in] ifc      Identifies the interface through which the message passed.
    * @param[in] dir      Specifies input or output.
    * @param[in] message  Pointer to the first byte of the message to log.
    * @param[in] length   The length (in bytes) of the message to log.
    */
    void logRU(InterfaceId const ifc, const InterfaceDir dir, const uint8_t message[], uint32_t const length);

    /**
    * Logs a digital I/O value to RU.
    *
    * @param[in] ifc           Identifies the interface through which the message passed.
    * @param[in] dir           Specifies input or output.
    * @param[in] channelId     The channel on which the value was read/written.
    * @param[in] digitalValue  The value to log.
    */
    void logRU(InterfaceId const ifc, const InterfaceDir dir, uint8_t const channelId, bool const digitalValue);

    /**
    * Logs an analog I/O value to RU.
    *
    * @param[in] ifc           Identifies the interface through which the message passed.
    * @param[in] dir           Specifies input or output.
    * @param[in] channelId     The channel on which the value was read/written.
    * @param[in] analogValue   The value to log.
    */
    void logRU(InterfaceId const ifc, const InterfaceDir dir, uint8_t const channelId, uint16_t const analogValue);

    /**
    * Handles the console commands defined for this component.
    *
    * @param[in] argc  Number of arguments in the argument array argv 
    * @param[in] argv  Arguments array 
    *
    * @return true if the console command was handled completely
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractLogHandler* corePtr();

  protected:

    /**
    * Implements a character buffer.
    */
    template <uint32_t maxSize> class CharBuffer
    {
    public:

      /**
      * Constructs and initializes the buffer to be empty.
      */
      CharBuffer();

      /**
      * Initializes the buffer to be empty.
      */
      void init();

      /**
      * Appends the given text to the internal buffer.
      *
      * @param[in] textToAppend  the text to be appended to the internal buffer.
      *
      * @return true if and only if the text was added to the RU buffer.
      */
      bool appendToBuffer(const char_t* const textToAppend);

      /**
      * Returns the number of characters in the internal buffer.
      *
      * @return the number of characters in the internal buffer.
      */
      uint32_t size() const;

      /**
      * Returns a pointer to the internal buffer.
      *
      * @return a pointer to the internal buffer.
      */
      char_t* buffer();

      /**
      * Returns a pointer to the internal buffer.
      *
      * @return a pointer to the internal buffer.
      */
      const char_t* buffer() const;

    private:

      /**
      * Current Size of the buffer
      */
      uint32_t bufferSize_;

      /**
      * Buffer to store data in form of text
      *
      * Note: The size of njru/ru buffers needs to be defined in the later phase of project development
      */
      char_t buffer_[maxSize];
    };

    /**
    * Implements a circular character buffer with support for transmitting the buffered characters
    * using BasicIP.
    */
    template <uint32_t maxSize> class CircularCharBuffer
    {
    public:

      /**
      * Constructs and initializes the buffer to be empty.
      */
      CircularCharBuffer();

      /**
      * Initializes the buffer to be empty.
      */
      void init();

      /**
      * Appends the given text to the internal buffer.
      *
      * @param[in] textToAppend  the text to be appended to the internal buffer.
      *
      * @return true if and only if the text was added to the RU buffer.
      */
      void appendToBuffer(const char_t* const textToAppend);

      /**
      * Transmits the buffered characters on the given BasicIP connection.
      *
      * @param[in] connectionId  the connection on which to transmit.
      */
      void writeBufferTo(const uint8_t connectionId);

      /**
      * Returns the number of used characters in the internal buffer.
      *
      * @return the number of used characters in the internal buffer.
      */
      uint32_t size() const;

      /**
      * Returns the number of unused characters in the internal buffer.
      *
      * @return the number of unused characters in the internal buffer.
      */
      uint32_t available() const;

    private:

      /**
      * The position in the buffer to write to
      */
      uint32_t posToWrite_;

      /**
      * The position in the buffer to read from
      */
      uint32_t posToRead_;

      /**
      * Circular buffer to store data in form of text
      *
      * Note: The size of njru/ru buffers needs to be defined in the later phase of project development
      */
      char_t buffer_[maxSize];
    };

    /**
    * Used as a local buffer to build lines of text.
    */
    typedef CharBuffer<256U> LineBuffer;

    /**
    * Constructor
    */
    AbstractLogHandler();

    /**
    * This function will add the N-JRU header to the given buffer in text format.
    *
    * @param[in]  compName      The name of the component that the message relates to.
    * @param[in]  eventTypeStr  A printable string corresponding with the event type
    * @param[out] buffer        The destination buffer.
    *
    * @return True if and only if the entire header was written to the buffer.
    */
    bool addNJRUHeaderToBuffer(const char_t* const compName, const char_t* const eventTypeStr, LineBuffer& buffer) const;

    /**
    * Adds the application Id, as a string, to the given buffer.
    *
    * @param[out] buffer  The destination buffer
    */
    virtual void addApplicationIdToBuffer(LineBuffer& buffer) const = 0;

    /**
    * If available, adds the position and speed of the train, as a string, to the given buffer.
    *
    * @param[out] buffer  The destination buffer
    */
    virtual void addPositionAndSpeedToBuffer(LineBuffer& buffer) const;

    /**
    * Returns the interface name for the given interface constant.
    *
    * @param[in] ifc  the interface for which the interface name is returned.
    *
    * @return the interface name for the given interface constant.
    */
    virtual const char_t* interfaceIdToString(InterfaceId const ifc);

    /**
    * This function will add the header to the RU Buffer in text format.
    *
    * @param[in] ifc           the interface from which the data is being logged.
    * @param[in] dir           indicates whether the data is sent or received.
    * @param[in] fragmentType  'S'=single fragment, 'F'=first fragment, 'C'=continuation,
    *                          'L'=last fragment
    *
    * @return true if and only if the whole header was added to the RU buffer.
    */
    bool addHeaderToRUBuffer(InterfaceId const ifc, InterfaceDir const dir, char_t const fragmentType);

    /**
    * Adds a message fragment to the RU buffer. The fragmentType parameter indicates whether
    * this is the only fragment of the message, the first fragment, the last fragment or a
    * fragment in between the first and last ones.
    *
    * @param[in] ifc           The interface from which the data is being logged.
    * @param[in] dir           Indicates whether the data is sent or received.
    * @param[in] fragment      Pointer to the first byte of the fragment to log.
    * @param[in] length        The length (in bytes) of the fragment to log.
    * @param[in] fragmentType  'S'=single fragment, 'F'=first fragment, 'C'=continuation,
    *                          'L'=last fragment.
    *
    * @return true if and only if the whole fragment was added to the RU buffer.
    */
    bool logFragmentToRU(
      InterfaceId const ifc, InterfaceDir const dir, const uint8_t fragment[], uint32_t const length,
      char_t const fragmentType);

    /**
    * Translate side to a printable character
    *
    * @return the character corresponding with which side the code executes on
    */
    char_t getSideChar() const;

    /**
    * Internal helper method to write data to NJRU/BDS
    *
    * @param[in] lineBuffer    Data to be written to NJRU/BDS
    * @param[in] filepath      Pointer to the path of the file from where this is logged.
    * @param[in] line          Line number from where this is logged.
    * @param[in] logToBDS      True if log to BDS is wanted
    *
    */
    virtual void writeToLogInternal(LineBuffer& lineBuffer, const char_t* const filepath, const int32_t line, const bool logToBDS);

    /**
    * writeToBDS()
    * This Function will write the text and data to BDS which are stored in the buffer.
    *
    * @param[in] str  combined String of text and Value after concatenation.
    *
    */
    virtual void writeToBDS(char_t * const str);

  private:

    /**
    * N-JRU Buffer to store data in form of text
    *
    * Note: The size of njru Buffer needs to be defined in the later phase of project development
    */
    CircularCharBuffer<maxNJRUBufferSize> njruBuffer;

    /**
    * RU Buffer to store data in form of text
    *
    * Note: The size of ru Buffer needs to be defined in the later phase of project development
    */
    CircularCharBuffer<maxRUBufferSize> ruBuffer;

    /**
    * Get the Component short name from the Id as input.
    *
    * @param[in] id - Id of the component whose short name is needed
    *
    * @return - pointer to the short name of the component.
    */
    const char_t* getCompShortName(const ComponentIDType id) const;

    /**
    * Get a pointer to a string corresponding with an eventType
    *
    * @param[in] eventType
    *
    * @return pointer to a string describing the eventType
    */
    const char_t* getEventTypeStr(const Event::EventType eventType) const;

    /**
    * Extract only the file name out of the string having complete path of the file.
    *
    * @param[in] str     the string in which the character is to be searched
    * @param[in] length  the max expected length of the string
    *
    * @return   Pointer to the string after the slash found in the string
    *           NULL if the no slash is found
    */
    const char_t* extractFileName(const char_t* const str, const size_t length) const;

    /**
    * maximum Message Separator Size including Null Character
    */
    static const uint8_t maxMsgSeparatorSize = 5U;


    /**
    * Flag to prevent multiple initialization.
    */
    bool initDone;

    /**
    * Flag to prevent multiple initialization.
    */
    bool initNjruConnectionDone;

#ifndef _DISPATCHER
    /**
    * Flag to prevent multiple initialization.
    */
    bool initRuConnectionDone;
#endif

    /**
    * Flag to prevent multiple initialization.
    */
    bool initBdsDone;
    
    /** 
    * Message Separator
    */ 
    char_t msgSeparator[maxMsgSeparatorSize];

    /** 
    * Length of Message Separator
    */ 
    uint8_t msgSeparatorLength;

    /**
    * Number of messages sent to N-JRU
    */
    uint32_t messageSentNjruCounter;

    /**
    * Number of messages truncated or not sent to N-JRU
    */
    uint32_t messageNotSentNjruCounter;

    /**
    * Number of messages sent to RU
    */
    uint32_t messageSentRuCounter;

    /**
    * Number of messages truncated or not sent to RU
    */
    uint32_t messageNotSentRuCounter;

    /**
    * Number of messages sent to BDS
    */
    uint32_t messageSentBdsCounter;

    /**
    * Number of messages truncated or not sent to BDS
    */
    uint32_t messageNotSentBdsCounter;

    /**
    * BDS Sequence Number to detect the missing messages.
    */
    uint8_t bdsSeqNumber;

    /**
    * N-JRU Level
    */
    uint8_t njruLevel;
    
    /** 
    * BDS log level
    */
    uint8_t bdsLevel;

    /**
    * writeToNJRU() 
    * This Function will write the text and data to NJRU which are stored in the buffer  
    *
    */
    void writeToNJRU();

    /**
    * writeToRU()
    * This Function will write the text and data to RU which are stored in the buffer
    *
    */
    void writeToRU();

    /**
    * Checks the String passed is Numeric or not 
    *
    * @param[in] logLevelStr - String which needs to be checked for Numeric Vaslue
    *
    * @return return true, if String is passed is numeric and single digit.
    *
    */
    bool isLegalLogLevel(const char_t logLevelStr[]) const;

  };
}

#endif
