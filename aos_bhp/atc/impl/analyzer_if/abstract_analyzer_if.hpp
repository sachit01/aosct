#ifndef AbstractAnalyzerIF_hpp
#define AbstractAnalyzerIF_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The AOS AnalyzerIF -component shall provide the following functionality:
*   - Report information about the data unit, available measurements data etc. To AOS Analyzer.
*   - External Interface(registerMeasurement– methods) that can be called by any component to
*     register measurement variables available for the AOS Analyzer
*   - Accept START-command from AOS Analyzer to start measurement
*   - When measurement started, report the registered measurement values to the AOS Analyzer
*     on every 10 cycles
*   - Accept STOP-command from AOS Analyzer to stop measurement
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-14    saprasad    Created
* 2017-01-10    saprasad    Added function prototype for AOS Analyzer Interface
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_event_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  class AbstractAnalyzerIF;
  /**
  * Static variable to store the single instance of AbstractAnalyserIF
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by corePtr() and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractAnalyzerIF* coreAnalyserIFInstancePtr = static_cast<AbstractAnalyzerIF*>(NULL);

  /**
  * The class AbstractAnalyserIF implements the interface defined by the ProcComponent class.
  *
  */
  class AbstractAnalyzerIF : public ProcComponent
  {

  public:
    /**
    * Interface that allows AbstractAnalyzerIF to read an enum value from its client.
    *
    * The client must derive and instantiate a concrete class from this and register
    * the instantiated object using @ref registerMeasurement.
    */
    class EnumValueGetter
    {
    public:
      /**
      * Returns the value to be measured.
      * @return the value to be measured.
      */
      virtual uint32_t getValue() const = 0;
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
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] measuredValue  Pointer will give bool value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const bool *const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] measuredValue  Pointer will give uint8_t value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const uint8_t minValue, const uint8_t maxValue, const uint8_t * const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] measuredValue  Pointer will give int16_t value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const int16_t minValue, const int16_t maxValue, const int16_t * const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] measuredValue  Pointer will give uint16_t value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const uint16_t minValue, const uint16_t maxValue, const uint16_t *const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] measuredValue  Pointer will give int32_t value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const int32_t minValue, const int32_t maxValue, const int32_t* const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] measuredValue  Pointer will give uint32_t value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const uint32_t minValue, const uint32_t maxValue, const uint32_t * const measuredValue);

    /**
    * registerMeasurement - External interface to register measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data(e.g "Speed")
    * @param[in] descr          Text describing the measurement data(e.g "Current vehicle speed")
    * @param[in] unit           Text telling which unit the value of this measurement data has (e.g "cm/s")
    * @param[in] minValue       Minimum value of measurement data(e.g speed 0)
    * @param[in] maxValue       Maximum value of measurement data (e.g speed 1000)
    * @param[in] valueGetter    Will retrieve enumeration value of measurement data
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      const uint32_t minValue, const uint32_t maxValue, const EnumValueGetter* valueGetter);

    /**
    * consoleCall - This will print all the analyzer measurement data available with AIF with its current values
    *
    * @param[in] argc  Number of arguments in the argument array argv
    * @param[in] argv  Arguments array
    *
    * @return true if the Call is successful.
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);

    /**
    * Get core instance pointer
    *
    * @return Pointer to single instance core object.
    */
    static AbstractAnalyzerIF* corePtr();

  protected:

    /**
    * Default Constructor
    */
    AbstractAnalyzerIF();

    /**
    * maxInputBufferLen : Maximum size of input buffer for reading command(START/STOP)
    * from AOS Analyzer using BasicIP
    *
    */
    static const uint8_t  maxInputBufferLen = 20U;

    /**
    * maxOutputBufferLen : Maximum size of output buffer for writing measurement data to AOS Analyzer using BasicIP
    *
    */
    static const uint16_t maxOutputBufferLen = 4096U;

    /**
    * inputBuffer : Buffer to be used while reading command (START/STOP) from AOS Analyzer using Basic IP
    *
    */
    //lint -e{1960} Using a union for this eliminates a lot of pointer casts
    typedef union
    {
      char_t charBuffer[maxInputBufferLen];
      uint8_t byteBuffer[maxInputBufferLen];
    } InputBuffer;

    /**
    * outputBuffer : Buffer to used to write AOS Analyzer unit/measurable data to AOS Analyzer using Basic IP
    *
    */
    //lint -e{1960} Using a union for this eliminates a lot of pointer casts
    typedef union
    {
      char_t charBuffer[maxOutputBufferLen];
      uint8_t byteBuffer[maxOutputBufferLen];
    } OutputBuffer;

    /**
    * maxTextLen : The maximum length of the text describing the Analyzer Unit or measurable start/end message.
    *
    */
    static const uint8_t maxTextLen = 20U;

    /**
    * writeToAOSAnalyzer - Write outputBuffer data to AOS analyzer
    */
    void writeToAOSAnalyzer(const OutputBuffer& outputBuffer, const uint32_t lenOfStrToAppend) const;

    /**
    * writeAIFUnitData - Write the Analyzer IF unit data to AOS Analyzer
    * The unit data describes the unit by name, version and protocol version.
    * After the start message all data is sent separated by line break until
    * the end message is sent.It shall be implemented in ATP/ATO
    * @param[in] void
    */
    virtual void writeAIFUnitData() const = 0;

    /** getConnectionID : Access function to get Analyzer connection ID.It shall be defined by
    *   ATP/ATO
    * @param[in] void
    * @return the connection ID for Analyzer IF
    */
    virtual uint8_t getConnectionID() const = 0;

  private:

    /**
    * initDone : Flag to prevent multiple initialization.
    */
    bool initDone;

    /**
    * Enum describes Analyzer IF states
    *
    */
    enum AIFStates
    {
      AIFNotConnected,  //!< AIF is not connected 
      AIFConnected,     //!< AIF is connected 
      AIFMeasuring      //!< AIF is measuring register measurement data
    };

    /**
    * aifState : variable which tells current state of Analyzer IF
    *            when AIF is connected with AOS Analyzer
    */
    AIFStates aifState;

    /**
    * union describes current Analyzer IF Minimum Value and Maximum Value
    *
    */
    //lint -e{1960} Using a union for this eliminates a lot of pointer casts
    typedef union 
    {
      int32_t  valueS;  //!< Signed integer (up to 32 bits)
      uint32_t valueU;  //!< Unsigned integer (up to 32 bits)
    } ValueRange;

    //lint -e{1960} Using a union for this eliminates a lot of pointer casts
    typedef union 
    {
      const void*     measureValue;       //!< Points to measurement data as void pointer
      const bool*     measureValueBool;   //!< Points to measurement data as Bool pointer
      const uint8_t*  measureValueU8;     //!< Points to measurement data as MeasureByte pointer
      const uint16_t* measureValueU16;    //!< Points to measurement data as MeasureWord pointer
      const uint32_t* measureValueU32;    //!< Points to measurement data as MeasureDoubleWord pointer
      const int16_t*  measureValueS16;    //!< Points to measurement data as MeasureSWord pointer
      const int32_t*  measureValueS32;    //!< Points to measurement data as MeasureSDoubleWord pointer
      const EnumValueGetter* valueGetter; //!< Points to a getter for MeasureEnum pointer
    } MeasureValuePtr;

    /**
    * maxMeasureDataList : The maximum number of Measurement Data allowed in AIF
    *
    */
    static const uint8_t maxMeasureDataList = 50U;

    /**
    * MeasurementTypeEnum : It will store the type of data in measureList ,maintain type of incoming
    * register measurement data
    */
    enum MeasurementTypeEnum
    {
      MeasureBool,         //!< Boolean
      MeasureByte,         //!< Unsigned 8 bit int 
      MeasureWord,         //!< Unsigned 16 bit int
      MeasureSWord,        //!< Signed 16 bit int  
      MeasureDoubleWord,   //!< UnSigned 32 bit int 
      MeasureSDoubleWord,  //!< Signed 32 bit int 
      MeasureEnum          //!< Enumeration
    };

    /***
    * MeasurementDataList : structure holding registered variables
    */
    struct MeasurementDataList
    {
      const char_t  *name;             //!< Name of measurement data
      const char_t  *descr;            //!< describing the measurement data
      const char_t  *unit;             //!< unit of measurement data
      ValueRange minValue;             //!< Minimum value of measurement data
      ValueRange maxValue;             //!< Maximum value of measurement data
      MeasureValuePtr measureValue;    //!< Points to measurement data
      MeasurementTypeEnum  type;       //!< The type of the measured data
      bool isSignedValue;              //!< Flag to indicate if value is signed or not
    };

    /**
    * measureList : MeasurementDataList structure variable
    *
    */
    MeasurementDataList measureList[maxMeasureDataList];

    /**
    * measureListCounter : Maintain counter value for measureList
    *
    */
    uint8_t measListCounter;

    /**
    * aifSendCurCycleCount : Maintain current cycle count for AIF Measurable data when write to AOS analyzer
    *
    */
    uint8_t aifSendCurCycleCount;

    /**
    * startMeasTimestamp : Start the timer using VFW API when measure started ("START") from AOS Analyzer
    *
    */
    int64_t startMeasTimestamp;

    /**
    * writeAIFMeasurableData - Write the Analyzer IF measurable data to AOS Analyzer
    * The measurable is the variables, the unit will report during measurement. After
    * the start message all data is sent separated by line break until the end message
    * is sent.
    *  @param[in] void
    */
    void writeAIFMeasurableList() const;

    /**
    * writeAIFParameterList - Write the starting and ending message to AOS Analyzer
    * @param[in] void
    */
    void writeAIFParameterList() const;

    /**
    * readAIFCmd - Read the AOS Analyzer command(START/STOP).
    * @param[out] aifCommand Content the type of command from AOS Analyzer
    */
    uint32_t readAIFCmd(char_t  aifCommand[]) const;

    /**
    * registerMeasurement - internal function to handler common mechanism for registering measurement data
    *
    * @param[in] nameOfMeasData Name of measurement data
    * @param[in] descr          Text describing the measurement data
    * @param[in] unit           Text telling which unit the value of this measurement data has
    * @param[in] measureValue   Pointer to the measurement variable
    * @param[in] type           MeasurementTypeEnum to specify the data type of measurement variable
    * @param[in] min            Minimum value of measurement variable
    * @param[in] max            Maximum value of measurement variable
    * @param[in] isSigned       Flag to indicate if type is signed
    * @return true if registration succeeded otherwise false
    */
    bool registerMeasurementInternal(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
      MeasureValuePtr const measureValue, MeasurementTypeEnum type, const ValueRange minValue, const ValueRange maxValue, const bool isSigned);

    /**
    * sendAIFSampleMeasData - Send the current value of Analyzer IF measurable data to AOS analyzer
    * @param[currentTimestamp]    Current time stamp
    */
    void sendAIFSampleMeasData(int64_t currentTimestamp);
  };

}
#endif
