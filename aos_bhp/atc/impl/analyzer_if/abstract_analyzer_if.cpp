/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file will implement all the functionality of  AOS Analyzer Interface.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-14    saprasad    Created
* 2017-01-06    saprasad    Implementation of AOS Analyzer IF functionality
* 2017-01-10    saprasad    Linting and Testing of AOS Analyzer IF functionality
* 2017-01-18    saprasad    Incorparated review comments
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "atc_base.hpp"
#include "abstract_basic_ip.hpp"
#include "abstract_console.hpp"
#include "abstract_config_base.hpp"
#include "abstract_config.hpp"
#include "atc_util.hpp"
#include "basic_ip.hpp"
#include "abstract_analyzer_if.hpp"
#include <vfw_string.h>

#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#include <cstdio>
#endif

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
namespace ATC
{

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractAnalyzerIF::AbstractAnalyzerIF() : ProcComponent(atcAnalyzerId, "AnalyzerIF", "AIF"), initDone(false)
  {
    if (coreAnalyserIFInstancePtr != NULL)
    {
      //Halt the AOS when the constructor init is failed
      ATC::aosHalt(__FILE__, __LINE__, "Constructor Init Failed");
    }
    // Initialize the AbstractAnalyzerIF class variables with default value
    measListCounter = 0U;
    aifSendCurCycleCount = 0U;
    aifState = AIFNotConnected;
    // Setup single instance pointer for core access
    coreAnalyserIFInstancePtr = this;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractAnalyzerIF::init(void)
  {
    // Prevent Multiple Initialization
    if (!initDone)
    {
#ifndef _DISPATCHER
      if (vfwGetSide() != VFW_B_SIDE)
      {
#endif
        const uint16_t portNumber = ATP::AbstractConfig::corePtr()->getAnalyzerIFPort();

        // Initialize the AbstractAnalyzerIF class buffer variables with default value
        memset(&measureList[0], 0x00, sizeof(measureList));
        //Initialize a connection via BasicIp. IP address doesn't need to be populated as it is a TCP host connection .
        initDone = ATC::AbstractBasicIP::corePtr()->initConnection(getConnectionID(), ATC::AbstractBasicIP::ConnectionTypeTcpHost,
          static_cast<char_t*>(NULL), portNumber, static_cast<uint32_t>(maxOutputBufferLen), static_cast<uint32_t>(maxInputBufferLen));
#ifndef _DISPATCHER
      }
      else
      {
        initDone = true;
      }
#endif
    }
    return initDone;// return true if init()  successful 
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractAnalyzerIF::run(void)
  {
    // Continue only if AOS Analyzer is connected with ATP/ATO !
    if (AbstractBasicIP::ConnectionStatusConnected == AbstractBasicIP::corePtr()->getConnectionStatus(getConnectionID()))
    {
      //Read the incoming command from AOS Analyzer
      char_t cmdStr[maxTextLen];
      // If there is no command from AOS analyzer it is become true
      bool unknownCmd = false;

      //Read the incoming command using  readAIFCmd
      uint32_t  byteRead = readAIFCmd(&cmdStr[0]);

      //aifState maintain internal states when AIF is connected to AOS Analyzer
      switch (aifState)
      {

      case AIFNotConnected:
      {
        // Write Unit Data to AOS Analyzer ,shall be define in ATP/ATO
        writeAIFUnitData();
        // Send the available measurement list to AOS Analyzer
        writeAIFMeasurableList();
        // Send the start and end message for ParameterList to AOS Analyzer
        // It is just required to make it compatible with AOS Analyzer
        writeAIFParameterList();
        //change the aifState 
        aifState = AIFConnected;
        break;
      }
      case AIFConnected:
      {
        // Check any request from AOSAnalyzer
        if (byteRead > 0U)
        {
          // Compare with "START" command coming from AOS Analyzer
          if (ATC::isTextMatch(&cmdStr[0], "START", sizeof("START")))
          {
            //change the  aifState
            aifState = AIFMeasuring;
            //save the timestamp when the "START" Command is received by AIF
            startMeasTimestamp = vfwGetReferenceTime();
          }
          else
          {
            unknownCmd = true;
          }
        }
        break;
      }
      case AIFMeasuring:
      {
        // Check any request from AOSAnalyzer 
        if (byteRead > 0U)
        {
          // Stop logging ,if it is a "STOP" command coming from AOS Analyzer
          if (ATC::isTextMatch(&cmdStr[0], "STOP", sizeof("STOP")))
          {
            //change the aifState
            aifState = AIFConnected;
          }
          else
          {
            unknownCmd = true;
          }
        }
        // Send AOS Measurable Sample data to AOS Analyzer if still logging 
        if (AIFMeasuring == aifState)
        {
          //send the AOS Sample value to AOS Analyzer
          sendAIFSampleMeasData(vfwGetReferenceTime());
        }
        break;
      }
      default:
        aifState = AIFNotConnected;
        break;

      }//End of Switch Case 

      // If it is unknown command coming from AOS Analyzer ,Report back to AOS Analyzer about it 
      if (true == unknownCmd)
      {
        OutputBuffer outputBuffer;
        const int32_t retVal = snprintf(&outputBuffer.charBuffer[0], maxOutputBufferLen, "Unknown command: %s\r\n", cmdStr);

        if ((retVal > 0) && (static_cast<size_t>(retVal) < maxOutputBufferLen))
        {
          // Write outputBuffer to AOS Analyzer IF using BasicIP 
          writeToAOSAnalyzer(outputBuffer, static_cast<uint32_t>(retVal));
        }
      }
    }
    else
    {
      //reset to initial state when "START/STOP" measure is not started 
      aifState = AIFNotConnected;
    }
  }
  /******************************************************************************
  * writeToAOSAnalyzer
  ******************************************************************************/
  void AbstractAnalyzerIF::writeToAOSAnalyzer(const OutputBuffer& outputBuffer, const uint32_t lenOfStrToAppend) const
  {
    //If outputBuffer has space then only write to AOS Analyzer 
    if (maxOutputBufferLen >= lenOfStrToAppend)
    {
      //write the AIF Data to AOS Analyzer using BasicIP call
      const uint32_t noOfCharsSent = AbstractBasicIP::corePtr()->writeBuf(getConnectionID(),
        &outputBuffer.byteBuffer[0], lenOfStrToAppend);

      //check whether complete AOS Analyzer IF has successfully written to AOS Analyzer or not 
      if (lenOfStrToAppend != noOfCharsSent)
      {
        //Log to RU 
        writeToLog(ATC::DetailedLog, "Analyzer IF : BasicIP Write failed!", __FILE__, __LINE__);
      }
    }
    else
    {
      // Log to RU
      writeToLog(ATC::DetailedLog, "Analyzer IF : Output buffer limit exceeded!", __FILE__, __LINE__);
    }
  }

  /******************************************************************************
  * writeAIFMeasurableList
  ******************************************************************************/
  void AbstractAnalyzerIF::writeAIFMeasurableList() const
  {
    OutputBuffer outputBuffer;
    uint32_t lenOfStrToAppend = 0U;

    // Append MeasurablesStart message to outputBuffer
    int32_t retVal = snprintf(&outputBuffer.charBuffer[0], maxOutputBufferLen, "[MeasurablesStart]\r\n");
    if ((retVal > 0) && (static_cast<size_t>(retVal) < maxOutputBufferLen))
    {
      lenOfStrToAppend += static_cast<size_t>(retVal);
    }

    uint32_t bytesLeft = maxOutputBufferLen - lenOfStrToAppend;

    for (uint8_t measCount = 0U; measCount < measListCounter; measCount++)
    {
      const char_t* type;

      switch (measureList[measCount].type)
      {
        case MeasureBool:
        case MeasureByte:
          type = "BYTE";
          break;
        case MeasureWord:
          type = "WORD";
          break;
        case MeasureDoubleWord:
        case MeasureEnum:
          type = "DWORD";
          break;
        case MeasureSWord:
          type = "SWORD";
          break;
        case MeasureSDoubleWord:
          type = "SDWORD";
          break;
        default:
          type = static_cast<const char_t*>(NULL);
      }

      if (type == NULL)
      {
        // Append "Undefined" to  outputBuffer if there is no proper types is present
        retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "Undefined");
      }
      else if (measureList[measCount].isSignedValue)
      {
        retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "\"%s\";\"%s\";%s;\"%s\";%d;%d\r\n",
          measureList[measCount].name, measureList[measCount].descr, type, measureList[measCount].unit,
          measureList[measCount].minValue.valueS, measureList[measCount].maxValue.valueS);
      }
      else
      {
        retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "\"%s\";\"%s\";%s;\"%s\";%u;%u\r\n",
          measureList[measCount].name, measureList[measCount].descr, type, measureList[measCount].unit,
          measureList[measCount].minValue.valueU, measureList[measCount].maxValue.valueU);
      }

      if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
      {
        lenOfStrToAppend += static_cast<uint32_t>(retVal);
        bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
      }
      else
      {
        // Stop if too small buffer or failure
        break;
      }
    }//end of for loop

    //Append measurement End message to outputBuffer
    retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "[MeasurablesEnd]\r\n");
    if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
    {
      //Calculate length of total string which need to write on AOS analyzer
      lenOfStrToAppend += static_cast<uint32_t>(retVal);
      // Write meassureList to AOS Analyzer IF using BasicIP 
      writeToAOSAnalyzer(outputBuffer, lenOfStrToAppend);
    }
  }

  /******************************************************************************
  * writeAIFParameterList
  ******************************************************************************/
  void AbstractAnalyzerIF::writeAIFParameterList() const
  {
    OutputBuffer outputBuffer;
    uint32_t lenOfStrToAppend = 0U;

    //It is required by the AOS Analyzer so that it will connected with ATP/ATO .
    //So just need to send the starting and end message for parameter list
    int32_t retVal = snprintf(&outputBuffer.charBuffer[0], maxOutputBufferLen, "[ParametersStart]\r\n");

    if ((retVal > 0) && (static_cast<size_t>(retVal) < maxOutputBufferLen))
    {
      lenOfStrToAppend += static_cast<size_t>(retVal);

      const uint32_t bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
      retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "[ParametersEnd]\r\n");
      // All good 
      if ((retVal > 0)  &&  (static_cast<size_t>(retVal) < bytesLeft))
      {
        //Calculate length of total string which need to write on AOS analyzer
        lenOfStrToAppend += static_cast<uint32_t>(retVal);
        // Write parameter starting & end message to AOS Analyzer IF using BasicIP
        writeToAOSAnalyzer(outputBuffer, lenOfStrToAppend);
      }
    }
  }

  /******************************************************************************
  * sendAIFSampleMeasData
  ******************************************************************************/
  void AbstractAnalyzerIF::sendAIFSampleMeasData(const int64_t currentTimestamp)
  {
    const uint8_t sendCycle = ATP::AbstractConfig::corePtr()->getSendCycleAIF();

    //Check whether AIF can send the sample data by comparing with config parameter sendCycleAIF
    if (sendCycle == aifSendCurCycleCount)
    {
      //Build string(collection of Sample data) to send
      OutputBuffer outputBuffer;
      uint32_t lenOfStrToAppend = 0U;

      //Append the time difference between currentTimestamp and  startMeasTimestamp into outputBuffer
      int32_t retVal = snprintf(&outputBuffer.charBuffer[0], maxOutputBufferLen, "%d;", static_cast<int32_t>(currentTimestamp - startMeasTimestamp));
      if ((retVal > 0) && (static_cast<size_t>(retVal) < maxOutputBufferLen))
      {
        lenOfStrToAppend += static_cast<size_t>(retVal);
      }

      uint32_t bytesLeft = maxOutputBufferLen - lenOfStrToAppend;

      for (uint8_t aifCount = 0U; aifCount < measListCounter; aifCount++)
      {
        // checking type of data in measureList and then parse accordingly 
        switch (measureList[aifCount].type)
        {
        case MeasureBool:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%u;",
            *measureList[aifCount].measureValue.measureValueBool ? 1U : 0U);
          break;
        case MeasureByte:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%u;",
            *measureList[aifCount].measureValue.measureValueU8);
          break;
        case MeasureWord:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%u;",
            *measureList[aifCount].measureValue.measureValueU16);
          break;
        case MeasureSWord:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%d;",
            *measureList[aifCount].measureValue.measureValueS16);
          break;
        case MeasureDoubleWord:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%u;",
            *measureList[aifCount].measureValue.measureValueU32);
          break;
        case MeasureSDoubleWord:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%d;",
            *measureList[aifCount].measureValue.measureValueS32);
          break;
        case MeasureEnum:
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "%u;",
            measureList[aifCount].measureValue.valueGetter->getValue());
          break;
        default:
          //Append '0;' into outputBuffer in case of default case
          retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "0;");
          break;
        }

        if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
        {
          lenOfStrToAppend += static_cast<uint32_t>(retVal);
          bytesLeft = maxOutputBufferLen - lenOfStrToAppend;
        }
      }

      //Append trailing character("\r\n") into outputBuffer
      retVal = snprintf(&outputBuffer.charBuffer[lenOfStrToAppend], bytesLeft, "\r\n");
      if ((retVal > 0) && (static_cast<size_t>(retVal) < bytesLeft))
      {
        lenOfStrToAppend += static_cast<uint32_t>(retVal);
        // Sending string to AOS Analyzer 
        writeToAOSAnalyzer(outputBuffer, lenOfStrToAppend);
      }
      // Reset aifSendCurCycleCount to zero so that it will start again to reach the sendCycle Count
      aifSendCurCycleCount = 0U;
    }
    else
    {
      //increment the aifSendCurCycleCount by 1 
      aifSendCurCycleCount++;
    }
  }

  /******************************************************************************
  * readAIFCmd
  ******************************************************************************/
  uint32_t AbstractAnalyzerIF::readAIFCmd(char_t aifCommand[]) const
  {
    //Hold the value of number of characters read by BasicIP when ATP is connected with AOS Analyzer
    uint32_t  bytesRead;
    uint8_t  inIdx = 0U;
    bool lineFound = false;

    InputBuffer inputBuffer;

    // Read the START/STOP command from AOS Analyzer using BasicIP call 
    bytesRead = AbstractBasicIP::corePtr()->readBuf(getConnectionID(), (&inputBuffer.byteBuffer[0]),
      maxInputBufferLen);

    //Proceed only when AIF read data from AOS Analyzer
    if (bytesRead > 0U)
    {
      // Search for terminate character(\n and \r are terminating characters for "START" and "STOP" command)
      while ((inIdx < bytesRead) && (false == lineFound))
      {
        char_t& ch = inputBuffer.charBuffer[inIdx];
        // Terminate character found !!
        if ((ch == '\n') || (ch == '\r'))
        {
          if (inIdx < maxInputBufferLen)
          {
            ch = '\0';
          }
          else
          {
            inputBuffer.charBuffer[maxInputBufferLen - 1U] = '\0';
          }
          lineFound = true;
        }
        ++inIdx;
      }
    }

    //If incoming command has terminating characters then copy into aifCommand
    if (true == lineFound)
    {
      //Copy the read command(inputBuffer) with terminating character into aifCommand
      aifCommand[0] = '\0';
      static_cast<void>(vfw_strlcpy(aifCommand, &inputBuffer.charBuffer[0], static_cast<size_t>(inIdx) + 1U));
    }

    return bytesRead;//return the number of character read by Analyzer AIF
  }

  /******************************************************************************
  * registerMeasurementInternal
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurementInternal(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    MeasureValuePtr const measureValue, const MeasurementTypeEnum type, const ValueRange minValue, const ValueRange maxValue, const bool isSigned) //lint -e{1746} It is already const
  {
    bool retVal = true;

    //Check for validity for different measurement pointer variables
    if ((NULL == nameOfMeasData) || (NULL == descr) || (NULL == unit) || (NULL == measureValue.measureValue))
    {
      writeToLog(ATC::BriefLog, "Register Measure Data is NULL", __FILE__, __LINE__);
      retVal = false;
    }
    else
    {
      // Add all the register measurable values to measureList till measureList is full  
      if (measListCounter < maxMeasureDataList)
      {
        //Copy all the incoming registered measurement data into AIF measureList array of structure
        measureList[measListCounter].name = nameOfMeasData;
        measureList[measListCounter].descr = descr;
        measureList[measListCounter].unit = unit;
        measureList[measListCounter].minValue = minValue;
        measureList[measListCounter].maxValue = maxValue;
        measureList[measListCounter].type = type;
        measureList[measListCounter].measureValue = measureValue;
        measureList[measListCounter].isSignedValue = isSigned;
        measListCounter++;
      }
      else
      {
        writeToLog(ATC::BriefLog, "MeasureList is full", __FILE__, __LINE__);
        retVal = false;
      }
    }
    return retVal;
  }

  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const bool *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueBool = measuredValue;
    min.valueU = 0U;
    max.valueU = 1U;

    return registerMeasurementInternal(nameOfMeasData, descr, "boolean", mvptr, MeasureBool, min, max, false);
  }

  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const uint8_t minValue, const uint8_t maxValue, const uint8_t *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueU8 = measuredValue;
    min.valueU = minValue;
    max.valueU = maxValue;
    
    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureByte, min, max, false);
  }

  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const uint16_t minValue, const uint16_t maxValue, const uint16_t *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueU16 = measuredValue;
    min.valueU = minValue;
    max.valueU = maxValue;

    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureWord, min, max, false);
  }


  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const int16_t minValue, const int16_t maxValue, const int16_t *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueS16 = measuredValue;
    min.valueS = minValue;
    max.valueS = maxValue;

    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureSWord, min, max, true);
  }


  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const uint32_t minValue, const uint32_t maxValue, const uint32_t *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueU32 = measuredValue;
    min.valueU = minValue;
    max.valueU = maxValue;

    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureDoubleWord, min, max, false);
  }

  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const int32_t minValue, const int32_t maxValue, const int32_t *const measuredValue)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.measureValueS32 = measuredValue;
    min.valueS = minValue;
    max.valueS = maxValue;

    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureSDoubleWord, min, max, true);
  }

  /******************************************************************************
  * registerMeasurement
  ******************************************************************************/
  bool AbstractAnalyzerIF::registerMeasurement(const char_t *const nameOfMeasData, const char_t *const descr, const char_t *const unit,
    const uint32_t minValue, const uint32_t maxValue, const EnumValueGetter* const valueGetter)
  {
    ValueRange min;
    ValueRange max;
    MeasureValuePtr mvptr;
    mvptr.valueGetter = valueGetter;
    min.valueU = minValue;
    max.valueU = maxValue;

    return registerMeasurementInternal(nameOfMeasData, descr, unit, mvptr, MeasureEnum, min, max, false);
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractAnalyzerIF::consoleCall(const uint32_t argc, const ConsoleArguments argv)
  {
    /*
    This functions parses the arguments searches for the "help", "trace" or any other Console
    component specific command calls and handles it. Returns true if completely handled
    else returns false. returning false will let other components handle the call. help always returns false.
    */
    bool retVal = false;
    char_t  outputBuffer[maxOutputBufferLen];

    // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
    if (isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      const char_t* const toWrite = "Analyzer      To print the detailed information of Registered Measurement variables";

      AbstractConsole::corePtr()->writeWithNewline(toWrite);
      retVal = false;
    }
    // Handle the "Analyzer" console command 
    else if (isTextMatch(&argv[0][0], "Analyzer", sizeof("Analyzer")) && (argc == 1U))
    {
      //check any AIF measurable data is present in the measureList
      if (measListCounter > 0U)
      {
        int32_t ret = snprintf(&outputBuffer[0], maxOutputBufferLen, "%-32s%-40s%-8s%-12s%-12s%-12s", "Name", "Description", "Unit", "MinVal", "MaxVal", "MeasuredValue");

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < maxOutputBufferLen))
        {
          AbstractConsole::corePtr()->writeWithNewline(&outputBuffer[0]);
        }

        for (uint8_t countVar = 0U; countVar < measListCounter; countVar++)
        {
          if (measureList[countVar].isSignedValue)
          {
            ret = snprintf(&outputBuffer[0], maxOutputBufferLen, "%-32s%-40s%-8s%-12d%-12d%-12d", measureList[countVar].name,
              measureList[countVar].descr, measureList[countVar].unit, measureList[countVar].minValue.valueS,
              measureList[countVar].maxValue.valueS, *measureList[countVar].measureValue.measureValueS32);
          }
          else
          {
            uint32_t unsignedValue = 0U;
            if ((measureList[countVar].type) == MeasureEnum)
            {
              unsignedValue = measureList[countVar].measureValue.valueGetter->getValue();
            }
            else
            {
              unsignedValue = *measureList[countVar].measureValue.measureValueU32;
            }
            ret = snprintf(&outputBuffer[0], maxOutputBufferLen, "%-32s%-40s%-8s%-12u%-12u%-12u", measureList[countVar].name,
              measureList[countVar].descr, measureList[countVar].unit, measureList[countVar].minValue.valueU,
              measureList[countVar].maxValue.valueU, unsignedValue);
          }

          if ((ret > 0)  &&  (static_cast<size_t>(ret) < maxOutputBufferLen))
          {
            //Print the measurable data on the console 
            AbstractConsole::corePtr()->writeWithNewline(&outputBuffer[0]);
            //Reset the outputBuffer buffer
            memset(&outputBuffer[0], 0x00, maxOutputBufferLen);
          }
        }
      }
      else
      {
        AbstractConsole::corePtr()->writeWithNewline("No registered measurable Value");
      }
      retVal = true;
    }
    else
    {
      //Do nothing
    }

    return retVal;
  }
  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractAnalyzerIF* AbstractAnalyzerIF::corePtr(void)
  {
    return coreAnalyserIFInstancePtr;
  }
}

//lint +esym(586,snprintf)
