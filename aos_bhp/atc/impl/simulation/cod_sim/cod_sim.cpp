/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This class contained the source code files for Common Odometer(COD) Simulation.
*  COD Sim replaces the GSP-2 COD in the SIL and HIL test environments and provide the following functionality:
*  1.Receive and respond to static configuration from the ATP once at startup.
*  2. Receive and respond to dynamic configuration from the ATP sporadically.
*  3. Store any incoming simulated speed/acceleration input data from the AOS PC-simulator.
*     Store it along with a time-stamp in the COD Sim "mirror".
*  4. Send simulated odometer mesurement data to the  "Odometry" ATP Component at requested intervals.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-04-28    akushwah    Created
* 2016-05-06    akushwah    Added the corePtr function
* 2016-05-25    akushwah    Implemented the CODSim Simulator functionality.
* 2016-06-09    arastogi    Updated to fix the interface change in event handler
* 2016-06-21    akushwah    Corrected the Config Response Channel name
* 2016-07-05    spandita    Updated with review comments (renamed to odoconfigResponseChannel_A & odoconfigResponseChannel_B)
                            Modified with name of channels
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-23    arastogi    Removed ATC::
* 2016-10-05    saprasad    Fixed Integration issue,implement consolecall to act as AOSPC and optimize code
* 2016-10-12    arastogi    Config response should be sent from runOut
*                           Config response should be sent only when config message is received.
*                           Speed should not be calculated from acceleration.
* 2016-10-03    arastogi    Fixed the timestamps to be int64.
*                           The speed interpolation now takes care of direction.
* 2016-10-24    arastogi    Bug fix for simulated speed.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include <cstdio>
#include "cod_sim.hpp"
#include "abstract_event_handler.hpp"
#include <vfw_string.h>
#ifdef WIN32
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif



/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  uint8_t my_max(const uint8_t a, const uint8_t b)
  {
    return (a > b) ? a : b;
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  namespace Sim
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    CODSim::CODSim() : IOComponent(atcCODSimId, "CODSim", "CS"),
      initDone(false),
      simulateSlip(false),
      simulateSlide(false),
      vMaxError(0),
      vMinError(0)
    {
      if (coreCODSimPtr != 0)
      {
        aosHalt(__FILE__, __LINE__, "CODSim Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreCODSimPtr = this;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void CODSim::preInit()
    {
      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_ChannelDesc codSimChannelRead;

      /** Channel handle returned by vfwChannelOpenRead()
      */
      VFW_ChannelDesc configChannelRead;

      // Name of channel to read messages from AOSPC
      char_t codSimReadChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to read messages from ATP Component Odometry
      char_t configReadChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write the config response messages to ATP Component Odometry
      char_t configResponseChannelName[VFW_CH_NAME_MAX_Z];

      // Name of channel to write messages to ATP Component Odometry
      char_t codSimValueWriteChannelName[VFW_CH_NAME_MAX_Z];

#ifdef _SIL
      static_cast<void>(vfw_strlcpy(&codSimReadChannelName[0], speedChannelSim_A, sizeof(codSimReadChannelName)));
      static_cast<void>(vfw_strlcpy(&configReadChannelName[0], odoConfigTelegramChannel_A, sizeof(configReadChannelName)));
      static_cast<void>(vfw_strlcpy(&configResponseChannelName[0], odoconfigResponseChannel_A, sizeof(configResponseChannelName)));
      static_cast<void>(vfw_strlcpy(&codSimValueWriteChannelName[0], odoMeasureDataTeleChannel_A, sizeof(codSimValueWriteChannelName)));
#else
      if (vfwGetSide() == VFW_B_SIDE)
      {
        static_cast<void>(vfw_strlcpy(&codSimReadChannelName[0], speedChannelSim_B, sizeof(codSimReadChannelName)));
        // In HIL , messages on VFW channels need to be sent to the opposite CPU, as VFW doesn't allow you to open a read and write 
        // channel with the same name on a single process
        static_cast<void>(vfw_strlcpy(&configReadChannelName[0], odoConfigTelegramChannel_A, sizeof(configReadChannelName)));
        static_cast<void>(vfw_strlcpy(&configResponseChannelName[0], odoconfigResponseChannel_A, sizeof(configResponseChannelName)));
        static_cast<void>(vfw_strlcpy(&codSimValueWriteChannelName[0], odoMeasureDataTeleChannel_A, sizeof(codSimValueWriteChannelName)));
      }
      else if (vfwGetSide() == VFW_A_SIDE)
      {
        static_cast<void>(vfw_strlcpy(&codSimReadChannelName[0], speedChannelSim_A, sizeof(codSimReadChannelName)));
        // In HIL , messages on VFW channels need to be sent to the opposite CPU, as VFW doesn't allow you to open a read and write
        // channel with the same name on a single process
        static_cast<void>(vfw_strlcpy(&configReadChannelName[0], odoConfigTelegramChannel_B, sizeof(configReadChannelName)));
        static_cast<void>(vfw_strlcpy(&configResponseChannelName[0], odoconfigResponseChannel_B, sizeof(configResponseChannelName)));
        static_cast<void>(vfw_strlcpy(&codSimValueWriteChannelName[0], odoMeasureDataTeleChannel_B, sizeof(codSimValueWriteChannelName)));
      }
      else
      {
        // To please lint:
        codSimReadChannelName[0] = '\0';
        configReadChannelName[0] = '\0';
        configResponseChannelName[0] = '\0';
        codSimValueWriteChannelName[0] = '\0';

        aosHalt(__FILE__, __LINE__, "Invalid Side");
      }
#endif

      // Open a channel to be used when reading from AOSPC
      codSimChannelRead = vfwChannelOpenRead(&codSimReadChannelName[0],
        speedSimChannelQueueSize, speedSimChannelTelSize, &codSimReadChannelName[0]);
      vfwChannelSetOverwritable(codSimChannelRead);

      // Open a channel to be used when reading from Odometry component in ATP
      configChannelRead = vfwChannelOpenRead(&configReadChannelName[0], odoConfigTeleQueueSize,
        odoConfigTeleMsgSize, &configReadChannelName[0]);

      // Open a channel to be used when writing to Odometry component in ATP
      codSimValueChannelWrite = vfwChannelOpenWrite(&codSimValueWriteChannelName[0], odoMeasureDataTeleQueueSize,
        odoMeasurementDataTelegramMsgSize);
      vfwChannelSetOverwritable(codSimValueChannelWrite);

      // Open a channel to be used when writing the config response to Odometry component in ATP
      configResponseChannelWrite = vfwChannelOpenWrite(&configResponseChannelName[0], odoConfigResponseQueueSize,
        odoConfigResponseMsgSize);

      if ((NULL != codSimChannelRead) && (NULL != configChannelRead) &&
        (NULL != codSimValueChannelWrite) && (NULL != configResponseChannelWrite))
      {
        // Synchronize with diversified channel (A/B)
        syncCodSimChannelRead = vfwSyncAddChannel(codSimChannelRead, ATC::trueVfw);
        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(syncCodSimChannelRead);

        // Synchronize with diversified channel (A/B)
        syncConfigChannelRead = vfwSyncAddChannel(configChannelRead, ATC::trueVfw);
        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(syncConfigChannelRead);
      }
      else
      {
        aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }
    }
    /******************************************************************************
    * init
    ******************************************************************************/
    bool CODSim::init(void)
    {
      if (!initDone)
      {
        //Setting the Simulated data Validity flag as false
        simulatedData.dataValid = false;
        //Setting the connectionEstablished flag as false
        connectionEstablished = false;

        //Initialise the Estimated Maximum Distance(dMax), Estimated Nominal Distance(dNom) and Estimated Minimum 
        //Distance(dMin) as 0.
        estimatedSimulatedData.nomDistance = 0;
        estimatedSimulatedData.minDistance = 0;
        estimatedSimulatedData.maxDistance = 0;

        //Timestamp previous
        estimatedSimulatedData.prevTimeStamp = 0;

        initDone = true;
      }
      return initDone;

    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void CODSim::runIn(void)
    {
      //call the codSimRead() function
      codSimRead();
      //call the configRead() function
      configRead();
    }

    /******************************************************************************
    * codSimRead
    ******************************************************************************/
    void CODSim::codSimRead(void)
    {
      // Read as long there is any message available on codSimChannelRead (only last message will be used)
      while (vfwSyncChannelStat(syncCodSimChannelRead) > 0U)
      {
        // Read one message 
        const uint32_t noOfBytesRead = static_cast<uint32_t>(vfwSyncChannelRead(
          syncCodSimChannelRead, &simMovementTelegram[0], (headerFooterLen + simMovementDataLen)));
        //check if the size of SimulatedMovement data
        if (noOfBytesRead >= (headerFooterLen + simMovementDataLen))
        {
          // Use vfw-functions to convert from network order to host order
          VFW_Buffer buffer;

          //Initialize a simMovementTelegram buffer for VFW_Buffer usage
          vfwInitBuffer(&buffer, &simMovementTelegram[0], noOfBytesRead);

          // Set buffer in read mode. Valid size is set to the amount of data 
          // filled in the raw_buffer. 
          vfwSetReadBuffer(&buffer, noOfBytesRead);

          // converting STX from network to host byte order
          uint8_t stx = vfwGetU8(&buffer);
          // converting VER from network to host byte order
          uint8_t ver = vfwGetU8(&buffer);
          // converting LEN from network to host byte order
          uint16_t len = vfwGetU16(&buffer);
          // converting nidMessageType from network to host byte order
          uint8_t nidMessageType = vfwGetU8(&buffer);
          // converting vSim from network to host byte order
          int16_t vSim = static_cast<int16_t>(vfwGetU16(&buffer));
          // converting aSim from network to host byte order
          int16_t aSim = static_cast<int16_t>(vfwGetU16(&buffer));
          // converting nSensorMinError from network to host byte order
          uint16_t nSensorMinError = vfwGetU16(&buffer);
          // converting nSensorMaxError from network to host byte order
          uint16_t nSensorMaxError = vfwGetU16(&buffer);
          // converting ETX from network to host byte order
          uint8_t etx = vfwGetU8(&buffer);
          //checks the consistency of telegram received
          if ((stx == STX) && (etx == ETX))
          { //checks version received should be same as protocol Version(1)
            if (ver == Sim::simMovementProtocolVer)
            { //checks the SimulatedMovement data type
              if (nidMessageType == simMovementNidMsgType)
              {
                if (len == simMovementDataLen)
                { //store the Simulated Movement data
                  simulatedData.speed = vSim;
                  simulatedData.acceleration = aSim;
                  simulatedData.nSensorMinError = nSensorMinError;
                  simulatedData.nSensorMaxError = nSensorMaxError;
                  simulatedData.timeStampSimMovData = vfwGetReferenceTime();//timeStampSimMovData in milliseconds
                  simulatedData.dataValid = true;
                }
                else
                {
                  writeToLog(ATC::BriefLog, "SimulatedMovement data length Error", __FILE__, __LINE__);
                }
              }
              else
              {
                writeToLog(ATC::BriefLog, "NOT SimulatedMovement Message", __FILE__, __LINE__);
              }
            }
            else
            {
              writeToLog(ATC::BriefLog, "Simulated Movement message Protocol Version not correct", __FILE__, __LINE__);
            }
          }
          else
          {
            writeToLog(ATC::BriefLog, "SimulatedMessage Length not consistent", __FILE__, __LINE__);
          }
        }
        else
        {
          writeToLog(ATC::BriefLog,"COD Simulation Message Size not correct", __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * configRead
    ******************************************************************************/
    void CODSim::configRead(void)
    {
      //set config telegram type to 0.
      configReadTelegram[0] = 0U;

      // If there is any message available on UserAppl_To_Odo_A/B channel
      if (vfwSyncChannelStat(syncConfigChannelRead) > 0U)
      {
        //Timestamp when Config telegram received
        timeStampConfigTelRec = vfwGetReferenceTime();  //timeStampConfigTelRec in millieconds

        // Read one message
        static_cast<void>(vfwSyncChannelRead(syncConfigChannelRead, &configReadTelegram[0], odoConfigTeleMsgSize));

        //extracting the telegram type of the telegram received
        uint8_t telegramType = configReadTelegram[0];
        //Extracting the qVersion of the telegram received
        uint8_t qVersion = configReadTelegram[1];

        //check the connection is Established or not 
        if (connectionEstablished)
        { //check the Telegram type i.e static or dynamic
          if (telegramType == dynConfigTelType)
          {
            //Parse and store Dynamic Configuration telegram in buffer           
            parseDynamicConfig();
          }
          else
          {
            //static or any other Configuration telegram
            writeToLog(ATC::BriefLog, "Static Configuration telegram Received after Connection is Established", __FILE__, __LINE__);
          }
        }
        else
        { //check the Telegram type i.e static or dynamic
          if (telegramType == staticConfigTelType)
          {//check for qVersion of telegram type
            if (qVersion == 1U)
            {
              //Parse and store static Configuration telegram in buffer
              parseStaticConfig();

              //Calculate the sendCycle
              sendCycle = my_max(static_cast<uint8_t>((staticConfig_ch1OdoMeasInterval * codInterval) / codSimInterval), 1U);
            }
            else
            {
              //set static configuration telegram qversion as zero
              staticConfig_qVersion = 0U;
              connectionEstablished = false;
              writeToLog(ATC::BriefLog, "No Connection is Established", __FILE__, __LINE__);
            }
          }
          else
          {
            //Dynamic Configuration telegram
            writeToLog(ATC::BriefLog, "Dynamic Configuration telegram Received before Connection is Established", __FILE__, __LINE__);
          }
        }
      }
      else
      {
        //Do Nothing
      }
    }

    /******************************************************************************
    *parseStaticConfig
    ******************************************************************************/
    void CODSim::parseStaticConfig()
    {
      VFW_Buffer localDummyBuffer;
      vfwInitBuffer(&localDummyBuffer, &configReadTelegram[0], odoConfigTeleMsgSize);
      vfwSetReadBuffer(&localDummyBuffer, odoConfigTeleMsgSize);

      //lint --e{534} ignoring return value

      /*configTgmType*/ vfwGetU8(&localDummyBuffer);
      staticConfig_qVersion = vfwGetU8(&localDummyBuffer);
      /*sensorConfig*/ vfwGetU8(&localDummyBuffer);
      /*mContrTraction*/ vfwGetU8(&localDummyBuffer);
      /*maxPulseCounterFrequency*/ vfwGetU8(&localDummyBuffer);
      /*tachoDirection1*/ vfwGetU8(&localDummyBuffer);
      /*tachoPulse1*/ vfwGetU16(&localDummyBuffer);
      /*tachoDirection2*/ vfwGetU8(&localDummyBuffer);
      /*tachoPulse2*/ vfwGetU16(&localDummyBuffer);
      /*vLowDoppler*/ vfwGetU16(&localDummyBuffer);
      /*vHighDoppler*/ vfwGetU16(&localDummyBuffer);
      /*aMaxAccDoppler*/ vfwGetU16(&localDummyBuffer);
      staticConfig_ch1OdoMeasInterval = vfwGetU8(&localDummyBuffer);
      /*ch1FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch1FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch1FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch1PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch2OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch2FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch2FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch2FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch2PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch3OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch3FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch3FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch3FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch3PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch4OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch4FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch4FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch4FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch4PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch5OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch5FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch5FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch5FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch5PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch6OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch6FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch6FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch6FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch6PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch7OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch7FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch7FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch7FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch7PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch8OdoMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch8FffisOdoMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch8FffisParamMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch8FffisStatusMsgInterval*/ vfwGetU8(&localDummyBuffer);
      /*ch8PzbLzbMeasInterval*/ vfwGetU8(&localDummyBuffer);
      /*powerSuppliedSensors*/ vfwGetU8(&localDummyBuffer);
    }

    /******************************************************************************
    *parseDynamicConfig
    ******************************************************************************/
    void CODSim::parseDynamicConfig(void)
    {
      VFW_Buffer localDummyBuffer;
      vfwInitBuffer(&localDummyBuffer, &configReadTelegram[0], odoConfigTeleMsgSize);
      vfwSetReadBuffer(&localDummyBuffer, odoConfigTeleMsgSize);

      //lint --e{534} ignoring return value

      /*configTgmType*/ vfwGetU8(&localDummyBuffer);
      dynamicConfig_qVersion = vfwGetU8(&localDummyBuffer);
      /*aMaxAcc*/ vfwGetU16(&localDummyBuffer);
      /*aMaxDec*/ vfwGetU16(&localDummyBuffer);
      /*gCur*/ vfwGetU8(&localDummyBuffer);
      /*qGdir*/ vfwGetU8(&localDummyBuffer);
      /*gradType*/ vfwGetU8(&localDummyBuffer);
      /*tachoWheelSize1*/ vfwGetU16(&localDummyBuffer);
      /*tachoWheelSizeError1*/ vfwGetU8(&localDummyBuffer);
      /*tachoWheelSize2*/ vfwGetU16(&localDummyBuffer);
      /*tachoWheelSizeError2*/ vfwGetU8(&localDummyBuffer);
      /*dopplerPulse*/ vfwGetU32(&localDummyBuffer);
      /*dopplerPrecision*/ vfwGetU8(&localDummyBuffer);
      /*calibrationFlag*/ vfwGetU8(&localDummyBuffer);
      /*calibrationSyncTime*/ vfwGetU64(&localDummyBuffer);
      /*appliedBrakeLevel*/ vfwGetU8(&localDummyBuffer);
      /*pzbLzbDtrainDirection*/ vfwGetU8(&localDummyBuffer);
    }

    /******************************************************************************
    *ConfigResponse
    ******************************************************************************/
    void CODSim::configResponse(void)
    {
      outConfigResponseBuf.telegramType = configResTelType;

      //extracting the telegram type of the telegram received
      uint8_t telegramType = configReadTelegram[0];
      if (telegramType == staticConfigTelType)
      {
        outConfigResponseBuf.qVersion = staticConfig_qVersion;
      }
      else if (telegramType == dynConfigTelType)
      {
        outConfigResponseBuf.qVersion = dynamicConfig_qVersion;
      }
      else
      {
        //Do nothing
      }

      //Only send a response if a valid config is received.
      if ((telegramType == staticConfigTelType) || (telegramType == dynConfigTelType))
      {
        outConfigResponseBuf.sdpVerMajor = Sim::sdpMajorVer;   //SDP SW version (major)
        outConfigResponseBuf.sdpVerMid = Sim::sdpMidVer;     //SDP SW version (middle)
        outConfigResponseBuf.sdpVerMinor = Sim::sdpMinorVer;   //SDP SW version (minor)

        //setting the ConfigStatus of ConfigResponse
        if ((telegramType == staticConfigTelType) && (connectionEstablished))
        {
          outConfigResponseBuf.configStatus = Sim::unexpectedStatic;
        }
        else if ((telegramType == dynConfigTelType) && (!connectionEstablished))
        {
          outConfigResponseBuf.configStatus = Sim::unexpectedDyn;
        }
        else if (telegramType == staticConfigTelType)
        {
          outConfigResponseBuf.configStatus = Sim::validStatWaitDyn;
        }
        else if (telegramType == dynConfigTelType)
        {
          outConfigResponseBuf.configStatus = Sim::configOK;
        }
        else
        {
          outConfigResponseBuf.configStatus = Sim::configNOK;
        }
        //Setting the Cal_status
        outConfigResponseBuf.calStatus = Sim::noCalibrationOngoing; // 1(NO Calibration ongoing )

                                                                    //Setting T_DV_TRAIN 
        outConfigResponseBuf.tDvTrain = static_cast<uint32_t>(timeStampConfigTelRec);

        //setting the distance and status in  config Response
        outConfigResponseBuf.calTachoDistance1 = Sim::measuredDistTacho1;
        outConfigResponseBuf.tachoCalResultStatus1 = Sim::resultConcludedCaliTacho1;
        outConfigResponseBuf.calTachoDistance2 = Sim::measuredDistTacho2;
        outConfigResponseBuf.tachoCalResultStatus2 = Sim::resultConcludedCaliTacho2;
        outConfigResponseBuf.calDopplerDistance = Sim::measuredDistDoppler;
        outConfigResponseBuf.dopplerCalResultStatus = Sim::resultConcludedCaliDoppler;

        uint8_t rawBuffer[ATC::odoConfigResponseMsgSize];
        VFW_Buffer messageBuffer;
        vfwInitBuffer(&messageBuffer, &rawBuffer[0], ATC::odoConfigResponseMsgSize);

        vfwPutU8(&messageBuffer, outConfigResponseBuf.telegramType);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.qVersion);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.sdpVerMajor);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.sdpVerMid);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.sdpVerMinor);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.configStatus);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.calStatus);
        vfwPutU32(&messageBuffer, outConfigResponseBuf.tDvTrain);
        vfwPutU32(&messageBuffer, outConfigResponseBuf.calTachoDistance1);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.tachoCalResultStatus1);
        vfwPutU32(&messageBuffer, outConfigResponseBuf.calTachoDistance2);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.tachoCalResultStatus2);
        vfwPutU32(&messageBuffer, outConfigResponseBuf.calDopplerDistance);
        vfwPutU8(&messageBuffer, outConfigResponseBuf.dopplerCalResultStatus);

        //Writing the config Response to the ATP component Odometry on channel "Odo_To_UserAppl_A/B"
        vfwChannelWrite(configResponseChannelWrite, &rawBuffer[0], odoConfigResponseMsgSize);
      }

      //set connectionEstablished as true if config telegram is Static and qVersion is 1
      // configReadTelegram[1] contain the qVersion of the Config telegram
      if ((telegramType == staticConfigTelType) && (configReadTelegram[1] == 1U))
      {
        connectionEstablished = true;
      }
      else
      {
        //Do nothing
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void CODSim::runOut(void)
    {
      cycleCount++;

      //prepare and send odometer Configuration Response
      configResponse();

      //call the function codSimValueWrite()
      codSimValueWrite();
    }

    /******************************************************************************
    * codSimValueWrite
    ******************************************************************************/
    void CODSim::codSimValueWrite()
    {//check if conenction is established or not
      if (connectionEstablished)
      {
        // If there is any message available on Odo_A/B_To_UserAppl channel
        if (simulatedData.dataValid) //check the valid simulated data in Mirror
        {//check if it is time to send the Odometer measurement data telegram
          if ((cycleCount % sendCycle) == 0U)
          {
            int64_t currentTimestamp = vfwGetReferenceTime();  //currentTimeStamp in milliseconds

            //Estimate Speed & Distance

            //calculate the estimated acceleration(1 cm/s2)  
            estimatedSimulatedData.acceleration = simulatedData.acceleration / 100;

            //calculate the estimated Nominal Speed(1 cm/s)
            const int32_t deltaTime = static_cast<int32_t>(currentTimestamp - simulatedData.timeStampSimMovData); // ms
            const int32_t deltaSpeed = ((static_cast<int32_t>(simulatedData.acceleration) * deltaTime) / (100 * 1000));
            if (simulatedData.speed > 0) //if the speed is positive add the delta
            {
              estimatedSimulatedData.nomSpeed = simulatedData.speed + static_cast<int16_t>(deltaSpeed);
              if (estimatedSimulatedData.nomSpeed < 0)
              {
                estimatedSimulatedData.nomSpeed = 0;
              }
            }
            else if (simulatedData.speed < 0) //if speed is negative subtract the delta
            {
              estimatedSimulatedData.nomSpeed = simulatedData.speed - static_cast<int16_t>(deltaSpeed);
              if (estimatedSimulatedData.nomSpeed > 0)
              {
                estimatedSimulatedData.nomSpeed = 0;
              }
            }
            else //if speed is 0 then do not change it
            {
              estimatedSimulatedData.nomSpeed = simulatedData.speed;
            }

            // Nominal Speed (1 cm/s)
            const double_t floatNomSpeed = static_cast<double_t>(estimatedSimulatedData.nomSpeed);
            //calculate the estimated Minimum Speed(1 cm/s)
            //nSensorMinError is in the range 0-100% with the resolution 0.01 % i.e. 100 % == 10000
            const double sensorMinError = abs(floatNomSpeed * simulatedData.nSensorMinError);
            const double_t floatMinSpeed = ((floatNomSpeed * 10000.0) - sensorMinError) / 10000.0;
            estimatedSimulatedData.minSpeed = static_cast<int16_t>(floatMinSpeed);
            //calculate the estimated Maximum Speed(1 cm/s)
            //nSensorMaxError is in the range 0-100% with the resolution 0.01 % i.e. 100 % == 10000
            const double sensorMaxError = abs(floatNomSpeed * simulatedData.nSensorMaxError);
            const double_t floatMaxSpeed = ((floatNomSpeed * 10000.0) + sensorMaxError) / 10000.0;
            estimatedSimulatedData.maxSpeed = static_cast<int16_t>(floatMaxSpeed);

            //calculate the difference between previous and current timestamp
            const int64_t timeDiffPrev_CurrenttimeStamp = currentTimestamp - estimatedSimulatedData.prevTimeStamp;

            //calculate the estimated Nominal Distance(1 cm)
            estimatedSimulatedData.floatNomDistance +=
              (static_cast<double_t>(estimatedSimulatedData.nomSpeed) * static_cast<double_t>(timeDiffPrev_CurrenttimeStamp)) / 1000.0;
            estimatedSimulatedData.nomDistance = static_cast<int32_t>(estimatedSimulatedData.floatNomDistance);

            //calculate the estimated Minimum Distance(1 cm)
            const double_t minDistance = (static_cast<double_t>(estimatedSimulatedData.minSpeed) *
              static_cast<double_t>(timeDiffPrev_CurrenttimeStamp)) / 1000.0;
            //calculate the estimated Maximum Distance(1 cm)
            const double_t maxDistance = (static_cast<double_t>(estimatedSimulatedData.maxSpeed) *
              static_cast<double_t>(timeDiffPrev_CurrenttimeStamp)) / 1000.0;

            if (estimatedSimulatedData.nomSpeed >= 0)
            {
              estimatedSimulatedData.floatMinDistance += minDistance;
              estimatedSimulatedData.floatMaxDistance += maxDistance;
            }
            else
            {
              estimatedSimulatedData.floatMinDistance += maxDistance;
              estimatedSimulatedData.floatMaxDistance += minDistance;
            }

            estimatedSimulatedData.minDistance = static_cast<int32_t>(estimatedSimulatedData.floatMinDistance);
            estimatedSimulatedData.maxDistance = static_cast<int32_t>(estimatedSimulatedData.floatMaxDistance);

            //Writing the Odometer Measurement data telegram to ATP component Odometry on channel "Odo_To_UserAppl_1_A/B"
            uint8_t rawBuffer[ATC::odoMeasurementDataTelegramMsgSize];
            VFW_Buffer messageBuffer;
            vfwInitBuffer(&messageBuffer, &rawBuffer[0], ATC::odoMeasurementDataTelegramMsgSize);

            //Prepare Odometer Measurement Data Telegram
            vfwPutU8(&messageBuffer, Sim::odoMeasDataTelType); // TELEGRAM_TYPE
            vfwPutU8(&messageBuffer, Sim::interfaceVerSupported); // Q_VERSION
            vfwPutU8(&messageBuffer, Sim::fullService); // Q_ODOSAFE
            vfwPutU16(&messageBuffer, Sim::noMaintanceDriverWarning); // Q_CONTROL
            vfwPutU8(&messageBuffer, Sim::directionKnown); // Q_DIR_ERR
            vfwPutU32(&messageBuffer, static_cast<uint32_t>(currentTimestamp)); // T_DV_TRAIN
            // timestamp of previous valid Speed & Distance data
            vfwPutU32(&messageBuffer, static_cast<uint32_t>(estimatedSimulatedData.prevTimeStamp)); // PROD_TIME
            vfwPutI16(&messageBuffer, estimatedSimulatedData.acceleration); // A_TRAIN
            vfwPutI16(&messageBuffer, static_cast<int16_t>(estimatedSimulatedData.maxSpeed+vMaxError)); // V_MAX
            vfwPutI16(&messageBuffer, estimatedSimulatedData.nomSpeed); // V_NOM
            vfwPutI16(&messageBuffer, static_cast<int16_t>(estimatedSimulatedData.minSpeed+vMinError)); // V_MIN
            vfwPutI32(&messageBuffer, estimatedSimulatedData.maxDistance); // D_MAX
            vfwPutI32(&messageBuffer, estimatedSimulatedData.nomDistance); // D_NOM
            vfwPutI32(&messageBuffer, estimatedSimulatedData.minDistance); // D_MIN
            vfwPutU8(&messageBuffer, Sim::unplausible); // T_RADAR_PLAUSIBLE
            vfwPutU8(&messageBuffer, simulateSlip ? Sim::slipStatusTacho : Sim::noSlipSlideStatusTacho); // SLIP_SLIDE_STATUS_1
            vfwPutU8(&messageBuffer, simulateSlide ? Sim::slideStatusTacho : Sim::noSlipSlideStatusTacho); // SLIP_SLIDE_STATUS_2
            vfwPutI16(&messageBuffer, Sim::filteredVelTacho1); // V_TACHO(1)
            vfwPutI16(&messageBuffer, Sim::filteredVelTacho2); // V_TACHO(2)
            vfwPutI16(&messageBuffer, Sim::filteredVelDoppler); // V_DOPPLER
            vfwPutI32(&messageBuffer, Sim::measureDistTacho1); // D_TACHO1
            vfwPutI32(&messageBuffer, Sim::measureDistTacho2); // D_TACHO2
            vfwPutI32(&messageBuffer, Sim::measureDistDoppler); // D_DOPPLER

            vfwChannelWrite(codSimValueChannelWrite, &rawBuffer[0], odoMeasurementDataTelegramMsgSize);

            //setting current time stamp as previous time Stamp
            estimatedSimulatedData.prevTimeStamp = currentTimestamp;
          }
          else
          {
            //Do Nothing
          }
        }
        else
        {
          //Do Nothing
        }
      }
      else
      {
        //Do Nothing
      }
    }


    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    *
    ******************************************************************************/
    CODSim& CODSim::instance(void)
    {
      static CODSim theOnlyCODSimInstance;

      return theOnlyCODSimInstance;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    CODSim* CODSim::corePtr(void)
    {
      return coreCODSimPtr;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool CODSim::consoleCall(const uint32_t argc, const ConsoleArguments argv)
    {
      bool retVal = false;
      uint32_t intVal;
      int16_t intVal16;

      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        char_t helpText[] =
          "simSlide <0|1> Disables/enables simulation of slide\n"
          "simSlip  <0|1> Disables/enables simulation of slip\n"
          "vMaxErr <int16> Sets the error value for vMax\n"
          "vMinErr <int16> Sets the error value for vMin\n";

        ATC::AbstractConsole::corePtr()->write(&helpText[0]);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "simSlip", sizeof("simSlip")) && ((argc == 1U) || (argc == 2U)))
      {
        if (argc == 2U)
        {
          if (sscanf(&argv[1][0], "%u", &intVal) == 1)
          {
            simulateSlip = intVal != 0U;
          }
        }

        char_t buffer[64];
        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "Simulation of slip is %s", simulateSlip ? "enabled" : "disabled");

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else if (ATC::isTextMatch(&argv[0][0], "simSlide", sizeof("simSlide")) && ((argc == 1U) || (argc == 2U)))
      {
        if (argc == 2U)
        {
          if (sscanf(&argv[1][0], "%u", &intVal) == 1)
          {
            simulateSlide = intVal != 0U;
          }
        }

        char_t buffer[64];
        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "Simulation of slide is %s", simulateSlide ? "enabled" : "disabled");

        if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else if (ATC::isTextMatch(&argv[0][0], "vMaxErr", sizeof("vMaxErr")) && (argc == 2U))
      {
        if (sscanf(&argv[1][0], "%hi", &intVal16) == 1)
        {
          vMaxError = intVal16;
        }

        char_t buffer[64];
        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "vMax Error is %d", vMaxError);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else if (ATC::isTextMatch(&argv[0][0], "vMinErr", sizeof("vMinErr")) && (argc == 2U))
      {
        if (sscanf(&argv[1][0], "%hi", &intVal16) == 1)
        {
          vMinError = intVal16;
        }

        char_t buffer[64];
        const int32_t res = snprintf(&buffer[0], sizeof(buffer), "vMin Error is %d", vMinError);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          retVal = true;
        }
      }
      else
      {
        // do nothing
      }

      return retVal;
    }
  }
}
