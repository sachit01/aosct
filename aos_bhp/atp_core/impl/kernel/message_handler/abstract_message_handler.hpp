#ifndef AbstractMessageHandler_hpp
#define AbstractMessageHandler_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines AbstractMessageHandler class which contains the core functionality
* of the MessageHandler
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-09    bhermans    Created
* 2016-04-03    bhermans    Removed radio_message_defs.hpp and renamed files
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-21    lantback    Make abstract constructor protected
* 2016-06-15    akushwah    Added the input argument for access function readMessage
* 2016-08-26    marlundg    Updated for ATP-Limited
* 2016-10-03    arastogi    Added event for first balise not in MA in Balise search.
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_radio_message_in.hpp"
#include "radio_message_out_position_report.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    class AbstractMessageHandler;
    /**
    * Static variable to store the single instance of AbstractMessageHandler
    *
    * Variable shall be setup during construction of the single instance used within ATP.
    * The variable is returned by corePtr() and used by the core ATP logic to access
    * adaptation objects through the core class.
    *
    * Note: During construction the variable shall be checked to guarantee that only
    *       one instance is created. Should the variable be set to non-zero the execution shall
    *       be immediately interrupted and a safe state issued.
    */
    static AbstractMessageHandler* coreMessageHandlerInstancePtr = static_cast<AbstractMessageHandler*>(NULL);

    /**
    * The class AbstractMessageHandler implements the interface defined by the ComponentBase class.
    *
    */
    class AbstractMessageHandler : public ATC::IOComponent
    {

    public:
     
      /** First Region Position in array */
      static const uint16_t region1Pos = 0U;

      /** Second Region Position in array */
      static const uint16_t region2Pos = 1U;
 
      /** Number of regions-channels */
      static const uint16_t numberOfRegions = 2U;

      /**
      * Implements the virtual runIn function.
      *
      */
      virtual void runIn(void);

      /**
      * Implements the virtual runOut function.
      */
      virtual void runOut(void);

      /**
      * RadioHandler will call readMessage to read an outgoing message prepared by MessageHandler
      *
      * @param[out] message   Message data and destination
      * @param[in] chId       Id of Radio Channel
      *
      * @return true if any message was available
      */
      bool readMessage(RadioMessageToSend& message, const uint16_t chId);

      /**
      * Remove any pending messages for the specified channel
      *
      * @param[in] chId       Id of Radio Channel
      */
      void popMessages(const uint16_t chId);

      /**
      * Validation of incoming AreaRequest
      *
      * @param[in] radioMessageToPeek   RadioMsg to validate
      * @param[in] chId      Id of Radio Channel
      *
      * @return true if a valid AreaRequest is received.
      */
      virtual bool validateAreaRequest(const RadioMessage& radioMessageToPeek, const uint16_t chId) const;
      
      /**
      * Access function to check if a RegistrationArea shall be sent
      *
      * @param[out] message   RegistrationArea radio message
      *
      * @return true if a RegistrationArea message is prepared to be sent
      */
      virtual bool getRegistrationArea(RadioMessage& message) const;

      /**
      * Validation of incoming ProtocolVersion
      *
      * @param[in] radioMessageToPeek     RadioMsg to validate
      * @param[in] chId                   Id of Radio Channel
      *
      * @return true if a valid ProtocolVersion is received.
      */
      virtual bool validateProtocolVersion(const RadioMessage& radioMessageToPeek, const uint16_t chId) const;

      /**
      * Access function to update ProtocolVersion status for a radio channel
      *
      * @param[in] versionValid status (false = invalid, true = valid)
      * @param[in] chId         Id of Radio Channel
      */
      virtual void setProtocolVersionStatus(const bool versionValid, const uint16_t chId);

      /**
      * Access function to check if a ProtocolVersionResponse shall be sent
      *
      * @param[out] message   ProtocolVersion radio message
      *
      * @return true if a ProtocoloVersion message is prepared to be sent
      */
      virtual bool getProtocolVersionResponse(RadioMessage& message) const;

      /**
      * Get the current Protocol Version in AOS
      *
      *  @return Current Protocol Version in AOS
      */
      const ProtocolVersion& getProtocolVersion() const;

      /**
      * RadioHandler will call getDefaultPositionReport to read a position report prepared by MessageHandler
      *
      *  @param[in] chId     Id of Radio Channel
      *  @param[out] message   Radio Message data and destination
      *
      *  @return true if any message was available for this channel Id (always available for region-channel)
      */
      virtual bool getDefaultPositionReport(RadioMessage& message, const uint16_t chId) const;

      /**
      * RadioHandler will call ackDefaultPositionReport to acknowledge that the variable length data of the current defaultPositionReportMsg has been sent to TCC
      * MessageHandler will only change the fixed parts of the defaultPositionReportMsg such as track, position, direction, speed, train-status etc. if the previous
      * defaultPositionReportMsg has not yet been sent to TCC
      *
      *  @param[in] chId     Id of Radio Channel
      *  @param[in] connected  True if the Radio Channel is connected
      */
      virtual void ackDefaultPositionReport(const uint16_t chId, const bool connected);
      /**
      * Access-function for any published EmAlert and its reason
      *
      *  @param[out] reason   The reason for EmergencyAlert
      *
      *  @return true if any EmergencyAlert published
      */
      virtual bool getEmAlertReason(EmAlertReasonInfo& reason) const;

      /**
      * Access-function to get the status of emergency alert set by unconditional shortening message.
      *
      *  @return true if any EmergencyAlert is set by unconditional shortening message.
      */
      virtual bool isEmAlertStatusSetByUncondMsg() const;

      /**
      * Function to indicate Approximate Position message accepted/rejected
      *
      * @return true if approximatePosition message is accepted
      */
       virtual bool getApproximatePosition() const;

      /**
      *  Returns the approximate front train position
      *
      *  @param[out] tnp   Front Track and position
      *
      *  @return true if any Approximate Position message is received during this cycle
      */
      virtual bool getApproxFrontPos(TrackAndPos& tnp) const;

      /**
      * Access-function for any published list of Area Region
      *
      *  @param[out] tccArea  The published list of Region Area
      *
      *  @return true if any text message published
      */
      virtual bool getAreaRequested(TCCAreas& tccArea) const;

      /**
      * Access-function for any published Join Command
      *
      *  @return true if any joinCommand is received
      */
      virtual bool getJoinCommand() const;

      /**
      * Access-function for any published PossessionAcknowledge
      *
      * @return PossessionAcknowledge if any PossessionAcknowledge message is published, NULL otherwise
      */
      const PossessionAcknowledge* getPossessionAcknowledge() const;

      /**
      * Access-function for any published Shunting Acknowledge
      *
      *  @param[out] shuntingAck   The published Shunting acknowledge
      *
      *  @return true if any Shunting Acknowledge is received
      */
      virtual bool getShuntingAcknowledge(ShuntingAcknowledge& shuntingAck) const;

      /**
      * Access-function for any published Yard Acknowledge
      *
      *  @param[out] yardAck   The published Yard acknowledge
      *
      *  @return true if any Yard Acknowledge is received
      */
      virtual bool getYardAcknowledge(YardAcknowledge& yardAck) const;

      /**
      * Access-function for any published RejectConfiguration
      *
      *  @param[out] info   The info associated with the RejectConfiguration
      *
      *  @return true if any RejectConfiguration published
      */
      virtual bool getRejectConfigurationInfo(RejectConfigInfo& info) const;

      /**
      * Access-function for any published RevokeEmAlert
      *
      *  @return true if any RevokeEmAlert published
      */
      virtual bool getRevokeEmAlert() const;

      /**
      * Access-function for any published Stop Train 
      *
      *  @return true if any StopTrain published
      */
      virtual bool getStopTrain() const;

      /**
      * Access-function for any published Unregistration and its info
      *
      *  @param[out] info   The info associated with the Unregistration
      *
      *  @return true if any Unregistration published
      */
      virtual bool getUnregInfo(UnregInfo& info) const;

      /**
      * Access-function for any published DriverLogonStatus and its associated info
      *
      *  @param[out] status   The info associated with the DriverLogon
      *
      *  @return true if any DriverLogonStatus published
      */
      virtual bool getDriverLogonStatus(LogonStatus& status) const;

      /**
      * Access-function for any published Movement Authority and its associated header information
      *
      *  @param[out] head   The info associated with the header of the Movement Authority
      *
      *  @return true if any MovementAuthority published
      */
      virtual bool getMAHead(MAHead& head) const;

      /**
      * Check if an MA was received in this execution cycle
      *
      *  @param[out] id   The ID of the last received MA
      *  @param[out] replyChannelId   The replyChannel to use
      *
      *  @return true if any MovementAuthority is received during this cycle
      */
      virtual bool getMAReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Get the status of if MA is from Scratch
      *
      *  @return true if MA is from scratch is received
      */
      virtual bool isMAFromScratch(void) const;

      /**
      * Check if an MA was received in this execution cycle
      *  @return true if any MovementAuthority is received during this cycle
      */
      virtual bool isValidMAReceived() const;

      /**
      * Check if Train Setup was received in this execution cycle
      *
      *  @param[out] id   The ID of the last setup msg
      *  @param[out] replyChannelId   The replyChannel to use
      *
      *  @return true if any Train Setup Data is received during this cycle
      */
      virtual bool getTrainSetupReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Return the maximum gradient for a received train setup
      *
      *  @param[out] maxGradient the maximum gradient of train setup
      *
      *  @return true if any Train Setup Data is accepted during this cycle
      */
      virtual bool getTrainSetupMaxGradient(int8_t& maxGradient);

      /**
      * Check if Train Setup was received in this execution cycle, calls virtual getTrainSetupReceived
      *
      *  @param[out] id   The ID of the last setup msg
      *
      *  @return true if any Train Setup Data is received during this cycle
      */
      bool getTrainSetupReceived(uint8_t& id) const;

      /**
      * Check if Approximate Position message was received in this execution cycle
      *
      *  @param[out] id   The ID of the last approx pos msg
      *  @param[out] replyChannelId   The replyChannel to use
      *
      *  @return true if any Approximate Position message is received during this cycle
      */
      virtual bool getApproxPosReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Check if Approximate Position message was received in this execution cycle, calls virtual getApproxPosReceived
      *
      *  @param[out] id   The ID of the last approx pos msg
      *
      *  @return true if any Approximate Position message is received during this cycle
      */
      bool getApproxPosReceived(uint8_t& id) const;

      /**
      * Check if Configuration Data was received in this execution cycle
      *
      *  @param[out] id   The ID of the last config data msg
      *  @param[out] replyChannelId   The replyChannel to use
      *
      *  @return true if any Configuration Data is received during this cycle
      */
      virtual bool getConfigDataReceived(uint8_t& id, uint16_t& replyChannelId) const;

      /**
      * Access-function for any published Text Message
      *
      *  @return true if any text message published
      */
      virtual const char_t* getTextMessage() const;

      /**
      * Access-function for any published configData
      *
      *  @return true if any configData message published
      */
      virtual bool getConfigDataReceived() const;

      /**
      * Access-function for any published Q_SETUP
      *
      *  @param[out] reason Reason for Train Setup
      *
      *  @return true if any text message published
      */
      virtual bool getQSetup(TrainSetupReason& reason) const;

      /**
      * Access-function for getting whether Train Setup message has been rejected by AOS
      *
      *  @return true, if AOS reject the incoming Train Setup message from TCC
      */
      virtual bool isTrainSetupRejectedByAOS() const;

      /**
      * Access-function for any published incoming ProtocolVersion info
      *
      *  @param[out] protocolVersionFromTCC   The version info associated with the ProtocolVersion
      *  @param[out] protocolVersionRequest   The response associated with the ProtocolVersion
      *
      *  @return true if any Protocol Version From TCC is published
      */
      virtual bool getIncomingProtocolVersionFromTCC(ProtocolVersion& protocolVersionFromTCC,
        ProtocolResponse& protocolVersionRequest) const;

      /**
      * Check if AOS Protocol Version and TCC Protocol Version matches for certain channel
      *
      * @param[in] chId   Id of Radio Channel
      *
      * @return true if Protocol Version is Matching
      */
      virtual bool isProtocolVersionMatching(const uint16_t chId) const;

      /**
      * Check if last position report for this channel has been sent to TCC (and the position report dynamic data is open for writing).
      *
      * @param[in] chId   Id of Radio Channel
      *
      * @return true if acknowledge is received for this channel.
      */
      virtual bool getAckDefaultPositionReport(const uint16_t chId) const;

      /**
      * Access function to check if Approximate Position Message is received earlier or not
      *
      * @return true if Approximate Position Message is received earlier
      */
      virtual bool isApproxPosReceivedEarlier() const;

      /**
      * Access function to check if startup message is sent to TCC or not
      *
      * @return true if Startup message is send to TCC
      */
      virtual bool isStartUpMessageSent() const;
 
      /**
      * Access-function for any published Path Message
      *
      *  @return the Path message if any Path message is published, NULL otherwise
      */
      const Path* getPath() const;

      /**
      * Access-function for any published Initiate Configuration
      *
      *  @param[out] initiateConfigValue    The published Initiate Configuration
      *
      *  @return true if any initiate Configuration is available
      */
      virtual bool getInitiateConfig(InitiateConfigReason& initiateConfigValue) const;

      /**
      * Access function to update the Message Handler's Registration Area for need to send reply to TCC
      *
      * @param[in] isNeeded is there a need to reply to TCC
      */
      virtual void setReplyRegAreaToTCC(const bool isNeeded);

      /**
      * Access function to acquire if the RegArea is selected by the driver 
      *
      *  @param[out] areaId selected area id
      *
      *  @return true if RegArea is selected by driver
      */
      virtual bool getRegAreaSelectedByDriver(uint8_t& areaId) const;

      /**
      * Access function to update area ID selected by the driver
      *
      * @param[in] areaId selected by driver
      */
      virtual void setRegAreaSelectedByDriver(const uint8_t areaId);
      
      /**
      * Access function to clear area ID selected by driver
      */
      virtual void clearAreaSelectedByDriver();

      /**
      * Access function to update the DMI Time.
      *
      * @param[in] updateNeeded Is there a need to update DMI Time
      */
      void setUpdateDMITime(const bool updateNeeded);

      /**
      * Update the system Time by calling utility function that calls setTimeOfDay application.
      *
      * @param[in] newTime Time to be set as system time
      *
      */
      void updateSystemTime(const uint32_t newTime);

      /**
      * Get function for the time to set as system time
      *
      * @return new time to set as system time that is received from TCC
      */
      uint32_t getTimeNew() const;

      /**
      * Get function for the time at which the system time update request was received
      *
      * @return the time at which the update time request was received
      */
      uint32_t getTimeAtReq() const;

      /**
      * Access function to check if update the DMI Time is needed.
      *
      * @return returns update DMI Time Flag
      */
      bool getUpdateDMITime() const;

      /**
      * Access function to check if any partly MA has been received and validated
      *
      * @return returns true  if partly MA is received and validated
      */
      bool getPartlyMaReceived() const;

      /**
      * Get the max search distance for Balise in Re-registration
      *
      * @param[out] maxDist   Maximum distance to search in re-reg
      *
      *  @return true if max search distance received in MA
      */
      virtual bool getMaxSearchDistInReReg(uint32_t& maxDist) const;

      /**
      * Get core instance pointer
      *
      * @return Pointer to single instance core object.
      */
      static AbstractMessageHandler* corePtr();

      /**
      * Access function to check if configuration data message has been received
      *
      * @return returns true  if configuration data is received
      */
      bool isConfigDataReceived() const;
      
      /**
      * Access function to acquire which channel made INITIATE_CONFIG in PRR
      *
      * @param[out] chId   Id of Radio Channel   
      *
      */
      void getReplyChannelInitConfig(uint16_t& chId) const;

      /**
      * Access function to update which channel INITIATE_CONFIG in PRR
      *
      *  @param[in] chId   Id of Radio Channel
      */
      void setReplyChannelInitConfig(const uint16_t chId);

      /**
      * Access function to check if RegistrationArea message has been sent to TCC or not
      *
      * @return current value of regAreaMsgSendToCentralTCC
      */
      bool getRegistrationAreaMessageSentToCentralTCC() const;

      /**
      * Access function to set the flag for RegistrationArea Message indicating whether message 
      * is send to Central TCC or not
      *
      *  @param[in] valueToSet   value to set to registrationAreaMessageSend variable
      */
      void setRegistrationAreaMessageSentToCentralTCC(const bool valueToSet);

      /**
      * Function to indicate Approximate Position message rejected
      *
      * @return true if approximatePosition message is rejected
      */
      bool isApproximatePositionMsgRejected() const;

      /**
      * Get the departure signal
      *
      * @param[out] acousticSignal   type of acoustic signal
      *
      *  @return true, if qSignal is received in MA
      */
      bool getDepartureSignal(AcousticSignal& acousticSignal) const;

    protected:
      /**
      * Constructor
      */
      AbstractMessageHandler();

      /**
      * Compensates the stored values for Brake- and BTM test.
      *
      * @param[in] diffTime - The time to be added to the stored time.
      *
      */
      void compensateCalendarCalculations(const int32_t diffTime) const;

      /**
      * Validation of incoming Message
      *
      * @param[in] msgType              Expected Message Type
      * @param[in] radioMessageToPeek   RadioMsg to validate
      * @param[in] chId                 Id of Radio Channel
      *
      * @return true if a valid message of msgType is received.
      */
      virtual bool validateMsgType(const RadioMessageType msgType, const RadioMessage& radioMessageToPeek, const uint16_t chId) const;

      /**
      * The container of the pointers to parsers for incoming messages
      */
      AbstractRadioMessageIn  **messagesInParsers;

      /**
      * The container of the pointers to creators of outgoing messages
      */
      AbstractRadioMessageOut **messagesOutCreators;

      /**
      * Receive queue is full
      */
      const ATC::Event receiveQueueFullError;

      /**
      * Send queue is full
      */
      const ATC::Event sendQueueFullError;

      /**
      * Validation of incoming message failed
      */
      const ATC::Event validationIncomingMessageFailed;

      /**
      * Train Setup Message rejected by AOS
      */
      const ATC::Event trainSetupRejected;

      /**
      * Validation of outgoing message failed
      */
      const ATC::Event validationOutgoingMessageFailed;

      /**
      * Invalid message type received
      */
      const ATC::Event invalidMessageType;

      /**
      * Parser for this message type is not implemented
      */
      const ATC::Event parserNotImplemented;

      /**
      * Parser for this messagetype is NULL
      */
      const ATC::Event parserNullPtr;

      /**
      * No valid positionreport available
      */
      const ATC::Event noValidPositionreport;

      /**
      * Initializes the cross compare module. Called by the init function.
      */
      virtual void initCrossCompare() const;

    private:

      /**
      * The queue of incoming messages
      */
      std::vector<RadioMessageReceived> messageQueueIn;

      /**
      * The queue of outgoing messages
      */
      std::vector<RadioMessageToSend> messageQueueOut;

      /**
      * Protocol version matching status for a certain region/channel (true = valid, false = invalid)
      */
      bool protocolVersionValid[numberOfRegions];

      /**
      * Approximate Position received Earlier or not from TCC
      */
      bool approxPosReceivedEarlier;

      /**
      * Approximate Position rejected by AOS
      */
      bool approxPosRejected;

      /**
      * Configuration data received Earlier or not from TCC
      */
      bool configurationDataRecv;

      /**
      * The default position report is always available for the different regions
      *
      * This data is encoded as messages in @ref defaultPositionReportMsg
      */
      PositionReport defaultPositionReport[numberOfRegions];

      /**
      * The default position report for the different regions
      *
      * This is the message encoded from the data in @ref defaultPositionReport
      */
      RadioMessageToSend defaultPositionReportMsg[numberOfRegions];

      /**
      * Flag to tell if at least one position report is created for the actual region. 
      */
      bool validPositionReport[numberOfRegions];

      /**
      * Registration Area
      */
      RadioMessageToSend registrationArea;

      /**
      * Is there a need to send Registration Area to TCC. Is there any incoming request area
      * message from TCC.
      */
      bool isSendRegAreaToTCC;

      /**
      * Is there a need to update Time to DMI
      */
      bool updateDMITime;

      /**
      * New time to set in ATP and DMI
      */
      uint32_t newTimeToSet;

      /**
      * The time at which the request to update time was was received
      */
      uint32_t timeAtReq;

      /**
      * Is registration area selected by driver
      */
      bool isRegAreaSelectedByDriver;

      /**
      * Selected RegArea
      */
      uint8_t selectedRegArea;

      /**
      * Position for NID_MSG_TYPE
      */
      static const uint8_t nidMessageTypePos = 0U;

      /**
      * Sizeof MessageHandler inqueue.
      */
      static const uint8_t inQueueSize = 5U;

      /**
      * Sizeof MessageHandler outqueue.
      */
      static const uint8_t outQueueSize = 5U;

      /**
      * Maximum size of the Protocol version message data
      */
      static const uint8_t maxProVerMsgData = 6U;

      /**
      * Message NID Id of the Protocol version message
      */
      static const uint8_t msgNidProVer = 200U;

      /**
      * Block NID Id of the Protocol version Block
      */
      static const uint8_t blockNidProVer = 2U;

      /**
      * Reply channel for INITIATE_CONFIG in PRR
      */
      uint16_t replyChInitConfig;

      /**
      * Flag to check whether RegistrationArea Message has been Sent to TCC or not
      */
      bool regAreaMsgSendToCentralTCC;

    };
  }
}

#endif
