#ifndef RadioMessageOutPositionReport_hpp
#define RadioMessageOutPositionReport_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutPositionReport creator is responsible for collecting
*  position-report data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_position.hpp"
#include "abstract_radio_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * Defines the storage of the collected information for PositionReport
    */
    struct PositionReport
    {
      TrackAndPos                     trailingTrackAndPosition;  //!< Train trailing position
      TrackAndPos                     leadingTrackAndPosition;   //!< Train leading position
      Pos::PosAccuracyState           positionClassification;    //!< Classification of position
      uint8_t                         orientationAndDirection;   //!< Travel direction and train orientation
      uint16_t                        trainSpeed;                //!< Speed of the train
      uint32_t                        trainCoreStatus;           //!< Status of the train as a bit field
      uint16_t                        frontConfidenceInterval;   //!< Current train front confidence interval
      uint16_t                        rearConfidenceInterval;    //!< Current train rear confidence interval
      TrackAndPos                     maTargetTrackAndPosition;  //!< Current MA target
      ATPMode                         atpMode;                   //!< Current ATP mode
      uint8_t                         atoMode;                   //!< Current ATO mode
      BrakeSystemType                 brakeSystem;               //!< Defines the type of brake system currently active
      bool                            possesionRequestReceived;  //!< Request to enter Possession mode
      bool                            shuntingRequestReceived;   //!< Request to enter Shunting mode
      bool                            yardRequestReceived;       //!< Request to enter Yard mode
      LastBalise                      lastBaliseReceived;        //!< Last read balise
      TrainName                       trainName;                 //!< Train name
      bool                            trainNameReceived;         //!< Driver request to change train name 
      bool                            etaConfirmationReceived;   //!< Request of Confirmation of arrival time
      ETAConfirmation                 etaConfirmation;           //!< Confirmation of arrival time
      bool                            cancelAreaReceived;        //!< Request to be transferred back to central TCC
      MessageAck                      msgAck;                    //!< This block is an acknowledge of the message NID_MSG.
      std::vector<StopDistData>       stopDistDataVec;           //!< Stop distance at different gradients
      std::vector<EventData>          eventDataVec;              //!< Event data
      std::vector<EventDataText>      eventDataTextVec;          //!< Event data with text
      bool                            externalDataReceived;      //!< Received Data from external AOS system
      ExternalData                    externalData;              //!< Data from external AOS system
    };

    /**
     * RadioMessageOutPositionReport is a creator for the outgoing PositionReport message
     */
    class RadioMessageOutPositionReport : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing PositionReport message
      *
      *  @param[in] chId   The channel-ID associated with this position report.
      *
      */
      RadioMessageOutPositionReport(const uint16_t chId);

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * RadioHandler will call ackDefaultPositionReport to acknowledge that the variable length data
      * of the current defaultPositionReportMsg has been sent to TCC
      * MessageHandler will only change the fixed parts of the defaultPositionReportMsg such as track,
      * position, direction, speed, train-status etc. if the previous
      * defaultPositionReportMsg has not yet been sent to TCC
      */
      virtual void ackDefaultPositionReport();

      /**
      * Get output channel id
      *
      * @return  channeld Id of the message
      */
      virtual uint16_t getChannelId() const;

      /**
      * Check if last position report has been sent to TCC (and the position report dynamic data is open for writing).
      *
      * @return true if acknowledge is received
      */
      virtual bool getAckDefaultPositionReport() const;

      /**
      * Returns the collected data used to create the outgoing message
      *
      * @return the collected data used to create the outgoing message
      */
      const PositionReport& getPositionReport() const;

    protected:

      /**
      * Collect Position Data for Position report message
      */
      void collectPositionData();

      /**
      * Assemble the additional blocks in adaptation
      *
      * @param[out] buffer  The buffer to be assembled
      */
      virtual void assembleAdditionalBlocks(VFW_Buffer &buffer);

    private:

      /**
      * Constructor for the creator of the outgoing PositionReport message
      */
      RadioMessageOutPositionReport();

      /**
      * Collect Message Acknowledge data
      */
      void collectMsgAckData();

      /**
      * Collect stopping distance data
      */
      void collectStopDistData();

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleMessageData();

      /**
      * Max number of eventData blocks
      */
      static const uint8_t eventDataSize = 50U;

      /**
      * Max number of errorMessageData blocks
      */
      static const uint8_t eventDataTextSize = 10U;

      /**
      * Max number of stopping distance blocks
      */
      static const uint8_t stopDistDataSize = 255U;

      /**
       * The collected data used to create the outgoing message
       * Will be cleared each ATP execution-cycle by invalidate()
       */
      PositionReport positionReport;

      /**
      * Flag to indicate slip clear status reported to TCC with front/rear train position
      */
      bool isSlipClearStatusReportedToTCC;

      /**
      *  RadioHandler will call ackDefaultPositionReport to acknowledge that the variable length
      *  data of the current defaultPositionReportMsg has been sent to TC
      */
      bool ackDefaultPositionReportReceived;

      /**
      * Reply channel ID to use for acknowledge for this instance of Position Report
      */
      const uint16_t dedicatedChannelId;

      /**
      * Boolean for message acknowledge to send
      */
      bool msgAckToSend;

    };
  }
}
#endif
