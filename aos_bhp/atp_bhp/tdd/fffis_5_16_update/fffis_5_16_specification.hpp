/**
\if AsMainPage
\mainpage Technical Design Description for FFFIS-AOS(5.16) Update
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2018-03-15 | Document Created to support FFFIS-AOS upgrade to 5.16  | spandita


\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TCC            | Train Control Center
FFFIS          | Form Fit Functional Interface Specification 


\section Introduction Introduction
This document describes the technical design description for the changes in AOS related to up-gradation of FFFIS-AOS version 5.16 document.

<B>Note:</B> Below mentioned code are only meant for understanding. Please ignore the lint,compiler warnings.


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement below mentioned changes.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1      | FFFIS TCC-AOS                                      | 5.16


\subsection TCCToAOSChanges Design Change Overview for the messages from TCC to AOS:

\subsection  ApproximatePosition   ApproximatePosition 

+ Refer \ref PARTLY_TRACK_DATA for partly track data blocks.
+ Create a bool member("approxPartlyTrackData") in ApproxPos structure in radio_message_types.hpp with following code.
\code
    struct ApproxPos
    {
      uint8_t                     approxPosId;
      TrackAndPos                 approxTrackAndpos;
	  bool 						  approxPartlyTrackData;
      std::vector<TrackData>      approxTrackDataVec;
    };
\endcode 
+ Create a private member as partlyTrkDataRecInPrevCycle in RadioMessageInApproximatePosition class.
+ Create the switch statement for approxPartlyTrackData field in parseMessageData function of RadioMessageInApproximatePosition class.
+ Set the approxPartlyTrackData and partlyTrkDataRecInPrevCycle to true in case statement of approxPartlyTrackData field in parseMessageData function of RadioMessageInApproximatePosition class.
+ Reset the partlyTrkDataRecInPrevCycle every time at the start of parseMessageData function of RadioMessageInApproximatePosition class.
+ Need to check for approxPartlyTrackData status and partlyTrkDataRecInPrevCycle status in validateApproxPosMessage function of RadioMessageInApproximatePosition class
 for train footprint validation.
 \code
 
   if ((lastTrackID == approxPosData.approxTrackAndpos.track) && (!isTrainStartFound) && (!approxPartlyTrackData)  && partlyTrkDataRecInPrevCycle  )
          {
 \endcode
 
+ Need to the check of approxPartlyTrackData and partlyTrkDataRecInPrevCycle status in publishTracks function of RadioMessageInApproximatePosition class to avoid any publish of data.
 \code
 if(approxPartlyTrackData && partlyTrkDataRecInPrevCycle)
 {
	 //do nothing
 }
 else
 {    approxPartlyTrackData = true;
	 //publish the tracks
 }
 
 \endcode

+ Need to check the status of  approxPartlyTrackData in invalidate function of RadioMessageInApproximatePosition class before clearing the trackdata vector.

\subsection CommandMessage CommandMessage	 

+ Refer \ref  SAFE_FOR_BOARDING_ACTIVATE for deletion of safe for boarding activate block.
+ Delete the safeForBoardingActivateVec member in CommandMessage structure of radio_message_in_command_message.hpp.
+ Delete the related code from constructor and invalidate function of RadioMessageInCommandMessage class.
+ Delete switch case statement "BTypeSafeForBoardingActivate" from parseMessageData function of RadioMessageInCommandMessage class.

\subsection PositionReportRequest PositionReportRequest
+ Create validateQ_INITIATE(const uint8_t val) function in abstract_radio_message_common.hpp.
\code
//abstract_radio_message_common.cpp
    bool validateQ_INITIATE(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(ConfigKnownByTCC));
    }

\endcode
+ Refer \ref Q_INITIATE for q_initiate field in PRR message.
+ Update the switch statement of "BTypeInitiateConfig" in parseMessageData function of RadioMessageInPositionReportRequest class.
\code 
        if (!validateQ_INITIATE(tmpValU8))
          {
            trace->write(ATC::detailedTrace, "Q_INITIATE invalid");
            parseDataValid = false;
          }
          else
          {
            positionReportRequest.initiateConfigData.qInitiate = static_cast<QInitiate>(tmpValU8);
          }

\endcode

+ Update the argument of getInitiateConfig function with variable of q_initiate in PositionReportRequest and AbstractMessageHandler class.
+ Update the data type of the protected member "initiateConfig"  to QInitiate in AbstractMode class.
+ Set the initiateConfig to ConfigUnknownByTCC in AbstractMode constructor.
+ Update the runPowerUpWaitConfigOrSleep function in PowerUpMode class with following code
\code
else if (isConfigButtonPressed ||
        (isValidInitiateConfigReceived && (initiateConfig == ConfigKnownByTCC) ||
        && isDriverLoggedIn))

\endcode
+ Update the runTrainConfigWaitQSetup function of TrainConfigMode class with following code
\code
   if (TG::AbstractTIC::corePtr()->getTICAvailable() && (ConfigUnknownByTCC == initiateConfig))
	   
\endcode

+ Submodes need to be created for TrainConfigMode class in order to adapt the changes for initiate config.(as currently we are handling different submodes of config depends upon the initiate config).(will be covered while implementation)
+ Reset the value of initiateConfig to ConfigUnknownByTCC in resetMode function of PowerUpMode class.

\subsection StopTrain StopTrain
+ Refer \ref Q_STOP for deletion of q_stop field.
+ Delete private member stopTrainReason and defaultValue from RadioMessageInStopTrain class.
+ Delete the parsing of stopTrainReason field in parseMessageData function of RadioMessageInStopTrain class.
+ Delete validateQ_STOP function from abstract_radio_message_common.cpp file.
+ Delete the argument("reason") from getStopTrain function of AbstractMessageHandler and RadioMessageInStopTrain class.
+ Update the getStopTrain function of RadioMessageInStopTrain class with following lines
\code

    bool RadioMessageInStopTrain::getStopTrain() const
    {
      return (DataValidated == dataProcessState);
    }
\endcode
+ Remove below lines from collectData function of DMIMessageOutTextMessage class.
\code
      else if (Kernel::AbstractMessageHandler::corePtr()->getStopTrain(stopTrainReason))
      {
        //Stop Train message received from TCC
        char_t indexInstring[maxDmiTextMsgIndexlen] = { '\0' };
        int32_t retValue = 0;
        switch (stopTrainReason)
        {

        case Kernel::StopTrainUndefined:
          //Convert the number to string
          retValue = snprintf(&indexInstring[0], maxDmiTextMsgIndexlen, "%u", DMICom::msgHdlrStopTrainUndefined);
          break;

        case Kernel::StopTrainCancelled:
          //Convert the number to string
          retValue = snprintf(&indexInstring[0], maxDmiTextMsgIndexlen, "%u", DMICom::msgHdlrStopTrainCancelled);
          break;

        default:
          break;
        }

        //check if read value is greater than zero
        if (retValue >= 0)
        {
          //Add the specifier:”$#$”
          static_cast<void>(vfw_strlcpy(&dmiTextMsgIndex[0], formatIndex, sizeof(dmiTextMsgIndex)));
          //Concatenate the id with $#$
          static_cast<void>(vfw_strlcat(&dmiTextMsgIndex[0], &indexInstring[0], sizeof(dmiTextMsgIndex)));
          dmiDataProcessState = DMIDataAvailable;
        }

      }

\endcode
+ Also need to update the english.ini for deleted ID's.
+ Update the calls of getStopTrain function in whole code of ATP.

\subsection AOSToTCCChanges Design Change Overview for the messages from AOS to TCC:

\subsection  PositionReport  PositionReport
+ Refer \ref MESSAGE_ACKNOWLEDGE for message acknowledge block.
+ Add the variable for MessageAck structure to PositionReport structure in radio_message_out_position_report.hpp.
\code 
 struct PositionReport
    {   .
		.
		std::vector<CancelArea>         cancelAreaVec;
		MessageAck                       msgAck;
		.
		.
	}
\endcode

+ Create private member function named as "collectMsgAckData" in the RadioMessageOutPositionReport class.
+ Call "collectMsgAckData" function in collectData() function of RadioMessageOutPositionReport class( need to check ackDefaultPositionReportReceived flag before calling collectMsgAckData function).
+ In "collectMsgAckData" function copy paste the code from collectData function of RadioMessageOutMessageAcknowledge class(change would be only storage of data in PositionReport structure member that is msgAck).
+ In invalidate function of RadioMessageOutPositionReport class invalidate the msgAck member (Should be done inside of ackDefaultPositionReportReceived block).



\subsection MessageAcknowledge MessageAcknowledge
+ Remove radio_message_out_message_acknowledge.cpp and radio_message_out_message_acknowledge.hpp file.
+ Remove MTypeMessageAcknowledge member from RadioMessageType enum in radio_message_types.hpp file.
+ Set MTypeRegistrationAreaMessage member of RadioMessageType enum to 134 value in radio_message_tyoes.hpp.


\subsection Block Design Change Overview for Blocks 

\subsection SAFE_FOR_BOARDING_ACTIVATE SAFE_FOR_BOARDING_ACTIVATE
+ Delete the structure SafeForBoardingActivate from radio_message_types.hpp.
+ Remove the member "BTypeSafeForBoardingActivate" from RadioBlockType enum.
+ Set the value 22 to BTypeDepartureWarning member of RadioBlockType enum.

\subsection INITIATE_CONFIG INITIATE_CONFIG
+ Create a member of QInitiate("Q_INITIATE") after blockType member in InitiateConfig structure of radio_message_types.hpp file.
+ Refer \ref Q_INITIATE for more details.
+ Delete the statusOfTrainConfig from InitiateConfig structure.

\subsection  MESSAGE_ACKNOWLEDGE  MESSAGE_ACKNOWLEDGE
+ Add "BTypeMessageAcknowledge" at location 44 in RadioBlockType enum of radio_message_types.hpp.
+ Create structure "MessageAck" in radio_message_types.hpp.
+ Refer below code for more details.
\code
    struct MessageAck
    {
      static const uint8_t blockType = static_cast<uint8_t>(BTypeMessageAcknowledge);
	  uint8_t msgId;
      bool    messageAccepted;
 
    };

\endcode

\subsection   PARTLY_TRACK_DATA  PARTLY_TRACK_DATA
+ Add "BTypePartlyTrackData" at location 45 in RadioBlockType enum of radio_message_types.hpp.
+ Create structure "PartlyTrackData" in radio_message_types.hpp.
+ Refer below code for more details. 
\code
    struct PartlyTrackData
    {
      static const uint8_t blockType = static_cast<uint8_t>(BTypePartlyTrackDatas);
   };

\endcode
\subsection Field Design Change Overview for Fields

\subsection B_TRAIN_CORE_STATUS B_TRAIN_CORE_STATUS
+ Replace the bit 18 to not used (initialized to 0 value).

\subsection Q_ALERT Q_ALERT
+ update the EmAlertReason enum in radio_message_types.hpp with "Detector triggered" member (at position 8).

\subsection Q_INITIATE Q_INITIATE
+ Create a enum as QInitiate in radio_message_types.hpp
\code

enum QInitiate
{
ConfigUnknownByTCC = 0,
ConfigKnownByTCC	

};
\endcode

\subsection NID_VEHICLE_TYPE NID_VEHICLE_TYPE
+ Update the VehicleType enum in atp_types.hpp file as per the FFFIS-TCC version 5.16.

\subsection Q_SIGNAL Q_SIGNAL
+ Update AcousticSignal enum in radio_message_types.hpp file as per the FFFIS-TCC version 5.16.

\subsection Q_SPEED Q_SPEED
+ Update SpeedChangeReason enum in atp_types.hpp file with "restrictive section" field as per the FFFIS-TCC version 5.16.

\subsection Q_UNREGISTRATION Q_UNREGISTRATION
+ Update the UnregInfo enum in radio_message_types.hpp as per the FFFIS-TCC ver5.16.

\subsection Q_REJECT_CONFIGURATION Q_REJECT_CONFIGURATION
+ Update the RejectConfigInfo enum in radio_message_types.hpp with "Rejected by dispatcher" field as per the FFFIS-TCC ver5.16.

\subsection Q_STOP Q_STOP
+ Remove the StopTrainReason enum from radio_message_types.hpp file.


\subsection Miscellaneous Changes

\subsection ConfigurableParameters Configurable Parameters
+ Need to move following code from init function of config.hpp file to abstract_config.hpp.
\code
      setGlobalNameToConfigParameter(balSearchSpeedId, "V_BALISESEARCH");
      setGlobalNameToConfigParameter(balSearchDistanceId, "D_BALISESEARCH");
      setGlobalNameToConfigParameter(yardSpeedId, "V_YARD");
      setGlobalNameToConfigParameter(rollAwayMarginId, "D_ROLLAWAY");
      setGlobalNameToConfigParameter(revSupMarginShuntId, "D_REVERSINGSHUNTING");
      setGlobalNameToConfigParameter(radioTimeOutId, "T_RADIOTIMEOUT");
      setGlobalNameToConfigParameter(fwToSwDelayId, "T_FIRSTWARNING");
      setGlobalNameToConfigParameter(swToSBDelayId, "T_SECONDWARNING");
      setGlobalNameToConfigParameter(maxSpeedMarginSBId, "V_SBOVERSPEED");
      setGlobalNameToConfigParameter(maxSpeedMarginEBId, "V_EBOVERSPEED");
\endcode
+ Need to add similar code for following parameters

+ Pm_ODOINACCURACY (Balise window percentage "balWindowPercentId")
+ S_VEHICLETYPE1 (1 to 20) (New Addition)
+ T_DEPARTUREWARNING (New Addition)
+ T_RADIOTIMEOUTBRAKE ("radioTimeOutSbId")




*/