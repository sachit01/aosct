/**
\if AsMainPage
\mainpage Technical Design Description for FFFIS-AOS(5.12) Update
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2018-01-29 | Document Created to support FFFIS-AOS upgrade to 5.12  | spandita


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
This document describes the technical design description for the changes in AOS related to up-gradation of FFFIS-AOS version 5.12 document.

<B>Note:</B> Below mentioned code are only meant for understanding. Please ignore the lint,compiler warnings.


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement below mentioned changes.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1      | FFFIS TCC-AOS                                      | 5.12


\subsection TCCToAOSChanges Design Change Overview for the messages from TCC to AOS:

\subsection PositionReportRequest PositionReportRequest

 + Need to change field name from CONFIRM_CONFIG to INITIATE_CONFIG and do the same update in all the places in AOS software.
 + Need to put the idle check before putting the value in waiting time variable in RadioMessageInPositionReportRequest::parseMessageData() function.(requirements will come in upcoming drops to handle this field)
	  \code
	  if(Kernel::AbstractModeControl::corePtr()->getIdleState())
	  {
	   positionReportRequest.waitingTimeData.estimatedTimeToWait = vfwGetU8(&buffer);
      }
     \endcode
	 
\subsection PossessionAcknowledge PossessionAcknowledge	 

+ Create private vector variable and reserve the size of 20 element(will get changed once it is confirmed by requirement engineers).
\code
std::vector<BaliseData>        baliseDataVec;
\endcode

+ Parse the field of BALISE_IDENTITY in parseMessageData function of RadioMessageInPossessionAcknowledge class and store the value in baliseDataVec variable.
+ Create a protected function in RadioMessageInPossessionAcknowledge class named as validateBalises.
+ Call the validateBalises after validateMode() function in validate() function of RadioMessageInPossessionAcknowledge class.
+ In validateBalises function check for continuous balise and duplication of balise ID.
+ Reject the message incase of any mismatch/error.
+ Separate TDD will be created to explain the persistent storage of balise data.

\subsection RejectConfiguration RejectConfiguration

+ Create an enum in file radio_message_types.hpp and declare the member as per the value defined in Q_REJECT_CONFIGURATION block in FFFIS-AOS 5.12.
	  \code
	  
    enum RejectConfigInfo
    {
      UnknownVehicleInTrain = 0,
      VehicleIsPartOfAnotherTrain= 1,
      ToManyVehicleInTrain = 2,
      DuplicatedVehicleID = 3,
      AOSEquippedVehicleNotInSleep = 4,
    };
     \endcode
+ Correct the type of variable rejectConfigReason to RejectConfigInfo enum type.
+ Do the necessary changes wrt the type of RejectConfigInfo variable in all the places in AOS software.
+ Create validateQ_REJECTCONFIGURATION function in abstract_radio_message_common.cpp file.
\code
    bool validateQ_REJECTCONFIGURATION(const uint8_t val)
    {
      return(val <= static_cast<uint8_t>(AOSEquippedVehicleNotInSleep));
    }
\endcode

\subsection TrainSetup TrainSetup
+ Remove the field ticAvailable from the class TrainSetup in train_setup.hpp file.
+ Remove all initialization of ticAvailable field in all the places in AOS.
+ Remove validateQ_TIC_AVAILABLE function from abstract_radio_message_common.cpp file.
+ Remove the enum TicStatus from radio_message_types.hpp file.
+ Remove the below code from RadioMessageInTrainSetup::parseMessageData function.
\code
      // Read & validate Q_TIC_AVAILABLE
      tmpValU8 = vfwGetU8(&buffer);
      if (!validateQ_TIC_AVAILABLE(tmpValU8))
      {
        trace->write(ATC::detailedTrace, "Q_TIC_AVAILABLE invalid");
        parseDataValid = false;
      }
      else
      {
        trainSetup.ticStatus = static_cast<TicStatus>(tmpValU8);
      }

\endcode

+ Rename the validateQ_TIMS_AVAILABLE function to validateQ_TIMS_SUPERVISION
+ Rename members and name of TimsStatus enum from radio_message_types.hpp file as per the FFFIS-AOS version 5.12
\code
 enum TimsSupStatus
    {
      TimsSupNotRequired = 0,
      TimsSupRequired = 1
    };

\endcode

+ Remove the trainBrakeSystem field from TrainSetup Structure in radio_message_in_train_setup.hpp.
+ Remove the brakeSystem from TrainSetup class in train_setup.hpp.
+ Remove in all the places where brake system is used from train setup.
+ Remove the parsing of M_BRAKE_SYSTEM field from parseMessageData function in RadioMessageInTrainSetup class.
+ Add the parsing of VEHICLE_TYPE_DATA field for brake system 3 in parseMessageData function of RadioMessageInTrainSetup class.
\code
    vehicleTypeData.brakeWeightLoadedBrakeSystem3 = vfwGetU16(&buffer);
    vehicleTypeData.brakeWeightEmptyBrakeSystem3 = vfwGetU16(&buffer);
\endcode

+ Rename the radio block member("BTypeVehicleIdData") to "BTypeVehicleData" in the RadioBlockType enum of radio_message_types.hpp file.
+ Update the parsing of VEHICLE_DATA in parseMessageData function in RadioMessageInTrainSetup class.
+ Update the name of "VehicleIdData" structure to "VehicleData" in radio_message_types.hpp file.
+ Add the parsing step for N_VALUE block in VEHICLE_DATA field in parseMessageData function in RadioMessageInTrainSetup class.
+ Remove the VEHICLE_LIST_DATA parsing from RadioMessageInTrainSetup class and its usage from AOS code.
+ Remove the radio block member("BTypeVehicleListData") from RadioBlockType enum of radio_message_types.hpp file.
+ Remove the "VehicleListData" structure from radio_message_types.hpp file.


\subsection MovementAuthority  MovementAuthority

+ Replace the members field named as "RtLocationStart, RtLocationEnd" to "not used field" in RouteType enum in atp_types.hpp file.
+ Remove the case statements in overall AOS code where "RtLocationStart/RtLocationEnd" has been being checked.
+ In target component, Remove the checks for route type to "RtLocationStart/RtLocationEnd" especially in getPrimaryTarget function.
+ Remove the "getLocationEndTarget and getLocationStartTarget" functions from abstract target class.
+ Create a new function named as "validateLocationData" in RadioMessageInMovementAuthority class.
+ Remove the checks for location mode targets from validatePrimaryTargetData and copied it to newly created function validateLocationData of RadioMessageInMovementAuthority class.
+ Create a new radio block member("BTypeLocationBorder") at 35th position in the RadioBlockType enum of radio_message_types.hpp file.
+ Create a structure for LOCATION_BORDER field in radio_message_types.hpp file.
\code 
    struct LocationBorder
    {
      static const uint8_t blockType = static_cast<uint8_t>(BTypeLocationBorder);
      TrackAndPos  startOfLocation;
      TrackAndPos  endOfLocation;
      uint16_t     allowedSpeed;
	  int8_t       gradientTwrdsLocStart;
      int8_t       gradientTwrdsLocEnd;
	  };

\endcode
+ Create a new class named as location target in target component.
+ Create a new function as publishLocationData in RadioMessageInMovementAuthority class.
+ Publish the data location data to location data target class in publishLocationData function of RadioMessageInMovementAuthority class.
+ Need to create private member function in location target class to store the LOCATION_DATA field of MA (applicable only when first MA with LOCATION_BORDER is received).
+ Correct the enum of LocationType in radio_message_types.hpp as per the FFFIS-AOS ver 5.12.
+ In parseMessageData function of class RadioMessageInMovementAuthority do the following changes:
\code
      // Fetch next data-block until M_END_OF_MESSAGE
      while ((nextMsgIdentifier != M_END_OF_MESSAGE) && (parseDataValid))
      {
        switch (nextMsgIdentifier)
        {
		 
	      case BTypeLocationBorder:
		  LocationBorder locBorder;
		  locBorder.startOfLocation.track = vfwGetU16(&buffer); 
		  locBorder.startOfLocation.position = vfwGetI32(&buffer); 
		  locBorder.endOfLocation.track = vfwGetU16(&buffer); 
		  locBorder.endOfLocation.position = vfwGetI32(&buffer); 
		  locBorder.allowedSpeed = vfwGetU16(&buffer); 
          locBorder.gradientTwrdsLocStart = vfwGetI8(&buffer);
          locBorder.gradientTwrdsLocEnd = vfwGetI8(&buffer);				
		  
		}
        // Fetch next msg-type (or M_END_OF_MESSAGE)
        nextMsgIdentifier = vfwGetU8(&buffer);
	  }
\endcode

+ Remove the code for parsing of  Keep Track Data block from parseMessageData function in RadioMessageInMovementAuthority class.
+ Remove the radio block member("BTypeKeepTrackData") from the RadioBlockType enum of radio_message_types.hpp file.
+ Remove publishKeepTrackData function from RadioMessageInMovementAuthority class.
+ Remove keepTrackDataVec vector from RadioMessageInMovementAuthority class.

\subsection AOSToTCCChanges Design Change Overview for the messages from AOS to TCC:

\subsection  PositionReport  PositionReport
+ Remove the brakeDistance member variable from the structure PositionReport in radio_message_out_position_report.hpp.
+ Remove all the code in radio_message_out_position_report.cpp for brakeDistance variable of positionReport member in RadioMessageOutPositionReport class.
+ Remove AosVersion structure from radio_message_types.hpp file.
+ Remove aosVersionVec member from PositionReport structure in radio_message_out_position_report.hpp.
+ Delete the getAosVersion function from abstract_radio_message_common.cpp file.
+ Update the vfwGetU32 function to vfwGetU64 function while parsing of ETA_CONFIRMATION block in parseMessageData function of RadioMessageOutPositionReport class. 
+ Add vector member "cancelAreaVec" in PositionReport structure in radio_message_out_position_report.hpp.
+ Reserve the size one to cancelAreaVec vector member in constructor of RadioMessageOutPositionReport class.
+ Add following code in assembleMessageData function of RadioMessageOutPositionReport class.
\code
      //Cancel Area Blocks
	  for (std::vector<CancelArea>::iterator it = positionReport.cancelAreaVec.begin();
        it != positionReport.cancelAreaVec.end(); ++it)
      {
        vfwPutU8(&buffer, CancelArea::blockType);
      }
\endcode


\subsection  StartUpMessage  StartUpMessage
+ Rename the ticStatus member to configSource.
+ Update the same in the all the places where ticStatus member is used in the code.
+ Remove the vehicleListDataVec member from StartUpMessage structure in radio_message_out_startup_message.hpp file.
+ Remove the radio block member("BTypeVehicleListData") from RadioBlockType enum of radio_message_types.hpp file.
+ Remove the "VehicleListData" structure from radio_message_types.hpp file.
+ Remove the following code from assembleMessageData function of RadioMessageOutStartUpMessage class.
\code
          // VEHICLE_LIST_DATA Block types can be 0..*
         for (std::vector<VehicleListData>::iterator it = startUpMessage.vehicleListDataVec.begin();
            it != startUpMessage.vehicleListDataVec.end(); ++it)
          {
            vfwPutU8(&buffer, VehicleListData::blockType);
            vfwPutU16(&buffer, it->noOfVehicles);
            vfwPutU8(&buffer, static_cast<uint8_t>(it->vehicleType));
          }
\endcode
+ Rename the vehicleIdDataVec member of StartUpMessage structure in radio_message_types.hpp to vehicleDataVec.
+ Refer \ref VEHICLE_DATA for vehicle structure changes.


\subsection  DriverInformation  DriverInformation
+ Remove the "cancelAreaVec" member from DriverInformation structure in radio_message_out_driver_information.hpp.
+ Remove the following code from assembleMessageData function of RadioMessageOutDriverInformation class.
\code
      //Cancel Area Blocks
	  for (std::vector<CancelArea>::iterator it = driverInfo.cancelAreaVec.begin();
        it != driverInfo.cancelAreaVec.end(); ++it)
      {
        vfwPutU8(&buffer, CancelArea::blockType);
      }
\endcode

\subsection commonChanges Design Change Overview for the common messages:

\subsection protocolVersion protocolVersion
 
 - Need to re-order the code for PROTOCOL_VERSION field in parseMessageData function of RadioMessageInProtocolVersion class.
 	  \code
         // Read & validate Q_PROTOCOL_RESPONSE
         tmpValU8 = vfwGetU8(&buffer);
         if (!validateQ_PROTOCOL_RESPONSE(tmpValU8))
         {
           trace->write(ATC::detailedTrace, "Q_PROTOCOL_RESPONSE invalid");
           parseDataValid = false;
         }
         else
         {
           incomingProtocolVersionData.incomingProtocolResponse = static_cast<ProtocolResponse>(tmpValU8);
         }
       // Read & validate PROTOCOL_VERSION
         if (vfwGetU8(&buffer) != static_cast<uint8_t>(BTypeProtocolVersion))
         {
           trace->write(ATC::detailedTrace, "PROTOCOL_VERSION invalid");
           parseDataValid = false;
         }
         else
         {
           //Major version
           incomingProtocolVersionData.incomingProtocolVersion.major = vfwGetU8(&buffer);
           //Minor version
           incomingProtocolVersionData.incomingProtocolVersion.minor = vfwGetU8(&buffer);
         }	 
	  
      \endcode
	  
 - Remove the code for subversion.
 - Places to remove the code is as below:
    + RadioMessageInProtocolVersion::parseMessageData 
	  \code
	  //Sub version
      incomingProtocolVersionData.incomingProtocolVersion.sub = vfwGetU8(&buffer);
      \endcode
	  
	+ Remove field named as sub and its operation from structure "ProtocolVersion" of file name "radio_message_types.hpp"
	
	+ Remove the below line from RadioMessageInProtocolVersion::getProtocolVersionFromTCC function
	  \code
	   //Sub version
        protocolVersionFromTCC.sub = incomingProtocolVersionData.incomingProtocolVersion.sub;
	  \endcode


\subsection Block Design Change Overview for Blocks 

\subsection CEILING_SPEED_DATA CEILING_SPEED_DATA

+ Remove the trainEnd field from CeilingSpeedData structure in radio_message_types.hpp file.
+ Remove the vfwGetU8 function call from BTypeCeilingSpeedData case statement in parseMessageData function of  RadioMessageInMovementAuthority class.
+ In publishSpeedTarget function of RadioMessageInMovementAuthority class remove the following code
\code
  bool delay = false;
        if (maData.ceilingSpeedDataVec[index].trainEnd == TrainEndFrontOfTrain)
        {
          delay = true;
        }
\endcode

+ Remove the enum TrainEnd from radio_message_types.hpp
+ Remove trainLengthDelay member function and related initialization in constructor from SpeedTarget class.

\subsection KEEP_TRACK_DATA KEEP_TRACK_DATA
+ Remove the class KeepTrackDataTarget and its related operation.

\subsection VEHICLE_TYPE_DATA VEHICLE_TYPE_DATA
+ Add two members in VehicleTypeData structure for  brake system type 3.
+ Rename already present members in VehicleTypeData structure as per the FFFIS-AOS version 5.12.

\subsection SET_TIME  SET_TIME 
+ Change the data type of "currentTime" in structure SetTime of radio_message_types.hpp to uint64_t.
+ Update the vfwGetU32 function to vfwGetU64 function while parsing of BTypeSetTime block in parseMessageData function of RadioMessageInCommandMessage class.
+ Update the vfwGetU32 function to vfwGetU64 function while parsing of BTypeSetTime block in parseMessageData function of RadioMessageInDriverLogonStatus class.

\subsection ETA_REQUEST  ETA_REQUEST
+ Update the vfwGetU32 function to vfwGetU64 function while parsing of BTypeETARequest block in parseMessageData function of RadioMessageInPath class.

\subsection VEHICLE_DATA VEHICLE_DATA
+ Add member in "VehicleIdData" structure(will be renamed to "VehicleData") for N_VALUE field in VEHICLE_DATA
\code
    struct VehicleData
    {
      static const uint8_t blockType = static_cast<uint8_t>(BTypeVehicleIdData);
	  uint16_t    numberOfVehicle;
      VehicleType vehicleType;
      uint16_t    vehicleNodeAddress;
      char_t      vehicleName[vehicleCarNameMaxLength + 1];
    };

\endcode

\subsection PROTOCOL_VERSION PROTOCOL_VERSION
+ Remove the "sub" member from ProtocolVersion structure.
+ Remove the code from constructor of ProtocolVersion structure for initialization of "sub" member.

\subsection Field Design Change Overview for Fields

\subsection B_TRAIN_CORE_STATUS B_TRAIN_CORE_STATUS

+ Replace the bit 1 to not used (initialized to 0 value).
+ Set the bit 19 as per the requirement specification.

\subsection M_BRAKE_SYSTEM M_BRAKE_SYSTEM

+ Add the one member at the end of enum BrakeSystem in radio_message_types.hpp for brake system 3.

\subsection Q_ALERT Q_ALERT

+ update the EmAlertReason enum as per the FFFIS-TCC version 5.12.

\subsection Q_ATP_MODE Q_ATP_MODE
+ Need to remove Automatic unload from mode control and its related operation(This change may required system engineer input and will decided at the time of implementation).

\subsection TID_PASSWORD TID_PASSWORD
+ Set the tccPasswordIdMaxLength to 44U.

\subsection T_CLOCK T_CLOCK
+ Change the data type of member("newETATime") to uint64_t of ETAConfirmation structure.

\subsection NID_VEHICLE_TYPE NID_VEHICLE_TYPE
+ Update the VehicleType enum as per the FFFIS-TCC version 5.12.

\subsection D_POSITION D_POSITION
+ Change the data type of D_POSITION to unit32_t in all the places in AOS.

\subsection Q_UNREGISTRATION Q_UNREGISTRATION
+ Update the UnregInfo enum in radio_message_types.hpp as per the FFFIS-TCC ver5.12.

*/