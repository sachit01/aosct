/**
\if AsMainPage
\mainpage Technical Design Description for FFFIS-AOS Adaptation (1.6)
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2018-03-19 | Created to support FFFIS-AOS Adaptation version 1.6    | akushwah


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
This document describes the technical design description for the changes in AOS related to up-gradation of FFFIS-AOS Adaptation version 1.6 document.

<B>Note:</B> Below mentioned code are only meant for understanding. Please ignore the lint,compiler warnings.


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement below mentioned changes.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1      | FFFIS TCC-AOS, BHPB Adaptation                     | 1.6
2      | FFFIS TCC-AOS                                      | 5.16


\subsection TCCToAOSChanges Design Change Overview for the messages:
Below is the list of Radio Message which will be updated under this TDD.
+ CommandMessage
+ MovementAuthority
+ Path
+ TrainSetup
+ PositionReport
+ ConfigurableParameters
\n

Also, Need to update the below field parameters.
+ M_BRAKE_SYSTEM
+ Q_SIGNAL


\subsection GeneralConcept General Concept for Handling Adaptation Blocks.
+ Create the derived class which will be inherited from parser/creator class.
+ Derived class will have invalidate function, which is responsible for calling the invalidate function of parser/creator and also invalidate
all the data members of its own class.
+ For Parser, create a virtual function parseAdditionalBlocks(VFW_Buffer* buffer, uint8_t adapBlockType) which return bool value indicating 
whether parsing is done correctly or not.

@image html general_concept_parser.png
@image latex general_concept_parser.png

+ For creator, create a virtual function assembleAdditionalBlocks(VFW_Buffer* buffer) which return bool value indicating whether assembling 
of data is done correctly or not.

@image html general_concept_creator.png
@image latex general_concept_creator.png

+ create file radio_message_types_bhp.hpp and defines all the adaptation related Radio Block Type, structures for blocks BHPB_CONFIG_VERSION,
BHPB_RADIO_AREA, BHPB_TRAIN_STATUS,BHPB_TRAIN_WEIGHT,BHPB_SAFE_FOR_BOARDING, enum for B_BHPB_TRAIN_STATUS,M_BRAKE_SYSTEM,Q_SIGNAL etc.
+ create the object of derived parser/creator classes in function MessageHandler::init().



\subsection CommandMessage CommandMessage Message
+ File Name: radio_message_in_command_message_bhp.hpp and radio_message_in_command_message_bhp.cpp
+ ClassName: RadioMessageInCommandMessageBHP
+ create structure for BHPB_TRAIN_WEIGHT and BHPB_SAFE_FOR_BOARDING in file radio_message_types_bhp.hpp.
\code
struct BHPBTrainWeight
{
uint16_t   noOfApplicationData;
uint16_t   measuredTrainWeight;
};

struct BHPBSafeForBoarding
{
uint16_t   noOfBytesApplicationData;
};
\endcode

+ create a protected virtual function parseAdditionalBlocks(VFW_Buffer* buffer, uint8_t adapBlockType) in class RadioMessageInCommandMessage.
This function will be called from function RadioMessageInCommandMessage::parseMessageData() as shown in below snippet.
This will internally call the virtual function parseAdditionalBlocks() of the derived class.
\code
bool RadioMessageInCommandMessage::parseMessageData()
{
..............
..............

// Fetch next data-block until M_END_OF_MESSAGE
while (nextMsgIdentifier != M_END_OF_MESSAGE)
{
switch (nextMsgIdentifier)
{
case BTypeTrainName:
{
........
break;
}
case BTypeSetTime:
{
......
break;
}
case BTypeReleaseBrake:
{
........
break;
}
case BTypeSafeForBoardingActivate:
{
.........
break;
}
case BTypeTextMessage:
{
.......
break;
}
default:
{
parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
break;
}
}

// Fetch next msg-type (or M_END_OF_MESSAGE)
nextMsgIdentifier = vfwGetU8(&buffer);
}
.....
}
\endcode

+ create a data members of BHPBTrainWeight and BHPBSafeForBoarding structure type to process the data for the blocks BHPB_TRAIN_WEIGHT and BHPB_SAFE_FOR_BOARDING.
+ define the function parseAdditionalBlocks() in class RadioMessageInCommandMessageBHP to parse the data N_LENGTH, W_WEIGHT and store them in the data members.
\code
bool RadioMessageInCommandMessageBHP::parseAdditionalBlocks(VFW_Buffer *buffer, uint8_t adapBlockType)
{
bool retvalue = false;

switch (adapBlockType)
{
case BTypeBHPBTrainWeight:
{
//get the data using vfw functions.
.........
retvalue = true;
}
case BTypeBHPBSafeForBoarding:
{
//get the data using vfw functions.
.........
retvalue = true;
}
default:
{
//Log error
break;
}
}

return retvalue;
}
\endcode
+ create a new class namely as ExtendedTrainSetup, which will extend the TrainSetup Class in adaptation part.
+ create a attribute trainWeight in ExtendedTrainSetup class to store the train weight value.
+ create an access function TSetup::setTrainWeight() to store the train weight value in trainWeight attribute.
+ call the setTrainWeight() function, once the weight value is extracted from command message.
+ publish the SafeForBoarding extracted data value by creating access function in message handler class as it will be used in future for ATO operation.


\subsection MovementAuthority Movement Authority Message
+ File Name: radio_message_in_movement_authority_bhp.hpp and radio_message_in_movement_authority_bhp.cpp
+ ClassName: RadioMessageInMovementAuthorityBHP
+ create structure for BHPB_RADIO_AREA in file radio_message_types_bhp.hpp.
\code

//Max string length of Radio Area field of an TCC-AOS communication packet

static const uint8_t radioAreaLength = 20U;

//Defines the storage of the parsed data-information of an arriving MA in BHP Adaptation

struct BHPBRadioArea
{
  uint16_t       noOfBytesApplicationData;
  TrackAndPos    trackIdAndPosOfItem;
  ValidDirection validDirection;
  char_t         radioArea[radioAreaLength + 1U];
};

\endcode

+ create a protected virtual function parseAdditionalBlocks(VFW_Buffer* buffer, uint8_t adapBlockType) in class RadioMessageInMovementAuthority.
This function will be called from function RadioMessageInMovementAuthority::parseMessageData() as shown in below snippet.
This will internally call the virtual function parseAdditionalBlocks() of the derived class.
\code
bool RadioMessageInMovementAuthority::parseMessageData()
{
  ..............
    ..............

    // BlockData
    uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

  // Fetch next data-block until M_END_OF_MESSAGE
  while (nextMsgIdentifier != M_END_OF_MESSAGE)
  {
    switch (nextMsgIdentifier)
    {
    case BTypeDepartureWarning:
    {
      ........
        break;
    }
    case BTypePartlyMa:
    {
      ......
        break;
    }
    case BTypeMaxSearchDist:
    {
      ........
        break;
    }
    case BTypeLocationBorder:
    {
      .........
        break;
    }
    case BTypeLocationData:
    {
      ..........
        .........
    }
    default:
    {
      parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
      break;
    }
    }

    // Fetch next msg-type (or M_END_OF_MESSAGE)
    nextMsgIdentifier = vfwGetU8(&buffer);
  }
  .....
}
\endcode

+ create a data member of BHPBRadioArea structure type to process the data for the block BHPB_RADIO_AREA.
+ define the function parseAdditionalBlocks() in class RadioMessageInMovementAuthorityBHP to parse the data N_LENGTH, NID_TRACK, D_POSITION, 
Q_DIRECTION and TID_RADIO_AREA and copy them in the data members.
\code
bool RadioMessageInMovementAuthorityBHP::parseAdditionalBlocks(VFW_Buffer *buffer, uint8_t adapBlockType)
{
  bool retvalue = false;

  switch (adapBlockType)
  {
  case BTypeBHPBTrainWeight:
  {
    //get the data using vfw functions.
    .........
      retvalue = true;
  }
  case BTypeBHPBSafeForBoarding:
  {
    //get the data using vfw functions.
    .........
      retvalue = true;
  }
  default:
  {
    //Log error
    break;
  }
  }

  return retvalue;
}
\endcode

+ BHPB Radio Area block parsed data shall be treated as Track Data items target in adaptation. Below operation are required in order to store it in targets as Track Data items.
  - create the derived class for TrackDataItemTarget.
    + File Name: track_data_item_target_bhp.hpp track_data_item_target_bhp.cpp
    + Class Name: TrackDataItemTargetBHP
  - create a enum for Adaptation Track data 
  \code
  \\TrackDataType declares the types for adaptation
    enum TrackDataTypeBHP
    {
      TrackDataTypeRadioArea = 129
    }
  \endcode

+ make the publishTrackDataItemTarget() as virtual and extend the functionality in RadioMessageInMovementAuthorityBHP class to publish the parsed data by adding the radio area 
related data in Track Data Item list.
+ create a access function in getBHBPRadioArea() in Target which will be used by new dmi message(description below) to get the radio area related data value.
+ create a new adaptation message class AbstractDMIMessageOutBHPBRadioArea (dmi_message_out_bhpb_radio_area.hpp and dmi_message_out_bhpb_radio_area.cpp)
in DMIHandler which will be inherited from AbstractDMIMessageOut class. This also requires update in DMI interface spec and DMI SW changes, which will be done in separate task.


\subsection Path Path Message
+ File Name: radio_message_in_path_bhp.hpp and radio_message_in_path_bhp.cpp
+ ClassName: RadioMessageInPathBHP
+ create structure for BHPB_CONFIG_VERSION in file radio_message_types_bhp.hpp.
\code

struct BHPBConfigVersion
{
uint16_t       noOfBytesApplicationData;
uint8_t        configurationMajorVersion;
uint8_t        configurationMinorVersion;
};

\endcode

+ create a protected virtual function parseAdditionalBlocks(VFW_Buffer* buffer, uint8_t adapBlockType) in class RadioMessageInPath.
This function will be called from function RadioMessageInPath::parseMessageData() as shown in below snippet.
This will internally call the virtual function parseAdditionalBlocks() of the derived class.
\code
bool RadioMessageInPath::parseMessageData()
{
..............
..............

// BlockData
uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

// Fetch next data-block until M_END_OF_MESSAGE
while (nextMsgIdentifier != M_END_OF_MESSAGE)
{
switch (nextMsgIdentifier)
{
case BTypeTracks:
{
........
break;
}
case BTypeETARequest:
{
......
break;
}
case BTypeSpeedChangePosition:
{
........
break;
}
default:
{
parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
break;
}
}

// Fetch next msg-type (or M_END_OF_MESSAGE)
nextMsgIdentifier = vfwGetU8(&buffer);
}
.....
}
\endcode

+ create a data member of BHPBConfigVersion structure type to process the data for the block BHPB_CONFIG_VERSION.
+ define the function parseAdditionalBlocks() in class RadioMessageInPathBHP to parse the data N_LENGTH, M_VERSION(Major and Minor) and copy them in the data members.
\code
bool RadioMessageInPathBHP::parseAdditionalBlocks(VFW_Buffer *buffer, uint8_t adapBlockType)
{
bool retvalue = false;

switch (adapBlockType)
{
case BTypeBHPBConfigVersion:
{
//get the data using vfw functions.
.........
retvalue = true;
}
default:
{
//Log error
break;
}
}

return retvalue;
}
\endcode
+ create a new class namely as ExtendedTrainSetup, which will extend the TrainSetup Class and will be used to store major and minor version.
+ create a function TSetup::setBHPBConfigVersionToTrainSetup() in TSetup class, which will set the value of major and minor in extended train setup storage.
+ call the TSetup::setBHPBConfigVersionToTrainSetup() to set the major and minor attributes in extended train setup once they are extracted.
+ create a access function TSetup::getBHBPConfigVersion() will be called to set the TCC version of ADS map in LCS path message 
in function LCSMessageOutPath::collectData().


\subsection TrainSetup TrainSetup Message
+ File Name: radio_message_in_train_setup_bhp.hpp and radio_message_in_train_setup_bhp.cpp
+ ClassName: RadioMessageInTrainSetupBHP
+ use the structure for BHPB_CONFIG_VERSION in file radio_message_types_bhp.hpp.
+ create a protected virtual function parseAdditionalBlocks(VFW_Buffer* buffer, uint8_t adapBlockType) in class RadioMessageInTrainSetup.
This function will be called from function RadioMessageInTrainSetup::parseMessageData() as shown in below snippet.
This will internally call the virtual function parseAdditionalBlocks() of the derived class.
\code
bool RadioMessageInTrainSetup::parseMessageData()
{
..............
..............

// BlockData
uint8_t nextMsgIdentifier = vfwGetU8(&buffer);

// Fetch next data-block until M_END_OF_MESSAGE
while (nextMsgIdentifier != M_END_OF_MESSAGE)
{
switch (nextMsgIdentifier)
{
case BTypeTrainName:
{
........
break;
}
case BTypeVehicleTypeData:
{
......
break;
}
case BTypeVehicleData:
{
........
break;
}
default:
{
parseDataValid = parseAdditionalBlocks(&buffer, nextMsgIdentifier);
break;
}
}

// Fetch next msg-type (or M_END_OF_MESSAGE)
nextMsgIdentifier = vfwGetU8(&buffer);
}
.....
}
\endcode

+ create a data member of BHPBConfigVersion structure type to process the data for the block BHPB_CONFIG_VERSION.
+ define the function parseAdditionalBlocks() in class RadioMessageInTrainSetupBHP to parse the data N_LENGTH, M_VERSION(Major and Minor) and copy them in the data members.
\code
bool RadioMessageInTrainSetupBHP::parseAdditionalBlocks(VFW_Buffer *buffer, uint8_t adapBlockType)
{
bool retvalue = false;

switch (adapBlockType)
{
case BTypeBHPBConfigVersion:
{
//get the data using vfw functions.
.........
retvalue = true;
}
default:
{
//Log error
break;
}
}

return retvalue;
}
\endcode
+ call the TSetup::setBHPBConfigVersionToTrainSetup() to set major and minor version in extended train setup once they are extracted.


\subsection PositionReport Position Report Message
+ File Name: radio_message_out_position_report_bhp.hpp and radio_message_out_position_report_bhp.cpp
+ ClassName: RadioMessageOutPositionReportBHP
+ use the structure for BHPB_TRAIN_STATUS in file radio_message_types_bhp.hpp.
\code

struct BHPBTrainStatus
{
uint16_t       noOfBytesApplicationData;
uint32_t       bhpbTrainStatus;
};

\endcode
+ create a protected virtual function assembleAdditionalBlocks(VFW_Buffer* buffer) in class RadioMessageOutPositionReport.
This function will be called from function RadioMessageOutPositionReport::assembleMessageData() as shown in below snippet.
This will internally call the virtual function assembleAdditionalBlocks() of the derived class, which will collect and assemble the data in the buffer.
\code
bool RadioMessageOutPositionReport::assembleMessageData()
{
.............
............

//Put EXTERNAL_DATA Block
for (std::vector<ExternalData>::iterator it = positionReport.externalDataVec.begin();
it != positionReport.externalDataVec.end(); ++it)
{
......
......
}

assembleAdditionalBlocks(VFW_Buffer* buffer);

vfwPutU8(&buffer, M_END_OF_MESSAGE);

// Total length of message
messageData.message.dataLength = static_cast<uint16_t>(vfwGetValidSize(&buffer));
}

traceAssembleData(assembleDataValid);

return assembleDataValid;
}
\endcode

+ create a data member of BHPBTrainStatus structure type to process the data for the block BHPB_TRAIN_STATUS.
+ define the function assembleMessageData() in class RadioMessageInTrainSetupBHP to collect and assemble the data N_LENGTH, B_BHPB_TRAIN_STATUS in the buffer.

\subsection M_BRAKE_SYSTEM M_BRAKE_SYSTEM
+ create a enum for the M_BRAKE_SYSTEM in vehicleCom Adaptation.
\code
enum BHPBBrakeSystemInUse
{
BHPBBrakeSystemInUseUndefined =0,
BHPBBrakeSystemInUsePneumatic,
BHPBBrakeSystemInUseECPB,
BHPBBrakeSystemInUseNotUsed,
};
\endcode
+ Update the access function VehicleCom::getBrakeSystemInUse() to map the enum values received from LCS to above defined AOS enum.
+ store the brake system in use in train setup from LCSMessageInECPBTrainComposition::publishData() by calling getBrakeSystemInUse() access function.

\subsection Q_SIGNAL Q_SIGNAL
+ create a enum for the Q_SIGNAL in message handler Adaptation.
\code
enum BHPBQSignal
{
BHPBQSignalCountryHorn =128,
BHPBQSignalTownHorn,
BHPBQSignalBell
};
\endcode
+ create a function MessageHandler::getQSignal() in message handler adaptation and defines the function getBHPBQSignal() to map the enum value defined above. 
This access function will be used in future.


\subsection ConfigurableParameters Configurable Parameters
+ create a new config parameter for P_ECPBBRAKESUSED and create the access function of it.
+ function Config::init(void) needs to updated with the configurable parameters mentioned below 
\code

.........
.........
// Then call the parent class init()
if (AbstractConfig::init())
{
//set Global Name to the corresponding Configuration parameter
setGlobalNameToConfigParameter(ecpbMinPercBrakeCarsId, "P_ECPBBRAKESREQUIRED");
setGlobalNameToConfigParameter(ecpbMinPercCarsForBrakeCalcId, "P_ECPBBRAKESUSED");
setGlobalNameToConfigParameter(maxEbFbDiffId, "Pr_BRAKEPRAPIDLOSS");
setGlobalNameToConfigParameter(timsMinBPLastCarId, "Pr_BRAKEPLOWEOT");
setGlobalNameToConfigParameter(timsMinBPLocoId, "Pr_BRAKEPLOWLOCO");
setGlobalNameToConfigParameter(maxEbFbDiffTimeoutId, "T_BRAKEPRAPIDLOSS");

retValue = true;
}
return retValue;

\endcode

+ Also, update the min and max value of the above config parameters according to chapter 5.3 5.3	Configurable Parameters in FFFIS TCC AOS, BHPB Adaptation ver 1.6.


*/