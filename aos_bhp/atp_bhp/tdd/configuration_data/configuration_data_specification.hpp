/**
\if AsMainPage
\mainpage Configuration Data Handling 
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-08-09 | Configuration Data Handling                   | akushwah


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | Automatic Train Protection Onboard System


\section Assumption Assumption
Pseudo code used in the document is only to understand the logic, please ignore lint and  syntax errors in pseudo codes.
During the implementation please do not use same code, the below pseudo code is written just for understanding the problem better.
During implementation, code with correct syntax and logic needs  to be used.
Also comments used in pseudo codes are not according to the coding guideline it is more descriptive to understand the logic.
Comments also should not be used as it is during the implementation.


\section Introduction Introduction

\subsection Design Design Overview

This TDD state the handling of Configuration Data Message handling(Requirement section 5.3.21.3 & 5.4.2 Configuration parameters) after receiving from TCCSim.
Below is the list of parameters which can be changed by TCC:
- BaliseSearchDistance
- BaliseSearchSpeed
- RevSupervisionMargin
- RevSupervisionMarginShunting
- RevSupervisionMarginFreeRoll
- RollAwayMargin


\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

The below requirements should be taken care in account while implementing the Configuration Data for AOS Changes. These requirement are still in draft state and not yet released by requirement team.
Req Id          | Requirement description                                                                                 
----------------| --------------------------------------------------------------------------------------------------------------
AOS 1633 S      | The AOS shall accept ConfigurationData message in ATP mode Power Up AND Configuration. When the message is received in other ATP mode the AOS shall Ignore the message AND Issue a Log event.
AOS 1737 S      | When ConfigurationData message is accepted the AOS shall store received runtime parameters if the parameters are valid (in range, known, allowed to be modified).
AOS 2421 S      | If the configuration parameters from ConfigurationData message are not valid the AOS shall issue a Safety Halt event.


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components
The updates required in configuration Data handling should be done in below mentioned component:
- Config Component: Change the return type of the below mentioned functions from void to bool and update the function processing accordingly.
+ AbstractConfig::setBaliseSearchDistance()
+ AbstractConfig::setBaliseSearchSpeed()
+ AbstractConfig::setRevSupervisionMargin()
+ AbstractConfig::setRevSupervisionMarginShunting()
+ AbstractConfig::setRevSupervisionMarginFreeRoll()
+ AbstractConfig::setRollAwayMargin()


- Message Handler Component : The class RadioMessageInConfigurationData of Message handler component need to be updated to incorporate the configuration data handling changes
+ function RadioMessageInConfigurationData::validateMode() needs to updated for mode validation i.e power up and configuration mode. Currently, its only get validated in power up and this needs to extend for configuration mode also.

\code 
//Pseudo code Start
bool modeValid = false;

ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();
switch (mode)
{
case ATPModePowerUp:
case ATPModeConfiguration:
modeValid = true;
break;

case ATPModeUndefined:
case ATPModeRegistration:
case ATPModeBaliseSearch:
case ATPModeNormal:
case ATPModeLocation:
case ATPModeStaffResponsible:
case ATPModeSplit:
case ATPModeJoin:
case ATPModeShuntingRoute:
case ATPModeAutomaticUnload:
case ATPModePoweringDown:
case ATPModeShunting:
case ATPModeYard:
case ATPModeUnregistered:
case ATPModeSafetyHalt:
case ATPModeSafeBrakeToStop:
case ATPModeSleeping:
case ATPModePossession:
trace->write(briefTrace, "Configuration Data message received in invalid ATP mode");
writeToLog(ATC::BriefLog, "Configuration Data message received in invalid ATP mode");
break;

case ATPModesCount:
default:
{
trace->write(briefTrace, "Error in ATP Mode, No Such ATP Mode defined");
writeToLog(ATC::BriefLog, "Error in ATP Mode, No Such ATP Mode defined");
break;
}
}
//Pseudo code End
\endcode


+ function RadioMessageInConfigurationData::validate() needs to be updated for checking the parameter validity via calling appropriate set functions.
Below is the pseudo code for the implementation which will be done after parsing the Configuration parameter name and value

\code
//Pseudo code Start
switch(configurationDataValue.textStringName)
{
case "BaliseSearchDistance":
{
if(!AbstractConfig::corePtr()->setBaliseSearchDistance(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
case "BaliseSearchSpeed":
{
if(!AbstractConfig::corePtr()->setBaliseSearchSpeed(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
case "RevSupervisionMargin":
{
if(!AbstractConfig::corePtr()->setRevSupervisionMargin(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
case "RevSupervisionMarginShunting":
{
if(!AbstractConfig::corePtr()->setRevSupervisionMarginShunting(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
case "RevSupervisionMarginFreeRoll":
{
if(!AbstractConfig::corePtr()->setRevSupervisionMarginFreeRoll(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
case "RollAwayMargin":
{
if(!AbstractConfig::corePtr()->setRollAwayMargin(configurationDataValue.textStringValue))
{
//Report a safety halt event
}
break;
}
default:
{
//Create a safety halt event and report it here
break;
}
}
//Pseudo code End
\endcode

\subsection ScreenImages Screen Images

\subsection ObjectsAndActions Objects and Actions

\section AdditionalMaterial Additional Material

*/