/**
\if AsMainPage
\mainpage Unregistration mode (5.1.21 and 5.3.21.7)
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.2     | 01-11-2017 | Proposed requirements implementation for Unregistration mode | prsrivas


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On Board System 

\section Introduction Introduction
Below changes are proposed for the conditions to enter Unregistration mode when the AOS is in Idling or Standstill state as per the requirements defined in Doors module SSRS_AOS_BHPB_Dev_Test under section 5.1.21 and 5.3.21.7. 

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 447	| The AOS shall change mode to Unregistered mode if an Unregistration message is received when the train state is idling.| Partially Implemented
AOS 1732| The AOS shall change mode to Unregistered if an Unregistration message is received in the following ATP modes: Configuration, Registration, Safe Brake to Stop at standstill. | Partially Implemented
AOS 2430| If an Unregistration message is received in ATP mode Balise Search AND there is no movement authority from TCC, the AOS shall: Raise a brake event AND Change mode to Unregistered when at standstill. | Not Implemented
AOS 2418| The AOS shall raise a SafetyHalt Event if an Unregistration message is received when the train state is NOT idling in following ATP mode:Normal, Staff Responsible, Shunting Route, Join, Split.| Not Implemented
AOS 2419| The AOS shall raise a SafetyHalt Event if an Unregistration message is received in following ATP Modes: Location, Automatic Unload, Safe Brake to Stop AND not at standstill, Balise Search with an active moment authority from TCC. | Not Implemented
AOS 2420| The AOS shall disregard an Unregistration message and raise a Log event in the following modes: Power Up, Possession, Shunting, Yard, Sleeping, Powering Down, Safety Halt, Unregistered. | Partially Implemented
      
\subsection ExternalInterfaceDescription External Interface Description
Interface to TCC shall be according to the FFFIS TCC-AOS version 5.8.

Following is the list of affected components:
- Message Handler
- Mode Control

To adapt the some requirements we need to create a new virtual function in abstract mode class which need to be inherited in the derived classes.
Refer \ref class_diagram
\subsection class_diagram Class diagram
@image html unregistration.png
@image latex unregistration.png

\section RequirementsImplementation Requirements Implementation
\subsection  Unregistration Unregistration
This sub-section covers the requirements defined in Doors module SSRS_AOS_BHPB_Dev_Test under section 5.1.21 and 5.3.21.7. \n

\subsection AOS447 AOS 447
- Affected Component  : MessageHandler
- Affected Function   : validateMode () of RadioMessageInUnregistration class
  - In the switch case, add the condition to check the train Idle state for all the modes except Configuration and Registration. Set the modeValid flag to True. \n
\b Note \b : <em> Present requirement is not clear yet as train is not in Idling state during Configuration and Registration mode. Respective query is raised to the requirements team.</em>

\subsection AOS1732 AOS 1732
- Affected Component : MessageHandler
- Affected Function  : validateMode () of RadioMessageInUnregistration class
  - In the switch case add the following for Configuration, Registration and Safe Brake To Stop modes:
    - For ATP modes Configuration and Registration set the modeValid flag to True. 
    - For ATP mode Safe Brake To Stop fetch the value of train standstill flag through the getter function isTrainStandStill() from Odometry. If train state is standstill, set the modeValid flag to True. \n
<b> Special case </b> : For the Safe Brake To Stop additionally check Idling state through getter function getIdleState() from Mode Control . \n
\n<b>- Psuedo code</b>
        \code
        switch (mode)
        {
          case ATPModeConfiguration:
          case ATPModeRegistration:
          {
                modeValid = True;
                break;
          }
          case ATPModeSafeBrakeToStop:
          {
               if((Pos::AbstractOdometry::corePtr()->isTrainStandStill()) && 
               (AbstractModeControl::corePtr()->getIdleState()))
               {
                modeValid = True;
               }
               break;
          }
        }
        \endcode

\subsection AOS2430 AOS 2430
- Affected component : MessageHandler & Mode Control
- Affected Function  : validateMode () of RadioMessageInUnregistration class, runIn() of AbstractMessageHandler class, initCrossCompare() of AbstractMessageHandler class 
- Added Function     : handleUnReg_Msg() in abstract_mode.cpp, handleUnReg_Msg() in balise_search_mode.cpp (<em>inherited from abstract_mode</em>) and handleUnReg_Msg() in unregistered_mode.cpp (<em>inherited from abstract_mode</em>).
  - In the runIn() of AbstractMessageHandler add the condition to fetch the value of Unregistration message through the function getUnregInfo(). Create a flag within the method and set it to True if unregistration message is received.
  - In the initcrosscompare(), add the Unregistration message flag to init cross compare.
  - In the switch case of validateMode (), for mode BaliseSearch :
    - Add condition to check that no movement authority is received.Set the modeValid value to True. Raise brake event.
  - In abstract_mode.cpp create a virtual function handleUnregistrationMode(). Add the condition to check if the unregistration message is received. Save the current mode as previous mode and set the current mode as Unregistered.
  - In balise_search_mode.cpp inherit the handleUnregistrationMode. Add the condition to check if train is at standstill and Unregistration message is received. If so, change the current mode to Unregistered. Set the Unregistration message flag to false.
  - In unregistered_mode.cpp inherit the handleUnregistrationMode. Add the condition to check if Unregistration message is received. Set the Unregistration message flag to false.
\n<b>- Psuedo code</b>
        - validateMode() of RadioMessageInUnregistration 
        \code
        case ATPModeBaliseSearch :
        {
           if((balise_search_mode == baliseSearchWaitMA) || (balise_search_mode == baliseSearchWaitBalise2))
           {
               modeValid = True;
              // Raise Service Brake event.
           }
        }
        \endcode

        - runIn() of AbstractMessageHandler
        \code   
          UnregInfo UnReg_Info;
          if (getUnregInfo(UnReg_Info))
          {
            UnRegMsg_ReceivedEarlier = true;
          }
        \endcode
        
        - initCrossCompare() of AbstractMessageHandler
        \code
         Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&UnRegMsg_ReceivedEarlier));
        \endcode

        - handleUnReg_Msg() of AbstractMode
        \code
        if ((UnRegMsg_ReceivedEarlier)
        {
           previousMode = currentMode;
           currentMode = ATPModeUnregistered; 
           // Remove the train setup
         }
        \endcode
        
        - handleUnReg_Msg() of BaliseSearchMode
        \code
        if((UnRegMsg_ReceivedEarlier)) && Pos::AbstractOdometry::corePtr()->isTrainStandStill())
        {
           previousMode = currentMode;
           currentMode = ATPModeUnregistered; 
           UnRegMsg_ReceivedEarlier = false;
           // Remove the train setup
        }
        \endcode

        - handleUnReg_Msg() of UnregisteredMode
        \code
        if(UnRegMsg_ReceivedEarlier)
        {
           UnRegMsg_ReceivedEarlier = false;
        }
        \endcode

\subsection AOS2418 AOS 2418
- Affected Component : Message Handler
- Affected Function  : validateMode () of RadioMessageInUnregistration class
  - In the switch case add the following for Normal, Staff Responsible, Shunting Route, Join and Split modes :
    - Add the condition to check if train is in Idling state or not through the getter function getIdleState() from Mode Control.
    - If Idling, set modeValid flag to True. If not Idling, set modeValid flag to False and raise Safety Halt event. \n
\n<b>- Psuedo code</b>
        \code
        if(AbstractModeControl::corePtr()->getIdleState())
        {
           modeValid = True;
        }
        else
        {
           modeValid = False;
           // Raise Safety Halt event
        }
        \endcode

\subsection AOS2419 AOS 2419
- Affected Component : Message Handler
- Affected Function  : validateMode () of RadioMessageInUnregistration class
  - In switch case add the following for Location, Automatic Unload, Safe Brake To Stop modes : 
    - For Location and Automatic Unload set the modeValid to False and raise Safety Halt event. 
    - For Safe Brake to Stop add the condition to check that train is not in standstill through getter function isTrainStandStill() from Odometry.Set the modeValid flag to false. Raise a Safety Halt event. \n
\n<b>- Psuedo code</b>
      \code
      case ATPModeLocation:
      case ATPModeAutomaticUnload:
      {
           modeValid = false;
           // Raise Safety Halt event
           break;
      }
      case ATPModeSafeBrakeToStop:
      {
          if(!AbstractModeControl::corePtr()->isTrainStandStill())
          {
             modeValid = false;
             // Raise Safety Halt event
             break;
          }
       }
      \endcode
  - In switch case, for Balise search mode add the condition to check that valid MA is received in Balise Search mode.
    - Set the modeValid flag to false. Raise a Safety Halt event. \n
\n<b>- Psuedo code</b>
     \code
     case ATPModeBaliseSearch:
     if ((balise_search_mode == baliseSearchWaitBalise2) || (balise_search_mode == baliseSearchFinishOK))
     {
        modeValid = false;
        // Raise safety halt event
        break;
     }
     \endcode
     
\subsection AOS2420 AOS 2420
Affected Component : Message Handler
Affected Function  : validateMode() of RadioMessageInUnregistration class
- In the switch case, for Power UP, Possession, Shunting, Yard, Sleeping, Powering Down, Safety Halt, Unregistration mode:
    - Set the modeValid value to False.
    - Raise log event.
*/