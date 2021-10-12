/**
\if AsMainPage
\mainpage Join Mode(section (5.1.14))
\endif

\section VersionLog Version Log

Version | Date       | Description                                          | Signature
------- | --------   | ---------------------------------------------------- | ---------
1.0     | 2017-11-09 | Proposed Changes for join/split/sleep requirements   | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TODO           | To be discussed

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 5.1.14 for join-mode.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description (Split)                                                 | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 1695| When the ATP mode changes to Split, the AOS shall request the Driver to confirm the Split mode. The AOS shall raise a Standstill event while waiting for the Driver to confirm the Split mode.| Not Implemented
AOS 2461| The AOS shall change the ATP mode to Split, when an MA with Q_ROUTE_TYPE equal to Split is accepted.| Implemented

Req     | Short requirement description (Join)                                                  | Description
------- | --------------------------------------------------------------------------------------| ------------
AOS 1696| When the ATP mode changes to Join, the AOS shall request the Driver to confirm the Join mode. The AOS shall raise a Standstill event while waiting for the Driver to confirm the Join mode.   | Not Implemented
AOS 1839| In the following modes the AOS shall, once the driver is logged in AND the vehicle is at standstill AND connected to TCC AND in ATO mode Manual, offer the driver the possibility to select ATP mode Configuration: Possession, Split, Join, Shunting, Shunting Route, Yard, Unregistered if no SafeBrakeToStop event is active. After drivers selection AOS shall switch to mode Configuration.| Not Implemented
AOS 2533| The AOS shall change the ATP mode to Join if: The ATO mode is Manual AND The train state is Idling AND A JoinCommand message is received from TCC. | Not Implemented
AOS 2534| The AOS shall change the ATP mode to Join, when an MA with Q_ROUTE_TYPE equal to Join is accepted. | Implemented
AOS 2462| The AOS shall request the Driver to acknowledge the transition to ATP Mode Sleeping if: The ATP mode is Join, Yard, Shunting OR PowerUp, AND The Sleeping signal is active, AND The train is at standstill, AND The Driver is logged in. The AOS shall change to ATP mode Sleeping after Driver acknowledge.| Not Implemented

Req     | Short requirement description (Sleeping)                                              | Description
------- | --------------------------------------------------------------------------------------| ------------
AOS 2564| In ATP mode Sleeping the AOS shall present the Sleeping screen to the Driver.| Not Implemented
AOS_BHPB 2636 | The AOS shall use the Non-Leading Locomotive (NONLEAD) digital input as the Sleeping signal.| Not Implemented
AOS 2464 | The AOS shall allow the Driver to select ATP mode Configuration if: The Sleeping signal becomes inactive AND The train is at standstill AND The ATO mode is Manual AND The AOS is connected to TCC. The AOS shall ask the Driver to confirm leaving sleeping mode upon selection AND change the ATP mode to Configuration upon confirmation. | Not Implemented
AOS 2482 | If the Sleeping signal becomes inactive in ATP mode Sleeping, the AOS shall: Raise a Standstill event continuously if the train is at standstill OR Raise a Log event if the train is NOT at standstill. | Not Implemented
AOS 2585 | If the Sleeping signal becomes inactive AND the train is at standstill in ATP mode Sleeping the AOS shall change ATP mode to SafetyHalt if A SafetyHalt event was issued since the train entered Sleeping mode. | Not Implemented
AOS 2586 | The AOS shall inhibit brake application in ATP mode Sleeping if the Sleeping signal is active.| Not Implemented
AOS 2588 | The AOS shall allow Brake application in ATP mode Sleeping when: The Sleeping signal is inactive AND The train is at standstill. The Brake application shall be allowed as long as the Sleeping signal is inactive. | Not Implemented


\subsection ExternalInterfaceDescription External Interface Description

The IF Spec ATP-DMI needs to be updated as follows:
- ATP Mode and States Message: 4=Allowed to enter Configuration, shall be added to Data14
- ATP Mode and States Message: 4=Join shall be added to Data13(Confirm change to mode needed)
- ATP Mode and States Message: 5=Sleep shall be added to Data13(Confirm change to mode needed)
- ATP Mode and States Message: 6=Split shall be added to Data13(Confirm change to mode needed)
- DMI_ToATP_Data: 24=Confirm Join
- DMI_ToATP_Data: 25=Confirm Sleep
- DMI_ToATP_Data: 26=Confirm Split


\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection EnterJoinSplit Enter Join- and SplitMode

\subsubsection AOS2533 AOS2533

Affected component  : Mode Control \n
Affected function :  NormalMode::handleMode() \n
- Check if TrainState is Idling (AbstractModeControl::isIdleState()) AND if JoinCommand is received in Message Handler (AbstractMessageHandler::getJoinCommand())
- If above condition is full-filled then set next mode to 'ATPModeJoin' (This is for the stationary train).


\subsubsection AOS2534_2461 AOS2534, AOS2461

Affected component  : Mode Control \n
Affected function :  AbstractMode::checkModeChangedByMAQRouteType() \n
- - Implemented -



\subsection LeaveJoinSplit Leave Join-, Split and SleepMode

\subsubsection AOS1839_2464 AOS1839, AOS2464

Affected component  : DMI Application \n
Affected function : ATP Mode and States Message from ATP to DMI \n
- New bit to indicate if Configuration Button shall be displayed.

Affected component  : DMI Handler \n
Affected function :  ATP Mode and States Message from ATP to DMI

- New method to tell when Configuration can be selected. Condition: (Modes: Possession, Split, Join, Shunting, Shunting Route, Yard, Unregistered if no SafeBrakeToStop event is active AND
DriverloggedIn AND StandSTill AND ConnectedTCC AND ATOmodeManual) OR (Mode: Sleep AND !NonLead AND StandSTill AND ConnectedTCC AND ATOmodeManual)


\subsection EnterSleep Enter SleepMode

\subsubsection AOS2462 AOS2462

Affected component  : Mode Control \n
Affected function : AbstractModeControl \n
- Add method for goto sleep criteria e g bool AbstractModeControl::isGotoSleepModeValid():

    If(NONLEAD && StandStill && DriverLoggedin)
        return true
    
- Add a substate in each mode (Join, Yard, Shunting and PowerUp) and evaluate sleep criteria in these modes (handleMode() method): Join, Yard, Shunting and PowerUp.

- Wait until confirmation is received from DMI: DMICom::DMIButtonConfirmSleep == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()

- Proceed to SleepMode


Affected component  : DMI Handler\n
Affected function : DMIMessageOutATPModesAndStatus::collectData()\n
   
- Detect sub-states when waiting for confirmation in Join, Yard, Shunting and PowerUp in collectData().

- Set the dmiConfirmModeChange = DMIConfirmModeChangeToSleep to have the DMI show confirmation Button if any sub-state above is valid.


\subsubsection AOS2564 AOS2564

Affected component  : DMI Application \n
Affected function : ? \n

- Display 'Sleeping Screen' when ATP-mode and states Data0=Sleeping


\subsection LeaveSleep Leave SleepMode

\subsubsection AOS2464_2482_2585 AOS2636, AOS2482, AOS2585

Affected component  : Mode Control \n
Affected function : SleepingMode::handleMode() \n

- See SleepMode StateMachine in 'Confirmation in DMI to enter Join and Sleep Modes'


\subsection Confirmation Confirmation in DMI to enter Join, Split and Sleep Modes
\subsubsection AOS1695_1696_2462 AOS1695, AOS1696, AOS2462

Affected component  : Mode Control \n
Affected function : handleMode() of  JoinMode class \n
- StateMachine Join Mode

@image html join_mode.png
@image latex join_mode.png

- Confirmation to enter Join-,Sleep and SplitModes checked with : 
  - DMICom::DMIButtonConfirmJoin == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()
  - DMICom::DMIButtonConfirmSleep == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()
  - DMICom::DMIButtonConfirmSplit == DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus()

Affected component  : Mode Control \n
Affected function : SleepMode::handleMode \n
- State-machine SleepMode

@image html sleep_mode.png
@image latex sleep_mode.png


Affected component  : Mode Control \n
Affected function : SplitMode::handleMode() \n
- State-machine SplitMode

@image html split_mode.png
@image latex split_mode.png

Affected component  : DMI Handler \n
Affected function : enum DMIConfirmModeChange \n
- DMIConfirmModeChangeToJoin shall be added
- DMIConfirmModeChangeToSleep shall be added
- DMIConfirmModeChangeToSplit shall be added


Affected component  : DMI Handler \n
Affected function : enum DMIButtonStatus \n
- DMIButtonConfirmJoin, DMIButtonConfirmSleep, DMIButtonConfirmSplit shall be added


Affected component  : DMI Handler \n
Affected function : DMIMessageOutATPModesAndStatus::collectData() \n
- Convert to DMIATPModeJoin from to ATPJoin mode
- Convert to DMIATPModeSplit from to ATPSplit mode
- Convert to DMIATPModeSleep from to ATPSleep mode
- The dmiConfirmModeChange shall be set to DMIConfirmModeChangeToJoin if mode is Join and sub-mode joinWaitJoinConfirm
- The dmiConfirmModeChange shall be set to DMIConfirmModeChangeToSleep if mode is Join and sub-mode joinWaitSleepConfirm
- The dmiConfirmModeChange shall be set to DMIConfirmModeChangeToSplit if mode is Split and sub-mode joinWaitSplitConfirm


Affected component  : DMI Application \n
Affected function : ATP Mode and States Message from ATP to DMI \n
- 4=Join shall be added to Data13(Confirm change to mode needed) in ATP-mode and states message
- 5=Sleep shall be added to Data13(Confirm change to mode needed)  in ATP-mode and states message
- 6=Split shall be added to Data13(Confirm change to mode needed) in ATP-mode and states message


Affected component  : DMI Application \n
Affected function : DMI_ToATP_Data \n
- Add button status for confirm to Join
- Add button status for confirm to Sleep
- Add button status for confirm to Split

\section AdditionalMaterial Additional Material
  

\section Tasks Tasks
    
Suggested tasks are:

- Update of IF Spec ATP-DMI & DMI Application
- Update of ATP Components (A suggestion is to do all updates in one task, since the modes are interwoven and quite some changes are similar)

*/