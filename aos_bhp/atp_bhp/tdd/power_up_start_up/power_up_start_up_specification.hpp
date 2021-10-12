/**
\if AsMainPage
\mainpage Power Up and Start Up Changes(section (6.1.2 & 6.3.1))
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-04-04 | Purposed Changes wrt Power up & start up requirement | spandita


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
Below changes are purposed as per the doors requirements defined in section 6.1.2(power up) & 6.3.1(start up)

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS  1715 | The AOS shall enter "vehicle ready" state if Signal Locomotive Ready is active.     |Not Implemented
AOS 1717  | In "vehicle ready" state the AOS shall start health state supervision of Vehicle Interface, TCC & set the AOS Status output.| Not Implemented
AOS 21    | In "vehicle ready" the AOS shall test the emergency brake relay. If the test fails a Safety Halt event shall be issued  | Not Implemented
AOS 1090  | If the test of the emergency brake relay is OK then AOS shall Activate the AOSOk signal & deactivate the emergency brake|Not Implemented
AOS 1674  | The AOS shall after that the driver is logged in change to mode Configuration if Q_SETUP field in CONFIRM_CONFIG to Re-Registration/Re-position             |Not Implemented
AOS 658   | In mode Power Up the AOS shall allow selection of mode Configuration by the Driver if AOSOk signal,cabin is active & driver is authorized      |Not Implemented
AOS 159   | The AOS shall set 'ATP reset' in "PositionReport" message if current mode is "Power Up"     | Implemented
AOS 1085  | The AOS shall clear ATP reset status in PositionReport message if Driver selection of mode Configuration resulted in mode change or yard/possession/shunting mode is acknowledged by TCC | Partially Implemented
AOS 1721S | At 'start up' the AOS shall check the integrity and the range of configuration, runtime and maintenance parameter values against the permitted ranges | Not Implemented
AOS 1188  | At 'start up' the AOS shall Toggle AOS Status output every 100 ms,Activate the SB and Deactivate all other outputs  |  Not Implemented
AOS 656   | At 'start up' the AOS shall verify the configuration and accessibility of the required core units:ATP,COD,OPC,BTM & DMI | Not Implemented
AOS 18S   | At 'start up' the AOS shall remain in 'start up' as long as any of the core units is starting up & issue a platform halt event incase of any failure | Not Implemented
AOS 1713  | When all core units are configured and accessible the AOS shall Start health state supervision of core units and enter in power up mode | Not Implemented
AOS 19    | At 'start up' the AOS shall compose a startup history presented to the Driver | Partially Implemented
 
 \subsection ExternalInterfaceDescription External Interface Description
 Following external interface description is used to implement the requirement described above.
 Seq No | Document Name                                      | Version
 -------|----------------------------------------------------|----------
 1.     | Interface Specification ATP - MMI                  | 1.35
 2.     | FFFIS TCC-AOS                                      | 5.8

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS1715 AOS 1715
Affected component  : Mode Control \n
Need to implement in powerUpActivation sub mode of power up mode. \n
Call to AbstractLocoIO::corePtr()->getCoreInputValue() function to get status of "LCS ready" state.\n

\subsection  AOS1717 AOS1717
Affected components  : Mode Control, Vehicle Comm and Radio Handler  \n
Regarding health supervision, \n
it is still open and transferred the queries to requirement team for further details.\n
Current design : \n
Create new public interface in vehicle com and radio handler like  startupAndHealthSupTest() \n
inside of startupAndHealthSupTest() \n
Check for connection status and return true if it is established.\n
Call to these public interface need to done in runPowerUpActivation() function \n
Regarding AOS Status output(lamp) \n
Create private member(atpStatusOutput) in power up mode class. \n
Create getter function in abstract mode control to get the value of atpStatusOutput member.\n
Set the atpStatusOutput to true.\n
Call the getter function of abstract mode control in runout function of loco IO and set the respective output pin.
 

\subsection  AOS21 AOS21
Affected component  : Mode control \n
Create the function in power up mode like performEmBrakeRelayTest() and returns true if successful.\n
call to this function need to be done in runPowerUpActivation() function of power up mode.\n
Brake relay test will be done in following manner :\n
- Release EB and verify it 
- Apply EB and verify it  
- Release EB and verify it \n
In case of any failure raise safety halt event .

\subsection  AOS1090 AOS1090
Affected  component  : Mode Control \n
Create private member(atpOkStatus) in power up mode class. \n
Create getter public member function in abstract mode control to get the value of atpOkStatus member.\n
Upon successful completion of brake relay test set the atpOkStatus to true. \n
Call the getter function of abstract mode control in runout function of loco IO and set the respective output pin.\n
Regarding the deactivation the Emergency brake :\n
Create private member(ebAppliedTestInPowerUp) in power up mode class. \n
Create getter public member function in abstract mode control to get the value of ebAppliedTestInPowerUp member.\n
Reset the ebAppliedTestInPowerUp \n
Call the getter function of abstract mode control in run function of abstract brake component and reset the ebApplied flag.\n


\subsection  AOS1674 AOS1674
Affected component  : mode control  \n
In  runTrainConfigWaitTSetup() function of train config mode \n 
Get the value of Q_SETUP block from getConfirmConfig() function of message handler and stored in confirmConfigValue variable .\n
and check the value against the list of available sub modes \n
code snippet: \n
  switch (confirmConfigValue) \n
  { \n
  case TrainSetupReRegistration: \n
  isNewConfig = false; \n
  break; \n
  }\n
\subsection  AOS658 AOS658
Affected  component  :  mode control\n
Get the status of cab active and AOSOK from locoIO component by using getCoreInputValue() function \n
Get the driver authorization status by using getDriverLoginSeqState(). \n
Check should be done in run function of abstract_mode_control. \n
if successful change the powerUp mode to configuration .

\subsection  AOS159 AOS159
Affected  component  : mode control \n
Implemented in runPowerUpStart() function.

\subsection  AOS1085 AOS1085
Affected function and component  :  mode control \n
Regarding the Yard OR Possession OR Shunting acknowledgment: \n
This should be handled in respective handle mode() function of respective mode.\n
To check the status of acknowledgment of Yard/Shunting/Possession \n
call getYardAcknowledge()/ getShuntingAcknowledge()/getPossessionAcknowledge functions of message handler.\n
if successful clear 'ATP reset' flag.\n
Regarding the Driver selection: \n
On successful completion of requirement AOS658 reset 'ATP reset' block of position report.

\subsection  AOS1721S AOS1721S
Affected component : mode control and config  \n
Need to implement new checkConfigParameterRange() function in config component.\n
and call to this function should be in runPowerUpTest() function of power up mode.\n
In case of any failure raise the AOS_HALT.\n

Note: currently AOS_HALT is assumed as platform halt which may get changed later.

\subsection  AOS1188 AOS1188
Affected component  : mode control \n

- Toggle the value of atpStatusOutput variable of abstract mode control. 
- Activate the SB \n
Create private member(sbAppliedTestInPowerUp) in power up mode class. \n
Create getter public member function in abstract mode control to get the value of sbAppliedTestInPowerUp member.\n
Set the sbAppliedTestInPowerUp to true. \n
Call the getter function of abstract mode control in run function of abstract brake component and set the sbApplied flag.
- Deactivate the other outputs.\n
No need to implement \n
As,In init() of ATP all outputs are getting deactivated.

The mentioned activities should be taken care in runPowerUpTest() of power up mode.

\subsection  AOS656 AOS656
Affected component  : mode control ,odometry & DMI Handler \n
Create the function startUpTestAndHealthSup() in power up mode and call to this function should be present in runPowerUpTest() of power up mode\n
Call following mentioned functions in this startUpTestAndHealthSup().\n
Current design :
- ATP : No Need to check
- COD : create the public function startupAndHealthSupTest() in odometry and check whether init is done.
- OPC : TODO(Will be updated once the interface between OPC & BTM will get fixed)
- BTM : TODO(Will be updated once the interface between OPC & BTM will get fixed)
- DMI : create the public function startupAndHealthSupTest() in DMI handler and check whether DMI is connected.

\subsection  AOS18S AOS18S
Affected component  :   mode control \n
In case of any kind of failure while checking the requirement no AOS656, Raise the AOS_HALT.\n
Assumption:AOS_HALT is assumed as platform halt.

\subsection  AOS1713 AOS1713
Affected component  : mode control ,odometry & DMI Handler \n
Create the function startUpTestAndHealthSup() in power up mode.\n
Call following functions in this function.\n
Regarding health supervision, \n
It is still open and transferred the queries to requirement team for further details.\n
current design :\n
- ATP : No Need to check 
- COD : create the public function startupAndHealthSupTest() in odometry and check whether init is done.
- OPC : TODO(Will be updated once the interface between OPC & BTM will get fixed)
- BTM : TODO(Will be updated once the interface between OPC & BTM will get fixed)
- DMI : create the public function startupAndHealthSupTest() in DMI handler and check whether DMI is connected.

Upon the success, change the mode to powerUpActivation.

\subsection  AOS19 AOS19
Affected component  : DMI handler and mode control \n
Function need to implement in DMI handler like addStartupMsg(char_t str). \n
Which should add string to the output buffer of start up history .\n
Update it with the message like COD/OPC/BTM is Up and running on completion.\n
After transmission of start up history message to DMI,invalidate the output buffer of start up history of DMI handler.\n

\section AdditionalMaterial Additional Material

*/