/**
\if AsMainPage
\mainpage PowerUp and StartUp Revisit (Req 5.1.3 & Req 5.3.1)
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-01-09 | TDD Power-Up & Start-Up requirements          | akushwah


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
Below changes are purposed as per the doors requirements defined in section Requirement 5.1.3 Power Up & 5.3.1 Startup 

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
ReqId           | Short requirement description                                                                                                                                                  | AOS Implementation Status 
--------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------| --------------------------
AOS_BHPB 2809 S | When start up procedure is completed AND the Locomotive Ready input is 'Ready for operation' the AOS shall start to control the following parallel vehicle interface outputs   | -
-               |  as required by the AOS supervision. Unless a system reaction is triggered the AOS shall set the outputs as follows: Traction cut-off: Traction allowed                        | Not Implemented
AOS 1674        | The AOS shall after that the driver is logged in change to mode Configuration, procedure Confirm Existing Train Configuration, if: The Sleeping signal is not active, AND      | -
-               | The Q_SETUP field, in block INITIATE_CONFIG, in the last PositionReportRequest message from TCC prior to driver login, is set to Re-registration or Re-position.               | Partially Implemented
AOS 658         | The AOS shall allow the driver to select the ATP mode Configuration if: The sleeping signal is not active ANDhe Driver is logged in AND The Q_SETUP field, in block            | -
-               | INITIATE_CONFIG, in the last PositionReportRequest message from TCC prior to driver login, is set to Registration                                                              | Partially Implemented
AOS 159         | The AOS shall set 'ATP reset' in "PositionReport" message if current mode is "Power Up".                                                                                       | Implemented
AOS 1085        | The AOS shall clear ATP reset status in PositionReport message if: Driver selection of mode Yard OR Possession OR Shunting was acknowledged by the TCC OR Driver selection of  | -
-               | mode Configuration resulted in mode change.                                                                                                                                    | Implemented
AOS 1721 S      | At 'start up' the AOS shall check the integrity and the range of configuration, runtime and maintenance parameter values against the permitted ranges. If the verification     | -
-               | fails the system shall issue a Platform Halt event.                                                                                                                            | Implemented
AOS 2702        | At 'startup', AOS shall read values for all Configuration parameters from non-volatile memory.                                                                                 | Partially Implemented
AOS 632         | At 'startup' the AOS shall enable the TCC radio link.                                                                                                                          | Implemented
AOS 1188 S      | At 'start up' AOS shall set the following vehicle interface outputs as follows: Emergency brake: Applied, Emergency Brake Applied: Applied, Service Brake: Applied,            | -
-               | AOS OK: AOS not operational, Buzzer: No sound, AOS Status: Toggle with default values of frequency and duty cycle as 1Hz and 50% respectively, unless the values are overridden| -
-               | by the adaptation                                                                                                                                                              | Partially Implemented
AOS 2684        | At 'start up' AOS shall use the following default values for the vehicle interface inputs: ATO mode selector: Manual, Cabin selection: No cabin active, Driving direction:     | -
-               | Neutral, Emergency Stop Active: Emergency brake not applied, Isolation Switch mode: Run mode                                                                                   | Partially Implemented
AOS_BHPB 2807 S | At 'start up' the AOS shall set the following vehicle interface outputs as follows: Traction cut-off: Traction not allowed                                                     | Not Implemented
AOS_BHPB 2808   | At 'start up' AOS shall use the following default values for the vehicle interface inputs: High Rail Specific: Rail, Non-Leading Locomotive: Leading, Emergency Brake Cut-out: | -
-               | Not 'cut-out', Traction Cut-off Feedback: Traction enabled, Brake Pressure: Brake not applied (configurable value)                                                             | Not Implemented
AOS 656         | At 'start up' the AOS shall verify the configuration and accessibility of the required core units:ATP,COD,OPC,BTM,DMI(at least one up and running if system configure with two)| Partially Implemented
AOS 18 S        | At 'start up' the AOS shall: Remain in 'start up' as long as any of the core units is starting up AND Issue a Platform Halt event if the integrity test fails for any core unit| Implemented
AOS 19          | At 'start up' the AOS shall compose a startup history presented to the Driver (if the Driver Interface is configured and accessible).                                          | Implemented
AOS 1713 S      | When all core units are configured and accessible the AOS shall: Complete the 'start up' procedure, Start health state supervision of the core units, Start the health         | -
-               | supervision of the TCC interface and Start the TCC message handling                                                                                                            | Partially Implemented
AOS 2683        | When start up procedure is completed the AOS shall start to control the following vehicle interface outputs as required by the AOS supervision. Unless a system reaction is    | -
-               | triggered the AOS shall set the outputs as follows: Buzzer: A short beep sound, AOS Status: On/Illuminated                                                                     | Not Implemented


 \subsection ExternalInterfaceDescription External Interface Description
 Following external interface description is used to implement the requirement described above.
 Seq No | Document Name                                      | Version
 -------|----------------------------------------------------|----------
 1.     | Interface Specification ATP - MMI                  | 2.13
 2.     | FFFIS TCC-AOS                                      | 5.11
 3.     | AOS Hardware I/O Interface                         | 1.5

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS_BHPB2809S  AOS_BHPB 2809 S 
No update is required for this requirement as digital output Traction cut-off will be removed in AOS Hardware I/O Interface ver1.6 onwards.\n
There is a possibility that this requirement might get updated/deleted. Hence, No update is required for this and we can ignore Traction cut-off output signal at this time of project.\n    

\subsection  AOS1674 AOS 1674
Affected components  : Mode Control, Message Handler  \n
In function RadioMessageInPositionReportRequest::parseMessageData(), rename the BTypeConfirmConfig RadioBlockType as BTypeInitiateConfig as per FFFIS TCC AOS ver5.11.\n
Rename the corresponding function name also. In FFFIS TCC AOS V5.11, CONFIRM_CONFIG block has been rename to INITIATE_CONFIG. \n
Hence, in order to make consistency in code and spec, the above renaming should be done.\n
Also, the Block description XML present in TCCSim also needs to updated accordingly.\n 


\subsection  AOS658 AOS 658
Affected  component  :  Mode Control\n
First part of the requirement is implemented in PowerUpMode::runPowerUpWaitConfigOrSleep() \n
Also, for the second part of requirement AOS 658 (After driver selection, the AOS shall change to ATP mode Configuration, procedure Create New Train Configuration) is \n
implemented in ConfigModeRequestSeq::runConfigDmiButtonConfirmed() \n.
Hence, remove the code line commonData.isATPReset = false from function PowerUpMode::runPowerUpFinishConfig() as mentioned below \n

\code
void PowerUpMode::runPowerUpFinishConfig(CommonDataForModes &commonData)
{
setNextMode(ATPModeConfiguration);
}
\endcode


\subsection  AOS159 AOS159
Affected  component  : Mode Control, Message Handler \n
Implemented in runPowerUpStart() function.

\subsection  AOS1085 AOS1085
Affected component  :  Mode Control, Message Handler \n
No need to reset the ATP here in the runPowerUpFinishConfig() function.\n
Hence, update the code for the function PowerUpMode::runPowerUpFinishConfig() as mentioned below

\code
void PowerUpMode::runPowerUpFinishConfig(CommonDataForModes &commonData)
{
setNextMode(ATPModeConfiguration);
}
\endcode

\subsection  AOS1721 AOS 1721
Affected component  :  Config \n
Already Implemented.

\subsection  AOS2702 AOS 2702
Affected component  :  Config \n
Already Implemented. Implementation Status in Doors needs to be changed.

\subsection  AOS632 AOS 632
Affected component  :  Radio Handler \n
Already Implemented.

\subsection  AOS1188S AOS 1188 S
Affected component  :  LocoIO, Mode Control \n
In function, AbstractLocoIO::runOut(void), check whether startup procedure is running via AbstractModeControl::corePtr()->getStartUpPassed().
If startup procedure is running, then set the below Digital Core outputs signals as mentioned below 
- Emergency brake: Applied \n
  + setCoreDigitalOutputValue(EmerBrake1, false); \n
  + setCoreDigitalOutputValue(EmerBrake2, false); \n
- Emergency Brake Applied: Applied \n
  + Currently Emergency Brake Applied(Emergency Brake Active) in LocoIO as digital Adaptation Output. This needs to be moved to Core part as mentioned in AOS HW IO Interface Ver1.5.
  + setCoreDigitalOutputValue(EmerBrakeActive, true); 
- Service Brake: Applied
  + setCoreDigitalOutputValue(ServiceBrake, false);
- AOS OK: AOS not operational
  + setCoreDigitalOutputValue(ATPOk, false);
- Buzzer: No sound
  + setCoreDigitalOutputValue(Buzzer, false);
- AOS Status: Toggle with default frequency(1 Hz) 
  In function PowerUpMode::runPowerUpTest(), Currently lamp status is being toggle after 2 secs.\n
  This implementation needs to be changed. \n
  create a virtual function toggleAOSLampStatus() in power_up_mode.hpp file which will return the constant variable defaultLampToggleFrequency = 10 \n
  so that it be modified by adaption in future.\n

\code
Update the function PowerUpMode::runPowerUpTest() as mentioned below:

if (0U == (toggleWaitCycle % toggleAOSLampStatus()))
{
//Toggle the AOS Status
commonData.atpLampStatus = !commonData.atpLampStatus;
}
//increment the toggle cycle
++toggleWaitCycle;
\endcode

  
\code
Pseudo Code for AOS AOS 1188S Implementation in AbstractLocoIO::runOut()

I have tried to make the AbstractLocoIO::runOut() function more readable, hence some cosmetic changes has been done.

Update the function AbstractLocoIO::runOut(void) as mentioned below:
bool isATPOk = false;
bool buzzerStatus = false;
bool sbApplied = true;

bool eb1Applied = true;
bool eb2Applied = true;

if (Kernel::AbstractModeControl::corePtr()->getStartUpPassed())
{
isATPOk = Kernel::AbstractModeControl::corePtr()->getATPOKStatus();
buzzerStatus  = Kernel::AbstractModeControl::corePtr()->getBuzzerStatus();
sbApplied = Supv::AbstractBrake::corePtr()->getSbApplied();

bool ebApplied = Supv::AbstractBrake::corePtr()->getEbApplied(); // Emergency Brake outputs are Active Low!
//Is Brake Test in Progress?
if ((Supv::AbstractBrake::corePtr()->getBrakeTestStatus() == Supv::BrakeTestStatusInProgress) && !ebApplied)
{
//Set value of Emergency brake outputs
eb1Applied = Supv::AbstractBrake::corePtr()->getEb1TestApplied();
eb2Applied = Supv::AbstractBrake::corePtr()->getEb2TestApplied();
}
else
{
eb1Applied = ebApplied;
eb2Applied = ebApplied;
}
}

//Set value of Emergency brake outputs
setCoreDigitalOutputValue(EmerBrake1, !eb1Applied);
setCoreDigitalOutputValue(EmerBrake2, !eb2Applied);

// Service Brake output is Active Low!
//set value for service brake output
setCoreDigitalOutputValue(ServiceBrake, !sbApplied);

//set value for ATPoK output signal
setCoreDigitalOutputValue(ATPOk, isATPOk);
//set value for Lamp output signal
setCoreDigitalOutputValue(Lamp, Kernel::AbstractModeControl::corePtr()->getLampStatus());
setCoreDigitalOutputValue(OFFOut, Kernel::AbstractModeControl::corePtr()->getPowerOffValue());
//set value for Buzzer output signal
setCoreDigitalOutputValue(Buzzer, buzzerStatus);

// All the core output values are updated now. Write these outputs.
writeOutputs();

\endcode


\subsection  AOS2684 AOS 2684
Affected component  : Mode Control and LocoIO \n
- ATO mode selector: Manual
- Cabin selection: No cabin active. This is already taken care in AbstractModeControl::manageCabActiveStatus()
- Driving direction: Neutral
- Emergency Stop Active: Emergency brake not applied
- Isolation Switch mode: Run mode

In function AbstractLocoIO::runIn(), set the default values of above signals as mentioned in below pseudo code.\n 
Also, Isolation Switch mode needs to be defined as per HW IF spec ver1.5. update the enum CoreDigitalInputs for Isolation A and Isolation B.\n
Update the LocoIO::LocoIO() with Isolation A and Isolation B for Input 1 and Input 2. \n
 - digitalInputs[ISOLA].init(1U, "IsolationA",true); \n
 - digitalInputs[ISOLB].init(2U, "IsolationB",false); \n

The handling of the Isolation Switch is not compatible with our current AOSPC and VSIM configuration.

\code
Pseudo Code for Requirement 2684

if(AbstractModeControl::corePtr()->getStartUpPassed())
{
  readInputs();
}
else
{
atoModeSwitchPos = ATOModeManualPos; \\ atoModeSwitchPos variable is implemented under ATO switch task, which is under review.
locoTravelDirection = DirNone;
isolationSwitchMode = RunMode;
emergencyStopActive = EBNotApplied;
}
\endcode

\subsection  AOS_BHPB2807 AOS_BHPB 2807
Affected component  :  LocoIO \n
No update is required for this requirement as digital output Traction cut-off will be removed in AOS Hardware I/O Interface ver1.6 onwards.\n
There is a possibility that this requirement might get updated/deleted. Hence, No update is required for this and we can ignore Traction cut-off output signal at this time of project.\n


\subsection  AOS_BHPB2808S AOS_BHPB 2808 S
Affected component  :  LocoIO \n
In Constructor LocoIO(), all the Digital/Analog inputs should have one more parameters in their init() function, which are being called from constructor LocoIO().\n
Also, this will lead to update the AbstractLocoIO::DigitalInput::init() and AbstractLocoIO::AnalogInput::init() functions, where the isValid will be assigned with value passed.\n 
\code
Pseudo Code for Constructor LocoIO()

// Initialize Digital Core Inputs.
digitalInputs[Manual].init(5U, "Manual",false);
digitalInputs[Supervised].init(6U, "Supervised",false);
digitalInputs[Automatic].init(7U, "Automatic",false);
digitalInputs[Cab1].init(8U, "Cabin1",false);
digitalInputs[Cab2].init(9U, "Cabin2",false);
digitalInputs[Fwd].init(10U, "Forward",false);
digitalInputs[Rev].init(11U, "Reverse",false);
digitalInputs[LCSRdy].init(12U, "LCSRdy",false);
digitalInputs[EmerStopActive].init(13U, "EmerStopActive",false);
digitalInputs[OFFIn].init(14U, "OFFIn",false);

// Initialize Digital Adaptation Inputs. Adaptation Inputs are placed after Digital Core Inputs in the inputs array.
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(RoadM)].init(15U, "RoadM",false);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(RailM)].init(16U, "RailM",true);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(Isolated)].init(17U, "TCOFB",false);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(NCU)].init(18U, "NONLEAD",false);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(EBCutOut1A)].init(19U, "EBCutOut1A",true);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(EBCutOut1B)].init(20U, "EBCutOut1B",false);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(EBCutOut2A)].init(21U, "EBCutOut2A",true);
digitalInputs[static_cast<uint8_t>(NumOfCoreDigitalInputs) + static_cast<uint8_t>(EBCutOut2B)].init(22U, "EBCutOut2B",false);


//Initialize Core Analog Inputs.
// TODO: Core Analog inputs are not yet defined


// Initialize Analog Adaptation Inputs. Adaptation Analog Inputs are placed after Analog Core Inputs in the inputs array.
analogInputs[static_cast<uint8_t>(NumOfCoreAnalogInputs) + static_cast<uint8_t>(BrakePressure1)].init(1U, "BrakePressure1");
analogInputs[static_cast<uint8_t>(NumOfCoreAnalogInputs) + static_cast<uint8_t>(BrakePressure2)].init(2U, "BrakePressure2");

Where as all the Digital/Analog Outputs will remain same.

\endcode


 \subsection  AOS656 AOS 656
 Affected component  :  Mode Control, BTM Handler, Odometry, DMI Handler, remove ATPApplication from tagging and add Mode control tag \n
 This is fully implemented in code now PowerUpMode::startUpTestAndHealthSup().

 \subsection  AOS18S AOS 18 S
 Affected component  :  Mode Control \n
 This is fully implemented in code now PowerUpMode::runPowerUpTest().

 \subsection  AOS19S AOS 19 S
 Affected component  :  Mode Control and DMI Handler, Add Mode Control tag \n
 This is fully implemented in code now PowerUpMode::startUpTestAndHealthSup().

 \subsection  AOS1713S AOS 1713 S
 Affected component  :  Mode Control \n
 This is fully implemented in code now PowerUpMode::runPowerUpTest() and PowerUpMode::runPowerUpActivation()

 \subsection  AOS2683S AOS 2683 S
 Affected component  :  Mode Control \n
 Buzzer: Short beep 
 AOS Status: On

 Update the below pseudo code in the function PowerUpMode::runPowerUpTest() as \n
 \code
 if ((supTestWaitCycle) >= waitCycle)
 {
 if (startUpTestAndHealthSup())
 {
 ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "Health supervision and Core integrity test Passed!", "MC");
 modeState = powerUpActivation;
 waitCycle = 0U;
 //Set the Buzzer(short beep) and AOS Status (on)
 commonData.atpLampStatus = true;
 AbstractLocoIO::coreptr()->raiseBuzzer(shortBeep); //Currently in implementation
 }
 else
 {
 ++waitCycle;
 }
 }
 else
 {
 ATC::AbstractLogHandler::corePtr()->writeToLog(ATC::BriefLog, "Health supervision and Core integrity test Failed!", "MC");
 //raise platform halt if exceeded the supTestWaitCycle
 ATC::aosHalt(__FILE__, __LINE__, "Core block Integrity and health supervision test failed!");
 }

\endcode

*/
