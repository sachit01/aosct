/** 
\if AsMainPage
\mainpage CODSim Component Specification
\endif

\ifnot AsMainPage
\class CODSim
\endif

\section VersionLog Version Log

Version | Date       | Description                   | Signature
------- | --------   | ----------------------------- | ---------
1.0     | 2016-05-05 | Initial                       | akushwah
1.1     | 2016-05-10 | Updated Prel Design for CODSim| akushwah
1.2     | 2016-05-12 | updated after review          | akushwah
1.3     | 2016-05-24 | incorporated review comment   | akushwah

\section Summary Executive Summary

\subsection Purpose Purpose
This document specifies the software design for the CODSim component. 
Sufficient details provided for the module to be implemented.

\subsection IntendedAudience Intended Audience
This Software Component Design specification is intended to be read by:
-   SW-Designers 
-   Developers
-   Component Testers
-   Sub System Testers
-   Validators
-   Assessors
-   Verifiers

The reader of this document is assumed to be knowledgeable up to the state of the art of the following items:
-   Software Development Skills
-   The C/C++ programming language
-   Common SW data structures & algorithms

All of these items may be used without further references to their definitions

\subsection HowToRead How to read this document
For a reader who just wants to get a first overview of the AOS_IF150 ATP SW environment it should be enough to read the chapter "Overview".
This chapter shall be consumed anyway by every reader since it might contain additional hints on where important information is to be found.
 
\subsection References References
\subsubsection ProjectDocuments Project Documents
Ref        | Document Name                            | Document Id       | Version
---------- | ---------------------------------------- | ----------------- | ---------
[SWAS]     | ATP Software Architecture Specification  | 3NSS?             | 1.0
[SYSREQ]   | ATP (Sub)System Requirements             | DOORS             | Baseline ?
[SIR]      | Safety Integrity Requirements            | ?                 | ?
?          | AOS Simulator-Interface Interflo 150     | 3NSS?             | 1.3
?          | GSP2 Common Odometer User Application IF | 3NSS012264D0033   | 4.6

\subsection DefinitionsAndABbreviations Definitions and Abbreviations
\subsubsection Definitions Definitions 
Term           | Definition                                   
-------------- | ------------------------------------------------------------------
GSP-2          | Generic Safe Platform 2

\subsubsection Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
VFW            | Vital Framework
COD            | Common Odometer

\subsection EN50128 EN50128 Requirements
ID         | Technique/Measure           | Justification
---------- | --------------------------- | -----------------------------------------------
A4.2       | Modelling                   | Class diagrams and flowcharts
A4.3       | Structured methodology      | ...
A4.4       | Modular Approach            | ...
A4.5       | Components                  | ...
          

\subsection Deviations Deviations and questions
The variable data/Telegram/Doc name mentioned as '?' needs to be clarified on the later development phase of project.  

\latexonly \newpage \endlatexonly
\section Overview Overview


\subsection GeneralFunctionality General Functionality
COD Sim replaces the GSP-2 COD in the SIL and HIL test environments and provide the following functionality:
  - Receive and respond to static configuration from the ATP once at start up. 
  - Receive and respond to dynamic configuration from the ATP sporadically.
  - Store any incoming simulated speed/acceleration input data from the AOS PC-simulator.Store it along with a time-stamp in the COD Sim "mirror".
  - Send simulated odometer measurement data telegram to the ATP Component "Odometry" at requested intervals.

\subsection Dependencies Dependencies

- CODSim component reads the SimulatedMovement message periodically on channel "vfwChCODSim" from AOSPC.
- CODSim component reads the Odometer Static Configuration Telegram(StaticConfigType) and Odometer Dynamic Configuration Telegram(DynamicConfigType)
  on channel "UserAppl_To_Odo_A/B" from ATP component "Odometry".
- CODSim component writes the Odometer Configuration Response (ConfigResponse) on channel "Odo_To_UserAppl_A/B" to ATP component "Odometry".
- CODSim component writes the Odometer Measurement Data Telegram (OutputPacketType1) on channel "Odo_To_UserAppl_1_A/B" to ATP component "Odometry".
 

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design
\subsection BehaviouralDescription Behavioural description
\subsubsection Initialisation Initialisation 

CODSim
- Assigns the names of the 'configReadChannelName' and 'codSimValueWriteChannelName' Channel based on the VFWSide.
- opens codSimReadChannelName channel for reading SimulatedMovement message from AOSPC on channel"vfwChCODSim"
- opens configReadChannelName channel for reading Odometer Static Configuration Telegram(StaticConfigType) and Odometer Static Configuration Telegram(DynamicConfigType) telegram
  from ATP component "Odometry" on channel "UserAppl_To_Odo_A/B".
- opens codSimValueWriteChannelName channel for writing Odometer Configuration Response (ConfigResponse) on channel "Odo_To_UserAppl_A/B" and Odometer Measurement Data 
  Telegram(OutputPacketType1) telegram on channel "Odo_To_UserAppl_1_A/B".
- Setting the SimulatedMovementData validity flag 'dataValid' as false.
- Calculate the variable sendCycle(number of CODSim cycles between each sending of the Odometer Measurement Data Telegram to the ATP component "Odometry") as per below equation

  + sendCycle = (CH_1_ODO_MEAS_INTERVAL*codInterval)/codSimInterval
  + where 
      + The CH_1_ODO_MEAS_INTERVAL value will be extracted from Static Odometry Configuration telegram
      + codInterval = Interval at which Common Odometer(COD) is running i.e 50msec 
      + codSimInterval = Interval at which CODSim is running i.e 100msec
- Initialise the Estimated Maximum Distance(dMax), Estimated Nominal Distance(dNom) and Estimated Minimum Distance(dMin) as 0. 

\subsubsection ModeDependentOperation Mode dependent operation

\subsubsection CommunicationEstablished Communication established
At system start-up the ATP component Odometry shall send an Odometer Static Configuration Telegram (StaticConfigType) on channel "UserAppl_To_Odo_A/B" 
with a value in Q_VERSION. As long as this value is equal to 1, the CODSim replies with an Odometer Configuration Response Telegram (ConfigResponse) 
with Q_VERSION equal to 1 on channel Odo_To_UserAppl_A/B. When both parts have sent Q_VERSION = 1 the negotiation is complete and Connection is established between COD simulator and ATP component Odometry and hence other communication can start.
In case ATP component Odometry sends any other value than 1, the CODSim replies with Q_VERSION = 0 and the negotiation ends as incomplete and NO Connection is established between COD simulator and ATP component Odometry and hence other communication will never start. 

An event will be reported to EventHandler if Q_VERSION received in Static Odometer Configuration Telegram is 0 during Start up and NO Communication is established.

The CODSim has two functions that must be called each execution-cycle:

\paragraph runIn runIn()
- calls CODSim::codSimRead() to read the SimulatedMovement message from AOSPC
- calls CODSim::configRead() to read the Odometer Static Configuration Telegram(StaticConfigType) and Odometer Static Configuration Telegram(DynamicConfigType)
- Prepare and send the Odometer Configuration Response Telegram (ConfigResponse) to ATP component Odometry.
  The Odometer Configuration Response (ConfigResponse) variable should be prepared and send on channel Odo_To_UserAppl_A/B as per below mentioned table

  Variable              | Value
------------------------| ------------------------------------------------------
telegramType            | 3
qVersion                | 0 or 1 based on Q_VERSION received from Odometer Static/Dynamic Configuration Telegram
sdpVerMajor             | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.16 SDP_VERMAJOR
sdpVerMid               | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.17	SDP_VERMID
sdpVerMinor             | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.18	SDP_VERMINOR
configStatus            | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.19	CONFIGURATION_STATUS
calStatus               | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.8	CAL_STATUS
tDvTrain                | current time stamp(currentTimestamp)
calTachoDistance1       | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.10	CAL_TACHO_DISTANCE
tachoCalResultStatus1   | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.85	TACHO_CAL_RESULT_STATUS 
calTachoDistance2       | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.10	CAL_TACHO_DISTANCE
tachoCalResultStatus2   | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.85	TACHO_CAL_RESULT_STATUS
calDopplerDistance      | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.6	CAL_DOPPLER_DISTANCE
dopplerCalResultStatus  | Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.23	DOPPLER_CAL_RESULT_STATUS

@image html cod_sim_run_in.png
@image latex cod_sim_run_in.png

   
\paragraph codSimRead codSimRead()
-  checks for any message available on vfwChCODSim channel to read from AOSPC
-  reads the SimulatedMovement Message buffer from AOSPC
-  extracts the speed, acceleration,nSensorMaxError,nSensorMinError from the SimulatedMovement Message
-  set the dataValid as true
-  convert the extracted data from network to host byte order via VFW
-  store the extracted speed,acceleration,nSensorMaxError,nSensorMinError and timestamp in buffer.
-  get the reference timeStamp via VFW and store the timeStamp

@image html cod_sim_codsimread.png
@image latex cod_sim_codsimread.png

\paragraph configRead configRead() 
-  checks for any message available on  UserAppl_To_Odo_A/B channel to read.
-  checks is there any connection established[refer section 1.4.1.3 Communication established] or not.
  +  If connection is established, checks the TELEGRAM_TYPE of Telegram.
    + report an event to Event Handler if TELEGRAM_TYPE = 1 (Odometer Static Configuration Telegram).The event is reported because the Odometer Static Configuration Telegram is not 
      expected more than once.
    + convert fields in the telegrams from network to byte order via VFW and store the Odometer Dynamic Configuration Telegram in buffer if TELEGRAM_TYPE = 2(Odometer Dynamic Configuration Telegram).
  +  If there is NO Connection[refer section 1.4.1.3 Communication established], checks the TELEGRAM_TYPE of Telegram.
    +  report an event to Event Handler if TELEGRAM_TYPE = 2 (Odometer Dynamic Configuration Telegram)because a Odometer Dynamic Configuration Telegram is not expected before 
       the communication is established by the reception of a Odometer Static Configuration Telegram.
    +  if TELEGRAM_TYPE = 1(Odometer Static Configuration Telegram), then checks the Q_VERSION. 
    +  set qVersion as 0 and report and event to Event Handler,if Q_VERSION = 0.
    +  perform the below if Q_VERSION = 1.
        +  establish connection.
        +  set qVersion as 1.
        +  convert the Odometer Static Configuration Telegram from network to host byte order via VFW.
        +  store the converted Odometer Static Configuration Telegram.
        +  calculate the sendCycle as per given equation during initialisation.

@image html cod_sim_configread.png
@image latex cod_sim_configread.png

\paragraph runOut runOut()
-  increment the CycleCount by 1.
-  calls the CODSim::codSimValueWrite() functions.

@image html cod_sim_run_out.png
@image latex cod_sim_run_out.png

\paragraph codSimValueWrite codSimValueWrite()
- checks if connection is established or not.
- check that there is valid simulation data available i.e. dataValid is true.
  + check if it is time to send the Odometer Measurement Data Telegram with simulated data to the ATP.
    +  get the current timestamp(currentTimestamp) via VFW
    +  estimate the Simulated data as per given Equation
      + vNom = vSim+ aSim * (currentTimestamp-timeStamp)
      + vMin = vNom (1- (nSensorMinError/100))
      + vMax = vNom (1+ (nSensorMaxError/100))
      + dNom = dNom(previous) +  vNom * ( currentTimestamp-prevTimeStamp)
      + dMin = dMin(previous) +  vMin * ( currentTimestamp-prevTimeStamp)
      + dMax = dMax(previous) +  vMax * ( currentTimestamp-prevTimeStamp)

      + where
      + timeStamp = time stamp saved when the SimulatedMovement Message was received from AOSPC.
      + prevTimeStamp = time stamp saved at the previous estimation of Estimated Maximum Distance(dMax), Estimated Nominal Distance(dNom) 
        and Estimated Minimum Distance(dMin).
    + NOTE: The unit and resolution of the vNom,vMin,vMax,vSim,aSim,dNom,dMin and dMax should be taken in to consideration while estimating the Simulated data.
    
-   create the Odometer Measurement Data Telegram as per given table below
 Variable               | Value
------------------------| ------------------------------------------------------
telegramType            |4
qVersion                |1
qOdoSafe                |1
qControl                |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.77	Q_CONTROL
qDirErr                 |1
tDvTrain                |current time stamp(currentTimestamp)
prodTime                |prevTimeStamp  
aTrain                  |aSim
vMax                    |As calculated above
vNom                    |As calculated above
vMin                    |As calculated above
dMax                    |As calculated above
dNom                    |As calculated above
dMin                    |As calculated above
tRadarPlausible         |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.93	T_RADAR_PLAUSIBLE
slipSlideStatus1        |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.83	SLIP_SLIDE_STATUS_1
slipSlideStatus2        |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.84	SLIP_SLIDE_STATUS_2
vTacho1                 |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.88	V_TACHO 
vTacho2                 |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.88	V_TACHO 
vDoppler                |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.26	V_DOPPLER
dTacho1                 |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.27	D_TACHO1/2
dTacho2                 |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.27	D_TACHO1/2
dDoppler                |Refer to the document "GSP2 Common Odometer User Application IF" chapter 3.12.28	D_DOPPLER

-   send Odometer Measurement Data Telegram on channel Odo_To_UserAppl_1_A/B  

@image html cod_sim_value_write.png
@image latex cod_sim_value_write.png

\subsection ClassDiagram Class Diagram


@image html cod_sim_class_diagram.png
@image latex cod_sim_class_diagram.png


\subsection ExternalInterfaces External Interfaces

See chapter 2.1 "Public Member Functions"


\subsection Diagnostics Diagnostics

\subsubsection Trace Trace-commands

\paragraph BriefTrace Level 1 (Brief trace)
Display the brief Messages sent/received once a second by CODSim.

\paragraph DetailedTrace Level 2 (Detailed)
Displays same as level 1 and in addition brief data of each message once a second by CODSim.

\paragraph VeryDetailedTrace Level 3 (Very detailed)
Displays same as level 2 and in addition detailed data of each message once a second by CODSim.

\paragraph Analyze Analyze
This is a simulator component which do not expect any values to be candidate for the Analyzer tool.

\subsection FaultReporting Fault Reporting
All errors/events shall be reported using a standardized ATP Error/Event handling.
The list of errors/events and their severity and categories remain to be defined.

\subsection TestFeatures Test Features
Describe any features that facilitate the testing of the design, both in development and at its target location.

\subsection Miscellaneous Miscellaneous functions
\subsubsection ConvertOrder Convert to network order
The Vfw-functions declared in vfw_buffer.h shall be used to convert to/from network-byte order before an incoming message is processed and before an outgoing message is sent.

Use
- wfwInitBuffer() to initialise a buffer for use.
- vfwPut*() to write values to buffer and convert to network byte order
- vfwGet*() to read values from buffer and convert to host byte order


\section PreProcessor Pre-Processor Directives

No pre-processor directives available for this component

\section DesignSafety Design Safety

\subsection SIR SIR Requirement tracing

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
 SIR-      | The SW Application shall only        | Memory is only allocated during init


\section DesignReliability Design Reliability
The failure modes of the design and their effect on operation and performance.

\section Traceability Traceability

\subsection Requirements Requirements
The applicable requirements for this component is defined in [SYSREQ] and [SIR]. 
However, the [SWAS] adds constraints on the design and implementation of this component.

Each applicable Safety Integrity Requirement is addressed in the Design Safety chapter.
       

*/

