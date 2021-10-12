/**
\if AsMainPage
\mainpage AOS-PC OBRD Simulator
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-10-02 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TIMS           | Train Integrity and Monitoring System
OBRD           | On-board Broken Rail Detection solution
OBRD BOS       | OBRD Back-Office Server

\section Introduction Introduction

\subsection Design Design Overview

The OBRD simulator will simulate the OBRD Equipment in the AOS-PC Environment.
It will act as an OBRD BOS (TCP-Client) and connect to the AOS Dispatcher (or ATP in SIL environment).

The main functionality will be to get the front-train position and path from the LCS-simulator and calculate the simulated
rear-end position (loco length, track-data and delay is provided through configuration/gui in AOS-PC).
The rear-end position is send to the AOS through the FFFIS AOS-OBRD OBRD unit status report.

The simulator will be integrated in AOS-PC, and thus have its own configuration and windows in AOS-PC.

The GUI will allow the user to set- and save parameters that is owned by the OBRD BOS.

Note: The descriptions in this document will concentrate on describing the changes/additions to existing code in AOS-PC. 
      It is therefore recommended to have the AOS-PC source code available.

The previous sollution with the 'TIMS Simulator' will be removed completely in both AOS-PC and ATP.      

All messages in FFFIS AOS-OBRD will be supported in version 1.0 of OBRD-Sim.

Parsed messages from AOS:

- Protocol Version Packet
- Message Rejection Packet

Generated messages from OBRD-Sim:

- Protocol Version Packet
- OBRD unit status report

 The simulated sequences are described in FFFIS AOS-OBRD, chapter 6.2 Messaging Sequence.


\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix

N/A - This TDD describes a test-tool, and is thus not covered in product-requirements.


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The OBRD simulator will fit in the current structure of AOS-PC, and will be an adaption for BHP.

The GUI related functionality is included in AOSPC-DLL, and the simulation and communication 
towards the ATP is located in OBRDSim-DLL.

The OBRDSim needs to exchange information with the LCSSim (Path and Train front-position) in order to simulate train rear-position.

The suggested architecture with respect to the updated/new class- and package view:

@image html obrdsimulation_architecture.png
@image latex obrdsimulation_architecture.png

The sequence for each Tick() from the Form1 Class and ending in ATPCom in OBRDSimulation:

@image html obrdsimulation_tick.png
@image latex obrdsimulation_tick.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

N/A

\subsection ExternalInterfaceDescription External Interface Description

The OBRDSafetyProtocol-class to pack- and unpack data from the safety-protocol is re-used from the ATP implementation.

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection ModuleAOSPC Module - AOS-PC

\subsection ModuleOBRDSimulation Module - OBRDSimulation

This is a new DLL to handle the actual OBRD Simulation. The LCSSimulation can be used as inspiration since the dataflow will be similar.

OBRDSimDLL.h:

TODO: 

 - List all config/GUI parameters as public variables.
 - In constructor, Read and store all the configuration parameters
 - In constructor, Create the ATPCom() with proper ip/port


OBRDSimDLL.cpp:

TODO: 

 - Tick() - Basically calling the helper methods CallATPCom() and RunOBRDSimulation() for each tick()

 - CallATPCom()
 
   Provide simulated values to the ATPCom object (the calculated Track/offset, timestamp and last car BP, eg atpCom->sndLastCarPressure = curLastCarPressure). Call the Tick() method of ATPCom to process in- and output data.

 - RunOBRDSimulation()

  The simulation shall send a ProtocolVersion message when a new connection is setup.
  If the ProtocolVersion is rejected, the connection shall be disconnected and re-initiated after a certain time.

  If ProtocolVersion is successfully verified the simulation shall calculate the position of the rear-end of train at a configured 
  intervall using the following input:

  - Track data from GUI/configuration
  - Path data received from LCSSimulation (new Path data replaces old received data)
  - Front-train position and timestamp from LCSSimulation
  - Trainlength from GUI/Configuration
  - Delay time for simulated OBRD meassurement from GUI/Configuration

  Statechart for the simulated behaviour:

@image html obrd_states.png
@image latex obrd_states.png


  The rear-end of train is calculated (and sent to ATP) at a configurable interval when in Normal mode.

  The front position of the train is received from the LCSSimulation every second and stored together with a time-stamp.
  In order to have a fresh value for the front position, the status-report to the ATP is synched with the reception of 
  the front-train position from LCS (i e OBRD-status report is only to be sent in multiples of LCS status report).
 
  With all the data-sources mentioned above it is possible to calculate the simulated rear-end of the train.

  The simulated delay for the transmission between OBRD, OBRD BOS and ATP is used when interpolating
  between the stored time- and front-position values from LCS:

@image html calc_rear_pos.png
@image latex calc_rear_pos.png


ATPCom.h:

TODO:

 - Define public values to be received- and sent from OBRD simulation (e g unsigned short sndLastCarPressure, unsigned char recv MajorProtocolVersion)
 - Define Log data for GUI (e g array<String^>^ guiToATP, array<String^>^ guiFromATP), similar to APTCom.h in LCSSimDLL.

ATPCom.cpp:

Preferable, take inspiration from ATPCom.cpp in LCSSimDLL to have a similar behaviour.

TODO: 

 - Tick() - Sends/Receives actual data to ATP: Connect (if not connected), call ReadFromATP(), call SendToATP()
 - ReadFromATP() - Reads ATP data from socket, and calls InterpretATPBuffer() to do the interpretation of data.
 - InterpretATPBuffer() - Parses the data into member variables in ATPCom
 - SendToATP()- Sends ATP data on a socket, and calls AssembleATPData() to assemble the simulated data into actual Messages in FFFIS AOS-OBRD.
 - AssembleATPData() - Assembles the data from member variables into the actual messages.

\subsection ModuleTimsSimulation Module - TimsSimulation

All code related to TimsSimulation shall be removed, since it will not longer fill any purpose.


\subsection ModuleTimsSimulationATP Module - TimsSimulation(ATP)

All code related to TimsSimulation in ATP shall be removed, since it will not longer fill any purpose.

\section UserInterfaceDesign User Interface Design
\subsection DescriptionOfTheUserInterface Description of the User Interface
\subsubsection ScreenImages Screen Images

The following menus shall be available via a TAB-bar in the OBRDSim.
All data in the menues shall also be included in the AOSPC.ini file.


The first TAB will include must frequently changed parameters, and the possibilty to inject different kind of faults:

@image html OBRDSim_gui_tab1.png
@image latex OBRDSim_gui_tab1.png

The second TAB is listing all tracks, and their length from the obrdsim_tracks.ini-file. It also contains the import possibility of Site-data to be stored in this ini-file.

@image html OBRDSim_gui_tab2.png
@image latex OBRDSim_gui_tab2.png

The third TAB lists the Safety Layer parameters, and the Version information sent in the Protocol Version Message.

@image html OBRDSim_gui_tab3.png
@image latex OBRDSim_gui_tab3.png

The last TABs will be presenting the last OBRD Status Report sent to AOS, and last received Rejection received from AOS.

@image html OBRDSim_gui_tab4.png
@image latex OBRDSim_gui_tab4.png

@image html OBRDSim_gui_tab5.png
@image latex OBRDSim_gui_tab5.png

\subsubsection AOSPCini AOSPC.ini updates

The AOS-PC ini-file will be updated with parameters related to communication and data transmitted from OBRD:

\code{.cpp}

[OBRDSim]
AOSOBRDPortToConnect = 30151

LocoLength = 100
TimeStampDelay = 1000
LastCarBrakePressure = 100
StatusReportPeriodicity = 5
FaultInjection = 0
EnableCommunication = 1

SiteId = 10
ReceiverID = 11
SenderID = 12
LocoID = 13

ProtocolMajorVersion = 1
ProtocolMinorVersion = 0

\endcode

\subsubsection Trackini obrdsim_tracks.ini 

A new ini-file, 'obrdsim_tracks.ini', is used to store information about all tracks and their length:

\code{.cpp}

TrackNumber_1= 1
Length_1= 1000
TrackNumber_2= 1000
Length_2= 1000
TrackNumber_3= 3
Length_3= 1000

\endcode

Note: The TrackNumber_<X> and Length_<X> means that X will have values from 1 up to as many tracks that needs to be defined (max 65535).

This data can (if wanted) be imported and parsed from a site-data xml-file in the GUI (trackId and length):

\code{.cpp}

  <Object>
    <ObjectType>BHPBTrack</ObjectType>
    <ObjectName>4400TS403</ObjectName>
    <Parameter>
      <Name>trackId</Name>
      <Value>
        <UnsignedShortValue>403</UnsignedShortValue>
      </Value>
    ...
    ...
      <Name>length</Name>
      <Value>
        <NumberValue>
          <Value>1000</Value>
          <Decimals>1</Decimals>
        </NumberValue>
      </Value>
    ...
    ...
  </Object>
    

\endcode


\section ImplementationDistribution Task Distribution

- GUI according to examples in this document
- Parsing of Site-Data(trackId and length) to be stored in the track-data ini-file.
- New OBRDSim Class
- New ATPCom Class


\section AdditionalMaterial Additional Material

- FFFIS AOS-OBRD  (3NSS015103D0234)
*/