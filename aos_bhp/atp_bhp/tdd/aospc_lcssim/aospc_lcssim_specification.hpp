/**
\if AsMainPage
\mainpage AOS-PC LCS Simulator
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-02-13 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
LCS            | Locomotive Control System

\section Introduction Introduction

\subsection Design Design Overview

The LCS simulator will simulate the EMD Locomotive Control System (LCS) in the AOS-PC Environment.
It will act as an LCS (TCP-Client) and connect to the AOS Dispatcher (or ATP in SIL environment).

The simulator will be integrated in AOS-PC, and thus have its own configuration and windows in AOS-PC.

The GUI will allow the user to set parameters that is owned by the LCS. It will also be able to inspect parameters 
transfered from AOS.

Note: The descriptions in this document will concentrate on describing the changes/additions to existing code in AOS-PC. 
      It is therefore recommended to have the AOS-PC source code available.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
123        | abcd                                  | efgh


The following messages will be supported in version 1.0 of LCSSim (the ATO-related messages is excluded):

Parsed messages from AOS:

- AOS Status
- ATP Command
- Movement Authority
- Train Composition Message
- Path

Generated messages from LCS Sim:

- Train Status
- Train Composition

The simulated sequences containing these messages are described in AOS-LCS BHPB Sequencing
INTERFLO 150 (3NSS015103D0172). The simulation will follow the scenarios in this document.


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The LCS simulator will fit in the current structure of AOS-PC, and be an adaption for BHP.

There are several DLLs in the AOSPC project, only the AOSPC-DLL and LCSSim-DLL will be affected.
The form- and GUI related functionality is included in AOSPC-DLL, and the simulation and communication 
towards the ATP is located in LCSSim-DLL.

@image html aospc_lcssim.png
@image latex aospc_lcssim.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

N/A

\subsection ExternalInterfaceDescription External Interface Description

N/A

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection ModuleAOSPC Module - AOS-PC

LCSSimForm.h:

This form needs to be updated with the new GUI parameters according to the User Interface chapter later in this document. It is 
basically the parameters included in messages described in the FFFIS AOS-LCS.

This form also includes the code that updates the data to be used in simulator (and to be sent to AOS) before lcsSim->Tick() is called.
After the lcsSim->Tick() is called (and data might have been received from AOS) the GUI elements shall be updated.

Pseudo code as follows:

\code{.cpp}
Tick()

  Setup the new input data to LCSSim - e g lcsSim->stateInactive = cbInactive->Checked;

  lcsSim->Tick();

  Update GUI from lcsSim->XXX - e g UPDATE_IF_DIFFERENT(tbHorn->Text, lcsSim->currHornActive ? "On" : "Off");
\endcode


TODO:
- Before Tick() is called, change the input data to LCSSim to the new output-data ('Train Status Data' and 'Train Composition Data' fetched from GUI.
- After Tick was called(), remove all unnecessary code.
- Update the new data received from ATP or simulator to GUI elements.
- Keep the update of arrays to update sent/received data to ATP:
  UPDATE_IF_DIFFERENT(lvDataFromATO->Items[i]->SubItems[1]->Text, lcsSim->atoCom->guiFromATO[i]);
  UPDATE_IF_DIFFERENT(lvDataToATO->Items[i]->SubItems[1]->Text, lcsSim->atoCom->guiToATO[i]);
- Remove all VSIM and LocoSim related code
- Remove Pantograph/PIC code

\subsection ModuleLCSSimDLL Module - LCSSimDLL

The LCSSimDLL module will include the functionality to simulate the LCS and to communicate with the ATP. This chapter outlines 
what changes and additions that is needed to adapt to BHP functionality.

TODO General in LCSSimDLL:
- Remove all VSIM and LocoSim related code
- Remove Pantograph/PIC code
- Files to save: 	ATOCom.cpp -> Rename to ATPCom.cpp

LCSSimDLL.h:
------------

TODO:
- Configuration: Remove old LCSSim configuration-code and reuse LocoSim code instead. Rename to ATPPort and ATPIP, set default port to 30150 and IP to 127.0.0.1


LCSSimDLL.cpp:
--------------

The Structure of LCSSimulation::Tick() method is as follows:

\code{.cpp}
void LCSSimDLL::LCSSimulation::Tick(void)
{
  CallATOCom();
  ..
  RunLCSSimulation();
  ..
}


void LCSSimDLL::LCSSimulation::CallATOCom(void)
{
  Setup Values to be send:
  e g atoCom->sndCurrMode = ...

  Tick()
    Open socket
    
    ReadFromATO()
      recCnt = atoSocket->ReceiveFrom(buffer, atoEndPointRemote);
      InterpreteATOBuffer(buffer, recCnt)

    SendToATO()
      sndMsgBytes[cnt++] = ...
      atoSocket->SendTo(sndMsgBytes, cnt, SocketFlags::None, atoEndPointRemote);

  Received data to simulation:
  e g currRefSpeed            = atoCom->recRefSpeed ...;
}
\endcode

TODO:
- Change from ATOCom to ATPCom
- Update to BHP parameters to be sent, and also state which message-type to be sent

\code{.cpp}
void LCSSimDLL::LCSSimulation::RunLCSSimulation(void)
{
...
...
}


\endcode

TODO:
- Remove old simulation, and update current LCS simulation parameters according to received data from AOS or GUI.
- Simulate data according to sequences as described in AOS-LCS BHPB Sequencing INTERFLO 150. A 

- Keep track of which message is to be sent to ATP next, and also if a status message is to be sent in response to status from AOS.
  (The SendToATP function will fetch data as needed from simulation.)



ATOCom.cpp:
-----------

TODO:
- Tick() Copy/Reuse TCP-Client implementation from LocoSimCom::Tick(), change variables accordingly.

- SendToATP()	Copy/Reuse code from LocoSimCom::SendToLocoSim() and create proper message according to LCS Simulation values.
  Application message format is described in FFFIS AOS-LCS INTERFLO 150. 
  The application message is created in AssembleData(...).
  Pseudo code as follows:

\code{.cpp}

  EmpMsg empMsg
  ClassDMsg classDMsg

  *appBuf = msg.getEMPBodyBuffer()

  AssembleAppData(appBuf) //Assemble application message with simulation data put in appBuf

  msg.addEMPEnvelope(..)

  Copy empMsg.getEMPBuffer() to classDMsg.getClassDBody()

  classDMsg.addClassDHeader(...)

  TCP-send classDMsg.getCLassDBuffer() to ATP (Socket->Send(....))
\endcode

- ReadFromATP()	Copy/Reuse code from LocoSimCom::ReadFromLocoSim() and decode all layers with EMP/Class-D API.
  Application message format is described in FFFIS AOS-LCS INTERFLO 150. 
  The application message is parsed in InterpretMessage(...).
  Pseudo code as follows:
  
\code{.cpp}

  EmpMsg empMsg
  ClassDMsg classDMsg

  *classDBuf = classDMsg.getClassDBuffer()

  TCP-receive to classDBuf from AOS

  classDMsg.parseClassDHeader(...)

  Copy classDMsg.getClassDBody() to empMsg.getEMPBuffer()

  empMsg.parseEmpHeader(..)

  InterpretAppData(empMsg.getEMPBodyBuffer()) // Decode application message and store data in LCS simulation data


\endcode

\section UserInterfaceDesign User Interface Design
\subsection DescriptionOfTheUserInterface Description of the User Interface
\subsubsection ScreenImages Screen Images
The following menus shall be available via a TAB-bar in the LCSSim. The information displayed is basically from the different message-types in the 
FFFIS AOS-LCS INTERFLO 150.
In addition to this, two extra tabs will be presenting the data last sent/received to/from ATP.

@image html gui1_1.png
@image latex gui_1.png

@image html gui1_2.png
@image latex gui_2.png

\subsubsection ObjectsAndActions Objects and Actions

Status information(2nd tab) and and Train Composition Data(3rd tab) in LCS can be modified in GUI. 
This information will be used in simulation towards the ATP.


\section ImplementationDistribution Task Distribution

- Implementation of LCS simulator main functionality (Clean old LCS simulator, TCP-client, include EMP/Class-D, AOS Status + Train Status, Hardcoded values)
- GUI for AOS Status/Train Status
- Implementation of ATP Command, MA, Path
- GUI for ATP Command, MA and Path
- Implementation of Train Composition (both directions)
- GUI for Train Composition


\section AdditionalMaterial Additional Material

- FFFIS for AOS-LCS communication: FFFIS AOS-LCS INTERFLO 150 (3NSS015103D0164)
- AOS-LCS BHPB Sequencing INTERFLO 150 (3NSS015103D0172)

*/