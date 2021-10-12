/**
\if AsMainPage
\mainpage Loco I/O
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-03-06 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction

\subsection Design Design Overview

Investigate the design for updating I/O:s according to latest requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS1173    | Vital Digital Outputs                 | Partially Implemented
AOS1174    | Vital Digital Inputs                  | Implemented
AOS1175    | Non Vital Digital Outputs             | Partially Implemented
AOS1176    | Non Vital Digital Outputs             | Partially Implemented

Input from System Department regarding I/O-Adaptation is also included in this document.

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The ATP-core/adaption for Loco I/O component needs to be updated according to requirements as follow.

Note: The requirements only specifies Core-IOs. Adaption I/O:s are implemented as specified in I/O document (currently
in work by Jan Kiesling)

@image html locoio_in.png
@image latex locoio_in.png

@image html locoio_out.png
@image latex locoio_out.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection LocoIOATP Loco I/O ATP 

\subsubsection LocoIOCore Loco I/O - Core

abstract_loco_io.hpp:
---------------------
The core inputs/outputs shall be declared in CoreOutputs/CoreInputs enums.

TODO: Move ISOL to adaptation

\subsubsection LocoIOAdaptions Loco I/O - Adaptions

loco_io.hpp:
------------
The adaptations inputs/outputs shall be declared in AdapOutputs/AdapInputs enums.

TODO: Change name on LEAD to NCU (Non Control Unit)

loco_io.cpp:
------------
The Constructor (LocoIO) must map the required core/adaptions inputs/outputs to the proper I/O signal according to the
System Architectural Design chapter.

TODO Core: Move ISOL to Adaptation

TODO Adap: Change name on LEAD to NCU (Non Control Unit)

\subsection LocoIOAOSPC Loco I/O AOS-PC

Note: In this section Inputs/Outputs refers to AOS (i e Output is Output from AOS, and Input to AOS-PC)

In AOS-PC no distinction is made between Vital/Non Vital I/O.

AOSPC.ini:
----------
The I/O:s are mapped to hw-signal in the [LocoSim] section.

TODO: Add Inputs: ISOL. NCU, EBC1A, EBC1B, EBC2A, EBC2B

LocoIO.h:
---------
Digital Outputs are declared LocoIO class.

TODO: -

LocoIO.cpp:
-----------
Digital Outputs are read and fetched from receivedOutputValuesA[] through the ReadOutputData() method.

Digital Inputs are fetched from GUI and sent via the SendInputData() method.

TODO: Add DI Adaption Values: ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B in SendInputData(), and set corresonding locoParams values.

TODO: Add ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B in LocoIO::Tick() signature 


LocoParams.h:
---------------
The mapping of Inputs/Outputs are declared in LocoParams class.

TODO: Add DI Adaption Values as Inputs: ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B

LocoParams.cpp:
---------------
The I/O:s are mapped to default hw-signals and initialized from ini-file in ReadFromIniFile().

TODO: Add initialization of DI Adaption Values: ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B

LocoSimDLL.cpp:
---------------

Simulation for Loco.

TODO: Add DI Adaption Values: ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B from GUI in LocoSimulation::Tick(), send further to LocoIo()->Tick()

LocoSimForm.h
-------------
GUI for LocoSim.

TODO: Add DI Adaption Values: ISOL, NCU, EBC1A, EBC1B, EBC2A, EBC2B from GUI to LocoSimul->Tick()

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

The inputs to AOS consists of buttons or other mechanism to enter values, the outputs from AOS are simply displayed in 
non editable check buttons (clbOutputs) and fetched from LocoOutputs (and receivedOutputValuesA[]) in LocoSimDll.h.

\subsubsection ScreenImages Screen Images

@image html locoio_gui.png
@image latex locoio_gui.png

\subsubsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution

  - AOS PC
  - AOS 
  
\section AdditionalMaterial Additional Material


*/