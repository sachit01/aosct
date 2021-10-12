namespace ATP::AnalyzerIF
{
/**
\if AsMainPage
\mainpage Analyzer IF Component Specification (BHP adaptation)
@anchor aif
\endif

\ifnot AsMainPage
\class AnalyzerIF
\endif

\section Purpose Purpose
This document specifies the software design for the BHP adaptation for the Analyzer IF component.

\latexonly \newpage \endlatexonly
\section Overview Overview
The AnalyzerIF class is the BHP adaptation of the core AbstractAnalyzerIF class. The main purpose of AnalyzerIF class
is to instantiate the AbstractAnalyzerIF class for use within ATP.

\subsection GeneralFunctionality General Functionality
This adaptation component creates and provides an instance for Analyzer IF (including the core) object.
It also is responsible for creating the \a UnitData message that is sent during initial phase of the communication setup with AOS Analyzer.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The AnalyzerIF adaptation component is dependent of the following components:
- BasicIP : To provide the connection id for the Analyzer IF that is used for handling the TCP/IP communication towards AOS Analyzer.

Other components are dependent on this components functionality by its public functions. <br>
Refer to Public Member Functions from \ref ATP::AnalyzerIF

\section FunctionalDesign Functional Design

\subsection Initialization Initialization
AnalyzerIF handles the instantiation of the core component (AbstractAnalyzerIF) within its constructor.

\subsection ModeDependentOperation Mode dependent operation
This component is not dependent on any ATP modes.

\subsection Scheduling Scheduling
The \a run function is not implemented in this adaptation component.

\subsection writeAIFUnitData The UnitData message
AnalyzerIF is responsible to implement the handling of the applications specific message \a UnitData.
This is realized in the writeAIFUnitData() function. The function creates the message consisting of the name, version and protocol version
formated as follows. The message is then written to the outgoing buffer.
 - [UnitDataStart]\\r\\n
 - Name  \%s\\r\\n
 - Version \%s\\r\\n
 - ProtocolVer \%u\\r\\n
 - [UnitDataEnd]\\r\\n

\section ClassDiagram Class Diagram
@image html analyzerif_class_diagram.png "Adaptation Analyzer IF class diagram"
@image latex analyzerif_class_diagram.png "Adaptation Analyzer IF class diagram"

\section Diagnostics Diagnostics
\subsection Console Console-commands
N/A

\subsection Analyze Analyze
References to variables to be accessed by an external Analyzer tool shall be prepared at initialization.

\section CoreAdaptation Core / Adaptation
N/A

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability
\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATP BHP.

Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

*/

}