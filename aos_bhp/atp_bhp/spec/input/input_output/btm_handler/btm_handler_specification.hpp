namespace ATP::IO
{

/** 
\if AsMainPage
\mainpage BTM Handler Component Specification (BHP adaptation)
@anchor bh
\endif

\ifnot AsMainPage
\class BTMHandler
\endif

\latexonly \setlength{\parskip}{4pt} \endlatexonly

\section Purpose Purpose
This document specifies the software design for BTM Handler, the adaptation of the BTM Handler component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The BTMHandler class is the adaptation of the core AbstractBTMHandler class. Its main purpose
is to instantiate the AbstractBTMHandler class for use within the ATP user process. BTMHandler generates a singleton
object and provides access to it.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The adaptation part of BTM Handler component is not dependent on any other component.
It only depends on its core class AbstractBTMHandler.

Other components have dependencies to this component because they use its public types and methods,
see \ref BTMHandler Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
There is no implementation of init() in the adaptation. All the initialization takes place in the core component's
init() function.

\subsection ModeDependentOperation Mode dependent operation
The adaptation part of BTM Handler is independent of any modes.

\subsection Scheduling Scheduling
The component is scheduled by calling the core component's runIn() and runOut() functions.

\section ClassDiagram Class Diagram
The class diagram for the BTMHandler adaptation is shown below:

@image html btm_handler_class_diagram.png Class Diagram
@image latex btm_handler_class_diagram.png Class Diagram

\section Diagnostics Diagnostics

\subsection Console Console Commands
N/A

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
All of the functionality is implemented in the core class. This class only instantiates the core class.

\section PreProcessor Pre-Processor Directives
N/A

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].
Common functional requirements are described in SCDS ATP BHP.

All relevant requirements are implemented by the core component.

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP] and are traced and described in the core SCDS.

*/

}
