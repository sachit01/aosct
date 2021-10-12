namespace ATP::Support
{

/** 
\if AsMainPage
\mainpage Cross Compare Component Specification
@anchor cc
\endif

\ifnot AsMainPage
\class CrossCompare
\endif

\section Purpose Purpose
This document specifies the software design for the CrossCompare class, the BHP adaptation
part of the Cross Compare component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The CrossCompare class is derived from the AbstractCrossCompare class. CrossCompare only
has an instance function in addition to the functionality of the abstract class.

The instance method shall be called in order to retrieve the instance of the component.
When the instance method is called for the first time, it instantiates the component.

\subsection DeploymentDiagram Deployment Diagram
N/A.

\subsection Dependencies Dependencies
Other components have dependencies to this component because they use its public types
and methods, see \ref CrossCompare Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
All the initialization activity is done in the Core part of the component.

\subsection ModeDependentOperation Mode dependent operation
The Cross Compare component is independent of ATP mode.

\subsection Scheduling Scheduling
All the scheduled activity is done in the Core part of the component.

\section ClassDiagram Class Diagram

@image html cross_compare_class_diagram.png "Adaptation CrossCompare class diagram"
@image latex cross_compare_class_diagram.png "Adaptation CrossCompare class diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
N/A.

\subsection Analyze Analyze
N/A.

\section CoreAdaptation Core / Adaptation
The core part of the component (AbstractCrossCompare) is responsible for the cross comparison of
vital data. The adaptation part (CrossCompare) is only responsible for instantiating and providing
access to the component, see \ref CrossCompare::instance().

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATP BHP.

*/

}
