namespace ATP::Support
{

/** 
\if AsMainPage
\mainpage Cross Compare Component Specification
@anchor cc
\endif

\ifnot AsMainPage
\class AbstractCrossCompare
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractCrossCompare class, the core part of the Cross Compare component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
AbstractCrossCompare is responsible for cross comparing all vital variables, input and output data from CPU-A and CPU-B.
(For the definition of what data is "vital", please see the requirement AOS_AS 73 S.)

The purpose of the comparison is to detect software and hardware errors that could be fatal. In case of a comparison
mismatch, a SafetyHalt Event is raised.

Every component registers its vital attributes and objects (which need to be cross compared) during that component's
initialization. Also, all input and output data shall be added to cross compare each cycle, after the data is received or
before it is sent, respectively.

For performance reasons, CRC-64 sums are computed for all data to be cross compared, and then these sums (rather than
the data itself) are cross compared.

\subsection DeploymentDiagram Deployment Diagram
N/A.

\subsection Dependencies Dependencies
This component has dependencies to the following components:

- ATP Application: To get the ATP application version string.
- Config: For retrieving configuration versions.
- DMI Handler: For displaying event information on DMI.
- Event Handler: For reporting an event if any mismatch has occurred.

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractCrossCompare Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
AbstractCrossCompare shall be initialized before all other components that depend on it. In the preInit function,
AbstractCrossCompare registers the callbacks for [VFW] to be able to send and synchronize the data
between CPU-A and CPU-B.

\subsection ModeDependentOperation Mode dependent operation
AbstractCrossCompare is independent of ATP Mode of operation.

\subsection Scheduling Scheduling
AbstractCrossCompare has two functions that must be called each execution cycle: runIn() and runOut().
These must be called after all other components that have data to be cross compared.

\subsubsection runIn runIn()
The \ref AbstractCrossCompare::runIn() function first cross compares the application
and configuration versions.

Then it cross compares the input data provided by other components (see
\ref SettingUpCrossComparison).

In case there is a mismatch in the input data, a SafetyHalt event is raised. However, if there
is a mismatch in the versions, platform halt is performed. This is necessary because if there
is a version mismatch, it would be detected immediately after initialization. At that point, the
logging of events isn't necessarily operational yet.

The versions are cross compared separately by this component (instead of having another component
add the versions as vital data) because a mismatch in the versions must be detected as early as
possible.

\subsubsection runOut runOut()
The \ref AbstractCrossCompare::runOut() function first cross compares all output data and then the
attributes and objects added for cross comparison (see \ref SettingUpCrossComparison).

If the cross comparison of output data succeeded, then this data will be written to its
associated VFW channel, otherwise a SafetyHalt event is raised.

To reduce CPU load, only a limited number of attributes and objects are cross compared in each
cycle. This number must be carefully chosen so that each attribute and object is compared at
least once per hour.

\subsection SettingUpCrossComparison Setting Up Cross Comparison

\subsubsection CrossComparisonVariablesObjects Cross Comparison of Variables and Objects

The function \ref AbstractCrossCompare::addCrossCompareData() must be used by every
component which has variables and/or objects to be cross compared. It will register the given
\ref CrossCompareObject for cross comparison. The caller must instantiate an object of one
of CrossCompareObject's concrete subclasses, depending on the kind of data that is to be cross
compared. These are the subclasses:

- \ref CrossCompareSimpleType <br />
This template class implements cross comparison of simple, scalar types such as bool, uint8_t,
uint16_t and so on. To shorten and simplify this, there are also typedefs, see CrossCompareBool,
CrossCompareUint8 etc.

- \ref CrossCompareEnum <br />
This template class implements cross comparison of enumeration types. Instantiate the template
for the enumeration type you need to cross compare.

- \ref CrossCompareArray <br />
This template class implements cross comparison of arrays of simple, scalar types. Instantiate
the template for the type you need to cross compare.

- \ref CrossCompareComplex <br />
This template class is used for implementing cross comparison of complex classes (rather than
simple attributes). A class that is passed as a template argument to CrossCompareComplex must
implement the functions getWriteCrossCompareMaxSize() and writeCrossCompare(). These functions
are used by CrossCompareComplex to perform the cross comparison.

\subsubsection CrossComparisonInputData Cross Comparison of Input Data

The function \ref AbstractCrossCompare::addCrossCompareInputData() must be used by
every component that has input data to be cross compared. It will raise a SafetyHalt event if
there is not enough space to accomodate the input data in the buffer provided by Cross Compare.

\subsubsection CrossComparisonOutputData Cross Comparison of Output Data

The class \ref CrossCompareOutputChannel must be used by every component that has
output data to be cross compared. It wraps a VFW output channel and adds functionality for
cross comparison.

\subsection ComparisonDetails Comparison Details

The cross comparison is implemented by configuring the cross comparison
functionality in VFW, providing callbacks that implement the actual comparison. VFW will
then call these callbacks in each cycle in order to perform the cross comparison.

These are the callbacks (called by VFW) which perform the cross comparison:

- \ref AbstractCrossCompare::writeCrossCompareDataCallbackRunIn
- \ref AbstractCrossCompare::writeCrossCompareDataCallbackRunOut
- \ref AbstractCrossCompare::writeCrossCompareDataCallbackVersionCheck
- \ref AbstractCrossCompare::receiveCrossCompareDataCallbackIn
- \ref AbstractCrossCompare::receiveCrossCompareDataCallbackOut
- \ref AbstractCrossCompare::receiveCrossCompareVersionCheck

The cross comparison callbacks clear their internal buffers after the cross
comparison.

In order to reduce the amount of data to compare, CRC-64 sums are computed
for all data to be cross compared, and then these sums (rather than the data itself) are
cross compared.

\subsection CallbacksForCrossComparison Callbacks For Cross Comparison

\section ClassDiagram Class Diagram

@image html abstract_cross_compare_class_diagram.png "AbstractCrossCompare class diagram"
@image latex abstract_cross_compare_class_diagram.png "AbstractCrossCompare class diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
N/A.

\subsection Analyze Analyze
N/A.

\section CoreAdaptation Core / Adaptation
The core part of the component is responsible for the cross comparison of vital data.
The adaptation part is only responsible for instantiating the component.

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Safety tagged requirements and common requirements are specified in SCDS ATP Core.

Only the architectural requirements traced explicitly to this component is included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req         | Chapter                       | Function
----------  | ----------------------------- | --------
AOS_AS 45 S | \ref ComparisonDetails        | \ref AbstractCrossCompare::writeCrossCompareDataRunIn() <br /> \ref AbstractCrossCompare::writeCrossCompareDataRunOut()
AOS_AS 56 S | \ref Scheduling               | \ref AbstractCrossCompare::runIn()
AOS_AS 57 S | \ref Scheduling               | \ref AbstractCrossCompare::runIn()
AOS_AS 71 S | \ref Scheduling               | \ref AbstractCrossCompare::runIn()
AOS_AS 71 S | \ref SettingUpCrossComparison | \ref AbstractCrossCompare::addCrossCompareInputData()
AOS_AS 71 S | \ref ComparisonDetails        | \ref AbstractCrossCompare::preInit()
AOS_AS 72 S | \ref Scheduling               | \ref AbstractCrossCompare::runOut()
AOS_AS 72 S | \ref SettingUpCrossComparison | \ref AbstractCrossCompare::addCrossCompareOutputData()
AOS_AS 72 S | \ref ComparisonDetails        | \ref AbstractCrossCompare::preInit()
AOS_AS 74 S | \ref Scheduling               | \ref AbstractCrossCompare::runOut()
AOS_AS 74 S | \ref SettingUpCrossComparison | \ref AbstractCrossCompare::addCrossCompareData()
AOS_AS 75 S | \ref ComparisonDetails        | \ref AbstractCrossCompare::receiveCrossCompareDataCallbackIn() <br /> \ref AbstractCrossCompare::receiveCrossCompareDataCallbackOut() <br /> \ref AbstractCrossCompare::receiveCrossCompareVersionCheck()

*/

}
