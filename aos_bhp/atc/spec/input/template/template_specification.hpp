/*

Instructions:

* Text enclosed in these tags <i>...</i> are placeholders or instructions.
  You need to replace such text with content (or in some cases remove the text).
  And remove the tags, of course!

* If a heading is not applicable, write "N/A" below that heading rather than remove it.

* The level of detail in a flowchart should be reasonable. Here are a few things to keep in mind:
  - The flowchart doesn't need to be an exact copy of the code.
  - It does not need to visualize every condition within the function.
  - It should not show functionality of called functions. Called functions can be referred to and explained separately.
  - Traces and internal logging should not be in the flowchart.

* Please arrange the class diagram in Visual Studio so that the width of the exported PNG image
  is a maximum of 1200 pixels wide.

* When exporting PNG images from Visio, choose a width of about 4 to 12 inches (400 to 1200 pixels),
  depending on the size and shape of the diagram.

* Please look at the HTML export to make sure that all images have a sensible size.

* Add a caption to all images (see @image below).

* Edit "namespace X::Y" (see below) to indicate the namespace where the component resides. This way,
  Doxygen can automatically create hyperlinks to all your classes, methods and variables.

*/

namespace X::Y
{

/**
\if AsMainPage
\mainpage <i>Component Name</i> Component Specification
\endif

<i>Please note that component names should be written with separate words (e.g. Log Handler)
and class names like in the code (e.g. LogHandler)</i>

\ifnot AsMainPage
\class Template
\endif

\section Purpose Purpose
This document specifies the software design of the <i>Component Name</i> component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
<i>Briefly describe the main function(s) of the component</i>

\subsection DeploymentDiagram Deployment Diagram
<i>Use this section if you want to visualize the component(s), application(s) and/or
system(s) that this component interacts with</i>

<i>If you don't supply this diagram, replace the image with "N/A"</i>

@image html dummy.png "Deployment diagram"
@image latex dummy.png "Deployment diagram"

\subsection Dependencies Dependencies
This component has dependencies to the following components:

+ <i>Abc, for handling bla bla ...</i>
+ <i>Def, for retreiving bla bla ...</i>

Other components have dependencies to this component because they use its public types
and methods, see \ref InsertClassNameHere Class Reference.

<i>Modify the reference above to point to the generated documentation for this component</i>

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization
<i>Describe the initialization</i>

<i>No flowchart is needed unless the initialization is complex</i>

\subsection ModeDependentOperation Mode Dependent Operation

\subsection Scheduling Scheduling
The <i>Component Name</i> component has these function(s) that must be called each execution cycle:

\subsubsection run run()
<i>Add function description</i>

<i>No flowchart is needed unless the function is complex</i>

@image html dummy.png "run() flow chart"
@image latex dummy.png "run() flow chart"

\subsection topic1 Topic 1
<i>Add sections like this one if you need to describe further topics</i>

<i>Each topic should describe a functionality but can also be used to describe methods,
state machines etc</i>

\subsection topicCrossCompare Cross-compare
<i>Add information about any special cross compare handling implemented in the component, e.g. Targets</i>
<i>Remove this topic if there is no such implementation</i>

\section ClassDiagram Class Diagram
<i>At a minimum, this section should contain a class diagram</i>

<i>If the component contains several classes and if you haven't described them already,
please provide a brief description of the responsibilities of each class (except for the
component class itself)</i>

@image html dummy.png "Class diagram"
@image latex dummy.png "Class diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
The following component-specific console commands are implemented:

+ <i>"command1" which prints bla bla ...</i>
+ <i>"command2" which sets bla bla ...</i>

\subsection Analyze Analyze
References to variables to be accessed by an external Analyzer tool are prepared at initialization.
Some of the statistics values may be of such interest.

\section CoreAdaptation Core / Adaptation
<i>Provide information about the relation between Core and Adaptation components</i>

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS XXX.

<i>In place of XXX, write either ATC, ATP Core, ATP BHP or Dispatcher</i>

The requirements relevant for this component are:

<i>Requirements that are common for all components should not be listed in the table, instead they should be listed in the 
traceability table in the main page.</i>

Req             | Chapter         | Function
--------------- | --------------- | --------
AOS 8888 S      | \ref runIn      | runIn()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS XXX.

<i>In place of XXX, write either ATC, ATP Core, ATP BHP or Dispatcher</i>

Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req             | Chapter         | Function
--------------- | --------------- | --------
AOS_AS 9999     | \ref runIn      | runIn()

*/

}
