namespace ATP
{

/** 
\if AsMainPage
\mainpage ATP Main Component Specification
@anchor atp_main
\endif

\ifnot AsMainPage
\class ATPMain
\endif

\section Purpose Purpose
This document specifies the software design for the adaptation of the AtpMain component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The AtpMain component is responsible to setup and interface the GSP-2 platform using the Vital IO Handler and the Vital Framework.

This component is a pure adaptation component without any inherited core component class.

The \ref mainFunction is called by the executable entry point \ref main().
\ref mainFunction in turn starts the loop through the vital framework.
This loop calls the \ref AtpMain::mainLoopFunction() at the end of the timeout and the \ref AtpMain::mainLoopFunction in turn calls \ref mainLoop which calls the \ref ATPApplication::run().

The AtpMain component manages the normal execution within the GSP-2 and other main application will be provided for other environments such as PC simulation.

\subsection DeploymentDiagram Deployment Diagram
N/A.

\subsection Dependencies Dependencies

The AtpMain component is dependent on or has dependencies with the following:
 - ATPApplication to call initialization and the run function.
 - VFW and VIOH Client interfaces are created and initialized.

Other components have dependencies to this component because they use its public types
 and methods, see \ref ATP::AtpMain Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

The initApplication function in the component is responsible to initialize the system environment before the
main loop can start executing.<br>

The init component should perform the following actions:
- Set up the Vital framework.
- Initialize the vital framework with the maximum number of checkpoints.
- Create the VIOH Client and initialize it.
- Create the instance of ATP Application.
- Add the other components of ATP to the component list.
- Call preInit() function of the application instance.
- Setup the cyclic sync timer and the callbacks for it.
- Setup the watchdog timeout
- Set up the handler to be called at vfwHalt().
- Set the handler to be called when halt has been invoked.
- Set the max time difference between CPU-A and CPU-B.
- Initialize cross compare by adding the state and version variable to be cross-compared.

The watchdog timer is to 3000 milliseconds during startup, but after that the value of \ref ATPApplication::atpAppCycleTime during the initalization phase.
When the initialization phase is finishing the \ref ATPApplication::atpAppCycleTime value is multiplied by the value of \ref AtpMain::overschedulingMarginDuringRunInPercentage + 100 and then divided by 100 to add the percentage of the overscheduling margin into the value.

The max time difference between CPU-A and CPU-B is handled by the VIOH Client but it set up by the AtpMain component during the initialization. This is the maximum time difference that the vital framework will allow before VFW will induce a fault.

There is additional initialization performed in the \ref mainLoop and will be described there.

\subsection ModeDependentOperation Mode dependent operation
N/A.

\subsection Scheduling Scheduling
The AtpMain will setup the VFW to cyclically trigger execution of the main ATP cycle, which will call upon the ATPApplication run function to perform the full functionality of the program.

\subsection mainFunction mainFunction()
The mainFunction is the starting point of the ATP software. It is responsible for setting up the 
environment. The main function should never exit and hence it ends with an infinite while loop. 

The class AtpMain implements the main function which is the entry point.
This then calls \ref AtpMain::mainFunction using the startup arguments.

It then handles the startup arguments which are simple and only decides which CPU the program executes on, A or B.

The flow diagram for the main function is shown below:

@image html atp_main_main.png "ATP main Function"
@image latex atp_main_main.png "ATP main Function"

\subsection mainLoop mainLoop()
The mainLoop is the function which is called by the vital framework upon the expiry of the sync timer.
This function is responsible for calling the interface functions of the ATP Application component.

The mainLoop should kick the watch dog timer which means to reset the VFW software watchdog using the vfwWatchdogKick function.
If this does not happen within "watchdogTimeout" milliseconds a SIGALRM will be sent towards our application.

After this, the first thing to happen in the mainLoop is a call to the Update() function of VIOH Client to get the most recent state of the inputs and
outputs.

The mainLoop should reactivate the cyclic sync timer using vfwSyncTimerReactivateCyclic() function.
This must be done within the callback, since if the timer is neither restarted nor stopped within the callback
it will be automatically stopped after the callback has returned.

It has 4 states. These are described in below flow diagram:<br>

@image html atp_main_mainLoop.png "ATP mainLoop"
@image latex atp_main_mainLoop.png "ATP mainLoop"

\subsection simulators Simulation
The AtpMain component has code specifically to enable it to be run with different simulators. During initialization one of the first things that is done is that right after creating the ATPApplication but before adding all components, the simulator components are added to the ATPApplication so these will also be handled alongside the normal components in ATPApplication.

When the normal loop is being run, the simulator are being called with the runIn functions before the ATPApplication is being run, while the runOut are being called after it.

In addition to this, there is code that is not being run when in a simulated environment, such as the initialization for the dispatcher, which is then handled seperately for the simulation.

\section ClassDiagram Class Diagram

@image html atp_main_class_diagram.png "AtpMain class diagram"
@image latex atp_main_class_diagram.png "AtpMain class diagram"

\section Diagnostics Diagnostics

\subsection Console Console-commands
N/A.

\subsection Analyze Analyze
N/A.

\section CoreAdaptation Core / Adaptation
N/A.

\section PreProcessor Pre-Processor Directives
N/A

\section Traceability Traceability
\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP BHP.

\subsection SSAS Architectural requirements
Safety tagged requirements and common requirements are specified in SCDS ATP BHP

Only the architectural requirements traced explicitly to this component is included in the table below.
Fulfillment of other architectural requirements allocated to the ATP is described in [SWAS].

Req         | Chapter             | Function
----------- | ------------------- | --------
AOS_AS-58 S | \ref mainLoop       | \ref ATP::AtpMain::mainLoop() 
AOS_AS-61 S | \ref mainFunction   | \ref ATP::AtpMain::mainFunction()
AOS_AS-79 S | \ref mainLoop       | \ref ATP::AtpMain::mainLoop() 
AOS_AS-81 S | \ref Initialization | \ref ATP::AtpMain::initApplication()
              

*/
}

