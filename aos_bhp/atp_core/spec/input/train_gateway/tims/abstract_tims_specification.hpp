namespace ATP::TG
{

/**
\if AsMainPage
\mainpage TIMS Component Specification (Core)
@anchor tm
\endif

\ifnot AsMainPage
\class AbstractTIMS
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractTIMS class, the core part of the TIMS component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
Detection of train integrity is essential for ensuring safety.
The TIMS component is responsible for the last car supervision and checks the integrity of the train.

AbstractTIMS performs the following:
- Updates the TIMS states, see \ref TIMSStates.
- Calculates the latest confirmed position of the last car of the train, see \ref UpdatingLastCarPosition.
- Calculates the latest confirmed rear position of the train, see \ref HandlingRearPosition.
- Handles manual integrity confirmations from the driver, see \ref HandlingDriverActions.

Please note that AbstractTIMS only handles position calculations and manual integrity confirmations.
Automated integrity reporting must be implemented by the adaptation class, see \ref CoreAdaptation.

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
AbstractTIMS depends on the following components:

- DMI Handler for getting input from the driver
- Message Handler for retrieving the most recent MA
- Mode Control for retrieving current ATP mode
- Odometry for retrieving travel direction, slip/slide and balise windows
- Position for retrieving the train's position, position accuracy etc
- Targets for retrieving expected travel direction and safety margin
- Tracks for translating between odo positions and track positions
- TSetup for retrieving the train setup

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractTIMS Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection init init()
Initializes member variables and starts cross-comparison.

\subsection ModeDependentOperation Mode Dependent Operation
TIMS Supervision can be \a Supervised in these ATP modes only:
- BaliseSearch
- Join
- Location
- Normal
- StaffResponsible

This mode dependency is implemented in \ref updateTimsSupervision.

Also, last car position is invalidated in ATP modes like Yard and Possession, and the
position history is cleared in ATP mode Configuration.

\subsection Scheduling Scheduling
The TIMS processing is performed by runIn(), run() and runOut() which must be called by the framework in each cycle.

\subsubsection runIn runIn()
AbstractTIMS::runIn() is empty but is needed as a placeholder for adaptation.

\subsubsection run run()
AbstractTIMS::run() performs the following activity:
- Handle driver input by calling \ref handleDmiActions.
- Update the position history by calling \ref updatePositionHistory (or \ref clearPositionHistory if ATP mode is Configuration).
- Update TIMS Availability by calling the virtual function \ref updateTimsAvailable.
- Update TIMS Supervision state (\a Not \a Supervised, \a Supervised or \a Inhibited) by calling the virtual function \ref updateTimsSupervision.
- Update TIMS Confirmed and TIMS Status by calling the virtual function \ref updateTimsStatus.
- Update the last car position by calling \ref updateOrClearLastCarPosition.
- Log an event if the train integrity changes to \a Broken or \a Intact.

\subsubsection runOut runOut()
AbstractTIMS::runOut() is empty but is needed as a placeholder for adaptation.

\subsection TIMSStates TIMS States
The TIMS states are:
- TIMS Availability, which indicates whether TIMS is available or not for this train.
- TIMS Confirmed, which indicates whether train integrity has been confirmed by an automated integrity report.
- TIMS Status, which indicates whether train integrity is \a Intact, \a Broken or \a Not \a Available.
- TIMS Supervision, which indicates whether TIMS is \a Supervised, \a Not \a Supervised or \a Inhibited.

The TIMS states are retrieved using these functions:
- \ref AbstractTIMS::getTimsAvailable()
- \ref AbstractTIMS::getTimsConfirmed()
- \ref AbstractTIMS::getTimsStatus()
- \ref AbstractTIMS::getTimsSupervision()

\subsubsection updateTimsAvailable updateTimsAvailable()
AbstractTIMS::updateTimsAvailable() updates the Train Integrity availability, i.e. \a True if TIMS is available.

Note: This function always returns false so it must be overridden in the adaptation class.

\subsubsection updateTimsStatus updateTimsStatus()
AbstractTIMS::updateTimsStatus() updates TIMS Status according to core requirements.

Note: The central functionality here must be implemented in the adaptation class. This entails setting
TIMS Status to \a Broken if any one of the integrity tests fail, or TIMS Confirmed to \a True
if all integrity tests pass.

\subsubsection updateTimsSupervision updateTimsSupervision()
AbstractTIMS::updateTimsSupervision() updates the train integrity supervision by setting it to one of the following:
- \a Not \a Supervised - if TIMS is not available or if TIMS is not requested by TCC, or if the current ATP mode doesn't allow TIMS; otherwise:
- \a Inhibited - if TIMS is inhibited by driver; otherwise:
- \a Supervised.

\subsection UpdatingLastCarPosition Updating Last Car Position

\subsubsection getAutomatedReportTime getAutomatedReportTime()
AbstractTIMS::getAutomatedReportTime() provides input to \ref updateLastCarPosition and returns the time when the most
recent automated integrity report was received.

This function is a placeholder for adaptation.

\subsubsection setPositionReport setPositionReport()
AbstractTIMS::setPositionReport() provides input to \ref updateLastCarPosition, namely train front and rear positions
as well as position accuracy.

This function must be called whenever a PositionReport message is sent to TCC.

\subsubsection updateOrClearLastCarPosition updateOrClearLastCarPosition()
AbstractTIMS::updateOrClearLastCarPosition() either clears or updates the last car position, depending on ATP mode.
The update is performed by calling \ref updateLastCarPosition.

\subsubsection updateLastCarPosition updateLastCarPosition()
AbstractTIMS::updateLastCarPosition() calculates the last car position, i.e. the position of the unconnected end
of the last car. This position is calculated from the position of the locomotive and the length of the train.
However, the calculation uses these values from different points in time, depending on TIMS Supervision and on
the direction of movement.

The calculation works as follows:
- If TIMS Supervision is \a Not \a Supervised or if the train is reversing, the current locomotive position and
  train length are used.
- If TIMS Supervision is \a Supervised and we have received an automated integrity confirmation, the last car
  position is fetched from the position history by calling \ref updateLastCarPositionSupervised.
- If TIMS Supervision is \a Inhibited and we have received a manual integrity confirmation from the driver,
  the last car position is fetched from the position history by calling \ref updateLastCarPositionManual.

If the last car position couldn't be calculated, it's given an initial value equal to the start position of the
most recently received MA from scratch (if the MA direction is forwards).

This function also updates the last car balise window, i.e. the balise window for the last car end of the train.
This balise window is fetched from AbstractOdometry if the current locomotive position was used (see above) or
from the position history if the last car position was taken from the history.

\subsubsection updateLastCarPositionManual updateLastCarPositionManual()
AbstractTIMS::updateLastCarPositionManual() calculates the last car position at the estimated time when
train integrity was manually confirmed.

The calculation uses a history of time-stamped position samples and uses the older of the two samples
that are closest in time to the estimated confirmation time.

\subsubsection updateLastCarPositionSupervised updateLastCarPositionSupervised()
AbstractTIMS::updateLastCarPositionSupervised() calculates the last car position at the estimated time when
train integrity was confirmed by an automated report.

The calculation uses a history of time-stamped position samples and uses the older of the two samples
that are closest in time to the estimated confirmation time.

\subsubsection updatePositionHistory updatePositionHistory()
AbstractTIMS::updatePositionHistory() updates the position history which is needed when updating the
last car position and last car balise window, see \ref updateLastCarPosition.

This function stores one sample in the position history every second. These samples contain:
- the position of the last car end of the train
- the last car balise window
- time stamp
- validity flag

A sample is only valid if we have received a TrainSetup, the position accuracy is \a Known and if we have no
indication of neither slip nor slide.

\subsubsection clearPositionHistory clearPositionHistory()
AbstractTIMS::clearPositionHistory() erases the position history. This is needed for initialization and
re-initialization (when ATP mode returns to Configuration).

\subsection HandlingRearPosition Handling Positions And Balise Windows

\subsubsection getFrontBaliseWindow getFrontBaliseWindow()
AbstractTIMS::getFrontBaliseWindow() returns the uncertainty of the train front position.

This value is either the locomotive end balise window or the last car balise window, whichever
end is closest to the latest MA target.

\subsubsection getRearBaliseWindow getRearBaliseWindow()
AbstractTIMS::getRearBaliseWindow() returns the uncertainty of the train rear position.

This value is either the locomotive end balise window or the last car balise window, whichever
end is furthest away from the latest MA target.

\subsubsection getRearPosition getRearPosition()
AbstractTIMS::getRearPosition() returns the rear end position of the train, as track number and position in track.

This function must be called whenever the rear end position needs to be sent in a PositionReport message
(to be transmitted to TCC).

The rear end position is either the lead locomotive position or the last car position, whichever is furthest
away from the latest MA target.

When going forwards (i.e. when the lead locomotive is pulling the cars), the last car position is the most
recently confirmed position of the unconnected end of the last car.

When going backwards (i.e. when the lead locomotive is pushing the cars), the last car position is calculated
from the locomotive position and the length of the train.

\subsubsection getRearPositionOdo getRearPositionOdo()
AbstractTIMS::getRearPositionOdo() returns the rear end position of the train, as an odometer position.
This position is only valid if \ref isRearPositionValid returns \a True.

See \ref getRearPosition for the definition of rear end position.

\subsubsection getSafePositionToDeleteTrack getSafePositionToDeleteTrack()
AbstractTIMS::getSafePositionToDeleteTrack() returns the rear end position extended by a margin for safely
deleting tracks and balises behind the train.

This value is used when deleting tracks (and the balises on those tracks) that are so far behind the train
that they can safely be deleted.

\subsubsection isRearPositionValid isRearPositionValid()
AbstractTIMS::isRearPositionValid() returns \a True if the rear end position is valid.

This function indicates whether \ref getRearPositionOdo would return a valid value.

If the travel direction of stored targets is 'reverse' then the rear position is valid. In any other travel direction,
the rear position is valid if and only if the last car position is valid.

\subsection HandlingDriverActions Handling Driver Actions

\subsubsection handleDmiActions handleDmiActions()
AbstractTIMS::handleDmiActions() handles the DMI input required for manual integrity reports from the driver.

\subsubsection isManualConfirmationNeeded isManualConfirmationNeeded()
The return value from AbstractTIMS::isManualConfirmationNeeded() indicates whether the driver needs to confirm a
manual train integrity confirmation.

\latexonly \newpage \endlatexonly
\section ClassDiagram Class Diagram

@image html abstract_tims_class_diagram.png "AbstractTIMS class diagram"
@image latex abstract_tims_class_diagram.png "AbstractTIMS class diagram"

\section Diagnostics Diagnostics

\subsection Console Console Commands
tims - prints the values of TIMS Availability, TIMS Supervision, TIMS Confirmed and TIMS Status

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
The AbstractTIMS class implements the core requirements.

To provide full functionality, these functions must be overridden by an adaptation class:

- \ref updateTimsAvailable
- \ref updateTimsStatus
- \ref getAutomatedReportTime

\section PreProcessor Pre-Processor Directives
No pre-processor directives available for this component.

\section Traceability Traceability

\subsection SSRS Functional requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATP Core.

The requirements relevant for this component are:

Req        | Chapter                              | Function
---------- | ------------------------------------ | --------
AOS 91 S   | \ref TIMSStates                      | AbstractTIMS::updateTimsSupervision()
AOS 212    | \ref HandlingRearPosition            | AbstractTIMS::getFrontBaliseWindow() <br /> AbstractTIMS::getRearBaliseWindow()
AOS 283    | \ref HandlingRearPosition            | AbstractTIMS::getRearPosition() <br /> AbstractTIMS::getSafePositionToDeleteTrack()
AOS 503    | \ref TIMSStates                      | AbstractTIMS::updateTimsStatus()
AOS 1039   | \ref Console                         | AbstractTIMS::consoleCall()
AOS 2181 S | \ref UpdatingLastCarPosition         | AbstractTIMS::updateLastCarPosition()
AOS 2703 S | \ref UpdatingLastCarPosition         | AbstractTIMS::updateLastCarPositionManual()
AOS 2704 S | \ref UpdatingLastCarPosition         | AbstractTIMS::updateLastCarPositionSupervised()
AOS 2707   | \ref TIMSStates                      | AbstractTIMS::updateTimsStatus()
AOS 2713   | \ref Scheduling                      | AbstractTIMS::run()
AOS 2769   | \ref HandlingDriverActions           | AbstractTIMS::handleDmiActions()
AOS 2770   | \ref UpdatingLastCarPosition         | AbstractTIMS::setPositionReport() <br /> AbstractTIMS::updateLastCarPosition()
AOS 3219 S | \ref HandlingDriverActions           | AbstractTIMS::handleDmiActions()
AOS 3262   | \ref UpdatingLastCarPosition         | AbstractTIMS::updateLastCarPosition()
AOS 3295   | \ref UpdatingLastCarPosition         | AbstractTIMS::updateLastCarPosition()

\subsection SSAS Architectural requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATP Core. Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req             | Chapter         | Function
--------------- | --------------- | --------
AOS_AS-47 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-50 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-70 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-71 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-72 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-88 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-89 S     |\ref Scheduling  | AbstractTIMS::run()
AOS_AS-90 S     |\ref Scheduling  | AbstractTIMS::run()

*/

}
