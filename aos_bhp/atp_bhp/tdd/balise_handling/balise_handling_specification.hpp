/**
\if AsMainPage
\mainpage BaliseHandling
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 27-02-2017 | Created                                       | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
ATP            | Automatic Train Protection

\section Introduction Introduction

\subsection Design Design Overview

This document deals with design of how is the handling of informations coming from balise is handled in accordance with the
type of balise, ATP modes and information from the balise.

The handling of information involves calculation of position from the balise information and taking appropriate actions.
i.e.- of Safe brake and logging depending upon the balise window, current position, slip and slide.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
List of affected/triggering requirements.

Req        | Short requirement description                           | Justification
---------- | --------------------------------------------------------| -----------------------
AOS 209    | Balise reading in ATP modes                             | patially implemented
AOS 1199   | Handling Red balise                                     | patially implemented
AOS 210    | Extracting Balise ID                                    | implemented
AOS 219    | Safety Halt at standstill                               | implemented
AOS 1200   | Balise calculation and Balise window                    | implemented
AOS 212    | Calculation of balise window percent                    | not implemented
AOS 213    | balise window limits                                    | implemented
AOS 214    | Safe brake with balise window, slip, slide and position | partially implemented
AOS 215    | Log event with missed balise and position               | partially implemented
AOS 216    | Safe brake with missed balise and position              | partially implemented
AOS 217    | Handle unknown balise                                   | partially implemented
AOS 218    | Safe brake with Safety margin                           | not implemented

\section SystemArchitecturalDesign System Architectural Design
@image html atp_balise_handling.png
@image latex atp_balise_handling.png

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection Component-ModeControl Component-ModeControl
The inputs from mode control will be needed to know the current ATP modes like Power up, power down and sleeping.\n
TODO:
- AOS 209 S
+ Use getModeId() to get the mode in order to know if balise should be read or not.

\subsubsection Component-Decode Component-Decode
The Decode component will be used to get balise details out of balise messages and calculate the position and balise window.\n
TODO:
- AOS 1199
+ In run(), use the NID_C = 1023 and NID_BG = 16383 to find the red balise.
+ In run(), issue an emergency brake event on detecting a red balise.\n
- AOS 210
+ In parseTelegramPacket(), extract the telegram packet and extract NID_BG to get the balise ID and save it.\n
- AOS 219 S
+ In run(), after reading the current position acquired from Odometry, compare it with the last position in history. If both the values are
same, trigger a safety halt event.\n
- AOS1200
+ In run(), when a balise is detected calcualte the current position via Odometry and update the Balise information vector.

\subsubsection Component-Position Component-Position
The relevant comparison related to balise window and current position is done in Position component. It also takes the appropriate
actions like Log event and safe brake to stop event via Event handler.\n
TODO:
- AOS 209 S
+ In run(), check for the current mode from ModeControl() and call readBalises().
+ In readBalises(), get the BaliseInfo from Decode and read the data out of it.\n
- AOS 210
+ In readBalise(), get the NID_BG data extracted from Telegram Header is used as balise ID.\n
- AOS 213
+ In readbalise(), make sure the balise window is less than the minimum balise window from odometry.\n
- AOS 214 S
+ In readBalises(), get the balise window value, current position accuracy state and slip and slide status.
+ If balise is encountered outside the balise window, current position accuracy state is known and slip and slide are 
not active: issue a safe brake to stop event.\n
- AOS 215
+ In checkMissedBaliseOnMovement(), check the state of current position and check if balise is missed.
+ In checkMissedBaliseOnMovement(), check if current position is known and an expected balise is missed: issue a Log event.\n
- AOS216 S
+ In checkMissedBaliseOnMovement(), check the state of current position and check if another balise is missed.
+ In checkMissedBaliseOnMovement(), check if current position is known and another expected balise is missed: issue a Safe brake to stop event.\n
- AOS217 S
+ In readbalise(), check the state of current position and get the next balise id to be detected.
+ In readBalise(), check if current position is known or approximate and an unexpected balise is received: issue a Safe brake to stop event.

\subsubsection Component-Odometry Component-Odometry
Safety margin of balise window, minimum balise window and balise window calculation as percentage of distance travelled since last balise comparison
is done in the Odometry component.\n
TODO:
- AOS 212
+ In updateBaliseWindow(), get the current position status and the current odo position, get the config parameter values of min balise window,
balise window percent and max balise window.
+ In updateBaliseWindow(), find the distance traveled since the last read balise and calculate the balise window as\n
balise window = MAX(minimum balise window, ((balise window percent/100)*distance travelled since last read balise))
- AOS 218 S
+ In updateBaliseWindow(), get the Safety margin from the train setup.
+ In updateBaliseWindow(), if balise window calculated is greater than Safety margin value issue a safe brake to stop event.

\subsubsection Component-EventHandler Component-EventHandler
All the event handling i.e.- Log event and Safe brake to stop events are handled via Event Handler component.

\section UserInterfaceDesign User Interface Design
NA

\subsection DescriptionOfTheUserInterface Description of the User Interface
NA

\subsubsection ScreenImages Screen Images
NA

\subsubsection ObjectsAndActions Objects and Actions

\section AdditionalMaterial Additional Material

*/