/**
\if AsMainPage
\mainpage Target and Track data(section (6.325.3 & 6.3.25.2 ))
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-04-20 | Proposed Changes wrt targets and tracks data requirement | spandita


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TODO           | To be discussed

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 6.3.25.2(tracks) & 6.325.3(targets)

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 283 | The AOS shall delete a track and all balises on that track if Keep track data is not set & the train is not in Location Mode & the train safe rear end has passed the tracks. | Partially Implemented
AOS 287 |  The AOS shall delete a stored balise only when the track on which it is placed is also deleted. | Implemented
AOS 2140 | The AOS shall delete all tracks and balises when the ATP mode changes to Yard, Shunting, Possession, Safety Halt OR Unregistration. | Partially Implemented
AOS 2142 | The AOS shall store the end of MA position as a Primary Target of type Q_ROUTE_TYPE. The AOS shall delete any other stored Primary target of the same Q_ROUTE_TYPE. | Partially Implemented
AOS 2280 | In ATP mode Staff Responsible, the AOS shall delete a Primary Target of type Staff Responsible if an MA of Q_ROUTE_TYPE Normal is accepted. | Not Implemented
AOS 281  | The AOS shall delete target other than a Primary Target if Keep track data is NOT set & The train safe rear end has passed over the target location for gradient targets and train length delayed ceiling speed targets or The train front has passed over the target location for all other target types.| Partially Implemented
AOS 2143 | The AOS shall delete a Primary Target and all the other targets if Keep track data is NOT set & The train is NOT in Location Mode & The train is at standstill with the train front within the MA margin of the Primary Target | Not Implemented
AOS 2144 | The AOS shall delete all targets when ATP mode changes to Yard OR Possession OR Shunting OR Safety Halt OR Unregistered OR Sleeping or leaving Location mode | Not implemented
AOS 2197 | The AOS shall delete all ceiling speed targets and gradient targets when the train is at standstill in Location mode.  |  Not implemented

 \subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS283 AOS283
Affected component  : mode control \n
Already implemented in handle mode function of normal mode. \n

\subsection  AOS2140 AOS2140
Affected components  : Mode Control  \n
Call removeAll() function of abstract_tracks in the handleMode() function of yard, shunting, possession, safety Halt and unregistration modes. \n

\subsection  AOS2142 AOS2142
Affected component  : abstract targets \n
In insertTargetInList() function of abstract target file. \n
- Iterate the target list. 
- Check for primary target if already present in target list.
- If yes, delete the already saved primary target and insert the new target at same place.


\subsection  AOS2280 AOS2280
Affected  component  : Mode Control \n
- Create a new file as staff responsible in mode control. 
- In handle function of staff responsible file check if MA of Q_ROUTE_TYPE = Normal is accepted by using getMAHead() function of message handler.
- If yes, iterate the target list and get the primary target object pointer.
- Check if the type of primary target is of staff responsible type.
- If yes, delete the target by using delTarget() of abstract target.


\subsection  AOS281 AOS281
Affected component  : targets  \n
- Get the ODO value of gradient and ceiling target in removePassed() function of abstract target.
- Get the rear ODO value by calling getCurrRearPosOdo() of position component.
- Check if it is greater than celing or gradient target ODO value.
- If yes, delete all the target except primary target.

\subsection  AOS2143 AOS2143
Affected component  : mode control  \n
In handle function of normal mode \n
- Check wheather the train is standstill.
- If Yes, Iterate the target list and get the primary target object pointer and its ODO value.
- Calculate the MA margin ODO in following way:
- If direction is positive : 
    calOdo = ODO of primary target - maMargin. \n
- If travel direction is negative : 
    calOdo = ODO of primary target + maMargin. \n
- Get the train front position from abstract position component.
- Incase of positive travel direction, Check if train front position is greater than or equal to calculate ODO value. 
- Or in case of negative direction, Check if train front position is less than or equal to calculated ODO value.
- If yes, Call removeAll() function of abstract targets. 

\subsection  AOS2144 AOS2144
Affected component  : mode control  \n
Call removeAll() function of abstract target in the handleMode() function of yard, shunting, possession, sleeping, location, safety Halt and unregistration modes. \n

\subsection  AOS2197 AOS2197
Affected component  : mode control  \n
Create a new file in mode control as location mode. \n
Create a new public interface(removeCeilingAndGradientTargets()) in abstract target file which will be used to delete only ceiling and gradient targets.\n
In handleMode() function of location mode check for standstill condition.\n
If yes, call removeCeilingAndGradientTargets() function of abstract target file.

\section AdditionalMaterial Additional Material

*/