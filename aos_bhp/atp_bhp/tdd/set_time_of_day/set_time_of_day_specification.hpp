/**
\if AsMainPage
\mainpage Set Time of Day process
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 19-05-2017 | Creation of process capable of setting the time on system and RTC.  | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP Onboard System
TCC            | Train Control Centre
ATP            | Automatic Train Protection
VFW            | Vital Framework
SIL            | Safety Integrity Level
RTC            | Real Time Clock
NTP            | Network Time Protocol

\section Introduction Introduction
The SetTimeOfDay application will be used by the ATP to set the time in the operating system and HW clock(RTC) using low level system calls.
This has to be done from a separate program/process as the ATP Application when executed on target is executed as the 'atpcu' user, whereas setting the time can only be done by the 'root' user.
This is a SIL-0 application.
The SetTimeOfDay program does not support running on PC and is not used when performing testing on PC.

The setTimeOfDay process will be called from CPU-A, set the time on CPU-A and vfw will take care of time sync between CUP A and B when vfwGetTimeOfDay() is called.

For time synchronization between CPU-A and CPU-C NTP server Daemon is to be setup in the CPU-A while NTP client will be on the CPU-C. CPU-A will use it's
local clock as reference which should have been already synced via. SetTimeOfDay application. This time would be used by the CPU-C to set it's system clock(done by NTP client).

\subsection Design Design Overview
\subsubsection SetTimeOfDay Set Time of day application
SetTimeOfDay is only run when called on the command line, and if the purpose is to set the time, it should receive the time to set in Epoch format, i.e. as the number of 
seconds elapsed since January 1st 1970 at 00:00:00 GMT, and do the needed operations to set the time. This is invoked from ATP via. system() call in the application code.

SetTimeOfDay can also be invoked to return its current version. This feature is used by the ATP startup scripts, to verify the version of the installed SetTimeOfDay binary during startup.

@image html set_time_of_day.png
@image latex set_time_of_day.png

\subsubsection NTPTimeSync NTP Time synchronization
The time synchronization between CPU-A and CPU-C is carried out by the use of NTP. Steps for using NTP for time sync:-
- Install NTP Daemon in CPU-A and CPU-C as Server and Client respectively.
- Configure the ntp.conf file and rc.conf file to enable NTP Daemon.
- Set the local clock as reference clock in the CPU-A.
- Add the starting of NTP Daemon in the ATP startup script to start time sync.

\subsubsection ATPUpdate ATP Updates
In the ATP side, following updates need to be done:-
- Creation of a wrapper function(updateTime()) in the Util that should be called when ATP needs to update the system calls.
- Calling of the wrapper function when DriverLogonStatus and CommandMessage is received from the TCC after extracting SET_TIME field.
- Calling of the vfwGetTimeOfDay() while sending of the Time message to the DMI for synchronizing ATP with the DMI. This should be done every time there is an update in ATP time from TCC.
- No time sync with this received time is done for OPC time synchronization as there is a separate MVB and VFW time exchange between OPC and ATP.
- Calling of the vfwGetTimeOfDay() while sending of the AOS status Message(system Time field) to the LCS for synchronizing ATP with the LCS via LIG.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
List of affected/triggering requirements.

Req        | Short requirement description                                                  | Justification
---------- | -------------------------------------------------------------------------------| -----------------------
AOS 473    | After powerup, AOS shall use build in date and time until synced with the TCC    | partially implemented
AOS 690    | AOS shall sync date and time when DriverLogonStatus OR CommandMessage is received | not implemented

\section SystemArchitecturalDesign System Architectural Design
@image html time_synchronisation.png
@image latex time_synchronisation.png

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description
setTimeOfDay application is itself an External Interface for ATP which can be called by the Command Line or by the ATP to set the system Time and RTC clock.

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection Component-MessageHandler Component-Message Handler
The message Handler will parse the value from SET_TIME field in the DriverLogonStatus and CommandMessage messages and update the system Time and RTC with this value.

\subsection Component-Misc Component-Misc
Utility function used as a Wrapper function to make the call of setTimeOfDay application/process.

\subsection Application-setTimeOfDay Application-SetTimeOfDay
This is the application which will set the system time and RTC of the GSP-2 hardware. It will internally make system level calls to set the time.
Running of the application via system()call should make sure that the application is run with 'root' access.

\subsection Component-others Component-others
Any application that needs to use the system time shall make the vfwGetTimeOfDay() call to get the updated system Time after the time 
synchronization with the TCC. Vehicle Comm and DMI Handler should use this call to acquire the time to update the message packet.

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

\subsubsection ScreenImages Screen Images

\subsubsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution

\section AdditionalMaterial Additional Material
- To comply with the GSP2_SIR there may be a need to implement this setTimeOfDay on CPU-B. This may lead to change in design and update in components.

*/