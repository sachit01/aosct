/**
\if AsMainPage
\mainpage Power Management
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-11-02 | First version                                 | keisele


\section Abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
DMI            | Driver Machine Interface

\section Introduction Introduction

\subsection Design Design Overview

Powermanagement includes the 2 states: Powering Off and Power Off. These states
model the controlled shutdown of the system. While Powering Off is an execution
stage, the Power Off stage is a hardware state in which the current is removed 
from the system. The usecase for Powermanagement is a train operator
that presses the AOS-Off button to request shutdown of the AOS system. The 
shutdown is implemented in hardware via a dedicated Digital output that 
will trigger the removal of current from the target system.

This TDD describes the steps to implement the Powering-down and Power Management requirements. 

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix

Req     | Short requirement description         | Justification
--------| ------------------------------------- | ------------------------------------------------------------------
AOS 6   | The connection between the AOS and the trains brake interface shall be constructed in such a way that no brake application shall be requested while the AOS is not powered. | Hw implemented
AOS_BHPB 2687S | If the AOS equipment is not powered, EB shall be ordered.| Hw implemented
AOS 137 | When ATP mode changes to Powering Down the AOS shall issue a Log event.  | Not implemented
AOS 138 | In ATP mode Powering Down the AOS shall power off after sending two PositionReport messages to the TCC. Two PositionReport messages is a default value that can be overriden by adaptation.  | Not implemented
AOS 139 |  In ATP mode Powering Down the AOS shall power off regardless of the number of sent PositionReport messages after a default value of 30 seconds unless the value is overuled by the adaptation. | Not implemented
AOS 2229 |  The ATP mode Powering Down shall only be possible to exit by powering off the AOS system. | Not implemented
AOS 2569 |  In ATP mode Powering Down the AOS shall present the Powering Down screen to the Driver.  | Already implemented
AOS 431 | In ATP mode Powering Down the AOS Status output shall be toggling with default values of frequency and duty cycle as 0.25Hz and 50% respectively, unless the values are overridden by the adaptation. | Not implemented
AOS 428  | The AOS shall include a self-holding design requiring the ATP to actively turn the power off. | Self-holding
AOS 442  | The AOS shall complete the power down regardless if the ATP On input is active while the powering down is in progress. | Self-holding
AOS 1134 | The AOS shall change the ATP mode to Powering Down if: ATP Off input is active for more than 5 seconds, AND Vehicle is at standstill, AND ATO mode is Manual, AND The current ATP mode is one of: Normal, Staff Responsible, Shunting Route, Split, Join AND Primary Target doesn't exist, OR The current ATP mode is one of: Power Up, Configuration, Registration, Balise Search, Possession, Shunting, Yard, Sleeping, Safe Brake to Stop, Safety Halt OR Unregistered. | Not implemented
AOS 1132 | The AOS shall delete all targets AND change the ATP mode to Powering Down if: The current ATP mode is Normal, Location, Staff Responsible, Shunting Route, Split OR Join, AND a Primary Target exists, AND ATP Off input is active for more than 5 seconds, AND Vehicle is at standstill, AND ATO mode is Manual, AND Communication with the TCC is lost. | Not implemented

\section SystemArchitecturalDesign System Architectural Design

Main implementation tasks are:

 - Detect when to transition to Powering Down mode:
   + This includes the AOS OFF signal monitoring
   + Transition into Powering Down mode is dependent on conditions of trainStandstill, currentATOMode, currentMode and primaryTargetPresent.
 - When in Powering Down mode:
   + send log message
   + wait for at 2 position messages to be sent or 30 seconds pass
   + toggle the AOS Status Led output pin with a certain frequency
   + Present DMI powerdown screen
 - When in last submode of Powering Down mode:
   + Raise the Power Off signal

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection General

 - Powering down mode can be entered from all modes so a Powering-down-mode-enter-check
   is done each Scheduler tick.
 - The Powering down mode itself has 3 submodes that are executed in sequence:
   + powerDownStateSendLog: This submode sends the log message
   + powerDownStateWaitPositionSent: This submode waits for 2 position messages to be sent or timeout
   + powerDownStateTurnOffLight: This submode sets the Power-Off.
 - When in powerDownStateSendLog and powerDownStateWaitPositionSent the AOS Status Led is toggled
   at a given frequency

\subsection ModeControl Mode Control

  - Add a predicate function powerDownRequested() to PowerDownMode class.
    + poweringDownRequested() should detect a rising edge on the "AOS Off" signal and start a
      timer on detection
  - Call the poweringDownRequested() predicate from AbstractModeControl()::run() on each Scheduler cycle, when
    it becomes true transition into PoweringDown mode. Never transition to another mode when current mode is PoweringDown mode.
  - Clear targets when entering PoweringDown mode by calling DS::AbstractTargets::removeAll()
  - Implement in PowerDownMode::handleMode() the 3 submodes powerDownStateSendLog, powerDownStateWaitPositionSent, powerDownStateTurnOffLight
    in the PowerDownMode class. This includes, sending log message, wait for 2 position requests and timeout and
    finally set the POWER OFF signal.
  - Add 2 virtual function that can be overridden by adaptation:
    + AbstractModeControl::getPoweringDownPosMessageTimeout(): return default position-message-sent timeout.
    + AbstractModeControl::getPoweringDownBlinkDuration(): return default blink duty cycle.
    + AbstractModeControl::getNumPositionMessages(): return number of positional messages to wait for.
  - To enable sending a logging message by the Powerdown submode add AbstractModeControl::writePowerDownLogMessage() that can
    be called by Powerdown submode that will send the Powering down log message.

\subsection LocoIO
  - The value of the output signal Power-Off should be fetched from the PowerDownMode class by implementing a
    function powerOffSignalValue() in AbstractModeControl that will query the PowerDownMode class. The value is true if the PowerDownMode is in submode powerDownStateTurnOffLight.

\subsection MessageHandler
  - A counter to AbstractRadioHandler that indicates the number of sent position messages. This is needed to determine when 2 position messages have been sent

\subsection DMIHandler DMI Handler
  - In the requirements it is stated that the DMI is already implemented


*/
