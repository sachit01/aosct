/**
\if AsMainPage
\mainpage Brake Test
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-10-02 | First version                                 | nsyed


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction

\subsection Design Design Overview

Investigate the design for Brake Test.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req              | Short requirement description         | Justification/Status
---------------- | ------------------------------------- | ------------------------------------------------------------------
AOS 2611         | The AOS shall offer the driver the possibility to perform a brake test when: \n The current ATP mode is other than Safety Halt OR Powering Down OR Sleeping, AND \n  The current ATO mode is Manual AND \n The driver is logged in AND \n The AOS is in state <b>Vehicle ready</b> AND \n The vehicle is at standstill. \n| Not Implemented
AOS 2612 S       | The AOS shall issue a standstill event during the brake test. | Not implemented
AOS 2615 S       | When a configurable max brake test interval in calendar time is reached AOS shall: \n Trigger a brake event to standstill AND \n	Inform the driver that a brake test is required AND \n At standstill issue a standstill event. | Not Implemented
AOS 2626         | The AOS shall at a configurable calendar time (brake test notification time) before the max brake test interval is reached: \n Inform the driver the time until the max brake test interval is reached. \n The indication shall remain until the max brake test interval is reached OR the Driver identification is changed. \n If the Driver identification is changed the indication shall appear again. | Not Implemented
AOS_BHPB 2756 S  | The AOS shall issue a Standstill event AND indicate as an event to the driver that a brake test must be performed when: \n EB cut-out inputs are configured for use AND \n Both EB cut-out inputs (EBC1 and EBC2) have been restored to Not Cut-out position after that any of the EB cut-out inputs indicated Cut-out. \n The AOS shall issue a standstill event as long as a successful brake test is not performed. | Not Implemented
AOS 2629 S       | The AOS shall inform the driver that a brake test is required in ATP mode Power up following a Safety Halt event when a fault in the following interfaces has occurred: \n EB orders OR \n EB feedbacks from vehicle system \n The AOS shall issue a standstill event as long as a successful brake test is not performed. | Not Implemented
AOS_BHPB 2768 S  | The AOS shall inform the driver that a brake test is required in ATP mode Power up following a Safety Halt event when a fault in the following interfaces has occurred: \n EB cut-out inputs \n The AOS shall issue a standstill event as long as a successful brake test is not performed. | Not Implemented
AOS 2630 S       | When the driver starts the brake test the AOS shall first await the conditions according to step 0 and \n after that each EB order shall be tested independently according to the table below. \n After each EB order/release AOS shall receive the expected EB feedback from the vehicle brake system before continuing to the next step in the brake test sequence. | Not Implemented
AOS 2633         | The AOS system shall define the EB feedback from the vehicle brake system as NOT available unless overridden by the adaptation. | Not Implemented
AOS 2634 S       | If the EB feedback from the vehicle brake system is not available (not used) the EB orders interaction with the vehicle brake system \n must be tested with a test interval ensured by other means than the brake test, for example a maintenance test. \n The justification of not using the EB feedback must be based on safety analysis of the AOS-Vehicle brake interface. | Not Implemented
AOS 2653 S       | If the EB feedback from the vehicle brake system is not available (not used) the AOS shall perform all steps in the brake test sequence \n but only verify the EB orders using the internal health supervision. | Not Implemented
AOS 2635 S       | The AOS shall consider the brake test passed successfully when all the following conditions are fulfilled: \n EB feedback from the vehicle (on order): \n The expected feedback indicating brake applied shall have been received within the configurable EB feedback time. \n Thereafter no state change is allowed until the order is withdrawn. \n EB feedback from the vehicle (on release): \n  AOS shall await the expected feedback indicating brake released. Thereafter no state change is allowed until the next order is issued. \n No internal or external failures related to the EB order and EB feedback health supervision are detected. \n The brake test has not been aborted. | Not Implemented
AOS 2636 S       | If the brake test fails the AOS shall: \n Interrupt the brake test AND \n Continue to issue a standstill event AND \n Inform the driver that a new brake test is required. \n The AOS shall supervise standstill until a successful brake test is performed, even if the max time interval has not expired. | Not Implemented
AOS 2638         | The driver shall have the possibility to abort the brake test. | Not Implemented
AOS 2640 S       | The AOS shall abort the brake test if: \n The driver has aborted the brake test. \n Brake is ordered by the event handling as a consequence of other reasons than the brake test sequence or a failure reaction related to it. \n The cabin is deactivated. \n Emergency Stop Active signal is detected. | Not Implemented
AOS 2654         | When the brake test is aborted the AOS shall interrupt the brake test and release the EB orders triggered by the brake test sequence. | Not Implemented
AOS 2642         | The AOS shall inform the driver about the status of the brake test: \n If the test cannot be started (condition in step 0 not fulfilled): <b>Unable to start Brake Test</b> \n if the test is in progress: <b>Brake Test in progress</b>. \n if the test is aborted: <b>Brake Test aborted</b>. if the test has failed: <b>Brake Test failed, perform new test!</b>. If the test is successful: <b>Brake Test successful</b>. | Not Implemented
AOS 2641         | If Unable to start Brake Test OR Brake Test Aborted OR Brake Test Failed the driver shall be informed about the reason for the status. | Not Implemented
AOS 2643         | If the brake test has not been successful, aborted or failed within a configurable time from initialization, the brake test has failed. | Not Implemented


\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection General
This TDD describes the implementation proposal for the Brake Test as per the requirements stated in \ref RequirementsTraceabilityMatrix. \n
The main components involved are:\n
<b>Mode Control</b>: Assess the need/possibility of a Brake Test and inform the same to the Driver via DMI Handler and also to the Brake component. \n
<b>Brake</b>: Performs the actual Brake test sequence by issuing the Brake Release/Apply orders via the LocoIO component. In addition to reading the internal feedback for Emergency brakes
from LocoIO Core, it also reads the feed back from the Brake Pressure valves (if available in adaptation) via the LocoIO adaptation.
Brake also updates the Mode Control and DMI Handler with the progress and result of the Brake Test performed. \n
<b>LocoIO</b>: Fetches the EB1 and EB2 orders from the Brake component and also provide feedback from the Brake pressure valves if available in the adaptation. \n

The interaction among these components is as described in the sequence diagram below:
@image html brake_test_sequence_diagram.png
@image latex brake_test_sequence_diagram.png

\subsection ModeControl Mode Control
A new function <code>manageBrakeTest()</code> needs to be defined in the Abstract Mode Control which does the following:
- Compare the current time with the last performed brake test time stored in config and set the flag <code>brakeTestMandatory</code> to <code>True</code> if the current time is greater.
- Checks the conditions in \ref AOS2611 and sets the flag <code>brakeTestPossible</code> if the conditions are fulfilled.
- Reads from the DMI Handler if the driver has pressed the BrakeTestStart/BrakeTestAbort button.

\subsection Brake Brake
A new function <code>performBrakeTest()</code> needs to be defined in Abstract Brake which performs the Brake Test Sequence and updates the Brake Test progress to DMI Handler and the Mode Control component.

\subsection AOS2611 AOS 2611
Covered in the <code>manageBrakeTest()</code> initiated in the Mode Control Component.

\subsection AOS2612S AOS 2612 S
In the <code>performBrakeTest()</code> initiated the Brake component, a standstill event will be issued during the execution of the brake test.

\subsection AOS2615S AOS 2615 S
Covered in the <code>manageBrakeTest()</code> initiated in the Mode Control Component.

\subsection AOS2630S AOS 2630 S
Covered in the <code>performBrakeTest()</code> initiated the Brake component. \n
For the EB orders, two new flags eb1TestApplied and eb2TestApplied are defined, which are set to true when EB1 and EB2 respectively need to be applied during the Brake test.
The LocoIO shall fetch this information via two new access functions: <code>getEb1TestApplied()</code> and <code>getEb2TestApplied()</code>.
After each EB apply/release orders mentioned in the table below, the following will be verified:
- Verify that the expected values are set on Core Digital Outputs: <code>EmerBrake1</code> and <code>EmerBrake2</code>.
- Verify that the expected values are read from the Brake Pressure valves, if available in adaptation (LocoIO, Brake).

Two new virtual functions <code>readEb1Feedback</code> and <code>readEb2Feedback</code> needs to be defined in Brake (Core) which return <code>True</code> in the absence of an implementation in adaptation (Brake Feedback system unavailable).\n
In the adaptation, these functions shall verify that the expected feedback is received, within the configured timeout. (New config parameters <code>ebApplyFbTimeout</code>, <code>ebReleaseFbTimeout</code> should be defined).\n
The timers can be defined either using the vfwTimers or by counting up the ATP cycles and be handled in Brake (Core).
Also, tolerance levels for the expected feedback from the brake pressure valves need to be defined as config parameters (<code>maxEbApplyFeedback</code>, <code>minEbReleaseFeedback</code>). \n
Example: \n
For EB Apply order: if receivedFeedback < maxEbApplyFeedback --> Expected feedback received! \n
For EB Release Order: if receivedFeedback > minEbReleaseFeedback --> Expected feedback received! \n

Step             | EB1 | EB2 | EB FB*
---------------- | ----|-----|--------
Step 0           |  0  |  0  |   0
Step 1           |  1  |  0  |   1
Step 2           |  0  |  0  |   0
Step 3           |  0  |  1  |   1
Step 4           |  0  |  0  |   0

The result of Step 0-4 is considered successful if: expected values are set on Core Digital Outputs &&  expected values are read from the Brake Pressure valves within the expected time at each step.

Step 5: Brake Test successful = Approved Results for Step 0-4.
EB FB: EB Feedback from Vehicle brake system

The above Brake Test Sequence shall be performed according to flow chart below in the Brake Adaptation.
@image html brake_test_with_feedback.png
@image latex brake_test_with_feedback.png

\subsection AOS2633 AOS 2633
This condition is implicit with the implementation of virtual functions <code>readEb1Feedback</code> and <code>readEb2Feedback</code>. In the absence of the implementation in adaptation, EB feedback will not be evaluated.

\subsection AOS2653S AOS 2653 S
Covered in the <code>performBrakeTest()</code> initiated the Brake component. \n

\subsection AOS2635S AOS 2635 S
Covered under <code>performBrakeTest()</code> initiated the Brake component.

\subsection AOS2636S AOS 2636 S
Covered under <code>performBrakeTest()</code> initiated in the Brake component and <code>manageBrakeTest()</code> initiated in the Mode Control Component.

\subsection AOS2638 AOS 2638
While the brake test is in progress, if getBrakeTestButtonAbort() returns True it implies that the driver has pushed the Brake Test abort button.

\subsection AOS2640S AOS 2640 S
A new function abortBrakeTest() defined in AbstractModeControl shall verify the following while Brake Test in progress and set the abortBrakeTest flag.:
- The driver has aborted the brake test.
- Brake is ordered by the event handling as a consequence of other reasons than the brake test sequence or a failure reaction related to it.
- The cabin is deactivated.
- Emergency Stop Active signal is detected.
The Brake Test will be aborted, if <code>abortBrakeTest()</code> returns <code>True</code>.

\subsection AOS2654 AOS 2654
If abortBrakeTest() returns True, EB orders triggered by Brake Test sequence shall be released in the Brake Component

\subsection AOS2642 AOS 2642
Covered under <code>manageBrakeTest()</code> initiated in the Mode Control Component and <code>performBrakeTest()</code> initiated in the Brake component.
The relevant event codes shall be defined in DMI and be used while creating the DMI events.

\subsection AOS2641 AOS 2641
Covered under <code>manageBrakeTest()</code> initiated in the Mode Control Component and <code>performBrakeTest()</code> initiated in the Brake component.
The relevant event codes shall be defined in DMI and be used while creating the DMI events.

\subsection AOS2643 AOS 2643
In the <code>performBrakeTest()</code> of the Brake component, before at Step 0, start the Brake Test execution timer configured with <code>brakeTestExecTime</code>.
The timer can be set either using the vfwTimer or by counting up the ATP cycles.
If the timer expires before the end of Step 4, the Brake Test is considered failed and the same is reported to driver via the DMI Handler with the message "Brake Test failed, perform new test!". 

\section NewConfigParams List of new Config Parameters:
- ebApplyFbTimeout
- ebReleaseFbTimeout
- maxEbApplyFeedback
- minEbReleaseFeedback
- brakeTestExecTime
- brakeTestNotifyTimeout


*/