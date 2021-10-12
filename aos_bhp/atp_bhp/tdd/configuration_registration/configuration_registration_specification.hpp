/**
\if AsMainPage
\mainpage Configuration Registration
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-04-28 | TDD update for Configuration & Registration   | akushwah


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
TCC            | Train Control Center
AOS            | Automatic Train Protection and Automatic Train Operation System

\section Introduction Introduction

\subsection Design Design Overview

The purpose of this TDD is to explain the different updates required for the current Configuration & Registration functionality of ATP SW
based on the section 6.1.3 & 6.1.4 of release requirement document "SSRS_AOS_IF150_BHPB_Exported" version 1.3(2017-04-17).

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req     | Short requirement description                                                                                                             | Description
------- | ------------------------------------------------------------------------------------------------------------------------------------------| ------------
AOS 30 S| The AOS shall in ‘Configuration’ mode accept and process a ‘TrainSetup’ message received from TCC                                         | Implemented
AOS 1203| The AOS shall state in the StartUp message to TCC if the configuration was assembled automatically by a TIC system or manually by a driver| Not implemented
AOS 1705| The AOS shall if a RejectConfiguration message is received from TCC restart a manual configuration procedure to be performed by the driver| -
     -  | The AOS shall inform the driver about the reason for the failed train configuration                                                       | Not Implemented
AOS 32  | After that the driver has selected mode Configuration the AOS shall initiate assembling of a new train configuration                      | Partially implemented
AOS 1261| The AOS shall request the driver to assemble a new train configuration if a TIC system is NOT available on-board                          | Partially Implemented
AOS 1744| The driver shall have the possibility to request the TIC system to deliver a new train configuration                                      | Not Implemented
AOS 46 S| The AOS shall reject a new train configuration if cars are configured on both sides of the locomotive                                     | Not Implemented
AOS 2208| The AOS shall inform the TCC in the StartUp message that TIMS is available if TIMS status is Intact                                       | Not Implemented
AOS 2209| The AOS shall inform the TCC in the StartUp message that TIMS is NOT available if TIMS status is Broken                                   | Not Implemented
AOS 2251| During manual entry of train configuration the AOS shall inform the Driver about TIMS availability                                        | Implemented
AOS 34  | The AOS shall send a ‘StartUp’ message to the TCC when a new train configuration, delivered by the TIC system or submitted by the driver, | -
     -  | is accepted by AOS                                                                                                                        | Not Implemented
AOS 2299| The AOS shall based on information from TIC (if available) or from the driver collect the train configuration and send it in the StartUp  | -
     -  | message to TCC in the following block/fields: VEHICLE_ID_DATA ,VEHICLE_LIST_DATA,B_DIRECTION (Locomotive orientation)                     | Not Implemented
AOS 2056| After that a TrainSetup message is received from TCC as an acceptance of a train configuration sent from AOS in the StartUp message,      | -
     -  | the AOS shall: Store the TrainSetup as either temporary or permanent, as given in the TrainsetUp message, Q_TS_STATE field, AND           | -
     -  | Enter mode Registration                                                                                                                   | Partially Implemented
AOS 2053| The AOS shall trigger a Safe Brake To Stop event if Registration or Re-configuration is set in the Q_SETUP field in the TrainSetup message| -
     -  | from TCC                                                                                                                                  | Not Implemented
AOS 1084|The AOS shall clear 'ATP reset' in "PositionReport" message if: 'TrainSetup' message has been received from TCC                            | Implemented
AOS1762S| If the TrainSetup message is received from the TCC the AOS shall: Inform the Driver about the received train configuration, i.e.  the     | -
     -  | number and type of cars connected to the locomotive AND Request the driver to confirm (accept or reject) the received train configuration.| Implemented
AOS 1758| The AOS shall send a AbortSetup message to TCC if the driver rejects a confirmation of the train configuration                            | Implemented
AOS 2236| The driver shall be requested to confirm a rejection of a train configuration                                                             | Partially implemented
AOS 1128| The AOS shall inform the TCC in the StartUp message that TIMS is NOT available if: TIMS status is Broken AND  TCC reported TIMS as NOT    | -
     -  | available. If any of the above two conditions is NOT fulfilled the AOS shall inform the TCC that TIMS is available                        | Not Implemented
AOS 2258| During confirmation of train configuration the AOS shall inform the Driver about TIMS availability                                        | Implemented
AOS 1761| After that the train configuration is accepted AOS shall send a StartUp message to TCC with the Q_ACKNOWLEDGE field set to Accepted in the| -
    -   | CONFIG_CONFIRMATION block                                                                                                                 | Partially implemented
AOS 40  | If the train configuration is accepted AOS shall: Store the TrainSetup received for confirmation from TCC as either temporary or permanent| -
    -   | , and Enter mode Registration                                                                                                             | Partially implemented
AOS 1824| The AOS shall trigger a Safe Brake to Stop event for the following combinations of Q_SETUP field in the 'TrainSetup' message received from| -
    -   | TCC and AOS information about train position: Registration and Approximate position, Registration and Known position, Registration and    | -
    -   | Doubtful position, Reregistration and Approximate position, Reregistration and Known position, Reregistration and Doubtful position       | -
    -   | Reposition and Approximate position,Reposition and Known position,Reconfiguration and Unknown position,Reconfiguration and Approximate    | -
    -   | position, Reconfiguration and Doubtful position                                                                                           | Partially implemented
AOS 56  | The AOS shall initiate a new registration procedure when: The Q_SETUP field in the 'TrainSetup' message received from TCC is equal to     | -
    -   | Registration, and The AOS information about train position is equal to Unknown                                                            | Implemented
AOS 793 | The AOS shall when a new registration procedure is initiated request the driver to enter the following information to be used when        | -
     -  | searching for the first balise: The train orientation on the track (direction as track, direction opposite to track), and The driving     | -
     -  | direction (forward, reverse)                                                                                                              | Implemented
AOS 794 | The AOS shall when requesting driver selection to support new registration, indicate to the driver if the train is a single locomotive or | -
     -  | a locomotive with cars                                                                                                                    | Implemented
AOS 1817| The AOS shall after that the driver has entered the information about the train orientation on the track and driving direction generate an| -
     -  | internal movement authority for searching for the first balise. The movement authority shall include: Balise search distance, AND Ceiling | -
     -  | speed, AND Driving direction as entered by the driver                                                                                     | Implemented
AOS 1030| The AOS shall switch to mode Balise Search after the internal movement authority is generated                                             | Implemented
AOS 1822| The AOS shall initiate a Reregistration procedure when: The Q_SETUP field in the 'TrainSetup' message accepted from TCC is equal to       | -
     -  | Reregistration, and The AOS information about train position is equal to Unknown                                                          | Partially implemented
AOS 1823| The AOS shall evaluate a 'MovementAuthority' message from TCC: If the movement authority is accepted for Re-registration AOS shall enter  | -
     -  | mode Balise Search. If the movement authority is not accepted AOS shall trigger a Safe Brake to Stop event                                | Partially implemented
AOS 1829| The AOS shall initiate a Reposition procedure when: The Q_SETUP field in the 'TrainSetup' message accepted from TCC is equal to Reposition| -
     -  | , and The AOS information about train position is equal to Unknown or Doubtful                                                            | Not Implemented
AOS 1830| AOS shall evaluate a 'MovementAuthority' message accepted from TCC: If an 'ApproximatePosition' message was not previously accepted from  | -
     -  | TCC, or the movement authority is not valid for entering mode Staff Responsible, AOS shall enter mode Safe Brake to Stop. If an           | -
     -  | approximate position is accepted from the TCC and the movement authority is valid AOS shall enter mode Staff Responsible                  | Not Implemented
AOS 1832| The AOS shall end a Reconfiguration procedure and enter mode Normal when: The Q_SETUP field in the 'TrainSetup' message accepted from TCC |  -
      - | is equal to Reconfiguration, and The AOS information about train position is equal to Known                                               | Not Implemented


\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsubsection Configuration Configuration
First task would be the requirement implementation for the configuration functionality, in which all the necessary changes for the configuration 
requirement which are not yet implemented or partially implemented will be taken care. 
List of Requirement Ids are mentioned below:\n
AOS 1203, AOS 1705, AOS 32, AOS 1261, AOS 1744, AOS 46 S, AOS 2208, AOS 2209, AOS 34, AOS 2299, AOS 2056, AOS 2053, AOS 2236, AOS 1128, AOS 1761, AOS 40.

- The Configuration functionality needs to be implemented as per the FFFIS TCC AOS refer \ref configuration_diagram . The configuration of train need to be
start once the Area selection is done and communication is established between AOS and TCC via Protocol Version functionality. This configuration needs to be done for a 
unregistered train after AOS startup is done. In present scenario, Empty Train Setup message need to be sent to show the Configuration page on DMI but as per new requirement once the
driver select the configuration button on DMI, AOS has to show the configuration page.

\subsection configuration_diagram Configuration diagram
@image html configuration.png
@image latex configuration.png

+ AOS 1203: Update the function RadioMessageOutStartUpMessage::collectData(), to set the value for Q_TIC_AVAILABLE parameter based on the train setup value(ticAvailable).

+ AOS 1705: If a validated RejectConfiguration message is received, update the function AbstractModeControl::run(void) in mode control to start the manual train configuration by setting the
sub state of Configuration as "trainConfigWaitNewConfigDMI". Also, use the access function AbstractMessageHandler::getRejectConfigurationInfo() provided by Message handler to set the value in function 
DMIMessageOutAtpNotification::collectData() of dmi_message_out_atp_notification.cpp for the value(reason) mentioned in Q_UNREGISTRATION. 
Need to update the English.ini file for the reason of rejection.

+ AOS 32: Need to incorporate the changes done in Train setup for renaming the getTemporaryTrainSetup(). Also, need to update the function TrainConfigMode::runTrainConfigStart() in mode control
accordingly(TBD). 

+ AOS 1261: Need to incorporate the changes done in Train setup for renaming the getTemporaryTrainSetup(). Also, need to update the function TrainConfigMode::runTrainConfigStart() in mode control
accordingly.Since we are not implementing TIC now, hence we need to hard code TIC functionality(TBD).

+ AOS 1744: In this phase of implementation, No need to do any thing. We will take care of it once we will implement TIC functionality(TBD).  

+ AOS 46 S: Update the function TrainConfigMode::runTrainConfigSendStartUp() for rejection of the message and changed the sub state to runTrainConfigFinishNOK.

+ AOS 2208: TIMS should provide the access function, if TIMS is intact or not. Since TIMS implementation is not being done so need hard code the access function of TIMS(TBD). 
No Update is required for this in current scope of implementation.

+ AOS 2209: TIMS should provide the access function, if TIMS is broken or not. Since TIMS implementation is not being done so need hard code the access function of TIMS(TBD).
No Update is required for this in current scope of implementation.

+ AOS 34: Check the mode as Configuration and Sub state as trainConfigWaitNewConfigDMI in RadioMessageOutStartUpMessage::collectData() function for the submission of Train configuration by driver and No update is required for TIC system as it is not yet implemented.  

+ AOS 2299: Still unclear how we will be deal with the VEHICLE_ID_DATA & VEHICLE_LIST_DATA. Currently AOS is processing only Vehicle ID Data and needs to update the List Data part once
it is cleared from the System Engineer/Requirement team.  

+ AOS 2056: No update is required for this requirement as TSetup data storage is already creating and new field for Q_TS_STATE in Train setup component. It will be set automatically when we are publishing the data from RadioMessageInTrainSetup::publishData() for train setup.
Currently we are entering the registration mode once we receive the Valid Train Setup message. Hence, no update is required for the this part of requirement.  


The Re-configuration is done when AOS has started after Train registration is already done at-least once. This Re-configuration needs to be implemented as per the given 
screen shot from FFFIS TCC AOS refer \ref reconfiguration_diagram.  

+ AOS 2053: Create an Safe Brake to Stop Event in radio_message_in_train_setup.cpp file and report the event in function RadioMessageInTrainSetup::publishData() when Q_SETUP value is
Registration or Re-configuration.

+ AOS 2236: Provide the access function in DMI Handler for the rejection of train Configuration and Mode control will reset the configuration by sub state as trainConfigStart in order to restart the re-configuration.

+ AOS 1128: TIMS should provide the access function, if TIMS is broken or not(TBD). Also, get the TIMS availability from Train setup stored. Based on these two condition mentioned in requirement, set the TIMS availability in Startup message accordingly.
Currently we don't have TIMS functionality hence no update is required for this requirement also.

+ AOS 1761: No update is required for this as it is already been implemented in collect() function of startup message.

+ AOS 40: Set the Train Setup as temporary or Permanent based on Q_TS_STATE value received from Train setup message, if current mode is Configuration and sub state as trainConfigFinishOK
and also set the current mode as Registration in mode control function TrainConfigMode::runTrainConfigFinishOK().

\subsection reconfiguration_diagram Reconfiguration diagram
@image html reconfiguration.png
@image latex reconfiguration.png

\subsubsection Registration Registration
The second task would be of the  requirement implementation for the Registration functionality, in which all the necessary changes for the registration 
requirement which are not yet implemented or partially implemented will be taken care. 
List of Requirement Id are mentioned below:\n
AOS 1824, AOS 1822, AOS 1823,AOS 1829, AOS 1830, AOS 1832.

The Registration functionality needs to be implemented as per the given screen shot below from FFFIS TCC AOS refer \ref registration_diagram . The scenario is same as currently implemented registration functionality but
we need to take the new parameters added to radio messages according to FFFIS AOS TCC. 

+ AOS 1824: Update the function TrainRegistrationMode::runTrainRegistrationStart() to raise an event(create and report safe brake event) with the help of access function provided in Message handler RadioMessageInTrainSetup::getQSetup()
and AbstractPosition::getAccuracyState() function of Position component.

\subsection registration_diagram Registration diagram
@image html registration.png
@image latex registration.png

Once the train setup message is received from TCC while doing Re-registration, AOS has to send the CONFIG_CONFIRMATION parameter as Ack and then train will move in Re-registration mode. 

+ AOS 1822: Update function TrainRegistrationMode::runTrainRegistrationStart() to check Q_SETUP value in Reregistration via access function RadioMessageInTrainSetup::getQSetup() and AbstractPosition::getAccuracyState() function of Position component, then set the sub-state of registration mode as trainRegistrationWaitReRegMA.

+ AOS 1823: Update the function TrainRegistrationMode::runTrainRegistrationWaitReRegMA() to raise the event in the else part of current code. 

\subsection reregistration_diagram Reregistration diagram
@image html reregistration.png
@image latex reregistration.png

+ AOS 1829: Update the function TrainRegistrationMode::runTrainRegistrationStart() to enter the newly created state runTrainReposition and check Q_SETUP value Reposition via access function RadioMessageInTrainSetup::getQSetup() and AbstractPosition::getAccuracyState() function of Position component, then only set the sub-state of registration mode as reposition state.

+ AOS 1830: Create the new function TrainRegistrationMode::runTrainReposition() to check the validity of MA via getMaHead() function of message handler and get the approximate message status via getApproximatePosition() function from message handler before setting it to staff responsible mode via setNextMode(ATPModeStaffResponsible).
Need to remove the processing of RadioMessageInApproximatePosition::invalidate() function as we need to check whether, AOS has received Approx message earlier or not. Also, create and report the safe brake to stop event when Approximate message is not received previously and MA is not Valid.

+ AOS 1832: Update the function TrainRegistrationMode::runTrainRegistrationStart() to enter newly created sub state runTrainReconfiguration for the registration state machine and check Q_SETUP value reconfiguration via access function RadioMessageInTrainSetup::getQSetup() and AbstractPosition::getAccuracyState() function of Position component, then only set the sub-state of registration mode as reconfiguration state.
In the Re-configuration state function, we need to set it to normal mode via setNextMode(ATPModeNormal)

\subsection train_registration_mode_state_machine registration state machine diagram
@image html train_registration_mode_state_machine.png
@image latex train_registration_mode_state_machine.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

<Describe if there are any alternatives to the chosen design.>

\subsection ExternalInterfaceDescription External Interface Description


\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection Component-n Component-n

<Describe the design/re-design for each of the affected components (even if the component is not changed itself it might also be affected by the changes).>

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

- A new page needs to be created in DMI to show the configuration, possession, shunting and yard mode button and the desired action will taken by Driver to move in different mode.
As soon the Driver login the next page what should appear should somewhat like this.
Proposal for the new dmi page is given below:

\subsection dminewpage dmi new page diagram
@image html dmi_new_page.png
@image latex dmi_new_page.png

\subsubsection ScreenImages Screen Images

<If applicable, include screen-shoots or other images to better describe the UI.>

\subsubsection ObjectsAndActions Objects and Actions

<If applicable, describe the actions trigged by certain UI events.>

\section ImplementationDistribution Task Distribution

<Suggest how to distribute the implementation in different tasks.>

\section AdditionalMaterial Additional Material

References has been taken from the below mentioned docs.
+ FFFIS TCC-AOS InterFlo 150 Version 5.8
+ Interface Specification ATP MMI InterFlo 150 Version 1.35

*/