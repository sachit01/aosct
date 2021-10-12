/**
\if AsMainPage
\mainpage Automatic Train Configuration
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-06-09 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description

\section Introduction Introduction

\subsection Design Design Overview

Investigate the design for Automatic Train Configuration

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS 1203   | The AOS shall state in the StartUp message to TCC if the configuration was assembled automatically by a TIC system or manually by a driver.| Partially implemented
AOS 1744   | The driver shall have the possibility to request the TIC system to deliver a new train configuration. | Partially implemented
AOS 34     | The AOS shall send a StartUp-message to the TCC when a new train configuration, delivered by the TIC system or submitted by the driver, is accepted by AOS | Partially implemented
AOS 2299   | The AOS shall based on information from TIC (if available) or from the driver collect the train configuration and send it in the StartUp message to TCC in the following block/fields:VEHICLE_ID_DATA, VEHICLE_LIST_DATA, B_DIRECTION (Locomotive orientation) | Partially implemented
AOS 32	   | After that the driver has selected mode Configuration the AOS shall initiate assembling of a new train configuration. | Implemented
AOS 1261   | The AOS shall request the driver to assemble a new train configuration if a TIC system is NOT available on-board. | Partially implemented
AOS 1262   | The AOS shall request the TIC system to assemble and deliver a train configuration if this system is installed and available on-board. | Not implemented
AOS 1742   | The maximum execution time for the TIC system to deliver a train configuration shall be configurable through a parameter. | Not implemented
AOS 1193   | If a new train configuration is not delivered by the TIC system on time  the AOS shall regard the TIC system as NOT available on-board. | Not implemented


\section SystemArchitecturalDesign System Architectural Design.

The interaction between the different components to full-fill the above mentioned requirements:

@image html vcom_tic_config.png
@image latex vcom_tic_config.png


\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection VehicleCom	Vehicle Com

\subsubsection General General

The VehicleCom component shall keep latest status-values received from the LCS until next status message is received from LCS.
If the connection towards LCS is disconnected (the VehicleCom will notice it by a timeout when receiving the LCS Status message), the access methods towards VehicleCom will return false (value not valid).

\subsubsection AOS1203 AOS1203

When writing to Preliminary TrainSetup from Vehicle COM, set the ticAvailable parameter to true and the brakeSystem to received Brake system in use (getBrakeSystemInUse()).
If writing to Preliminary TrainSetup from DMI, set the ticAvailable parameter to false.

\subsection ModeControl Mode Control

\subsection AOS1744 AOS1744

If an error occurs (e g time-out) when calling TIC::getStatus() from Modecontrol (in state TCWaitTIC), the Config Sub mode is set to TCWaitNewConfigDMI, and the driver can either enter config manually or
send a request to try TIC again.  In method runTrainConfigWaitNewConfigDMI(), check if 'TIC-Request button' was entered by calling DMI getDMIButtonStatus() (TBD which bit to check).
If TIC is requested -> call TIC::requestConfig() and go to sub-state TCWaitTIC.

\subsubsection AOS34AOS2299 AOS34, AOS2299

Mode Control will be in either TCWaitNewConfigDMI or TCWaitTIC, depending if TIC is available or not. The DMI or VCOM will put received configuration in preliminaryTSetup.
TCWaitNewConfigDMI mode is already implemented. TCWaitTIC shall call getPreliminaryTrainSetup() to see if any data is pushed, and continue to TCSendStartUp.

\subsubsection AOS32 AOS32

This requirement is already full-filled and independent if TIC is used or not. (Transition from PowerUp- to Configuration mode).

\subsection MessageHandler Message Handler

\subsubsection AOS34AOS2299 AOS34, AOS2299

B-Direction will be hard-coded for TIC to B-end facing cars.

\subsubsection AOS12611262 AOS1261, AOS1262

In ModeControl state TCWaitQSetup a check shall be done if TIC is available (TIC::getTicAvailable(), if TIC is not available the next state is TCWaitNewConfigDMI.
If TIC is available a call to TIC::requestConfig() shall be made and proceed to sub-state TCWaitTIC.

\subsubsection AOS1742AOS1193 AOS1742, AOS1193

The timeout is implemented in the TIC component. When waiting in sub-state TCWaitTIC and the TIC::getStatus() is returning 'error' (as a cause for the timeout), the next state will be TCWaitNewConfigDMI, to be able to 
enter the configuration manually. TBD: It shall be possible to retry with a button in DMI according to AOS1744(for instance if the TIC is repaired)? 


\subsection TSetup Train Setup

\subsubsection VehicleTypeSetup VehicleTypeSetup

- Add a new attribute in class TrainSetup to store loaded/unloaded status.
- Add new attributes in class TrainSetup to store total train weight (dynamic/ECPB/Pneumatic and loaded/unloaded for all types i e Totally 6 different values)
- Add new storage for VehicleTypeSetup (similar to CarSetupStorage/CarSetup)

@image html vehicle_type_setup.png
@image latex vehicle_type_setup.png

The received Vehicle Types in TrainSetup shall be stored in the new storage for VehicleTypeSetup, the loaded/unloaded flag in TrainSetup is set to 'loaded'.
When an MA arrives the loaded/unloaded flag is also updated according to MA.

When Vehicle Types are stored, and when an MA arrives, a new weight shall be calculated for the train. This weight will be the sum of all cars (according to the vehicle types and status of 
unloaded/loaded flag).All different types are calculated (Dynamic/ECPC and Pneumatic).

\subsection ScreenImages Screen Images

\subsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution

The following tasks needs to be synchronized:

- #4777 AOSPC, Automatic train configuration: Implementation - Simulation
- #4736 Preliminary SCDS for TIC Component -SCDS
- #4736 TIC - Implementation - ATP BHP
- #4776 Automatic train configuration: Implementation - ATP BHP
- #???? VehicleTypeSetup in DataStorage

\section AdditionalMaterial Additional Material

For easier reference, the state-chart for the Configuration Mode is included:

@image html train_config_mode.png
@image latex train_config_mode.png


*/