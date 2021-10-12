/**
\if AsMainPage
\mainpage Doppler Radar configuration in COD
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 17-07-2017 | Creation of process capable of configuring the Doppler Radar.  | adgupta


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
COD            | Common Odometry

\section Introduction Introduction
The Doppler radar is a part of the Commom Odometry system(COD) and is used for the calculation of the vehicle speed and distance traveled. This is in addition to the tachometer
which is also available in the COD for the same reason.
ATP should be responsible for the configuration of the Doppler radar. Major steps in the configuration process are:
- Creation of the config parameters which enables/disables the Doppler Radar in COD system.
- Creation of other parameters that will be used as the values given to the static and dynamic Configuration Telegram.
- Using of the config parameters to update the static and dynamic configuration Telegrams
- Handling of the data coming back from the COD system related to the Doppler Radar in the Odometer configuration Response/Measurement Telegram.

\subsection Design Design Overview
\subsubsection CreationOfConfigParameters Creation of config Parameters
The following config parameters needs to be created and implemented for configuring the Doppler Radar in the COD.
- COD Sensor Config: Used to select what sensors(Tachometer, Doppler radar or both) will be used for speed and distance calculation in the COD system.
- Traction Control: Used to specify the type of traction system with intentional slipping and/or sliding mounted.
- Doppler Lower Speed range: Lower Doppler radar speed range
- Doppler Upper Speed range: Upper Doppler radar speed range
- Max Doppler Acceleration: Highest allowed acceleration/Deceleration from Doppler radar.
- Doppler Pulse: Doppler radar pulse Rate
- Doppler Precision: Doppler Radar pulse rate precision

\subsubsection UpdatingCODConfig Updating of the configuration Telegrams
- Acquire the value of "COD Sensor Config" config parameter to set the value of the SENSOR_CONFIG field of the Odometer Static Configuration Telegram should be set to 1.
- Acquire the corresponding config parameter values to populate configuration Telegram variables M_CONTR_TRACTION(Traction Control), V_LOW_DOPPLER(Doppler Lower Speed range),
V_HIGH_DOPPLER(Doppler Upper Speed range), A_MAXACC_DOPPLER(Max Acceleration), DOPPLER_PULSE(Doppler Pulse), DOPPLER_PRECISION( Doppler Radar pulse rate precision).

\subsubsection HandlingCODResponse Handling of COD Response/Measurement
Check the value of T_RADAR_PLAUSIBLE in Odometer Measurement data Telegram and depending upon the values of M_CONTR_TRACTION variable.
Take appropriate action depending upon the response specified in the GSP 2 Common Odometry Interface Spec. Doc:3NSS012264D0033 section 3.6 and 3.7 version 5.1.
The actions will be in the form of logging onto the N-JRU and Console.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
List of affected/triggering requirements.

Req        | Short requirement description                                                  | Justification
---------- | -------------------------------------------------------------------------------| -----------------------
-          | TBD                                                                            | -

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description
The Odometry component in ATP will make use of the configurations and interfaces with the COD system outside the ATP. This will also depend on the availability and
response of the Doppler Radar in the COD system.

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection Component-Config Component-Config
The Config component should be able to handle the addition of new config parameters and assure the availability of these parameter when any other component queries.

\subsection Component-Odometry Component-Odometry
- Updating of the static and dynamic Configuration Telegrams to configure the COD such that the Doppler Radar is also configured.
- Updating handling of the Response and Measurement from the COD.

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

\subsubsection ScreenImages Screen Images

\subsubsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution
The described design will be implemented as 2 tasks:
- Config Parameters update: Adding and handling of the Config parameters to facilitate Doppler radar configuration and operation.
- Odometry update: Making use of the config parameters in Odometry to interface with the COD system and configure/enable operation of the Doppler Radar.

\section AdditionalMaterial Additional Material
To comply with the GSP 2 Common Odometry Interface Spec. Doc:3NSS012264D0033 to handle the Response from COD system.

*/