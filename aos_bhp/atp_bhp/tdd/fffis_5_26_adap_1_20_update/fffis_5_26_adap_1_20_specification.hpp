/**
\if AsMainPage
\mainpage Technical Design Description for FFFIS-AOS v5.26 & BHPB v1.20 Update
\endif

\section VersionLog Version Log
Version | Date       | Description                                    | Signature
------- | ---------- | -----------------------------------------------|----------
1.0     | 2019-01-28 | Document created to support FFFIS-AOS upgrade  | csundin


\section Abbreviations Abbreviations
Abbreviation   | Definition
-------------- | ----------
AOS            | ATP-On board system
ATP            | Automatic Train Protection
FFFIS          | Form Fit Functional Interface Specification 
MA             | Movement Authority
TCC            | Train Control Center
TCCSim         | TCC Simulator
TDD            | Technical Design Description


\section Introduction Introduction
This document describes the changes in AOS related to upgrading the TCC interface according to FFFIS TCC-AOS v5.26 and FFFIS TCC-AOS BHPB v1.20.


\subsection ExternalInterfaceDescription External Interface Description
The following external interface descriptions are used to implement below mentioned changes.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|--------
1      | FFFIS TCC-AOS                                      | 5.26
2      | FFFIS TCC-AOS BHPB                                 | 1.20


\section Changes Changes to be implemented


\subsection CoreChanges Changes between v5.20 & v5.26 of FFFIS TCC-AOS

<b>Section 5.1.4.5 Path Data:</b> added: up to 150 TRACKS and 50 SPEED_CHANGE_POSITION in one Path message <br />
  => constants "trackSize" and "speedChangePositionSize" must be changed

<b>Section 5.2.2.3 PositionReport:</b> under B_DIRECTION, added: <br />
  Registered modes except Location: Movement direction of valid MA, last valid MA if idling. <br />
  Location: Movement direction based on selected travel direction by driver or ATO. <br />
  Unregistered modes: N/A. <br />
  => this is correctly implemented for Location mode but needs to be corrected for other modes

<b>Section 5.3.34 TRACK_DATA:</b> changed: L_TRACK replaces two D_POSITION <br />
  => TCCSim: all MA:s must be edited (remove startPosition and subtract startPosition from all other positions) <br />
  => ATP: Implementation step 1: in Track and TrackData, set start position to zero and end position to track length; then test thoroughly <br />
  => ATP: Implementation step 2: in Track and TrackData, replace start and end positions with track length; then test thoroughly

<b>Section 5.4.22 NID_BG:</b> max value was changed from 65535 to 16383 <br />
  => add range check in RadioMessageInPossessionAcknowledge::parseMessageData() <br />
  => add range check in RadioMessageInMovementAuthority::parseMessageData()

<b>Section 6.5 Config params:</b> Added 6 params <br />
  Pm_EBOVERSPEED (for AbstractConfig::SpeedMarginEBId) <br />
  Pm_SBOVERSPEED (for AbstractConfig::speedMarginSBId) <br />
  V_EBOVERSPEEDMAX (for AbstractConfig::maxSpeedMarginEBId) <br />
  V_EBOVERSPEEDMIN (for AbstractConfig::minSpeedMarginEBId) <br />
  V_SBOVERSPEEDMAX (for AbstractConfig::maxSpeedMarginSBId) <br />
  V_SBOVERSPEEDMIN (for AbstractConfig::minSpeedMarginSBId) <br />
  => add these global parameter names <br />
  => check if min/max/default values need to be updated

<b>Section 6.5</b>: Deleted V_EBOVERSPEED and V_SBOVERSPEED <br />
  => remove the setting of these global names (note: V_EBOVERSPEEDMAX and V_SBOVERSPEEDMAX are the correct names for these parameters)

<b>Section 6.5</b>: "Pr_EBMARGINADDED" renamed to "P_EBMARGINADDED" <br />
  => rename the global name

\subsection AdapChanges Changes between v1.12 & v1.20 of FFFIS TCC-AOS BHPB

<b>Section 4.1.1.1</b> deleted: BHPB_TRAIN_WEIGHT <br />
<b>Section 4.2.5</b> deleted: BHPB_TRAIN_WEIGHT <br />
<b>Section 5.1</b> deleted: BHPB_TRAIN_WEIGHT <br />
  => remove this field in TCCSim <br />
  => in RadioMessageInCommandMessageBHP, remove bhpTrainWeightVec, trainWeightSize etc <br />
  => instead, use the weights calculated by RadioMessageInTrainSetup and sent to AbstractTSetup

<b>Section 4.1.2.1 PositionReport:</b> BHPB_LOAD_STATUS_CHANGE was removed <br />
  => move the attribute and the related bits of code to RadioMessageOutStartUpMessageBHP (see below) <br />
  => make the corresponding change for TCCSim

<b>Section 4.1.2.2 StartUpMessage:</b> BHPB_LOAD_STATUS was added <br />
  => create RadioMessageOutStartUpMessageBHP (derived from RadioMessageOutStartUpMessage) and add the relevant bits from PositionReport

<b>Section 5.2.1:</b> added value for Q_TRACK_DATA_TYPE:
  Track data item type                        | Id     | N_VALUE
  ------------------------------------------- | ------ | -------
  Level crossing, set max approach speed      | 129    | Distance for the approach speed
  => new enum needed, e.g. "BHPBTrackDataType" <br />
  => extend the range check in (or for) RadioMessageInMovementAuthorityBHP <br />
  Suggestion 1: move validateQ_TRACK_DATA_TYPE() to RadioMessageInMovementAuthority and override it in RadioMessageInMovementAuthorityBHP <br />
  Suggestion 2: move all validate functions to AbstractRadioMessageIn (or to new class AbstractRadioMessage in case these functions are needed in out messages as well)

<b>Section 5.4:</b> added D_EXTREVERSING, V_EXTREVERSINGSPEED, V_MINIMUMAPPROACHSPEED <br />
  => implement these parameters and set their global names (but use "V_MINAPPROACHSPEED" instead)

<b>Section 5.4:</b> S_TYPICALCONFIGCARTYPE* renamed to S_TYPICALCONFIGCAR* <br />
  => no impact (already implemented)

<b>Section 6.2.1:</b> added "Entering Car Dumper" <br />
  => no impact (no new signalling)

<b>Section 6.2.2:</b> added "Movement Inside Car Dumper" <br />
  => no impact (no new signalling)

<b>Section 6.2.3:</b> added "Exiting Car Dumper" <br />
  => no impact (no new signalling)

*/
