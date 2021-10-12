/**
\if AsMainPage
\mainpage Technical Design Description for EMD - Pneumatic break system
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2019-02-08 | Document creation                                      | ramyashree
2.0     | 2019-02-20 | OBRD status report on train setup acceptance           | ramyashree

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
TCC            | Train Control Center
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of break system of type Pneumatic.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req          | Short requirement description                                                                                                                                                               | Description 
------       | -------------------------------------------------------------------------------------- -----------------------------------------------------------------------------------------------------| ------------
AOS_BHPB 3317| If the configured locomotive type is EMD AND the brake system in use is Pneumatic the AOS shall store the TrainSetup apply SB and wait for valid feed-back from OBRD within propagated time.| -
          -  | Valid feed-back is when the Last Car Brake Pressure has dropped below a configurable value within the time Tp.                                                                              | Implemented
AOS_BHPB 3318| If valid feed-back is not received within the configurable time, then train setup shall be aborted by: Release SB. Sending AbortSetup message to TCC.                                       | -
          -  | Informing the Driver about the reason. Allow the driver to restart Configuration.                                                                                                           | Implemented
AOS_BHPB 3319| If valid feed-back is received within the configurable time OR if the configured locomotive type is other than EMD with brake system in use Pneumatic,                                      | -
          -  | the AOS shall: Release SB. Request the driver to confirm that "Pre-departure Tests" are performed.                                                                                          | Implemented

\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1      | FFFIS TCC-AOS                                      | 5.16

\subsection EMD Pneumatic Break System Design Change Overview:
- When train state is in trainConfigConfirmNewConfigFrmTCC check if train setup is valid. After accepting train setup, jump to new sub state trainConfigCheckValidBrkPrFeedBack.
In adaptation override runTrainConfigCheckValidBrkPrFeedBack() and do the following:
    + check if locomotive type is EMD with brake system in use Pneumatic. \n
    + Apply Service Brake. \n
    + Check valid feed back from OBRD \n
    + if valid feed back received change submode to trainConfigTSetupAccepted.
    + if valid feed back not received change mode to new submode trainConfigSendAbortSetupToTCC.
    + In RadioMessageOutAbortSetup::collectData() check if train config is trainConfigSendAbortSetupToTCC then send abort message to TCC.
    + in runTrainConfigSendAbortSetupToTCC() set submode trainConfigRejectedOrAborted. \n
The applying service break and check for last car break pressure is in evaluatePneumaticBrakeSystem() in TIMS.
- Mode Control: Below train states will be handled in mode control: \n
    + trainConfigConfirmNewConfigFrmTCC. \n
    + trainConfigCheckValidBrkPrFeedBack. \n
    + trainConfigSendAbortSetupToTCC.

@image html pneumatic_modecontrol_obrd.png
@image latex pneumatic_modecontrol_obrd.png

*/