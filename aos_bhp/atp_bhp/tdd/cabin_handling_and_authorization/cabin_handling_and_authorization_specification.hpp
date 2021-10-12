/**
\if AsMainPage
\mainpage Technical Design Description for Cabin Handling & Authorization (Driver Login)
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | ---------------------------------------------          | ---------
1.0     | 2017-04-06 | Document creation                                      | skothiya
2.0     | 2017-04-07 | Document updated for peer review comments              | skothiya
3.0     | 2017-04-07 | Document update                                        | skothiya
4.0     | 2017-04-11 | Document updated after final review comments           | skothiya
5.0     | 2017-04-12 | Document updated                                       | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
ATPOK          | ATP OK output signal 

\section Assumption Assumption
   - According to ATP implementation, Driver Login Status "not authorized" is mapped to "driverLoggedOut" and "Driver Authorized" is mapped to "driverLoggedIn" state.
   - AOSOK signal is considered as ATPOK in ATP implementation.
   - For requirement AOS2196 new button "Authorize" to authorize the driver in yard mode need to be implemented on DMI.

\section Introduction Introduction
This document describes the technical design description of Cabin Handling and Authorization (Driver Login) function for AOS BHP.
Document contains the description of design/design changes to implement below mentioned requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req    | Short requirement description                                                                                                             | Description 
------ | -------------------------------------------------------------------------------------- ---------------------------------------------------| ------------
AOS1314| The AOS shall read the cabin input signal and activate the cabin if related input signal becomes active AND if no cabin is already active.| Implemented
AOS1436| When the AOS is configured with two cabins and both cabin inputs are active at the same time, the AOS shall issue a Brake event.          | Not Implemented
AOS1298| The AOS shall issue a Brake event if No cabin is active AND ATP mode is other than Sleeping                                               | Not Implemented
AOS146 | The AOS shall initially, at system start, classify Driver Login status as 'Not Authorized'.                                               | Implemented
AOS103 | The AOS shall classify Driver Login status as 'Not Authorized' on deactivation of the cabin.                                              | Not Implemented
AOS2196| The AOS shall request the Driver to acknowledge authorization when ATP Mode is Yard And There is no radio communication                   | Partially Implemented
AOS769 | The AOS shall request the Driver to log in and send login information to TCC when AOSOK signal is active and Cabin is Active              | Partially Implemented
AOS144 | The AOS shall accept and process a 'DriverLogonStatus' message, received from TCC when AOSOK signal is active and cabin is active         | Implemented
AOS149 | The AOS shall classify the Driver Login status as 'Driver Authorized' if the received 'DriverLogonStatus'message acknowledges credentials.| Implemented
AOS150 | The AOS shall classify the Driver Login status as 'Not Authorized'  if the received 'DriverLogonStatus' message rejects the credentials.  | Implemented
AOS154 | The AOS shall apply service brake while Driver Login status is Not Authorized except in ATP Mode sleeping                                 | Not Implemented
AOS145 | TThe AOS shall issue a Log event and discard a DriverLogonStatus message, received from the TCC, if  Driver Login status is Authorized    | Not Implemented
AOS2112| When the Driver has confirmed change to ATP mode Yard (in case no TCC communication) AOS shall classify the login status Authorized       | Not Implemented
AOS148 | When the Driver issues a manual log out request the AOS shall: Issue log event AND Change Driver Login status to 'Not Authorized'.        | Partially Implemented
AOS823 | The AOS shall offer the Driver the possibility to Log out if: The Authorization is Driver Authorized AND The vehicle is at standstill.    | PartiallY Implemented
AOS2195| When ATP mode changes from ATP mode Yard to Configuration the AOS shall change the Driver Login status to Not Authorized.                 | Not Implemented


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 1.35
2      | FFFIS TCC-AOS                                      | 5.8



\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
  This section describes affected components and function with logic need to be added/changed to implement the different requirements of Cabin Handling and Authorization in ATP.

\subsection  Cabin Cabin Handling
  - Covered BHP Requirement: AOS1314, AOS1436, AOS1298 \n
  - Affected function and component  : void AbstractModeControl::manageCabActiveStatus(void) & Mode Control \n
     + Description of Changes:Implemention of a function to read cabin related signals (using LOCOIO component) and set cabin status inside ATP state machine with below logic. \n
@image html cabin_handling_flow.png
@image latex cabin_handling_flow.png

\subsection AOS146 AOS146
    - Affected function and component :DriverLoginSeq::DriverLoginSeq() & Mode Control \n
     + Description of Changes: Initializing Login status value to "Not Authorized" in constructor of DriverLoginSeq class.

      + Pseudo code\n
        DriverLoginSeq::DriverLoginSeq()\n
        {\n
          // Below statement is covering Requirement\n
          seqState = driverLoggedOut;\n
          loginFailedCycleCount = 0U;\n
        }\n


\subsection AOS1032 AOS1032
  - 1st Affected function and component : void AbstractModeControl::manageCabActiveStatus(void) & Mode Control      
      + Description of Changes: Need to make changes in manageCabActiveStatus method to store the previous active cab value.
  - 2nd Affected function and component :void DriverLoginSeq::runDriverLoggedIn() & Mode Control 
      + Description of Changes: Need to make changes in runDriverLoggedIn method of DriverLoginSeq class to make DriverLoginSeq::seqState = driverLoggedOut  if current active cab is not equal to previous active cab


\subsection AOS154 AOS154
  - Affected function & component : DriverLoginSeq::runDriverLoginVerification() & Mode Control
     + Description of Changes: New function need to be implemented in DriverLoginSeq.cpp file to apply service break if current mode is other than sleeping mode and Driver Login state is not driverLoggedIn.
       This function will be called from DriverLoginSeq::run() method every time.
     + Pseudo code\n
       void DriverLoginSeq::checknApplyBreak(void)\n
       {\ 
         //If driver is not authorized and train is not in sleeping mode apply service break\n
        if (seqState != driverLoggedIn && AbstractModeControl::corePtr()->getCurrentMode() != ATPModeSleeping)\n
          ATC::AbstractEventHandler::corePtr()->reportEvent(driverNotAuthSB, __FILE__, __LINE__);\n
       }\n



\subsection AOS2196 AOS2196
  - 1  Affected function and component : void DMIMessageOutATPModesAndStatus::collectData() & DMI Handler
      + Description of Changes: Need to make changes in DMIMessageOutATPModesAndStatus method for following logic:
         Check current mode is yard mode, activate "Authorize" button/option in yard window when ATPOK signal is active, train is not moving, cabin is active and no radio communication between TCC and ATP .\n
        + Pseudo Code: \n
           if (RadioCom::AbstractRadioHandler::corePtr()->getConnected() && ATPOK && Pos::AbstractOdometry::corePtr()->isTrainStandStill())\n
             Then activate "Authorize" button/option in yard mode window.\n

  - 2  Affected function and component : DMIMessageInDMIToATPData::validateMode() & DMI Handler
       + Description of Changes: Need to make changes in DMIMessageInDMIToATPData method of DMIMessageInDMIToATPData class to check if Authorize" button/option is pressed.
        + Pseudo code\n
         if ((buttonStatus == DMIATPBrakeRelease) || (buttonStatus == DMIATPAlertButton)\n
         || (buttonStatus == DMIATPEnterYardMode) || (buttonStatus == DMIATPTrainConfig)\n
         || (buttonStatus == DMIATPDriverLogOut) || buttonStatus == DMIATPAuthorizedYardMode)\n
         {\n
           modeValid = true;\n
         }\n


  - 3  Affected function and component :void DriverLoginSeq::runDriverLoggedOut() & Mode Control
      + Description of Changes: Need to add below implementation in runDriverLoggedOut method of DriverLoginSeq class
       + Pseudo code:\n
       if ((AbstractModeControl::corePtr()->getCurrentMode() == ATPModeYard) && (RadioCom::AbstractRadioHandler::corePtr()->getConnected()))\n
       {\n
       ////set driver status to authorized (logged-in) in yard mode as TCC not connected\n
       seqState = driverLoggedIn;\n
       }\n


\subsection AOS2195 AOS2195
   - Affected function and component :void DriverLoginSeq::runDriverLoggedIn() & Mode Control
     + Description of Changes: Need to add below logic to set Driver Login state to 'Not Authorized' in runDriverLoggedIn() method of DriverLoginSeq class.
       + Pseudo code\n
        If(currentMode == configuration && previousMode == Yard)\n
        Then set seqState = DriverLoggedout\n

\subsection AOS769 AOS769
   - Affected function and component : void DMIMessageOutATPModesAndStatus::collectData() & DMI Handler 
     + Description of Changes: Need to add below condition in collectData function of DMIMessageOutATPModesAndStatus class to control Login button activation/deactivation on 
                             the basis of AOSOK signal, train is standstill and cabin is active.
         + Pseudo code\n
             Set bit for Radio Available in dmiIntegrityRelatedData and if ATPOK signal is OK and train is not moving\n
             if (RadioCom::AbstractRadioHandler::corePtr()->getConnected() && ATPOK && Pos::AbstractOdometry::corePtr()->isTrainStandStill())\n
             {\n
               dmiIntegrityRelatedData = dmiIntegrityRelatedData | radioAvailable;\n
             }\n

\subsection Requirement AOS144, AOS149 & AOS150 
   - Affected function and component  : DriverLoginSeq::runDriverLoginVerification() & Mode Control 
     + Already Implemented requirement Below function implementation is fulfilling requirement AOS149 & AOS150:
      + Pseudo code: \n
        void DriverLoginSeq::runDriverLoginVerification()\n
        {\n
         LogonStatus lStatus;\n
         //Get Driver login status message from message handler\n
         if (AbstractMessageHandler::corePtr()->getDriverLogonStatus(lStatus))\n
         {\n
          //Login verified by TCC\n
          if (lStatus == DriverLogonSuccesful)\n
          {\n
           seqState = driverLoggedIn;\n
          }\n
          else\n
          {\n
           seqState = driverLoginFailed;\n
           loginFailedCycleCount = 0U;\n
           }\n
         }\n
        }\n

       Above function is getting called only when Driver is in Logged-out state, which is requirement AOS144.

      + Pseudo code: \n
        bool RadioMessageInDriverLogonStatus::validate()\n
        {\n
         trace->write(briefTrace, "Validating DriverLogonStatus");\n

        // Parse, validate and publish data\n
        if (DataAvailable == dataProcessState && current Driver login status == "not authorized"/"DriverLogonFailed")\n
        {\n
         if (parseMessageData())\n
         {\n
          if (validateMode())\n
          {\n
            dataProcessState = DataValidated;\n
          }\n
         }\n
       }\n

        return (DataValidated == dataProcessState);\n
      }\n


\subsection AOS145 AOS145
   - Affected function and component :RadioMessageInDriverLogonStatus::validate() & Message Handler\n
     + Description of Changes: In RadioMessageInDriverLogonStatus::validate() need to add a condition to check if current Driver Login state is "driverLoggedIn".\n

     + Pseudo code:\n

     bool RadioMessageInDriverLogonStatus::validate()\n
     {\n
       trace->write(briefTrace, "Validating DriverLogonStatus");\n

       // Parse, validate and publish data\n
      if (DataAvailable == dataProcessState)\n
      {\n
       if((AbstractMessageHandler::corePtr()->getDriverLoginSeqState()) != DriverLoginSeq::driverLoggedIn)\n
       {\n
        if (parseMessageData())\n
        {\n
         if (validateMode())\n
         {\n
           dataProcessState = DataValidated;\n
         } \n
        }\n
       }\n
       else\n
       {\n
         //Log Event for not valid DriverLogonStatus message\n
       }\n
      }\n
        return (DataValidated == dataProcessState);\n
     }\n

\subsection AOS2112 AOS2112
   - Affected function and component :void DriverLoginSeq::runDriverLoggedOut() & Mode Control
     + Description of Changes: Need to add below implementation in runDriverLoggedOut method of DriverLoginSeq class
     + Pseudo code:\n
     if ((AbstractModeControl::corePtr()->getCurrentMode() == ATPModeYard) && (RadioCom::AbstractRadioHandler::corePtr()->getConnected()))\n
      {\n
        ////set driver status to authorized (logged-in) in yard mode as TCC not connected\n
        seqState = driverLoggedIn;\n
      }\n


\subsection AOS148 AOS148
    - Affected function and component:\n
      + 1 - void DriverLoginSeq::runDriverLoggedIn() & Mode Control\n
          - Description of Changes: In runDriverLoggedIn method of DriverLoginSeq need to add below code to logout the driver when logout button will be pressed in DMI.\n
            + Pseudo code\n
             if (DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIATPDriverLogOut)\n
             {\n
              Issue a log event regarding driver logout\n
              seqState = driverLoggedOut;\n
             }
      + 2 - bool DMIMessageInDMIToATPData::validateMode() & DMI Handler\n
           - Description of Changes:Condition regarding Logout Button press need to be added in validateMode function of DMIMessageInDMIToATPData class.\n
              + Pseudo code\n
               if ((buttonStatus == DMIATPBrakeRelease) || (buttonStatus == DMIATPAlertButton)\n
               || (buttonStatus == DMIATPEnterYardMode) || (buttonStatus == DMIATPTrainConfig)\n
               || (buttonStatus == DMIATPDriverLogOut))\n
               {\n
                modeValid = true;\n
               }


\subsection AOS823 AOS823
   - Already Implemented, AOS accepting logout when train is in standstill and driver is already authorizedDMI.

*/