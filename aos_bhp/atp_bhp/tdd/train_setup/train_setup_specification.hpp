/**
\if AsMainPage
\mainpage Technical Design Description for Train Setup
\endif

\section VersionLog Version Log
Version | Date       | Description                                            | Signature
------- | --------   | -------------------------------------------------------|---------------
1.0     | 2017-04-20 | Document creation                                      | skothiya
2.0     | 2017-04-26 | Document updated for peer review comments              | skothiya

\section Abbreviations Abbreviation
Abbreviation   | Definition
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
This document describes the technical design description of Train Setup function for AOS BHP.
Document contains the description of design/design changes to implement below mentioned requirements.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix
Req    | Short requirement description                                                                                                             | Description
------ | -------------------------------------------------------------------------------------- ---------------------------------------------------| ------------
AOS2155| The AOS shall store a new Train Setup OR replace an existing Train Setup only in ATP Mode Configuration.                                  | Partially Implemented
AOS2211| The AOS shall update the current stored Train Setup in ATP mode Shunting Route if a TrainSetup message is accepted                        | Not Implemented
AOS2154| The AOS shall update the current stored temporary Train Setup if a TrainSetup message with Q_TS_STATE as permanent is accepted.           | Not Implemented
AOS2156| The AOS shall delete the current Train Setup when entering any of the following ATP modes; Yard, Possession, Shunting Safety Halt........ | Not Implemented


\subsection ExternalInterfaceDescription External Interface Description
Following external interface description is used to implement the requirement described above.
Seq No | Document Name                                      | Version
-------|----------------------------------------------------|----------
1.     | Interface Specification ATP - MMI                  | 1.35
2      | FFFIS TCC-AOS                                      | 5.8

\subsection TrainSetupWorking Train Setup implementation overview: 
       ATP maintain 2 kind of train setup data structure:
        + ATP stores preliminary train setup to hold train configuration which is not negotiated between TCC and Driver and can not be used to drive.
        + ATP stores actual train setup after the negotiation between TCC and driver.

  - Train Setup can be created/updated in below two ways in ATP:
        + During the Registration process, driver will configure the train data from DMI, which will be stored in preliminary train setup.
          TCC shall send confirmation for train configuration data in Train Setup message.
          ATP will store data in train setup on receiving Train Setup message with Q_SETUP = Registration.

        + In case of Reregistration/Reconfiguration TCC shall send Train Setup message with Q_SETUP = Reregistration.\n
          On receiving Train Setup message with Q_SETUP = Reregistration, ATP will store the data in preliminary train setup and will ask driver for the confirmation.
          After receiving confirmation from driver ATP will store the data in train setup.

\subsection Assumption Assumption
  - Creating or updating train setup in registration and reregistration mode will be not covered in scope of this TDD.
    TDD will only cover to provide interfaces to set/get train setup.
    Calling these interfaces in case registration and reregistration scenario will be handle by separate TDD.


\section DetailedDescriptionOfDesign Detailed Description of Components Design/Changes for requirements
  This section describes affected components and function with logic need to add/change to implement the different requirements of Train Setup in ATP.

  \subsection AOS2155 AOS2155
The AOS shall store a new Train Setup OR replace an existing Train Setup only in ATP mode Configuration.\n
\n
  - Affected component :  TSetup 
    + Description of Changes: Rename existing parameter trainTemporarySetupStorage to preTrainSetupStorage.in AbstractTSetup class.
     + Pseudo Code: \n
       //// Preliminary storage of train setup , stores the train setup that is not negotiated with AOS and TCC \n
       TrainSetupStorage preTrainSetupStorage;


\subsection AOS2211 AOS2211
The AOS shall update the current stored Train Setup in ATP mode Shunting Route if a TrainSetup message is accepted.\n
\n
  -  Flow to update Train Setup in ATP mode Shunting Route.
  @image html train_setup_message_flow_in_shunting_route_mode.png
  @image latex train_setup_message_flow_in_shunting_route_mode.png

  - 1  Affected function & component : RadioMessageInTrainSetup::validateMode() & Message Handler
     + Description of Changes: In function validateMode() method of RadioMessageInTrainSetup class, need to add logic to allow TSETUP message in Shunting Route mode.

       + Pseudo Code: \n
               bool RadioMessageInTrainSetup::validateMode() const\n
               {\n
                 bool modeValid = true;\n
                 \n
                 ATPMode mode = AbstractModeControl::corePtr()->getCurrentMode();\n
                 switch (mode)\n
                 {\n
                   case ATPModeConfiguration:\n
                   .\n
                   .\n
                   ////In Shunting Route mode need to allow TSETUP message\n
                   case ATPModeShuntingRoute:\n
                        break;\n
                   .\n
                   .\n
               }

  - 2  Affected function and component :AbstractTSetup::setTrainSetup(const TrainSetup &trainSetup) & TSetup
     + Description of Changes: Add a condition (ATPModeYard == ATPModeShuntingRoute) in setTrainSetup() method of AbstractTSetup class.to allow to update train setup in shunting route mode.\n

       + Pseudo Code: \n
             bool AbstractTSetup::setTrainSetup(const TrainSetup &trainSetup)\n
             {\n
               bool operationSucceed = false;\n
               ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();\n
               Kernel::TrainConfigModeState configModeState =\n
               Kernel::AbstractModeControl::corePtr()->getTrainConfigModeState();\n
               \n
               ////Should only be allowed to write the train setup when \n
               //// AOS in config mode OR shunting route mode\n
               ////train configuration is negotiated between TCC and driver\n
               if (((ATPModeConfiguration == modeType) &&\n
               (configModeState == Kernel::TrainConfigMode::trainConfigWaitSetupBlock)) || (ATPModeShuntingRoute == modeType && trainConfigAccepted))\n
               {\n
                  //// Update train setup\n
               }\n
             

  - 3  Affected component : Mode Control
      + Description of Changes: Need to define new class to handle shunting route mode. 
                                This class will define handlemode() function.
                                


\subsection AOS2154 AOS2154
The AOS shall update the current stored temporary Train Setup if a TrainSetup message with Q_TS_STATE as permanent is accepted.\n
  - 1  Affected component : TSetup
      + Description of Changes: Add a new parameter in TrainSetup class corresponding Q_TS_STATE parameter of TrainSetup message.
        + Pseudo Code: \n
          class TrainSetup\n
          {\n
            public:\n
            VehicleType vehicleType;      //!< Vehicle type of leading locomotive\n
            uint16_t    maxSpeed;         //!< Max allowed speed for this train\n
            uint16_t    brakeResponseTime;//!< Brake response time for this train\n
            uint16_t    brakeAbility;     //!< Brakeability for this train\n
            uint32_t    length;           //!< Length of this train\n
            uint8_t     orientation;      //!< Orientation, see B_DIRECTION\n
            bool        ticAvailable;     //!< TIC available on this train\n
            bool        timsAvailable;    //!< TIMS available on this train\n
            uint16_t    carCount;         //!< Number of cars connected to this train\n
            TrainSetupChangeDetails\n
            changeDetails;    //!< Change details for this train setup
            Kernel::BrakeSystem brakeSystem;      //!< Type of brake system currently active\n
            ATP::TrainState       trainState;    //!< Corresponding to Q_TS_STATE parameter (permanent or temporary)\n
          };

  -2 Affected function and component : AbstractTSetup::isAnyTSetUpFieldChange(const TrainSetup& trainSetup) & TSetup
     + Description of Changes: Need to add a new check in isAnyTSetUpFieldChange() method, to not allow change/update in train setup if trainState (Q_TS_STATE) is already permanent.
       + Pseudo Code: \n
       bool AbstractTSetup::isAnyTSetUpFieldChange(const TrainSetup& trainSetup) const\n
       {\n
       TrainSetup *pTrainSetup = trainSetupStorage.data;\n
       bool anySetupFieldChanged = false;\n
       if ((pTrainSetup != NULL)\n
       && (trainSetup.trainState != TrainState::TrainStatePermanent)\n
       && ((pTrainSetup->brakeAbility != brakeAbilityValue)\n
       || (pTrainSetup->brakeResponseTime != brakeResponseTimeValue)\n
       || (pTrainSetup->carCount != trainSetup.carCount)\n
       || (pTrainSetup->length != trainSetup.length)\n
       || (pTrainSetup->maxSpeed != trainSetup.maxSpeed)\n
       || (pTrainSetup->orientation != trainSetup.orientation)\n
       || (pTrainSetup->ticAvailable != trainSetup.ticAvailable)\n
       || (pTrainSetup->timsAvailable != trainSetup.timsAvailable)\n
       || (pTrainSetup->vehicleType != VehicleGeneralCar))\n
       {\n
       anySetupFieldChanged = true;\n
       }\n
       return anySetupFieldChanged;\n
       }\n

\subsection AOS2156 AOS2156
The AOS shall delete the current Train Setup when entering any of the following ATP modes; Yard, Possession, Shunting, Safety Halt, Sleeping, Power Down OR Unregistration.\n
\n
  - 1 Affected component : TSetup
       + Description of Changes: Need to add a public function to remove.train setup.
         + Pseudo Code: \n
             bool AbstractTSetup::removeTrainSetup(void)\n
             {\n
             bool operationSucceed = true;\n
             ATPMode modeType = Kernel::AbstractModeControl::corePtr()->getCurrentMode();\n
             if ((modeType == ATPModeYard) || (modeType == ATPModePossession) ||\n
             (modeType == ATPModeShunting) || (modeType == ATPModeSafetyHalt) ||\n
             (modeType == ATPModeSleeping) || (modeType == ATPModeUnregistered))\n
             {\n
             trainSetupStorage.invalidate();\n
             trainPreliminarySetupStorage.invalidate();\n

             ///////Removing Car setup\n
             for (uint8_t i = 0U; i < maxCarCount; ++i)\n
             {\n
             carSetupStorage[i].invalidate();\n
             carPreliminarySetupStorage[i].invalidate();\n
             }\n
             }\n
             else\n
             {\n
             operationSucceed = false;\n
             }\n

             return operationSucceed;\n
             }\n

  - 2 Affected function & component : AbstractModeControl::run(void) & Mode control
       + Description of Changes: Need to add logic to call removeTrainSetup() method from run() method of AbstractModeControl class, whenever mode will be changed to below modes
               + Yard Mode
               + Possession Mode
               + Shunting
               + Safety Halt
               + Sleeping
               + Power Down or Unregistration
               
         + Pseudo Code:  Only pseudo code for YardMode is described below, for rest of the above mentioned modes same function call need to be implemented.\n
                 void AbstractModeControl::run(void)\n
                 {\n
                  .\n
                  .\n
                  /////change the mode to yard mode if yard mode button is pressed from DMI\n
                  if((DMICom::AbstractDMIHandler::corePtr()->getDMIButtonStatus() == DMICom::DMIATPEnterYardMode)\n
                  && (Pos::AbstractOdometry::corePtr()->isTrainStandStill()))\n
                  {\n
                   /////update current and previous mode\n
                   previousMode = currentMode;\n
                   currentMode = ATPModeYard;\n
                   /////Removing train setup  \n
                   DS::AbstractTSetup::corePtr()->removeTrainSetup();\n
                  }\n
                 }
*/