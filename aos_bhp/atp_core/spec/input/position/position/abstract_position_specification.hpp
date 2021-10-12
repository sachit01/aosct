namespace ATP::Pos
{

  /**
  \if AsMainPage
  \mainpage Position Component Specification
  @anchor pos
  \endif

  \ifnot AsMainPage
  \class Template
  \endif

  \section Purpose Purpose
  This document specifies the software design for the core part of the Position component. This design is implemented in the class AbstractPosition.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The Position component is responsible for front and rear positions of the train from antenna position estimation.
  The component provides the following functionality:
  - Calculate the estimated position of the AOS vehicle based on input from the Odometry component and balise information from the Decode component.
  - Translate the estimated vehicle position into track id and position within the track for use by other components.
  - Responsible for defining and publishing the current accuracy state of the position
    considering the balise window and slip/slide information.
  - The estimated vehicle position is corrected using an offset calculated by comparing expected and true balise positions.
  - Resetting the balise window on passing an expected balise.

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Position component has dependencies to the following components:
   - Tracks: To provide information about tracks and expected balises.
   - Targets: To provide the travel direction of stored targets.
   - Decode: To provide information about detected balises.
   - Odometry: To provide the ODO direction, odometer reading, slip/slide and balise window information.
   - Message Handler: To provide the information if approximate position message received and validated.
   - Mode Control: To provide the information of the current mode.
   - Event Handler: To provide the interfaces to raise the error events.
   - TSetup: To provide the train configuration parameters like train length and orientation.

   Other components have dependencies to this component because they use its public types
   and methods, see \ref AbstractPosition Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  During initialization, the core component initialize internal variables and cross comparison of vital variables.

  \subsection ModeDependentOperation Mode Dependent Operation
  The Position component is executed in all modes. The actions will differ based on both the general ATP mode and current position accuracy state.

  \subsubsection AccuracyStatesinATPModes Possible Accuracy states in ATP Modes

  The table below indicates the possible accuracy states in different ATP Modes.

  Mode                | Accuracy State |
  --------------------|----|
  ATPModePowerUp       | U |
  ATPModePoweringDown  | U |
  ATPModeConfiguration | U,K |
  ATPModeRegistration  |U,K,A|
  ATPModeBaliseSearch  | U |
  ATPModeNormal        | K|
  ATPModeStaffResponsible | K,A|
  ATPModeLocation      | K |
  ATPModeShunting            | U |
  ATPModeShuntingRoute       | K |
  ATPModeSleeping            | U |
  ATPModePossession          | U |
  ATPModeYard                | U |
  ATPModeSplit           | K |
  ATPModeJoin            | K |
  ATPModeUnregistered        | U |
  ATPModeSafeBrakeToStop     | A,D,U |
  ATPModeSafetyHalt          | U |

  U - Unknown, A - Approximate, K - Known, D - Doubtful


  \subsubsection ActionsAndSupervisionInModes Actions and supervisions in different ATP modes

  The below table indicates the different function related to balise data processing performed in the ATP Modes.

  Mode                       | Check balise id validity | Check balise to be within expected balise window | Check danger for shunting in Yard and Shunting modes | Missed Balise Supervision | Clear Encountered Balise | Possession Mode Balise Handling
  --------------------       |----|----|----|----|----|----|
  ATPModePowerUp             | | | | | | |
  ATPModePoweringDown        | | | | | | |
  ATPModeConfiguration       | | | | |x| |
  ATPModeBaliseSearch        |x| | |x| | |
  ATPModeNormal              |x|x| |x| | |
  ATPModeStaffResponsible    |x|x| |x| | |
  ATPModeLocation            |x|x| |x| | |
  ATPModeShunting            | | |x| |x| |
  ATPModeShuntingRoute       |x|x| |x| | |
  ATPModeSleeping            | | | | | | |
  ATPModePossession          | | | | | |x|
  ATPModeYard                | | |x| |x| |
  ATPModeUnregistered        | | | | | | |
  ATPModeSafeBrakeToStop     |x|x| |x| | |
  ATPModeSafetyHalt          | | | | | | |
  ATPModeSplit               |x|x| |x| | |
  ATPModeJoin                |x|x| |x| | |
  ATPModeRegistration        | | | | |x| |

  x - Indicates the following functions will be executed in those states.

  \subsection Scheduling Scheduling
  The processing in Position component is done in run() function, which is called in each execution cycle.

  \subsubsection run run()
  The class method run() performs the following functions:
  - Update current position of the train based on odometry changes. Refer to \ref PositionCalc for more details. 
  - Update the position accuracy state. Refer to \ref PositionAccuracyState for more details. 
  - Detected balise are checked against the expected balise position and balise window.
    Refer to \ref DetectedBaliseSupervision and \ref ExpectedWindow for more details.
  - Missed Balise Supervision, refer to \ref MissedBaliseSup. 
  - Check danger for shunting in yard and shunting modes, refer to \ref MoveInYard. 
  - Check if the safety margin is crossed and update the accuracy state to Doubtful.
  - Balise handling in Balise Search Mode. Refer to \ref BaliseSearchMode.
  - Checking if the balise is present in the possession balise list. Refer to \ref PossessionMode. 

  Table \ref ActionsAndSupervisionInModes described in which mode the different checks/operation shall be performed.

  \subsubsection PositionCalc Position calculations

  The component translates the odometer position from Odometry component into the valid trackid and positions.
  The translation covers three different values for different positions:
   - Balise antenna position 
   - Nominal front position, according to expected movement direction 
   - Nominal rear position, according to expected movement direction 

  The value received from Odometry specifies the odometer value representing the expected balise antenna position under the vehicle.

  The conversion of balise antenna position to above mentioned positions is calculated using the train configuration read from TSetup
  in the below functions:
  - AbstractPosition::getLocoEndPosOdo()
  - AbstractPosition::getTrailingPosForDirectionOdo()
  - AbstractPosition::getLeadingPosForDirectionOdo()

  \subsubsection PositionAccuracyState Position Accuracy State
  The component uses these states to indicate the accuracy of the calculated position:
   - Unknown
   - Approximate
   - Known
   - Doubtful

  The <b>Unknown</b> state is set at power-up, unregistration, transition to mode Possession or in severe error conditions.
  Transitions from <b>Unknown</b> to <b>Approximate</b> or <b>Known</b> requires manual procedures, like Registration, or a
  TCC message, such as a defined approximate position. 

  The <b>Approximate</b> position is set upon request from TCC which includes valid track data.
  The <b>Approximate</b> state changes to <b>Known</b> state when a valid balise is read by AOS, according to a subsequent MA. 

  The <b>Known</b> state is the main operational state.
  Transitions from <b>Known</b> to other states occur in the below scenarios:
  - Balise handling errors like balise found outside the balise window, etc.
  - Certain ATP mode changes, for instance to SafetyHalt, see \ref AccuracyStatesinATPModes for more details.

  The <b>Doubtful</b> state is used in situations where the detected balise is not in the MA list or balise
  window exceeds the safety margin. The positions will be calculated according to the available MA data. 

  Refer to the table for more details on accuracy in different ATP Modes see \ref AccuracyStatesinATPModes.

  \subsubsection readBalises Read Balises

  Read balise function does the below functions:

  \paragraph DetectedBaliseSupervision Detected Balise Supervision
  Each detected balise will be interpreted and evaluated by the position component to confirm with expected balise data from the MA.
  The checks on detected balises are listed below.

  \paragraph IdValidity Check balise id validity

  Every received balise will be cross checked with expected balise data received from the MA. If the balise is
  not part of the track data a SafeBrakeToStop event will be raised.

  \paragraph ExpectedWindow Check balise to be within expected balise window

  Each received balise shall be checked to determine if received balise position corresponds to the expected position with allowance
  for the current balise window. If a balise is received outside the current balise window and slip indication is active and position is valid,
  then the offset is adjusted accordingly. Refer to \ref CalculationOfOdometerOffset for details on offset calculations and adjustments.

  \paragraph FindNextExpectedbalise Find Next Expected Balise

  When the balise is found or missed, position component will try to find the next expected balise by iterating the balise list in the direction of movement.

  When the previous balise is not found in the list and the next balise is invalid, then the next balise/previous balise is updated as the first balise based on the travel direction.

  \subsubsection ClearEncounteredBalise  Clear Encountered Balise
  This function is used to clear any balise information that are read and it is also frees up queue in Decode. This is typically used in modes that does not read balise
  information. Refer to AbstractPosition::clearEncounteredBalises.

  \subsubsection MoveInYard Check danger for shunting in Yard and Shunting modes

  In yard/shunting mode the detected balise shall be checked whether if the balise is marked as "danger for shunting" then an event shall be reported with an emergency brake. 
  The detected balises can be used to prevent a unregistered train to exit a yard/shunting area.

  \subsubsection MissedBaliseSup Missed Balise Supervision
  Missed balise supervision involves checking for any missed balise.
  The following conditions apply:
   - A single missed balise is allowed, i.e. the balise before and after the missing one is detected at the correct position. 
   - Two missing balises in a row is not allowed, the occurrence shall set the position accuracy state to <b>Doubtful</b> and
     trigger a mode transition to SafeBrakeToStop. 

   Any missing balise will be reported as an event with the baliseId as dynamic text.

  \paragraph MissedBaliseSupMov Missed Balise Supervision on Movement
  The missed balise supervision shall monitor movements and detect if any balise should have been detected as a result of the last position increment.
  The supervision shall only supervise missing balises in position accuracy state <b>Known</b>.

  The Position component will keep track of the previous expected balise and the next expected balise.
  The next expected balise is the one which the train should find next and the one which is reported missed if the odometer reading is passed the threshold to detect it.
  The previous and next expected balise are updated when:
  - A balise is detected. The previous balise is updated to the detected balise and the next balise is found from the balise list.
  - A balise is missed. The previous balise is updated to the missed balise and the next balise is found from the balise list.
  - The odometer direction changes. The previous and next balise id are interchanged.
  - If the next expected balise id is unknown, the balise list is checked to update the next expected balise id.

  @image html missed_balise_on_movement.png "Missed Balise on movement"
  @image latex missed_balise_on_movement.png "Missed Balise on movement"
  

  \paragraph MissedBaliseSupDet Missed Balise Supervision on Detection
  When a balise is detected, the Position component shall check if there were any balises that were missed in between the last detected balise and the current detected balise.
  This is done by iterating over the balise list in the direction of movement from the last detected balise to the current detected balise and checking if there is any intermediate missed balise. 
  

  \subsubsection CalculationOfOdometerOffset Calculation of Odometer Offset
  The odometer offset is used to compensate the slip/slide and accuracy drift between the internal odometer based references values present in balise list and the actual value received from odometry.
  The internal reference system is based on odometer positions and these values do not change, i.e. once a trackid and position reference is stored internally and converted to a calculated odometer value the
  defined odometer value never changes as long as it is stored. Based on detected balise positions, as calculated by the Decode,
  the estimated position of the vehicle may require adjustments due to the slip/slide or other inaccuracies from wayside. This adjustment is collected in the "odometer offset" variable.

  <b>Note:</b> There are two exceptions to the rule that an odometer value calculated based on an trackid and position reference never changes:
  In case of a vehicle/train orientation change due to new train configuration data from TCC or in case of a reset of the odometer offset value due to large values (value range limitations).

  The odometer offset value is adjusted to fit discrepancies thus if a balise X is expected at odoX but is detected at position detectedOdoX the difference (odoOffset = odoX - detectedOdoX) is added to the odometer offset:
  current Odo Position  =  current Odo Position  + odoOffset

  The odometer offset update should be done only for below conditions:
   - Expected balise found inside balise window in position accuracy state is Known. 
   - Expected balise found outside balise window, slip is active and position accuracy state is Known. 
   - Expected balise found and position accuracy state is Approximate. 
   - Expected balise found and position accuracy state is Doubtful. 

  Before the updated position is accepted the following tests shall be performed:
   -  The train front and rear positions shall be within current tracks:
      + If both the front and rear positions are outside of stored tracks then mode set to ATPModeSafetyHalt. 
      + If either the front or the rear position is outside of stored tracks then mode set to ATPModeSafeBrakeToStop. 
   -  The updated train position front and rear should be within current MA.
   -  The updated position shall not indicate any further missed balises refer to \ref MissedBaliseSupDet for more details. 

  \subsubsection BaliseSearchMode Balise Search Mode 

  The Balise search handling varies based on the different submodes.
  - When the Balise Search submode is <b>wait for second balise</b> or <b>search finished successfully</b>:
         + Update the current front and rear position
         + Read the Balises. Refer to \ref readBalises.
         + Check for Missed Balise on Movement. Refer to \ref MissedBaliseSupMov
         + Update the previous and next Balise Information
  - When the Balise search submode is <b>start</b>:
         + Set Position to Unknown at starting of balise search start
  - When the Balise search submode to <b>wait for balise for new registration</b>:
         + If balise is found then Balise Information of first and previous balise is updated.
  - When the Balise search submode is <b>wait for balise for re-registration</b>:
         + If balise is found then check the detected balise is in the MA list 
            - Update the previous and next Balise Information
            - Calculate the odooffset and update the current odo Position with the offset.
            - set the position to Known
         + Raise a SafeBrakeToStop event if the balise is not found.
  - When the Balise search submode is <b>wait for MA</b>:
         + MA is received and Balise is found then odooffset is calculated and update the current odo Position with the offset.
         + Balise is found and the second balise not found before MA then raise a safe brake to stop if the second balise is same as first balise.
         + Service brake event will be raised if more than one balise is detected before any MA is received.


  \subsubsection PossessionMode Possession Mode Balise Handling
  
  The Balise information is retrieved from the Decode component. Every detected balise is checked against the list of possession balises in the Tracks component.
  A Service brake event will be raised if the detected balise is not found in the possession balise list.
   
  \section ClassDiagram Class Diagram
  @image html position_component.png "Abstract Position Class Diagram"
  @image latex position_component.png "Abstract Position Class Diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console Commands
  The console commands defined in AbstractPosition are:
  + expBal: Prints previous and next balise info.
  + pos: Prints odo position, offset, track, position and accuracy. 

  \subsection Analyze Analyze
  The registered measurement data for analysis are
  + antennaTrack: Current antenna Track ID.
  + antennaPos: Current antenna position.
  + accuracyState: Current accuracy state.

  \section CoreAdaptation Core / Adaptation
  The Position component is split in a core and one adaptation part.
  AbstractPosition defines the below pure virtual function that has to be implemented by adaptation.
  + AbstractPosition::getBaliseAirGap() - returns the BaliseAirGap value.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.
  The requirements relevant for this component are:

  Req        | Chapter         | Function
  ---------- | --------------- | --------
  AOS 1799 S |\ref MoveInYard  | AbstractPosition::run()
  AOS 1289   |\ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 71 S   |\ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 77 S   |\ref Scheduling | AbstractPosition::updateCurrentOdoPos()
  AOS 209 S  |\ref PositionAccuracyState   | AbstractPosition::run()
  AOS 1200   |\ref run | AbstractPosition::readBalises()
  AOS 214 S  |\ref ExpectedWindow | AbstractPosition::readBalises()
  AOS 215    |\ref MissedBaliseSupDet | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 216 S  |\ref MissedBaliseSupDet | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 217 S  |\ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 211 S  |\ref Scheduling | AbstractPosition::run()
  AOS 186 S  |\ref Scheduling |AbstractPosition::updateCurrentPos()
  AOS 188 S  |\ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 189 S  |\ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 2391   |\ref Scheduling |AbstractPosition::updateCurrentPos()
  AOS 104    | \ref AccuracyStatesinATPModes | AbstractPosition::run()
  AOS 1305   | \ref Scheduling | AbstractPosition::run()
  AOS 1304   | \ref MissedBaliseSupMov | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 1313   | \ref MissedBaliseSupMov | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 1306   |\ref MissedBaliseSupMov  | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 1307   | \ref Scheduling | AbstractPosition::readBalises()
  AOS 1308   | \ref MissedBaliseSupMov | AbstractPosition::checkMissedBaliseOnDetection(), AbstractPosition::checkMissedBaliseOnMovement()
  AOS 1309   | \ref ExpectedWindow | AbstractPosition::readBalises()
  AOS 2094   | \ref Scheduling | AbstractPosition::readBalises()
  AOS 2390   | \ref DetectedBaliseSupervision |AbstractPosition::updateCurrentOdoPos()
  AOS 1951   | \ref Scheduling | AbstractPosition::readBalises()
  AOS 3273 S | \ref DetectedBaliseSupervision | AbstractPosition::readBalises()
  AOS 1287   | \ref AccuracyStatesinATPModes | AbstractPosition::run()
  AOS 1958   | \ref MissedBaliseSupMov | AbstractPosition::checkMissedBaliseOnDetection()
  AOS 2082   | \ref BaliseSearchMode | AbstractPosition::readBalises()
  AOS 2791   | \ref PossessionMode | AbstractPosition::handlePossession()
  AOS 1799 S | - | -

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].

  Common architectural requirements are specified in SCDS ATP Core
  */
}

