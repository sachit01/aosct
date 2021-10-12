/**
\if AsMainPage
\mainpage Movement Authority(section (6.3.22.3))
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-05-22 | Proposed Changes for MA requirements          | spandita


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TODO           | To be discussed

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 6.3.22.3 for movement authority.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 2031 |The AOS shall only accept an MA in the following ATP modes registration, balise search, normal, staff responsible, shunting route, location and reject in all other modes. | Partially Implemented
AOS 2032 | Any received MA shall be checked for consistency. The MA shall be rejected if it’s not consistent. The MA is consistent if  The TRACK_DATA in the MA are continuous and have same direction as the MA, and  | -
     -    | NID_TRACK and D_POSITION defined in the MA represents a valid position in the tracks defined in TRACK_DATA block of MA or the track data stored in AOS, and  | -
     -    | all NID_TRACK and D_POSITION defined in the MA are in between the start of MA position and the end of MA position, start/end positions included and end position of MA is ahead of the start position of MA in the travel direction and | -
     -    | the TRACK_DATA in MA is consistent with the track data stored in AOS and the BALISE_DATA in MA is consistent with the balise data stored in AOS. | Partially Implemented
AOS 2033 | When in train state Idling the AOS shall accept an MA from scratch if the MA covers the train footprint. | Partially Implemented
AOS 2263 | If the AOS is connected to more than one TCC, it shall accept an MA from scratch with the PARTLY_MA block if the AOS is in ATP mode Normal, Staff Responsible or Shunting Route and in train state Idling, or  | -
        -|  ATP mode Registration AND following Re-registration procedure. | Not Implemented
AOS 2264 | The AOS shall accept an MA extension in train state Idling if MA from scratch with PARTLY_MA block has been accepted and the new MA extends the MA from scratch and the combined MA covers the train footprint. | Not Implemented
AOS 2034 | The MA shall be accepted in Balise Search mode if the MA has Q_ROUTE_TYPE of “Normal”, and AOS is following Registration procedure and has detected the first balise and no other MA has been accepted in “Balise Search” mode and  MA contains in the BALISE_DATA the id of the first passed balise.If the MA does not fulfill the content above a Safe Brake To Stop event shall be triggered. |Partially Implemented
AOS 2035 | An MA shall be accepted in Registration mode if AOS is following Reposition procedure AND the MA has Q_ROUTE_TYPE of Staff Responsible AND Approximate Position message has been received OR The AOS is following Re-registration procedure AND the MA has Q_ROUTE_TYPE of Re-registration. | Partially Implemented
AOS 2036 | In Staff Responsible mode if the position is Known, the AOS shall accept an MA of  Q_ROUTE_TYPE Normal if Train is NOT Idling state AND the MA extends the Staff responsible target or train is in Idling state. | Not Implemented
AOS 2450 | In Staff Responsible mode, the AOS shall accept an MA of Q_ROUTE_TYPE Staff Responsible only when the train is in Idling state. | Not Implemented
AOS 2037 | The AOS shall accept an MA when train state is Idling in Normal mode with any of the following Q_ROUTE_TYPE's Normal, Split, Join, Shunting Route, Staff Responsible. | Partially Implemented
AOS 2038 | The AOS shall in Normal mode accept an MA of Q_ROUTE_TYPE of Normal when not in train state Idling if the MA extends the current active Normal target. | Partially Implemented
AOS 2194  | The AOS shall in Normal mode accept an MA of Q_ROUTE_TYPE of Location End when not in train state Idling if the position of the Location End is the same as the current active Normal target, | -
    -    | the direction of Location End MA is the same as the current active Normal target and   | -
     -   | A Normal MA with KEEP_TRACK_DATA block has been received. | Partially Implemented
AOS 2203 | In Normal mode if a Primary target of type Location End exists, the AOS shall only accept an MA of Q_ROUTE_TYPE of Location Start. | Not implement
AOS 2134  | The AOS shall accept an MA of Q_ROUTE_TYPE of Location Start when not in train state Idling if target of type “Location End” exists, the direction of MA is opposite to the direction of “Location End” target,| -
  -        | the position of Location End target is after the Location Start in the direction of Location End target and | - 
 -          |the Location area is big enough to accommodate the train length. | Not Implemented
AOS 2039 | In Shunting Route mode, the AOS shall accept an MA of Q_ROUTE_TYPE of Shunting Route if The train state is Idling OR The MA extends the current active Shunting Route target. | Not Implemented
AOS 2040 | In Location mode, an MA which expands the Location area boundary shall be accepted if the Q_ROUTE_TYPE of MA is Location End or Location Start and the direction of MA is consistent with the Location start and Location End Targets. | Not Implemented
AOS 2041  | In Location mode, an MA which shrinks the Location area boundary shall be accepted if the Q_ROUTE_TYPE of MA is Location End or Location Start, the direction of MA is consistent with the Location Start and Location End Targets and | -
    -     |  the train footprint is inside the reduced Location area boundary and  the train is not moving towards the boundary being reduced | Not Implemented
AOS 2433  | The AOS shall raise a Brake event when an MA of Q_ROUTE_TYPE Unconditional Shortening of MA is received in ATP mode Location,Split,Join and  delete all targets at standstill. | Not Implemented
AOS 2434   |The AOS shall raise a SafeBrakeToStop event when an MA of Q_ROUTE_TYPE Unconditional Shortening of MA is received in ATP mode Balise Search. | Not Implemented
AOS 2435   | If an MA of Q_ROUTE_TYPE Unconditional Shortening of MA is received AND the MA end is within the distance required by the train to stop with SB application, the AOS shall raise a Brake event and delete all targets at standstill. | Not Implemented

 \subsection ExternalInterfaceDescription External Interface Description
 Following external interface description is used to implement the requirement described above.
 Seq No | Document Name                                      | Version
 -------|----------------------------------------------------|----------
1.      | FFFIS TCC-AOS                                      | 5.8

\section DetailedDescriptionOfComponents Detailed Description of Components

To adapt the some requirements we need to create a new virtual function in abstract mode class which need to be inherited in the derived classes.
Refer \ref class_diagram
\subsection class_diagram Class diagram
@image html maclass_diagram.png
@image latex maclass_diagram.png

Call to these virtual functions will be done via new public function named as isValidQRouteType of abstract mode control class. \n
Code Snippet: \n
modeList[currentMode]->isValidQRouteType() ; \n

\subsection AOS2031 AOS2031
Affected component  : Message Handler \n
Affected Function : validateMode() of RadioMessageInMovementAuthority class \n
Description: Set the modeValid variable to true in registration, balise search, normal, staff responsible, shunting route, location ATP modes and reset in all other modes.

\subsection  AOS2032 AOS2032
Affected components  : Message Handler  \n
Affected Function : validatePrimaryTargetData() of RadioMessageInMovementAuthority class. \n
- In validatePrimaryTargetData check whether the end position of MA is greater than the start position of MA in travel direction.
- Check if the start of MA track is same as the first track provided by TRACK_DATA of MA.
- Check also maHead start position is in between the distanceLeg0 and distanceLeg1 of first track.
- If above condition is true, Set the primaryTagetDataValid to true.

\subsection  AOS2033 AOS2033
Affected component  : Message Handler \n
Affected Function : validatePrimaryTargetData() function of RadioMessageInMovementAuthority class.\n
- Check for idling state by using the AbstractModeControl::corePtr()->isIdleState() function.
- If true, Check whether the rear and front end position of train lies in between of start and end position of MA(ie D_POSITION in received MA).
- Rear End and Front End position of train will be received via public function getCurrRearPos and getSafeFrontPos of position component.
- If yes ,Set the primaryTagetDataValid to true.


\subsection  AOS2263 AOS2263
Affected  component  : Message Handler \n
Affected Function : validateMode() ,publishPrimaryTarget(),remove functions of abstract tracks & abstract targets of RadioMessageInMovementAuthority class. \n
- Create private flag isPartlyMaRecieved, prevPartlyMAStatus and maHead variable as maPartlyHead in RadioMessageInMovementAuthority class.
- Create getter function for isPartlyMaRecieved flag in abstract message handler.
- Create a new private member numOfTCCConnected and its getter function in abstract radio handler to return the number of TCC connected.
- In validateMode() validate the mode refer \ref AOS2031 for more details.
- Copy the value of isPartlyMaRecieved to prevPartlyMAStatus and set the isPartlyMaRecieved flag in the partly MA switch statement case of nextMsgIdentifier variable in parseMessageData function if PARTLY_MA field is present in received MA and numOfTCCConnected is two else reset the isPartlyMaRecieved flag.
- Status of numOfTCCConnected will be get from its getter function in abstract Radio handler.
- In validateTrackData() function put the check for prevPartlyMAStatus is set with the idle condition check refer below code snippet for more detail. 
- code snippet: \n
if (AbstractModeControl::corePtr()->isIdleState() && prevPartlyMAStatus ) \n
{ \n
if (maData.trackDataVec.begin()->previousTrack != 0U) \n
{ \n
} \n
} \n
- In validateTrackData() function check if the isPartlyMaRecieved and prevPartlyMAStatus is set.
- If true, skip the check for the train foot print.
- Follow the validation procedure for the balise and target data as per the defined validation requirements.
- In publishPrimaryTarget function check if isPartlyMaRecieved is set.
- If true, Copy the data from maHead to maPartlyHead else publish the primary target as per the other publish requirements.
- Code Snippet:\n
if(isPartlyMaRecieved) \n
{ \n
  maPartlyHead =  maHead; \n
  publishPrimaryTargetValid = true; \n
} \n
else \n
{ \n
\\Publish primary target as per publish requirements \n
publishPrimaryTargetValid = true; \n
} \n
- Need to check the isPartlyMaRecieved flag status in remove functions of abstract tracks & abstract targets by using the getter function in abstract message handler.
- If isPartlyMaRecieved flag is set do not delete any targets,track & balise.

\subsection  AOS2264 AOS2264
Affected  component  : Message Handler \n
Affected Function : validatePrimaryTargetData(),publishPrimaryTarget() of RadioMessageInMovementAuthority class.
- Create private flag isPartlyMaRecieved, prevPartlyMAStatus and maHead variable as maPartlyHead in RadioMessageInMovementAuthority class.
- Create getter function for isPartlyMaRecieved flag in abstract message handler.
- In validateMode() validate the mode refer \ref AOS2032 for more details.
- Copy the value of isPartlyMaRecieved to prevPartlyMAStatus and set the isPartlyMaRecieved flag in the partly MA switch statement case of nextMsgIdentifier variable in parseMessageData function if PARTLY_MA field is present in received MA else reset the isPartlyMaRecieved flag.
- In validateTrackData() function put the check for prevPartlyMAStatus is set with the idle condition check, refer below code snippet for more detail.
- code snippet: \n
if (AbstractModeControl::corePtr()->isIdleState() && prevPartlyMAStatus ) \n
{ \n
if (maData.trackDataVec.begin()->previousTrack != 0U) \n
{ \n
} \n
} \n
- In validateTrackData() function check if the isPartlyMaRecieved and prevPartlyMAStatus is set.
- If true, skip the check for the train foot print.
- In validatePrimaryTargetData() function check if prevPartlyMAStatus is set .
- If true, check if position of endOfMATrackAndPos in maPartlyHead is less than the received endOfMATrackAndPos of maHead.
- Also need to check the Train front position and end position is in between of startOfMATrackAndPos of maPartlyHead and received endOfMATrackAndPos of maHead.
- If true ,Set the primaryTagetDataValid valid. 
- If all validation procedure passes, publish the maHead  as primary target to target by using addTarget function of target  and reset the maPartlyHead values.

\subsection  AOS2034 AOS2034
Affected  component: Message Handler,Mode Control(balise Search) \n
Affected Function: publishTracks,validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in balise search mode refer \ref balise_search_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in balise search switch mode statement.
- If isValidQRouteType() returns true, Check for presence of first balise in the received MA by iterating the baliseDataVec vector of RadioMessageInMovementAuthority class in the publishTracks() function(already present in the code).
- If yes, Accept the MA otherwise raise Safe brake to stop event.

\subsection balise_search_diagram Balise Search diagram
@image html balise_search.png
@image latex balise_search.png

\subsection  AOS2035 AOS2035
Affected  component: Message Handler,mode Control(registration mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority   
- Create isValidQRouteType() function in registration mode refer \ref registration_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in registration switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.

\subsection registration_diagram Registration diagram
@image html registration.png
@image latex registration.png

\subsection  AOS2036 AOS2036
Affected  component: Message Handler,mode Control(Staff Responsible Mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in Staff responsible mode refer \ref staffresponsible_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in Staff responsible switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- MA Extension will be checked in the validatePrimaryTargetData function as per the requirements defined in \ref AOS2032
- If validatePrimaryTargetData function returns true, publish the primary target,tracks and balises. 

\subsection staffresponsible_diagram Staff Responsible diagram
@image html staffresponsible.png
@image latex staffresponsible.png

\subsection  AOS2450 AOS2450
Affected  component: Message Handler,mode Control(Staff Responsible Mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in Staff responsible mode refer \ref staffresponsible_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in Staff responsible switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- MA from scratch will be checked and added in the validatePrimaryTargetData function as per the requirements defined in \ref AOS2032


\subsection  AOS2037 AOS2037
Affected  component: Message Handler,mode Control(Normal Mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.

\subsection normal_diagram Normal diagram
@image html normal.png
@image latex normal.png

\subsection  AOS2038 AOS2038
Affected  component: Message Handler,mode Control(Normal Mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- MA Extension will be checked in the validatePrimaryTargetData function as per the requirements defined in \ref AOS2032
- If validatePrimaryTargetData function returns true, publish the primary target,tracks and balises.

\subsection  AOS2194 AOS2194
Affected  component: Message Handler,mode Control(Normal Mode) \n
Affected Function: validateMode &  validatePrimaryTargetData. function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- Call getKeepTrackDataRec() of new target(will be done by Sunil as he is doing keep track data TDD) in validatePrimaryTargetData
- If true, Check if the location end and direction of new MA is same as current active normal target.
- If yes, Set the primaryTagetDataValid to valid.

\subsection  AOS2203 AOS2203
Affected  component: Message Handler,mode Control(Normal Mode) \n
Affected Function:  validateMode function of RadioMessageInMovementAuthority
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.

\subsection  AOS2134 AOS2134
Affected  component: Message Handler \n
Affected Function:  validatePrimaryTargetData,validateTrackData and isValidQRouteType function of RadioMessageInMovementAuthority and mode control.
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- In validatePrimaryTargetData function check the direction of new MA should be opposite to direction of stored primary target that is location end type.
- If yes, Check the position of location end target is greater the location start in the direction of location end target.
- Get the front and rear end of train by using position public function in validateTrackData function.
- Check the front and rear end of train is in between of tracks provided by MA.
- Set the trackDataValid to true.
- Pass the primary target object to addTarget function of targets in publishPrimaryTarget function.

\subsection  AOS2039 AOS2039
Affected  component: Message Handler,mode Control(Shunting Route Mode) \n
Affected Function: validateMode  function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in shunting route mode refer \ref shunting_route for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in shunting route switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- MA will be checked in the validatePrimaryTargetData function as per the requirements defined in \ref AOS2032
- If validatePrimaryTargetData function returns true, publish the primary target,tracks and balises.

\subsection shunting_route Shunting Route diagram
@image html shunting_route.png
@image latex shunting_route.png

\subsection  AOS2040 AOS2040
Affected  component: Message Handler \n
Affected Function: validateMode & validatePrimaryTargetData  function of RadioMessageInMovementAuthority
- Create isValidQRouteType() function in location mode refer \ref location_mode for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in location switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- if validateMode returns true, Check the direction of MA is same as already stored location start and location end target in validatePrimaryTargetData function.
- If yes, set the primaryTagetDataValid to true.

\subsection location_mode Location Mode diagram
@image html location_mode.png
@image latex location_mode.png

\subsection  AOS2041 AOS2041
Affected  component: Message Handler \n
Affected Function: validatePrimaryTargetData  function of RadioMessageInMovementAuthority
- Create isValidQRouteType() function in normal mode refer \ref normal_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in normal switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.
- If true, Check the direction of MA is same as already stored same target in validatePrimaryTargetData function.
- If true, Check whether the rear and front end position of train lies in between of new location start/end boundary and already other type stored target.
- Rear End and Front End position of train will be received via public function getCurrRearPos and getSafeFrontPos of position component.
- Check whether train is not moving towards the new location start/end target.
- Upon successful operation of above steps set the primaryTagetDataValid to true.   

\subsection  AOS2433 AOS2433
Affected  component: Message Handler \n
Affected Function: handleMode function of Mode control, run function of abstract targets 
- Create a new private member in abstract targets as delTargetAtStandStill .
- Create the setter function in abstract targets as delTargetAtStandStill
- Check in run() of abstract targets component whether the delTargetAtStandStill is set.
- If set and train is at standstill call removeAll() function of abstract targets component.

\subsection  AOS2434 AOS2434
Affected  component: Message Handler,mode Control(Balise search Mode) \n
Affected Function: validateMode function of RadioMessageInMovementAuthority 
- Create isValidQRouteType() function in balise search mode refer \ref balise_search_diagram for more details.
- Call to isValidQRouteType() function need to done in validateMode function of message handler in balise search switch mode statement.
- Set the modeValid to true if isValidQRouteType() returns true.

\subsection  AOS2435 AOS2435
Affected  component: Message Handler,mode Control \n
Affected Function: validateMode function of RadioMessageInMovementAuthority
- Check the Q_ROUTE_TYPE is equal to  unconditional shortening of MA in the Normal,shunting Route,Staff responsible mode switch statement.
- If Yes, call getDistanceToBCA  function of supervise component and compare it with MA end .
- If the MA end is small as compared to value provided by getDistanceToBCA of supervise component, raise the SB brake event with delTargetAtStandStill field to true.
- If the MA end is greater then delete the already saved primary target and publish the MA as primary target in publishPrimaryTarget() function, refer AOS 2436 for more details.

\section AdditionalMaterial Additional Material

*/