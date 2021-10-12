/**
\if AsMainPage
\mainpage My New Feature
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-03-10 | First version                                 | rquensel


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system

\section Introduction Introduction
Below changes are proposed as per the doors requirements defined in section 5.3.4.4 and 5.3.4.5.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req        | Short requirement description         | Description
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS 2724        | The AOS shall divide the brake model into three steps corresponding to a range of the train speed. | Not implemented
AOS 2746        | The A and B parameters in the brake model shall be determined based on input data for the brake system type, a safety margin shall be included to handle uncertainty in the input data. | Configuration requirement of parameters.
AOS 2733        | For each brake model (brake system type), AOS shall use the same configuration values for service brake and emergency brake. | Configuration requirement of parameters.
AOS 2753        | When determine the configuration values for a brake model (brake system type), the most restrictive (safe) values of emergency brake and Service brake shall be used. | Configuration requirement of parameters.
AOS 2729        | It shall be possible to configure the number of steps to use in the brake model. | Not implemented
AOS 2726        | The AOS shall have the possibility to handle a minimum of 1 and maximum of 3 different configurations of the brake model (one for each brake system type). | Not implemented
AOS_BHPB 2884   | AOS shall use three brake system types for the respectively vehicle types. | Not implemented
AOS 2728        | Lambda shall be calculated by using the brake weight of vehicles with brake received in the TrainSetup message. The AOS shall use brake weight from brake system in use. | Not implemented
AOS 2735        | When calculating lambda AOS shall use the loading status based on the MovementAuthority message. | Not implemented
AOS 2731        | For each brake system type, AOS shall define "Maximum used lambda" as a configurable variable. The Lambda used in the brake model shall be set to the minimum value of "Maximum used lambda" and the calculated lambda. | Not implemented
AOS 2732        | For each brake system type, AOS shall define "Minimum allowed lambda" as a configurable variable. | Not implemented
AOS_BHPB 2885   | If the configured locomotive type is EMD AND the brake system in use is ECPB, AOS shall compensate lambda. | Not implemented
AOS_BHPB 2886   | The AOS shall recalculate lambda when specific input has changed. | Not implemented
AOS_BHPB 2640   | The AOS shall issue a Brake event if braking if too many cars have non functional brakes.  | Not implemented
AOS_BHPB 2649   | AOS shall issue a Brake event if calculated lambda is below a calculated value. | Not implemented
AOS 2174        | When an MA from scratch is accepted the AOS shall update the current brake ability based on M_LOADED AND N_ADHESION. | Not implemented
AOS_BHPB 2947   | AOS shall use N_ADHESION value of 100 for BHP project irrespective of the value sent in MA. | Not implemented
AOS 2175        | When a track data item target of type Adhesion change is passed the AOS shall update the current brake ability based on the adhesion value (N_VALUE) of the target. | Unplanned
AOS_BHPB 3006   | The AOS shall not update the current brake ability based on the adhesion value (N_VALUE) when a track data item target of type Adhesion change is passed. | Not implemented
AOS 2172        | The AOS shall set the current brake ability and brake delay time to the values defined in the configuration parameters if no valid TrainSetup is present. | Not implemented
AOS 2730        | Brake delay (Td) shall be calculated based on a formula base on length. | Not implemented



\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection AOS_2724 AOS 2724

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- The AOS shall use the following brake model to determine the brake ability (d) that is used in calculating the brake curves and in deceleration supervision. The brake model is divided in three steps. Each formula (step) corresponds to a range of the train speed. A(n), B(n) and V(n) parameters are configured variables.
 -# d = A1 * lambda + B1 , v  <= V1
 -# d = A2 * lambda + B2 , V1 < v  <= V2
 -# d = A3 * lambda + B3 , V2 < v

- Create a class for BrakeAbility in Train Setup. It should be able to get the brake ability for different speeds, the most restrictive brake ability for a speed.
- BrakeAbility values should be cleared when removing the train setup.  Default values should then be used.
 
\subsection AOS_2746 AOS 2746

Affected component  : Configuration values \n
- The A and B parameters in the brake model shall be determined based on input data for the brake system type. When determining the parameters a safety margin shall be included to handle uncertainty in the input data.
- This is done when the configuration values are determined.

\subsection AOS_2733 AOS 2733
Affected component  : Configuration values \n
- For each brake model (brake system type), AOS shall use the same configuration values for service brake and emergency brake.
- This is done when the configuration values are determined.

\subsection AOS_2753 AOS 2753
Affected component  : Configuration values \n
- When determine the configuration values for a brake model (brake system type), the most restrictive (safe) values of emergency brake and Service brake shall be used.
- This is done when the configuration values are determined.

\subsection AOS_2729 AOS 2729
Affected component  : Configuration values \n
- It shall be possible to configure the number of steps to use in the brake model. If the upper bound of the speed range is set to zero it means that there is no upper bound for that step and the next steps shall not be used.
- This is done when the configuration values are determined.
 
 
\subsection AOS_2726 AOS 2726

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- The AOS shall have the possibility to handle a minimum of 1 and maximum of 3 different configurations of the brake model (one for each brake system type). The brake weights for each brake system types are defined as type 1 to type 3 in the TrainSetup message, VEHICLE_TYPE_DATA block, from TCC. 


\subsection AOS_BHPB_2884 AOS_BHPB 2884

Affected component  : Train Setup \n
Affected function : BrakeAbility class \n

- AOS shall use the following brake system types for the respectively vehicle type:
 -#	 EMD: Brake system type 1, if the stored brake system in use is Pneumatic, and Brake system type 2 if the stored brake system in use is ECPB
 -#  TMM:  Brake system type 1 
 -#	 Hi-Rail:  Brake system type 1 

\subsection AOS_2728 AOS 2728

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- Lambda shall be calculated by dividing the "sum of brake weight of vehicles with brake" by the "dynamic weight of the train" received in the TrainSetup message, VEHICLE_TYPE_DATA block from TCC. The AOS shall use brake weight from brake system in use.

\subsection AOS_2735 AOS 2735

Affected component  : Train Setup, Message Handler \n
Affected function : Abstract BrakeAbility class, RadioMessageInTrainSetup::publishData() \n

- When calculating lambda, AOS shall use the loading status based on the MovementAuthority message, M_LOADED field from TCC:
 -# When train is loaded, the "Brake weight loaded" and "Dynamic weight loaded" in the TrainSetup message, VEHICLE_TYPE_DATA block from TCC shall be used.
 -# When the train is empty the "Brake weight empty" and the "Dynamic weight empty" in the TrainSetup message, VEHICLE_TYPE_DATA block from TCC shall be used.
 -# If loading status is not defined, lambda shall be calculated based on loaded values.

\subsection AOS_2731 AOS 2731

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- For each brake system type, AOS shall define "Maximum used lambda" as a configurable variable. The Lambda used in the brake model shall be set to the minimum value of "Maximum used lambda" and the calculated lambda. 

\subsection AOS_2732 AOS 2732

Affected component  : Train Setup, Message Handler, DMI Handler \n
Affected function : Abstract BrakeAbility class, RadioMessageInTrainSetup::publishData() \n

- For each brake system type, AOS shall define "Minimum allowed lambda" as a configurable variable.
If the calculated lambda, based on TrainSetup message, VEHICLE_TYPE_DATA block from TCC, is lower than "Minimum allowed lambda" AOS shall:
 -# Send a MessageAcknowledge message with Q_MESSAGE_STATUS field set to Message not accepted, AND
 -# Inform the driver, AND
 -# Issue a Safe Brake To Stop event
- Lambda for both empty and loaded brake weights shall be checked.



\subsection AOS_BHPB_2885 AOS_BHPB 2885

Affected component  : Train Setup, Message Handler \n
Affected function : BrakeAbility class \n

- If the configured locomotive type is EMD AND the brake system in use is ECPB, AOS shall compensate lambda with regards to the percentage of cars with functional brakes. Only the brake weight of the cars with functional brakes shall be considered in lambda calculation. 
- Percentage of cars with functional brakes is the minimum value of the following parameters:
 -# Percentage of operative brakes, ECPB, reported in Train Status Message on the LCS interface"
 -# P_ECPBBRAKESUSED which is received in the ConfigurationData message from TCC.

- The lambda calculation is done by: ("brake weight of the locomotive" + "sum of the brake weight of all cars" * "minimum percentage of 1 and 2") / "dynamic weight of the locomotive + sum of dynamic weight of all cars"
 
 \subsection AOS_BHPB_2886 AOS_BHPB 2886

Affected component  : Train Setup, Message Handler \n
Affected function : BrakeAbility class \n

- The AOS shall recalculate lambda if:
 -# The configured locomotive type is EMD AND
 -# The current brake system in use is ECPB AND
 -# Percentage of operative brakes, ECPB, from LCS is updated AND 
 -# The Percentage value is higher than P_ECPBBRAKESREQUIRED received in the ConfigurationData message from TCC


\subsection AOS_BHPB_2640 AOS_BHPB 2640

Affected component  : Train Setup \n
Affected function : BrakeAbility class \n

- The AOS shall issue a Brake event if:
 -# The configured locomotive type is EMD AND
 -# The current brake system in use is ECPB AND
 -# The percentage of cars with functional brakes reported inTrain Status Message on the LCS interface is below a configurable value.


\subsection AOS_BHPB_2649 AOS_BHPB 2649

Affected component  : Train Setup \n
Affected function : BrakeAbility class \n

- The AOS shall issue a Brake event if:
 -# The configured locomotive type is EMD AND
 -# The current brake system in use is ECPB AND
 -# The percentage of cars with functional brakes reported in Train Status Message on the LCS interface results in a calculated lambda which is below "Minimum allowed lambda".

\subsection AOS_2174 AOS 2174

Affected component  : Train Setup, Message Handler \n
Affected function : Abstract BrakeAbility class \n


- When an MA from scratch is accepted the AOS shall update the current brake ability based on:
 -# M_LOADED AND
 -# N_ADHESION.
- If these field variables are updated in an extension MA they shall be ignored.


\subsection AOS_BHPB_2947 AOS_BHPB 2947

Affected component  : Train Setup \n
Affected function : BrakeAbility class \n

- AOS shall use N_ADHESION value of 100 for BHP project irrespective of the value sent in MA.


\subsection AOS_BHPB_3006 AOS_BHPB 3006

Affected component  : Train Setup \n
Affected function : BrakeAbility class \n

- The AOS shall not update the current brake ability based on the adhesion value (N_VALUE) when a track data item target of type Adhesion change is passed.

 
\subsection AOS_2172 AOS 2172

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- The AOS shall set the current brake ability and brake delay time to the values defined in the configuration parameters if no valid TrainSetup is present.
 
 
 
\subsection AOS_2730 AOS 2730

Affected component  : Train Setup \n
Affected function : Abstract BrakeAbility class \n

- Brake delay (Td) shall be calculated based on the following formula. In the formula t1, t2 and t3 are configured parameters: 
Td = t1 + t2*L + t3*L2
 
 
\section AdditionalMaterial Additional Material

<Anything not covered in other sections can be added here.>

*/