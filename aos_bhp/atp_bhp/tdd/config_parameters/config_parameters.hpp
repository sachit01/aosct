/**
\if AsMainPage
\mainpage Configuration Parameters
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.2     | 2017-12-20 | Configuration Parameters                  | prsrivas


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | Automatic Train Protection Onboard System
ATO            | Automatic Train Operation
EB             | Emergency Brake
BP1            | Brake Pressure 1 
BP2            | Brake Pressure 2

\section Introduction Introduction
This document describes the requirements and implementation for Configuration Parameters needed for AOS changes. \n
Config component provides access to the stored configuration parameters in each unit. Each config parameter is assigned with Ids, 
min/max operating range, operating unit, default values and memory area configuration. \n 
The Configuration parameters are categorised into 3 types:
- Maintenance parameters
- Configuration parameters
- Runtime parameters

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
The below requirements are need to be implemented for Configuration Parameters Changes, as defined in the Doors module SSRS_AOS_BHPB_Dev_Test under section 5.4.3.

Req Id    | Requirement description                                                                                        | Description
----------| -------------------------------------------------------------------------------------------------------------- |------------- 
AOS 1671  | The AOS shall allow configuration towards one or two cabin. | Not Implemented
AOS 2605  | The AOS shall be configurable regarding the locomotive type. | Not Implemented
AOS 2301  | The AOS shall provide following configuration parameters for service and emergency brake deceleration supervision: Percentage of expected deceleration below which the deceleration is not sufficient, and Time limit for escalation of Service/Emergency Brake application if deceleration is not sufficient. | Not Implemented
AOS 2690  | It shall be possible to configure if the AOS shall read the ATO Mode inputs or not. | Not Implemented
AOS 2694  | It shall be possible to configure if the AOS shall read the Emergency Stop Active (EMS) input or not. | Not Implemented
AOS 2714  | It shall be possible to configure the AOS whether or not the vehicle allows access to Service Brake. | Not Implemented
AOS_BHPB 2643 | It shall be possible to configure the minimum percentage of cars with functional brakes on an ECPB equipped train. | Not Implemented
AOS_BHPB 2681 S | The AOS shall be configurable regarding the use of each of the following input signals EB cut-out, Rail/Road. | Not Implemented
AOS_BHPB 2773 S | It shall be possible to configure the evaluation time for comparing the EB feedback input signals (BP1 and BP2). | Not Implemented
AOS_BHPB 2829 S | The AOS shall be configurable regarding the use of the following input signal: Non-leading Locomotive.   | Not Implemented
AOS_BHPB 2858 | It shall be possible to configure the lowest allowed  brake pipe pressure in the locomotive for considering the train as intact. | Not Implemented
AOS_BHPB 2859 | It shall be possible to configure the lowest allowed brake pipe pressure at the last car for considering the train as intact. | Not Implemented


\subsection Design Design Overview
The Config component requirements are categorised into Core and Adaptation part. 
Following is the list of functions implemented for the "Core" and "Adaptation" parts of the code respectively.
- Core::
  - getSbExpectedDecLimit() 
  - getEbExpectedDecLimit()
  - getSbTimeoutExpDec()
  - getEbTimeoutExpDec()
  - getLocoType()
  - getcabinConfiguration()
  - getAtoSwitchInputsAvail()
  - getESAInputAvail()
  - getSbAvail()

- Adaptation::
  - getRailRoadInputAvail()
  - getMaxEbFbDiffTimeout() 
  - getECPBMinPercBrakeCars()
  - getNonLeadingLocoInput()
  - getTimsMinBPLoco()
  - getTimsMinBPLastCar()

\subsection ParametersTable Parameters Table
Below table describes the details of the configuration parameters need to be implemented for Core and Adaptation of the Config component:

Config ID | Name                | Description                             | Unit     |  Min Value | Max value    | Default Value  |  Access function |  
----------|---------------------|-----------------------------------------|----------|----------  |--------------|----------------| -----------------|
179 |sbExpectedDecLimit |Percentage of expected deceleration below which deceleration is not sufficient for Service Brake application | % | 1 | 100 | 60 | getSbExpectedDecLimit() | 
180 |ebExpectedDecLimit |Percentage of expected deceleration below which deceleration is not sufficient for Emergency Brake application| % | 1 | 100 | 60 | getEbExpectedDecLimit() |
181 |ebTimeoutExpDec   |Time limit for escalation of Emergency brake when deceleration is not sufficient | s | 0 | 255 | 10 | getSbTimeoutExpDec() |
182 |sbTimeoutExpDec   |Time limit for escalation of Service brake when deceleration is not sufficient  | s | 0 | 255 | 10 | getEbTimeoutExpDec() |
183 |locoType          |Locomotive type           | nr | 0 | 255 | 50 |  getLocoType() |
184 |cabinConfiguration |Configuration towards one or two cabins | nr | 1 | 2 | 2 | getcabinConfiguration() |
185 |atoSwitchInputsAvail |ATO switch inputs available | bool | false | true | false| getAtoSwitchInputsAvail()  |
186 |esaInputAvail    |Emergency Stop input available | bool | false | true | false| getESAInputAvail() |
187 |sbAvail | Service Brake access available |  bool | false | true | false| getSbAvail() |
310 |railRoadInputAvail |Rail/Road input available | bool | false |  true | false| getRailRoadInputAvail() | 
312 |maxEbFbDiffTimeout | Evaluation time for comparing EB feedback input signals BP1 and BP2 | s | 0 | 20 | 5 | getMaxEbFbDiffTimeout() |
313 |ecpbMinPercBrakeCars |Minimum percentage of cars with functional brakes on ECPB equipped train | % | 0 | 100 | 60 | getECPBMinPercBrakeCars() |
314 |nonLeadingLocoInput | Input signal for Non leading Locomotive should be used or not | bool | false | true | false | getNonLeadingLocoInput() |
315 |timsMinBPLoco | Lowest allowed brake pipe pressure in locomotive to consider the train as intact | kPa | 0 | 1000 | 500 |  getTimsMinBPLoco() |
316 |timsMinBPLastCar | Lowest allowed brake pipe pressure in locomotive to consider the train as intact | kPa | 0 | 1000 | 500 | getTimsMinBPLastCar() |
*/