/**
\if AsMainPage
\mainpage LCS Warning Curve Message
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2018-01-16 | First version                                 | skothiya
2.0     | 2018-01-24 | Review comments incorporated                  | skothiya
3.0     | 2018-01-30 | Review comments incorporated                  | skothiya


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
MA             | Movement Authority

\section Introduction Introduction

\subsection Design Design Overview

Investigate the design for implementing ATP warning curve message to be used in AOS-LCS interface. 
"ATP warning curve" message will be send to LCS from ATP and will contain the sample of first warning curve (speed and position).
The message parameters are according to section 4.1.6 in  FFFIS AOS-LCS Ver1.5.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
xx         | xx                                    | xx

\section ChoosenDesign Design Description
This design describes the preparing and sending ATP to send "ATP Warning Curve" message to LCS in ATP.
Design is described in below points:

 - "ATP Warning Curve Message" will be prepared and send to LCS:
      +  When MA will be received (either from scratch or extended) Or
      +  When Most Restrictive Target will be passed
 - "ATP Warning Curve" message will contain the sampled value of first warning curve calculated from the train front to the most restrictive target.
    + The sampled value will use permitted speed (minimum of first warning curve speed or ceiling speed). Permitted speed is already getting calculated in target calculation
      - For the calculation of permitted speed please refer section "1.4.2.7" of SCDS for Brake Calculation component.
      - In current implementation second warning curve speed is taken as permitted speed we need to change it for the first warning curve speed.

\subsection LCSDesign LCS Message Design
- A new class LCSMessageOutWarningCurveData" will be created in VechicleCom component.
    + Class will prepare and send the Warning Curve Message to LCS
    + In collectData() method below described algorithm will be implemented


\subsection SampleCalculation Speed Position Sample Calculation
    + New method will be implemented in "LCSMessageOutWarningCurveData" class (in VechicleCom component).
      This method will implement below logic:
       + This method will calculate the sampled values for first warning curve from train front to the end of most restrictive target.
       + This message will take maximum number (configurable parameter) of sampled points.
       + First sampled value will be the train front (position) and permitted speed at this point.
          - To calculate permitted speed at train front first warning curve will calculated at this point
          - permitted speed will be set to minimum of current ceiling speed or calculated first warning curve speed.
       + Second sample point will be taken at distance where Brake Curve Area (BCA) will start if first sample was not in BCA.
         For the calculation of BCA distance please refer section 1.4.11 section in SCDS_abstract_supervise.
       + Next sample will be calculated when there will be a change in first warning curve speed.
         - To calculate next sample of first warning curve speed a configuration parameter for speed difference to take sample will be used. 
          + Next Sample Speed (Vs) = Warning curve speed at the beginning - configured sample speed difference (Vd) 
           Mentioned in below graphical representation:
           @image html warning_curve_sampling_graph.png
           @image latex warning_curve_sampling_graph.png

    + Then using BrakeCalculations::calcFirstWarningCurveDistance method distance will be calculated by providing sampled first warning curve speed as input.
    + Using the calculated distance position (track,position)will be calculated.
    + Then calculated sampled value of position and speed will be filled in message.
    + Below algorithm will be used to calculate the sampled values:
       @image html warning_curve_message_algo.png
       @image latex warning_curve_message_algo.png

\subsection ConfigParameter Configuration parameter
 - Two New parameters will be added as static config parameter
    + Parameter for sampling difference of speed in cm/s
    + Maximum number of sampling in Warning Curve Message


*/