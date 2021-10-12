/**
\if AsMainPage
\mainpage TIMS BHP Adaptation
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-12-22 | First version                                 | marlundg


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description


\section References References

Reference      | Document
-------------- | ------------------------------------------------------------------
1              | Train Integrity Algorithm Design Doc (3NSS015103D0205) Ver 1.2


\section Introduction Introduction

BHP adaptation of TIMS.

\subsection Design Design Overview

Investigate the design for TIMS BHP Adaptation in product and test simulation.


\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req        | Short requirement description         | Justification
---------- | ------------------------------------- | ------------------------------------------------------------------
AOS_BHPB 2850 | The AOS shall assess train integrity via GPS input according to the algorithm described in ref. [105]. | Not Implemented
AOS_BHPB 2853 | If ECPB system is used and configuration allows the ECPB train integrity input to be used, the AOS shall confirm train integrity, each time the following conditions are fulfilled: ECPB train Integrity confirmation is received from the vehicle, AND A valid brake pressure level for the vehicle received from all used input ports (BP1 and/or BP2 depending on configuration)  >=  The configured brake pressure limit for considering the train as intact.| Not Implemented
AOS_BHPB 2854 | The AOS shall set train integrity status to 'broken' if the brake pressure level for the vehicle received from input port  BP1 or BP2  < The configured minimum brake pressure limit for considering the train as intact.| Not Implemented
AOS_BHPB 2855 | If the brake system in use is Pneumatic the AOS shall set train integrity status to 'broken' if the train integrity assessed via GPS input is considered as 'broken'. | Not Implemented
AOS_BHPB 2852 (Draft) | NOT ECPB and valid GPS AND Valid Vehicle BP AND Valid Last Car BP | Not Implemented
AOS_BHPB 2867 (Draft) | Ref: AOS_BHPB 2843 For new train configurations, the AOS shall inform the TCC in the StartUp message that TIMS is available if the following conditions are fulfilled... | Not Implemented
AOS_BHPB 2866 (Draft) | Ref: AOS_BHPB 2844 For existing train configurations, the AOS shall inform the TCC in the StartUp message that TIMS is available if the following conditions are fulfilled..| Not Implemented
AOS_BHPB 2883 (PeerReviewed) | It shall be possible to configure if the ECPB train integrity input is allowed to be used when evaluating train integrity.  | Not Implemented
AOS_BHPB 2882 (PeerReviewed) | It shall be possible to configure the lowest allowed  brake pipe pressure in the locomotive for considering the train as intact.  | Not Implemented
AOS_BHPB 2881 (PeerReviewed) | It shall be possible to configure the lowest allowed brake pipe pressure at the last car for considering the train as intact. | Not Implemented
AOS_BHPB 2851 (Draft) | Ref GPS |  Not Implemented
AOS_BHPB 2857 (Draft) | It shall be possible to configure if the ECPB train integrity input is allowed to be used when evaluating train integrity. | Not Implemented
AOS_BHPB 2858 | It shall be possible to configure the lowest allowed  brake pipe pressure in the locomotive for considering the train as intact. | Not Implemented
AOS_BHPB 2859 | It shall be possible to configure the lowest allowed brake pipe pressure at the last car for considering the train as intact. | Not Implemented
AOS_BHPB 2860 | It shall be possible to configure the GPS accuracy safety margin. | Not Implemented
AOS_BHPB 2861 | It shall be possible to configure the maximum number of GPS map reference points. | Not Implemented
AOS_BHPB 2862 (Draft) | It shall be possible to configure the multiplication factor used in the GPS distance calculation formula. | Not Implemented
AOS_BHPB 2863 (Draft) | It shall be possible to configure a timeout for considering GPS communication lost. | Not Implemented
AOS_BHPB 2864 (Draft) | It shall be possible to configure the distance used when evaluating train integrity based on GPS input that defines an area where train integrity is unknown. | Not Implemented


\section SystemArchitecturalDesign System Architectural Design.

\subsection ChosenSystemArchitecture Chosen System Architecture

@image html tims_class_diagram.png
@image latex tims_class_diagram.png

\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

\subsection ExternalInterfaceDescription External Interface Description

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection TIMS TIMS

The TIMS adaption class is populated with methods to process the different sources for TIMS evaluation.

\subsubsection Combination Combination of Integrity Sources

Depending if ECPB is available or not, different sources are used for evaluation of the TIMS status.
This flowchart describes the implementation for the BHP specific evaluateTIMSStatus().

@image html evaluate_tims_status.png
@image latex evaluate_tims_status.png

\subsubsection ECPBTIMSCOnfig ECPB TIMS confirmation

Flowchart of method used in evaluateECPBConfirmation()

@image html evaluate_ecpb_confirmation.png
@image latex evaluate_ecpb_confirmation.png

\subsubsection GPSEval GPS evaluation

Flowchart of method used in evaluateGPSStatus() for evaluating the length of the train with front- and last car GPS.

@image html evaluate_gnss.png
@image latex evaluate_gnss.png

Reference 1 is further describing formulas for calculating distance between two GPS points.

See Additional Material for an example how to generate a lookup-table with 1000 values for the first quadrant of a sine-table (genSinTable()).

Prototype functions for calculating Sine, Cosine, ASine and ACosine is also found in Additional Material.
A prototype function to calculate distance between GPS points is also provided.

Fixed point arithmetic is used to avoid floating point variables. This is 10 exp 9 in the example but might be a value of eg 2 exp 30 for better
performance.

The Estimated train length is calculated with 3 terms, LocoFront to First RefPoint + Stored RefPoints total length + (Last RefPint to Rear GPS Pos)

@image html get_estimated_train_length.png
@image latex get_estimated_train_length.png


\subsubsection LastCarBP Last car brake pressure evaluation

Flowchart of method used in evaluateLastCarBP() to evaluate brake pressure in last car.

@image html evaluate_last_car_bp.png
@image latex evaluate_last_car_bp.png

\subsubsection VehicleBP Vehicle brake pressure evaluation

Flowchart of method used in evaluateVehicleBP() to evaluate brake pressure in vehicle.

@image html evaluate_vehicle_bp.png
@image latex evaluate_vehicle_bp.png

\subsubsection ProcessPosition Process Position

The front position (odoValue) of the train is stored continuously together with time to be able to match reading from the last car GPS.
This is to find a corresponding reading for the front and rear GPS.

Reference values (according to Train Integrity Algorithm Design Doc) are stored along the way after a certain distance from previous reference point.

@image html process_position.png
@image latex process_position.png

\subsubsection Startup Functionality to determine if TIMS is available at startup.

This flowchart describes the implementation for the BHP specific evaluateTIMSAvailable().

@image html evaluate_tims_available.png
@image latex evaluate_tims_available.png


\subsubsection Configuration Configuration

The following configuration parameters are identified from requirements and reference 1.

GPS Data Validation:

- Minimum GPS accuracy
- Minimum HDOP
- GPSTypeAcc
- Time to detect communication loss

2DMap:

- Distance between storing Reference points
- Max number of reference points to store
- External GPS time delay

Train Length Calculation:

- Correction factor for max trace curvature and gradient
- Safety margin for GPS positioning of last reference point and Rear position
- D_unknown (see reference 1)
- Maximum distance between last refPoint and GPSRear
- Radius of earth

TIMS evaluation:

- ECPB train integrity input is allowed or not
- Lowest allowed brake pipe pressure in the locomotive for considering the train as intact
- Lowest allowed brake pipe pressure at the last car for considering the train as intact

\subsubsection Console Console

Console commands to print out:
 - The stored reference points 
 - Vital position points.
 - Last received GPS Front and Rear

\subsection VehicleCom VehicleCom

\subsubsection LCS Train Status Message

The received GPS Positions in Train status Message will correspond to FFFIS AOS-LCS Ver 2.2.

I e the decoding of the data needs to be updated accordingly and stored in an update version of  'struct GPSPositionType'.

\subsection AOSPC AOSPC

\subsubsection LCSSim LCSSim

Simulation of GPS Position:

There will be two modes to simulate the GPS Position:
 - Simulate train traveling in a straight direction by inserting an angle from 0-360 degrees.
 - Simulate train traveling in a circle by entering the radius of the circle.
  
The logic for simulation is implemented in LCSSimDLL::LCSSimulation::RunLCSSimulation(void) in the block '// Simulation Processing'.

The position for mode 1 will be calculated by reading the speed from the AOS Status message:

- New latitude = currentLatitude + sin(angle) * (v * t), where v = speed from AOS (ATPCom->recAOSVehicleSpeed) converted to radians/sec and t is the periodicity for the calculation.
- New longitude = currentLongitude + cos(angle) * (v * t), where v = speed from AOS (ATPCom->recAOSVehicleSpeed) converted to radians/sec and t is the periodicity for the calculation.

The position for mode 2 will also be calculated by reading the speed from the AOS Status message:

- angle = (v * t)/r, where v = speed from AOS (ATPCom->recAOSVehicleSpeed) converted to radians/sec and t is the periodicity for the calculation.

- New latitude = currentLatitude + r * sin(angle)
- New longitude = currentLongitude + r * cos(angle)

If the simulation is enabled then the output from LCSSim will be put in the following output variables:

\code{.c}

atpCom->sndFrontGPSLatitude = currFrontGPSLatitude;
atpCom->sndFrontGPSLongitude = currFrontGPSLongitude;
atpCom->sndFrontGPSTime = currFrontGPSTime;
atpCom->sndFrontGPSSpeed = currFrontGPSSpeed;

atpCom->sndRearGPSLatitude = currRearGPSLatitude;
atpCom->sndRearGPSLongitude = currRearGPSLongitude;
atpCom->sndRearGPSTime = currRearGPSTime;
atpCom->sndRearGPSSpeed = currRearGPSSpeed;

\endcode

The RearGPS position is starting x meter 'behind' the front position. The Speed will be the same for front- and rear position but the GPS Time difference can be adjusted 
from GUI.

The start position for rear position of the train in straight mode will be N 0,0 E 0,0  and train will be moving in the bearing provided in GUI.

The start position for the rear position of the train in circular mode will be N 0,0 E (radius in rad), and train will be moving anticlockwise in the circles circumference. 

Two more fields are added;
- Lock last car GPS position to simulate TIMS broken
- GPS Measurement not available

\section UserInterfaceDesign User Interface Design

\subsection DescriptionOfTheUserInterface Description of the User Interface

\subsubsection ScreenImages Screen Images

The LCSSim GUI is updated with an extra tab for GPS Simulation parameters.

@image html lcssimgui.png
@image latex lcssimgui.png

The GPSFront/GPSRear Tabs will also be slightly updated to reflect the new GPS Message format in LCS-AOS FFFIS 2.2.

\subsubsection ObjectsAndActions Objects and Actions

\section ImplementationDistribution Task Distribution & Time Estimation

Suggested Scrum Tasks:

- TIMS Status Evaluations
- Storage Class (2DMap) and calculations for Reference Points and Vital Positions
- Calculation method between 2 GPS Positions
- Configuration Parameters
- Console Printouts
- SCDS
- MsgHandler TrainStatus GPS-Position
- AOSPC Simulation


Time Estimates:

- TDD

    - TDD for AOS and AOSPC (5d) 

    TDD Total: 5d

- TIMS
    
    - TIMS Evaluation Methods and combination of sources (3d)
    - Calculation Methods (2d)
    - Storage for VitalPosition and GPStime (1d)
    - Reference Points Handling (Storing, Accessing and Deleting) (3d)
    - Configuration Parameters (2d)
    - Console printouts (1d)
    - SCDS (Reuse also from TDD) (4d)
    - Upgrade LCS Train Status GPS Position Interface to 2.2 (1d)
    
    TIMS Total: 17d

- AOSPC

    - Simulation of GPS Position from AOS Status Message (5d)
    - Fault simulation (2d)
    - Upgrade LCS Train Status GPS Position Interface to 2.2 (1d)
    
    AOS PC Total: 8d

- Testing 

    - With/Without ECPB 
    - Combination of train integrity sources
    - Configuration Parameters 
    - Different layouts (GPS simulation data)
    - Different Faults (Invalid GPS data, Brake pressure etc, Loss of last car (GPS last car frozen), ECPB Waiting for confirmation, etc )
    
    Testing Total: 5d

Work in Total: ~35d

    
\section AdditionalMaterial Additional Material

Outstanding questions for FFFIS LCS-AOS/EMD:

- In what cases are Trainintegrity status = Not Asserted (TrainStatus)

Other questions:

- Only TIMS Integrity Broken is reported in Pos Status Report (not Pending)


\subsection PrototypeLookup Prototype for Sine/Cosine Lookup Table

\code{.c}

// sine_lookup_protoype.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#define _USE_MATH_DEFINES
#include <math.h>
#include <conio.h>

const __int64 FirstQuad   = 1570796327;
const __int64 SecQuad     = 3141592654;
const __int64 ThirdQuad   = 4712388980;
const __int64 FouthQuad   = 6283185307;

const int EarthRadie= 6371000;

const __int64 Factor =  1000000000;

const int TableSize = 1001;

typedef struct {
__int64 radians;
__int64 sinValue;
} sinValType;

sinValType sineTable[TableSize];


// Function to pre-generate a sine-table for first quadrant.
void genSinTable()
{
for (int i = 0; i < TableSize; i++)
{
double radians = (M_PI / 2 / (TableSize-1)) * i;
double sinVal = sin(radians);

sineTable[i].radians = radians * Factor;
sineTable[i].sinValue = sinVal * Factor;
}
}

__int64 getInterpolatedSine(__int64 rad)
{
int i = 0;
__int64 retVal = 0;

while (sineTable[i].radians < rad && (i < TableSize))
{
++i;
}

if (sineTable[i].radians == rad)
{
retVal = sineTable[i].sinValue;
}
else
{
__int64 tmp = ((sineTable[i].sinValue - sineTable[i - 1].sinValue) * Factor);
tmp /= (sineTable[i].radians - sineTable[i - 1].radians);

tmp *= (rad - sineTable[i - 1].radians);
tmp /= Factor;

retVal = tmp + sineTable[i - 1].sinValue;
}

return(retVal);
}

// Public function to lookup Sine
__int64 lookupSine(__int64 rad)
{
__int64 retVal;

if (rad <= FirstQuad)
{
retVal = getInterpolatedSine(rad);
}
else if (rad <= SecQuad)
{
retVal = getInterpolatedSine(SecQuad - rad);
}
else if (rad <= ThirdQuad)
{
retVal = -getInterpolatedSine(rad - SecQuad);
}
else
{
retVal = -getInterpolatedSine(FouthQuad - rad);
}

return retVal;
}

// Public function to lookup Cosine
__int64 lookupCosine(__int64 rad)
{
if (rad > FouthQuad)
{
rad -= FouthQuad;
}

__int64 retVal = lookupSine(rad + FirstQuad);

return retVal;
}

__int64 getInterpolatedArcSine(__int64 sinValue)
{
int i = 0;
__int64 retVal = 0;

while (sineTable[i].sinValue < sinValue && (i < TableSize))
{
++i;
}

if (sineTable[i].sinValue == sinValue)
{
retVal = sineTable[i].radians;
}
else
{
__int64 tmp = ((sineTable[i].radians - sineTable[i - 1].radians)  * Factor) / (sineTable[i].sinValue - sineTable[i - 1].sinValue);
tmp *= (sinValue - sineTable[i - 1].sinValue);
tmp /= Factor;

retVal = tmp + sineTable[i - 1].radians;
}

return(retVal);
}

// Public function to lookup ArcSine
__int64 lookupArcSine(__int64 sineVal)
{
__int64 retVal;

if (sineVal > 0)
{
retVal = getInterpolatedArcSine(sineVal);
}
else
{
retVal = -getInterpolatedArcSine(sineVal);
}

return retVal;
}


// Public function to lookup ArcCosine
__int64 lookupArcCosine(__int64 cosVal)
{
__int64 retVal = 0;

if (cosVal > 0)
{
retVal = FirstQuad - lookupArcSine(cosVal);
}
else
{
retVal = FirstQuad + lookupArcSine(cosVal);
}

return retVal;
}

typedef struct
{
__int64 latitude;
__int64 longitude;

}gpsPos;

// Public function to get distance between 2 GPS Points
int getDistance(gpsPos gpsPos1, gpsPos gpsPos2)
{
int retVal;

__int64 term1 = (lookupSine(gpsPos1.latitude) * lookupSine(gpsPos2.latitude)) / Factor;
__int64 temp = (lookupCosine(gpsPos1.latitude) * lookupCosine(gpsPos2.latitude))/ Factor;
__int64 term2 = (temp * lookupCosine(gpsPos1.longitude - gpsPos2.longitude)) / Factor;

__int64 factor1 = lookupArcCosine(term1 + term2);

retVal = (EarthRadie * factor1) / Factor;

return retVal;
}

int main()
{
genSinTable();

for (int i = 0; i < TableSize; i++)
{
printf("%lld %lld", sineTable[i].radians, sineTable[i].sinValue);
printf("    Lookuptable: %lld and in between %lld\n", lookupSine(sineTable[i].radians), (lookupSine(sineTable[i].radians) + lookupSine(sineTable[i+1].radians)) / 2);
}

gpsPos gpsPos1;
gpsPos gpsPos2;

// Test 1376m
gpsPos1.latitude = 0;
gpsPos1.longitude = 0;
gpsPos2.latitude = 0;
gpsPos2.longitude = 215926; // Multiplied with Factor

printf("Distance between pos 1 and 2 : %d meter", getDistance(gpsPos1, gpsPos2));

_getch();

return 0;
}

\endcode


*/
