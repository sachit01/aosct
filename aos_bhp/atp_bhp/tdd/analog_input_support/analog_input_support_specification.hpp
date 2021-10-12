/**
\if AsMainPage
\mainpage Analog Input support for AOS 
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-06-29 | Analog Input Signal support                   | akushwah


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | Automatic Train Protection and Automatic Train Operation
AOSPC          | Automatic Train Protection and Automatic Train Operation Personal Computer

\section Introduction Introduction

\subsection Design Design Overview

The CoHP-2 platform supports one analog board for inputs and outputs. The board provides 8 inputs and 2 outputs. AOS will only use analog inputs and no analog output is defined or will be used.
AOS uses two analog brake pressure sensor inputs to allow monitoring of brake application.The purpose of TDD is to explain what all needs to be implemented in order to support the Analog Inputs in AOS Core.
It will also explain the update required in AOSPC simulator for simulating the brake pressure analog inputs for testing environment.

@image html brake_pressure_sensor_input.png
@image latex brake_pressure_sensor_input.png

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

The below Hardware Requirement ID should be taken care in account while implementing the Analog Inputs Support for AOS Changes.
Req Id          | Hardware Requirement description                                                                                 
----------------| --------------------------------------------------------------------------------------------------------------
AOS HW-038      | AOS shall provide configuration settings to define if Brake Pressure inputs are available on vehicle.
AOS HW-039      | AOS project adaptation shall provide translation of input sensor measurement range to above listed unit.
AOS BHPB HW-017 | AOS shall setup the sensors to accept input range 4-20 mA corresponding to pressure of 0-200 psig. AOS shall translate pressure values to kPa.
AOS BHPB HW-018 | AOS shall monitor sensor inputs and require activation of brake if values not correspond after filtering.
AOS BHPB HW-019 | The vehicle shall use two sensors mounted on the main brake pipe.
AOS BHPB HW-020 | The vehicle shall in placement minimize risk of common cause of failure, for example by sensor clogging.

NOTE: {AOS HW xxx}. This numbering is preliminary and may change as the AOS IF 150 BHPB HW Interface is not yet released.

\section SystemArchitecturalDesign System Architectural Design

\subsection ChosenSystemArchitecture Chosen System Architecture

The updates required in Analog Input Support for AOS should be done in below mentioned phases
1. AOSPC  - In AOSPC, need to create a new tab in LocoSim which will contain the simulated Brake Pressure Analog Input signal.
2. VIOH Sim  - The SimulatedInputs message format will be changed in order to support the analog inputs in AOSPC simulation, 
hence the corresponding implementation regarding input message handling needs to be taken care. 
3. LocoIO - LocoIO component acts as an interface between the AOS software and the Analog Input. Its main
task is to translate the analog input signals from hardware to the variables used in AOS software.
4. SCDS : VIOHSIm SCDS and LocoIo SCDs also needs to be updated.
5. IF update : AOS_SimulatorInterface.doc need to be updated to include the B_ANALOG_INPUT_VALUE in SimulatedInputs message.

Note: Its not necessary to follow the sequence of update mentioned above and It can be done in any order.


\subsection DiscussionOfAlternativeDesigns Discussion of Alternative Designs

<Describe if there are any alternatives to the chosen design.>

\subsection ExternalInterfaceDescription External Interface Description

These changes will have a impact on the AOSPC. AOSPC need to be updated as described below section \ref AOSPC

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection VIOHSim VIOH Sim
The SimulatedInputs message format will be changed in order to support the analog inputs in AOSPC simulation,
hence the corresponding implementation regarding input message handling needs to be taken care.
1. Need to implement or modify the below function in vioh_client_sim.cpp
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOURegister(const VIOH_listType *const pInputs, const VIOH_listType *const pOutputs)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOURegisterSync (const VIOH_listType *const pInputs, const VIOH_listType *const pOutputs, const uint16_t *const pLimitLow, const uint16_t *const pLimitHigh)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOURegisterResult (VIOH_confRespType *const pResult)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOUGetState (uint8_t Id, uint16_t *pValue, VIOH_healthStateType *pHealthState)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOUSetOutput (const uint8_t Id, const uint16_t value)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOUGetDeviceState (VIOH_healthStateType *const pHealthState)
   +  VIOH_clientResultType VIOHnames::VIOHClient::AIOUGetRevisionId (uint32_t *const pRevisionId)

2. Need to update the VIOHSim::readSimulatedInputs() function for the extraction of simulated analog signal value from VFW channels.

3. Need to update the AIOUGetState() function similarly to VIUGetState() function. This function will returns the input value and the health state of the specified analogue input.


\subsection LocoIO LocoIO component
LocoIO component acts as an interface between the AOS software and the Analog Input. Its main
task is to translate the analog input signals from hardware to the variables used in AOS software.
The below function needs to be updated for the analog input support.
1. AbstractLocoIO::init(void) : Need to update the State machine for the Analogue Input signal for the initialization of it. It will be similar to the Digital input signal as mentioned in VIOH document.
Also, initialize the brake pressure sensor signal 1 & 2 in the constructor of LocoIO LocoIO::LocoIO(void) in the adaptation section.

2. AbstractLocoIO::readInputs(void) : This function will be extended for the analogue Inputs received from the VIOH client.

3. create the below access functions 
  + getAnalogInputRawvalue() : it will provide the raw value of the Analog Signals i.e Brake pressure sensor 1 & Brake pressure sensor 2 which will be in 0-200 psig (4-20mA).
  + getAnalogInputScaledvalue() : it will provide the scaled value of the Analog Signals i.e Brake pressure sensor 1 & Brake pressure sensor 2, which will be in 0-1000Kpa. Raw Value 4-20 mA will be translated to 0-1000 Kpa.

4. AbstractLocoIO::consoleCall() : Update the console call to get the raw value and scaled value displayed on the console by console command locoIO.

\subsection InterfaceUpdate Interface Update
AOS_SimulatorInterface.doc need to be updated to include the M_ANALOG_INPUT_VALUE in SimulatedInputs message.\n
There will be 8 Analog inputs in which first and second will be of brake pressure analog input signal 1 & 2 respectively and rest analog signal is still undefined. 

\section UserInterfaceDesign User Interface Design

\subsection AOSPC AOSPC
In AOSPC, need to create a new tab(Brake Pressure) in LocoSim which will contain the simulated Brake Pressure Analog Input signal.
The Brake Pressure tab will be divided in to two parts:\n
- Automatic Simulation : This part will contain the three check-boxes namely No Brake,Service Brake and Emergency Brake.\n
At a time, only one check-box can be selected. When No Brake check-box is selected, it implies 1000Kpa pressure is being received from the sensor.\n
Similarly, SB check-box selection imply 500Kpa pressure received from sensor and EB check-box imply 0Kpa is being received from brake pressure Sensor.\n

- Manual Simulation : There will be a box in which we can manually can enter brake pressure in between 0-1000Kpa and it means that entered value in box is the brake pressure received from sensor.

Note: From the above two parts, only one will be active at a time and other will be grayed out.

Below is the list of function needs to be updated for AOSPC changes in order to support the Analog signal in AOS: \n
a. Need to create an integer array for analogInput member variable in LocoIO class in locoIO.h file.\n
b. Create memory for the analogInput Array in constructor of LocoIO().\n
c. Create the function LocoSimDLL::LocoIO::SetAnalogInputValue(Inputs Signal, uint16_t NewState) and fill the analogInput array accordingly.\n
d. LocoSimDLL::LocoIO::SendInputData() function in AOSPC solution need to be updated to read the analog brake sensor 1 & 2 pressure value via from GUI and write it to WriteAOSInputIO().\n
e. LocoSimDLL::VIOHSimConnection::WriteAOSInputIO() needs to be updated to handle the analog input before writing the data buffer to SendData() on socket.\n

Also, the analog inputs will be mapped to the value mentioned in AOSPC.ini file under Analog Input mapping Inputs.\n
Currently Brake pressure sensor 1 data and Brake pressure sensor 2 data will be mapped to input 1 & 2 of analog input respectively.\n
The rest of the Analog inputs is not yet defined, hence it will mapped to undefined.

\subsubsection ScreenImages Screen Images

<If applicable, include screen-shoots or other images to better describe the UI.>

\subsubsection ObjectsAndActions Objects and Actions

<If applicable, describe the actions trigged by certain UI events.>

\section AdditionalMaterial Additional Material

SCDS for the abstract_Loco_IO , Loco_IO and VIOH Sim will be updated to incorporate the changes done  as per the section \ref VIOHSim and \ref LocoIO respectively.


*/