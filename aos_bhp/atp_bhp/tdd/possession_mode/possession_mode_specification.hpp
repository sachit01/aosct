/**
\if AsMainPage
\mainpage Balise Handling(Possession Ack message and Possession Mode)
\endif

\section VersionLog Version Log

Version | Date       | Description                                                 | Signature
------- | --------   | ----------------------------------------------------------- | ---------
1.0     | 2018-03-05 | Proposed Changes for Balise Handling in Possession Mode.    | spandita
1.1     | 2018-03-09 | Review comments fixed                                       | spandita

\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface
TODO           | To be discussed
SB             | Service Brake
FFFIS          | Form Fit Functional Interface Specification 
TCC            | Train Control Center

\section Introduction Introduction

Below changes has been proposed as per the FFFIS_TCC version-5.12 for balise handling in possession mode.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS 2787 | The AOS shall store all balise identities sent in the PossessionAcknowledge message when ATP mode changes to Possession after all the tracks and balises stored previously have been deleted. | Not Implemented

 \subsection ExternalInterfaceDescription External Interface Description
 Following external interface description is used to implement the requirement described above.
 Seq No | Document Name                                      | Version
 -------|----------------------------------------------------|----------
1.      | FFFIS TCC-AOS                                      | 5.12
2.      | TDD for FFFIS-TCC update 5.12                      | 1.0

\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection PossessionAcknowledge RadioMessageInPossessionAcknowledge class

+ Create private vector variable and reserve the size of 50 element.
\code
std::vector<BaliseData>        baliseDataVec;
\endcode

+ Parse the field of BALISE_IDENTITY in parseMessageData function of RadioMessageInPossessionAcknowledge class and store the value in baliseDataVec variable.
+ Invalidate the vector (baliseDataVec) in every cycle in invalidate function of RadioMessageInPossessionAcknowledge class.
+ Create a protected functions (validateBalises and publishBalises)in RadioMessageInPossessionAcknowledge class. 
+ Call the validateBalises after validateMode() function in validate() function of RadioMessageInPossession Acknowledge class.
+ In validateBalises function check if balises are duplicated or not.
+ validateBalises function should return false if the balises are duplicated and should return true if not duplicated.
+ Upon successful validation of input balise publish the balise data to AbstractTracks class in publishBalises function.(Refer \ref AbstractTracks for more details)

\subsection DataStorage Persistent Data Storage 

\subsection AbstractTracks AbstractTracks class
+ Create a fixed array for Balise ID of type uint16_t as private member(should be named as possessionBaliseList) in AbstractTracks class.
\code
uint16_t possessionBaliseList[maxPossessionBaliseCount];
\endcode
+ Set the maximum number of elements of array(possessionBaliseList) to 50 value.
+ Create the following public functions in AbstractTracks class to perform following specific operation.
\code
/*
 * will be used to search the received balise from BTM in the balise list received from TCC.
 * Searching method will be binary search 
 */
\endcode
- getPossessionBalise(const uint16_t balise id) (balise ID as input argument). 
\code
/*
 * Will be used to add the balise ID's received in possession ack message.
 * Addition of balises will be done in increment order 
 */
\endcode
- addPossessionBalise(const uint16_t balise id)(balise ID as input argument).
\code
/*
 * Will be used to clear the possession balise list  
 */
 \endcode
- clearPossessionBaliseList()

+ getPossessionBalise function should returns true if the passing argument(balise id) matches with the stored balise in possessionBaliseList array.

\subsection ModeControl Possession Mode class

+ Create the submodes for possession class like possessionStart, possessionSetup.
+ Create the virtual protected functions as runPossessionStart, runPossessionSetup.
+ Delete the train setup, Tracks and targets in runPossessionStart function and change the substate to possessionSetup.
+ In possessionSetup submode create the internal train setup.
+ Depends upon the FFFIS-TCC sequencing document status further mode related implementation will be done here.
 
\subsection Position Position 

+ In ATPModePossession switch statement read the balise from decode component using getBaliseInformation function.
+ call getPossessionBalise from AbstractTracks class and pass the received balise id as input argument.
+ Raise the Service brake event if the getPossessionBalise function returns false.

\subsection Misc Misc 
+ Cross compare the elements of possessionBaliseList array.

\section AdditionalMaterial Additional Material

*/