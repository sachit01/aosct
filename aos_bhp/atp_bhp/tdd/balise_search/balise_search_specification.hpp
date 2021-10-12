/**
\if AsMainPage
\mainpage Balise Search Changes(section 6.1.5)
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
1.0     | 2017-02-27 | Purposed Changes wrt Balise Search Req        | spandita


\section Abbreviations Abbreviations
Abbreviation   | Definition                                   
-------------- | ------------------------------------------------------------------
TDD            | Technical Design Description
AOS            | ATP-On board system
ATP            | Automatic Train Protection
MA             | Movement Authority
DMI            | Driver Machine Interface

\section Introduction Introduction
Below changes are purposed as per the doors requirements defined in section 6.1.5.

\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 
Req     | Short requirement description                                                         | Description 
------- | --------------------------------------------------------------------------------------| ------------
AOS1948 | The AOS shall supervise the configurable ceiling speed in Balise Search ATP mode      |Implemented
AOS64   | The AOS shall trigger brake if train has traveled distance more than balise search distance | Partially Implemented
AOS65   | The AOS shall send message to the TCC when a valid first balise is found             |Implemented
AOS1942 | The AOS shall trigger a brake if distance traveled exceed the allowed configurable value |Not implemented
AOS1286 | The AOS shall trigger a brake if the first balise is not present in a received MA(REG)|Implemented
AOS1287 | The AOS shall calculate the train position w.r.t position of first balise             |Implemented
AOS1289 | The AOS shall trigger a brake if second balise is identical with the first balise     |Not implemented
AOS71   | The AOS shall trigger a brake if received balise ID is not equal to expected balise ID present in received MA| Partially Implemented
AOS77   | The AOS shall trigger a brake if received balise is not present in received MA (re-reg) |Partially implemented
AOS1093 | The AOS shall trigger a brake if expected balise is not received and train has traveled a distance longer than MAX_SEARCH_DISTANCE |Not Implemented

\subsection ExternalInterfaceDescription External Interface Description

Need to analyze the interface of TAB on DMI which shall be used to print the error messages.

\section DetailedDescriptionOfComponents Detailed Description of Components

Following are the list of affected components:

- Position
- DMI handler
- Supervise
- Mode Control(TBD)

\subsection AOS64 AOS64
Affected component  : DMI handler \n
Need to implement new class in DMI which will take DMI event codes as input via event handler.\n
File name shall be named as dmi_messages_out_text_message.cpp.

\subsection  AOS1942 AOS1942
Need to discuss regarding the placement of code. \n
May we can put the code in runBaliseSearchWaitBalise2() function of mode control .

\subsection  AOS1289 AOS1289
Affected function and component  : checkMissedBaliseOnDetection() & position \n
Need to implement else part of check of follwing code: \n
  if ((!isBaliseReceived) && (nextBaliseInfo.nidBG != 0U)) \n
      { \n
	  } \n
and trigger the safe brake to stop.

\subsection AOS71 AOS71
Affected function and component : checkMissedBaliseOnDetection(),run() & position \n
Implementation need to be done to check for presence of second balise in balise List. \n
Also need to correct type of event to safe brake to stop. \n

\subsection AOS77 AOS77
Affected function and component : run() & position \n
Need to implement the check for presence of first balise in re-registration mode \n
and trigger safe brake to stop if not found in balise list  \n
if (((isNewConf) && isBaliseinList)) \n
{  \n
}   \n
else \n
{     \n
	//raise safe brake to stop \n
} \n

\subsection AOS1093 AOS1093 
Affected component : DMI handler \n
Need to implement new class in DMI which will take DMI event codes as input via event handler \n
File name shall be named as dmi_messages_out_text_message.cpp \n

\section AdditionalMaterial Additional Material
SCDS of Position needs to regenerate again.

*/