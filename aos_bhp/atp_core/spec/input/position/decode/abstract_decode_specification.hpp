namespace ATP::Pos
{
  /**
  \if AsMainPage
  \mainpage Abstract Decode Component Specification
  @anchor dec
  \endif

  \ifnot AsMainPage
  \class AbstractDecode
  \endif

  \section Purpose Purpose
  This document specifies the software design for the AbstractDecode class, the core part of the Decode component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality

  The main functions of the Abstract Decode component is to receive and decode balise telegram messages from the BTM Handler.
  When the BTM receives a balise, the balise telegram will get a begin and end timestamp.
  These timestamps are used to interpolate the position from a list of stored positions and timestamps
  (history list).

  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  This component has dependencies to the following components:
  + BTM Handler: To get BTM Telegrams.
  + Odometry: To acquire odometer position and speed.
  + Event Handler: To report event.

  Other components have dependencies to this component because they use its public types
  and methods, see \ref AbstractDecode Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design

  \subsection Initialization Initialization
  During initialization AbstractDecode::initCrossCompare is called in order to add the persistent attributes to cross compare.

  \subsection ModeDependentOperation Mode dependent operation.
  N/A

  \subsection Scheduling Scheduling
  The Abstract Decode component has these function(s) that must be called each execution cycle:

  \subsubsection run run()
  The method run() performs the following functions:

  - Reset danger for shunting information.

  - Check that the balise output queue is empty to make sure all received balises from
  the previous cycle have been consumed. If not, then raise a 'SafetyHalt' event.

  - Read the odometer values (Position and Speed). Maintain the Position History list,
  which is described in the \ref BalisePosCalc chapter.
  
  - Store the current odometer value in a list, this list is later used for the standstill condition when a balise is received.

  - Read all balises from the BTM Handler. For each balise take the following actions:
    - Check if a balise is received at standstill, see \ref BaliseAtStandstill.
    - Do the parsing of required fields of Telegram packet. The required fields are described in \ref InterpretBaliseTelegram.
    - Check the validity of telegram header, see \ref BaliseTelegramHeader.
    - Check for red balise condition which is described in \ref RedBaliseDetec.
    - If it is not a red balise, and the balise header NID_C has the configured country code for this project, take the following actions:
      - Check if Danger for Shunting is present in the telegram, see \ref DangerForShuntingPacket.
      - Calculate the current odometer position of the balise according to \ref BalisePosCalc.
      - Store NID_BG and the odometer position in the output vector queue.

  - Filter the maximum and minimum speed over one second and use the average to diminish any minor detection flaws.
    - Check if the filtered max and minimum speed is above the nominal speed and a margin for over a certain amount of time and if so issue a brake event.

  \subsection InterpretBaliseTelegram Interpret received balise telegrams
  Available balise telegrams are read from a queue in the BTM Handler component. The received telegram is checked for validity.
  The balise telegram header is described in [ERTMS] chapter 8, the separate packets and variables are described in [ERTMS] chapter 7.
  
  The balise telegram consists of a \ref BaliseTelegramHeader and one optional \ref DangerForShuntingPacket. Log an event if an unknown NID_PACKET is found and ignore the contents. All telegrams
  are terminated with the mandatory \ref EndofInfo.

  \subsubsection BaliseTelegramHeader Balise Telegram Header
  No    | Variable   | Bits | Checked | Valid Values | Remark
  ----- | ---------- | ---- | --------| ------------ |  -------
  1     | Q_UPDOWN   | 1    | Yes     | 1            | Up link telegram
  2     | M_VERSION  | 7    | Yes     | 0x10, 0x11   | Version of the ETCS language (0x10 = version 1.0, 0x11 = version 1.1)
  3     | Q_MEDIA    | 1    | Yes     | 0            | Qualifier to indicate the type of media (Balise)
  4     | N_PIG      | 3    | No      | N/A          | Position in Group
  5     | N_TOTAL    | 3    | No      | N/A          | Total number of balise(s) in the group
  6     | M_DUP      | 2    | No      | N/A          | Flags to tell whether the balise is a duplicate of one of the adjacent balises. 
  7     | M_MCOUNT   | 8    | No      | N/A          | Message Counter
  8     | NID_C      | 10   | Yes     | 0-1023       | Identity number of the country or region
  9     | NID_BG     | 14   | Yes     | 1-16383      | Identity number of the Balise group
  10    | Q_LINK     | 1    | No      | N/A          | Link Qualifier

  The balise telegram header is considered as valid if all variables marked with Yes in the Checked column have valid values.
  It is only then the balise is used and published to other components.

  \subsubsection DangerForShuntingPacket Danger for Shunting packet
  If danger for shunting is present, write to log and store the condition for other components to use.
  No    | Variable   | Bits | Checked   | Valid Values | Remark
  ----- | ---------- | ---- |  ---------| ------------ | --------
  1     | NID_PACKET | 8    | Yes       | 132          | Transmission of the aspect of a shunting signal
  2     | Q_DIR      | 2    | No        | N/A          | Validity direction of transmitted data
  3     | L_PACKET   | 13   | Yes       | 24           | Packet length
  4     | Q_ASPECT   | 1    | Yes       | 0            | Stop if in SH mode

  \subsubsection EndofInfo End Of Information packet
  No    | Variable   | Bits | Checked   | Valid Values | Remark
  ----- | ---------- | ---- | ----------| ------------ | ----------------
  1     | NID_PACKET | 8    | Yes       | 255          | End of Information

  \subsection RedBaliseDetec Red Balise detection
  The core implementation does not detect red balises. Detection of a Red Balise has to be imlemented by adaptation. If detected, issue a Brake event until standstill. 
  At standstill reset the red balise detection.

  \subsection BalisePosCalc Balise position calculation
  Calculate the position of the balise using the balise telegram received from the BTM Handler component. The odometer position calculation is done as follows:

  Current odometer(Position) and current speed values are read from the Odometry Component in every cycle. 
  A Position History List is maintained to store the Odometry position and corresponding timestamp depending on 
  following condition :
  - If the current speed is below ~ 200cm/s, the position and corresponding timestamp is saved if we have moved more than 20 cm.
  - If the current speed is above or equal to 200cm/s, the position and corresponding timestamp is saved every cycle.

  When a valid Balise telegram is received, the correct odometer value is calculated as per below formula.

  First calculate the center balise time \f$T_x\f$ as the mean value of \f$T_{begin}\f$  and  \f$T_{end}\f$  timestamps in the BTM telegram.

  \f[T_x = \frac{(T_{end} - T_{begin})}{2} + T_{begin}\f]

  Now, there will be three different cases based on \f$T_{begin}\f$ which is compared with the Position History.

  Case 1: If the calculated \f$T_{begin}\f$ \f$T_x\f$  is older than the oldest sample stored in the Position History, then
  generate a 'SafeBrakeToStop' event, in this case no need to calculate odometer value.

  Case 2: If \f$T_x\f$ lies between the two timestamps \f$T_{n}\f$ and \f$T_{n-1}\f$ in the Position History list, then linear
  interpolation is used using the below formula for the balise odometer value.

  \f[ ODO_x = ODO_{n-1} + \frac{(ODO_n - ODO_{n-1})(T_x - T_{n-1})}{T_n - T_{n-1}}\f]

  Where \f$ODO_n\f$ and \f$ODO_{n-1}\f$ are the saved odometer values at times \f$T_n\f$ and \f$T_{n-1}\f$ respectively.

  Case 3: If \f$T_x\f$  is newer or more recent than the latest saved Position History,
  then use the last saved odometer position.

  \f$ODO_x\f$ = The last saved odometer Position.

  \subsection BaliseAtStandstill Balise Detection at StandStill 

  AbstractDecode provides the functionality to detect if a balise telegram is received when the train is at standstill (speed is zero)
  and generate a 'SafetyHalt' event.
  AbstractDecode will save the odometer readings during one second in order to see if we are in standstill during that period.

  When a new balise telegram is received, AbstractDecode will check if the current odometer reading is the same as the oldest reading(first array element) in the array.
  This would indicate the train was standing still when a balise telegram is received and would generate a 'SafetyHalt' event.

  \subsection BaliseTelegramOut Balise telegrams Output Queue

  The received balise telegrams are queued in an output queue. The size of output queue is defined by the adaptation, based on project specific considerations. There is an
  access function to return the oldest telegram message first, and in subsequent calls the rest of the telegrams are returned until the output queue is empty.
  This queue is checked at the beginning of each cycle to make sure all previous messages have been consumed. If they are not, a 'SafeBrakeToStop' event will be generated.

  \section ClassDiagram Class Diagram

  @image html abstract_decode_class_diagram.png "Class diagram"
  @image latex abstract_decode_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console-commands
  N/A

  \subsection Analyze Analyze
  N/A

  \section CoreAdaptation Core / Adaptation

  The adaptation part of the Decode component has a virtual method isRedBalise() that overrides the functionality to detect the red Balise.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component.

  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].

  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        |  Chapter                     | Function
  ---------- | -----------------------------| ---------
  AOS 210    | \ref InterpretBaliseTelegram | AbstractDecode::parseTelegramPacket()
  AOS 211 S  | \ref InterpretBaliseTelegram | AbstractDecode::parseTelegramPacket()
  AOS 219 S  | \ref run                     | AbstractDecode::run()
  AOS 1099   | \ref run                     | AbstractDecode::run()
  AOS 1198   | \ref run                     | AbstractDecode::run()
  AOS 1199 S | \ref run                     | AbstractDecode::run()
  AOS 1200   | \ref BalisePosCalc           | AbstractDecode::calculateOdometerPosition
  AOS 1799 S | \ref InterpretBaliseTelegram | AbstractDecode::parseTelegramPacket()


  \subsection SSAS Architectural requirements
  The architectural requirements are defined in [SSAS-APP].

  Common requirements are specified in SCDS ATP Core.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}
