namespace ATP::DS
{
  /**
  \if AsMainPage
  \mainpage Tracks Component Specification
  @anchor trk
  \endif

  \ifnot AsMainPage
  \class AbstractTracks
  \endif

  \section Purpose Purpose
  This document specifies the software design for the class AbstractTracks, the core part of the Tracks component.

  \latexonly \newpage \endlatexonly
  \section Overview Overview

  \subsection GeneralFunctionality General Functionality
  The Tracks core component implements the data storage of all tracks and balises within the AOS. The
  tracks and balises are stored as objects in separate virtual dynamic memory class and are
  accessible for external components as a tracks list and a balise lists using iterators.


  \subsection DeploymentDiagram Deployment Diagram
  N/A

  \subsection Dependencies Dependencies
  The Tracks component is dependent on Position component to Current odometer position at antenna.

  Other components have dependencies to this component for the public types and methods, 
  see \ref AbstractTracks Class Reference.

  \latexonly \newpage \endlatexonly
  \section FunctionalDesign Functional Design
  The functions Track::initMemPoolSize() and Balise::initMemPoolSize() are implemented in AbstractTracks::init() to 
  initialize and allocate memory pool for dynamic memory substitution.

  \subsection Initialization Initialization
  The AbstractTracks::init() function initializes the list of maximum number of tracks and balises that can be stored and only after the initialization
  is successful can the CrossCompare be initialized. In the constructor as a part of the initialization it clears all the balises from possession balises list.
  
  \subsection ModeDependentOperation Mode Dependent Operation
  Tracks Component is independent of modes and states. 

  \subsection Scheduling Scheduling
  The core part of Tracks component is data driven. The below functions are utility functions implemented in ATP::DS::Track class.

  + Track::isTandPInTrack()  - Checks if track and position is present in this track.
  + Track::isOdoPosInTrack() - Checks if the odometer position is present in this track.
  + Track::getOdoAt0Cm()     - It returns the current Odometer value at leg 0 position of the train.
  + Track::getOdoAtNexTrackLimit() - It returns the Odometer value limit at next track. 
  + Track::getOdoAtPrevTrackLimit() - It returns the Odometer value limit for previous track.

  None of the below functions are scheduled but acts as interface methods for other components.
  The interface methods call the component to extract information on tracks and balises.
  
  \subsection TrackBaliseListHandling  Track and Balise List Handling
  The tracks list and balises list maintains all currently stored tracks and balises respectively. 
  The order of the insertion into the list is preserved. 
  The tracks list and balises list operate with reference to Track and Balise class respectively implemented in the core.
  The data structure to store tracks and balises list is through GPList().
  GPList is double linked list used to hold elements of specific type.

  \subsection TrackInsertion Track Insertion
  The flow chart describes the procedure to add track entry to tracks list.
  @image html abstract_tracks_addTrack.png  "Abstract Track Add Tracks"
  @image latex abstract_tracks_addTrack.png  "Abstract Track Add Tracks"

  \subsection BaliseInsertion Balise Insertion
  The flow chart describes the procedure to add balise entry to balises list.
  @image html abstract_tracks_addBalise.png "Abstract Track Add Balise"
  @image latex abstract_tracks_addBalise.png "Abstract Track Add Balise"

  \subsection TrckandBalDeletion Track and Balise Deletion
  Tracks and Balises cannot be deleted individually. They can only be removed through the following overloaded functions.
  They take either tracks and position or odometer positions as input to delete tracks and balises.

  + AbstractTracks::removePassed(): This function shall remove all of the tracks that has been passed, 
   in respect to the position of the end of the last unit in the opposite of the travel direction, 
  from here on known as rear-position, and that this position includes all necessary margins.

  + AbstractTracks::removeNotCovered(): This function shall remove all of the tracks that are outside the range specified in respect to the position
  of the front of the first unit towards the travel direction, from here known as lead-position and that this position includes all necessary margins.
  The calling function shall take care that rear-position and lead-position includes all the necessary margins.
  All the corresponding balise objects will also get deleted.

  + AbstractTracks::removeAll() : This function shall delete all track and balise objects from their respective lists. 

  \subsection PosBaliseHandling Balise Handling in Possession
  All the balises in possession are handled in one dimensional array. \n
  The following functions are used to perform operation on possession balise list.
  + AbstractTracks::isBaliseInPossessionList()  : This function checks if balise is present in the possession balise list.
  + AbstractTracks::clearPossessionBaliseList() : This function shall delete all the elements of the possession balise list.
  + AbstractTracks::addPossessionBalise()       : This function shall add a new possession balise to existing  possession balise list.

  \subsection calculateTrackAndPos Calculate Track Position
  This function implements the Track Iterator across the tracks list to check if odometer value is in the track.
  It checks the current Odometer direction to set calculated odometer values \a odoAt0Cm.

  \subsection OdoAt0cm OdoAt0cm
  \a OdoAt0cm is the calculated Odometer reading measured at leg 0. OdoAt0cm helps to calculate the position of the train on the track. Leg 0 is the start position of the track. 
 
 \section ClassDiagram Class Diagram
  @image html abstract_tracks_class_diagram.png "Class diagram"
  @image latex abstract_tracks_class_diagram.png "Class diagram"

  \section Diagnostics Diagnostics

  \subsection Console Console commands
  The following component-specific console commands are implemented:

  + tracks  : To print list of tracks in the tracks list.
  + balises : To print the list of balises in balises list.
  + posBal  : To print the list of possession balises in possession balises list.

  \subsection Analyze Analyze
  No values are registered for analysis for Tracks component.
  
  \section CoreAdaptation Core / Adaptation
  All the major functionality is implemented in core part of the Tracks component.

  \section PreProcessor Pre-Processor Directives
  No pre-processor directives available for this component
 
  \section Traceability Traceability

  \subsection SSRS Functional requirements
  The functional requirements are defined in [SSRS].
  Common functional requirements are described in SCDS ATP Core.

  The requirements relevant for this component are:

  Req        | Chapter                 | Function
  ---------- | ----------------------- | --------
  AOS 283    | \ref TrckandBalDeletion | AbstractTracks::removePassed()
  AOS 287    | \ref TrckandBalDeletion | AbstractTracks::removePassed()
  AOS 1038   | \ref Console            | AbstractTracks::consoleCall()
  AOS 1120 S | -                       | -
  AOS 1723 S | \ref TrckandBalDeletion | AbstractTracks::removeNotCovered()
  AOS 2140   | \ref TrckandBalDeletion | AbstractTracks::removeAll()
  AOS 2591   | \ref TrckandBalDeletion | AbstractTracks::removeAll()
  AOS 2787   | \ref PosBaliseHandling  | AbstractTracks::addPossessionBalise()
  AOS 2791 S | \ref PosBaliseHandling  | AbstractTracks::addPossessionBalise()
  AOS 3018   | \ref PosBaliseHandling  | AbstractTracks::clearPossessionBaliseList()
  AOS 3165   | \ref TrckandBalDeletion | AbstractTracks::removeNotCovered()

  \subsection SSAS Architectural Requirements
  The architectural requirements are defined in [SSAS-APP].
     
  Common functional requirements are described in SCDS ATP Core.

  Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

  */
}
