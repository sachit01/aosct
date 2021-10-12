namespace ATC
{

/** 
\if AsMainPage
\mainpage AbstractConfigBase Component Specification
@anchor cfg
\endif

\ifnot AsMainPage
\class AbstractConfigBase
\endif

\section Purpose Purpose
This document specifies the software design for the AbstractConfigBase class, the ATC part of the Config component.

\latexonly \newpage \endlatexonly
\section Overview Overview

\subsection GeneralFunctionality General Functionality
The AbstractConfigBase class is the common (base) implementation of the Config component. AbstractConfigBase
is a shared component and must be used by the AOS applications that need configuration parameters, for example
ATP and Dispatcher. The name of the component has the suffix "Base" added since there will be
an AbstractConfig component defined in the ATP core implementation which follows the naming standard there.

AbstractConfigBase implements the basic functionality to read/write configuration parameter files from/to
non-volatile storage and supply other components within AOS with the values of these parameters.

The component also handles updating the configuration values if new such values are
supplied by TCC at startup. These updates will however not be written to the configuration files.

A class derived from AbstractConfigBase needs to set up its configuration files and the parameters
contained in each file, see \ref AddingParameterFiles and \ref AddingConfigurationParameters.

AbstractConfigBase must have a concrete subclass in adaptation. An instance of this concrete subclass
will be instantiated and scheduled by the user process scheduling.

AbstractConfigBase supports the use of six config files, one each for these parameter categories:
- run-time parameters which are updated by ATP at run-time
- maintenance parameters which are updated during maintenance
- instance parameters which are specific to the vehicle
- type parameters which are specific to the vehicle type
- common parameters which are common to all instances and vehicle types
- parameters for Dispatcher

\subsection DeploymentDiagram Deployment Diagram
N/A

\subsection Dependencies Dependencies
The AbstractConfigBase component reads from and writes to non-volatile storage using
the [VFW] NVSH File interface, and is thus dependent on the NVSH and buffer functionality in VFW.

Other components have dependencies to this component because they use its public types
and methods, see \ref AbstractConfigBase Class Reference.

\latexonly \newpage \endlatexonly
\section FunctionalDesign Functional Design

\subsection Initialization Initialization

\subsubsection init init()
The \ref AbstractConfigBase::init() method reads the configuration parameters from
all configuration files that the adaptation class has set up.

Before calling init(), the core and adaptation implementations of init() must first set up
their configuration files (see \ref AddingParameterFiles) and their parameters (see \ref
AddingConfigurationParameters). Then they call init() in the base class to read the parameters.

init() checks if the version numbers read from NVSH match the expected versions.

init() checks the validity of each read parameter value, including the range.

The init() method will need to be completed before the init() method of any other component can be run,
since the config init() method reads the parameters used by other components. Since the reading of files
is done asynchronously, init() must be run repeatedly until all files are read and only then can
the initialization of other components continue. init() returns true when all files have been read.

\subsection ModeDependentOperation Mode Dependent Operation
The config component is not mode dependent.

\subsection Scheduling Scheduling
The \ref AbstractConfigBase::run() method must be called each execution cycle.

\subsubsection run run()
Some configuration parameters are changed at run time and these changes need
to be written to non-volatile storage. This is done by the run() method.

\subsection AddingParameterFiles Adding Parameter Files
The ConfigFile class manages reading and writing parameters from/to a given NVSH config
parameter file. The adaptation must create one instance of ConfigFile for each parameter file that
is to be read.

\subsection RearWriteParameterFiles Reading and Writing Parameter Files
To read parameters, AbstractConfigBase calls \ref ConfigFile::readParameters() for each instance
of ConfigFile in order to start reading the corresponding parameter file. When a file has been
read, ConfigFile calls a callback function which passes a VFW_Buffer back to AbstractConfigBase.
AbstractConfigBase then de-serializes the parameters from the content in the VFW_Buffer and stores
them in config items, see \ref ConfigItemSubclasses.

When writing parameters, AbstractConfigBase serializes config items into a VFW_Buffer and then
passes this buffer to ConfigFile by calling \ref ConfigFile::writeParameters.

In both cases, the VFW_Buffer contains the entire payload of the parameter file, i.e. the config
version followed by the config parameters.

\subsection AddingConfigurationParameters Adding Configuration Parameters
For unique identification of the different configuration parameters, there is a integer datatype called ConfigIdType.
The config ID is used for identifying every config item that is to be created, and subsequently requested.
The core and adaptation are responsible for defining their respective config item IDs in their header files.

The core and adaptation implementations of Config must use the add<type>ConfigItem() methods to add their respective
configuration parameters. These methods exist for all supported data types:
- \ref AbstractConfigBase::addBoolConfigItem()
- \ref AbstractConfigBase::addInt8ConfigItem()
- \ref AbstractConfigBase::addInt16ConfigItem()
- \ref AbstractConfigBase::addInt32ConfigItem()
- \ref AbstractConfigBase::addInt64ConfigItem()
- \ref AbstractConfigBase::addIPaddrConfigItem()
- \ref AbstractConfigBase::addStringConfigItem()
- \ref AbstractConfigBase::addUint8ConfigItem()
- \ref AbstractConfigBase::addUint16ConfigItem()
- \ref AbstractConfigBase::addUint32ConfigItem()
- \ref AbstractConfigBase::addUint64ConfigItem()

\subsection ReadingOfConfigurationParameters Getting Configuration Parameters
After initialization, core and adaptation implementations of Config use the
provided \ref AbstractConfigBase::getConfig() methods to read the configuration parameters.

The core and adaptation implementations will provide dedicated getters for other components to
retrieve parameter values.

\subsection WritingOfRunTimeConfigurationParameters Setting Runtime Configuration Parameters
External components are to define dedicated setters for each run time config parameter and each setter will call
\ref AbstractConfigBase::setRunTimeConfigValue() to store the value.

Each cycle, \ref AbstractConfigBase::run() will check if a run time parameter has been changed, and if so, call
\ref AbstractConfigBase::saveRunTimeParameters() to save the changed value(s).

\subsection UpdatingParameterValues Parameters Updated by TCC
During startup, AOS might receive updated configuration data from TCC.

Each parameter value that can be sent from TCC is identified by a global name. These
names must be set during initialization, by the core and adaptation classes, using
\ref AbstractConfigBase::setGlobalNameToConfigParameter().

The actual value is set using \ref AbstractConfigBase::putConfigString().

\subsection DisplayParametersInConsole Display Parameters in Console
The console must be able to display the configuration parameters. This is implemented in the
\ref AbstractConfigBase::consoleCall() method, where \ref AbstractConfigBase::getConfigInfo() is used for fetching
the information to be displayed.

\subsection TheBaseConfigItemClass The BaseConfigItem Class
The \ref BaseConfigItem class contains attributes common to all subclasses and methods to read them.
It also defines a number of pure virtual methods that are to be implemented in the subclasses and used
for input and output of the subclass item value.

- BaseConfigItem::readValueFromBuffer()<br />
Uses the correct vfwGet<Datatype>() method to read the item value from a buffer that contains the file data
and stores this value in the item.
- BaseConfigItem::parseStringToValue()<br />
Called when configuration data is received from TCC. This input is in text format and must be parsed and
stored as the correct item data type. If parsing to the correct data type is not possible, or if the value
is outside the boundaries of the item, the value is not stored and the method returns false.
- BaseConfigItem::getValueString()<br />
Retrieves a string representation of the item value.
- BaseConfigItem::getValueStrings()<br />
Retrieves a string representation of the item value and its boundaries. For the string item subclass,
this returns "0" for min and the text representation of the size for max. The bool item subclass returns
"0" for min and "1" for max.

\subsection ConfigItemSubclasses The <Type>ConfigItem Subclasses
These classes are subclasses of \ref BaseConfigItem. There is one subclass for each data type a parameter in the system might use.

- The different integer subclasses stores the value and the boundaries for it using their respective data type.
- The string and IP address subclasses contain a character array. The minimum and maximum values refer to the length of the string in that array.
- The bool subclass only stores the value and no boundaries.

All subclasses also have to implement the abstract methods from the base config item class,
with their respective different data types on the inside. This system must only handle fixed
length strings to always use the same amount of memory, but the vfw buffers try to optimize
space by removing null characters and instead store size and actual text only. Therefore all
strings shorter than the size set in the item will need to be padded with a special character
up to size characters when writing to the buffer, and unpadded when reading from the buffer.

@image html config_item_classes.png "BaseConfigItem and its derived classes"
@image latex config_item_classes.png "BaseConfigItem and its derived classes"

\subsection ErrorHandling Error Handling
Errors that occur during reading or writing configuration data are handled by writing a log
message and performing platform halt. The errors checked are:

- Configuration version mismatch
- Unknown parameters
- Missing parameters
- Duplicate parameters
- Parameter in wrong config file
- Reading or writing out of range values
- Writing to a read-only config file

\subsection CrossCompare Cross Comparison
ATC does not implement cross comparison, so if this is required then it must be
implemented in a derived class. AbstractConfigBase, ConfigFile and BaseConfigItem support
cross comparison through the use of CrossCompareComplex (see CrossCompare SCDS in ATP Core).

A class derived from AbstractConfigBase must add each instance of AbstractConfigBase,
ConfigFile and BaseConfigItem for cross comparison.

\section ClassDiagram Class Diagram
@image html abstract_config_base_class.png "Class diagram"
@image latex abstract_config_base_class.png "Class diagram"

\section Diagnostics Diagnostics

\subsection ConsoleCommands Console Commands

+ `config all` : lists all configuration parameters.
+ `config <n>` : lists the configuration parameter with id n.

\subsection Analyze Analyze
N/A

\section CoreAdaptation Core / Adaptation
AbstractConfigBase is the base class for the core and adaptation classes:
- AbstractConfig, the core class, implements access to the config parameters used by other core components.
- Config, the adaptation class, implements access to the config parameters used by other adaptation components.
  It is also responsible for creating the instance of the component and the instances of ConfigFile.

\section PreProcessor Pre-Processor Directives
No component-specific pre-processor directives are used in this component.

\section Traceability Traceability

\subsection SSRS Functional Requirements
The functional requirements are defined in [SSRS].

Common functional requirements are described in SCDS ATC.

The requirements relevant for this component are:

Req         | Chapter                                | Function
----------- | -------------------------------------- | --------
AOS 1040    | \ref DisplayParametersInConsole        | \ref AbstractConfigBase::consoleCall()
AOS 1143    | \ref Initialization                    | \ref ConfigFile::readParameters()
AOS 1144 S  | \ref Initialization                    | \ref BaseConfigItem::readValueFromBuffer()
AOS 1721 S  | \ref ErrorHandling                     | \ref BaseConfigItem::readValueFromBuffer()
AOS 1737 S  | \ref UpdatingParameterValues           | \ref BaseConfigItem::getGlobalName() <br /> \ref AbstractConfigBase::putConfigString()
AOS 2338    | \ref Initialization                    | \ref AbstractConfigBase::init() <br /> \ref AbstractConfigBase::getRuIp() <br /> \ref AbstractConfigBase::getRuPort() <br /> etc
AOS 2702    | \ref Initialization                    | \ref AbstractConfigBase::init()

\subsection SSAS Architectural Requirements
The architectural requirements are defined in [SSAS-APP].

Common requirements are specified in SCDS ATC.

Only the architectural requirements traced explicitly to this component are included in the table below.
Fulfilment of other architectural requirements allocated to the ATP is described in [SWAS].

Req         | Chapter                                | Function
----------- | -------------------------------------- | --------
AOS_AS 57 S | \ref Initialization                    | \ref ConfigFile::crossCompareVersion()

*/

}
