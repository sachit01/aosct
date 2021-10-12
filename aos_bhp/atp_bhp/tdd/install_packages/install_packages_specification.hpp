/**
\if AsMainPage
\mainpage Install Packages
\endif

\section VersionLog Version Log

Version | Date       | Description                                   | Signature
------- | --------   | --------------------------------------------- | ---------
0.1     | 2018-04-24 | Preliminary version                           | csundin
0.2     | 2018-05-03 | Further work                                  | csundin


\section Abbreviations Abbreviations

Abbreviation   | Definition
-------------- | ----------
HIL            | Hardware In the Loop (uses a target test build with simulated environment)
SIL            | Software In the Loop (uses a Windows test build with simulated environment)
TDD            | Technical Design Description
VSIM           | Vehicle SIMulation (uses a target build with real hardware environment)


\section Introduction Introduction

Install packages will be created in order to make distribution and installation
of the AOS SW easier and more reliable. For Unix-like target systems, the .deb
package format will be used. For Windows targets, Windows install packages will be
created.

A .deb package is a Unix "ar" archive that contains two Unix "tar" archives plus
one more more files containing metadata. This means that it's easy to create .deb
packages using standard Unix commands. For details, see: https://en.wikipedia.org/wiki/Deb_(file_format)


\subsection Design Design Overview

Install packages will be created for the target systems CPU A, CPU B, CPU C and DMI.
Additionally, variants of the packages will be created for the HIL and VSIM configurations.

In a later stage, Windows packages for the SIL configuration will also be
created, but using a Windows-specific format, which is TBD.


\subsection RequirementsTraceabilityMatrix Requirements Traceability Matrix 

Req          | Short requirement description         | Status
------------ | ------------------------------------- | ------
GSP2_SIR12   | The SW application binaries and files (e.g. configuration, runtime and maintenance data files) shall be CRC protected over the complete life cycle. | Partially covered
GSP2_SIR13   | Before the application is started or copied into a RAM Disk (please refer to GSP2_SIR14) the CRC of the application binaries and files (e.g. configuration, runtime and maintenance data files) shall be verified. In the case of a mismatch the application shall not start or shall go to halt state. | Partially covered
GSP2_SIR14   | If the software application is copied from permanent storage into a RAM Disk before it is loaded the integrity shall be verified by checking the CRC. | Covered
GSP2_SIR15   | The SW Application should use the Non-Volatile Storage Handler to store application data (runtime data, maintenance data and configuration data). The NVSH Handler provides routines to verify the files during runtime. The SW application shall call these routines once per hour. | Not covered
GSP2_SIR16   | The SW application shall cross compare its version information directly after start up. In the case of mismatch the SW application shall write a log message followed by a call of the VFW HALT Macro. | Not covered
GSP2_SIR17   | The version information of configuration, runtime and maintenance data shall be cross compared directly after start up. In the case of mismatch the SW application shall write a log message followed by a call of the VFW HALT Macro. | Not covered
GSP2_SIR36   | The SW Application shall ensure that a matching version of all internally interfaced SW Applications is installed on the target.Therefore the SW Application shall perform a version check between the expected version and the installed version of the interfaced SW Application. | Partially covered
GSP2_SIR36_1 | GSP User Application Refinement: That includes all SW Applications part of the GSP-2 as well as other Software Applications executed on GSP-2. | Partially covered
GSP2_SIR36_1 | The AOS shall perform a version check for the SDP and Vital IO Handler versions and also installed binaries (for example: Dispatcher, Radio) versions. | Not covered
GSP2_SIR36_1 | The AOS shall perform a version check for all used shared libraries (except OS libraries for allowed POSIX functions, see AOS 1522). A run-time version check for used static libraries is however not requested. Usage of the correct version and binary of a static library must be ensured during the build process of the application, see also AOS 1364. | Not covered
GSP2_SIR36_2 | GSP Refinement1: The SW Application shall call the VFW_ASSERT_VERSION macro to ensure that the matching VFW Version is installed on the target. In case of a mismatch the macro will enter the Halt state. | Not covered
GSP2_SIR37   | The SW Application shall ensure that a matching version of the configuration file, runtime data file and maintenance data file is loaded into each item. In the case of a mismatch the SW application shall create a log message and the System shall be put into the Halt State by calling the VFW Halt macro. | Not covered
GSP2_SIR40   | The version of the installed SW, configuration, runtime data and maintenance data files shall be verified after installation. | Partially covered
GSP2_SIR18   | It shall be possible to identify loaded files by name and version. | Not covered
GSP2_SIR18   | The Version Information shall be hard coded and represented by a variable for holding the version. | Partially covered


\section SystemArchitecturalDesign System Architectural Design

Not applicable


\section DetailedDescriptionOfComponents Detailed Description of Components

\subsection DebPackageCreation Creation of .deb packages

The .deb packages will be created using the Linux commands "ar", "tar" and "gzip",
according to the .deb format specification, see https://en.wikipedia.org/wiki/Deb_(file_format)

In short:

<ul>
<li>Metadata and installation scripts will be stored in the compressed archive "control.tar.gz"</li>
<li>Program and data files will be stored in the compressed archive "data.tar.gz"</li>
<li>The above-mentioned archives will be stored in the deliverable .deb file as an "ar" archive</li>
</ul>


\subsection IntegrityChecks Checking package integrity

Checksums of the programs and scripts will be calculated when creating the packages. These checksums
will be calculated again and checked after installation of the applications and before starting the
applications.

Assumption: The data files are checked by the application(s) that use them.

Notes:
* opkg-cl does not use "md5sums" to check package integrity
* in the install script, check package integrity before installing?
* use md5 or crc64 for files after install?

Details TBD in Phase 2


\subsection VersionChecks Checking software versions

TBD in Phase 2


\subsection DmiPackageCreation Creation of DMI package

TBD in Phase 4


\subsection SilPackageCreation Creation of SIL package

TBD in Phase 5


\subsection BuildProcedures Build Procedures

\subsubsection BuildProcedure Build Procedure for CPU A/B/C

Log in to the virtual "FedoraBuildMachine" and issue the following commands:

\code
cd ~/p_drive/atp_bhp/target
make compile_pdrive_noclean
make generate-deployment VER=version_string
\endcode

The deliverables can now be found in:

\code
~/p_drive/atp_bhp/target/_tmp_vsim
\endcode


\subsubsection DmiBuildProcedure Build Procedure for DMI

TBD in Phase 4


\subsubsection SilBuildProcedure Build Procedure for SIL

TBD in Phase 5


\subsection InstallProcedures Installation Procedures

\subsubsection TargetInstallProcedure Installation Procedure for the target system

Installation will be done according to section 5.3.4 in the GSP-2 User Manual [3NSS012264D0079].

In short:

<ul>
<li>Set the IP address on the PC to 192.168.2.1</li>
<li>Connect the Ethernet cable to the PC and to a connector on the left side on the Net I/O board (i.e. the 192.168.2.x network)</li>
<li>Use WinSCP to transfer the deliverables to CPU A, B and C</li>
<li>Use PuTTY to log in to each CPU and run the installation script</li>
</ul>


\subsubsection VsimInstallProcedure Installation Procedure for VSIM

TODO: this only works in Gbg:

Connect to the VSIM network using VPN.
Log in to the virtual "FedoraBuildMachine" and issue the following commands:

\code
cd ~/p_drive/atp_bhp/target
make deploy-vsim-deb VER=version_string
\endcode


\subsubsection HilInstallProcedure Installation Procedure for HIL

TODO: this only works in Gbg:

Connect to the HIL network using VPN.
Log in to the virtual "FedoraBuildMachine" and issue the following commands:

\code
cd ~/p_drive/atp_bhp/target
make deploy-hil-deb VER=version_string
\endcode

\subsubsection DmiInstallProcedure Installation Procedure for DMI

TBD in Phase 4


\subsubsection SilInstallProcedure Installation Procedure for SIL

TBD in Phase 5


\subsection ListOfDeliverables List of Deliverables

\subsubsection TargetDeliverables Target Deliverables

The deliverables consist of a .deb package and an installation script for each of the CPUs A, B and C.
Example for release version 1.1.0:

\code
atp_bhp_cpu_a_1.1.0.deb
install_atp_bhp_cpu_a_1.1.0.sh

atp_bhp_cpu_b_1.1.0.deb
install_atp_bhp_cpu_b_1.1.0.sh

atp_bhp_cpu_c_1.1.0.deb
install_atp_bhp_cpu_c_1.1.0.sh
\endcode


\subsubsection DmiDeliverables DMI Deliverables

TBD in Phase 4


\subsection ListOfContents Package Contents

\subsubsection VsimContents VSIM Package Contents

CPU A:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/announce.sh	                    | /opt/bin/
/atp_bhp/target/cpu_a/startup_system_generic.sh | /opt/bin/
/atp_bhp/target/cpu_a/vsim/startup_system.sh    | /opt/bin/
/atp_bhp/target/cpu_a/vsim/atp_bhp_vsim         | /opt/bin/aos/
/settimeofday/Linux_ppc/settimeofday            | /opt/bin/aos/
/TimeSyncServer/bin/Linux_ppc/TimeSyncServer    | /opt/bin/aos/

CPU B:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/linger.sh                       | /opt/bin/
/atp_bhp/target/cpu_b/startup_system_generic.sh | /opt/bin/
/atp_bhp/target/cpu_b/vsim/startup_system.sh    | /opt/bin/
/atp_bhp/target/cpu_b/vsim/atp_bhp_vsim         | /opt/bin/aos/
/settimeofday/FreeBSD_arm/settimeofday          | /opt/bin/aos/
/TimeSyncServer/bin/FreeBSD_arm/TimeSyncServer  | /opt/bin/aos/

CPU C:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/announce.sh                     | /opt/bin/
/atp_bhp/target/cfg_data.bin                    | /optdata/data/aos/
/atp_bhp/target/mnt_data.bin                    | /optdata/data/aos/
/atp_bhp/target/rt_data.bin                     | /optdata/data/aos/
/dispatcher/target/cfg_disp_data.bin            | /optdata/data/aos/
/dispatcher/target/startup_system_generic.sh    | /opt/bin/
/dispatcher/target/vsim/startup_system.sh       | /opt/bin/
/dispatcher/target/vsim/dispatcher_vsim         | /opt/bin/aos/
/TimeSyncServer/bin/Linux_ppc/TimeSyncServer    | /opt/bin/aos/


\subsubsection HilContents HIL Package Contents

CPU A:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/announce.sh                     | /opt/bin/
/atp_bhp/target/cpu_a/startup_system_generic.sh | /opt/bin/
/atp_bhp/target/cpu_a/hil/startup_system.sh     | /opt/bin/
/atp_bhp/target/cpu_a/hil/atp_bhp_hil           | /opt/bin/aos/

CPU B:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/linger.sh                       | /opt/bin/
/atp_bhp/target/cpu_b/startup_system_generic.sh | /opt/bin/
/atp_bhp/target/cpu_b/hil/startup_system.sh     | /opt/bin/
/atp_bhp/target/cpu_b/hil/atp_bhp_hil           | /opt/bin/aos/

CPU C:

File in repository                              | Installation path
----------------------------------------------- | -----------------
/atp_bhp/target/announce.sh                     | /opt/bin/
/atp_bhp/target/cfg_data.bin                    | /optdata/data/aos/
/atp_bhp/target/mnt_data.bin                    | /optdata/data/aos/
/atp_bhp/target/rt_data.bin                     | /optdata/data/aos/
/dispatcher/target/cfg_disp_data.bin            | /optdata/data/aos/
/dispatcher/target/startup_system_generic.sh    | /opt/bin/
/dispatcher/target/hil/startup_system.sh        | /opt/bin/
/dispatcher/target/hil/dispatcher_hil           | /opt/bin/aos/


\subsubsection DmiContents DMI Package Contents

TBD in Phase 4


\subsubsection SilContents SIL Package Contents

TBD in Phase 5


\section UserInterfaceDesign User Interface Design

Not applicable


\section ImplementationDistribution Task Distribution

\subsection Phase1 Phase 1: Preliminary .deb packages for VSIM

<ul>
<li>Test and correct any errors in the Make files and build scripts</li>
<li>Update the installation instruction 1DOC-1025201</li>
</ul>

\subsection Phase2 Phase 2: Integrity checks for .deb packages

<ul>
<li>Determine how to implement the integrity and version checks and update the TDD</li>
<li>Calculate checksums in the build scripts and check them during installation</li>
<li>Check software versions during installation</li>
<li>Update the installation instruction 1DOC-1025201</li>
</ul>

\subsection Phase3 Phase 3: .deb packages for HIL

<ul>
<li>Remove TimeSyncServer from the HIL and EMD packages</li>
<li>Test and correct any errors in the Make files and build scripts</li>
</ul>

\subsection Phase4 Phase 4: Windows install packages for DMI

<ul>
<li>Create a Windows install package containing DMI, NJRU, BTAutoRun and any associated data files</li>
<li>Test and correct any errors in the DMI install package</li>
<li>Update the installation instruction 1DOC-1025202</li>
</ul>

\subsection Phase5 Phase 5: SIL

<ul>
<li>Investigate which distribution format to use</li>
<li>Update this document with the findings</li>
<li>Create and test the distribution</li>
</ul>


\section AdditionalMaterial Additional Material

Document number | Document title
--------------- | --------------
3NSS012264D0079 | GSP-2 User Manual
1DOC-1025201    | Installation Instruction AOS_IF150_BHP
1DOC-1025202    | DMI Installation and configuration

*/
