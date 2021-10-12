

rem * %name: startLintAll.bat %
rem %version: 1.1 %
rem %created_by: bhermans %
rem %date_created: 2015-10-28 14:46 %
rem %Creation date of original object: Fri Nov 17 13:53:54 2000 %
rem history:
rem     2015-03-27    bhermans     Tailored for PC-Lint

set CROSS_COMPILER_DIR=D:\GSP-2\CROSS_RHEL4_x86_Linux_ppc

rem short output non verbose
rem lint-nt +b -vm         X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJx86.tmp
rem lint-nt +b -vm         X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJx86.tmp
                                                 
rem lint-nt +b -vm     freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJbsd.tmp
rem lint-nt +b -vm     freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJbsd.tmp
                                                 
rem lint-nt +b -vm       LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJlinux.tmp
lint-nt +b -vm       LINUX\gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJlinux.tmp
lint-nt +b -vm       LINUX\gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJlinux.tmp


rem long output verbose + stack report 
rem lint-nt +b -voifhm     X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntCx86.tmp)" vio_project.lnt files_client.lnt > lintClientJx86.tmpvoifhm
rem lint-nt +b -voifhm     X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSx86.tmp)" vio_project.lnt files_server.lnt > lintServerJx86.tmpvoifhm

rem lint-nt +b -voifhm freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntCbsd.tmp)" vio_project.lnt files_client.lnt > lintClientJbsd.tmpvoifhm
rem lint-nt +b -voifhm freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSbsd.tmp)" vio_project.lnt files_server.lnt > lintServerJbsd.tmpvoifhm

rem lint-nt +b -voifhm   LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntClin.tmp)" vio_project.lnt files_client.lnt > lintClientJlinux.tmpvoifhm
rem lint-nt +b -voifhm   LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSlin.tmp)" vio_project.lnt files_server.lnt > lintServerJlinux.tmpvoifhm

