

##* %name: startLintAll.sh %
## %version: 1 %
## %created_by: bhermans %
## %date_created: 2015-04-28 10:59 %
## %Creation date of original object: Fri Nov 17 13:53:54 2000 %
## history:
## 1.5    2015-01-30    azacher     use different stack files, create managment report AND full debug report



## short output non verbose
/localProducts/FlexeLint/src/flexelint +b -vm         X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJx86.tmp &
/localProducts/FlexeLint/src/flexelint +b -vm         X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJx86.tmp &
                                                 
/localProducts/FlexeLint/src/flexelint +b -vm     freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJbsd.tmp &
/localProducts/FlexeLint/src/flexelint +b -vm     freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJbsd.tmp &
                                                 
/localProducts/FlexeLint/src/flexelint +b -vm       LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_client.lnt > lintClientJlinux.tmp &
/localProducts/FlexeLint/src/flexelint +b -vm       LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt vio_project.lnt files_server.lnt > lintServerJlinux.tmp &


## long output verbose + stack report 
/localProducts/FlexeLint/src/flexelint +b -voifhm     X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntCx86.tmp)" vio_project.lnt files_client.lnt > lintClientJx86.tmpvoifhm &
/localProducts/FlexeLint/src/flexelint +b -voifhm     X86/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSx86.tmp)" vio_project.lnt files_server.lnt > lintServerJx86.tmpvoifhm &

/localProducts/FlexeLint/src/flexelint +b -voifhm freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntCbsd.tmp)" vio_project.lnt files_client.lnt > lintClientJbsd.tmpvoifhm &
/localProducts/FlexeLint/src/flexelint +b -voifhm freeBSD/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSbsd.tmp)" vio_project.lnt files_server.lnt > lintServerJbsd.tmpvoifhm &

/localProducts/FlexeLint/src/flexelint +b -voifhm   LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntClin.tmp)" vio_project.lnt files_client.lnt > lintClientJlinux.tmpvoifhm &
/localProducts/FlexeLint/src/flexelint +b -voifhm   LINUX/gcc-include-path.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt +"stack(&file=stacklntSlin.tmp)" vio_project.lnt files_server.lnt > lintServerJlinux.tmpvoifhm &

