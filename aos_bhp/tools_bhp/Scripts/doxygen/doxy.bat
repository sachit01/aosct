@echo off
rem ======================================================================
rem Batch file to generate doxygen based component documentations (SCDS's)
rem ======================================================================
rem History
rem 2016-03-16  lantback  Added creation of output directory if missing
rem 2016-04-25  lantback  Added core/adaptation split
rem 2016-04-28  lantback  Added debug/open/help commands, copy/rename of pdf file
rem 2016-05-03  lantback  Added atc component handling, removed all command
rem 2016-08-28  bhidaji   Fixed the issue copying the created pdf in ATC_Core to the output directory
rem 2016-12-20  nsyed     Add support to generate SCDS for all components at once
rem 2016-12-29  nsyed     Optimized the script, removed redundant code

SETLOCAL ENABLEEXTENSIONS enabledelayedexpansion

set genDispatcher=
set viewHTML=
set viewPDF=
set openCmd=
set group=
set component=
set debug=
set container=
set repoRoot=P:\aos_bhp
set outputPath=%repoRoot%\tools_bhp\Scripts\doxygen\SCDS

rem Parse core/adap commands
rem ====================

if "%1"=="help" (
    goto :printHelp
)
if "%1"=="debug" (
    set debug=true
    shift
)
if "%1"=="open" (
    set openCmd=true
    shift
)
if "%1"=="core" (
    set genCore=true
    set container=atp_core
    shift
)
if "%1"=="adap" (
    set genAdap=true
    set container=atp_bhp
    shift
)
if "%1"=="atc" (
    set genATC=true
    set container=atc
    shift
)
if "%1"=="disp" (
    set genDispatcher=true
    set container=dispatcher
    shift
)

if "%1"=="all" (
    if %2x==x (
        echo Please specify the Output Directory path
        goto done
    )
    echo Generating SCDS for all the components...
    call doxy_all.bat %2
    goto done
)

if "%container%"=="" (
    echo ERROR: Specifier missing: core, adap, atc or disp
    goto printHelp
)

rem Check for valid parameters
rem ==========================
if %1x==x (
    echo ERROR: group name missing
    goto printHelp
)
set input=%1

rem Find the doxygen setup file
rem ============================
for /r %repoRoot%\%container%\spec\input %%x in (*doxygen_setup) do (
    set doxyFile=%%x

    for /F "tokens=6,7 delims=\" %%a in ("!doxyFile!") do (
        set group=%%a\
        set component=%%b\
    )

    rem Check if component exists in a sub-folder
    set str1=!component:doxygen=!
    if not "!str1!"=="!component!" (
        rem no subfolders exist
        set component=!group!
        set group=
    )

    if %input%\==!component! (
        set SCDSFile=%%~nxx
        set SCDSFile=SCDS_!SCDSFile:_doxygen_setup=!.pdf
        goto break
    )
)

:break
shift

rem Check if output shall be viewed
rem ===============================
if "%1"=="html" (
    set viewHtml=true
    shift
)
if "%1"=="pdf" (
    set viewPdf=true
    shift
)

if "%debug%"=="true" (
    echo ===============================
    echo Debug info
    echo genDispatcher=    %genDispatcher%
    echo viewHTML=         %viewHTML%
    echo viewPDF=          %viewPDF%
    echo openCmd=          %openCmd%t
    echo container=        %container%
    echo group=            %group%
    echo component=        %component%
    echo outputPath =      %outputPath%
    echo ECHO turned on
    echo ===============================
    echo ON
)

rem Create output directories if not existing
rem =========================================
if not exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component! (
    mkdir %repoRoot%\%container%\spec\output\%container%_all\!group!!component!
)

rem Jump directly to view result file if open specified
rem ===================================================
if "%openCmd%"=="true" goto :open

rem Delete refman.pdf at this point (if locked/open better fail here than later)
rem ============================================================================
if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
    del %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf
)
    
if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
    echo ERROR: Unable to delete file: %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf > %repoRoot%\%container%\spec\output\!group!!component!pdfLog.txt
)

echo Run doxygen for component : !group!!component!

doxygen !doxyFile! >%repoRoot%\%container%\spec\output\%container%_all\!group!!component!doxyLog.txt 2>&1

echo Run LaTeX PDF generator
call %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\make.bat > %repoRoot%\%container%\spec\output\%container%_all\!group!!component!pdfLog.txt 2>&1

rem Check that generation went ok
rem =============================
if errorlevel 1 goto end
echo SCDS successfully generated for component: %component%
echo Resulting SCDS is stored in %outputPath%

rem Rename and copy PDF file of component
rem =====================================
if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
    if not exist %outputPath%\%container% mkdir %outputPath%\%container%
    echo %outputPath%\%container%\%SCDSFile%
    copy /Y %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf %outputPath%\%container%\%SCDSFile% >> %repoRoot%\%container%\spec\output\%container%_all\!group!!component!pdfLog.txt 2>&1
    echo Done.
    echo .
)

rem View files, if asked for
rem =======================
:open
if "%viewHtml%"=="true" (
    if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!html\index.html %repoRoot%\%container%\spec\output\%container%_all\!group!!component!html\index.html
)
if "%viewPdf%"=="true" (
    if exist %outputPath%\%container%\!SCDSFile! start %outputPath%\%container%\!SCDSFile!
)

rem Done!
rem =====
goto :done

:printHelp
@if "%debug%"=="true" goto quiteEnd
echo.
echo Automated generation of SCDS files for the components in ATP and Dispatcher. Generates html 
echo and pdf files for  ATC, Core, Adaptation or Dispatcher components. 
echo.
echo Usage 1: doxy [core ^| adap ^| atc ^| disp] component [html] [pdf]
echo    core       - Generate core component
echo    adap       - Generate adaptation component
echo    atc        - Generate atc component
echo    component  - Component to generate documentation for
echo    html       - View the generated html file
echo    pdf        - view the generated pdf file
echo.
echo Usage 2: doxy all [path]
echo          This generates the SCDS for all the components and stores them 
echo          in the location specified as the input parameter (path) under the folder SCDS.
echo.

:done
echo.
echo =================
echo Done

:end
set viewHTML=
set viewPDF=
set openCmd=
set group=
set component=
set debug=
set outputPath=
set container=

@:quiteEnd
