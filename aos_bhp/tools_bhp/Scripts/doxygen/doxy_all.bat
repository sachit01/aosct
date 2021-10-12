@echo off
rem ======================================================================
rem Batch file to generate doxygen based component documentations (SCDS's)
rem ======================================================================
rem History
rem 2016-12-21 nsyed Created

@setlocal enabledelayedexpansion enableextensions

rem Setup default values to the Local variables
rem =======================================================
set containerList=atc,atp_core,atp_bhp,dispatcher
set repoRoot=P:\aos_bhp
set outputPath=%repoRoot%\tools_bhp\Scripts\doxygen\SCDS
set /a pass=0
set /a fail=0

rem Parse input variables
rem ======================

if not %1x==x (
    set outputPath=%1
    if not exist !outputPath! (
        echo Please enter a valid location to store the results
        goto end
    )
    set outputPath=!outputPath!
)

echo Removing old files and logs...
rem Clear old logs and files if any
if exist %outputPath%\failingSCDS.txt del %outputPath%\failingSCDS.txt
if exist %outputPath% rd /s /q %outputPath%

if exist %repoRoot%\atc\spec\output\atc_all del /s /q %repoRoot%\atc\spec\output\atc_all\*.* > nul 2>&1
if exist %repoRoot%\atp_core\spec\output\atp_core_all del /s /q %repoRoot%\atp_core\spec\output\atp_core_all\*.* > nul 2>&1
if exist %repoRoot%\atp_bhp\spec\output\atp_bhp_all del /s /q %repoRoot%\atp_bhp\spec\output\atp_bhp_all\*.* > nul 2>&1
if exist %repoRoot%\dispatcher\spec\output\dispatcher_all del /s /q %repoRoot%\dispatcher\spec\output\dispatcher_all\*.* > nul 2>&1


echo Generating SCDS for all the components...
rem Iterate over the doxygen setup files for each component
rem =======================================================

for %%c in (%containerList%) do (
    set container=%%c
    call :genSCDS
)
goto verify

:genSCDS
for /r %repoRoot%\%container%\spec\input %%x in (*doxygen_setup) do (
    set moveToNextComp=false
    set doxyFile=%%x
    set SCDSFile=%%~nxx
    set SCDSFile=SCDS_!SCDSFile:_doxygen_setup=!.pdf

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

    if not "template\"=="!component!" (
        rem Create Output directories if they don't exist
        rem =================================================
        if not exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component! (
            mkdir %repoRoot%\%container%\spec\output\%container%_all\!group!!component!
        )    

        rem Delete refman.pdf at this point (if locked/open better fail here than later)
        rem ============================================================================
        if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
           del %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf
        )

        if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
            echo ERROR: Unable to delete file: %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf > %repoRoot%\%container%\spec\output\%container%_all\!group!!component!pdfLog.txt
        )

        echo Run doxygen for component : !group!!component!
        rem =================================================

        doxygen !doxyFile! >%repoRoot%\%container%\spec\output\%container%_all\!group!!component!doxyLog.txt 2>&1

        echo Run LaTeX PDF generator...
        rem ============================

        rem =========================================================

        cd %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex

        if !moveToNextComp!==false (        
           call make > %repoRoot%\%container%\spec\output\%container%_all\!group!!component!pdfLog.txt 2>&1
        )

        rem Rename and copy PDF file of component
        rem =====================================
        if exist %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf (
            if not exist %outputPath%\pdf\%container% mkdir %outputPath%\pdf\%container%
            copy /Y %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\refman.pdf %outputPath%\pdf\%container%\!SCDSFile! >> %repoRoot%\%container%\spec\output\%container%_all\!group!!component!pdfLog.txt 2>&1
            rem Clean-up
            del /s /q %repoRoot%\%container%\spec\output\%container%_all\!group!!component!latex\*.* > nul 2>&1
            echo Done. Moving on to the next component...
            echo .
        )
    )
)

exit /B

:nextComponent
set moveToNextComp=true
exit /B

:verify
echo Verify if the SCDS are generated for all the components
echo ========================================================

for %%c in (%containerList%) do (
    set container=%%c
    set tempExcpectedSCDS=%repoRoot%\tools_bhp\scripts\doxygen\expected_!container!_SCDS.txt
    call :start_verify
)


echo Generate the "master" index to all the html SCDSs
echo ========================================================
for %%c in (%containerList%) do (

    cd %repoRoot%\%%c\spec\input\%%c_all
    doxygen %%c_all_doxygen_setup > %repoRoot%\%%c\spec\output\%%c_all\doxyLog.txt 2>&1
    echo %%c : Done!

    echo Cleaning-up..
    rem Remove duplicate folders TODO: These folders should not be created in the first place, update script to skip creating these folders
    if exist %repoRoot%\%%c\spec\output\%%c_all\%%c_all rd /s /q %repoRoot%\%%c\spec\output\%%c_all\%%c_all
    
    rem Copy the html files to the specified Output folder
    xcopy %repoRoot%\%%c\spec\output\%%c_all %outputPath%\%%c_all /s /i > %repoRoot%\%%c\spec\output\%%c_all\doxyLog.txt 2>&1

)
goto done

:start_verify
for /F %%l in (%tempExcpectedSCDS%) do (
    set findSCDSFile=%%l
    set found=false
    call :find_SCDS
    if !found!==false (
        set /a fail+=1
        echo !findSCDSFile! >> %outputPath%\failingSCDS.txt
    )
)
exit /B

:find_SCDS
for /r %outputPath%\pdf\%container% %%x in (*.pdf) do (
    if "%%~nxx"=="%findSCDSFile%" (
        set /a pass+=1
        set found=true
        goto continue
    )
)
:continue
exit /B

:done

echo ========================================================
echo Results
echo ========================================================
echo Total SCDS generated successfully : %pass%
echo Number of SCDS generation(s) failed : %fail%
echo The following SCDS generation(s) failed :
type %outputPath%\failingSCDS.txt

:end
