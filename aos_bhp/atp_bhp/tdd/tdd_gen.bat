@echo off
rem ======================================================================
rem Batch file to generate doxygen based TDDs
rem ======================================================================
rem History
rem 2017-02-14  marlundg  First version (stripped version of doxy.bat for generating SCDS)

SETLOCAL ENABLEEXTENSIONS enabledelayedexpansion

set feature=
set debug=
set repoRoot=P:\aos_bhp
set container=atp_bhp

if "%1"=="debug" (
    set debug=true
    shift
)

set feature=%1
set doxyFile=%repoRoot%\%container%\tdd\%1\%1_doxygen_setup

cd %feature%

if "%debug%"=="true" (
    echo ===============================
    echo Debug info

    echo feature=        %feature%
    echo doxyFile =        %doxyFile%
    echo ECHO turned on
    echo ===============================
    echo ON
)

if "%feature%"=="" (
    goto printHelp
)


rem Delete refman.pdf at this point (if locked/open better fail here than later)
rem ============================================================================
if exist %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf (
    del %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf
)
    
if exist %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf (
    echo ERROR: Unable to delete file: %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf > %repoRoot%\%container%\tdd\%feature%\%feature%_pdfLog.txt
)

echo Run doxygen for feature : %feature%

doxygen %doxyFile% >%repoRoot%\%container%\tdd\%feature%\%feature%_doxyLog.txt 2>&1

echo Run LaTeX PDF generator
cd %repoRoot%\%container%\tdd\%feature%\latex
call make > %repoRoot%\%container%\tdd\%feature%\%feature%_pdfLog.txt 2>&1

rem Rename and copy PDF file of feature
rem =====================================
if exist %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf (
    copy /Y %repoRoot%\%container%\tdd\%feature%\latex\refman.pdf %repoRoot%\%container%\tdd\%feature%\%feature%.pdf >> %repoRoot%\%container%\tdd\%feature%\%feature%_pdfLog.txt 2>&1
    echo Done.
    echo .
)

rem Check that generation went ok
rem =============================
if errorlevel 1 goto fault
echo TDD successfully generated for function: %feature%
echo Resulting TDD is stored in %repoRoot%\%container%\tdd\%feature%
goto :done

:fault
echo Something went wrong, please check  %repoRoot%\%container%\tdd\%feature%\%feature%_pdfLog.txt or %repoRoot%\%container%\tdd\%feature%\%feature%_doxyLog.txt
goto :done

:printHelp
echo.
echo Automated generation of TDD-pdf file for a specified feature.
echo.
echo Usage: tdd_gen [debug] ^<feature^>
echo        debug       - Debug script
echo        ^<feature^>   - The feature-directory (located in %repoRoot%\%container%\tdd\ )
echo.
echo The feature directory shall contain:
echo   ^<feature^>_doxygen_setup
echo   ^<feature^>_specification.hpp
echo.
echo  - See %repoRoot%\%container%\tdd\template for example -
echo.

:done

set feature=
set debug=
set container=

cd ..