@echo off
rem ======================================================================
rem Batch file to open doxygen based component documentations (SCDS's)
rem ======================================================================
rem History
rem 2016-03-17  lantback  Created
rem

if %1==help goto printHelp
if %1==-help goto printHelp
if %1==--help goto printHelp

echo.
echo Open doxygen generated documentation for component: %1\%2
echo.

rem Check for valid parameters
rem ==========================
if %1x==x goto groupMissing
if %2x==x goto componentMissing

rem Check that component exists
rem ===========================
if not exist p:\atp_core\impl\%1 goto groupNonExisting
if not exist p:\atp_core\impl\%1\%2 goto componentNonExisting

rem Check if output directories exist, if not create
rem ================================================
if not exist p:\atp_core\spec\output\%1 goto ouputNotExisting
if not exist p:\atp_core\spec\output\%1\%2 goto ouputNotExisting

rem Open file
rem ================================================
if %3==html goto viewHtml
if %3==HTML goto viewHtml
goto viewPdf

:viewHtml
if not exist p:\atp_core\spec\output\%1\%2\html\index.html goto outputNotExisting
p:\atp_core\spec\output\%1\%2\html\index.html
goto end

:viewPdf
if not exist p:\atp_core\spec\output\%1\%2\latex\refman.pdf goto outputNotExisting
start p:\atp_core\spec\output\%1\%2\latex\refman.pdf
goto end



:groupMissing
echo ERROR: group missing
goto printHelp

:componentMissing
echo ERROR: component missing
goto printHelp

:groupNonExisting
echo ERROR: group does not match a valid directory
goto printHelp

:componentNonExisting
echo ERROR: component does not match a valid directory
goto printHelp

:outputNotExisting
echo ERROR: Doxygen generated file does not exist
goto printHelp


:printHelp
echo.
echo Usage: doxy group component
echo    group      - Group where component is placed
echo    component  - Component to generate documentation for
echo.


:end