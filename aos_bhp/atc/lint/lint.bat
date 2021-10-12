@echo off
rem ======================================================================
rem Batch file to lint ATP files
rem ======================================================================
rem History
rem 2015-03-27  bhermans  Tailored for PC-Lint
rem 2016-05-30  bhermans  Lint all files or per component
rem 2018-08-03  marlundg  Extract all *.cpp files from components instead of specifying each of them.

set REPO_ROOT=P:\aos_bhp
set COMMON_DIR=%REPO_ROOT%\atc\impl
set CORE_DIR=%REPO_ROOT%\atp_core\impl
set ADAP_DIR=%REPO_ROOT%\atp_bhp\impl
set DISP_DIR=%REPO_ROOT%\dispatcher\impl

set LINT_DIR=%REPO_ROOT%\atc\lint\components_to_lint

if not exist %LINT_DIR% (
  mkdir %LINT_DIR%
)

set CROSS_COMPILER_DIR=C:\MinGW
if "%1"=="help" (
  goto :printHelp
)
if "%1"=="" (
  goto :printHelp
)

rem
rem Extract all .cpp files from all components and put into <component_name>.lnt files.
rem
dir /b /s %COMMON_DIR%\aos_allocator\*.cpp > %LINT_DIR%\aos_allocator.lnt 2> output.err

dir /b /s %COMMON_DIR%\aos_template_library\*.cpp > %LINT_DIR%\aos_template_library.lnt 2> output.err

dir /b /s %COMMON_DIR%\application\*.cpp > %LINT_DIR%\application.lnt 2> output.err

dir /b /s %COMMON_DIR%\atc_base\*.cpp > %LINT_DIR%\atc_base.lnt 2> output.err

dir /b /s %COMMON_DIR%\atc_types\*.cpp > %LINT_DIR%\atc_types.lnt 2> output.err

dir /b /s %COMMON_DIR%\fixed_size_mempool\*.cpp > %LINT_DIR%\fixed_size_mempool.lnt 2> output.err

dir /b /s %COMMON_DIR%\math\*.cpp > %LINT_DIR%\math.lnt 2> output.err

dir /b /s %COMMON_DIR%\serialization\*.cpp > %LINT_DIR%\serialization.lnt 2> output.err

dir /b /s %COMMON_DIR%\util\*.cpp > %LINT_DIR%\util.lnt 2> output.err

dir /b /s %COMMON_DIR%\vfw\*.cpp > %LINT_DIR%\vfw.lnt 2> output.err

dir /b /s %COMMON_DIR%\analyzer_if\*.cpp > %LINT_DIR%\analyzer_if.lnt 2> output.err
dir /b /s %ADAP_DIR%\analyzer_if\*.cpp >> %LINT_DIR%\analyzer_if.lnt 2> output.err

dir /b /s %COMMON_DIR%\basic_ip\*.cpp > %LINT_DIR%\basic_ip.lnt 2> output.err
dir /b /s %ADAP_DIR%\basic_ip\*.cpp >> %LINT_DIR%\basic_ip.lnt 2> output.err

dir /b /s %COMMON_DIR%\config\*.cpp > %LINT_DIR%\config.lnt 2> output.err
dir /b /s %CORE_DIR%\config\*.cpp >> %LINT_DIR%\config.lnt 2> output.err
dir /b /s %ADAP_DIR%\config\*.cpp >> %LINT_DIR%\config.lnt 2> output.err

dir /b /s %COMMON_DIR%\console\*.cpp > %LINT_DIR%\console.lnt 2> output.err
dir /b /s %ADAP_DIR%\console\*.cpp >> %LINT_DIR%\console.lnt 2> output.err

dir /b /s %CORE_DIR%\data_storage\targets\*.cpp > %LINT_DIR%\targets.lnt 2> output.err
dir /b /s %ADAP_DIR%\data_storage\targets\*.cpp >> %LINT_DIR%\targets.lnt 2> output.err

dir /b /s %CORE_DIR%\data_storage\tracks\*.cpp > %LINT_DIR%\tracks.lnt 2> output.err
dir /b /s %ADAP_DIR%\data_storage\tracks\*.cpp >> %LINT_DIR%\tracks.lnt 2> output.err

dir /b /s %CORE_DIR%\data_storage\tsetup\*.cpp > %LINT_DIR%\tsetup.lnt 2> output.err
dir /b /s %ADAP_DIR%\data_storage\tsetup\*.cpp >> %LINT_DIR%\tsetup.lnt 2> output.err

dir /b /s %CORE_DIR%\dmi_comm\dmi_handler\*.cpp > %LINT_DIR%\dmi_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\dmi_comm\dmi_handler\*.cpp >> %LINT_DIR%\dmi_handler.lnt 2> output.err

dir /b /s %CORE_DIR%\dmi_comm\dmi_channel\*.cpp > %LINT_DIR%\dmi_channel.lnt 2> output.err

dir /b /s %COMMON_DIR%\event_handler\*.cpp > %LINT_DIR%\event_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\event_handler\*.cpp >> %LINT_DIR%\event_handler.lnt 2> output.err

dir /b /s %COMMON_DIR%\log_handler\*.cpp > %LINT_DIR%\log_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\log_handler\*.cpp >> %LINT_DIR%\log_handler.lnt 2> output.err

dir /b /s %CORE_DIR%\input_output\btm_handler\*.cpp > %LINT_DIR%\btm_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\input_output\btm_handler\*.cpp >> %LINT_DIR%\btm_handler.lnt 2> output.err

dir /b /s %CORE_DIR%\input_output\loco_io\*.cpp > %LINT_DIR%\loco_io.lnt 2> output.err
dir /b /s %ADAP_DIR%\input_output\loco_io\*.cpp >> %LINT_DIR%\loco_io.lnt 2> output.err

dir /b /s %CORE_DIR%\kernel\atp_application\*.cpp > %LINT_DIR%\atp_application.lnt 2> output.err
dir /b /s %ADAP_DIR%\kernel\atp_application\*.cpp >> %LINT_DIR%\atp_application.lnt 2> output.err

dir /b /s %CORE_DIR%\kernel\message_handler\*.cpp > %LINT_DIR%\message_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\kernel\message_handler\*.cpp >> %LINT_DIR%\message_handler.lnt 2> output.err

dir /b /s %CORE_DIR%\kernel\mode_control\*.cpp > %LINT_DIR%\mode_control.lnt 2> output.err
dir /b /s %ADAP_DIR%\kernel\mode_control\*.cpp >> %LINT_DIR%\mode_control.lnt 2> output.err

dir /b /s %CORE_DIR%\position\decode\*.cpp > %LINT_DIR%\decode.lnt 2> output.err
dir /b /s %ADAP_DIR%\position\decode\*.cpp >> %LINT_DIR%\decode.lnt 2> output.err

dir /b /s %CORE_DIR%\position\odometry\*.cpp > %LINT_DIR%\odometry.lnt 2> output.err
dir /b /s %ADAP_DIR%\position\odometry\*.cpp >> %LINT_DIR%\odometry.lnt 2> output.err

dir /b /s %CORE_DIR%\position\position\*.cpp > %LINT_DIR%\position.lnt 2> output.err
dir /b /s %ADAP_DIR%\position\position\*.cpp >> %LINT_DIR%\position.lnt 2> output.err

dir /b /s %CORE_DIR%\radio_comm\radio_channel\*.cpp > %LINT_DIR%\radio_channel.lnt 2> output.err

dir /b /s %CORE_DIR%\radio_comm\radio_handler\*.cpp > %LINT_DIR%\radio_handler.lnt 2> output.err
dir /b /s %ADAP_DIR%\radio_comm\radio_handler\*.cpp >> %LINT_DIR%\radio_handler.lnt 2> output.err

dir /b /s %CORE_DIR%\supervision\brake\*.cpp > %LINT_DIR%\brake.lnt 2> output.err
dir /b /s %ADAP_DIR%\supervision\brake\*.cpp >> %LINT_DIR%\brake.lnt 2> output.err

dir /b /s %CORE_DIR%\supervision\brake_curve_calculations\*.cpp > %LINT_DIR%\brake_curve_calculations.lnt 2> output.err

dir /b /s %CORE_DIR%\supervision\supervise\*.cpp > %LINT_DIR%\supervise.lnt 2> output.err
dir /b /s %ADAP_DIR%\supervision\supervise\*.cpp >> %LINT_DIR%\supervise.lnt 2> output.err

dir /b /s %CORE_DIR%\supervision\target_calculation\*.cpp > %LINT_DIR%\target_calculation.lnt 2> output.err
dir /b /s %ADAP_DIR%\supervision\target_calculation\*.cpp >> %LINT_DIR%\target_calculation.lnt 2> output.err

dir /b /s %CORE_DIR%\support\cross_compare\*.cpp > %LINT_DIR%\cross_compare.lnt 2> output.err
dir /b /s %ADAP_DIR%\support\cross_compare\*.cpp >> %LINT_DIR%\cross_compare.lnt 2> output.err

dir /b /s %CORE_DIR%\train_gateway\tic\*.cpp > %LINT_DIR%\tic.lnt 2> output.err
dir /b /s %ADAP_DIR%\train_gateway\tic\*.cpp >> %LINT_DIR%\tic.lnt 2> output.err

dir /b /s %CORE_DIR%\train_gateway\tims\*.cpp > %LINT_DIR%\tims.lnt 2> output.err
dir /b /s %ADAP_DIR%\train_gateway\tims\*.cpp >> %LINT_DIR%\tims.lnt 2> output.err

dir /b /s %CORE_DIR%\train_gateway\vehicle_com\*.cpp > %LINT_DIR%\vehicle_com.lnt 2> output.err
dir /b /s %ADAP_DIR%\train_gateway\vehicle_com\*.cpp >> %LINT_DIR%\vehicle_com.lnt 2> output.err

dir /b /s %ADAP_DIR%\atp_main\*.cpp > %LINT_DIR%\atp_main.lnt 2> output.err

rem Concatenate all component-lint files into all.lnt
if exist %LINT_DIR%\all.lnt del %LINT_DIR%\all.lnt
for %%f in (%LINT_DIR%\*.lnt) do type %%f >> %LINT_DIR%\all.lnt

rem Common configuration files
set LINT_CONFIG=MinGW\gcc-include-path.lnt env-vc10.lnt au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt AOS.lnt WIN32.lnt %COMMON_DIR%\pathConfig.lnt %CORE_DIR%\pathConfig.lnt

rem Application-specific configuration
set a=%1
if not %a:\dispatcher\=% == %a% (
  set LINT_CONFIG=%LINT_CONFIG% %DISP_DIR%\pathConfig.lnt
) else (
  set LINT_CONFIG=%LINT_CONFIG% %ADAP_DIR%\pathConfig.lnt
)

rem Lint a specific file, component or all.
if exist %1 goto :lintfile

echo Starting to lint component: %1
lint-nt +b -vm %LINT_CONFIG% %LINT_DIR%\%1.lnt
goto :done

:lintfile
echo Starting to lint file: %1
lint-nt +b -vm %LINT_CONFIG% -u %1
goto :done

:printHelp
echo.
echo PC Lint check for either all or a selected ATP component or a single file. 
echo.
echo Usage: lint component/file
echo    component  - Component to lint. (Specify "all" to lint all ATP components )
echo.
echo    filename   - Name of a single file with source-code to lint.
echo.

:done
echo Done
