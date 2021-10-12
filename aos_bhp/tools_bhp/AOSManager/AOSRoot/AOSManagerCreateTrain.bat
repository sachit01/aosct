echo off
SetLocal EnableDelayedExpansion
rem History
rem Date       Sign 	Comment                                    
rem 2017-10-16 akushwah	created for AOSManager tools

if [%1] == [] goto usage
if [%2] == [] goto usage
if [%3] == [] goto usage
if %1 LSS 1 goto usage
if %1 GTR 50 goto usage
if %2 LSS 1 goto usage
if %2 GTR 350 goto usage
if %3 LSS %2 goto usage rem The offset cannot be smaller than number of vehicles
goto ok

:usage
echo Usage:
echo AddTrain [2..50] [Number of vehicles (1-350)] [Train Node-address offset (>=Number of vehicles)]
echo Example:
echo AddTrain 2 20 30 , will generate a train2 directory setup with 1 loco + 19 cars with node addresses 31-50.
echo AddTrain 3 20 30 , will generate a train3 directory setup with 1 loco + 19 cars with node addresses 61-80.
goto end

:ok
echo OK to create Train %1 in %CD%\Train%1? (Ctrl-C to abort, Enter to continue)
pause

rem create the directories and copy files
xcopy /Y TestATP Train%1\

rem md Train%1\AOSPC
rem if not exist "AOSPC.exe" copy AOSPC\AOSPC.exe %CD%
rem copy AOSPC\LCSSimDLL.dll Train%1\AOSPC
copy AOSPC\AOSPC.ini Train%1\AOSPC.ini
rem copy AOSPC\LocoSimDLL.dll Train%1\AOSPC
rem copy AOSPC\TimsSimDLL.dll Train%1\AOSPC
rem copy AOSPC\SiteDataExtDLL.dll Train%1\AOSPC

rem md Train%1\DMI
rem copy DMI\DMI.exe Train%1\DMI
copy DMI\DMI.ini Train%1\DMI.ini
rem copy DMI\MMICL.DLL Train%1\DMI
rem copy DMI\Swedish.ini Train%1\DMI
rem copy DMI\English.ini Train%1\DMI
rem copy DMI\*.bmp Train%1\DMI

rem md Train%1\N-JRU
rem copy N-JRU\N-JRU.exe Train%1\N-JRU
rem copy N-JRU\N-JRU.ini Train%1\N-JRU

rem create log-directory for N-JRU
md Train%1\Log

rem edit the ini-files
rem calculate port numbers
set /a ATP_AnalyzerIF_Port=30060 + (%1 * 100)
set /a ATP_MMI_Channel1=30030 + (%1 * 100)
set /a ATP_MMI_Channel2=30036 + (%1 * 100)
set /a ATP_NJRU_Port=30031 + (%1 * 100)
set /a ATP_RU_Port=30080 + (%1 * 100)
set /a ATP_Console_Port=30065 + (%1 * 100)
set /a ATP_TCC_Radio_Channel1=30032 + (%1 * 100)
set /a ATP_TCC_Radio_Channel2=30033 + (%1 * 100)
set /a ATP_TCC_Radio_Channel3=30034 + (%1 * 100)
set /a SimCod_Odometry_Config=30070 + (%1 * 100)
set /a SimCod_Odometry_Config_Response=30071 + (%1 * 100)
set /a SimCod_Odometry_Meas=30072 + (%1 * 100)
set /a Sim_VIOHSim=30090 + (%1 * 100)
set /a Speed_Sim=30091 + (%1 * 100)
set /a ATP_LCS_Port=30050 + (%1 * 100)
set /a ATP_TIMS_Port=30093 + (%1 * 100)
set /a BTMHdlrOpcStatus=30041 + (%1 * 100)
set /a BTMHdlrOpcBtmTel=30040 + (%1 * 100)
set /a BTMHdlrCommand=30042 + (%1 * 100)
set /a BTMHdlrOpcAppStatus=30044 + (%1 * 100)
set /a BTMHdlrOpcClockStatus=30045 + (%1 * 100)
set /a BTMHdlrTigrisOffset=30047 + (%1 * 100)
set /a Sim_OPCSim=30092 + (%1 * 100)
set /a ATP_OBRD_Port=30051 + (%1 * 100)


Support\EditIniFile COMMON_DATA.BIN AnalyzerIFPort %ATP_AnalyzerIF_Port% Train%1\test_atp_cfg.ini
Support\EditIniFile Simulation portNrDMIChannel1 %ATP_MMI_Channel1% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrDMIChannel2 %ATP_MMI_Channel2% Train%1\test_atp.ini
Support\EditIniFile COMMON_DATA.BIN NjruPort %ATP_NJRU_Port% Train%1\test_atp_cfg.ini
Support\EditIniFile COMMON_DATA.BIN RuPort %ATP_RU_Port% Train%1\test_atp_cfg.ini
Support\EditIniFile COMMON_DATA.BIN ConsolePort %ATP_Console_Port% Train%1\test_atp_cfg.ini
Support\EditIniFile Simulation portNrRadiochannel1 %ATP_TCC_Radio_Channel1% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrRadiochannel2 %ATP_TCC_Radio_Channel2% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrRadiochannel3 %ATP_TCC_Radio_Channel3% Train%1\test_atp.ini
Support\EditIniFile INSTANCE_DATA.BIN RadioId %1 Train%1\test_atp_cfg.ini
Support\EditIniFile Simulation portNrSimCodOdometryConfig %SimCod_Odometry_Config% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrSimCodOdometryConfigResponse %SimCod_Odometry_Config_Response% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrSimCodOdometryMeas %SimCod_Odometry_Meas% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrSimVIOHSim %Sim_VIOHSim% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrSpeedSim %Speed_Sim% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrLCS %ATP_LCS_Port% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrTIMSSim %ATP_TIMS_Port% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcStatus %BTMHdlrOpcStatus% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcBtmTel %BTMHdlrOpcBtmTel% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrCommand %BTMHdlrCommand% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcAppStatus %BTMHdlrOpcAppStatus% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcClockStatus %BTMHdlrOpcClockStatus% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrTigrisOffset %BTMHdlrTigrisOffset% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrOPCSim %Sim_OPCSim% Train%1\test_atp.ini
Support\EditIniFile Simulation portNrOBRDSim %ATP_OBRD_Port% Train%1\test_atp.ini

rem delete environment variables no longer used in order to not run out of environment space and also to not use tem later by mistake
rem use quotes in order to exclude possibility of trailing white-space characters
set "ATP_AnalyzerIF_Port="
set "ATP_MMI_Channel1="
set "ATP_MMI_Channel2="
set "ATP_NJRU_Port="
set "ATP_RU_Port="
set "ATP_Console_Port="
set "ATP_TCC_Radio_Channel1="
set "ATP_TCC_Radio_Channel2="
set "ATP_TCC_Radio_Channel3="
set "SimCod_Odometry_Config="
set "SimCod_Odometry_Config_Response="
set "SimCod_Odometry_Meas="
set "Sim_VIOHSim="
set "Speed_Sim="
set "ATP_LCS_Port="
set "ATP_TIMS_Port="
set "BTMHdlrOpcStatus="
set "BTMHdlrOpcBtmTel="
set "BTMHdlrCommand="
set "BTMHdlrOpcAppStatus="
set "BTMHdlrOpcClockStatus="
set "BTMHdlrTigrisOffset="
set "Sim_OPCSim="
set "ATP_OBRD_Port="

rem edit the AOSPC.ini-files
set /a LocoSim_Server_Port=55090 + (%1 * 100)
set /a ATP_Console_Port=30065 + (%1 * 100)
set /a VIOH_SimPort_To_Connect=30090 + (%1 * 100)
set /a COD_SimPort_To_Connect=30091 + (%1 * 100)
set /a RemotePort_To_Connect=30098 + (%1 * 100)
set /a OPC_SimPort_To_Connect=30092 + (%1 * 100)
set /a DMI_InternalPort_To_Connect= 30096 + (%1 * 100)
set /a DMI_Port_To_Connect= 30097 + (%1 * 100)
set /a OBRD_Port_To_Connect=30051 + (%1 * 100)
set /a OBRD_LocoId=30051 + (%1 * 100)

rem Create variables for all node addresses and vehicle types
set /a startNodeAddress = 1 + ((%1-1) * %3)
set /a endNodeAddress = ((%1-1) * %3) + %2

set /a i = 1

for /L %%r in (%startNodeAddress%, 1, %endNodeAddress%) do (

    set /a RoadNumber_!i!=%%r
    
    if %%r LEQ !startNodeAddress! (
        set /a VehicleType_!i!=1
    ) else (
        set /a VehicleType_!i!=2
    )
    
    SET /a i+=1
)

Support\EditIniFile AOSPC LocoName "Loco %1" Train%1\AOSPC.ini
Support\EditIniFile AOSPC ATPConsolePortToConnect %ATP_Console_Port% Train%1\AOSPC.ini
Support\EditIniFile RemoteInterface Port %RemotePort_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile RemoteInterface Enabled "1" Train%1\AOSPC.ini
Support\EditIniFile LocoSim LocoSimServerPort %LocoSim_Server_Port% Train%1\AOSPC.ini
Support\EditIniFile LocoSim VIOHSimPortToConnect %VIOH_SimPort_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile LocoSim CODSimPortToConnect %COD_SimPort_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile LocoSim OPCSimPortToConnect %OPC_SimPort_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile LocoSim LocoName "Loco %1" Train%1\AOSPC.ini
Support\EditIniFile LocoSim ATPFile "%CD%\Train%1\test_atp.exe" Train%1\AOSPC.ini
Support\EditIniFile DMIInterface Enabled "1" Train%1\AOSPC.ini
Support\EditIniFile DMIInterface Port %DMI_Port_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile DMIInterface InternalPort %DMI_InternalPort_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile OBRDSim AOSOBRDPortToConnect %OBRD_Port_To_Connect% Train%1\AOSPC.ini
Support\EditIniFile OBRDSim LocoID %1 Train%1\AOSPC.ini

rem Remove old parameters for node-addresses and vehicle types avoiding conflict with the new generated.
findstr /V /i "RoadNumber_ VehicleType_" Train%1\AOSPC.ini > Train%1\AOSPC.tmp
move /Y Train%1\AOSPC.tmp Train%1\AOSPC.ini

rem Update AOSPC.ini for all node addresses and vehicle types
for /L %%i in (1, 1, %2) do (
    Support\EditIniFile LCSTrainComp RoadNumber_%%i !RoadNumber_%%i! Train%1\AOSPC.ini  
    Support\EditIniFile LCSTrainComp VehicleType_%%i !VehicleType_%%i! Train%1\AOSPC.ini
)

set "ATP_Console_Port="
set "LocoSim_Server_Port="
set "VIOH_SimPort_To_Connect="
set "COD_SimPort_To_Connect="
set "RemotePort_To_Connect="
set "OPC_SimPort_To_Connect="
set "DMI_Port_To_Connect="
set "DMI_InternalPort_To_Connect="
set "AOSOBRDPortToConnect="

rem Clear variables for all node addresses and vehicle types
for /L %%i in (1, 1, %2) do (
    set 'RoadNumber_%%i='
    set 'VehicleType_%%i='
)

set /a LCSSim_ATO_Port=30050 + (%1 * 100)
set /a LCSSim_LocoSim_Port=55090 + (%1 * 100)

Support\EditIniFile LCSSim LCSPort %LCSSim_ATO_Port% Train%1\AOSPC.ini
Support\EditIniFile LCSSim LocoSimPort %LCSSim_LocoSim_Port% Train%1\AOSPC.ini

set "LCSSim_ATO_Port="
set "LCSSim_LocoSim_Port="

set /a TIMS_Port=30093 + (%1 * 100)

Support\EditIniFile TimsSim TIMSPort %TIMS_Port% Train%1\AOSPC.ini
rem EditIniFile TimsSim ES2Id %1 Train%1\AOSPC\AOSPC.ini

set "TIMS_Port="

rem edit the NJRU.ini-files
rem set /a NJRU_Port=30031 + (%1 * 100)

rem Support\EditIniFile IP ListenPort %NJRU_Port% Train%1\N-JRU\N-JRU.ini
rem Support\EditIniFile Log Path "%CD%\Log%1" Train%1\N-JRU\N-JRU.ini

rem set "NJRU_Port="

rem edit the DMI.ini-files
set /a MMICL_Port=30030 + (%1 * 100)
rem set /a NJRU_Port=30031 + (%1 * 100)
set /a Remote_Port=30097 + (%1 * 100)
set /a Remote_Internal_Port=30096 + (%1 * 100)

Support\EditIniFile MMICL ComPort %MMICL_Port% Train%1\DMI.ini
rem Support\EditIniFile N-JRU Port %NJRU_Port% Train%1\DMI\DMI.ini
Support\EditIniFile RemoteInterface Enabled "1" Train%1\DMI.ini
Support\EditIniFile RemoteInterface Port %Remote_Port% Train%1\DMI.ini
Support\EditIniFile RemoteInterface InternalPort %Remote_Internal_Port% Train%1\DMI.ini

set "MMICL_Port="
rem set "NJRU_Port="
set "Remote_Port="
set "Remote_Internal_Port="


rem create shortcuts
rem Support\CreateShortcut "%CD%\Train%1\DMI\DMI.Exe" "%CD%\Train%1\DMI" "%UserProfile%\Desktop\DMI_%1.lnk" "DMI %1"
rem Support\CreateShortcut "%CD%\Train%1\AOSPC\AOSPC.Exe" "%CD%\Train%1\AOSPC" "%UserProfile%\Desktop\AOSPC_%1.lnk" "AOSPC %1"
rem Support\CreateShortcut "%CD%\Train%1\N-JRU\N-JRU.Exe" "%CD%\Train%1\N-JRU" "%UserProfile%\Desktop\N-JRU_%1.lnk" "N-JRU %1"

:end
