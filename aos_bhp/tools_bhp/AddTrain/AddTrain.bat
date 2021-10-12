echo off
SetLocal EnableDelayedExpansion
rem History
rem Date       Sign 	Comment                                    
rem 2013-04-30 Bo H	    Corrected NJRU_Port setting for ATO & ATP1/2
rem 2013-05-07 Bo H	    Individual log-paths for N-JRU
rem 2013-09-05 Antbäck  Added IP-connection ATP1-ATO to settings     
rem 2013-11-26 Bo H	    Add LocoName for LocoSim
rem 2013-11-26 Bo H	    Replace LCSSim with AOSPC, add LocoName
rem 2014-04-05 Hidaji   Replace LOCOSim with AOSPC, add ES2 id, Fix bug in LCS_Port
rem 			   Added TIMSsim, removed locosim shortcut
rem 2014-07-25 Hidaji	SiteDataExtDLL
rem 2014-08-21 Bo H     Support trains with 2 < id < 99 
rem 2015-02-19 Bo H	    Corrected ATO_AnalyzerIF_Port
rem 2017-09-25 akushwah	Adapted for BHP Project
rem 2018-05-27 Marlundg N-JRU changed to RU
rem 2018-09-18 Marlundg Added Node- and Vehicle parameters for TrainComposition
rem 2018-12-12 Marlundg Added support for OBRDSim.
rem 2019-02-07 rquensel Added extra config file

if [%1] == [] goto usage
if [%2] == [] goto usage
if [%3] == [] goto usage
if %1 LSS 2 goto usage
if %1 GTR 300 goto usage
if %2 LSS 1 goto usage
if %2 GTR 350 goto usage
goto ok

:usage
echo Usage:
echo AddTrain [2..200] [Number of vehicles (1-350)] [Train Node-address offset]
echo Example:
echo AddTrain 2 20 30 , will generate a train2 directory setup with 1 loco + 19 cars with node addresses 31-50.
echo AddTrain 3 20 30 , will generate a train3 directory setup with 1 loco + 19 cars with node addresses 61-80.
goto end

:ok
echo OK to create Train %1 in %CD%\train%1? (Ctrl-C to abort, Enter to continue)
pause

rem create the directories and copy files
md train%1
xcopy /Y TestATP train%1\TestATP\
xcopy /Y AOSPC train%1\AOSPC\
xcopy /Y DMI train%1\DMI\

md train%1\RU
copy RU\* train%1\RU

rem create log-directory for RU
md Log%1

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


Support\EditIniFile COMMON_DATA.BIN AnalyzerIFPort %ATP_AnalyzerIF_Port% train%1\TestATP\test_atp_cfg.ini
Support\EditIniFile Simulation portNrDMIChannel1 %ATP_MMI_Channel1% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrDMIChannel2 %ATP_MMI_Channel2% train%1\TestATP\test_atp.ini
Support\EditIniFile COMMON_DATA.BIN NjruPort %ATP_NJRU_Port% train%1\TestATP\test_atp_cfg.ini
Support\EditIniFile COMMON_DATA.BIN RuPort %ATP_RU_Port% train%1\TestATP\test_atp_cfg.ini
Support\EditIniFile COMMON_DATA.BIN ConsolePort %ATP_Console_Port% train%1\TestATP\test_atp_cfg.ini
Support\EditIniFile Simulation portNrRadiochannel1 %ATP_TCC_Radio_Channel1% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrRadiochannel2 %ATP_TCC_Radio_Channel2% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrRadiochannel3 %ATP_TCC_Radio_Channel3% train%1\TestATP\test_atp.ini
Support\EditIniFile INSTANCE_DATA.BIN RadioId %1 train%1\TestATP\test_atp_cfg.ini
Support\EditIniFile Simulation portNrSimCodOdometryConfig %SimCod_Odometry_Config% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrSimCodOdometryConfigResponse %SimCod_Odometry_Config_Response% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrSimCodOdometryMeas %SimCod_Odometry_Meas% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrSimVIOHSim %Sim_VIOHSim% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrSpeedSim %Speed_Sim% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrLCS %ATP_LCS_Port% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrTIMSSim %ATP_TIMS_Port% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcStatus %BTMHdlrOpcStatus% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcBtmTel %BTMHdlrOpcBtmTel% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrCommand %BTMHdlrCommand% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcAppStatus %BTMHdlrOpcAppStatus% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrOpcClockStatus %BTMHdlrOpcClockStatus% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrBTMHdlrTigrisOffset %BTMHdlrTigrisOffset% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrOPCSim %Sim_OPCSim% train%1\TestATP\test_atp.ini
Support\EditIniFile Simulation portNrOBRDSim %ATP_OBRD_Port% train%1\TestATP\test_atp.ini


rem delete environment variables no longer used in order to not run out of environment space and also to not use them later by mistake
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
set /a OPC_SimPort_To_Connect=30092 + (%1 * 100)
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

Support\EditIniFile AOSPC LocoName "Loco %1" train%1\AOSPC\AOSPC.ini
Support\EditIniFile AOSPC ATPConsolePortToConnect %ATP_Console_Port% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim LocoSimServerPort %LocoSim_Server_Port% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim VIOHSimPortToConnect %VIOH_SimPort_To_Connect% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim CODSimPortToConnect %COD_SimPort_To_Connect% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim OPCSimPortToConnect %OPC_SimPort_To_Connect% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim LocoName "Loco %1" train%1\AOSPC\AOSPC.ini
Support\EditIniFile LocoSim ATPFile "%CD%\train%1\TestATP\test_atp.exe" train%1\AOSPC\AOSPC.ini
Support\EditIniFile OBRDSim AOSOBRDPortToConnect %OBRD_Port_To_Connect% train%1\AOSPC\AOSPC.ini
Support\EditIniFile OBRDSim LocoID %1 train%1\AOSPC\AOSPC.ini

rem Remove old parameters for node-addresses and vehicle types avoiding conflict with the new generated.
findstr /V /i "RoadNumber_ VehicleType_" train%1\AOSPC\AOSPC.ini > train%1\AOSPC\AOSPC.tmp
move /Y train%1\AOSPC\AOSPC.tmp train%1\AOSPC\AOSPC.ini

rem Update AOSPC.ini for all node addresses and vehicle types
for /L %%i in (1, 1, %2) do (
    Support\EditIniFile LCSTrainComp RoadNumber_%%i !RoadNumber_%%i! train%1\AOSPC\AOSPC.ini  
    Support\EditIniFile LCSTrainComp VehicleType_%%i !VehicleType_%%i! train%1\AOSPC\AOSPC.ini
)

set "ATP_Console_Port="
set "LocoSim_Server_Port="
set "VIOH_SimPort_To_Connect="
set "COD_SimPort_To_Connect="
set "OPC_SimPort_To_Connect="
set "AOSOBRDPortToConnect="

rem Clear variables for all node addresses and vehicle types
for /L %%i in (1, 1, %2) do (
    set 'RoadNumber_%%i='
    set 'VehicleType_%%i='
)

set /a LCSSim_ATO_Port=30050 + (%1 * 100)
set /a LCSSim_LocoSim_Port=55090 + (%1 * 100)

Support\EditIniFile LCSSim LCSPort %LCSSim_ATO_Port% train%1\AOSPC\AOSPC.ini
Support\EditIniFile LCSSim LocoSimPort %LCSSim_LocoSim_Port% train%1\AOSPC\AOSPC.ini

set "LCSSim_ATO_Port="
set "LCSSim_LocoSim_Port="

set /a TIMS_Port=30093 + (%1 * 100)

Support\EditIniFile TimsSim TIMSPort %TIMS_Port% train%1\AOSPC\AOSPC.ini
rem EditIniFile TimsSim ES2Id %1 train%1\AOSPC\AOSPC.ini

set "TIMS_Port="

rem edit the NJRU.ini-files
set /a NJRU_Port=30031 + (%1 * 100)
set /a RU_Port=30080 + (%1 * 100)

Support\EditIniFile IP ListenPort %NJRU_Port% train%1\RU\RU.ini
Support\EditIniFile IP RU-ListenPort %RU_Port% train%1\RU\RU.ini
Support\EditIniFile Log Path "%CD%\Log%1" train%1\RU\RU.ini

set "NJRU_Port="
set "RU_Port="

rem edit the DMI.ini-files
set /a MMICL_Port=30030 + (%1 * 100)
set /a NJRU_Port=30031 + (%1 * 100)

Support\EditIniFile MMICL ComPort %MMICL_Port% train%1\DMI\DMI.ini
Support\EditIniFile N-JRU Port %NJRU_Port% train%1\DMI\DMI.ini

set "MMICL_Port="
set "NJRU_Port="


rem create shortcuts
Support\CreateShortcut "%CD%\train%1\DMI\DMI.Exe" "%CD%\train%1\DMI" "%UserProfile%\Desktop\DMI_%1.lnk" "DMI %1"
Support\CreateShortcut "%CD%\train%1\AOSPC\AOSPC.Exe" "%CD%\train%1\AOSPC" "%UserProfile%\Desktop\AOSPC_%1.lnk" "AOSPC %1"
Support\CreateShortcut "%CD%\train%1\RU\RU.Exe" "%CD%\train%1\RU" "%UserProfile%\Desktop\RU_%1.lnk" "RU %1"

:end
