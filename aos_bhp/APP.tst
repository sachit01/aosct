-- VectorCAST 6.4v (08/01/17)
-- Test Case Script
-- 
-- Environment    : APP
-- Unit(s) Under Test: abstract_application_base abstract_atp_application atp_application
-- 
-- Script Features
TEST.SCRIPT_FEATURE:C_DIRECT_ARRAY_INDEXING
TEST.SCRIPT_FEATURE:CPP_CLASS_OBJECT_REVISION
TEST.SCRIPT_FEATURE:MULTIPLE_UUT_SUPPORT
TEST.SCRIPT_FEATURE:MIXED_CASE_NAMES
TEST.SCRIPT_FEATURE:STANDARD_SPACING_R2
TEST.SCRIPT_FEATURE:OVERLOADED_CONST_SUPPORT
TEST.SCRIPT_FEATURE:UNDERSCORE_NULLPTR
TEST.SCRIPT_FEATURE:FULL_PARAMETER_TYPES
TEST.SCRIPT_FEATURE:STRUCT_DTOR_ADDS_POINTER
TEST.SCRIPT_FEATURE:STATIC_HEADER_FUNCS_IN_UUTS
--

-- Subprogram: <<INIT>>

-- Test Case: ATPApplication_instance.001
TEST.SUBPROGRAM:<<INIT>>
TEST.NEW
TEST.NAME:ATPApplication_instance.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Unit: atp_application

-- Subprogram: (cl)ATP::Kernel::ATPApplication::addAllComponents

-- Test Case: ATPApplication_addAllComponents.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::addAllComponents
TEST.NEW
TEST.NAME:ATPApplication_addAllComponents.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::EventHandler::instance.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::BasicIP::instance.return.ATP::BasicIP.<<constructor>>.BasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::LogHandler::instance.return.ATP::LogHandler.<<constructor>>.LogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::BrakeCalculations::instance.return.ATP::Supv::BrakeCalculations.<<constructor>>.BrakeCalculations().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::CrossCompare::instance.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_addAllComponents.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::addAllComponents
TEST.NEW
TEST.NAME:ATPApplication_addAllComponents.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::EventHandler::instance.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::BasicIP::instance.return.ATP::BasicIP.<<constructor>>.BasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::LogHandler::instance.return.ATP::LogHandler.<<constructor>>.LogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::BrakeCalculations::instance.return.ATP::Supv::BrakeCalculations.<<constructor>>.BrakeCalculations().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::CrossCompare::instance.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.AOS_HALT_CALLED:true
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::clearExecTimes

-- Test Case: ATPApplication_clearExecTimes.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::clearExecTimes
TEST.NEW
TEST.NAME:ATPApplication_clearExecTimes.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getApplicationName

-- Test Case: ATPApplication_getApplicationName.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getApplicationName
TEST.NEW
TEST.NAME:ATPApplication_getApplicationName.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getApplicationName.return:"ATP"
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getApplicationVersionString

-- Test Case: ATPApplication_getApplicationVersionString.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getApplicationVersionString
TEST.NEW
TEST.NAME:ATPApplication_getApplicationVersionString.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getApplicationVersionString.return:"1.64.0 (with LSSD)"
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getBlockNr

-- Test Case: ATPApplication_getBlockNr.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getBlockNr
TEST.NEW
TEST.NAME:ATPApplication_getBlockNr.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getBlockNr.return:ATPBlock
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getDispatcherVersionString

-- Test Case: ATPApplication_getDispatcherVersionString.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString
TEST.NEW
TEST.NAME:ATPApplication_getDispatcherVersionString.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString.return:"1.47.0"
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: ATPApplication_getDispatcherVersionString.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString
TEST.NEW
TEST.NAME:ATPApplication_getDispatcherVersionString.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString.return:"1.41.6"
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getLastExecTime

-- Test Case: ATPApplication_getLastExecTime.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getLastExecTime
TEST.NEW
TEST.NAME:ATPApplication_getLastExecTime.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getLastExecTime.return:2
TEST.END

-- Test Case: ATPApplication_getLastExecTime.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getLastExecTime
TEST.NEW
TEST.NAME:ATPApplication_getLastExecTime.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getLastExecTime.return:0
TEST.END

-- Test Case: ATPApplication_getLastExecTime.003
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getLastExecTime
TEST.NEW
TEST.NAME:ATPApplication_getLastExecTime.003
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getLastExecTime.return:2147483647
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getMaxExecTime

-- Test Case: ATPApplication_getMaxExecTime.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMaxExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMaxExecTime.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMaxExecTime.return:2
TEST.END

-- Test Case: ATPApplication_getMaxExecTime.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMaxExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMaxExecTime.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMaxExecTime.return:0
TEST.END

-- Test Case: ATPApplication_getMaxExecTime.003
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMaxExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMaxExecTime.003
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMaxExecTime.return:0
TEST.END

-- Test Case: ATPApplication_getMaxExecTime.004
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMaxExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMaxExecTime.004
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMaxExecTime.return:2147483647
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getMinExecTime

-- Test Case: ATPApplication_getMinExecTime.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMinExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMinExecTime.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMinExecTime.return:2
TEST.END

-- Test Case: ATPApplication_getMinExecTime.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMinExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMinExecTime.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMinExecTime.return:255
TEST.END

-- Test Case: ATPApplication_getMinExecTime.003
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMinExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMinExecTime.003
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMinExecTime.return:0
TEST.END

-- Test Case: ATPApplication_getMinExecTime.004
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getMinExecTime
TEST.NEW
TEST.NAME:ATPApplication_getMinExecTime.004
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getMinExecTime.return:2147483647
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::getVIOHClientHandle

-- Test Case: ATPApplication_getVIOHClientHandle.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle
TEST.NEW
TEST.NAME:ATPApplication_getVIOHClientHandle.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.VIOHnames::VIOHClient::VIOHClient(const uint32_t,const VIOHnames::VIOH_taskType,const uint32_t).clientId:0
TEST.VALUE:uut_prototype_stubs.VIOHnames::VIOHClient::VIOHClient(const uint32_t,const VIOHnames::VIOH_taskType,const uint32_t).taskType:enTTVITALSWIT
TEST.VALUE:uut_prototype_stubs.VIOHnames::VIOHClient::VIOHClient(const uint32_t,const VIOHnames::VIOH_taskType,const uint32_t).taskCycle:100
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle.return.VIOHnames::VIOHClient.<<constructor>>.VIOHClient(const VIOHnames::VIOHClient&).<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle.return.VIOHnames::VIOHClient.myclientId:0
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle.return.VIOHnames::VIOHClient.mytaskType:enTTVITALSWIT
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle.return.VIOHnames::VIOHClient.mytaskCycle:100
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::init

-- Test Case: ATPApplication_init.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::EventHandler::instance.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::BasicIP::instance.return.ATP::BasicIP.<<constructor>>.BasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::LogHandler::instance.return.ATP::LogHandler.<<constructor>>.LogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::CrossCompare::instance.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.003
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.003
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.VECTORCAST_INT1:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::AbstractCrossCompare.<<constructor>>.AbstractCrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::vcast_concrete_AbstractMessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.VALUE:atp_application.<<GLOBAL>>.ATC::coreBasicIPInstancePtr.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.004
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.004
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:true
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.005
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.005
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.006
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.006
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.initDone:true
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConfigBase::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.level:BriefLog
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.text:"Register measurement failed for analyzer","ATP VERSION=1.41.6: Init done!"
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.007
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.007
TEST.COMPOUND_ONLY
TEST.VALUE:abstract_application_base.<<GLOBAL>>.ATC::coreAbstractApplicationBasePtr:<<null>>
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.008
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.008
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:false
TEST.VALUE:uut_prototype_stubs.ATC::EventHandler::instance.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::BasicIP::instance.return.ATP::BasicIP.<<constructor>>.BasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::LogHandler::instance.return.ATP::LogHandler.<<constructor>>.LogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::CrossCompare::instance.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.009
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.009
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.010
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.010
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:true
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.011
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.011
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.level:BriefLog
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.text:"Register measurement failed for analyzer","ATP VERSION=1.41.6: Init done!"
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.012
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.012
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.level:BriefLog
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.text:"Register measurement failed for analyzer","ATP VERSION=1.41.6: Init done!"
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.013
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.013
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.014
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.014
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.VALUE:atp_application.<<GLOBAL>>.ATC::coreBasicIPInstancePtr.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.015
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.015
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::EventHandler::instance.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::BasicIP::instance.return.ATP::BasicIP.<<constructor>>.BasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Console::instance.return.ATP::Console.<<constructor>>.Console().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::LogHandler::instance.return.ATP::LogHandler.<<constructor>>.LogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::AnalyzerIF::instance.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Support::CrossCompare::instance.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_init.016
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::init
TEST.NEW
TEST.NAME:ATPApplication_init.016
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole(const char_t*const).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Config::instance.return.ATP::Config.<<constructor>>.Config().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Decode::instance.return.ATP::Pos::Decode.<<constructor>>.Decode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Odometry::instance.return.ATP::Pos::Odometry.<<constructor>>.Odometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::Position::instance.return.ATP::Pos::Position.<<constructor>>.Position().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::MessageHandler::instance.return.ATP::Kernel::MessageHandler.<<constructor>>.MessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::instance.return.ATP::Kernel::ModeControl.<<constructor>>.ModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::ModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::instance.return.ATP::Supv::Brake.<<constructor>>.Brake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Brake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::init.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Supv::Supervise::instance.return.ATP::Supv::Supervise.<<constructor>>.Supervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::LocoIO::instance.return.ATP::IO::LocoIO.<<constructor>>.LocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::BTMHandler::instance.return.ATP::IO::BTMHandler.<<constructor>>.BTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::instance.return.ATP::DS::Targets.<<constructor>>.Targets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::Targets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::Tracks::instance.return.ATP::DS::Tracks.<<constructor>>.Tracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::TSetup::instance.return.ATP::DS::TSetup.<<constructor>>.TSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioHandler::instance.return.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::DMIHandler::instance.return.ATP::DMICom::DMIHandler.<<constructor>>.DMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::instance.return.ATP::TG::VehicleCom.<<constructor>>.VehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::VehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::instance.return.ATP::TG::TIMS.<<constructor>>.TIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::instance.return.ATP::TG::TIC.<<constructor>>.TIC().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::vcast_concrete_AbstractMessageHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::vcast_concrete_AbstractRadioHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::init.return:true
TEST.VALUE:uut_prototype_stubs.ATP::TG::TIC::getLocoOrientationAvailable.return:true
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::reportEvent.event.ATC::Event.<<constructor>>.Event().<<call>>:0
TEST.VALUE:atp_application.<<GLOBAL>>.ATC::coreBasicIPInstancePtr.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::init.return:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::preInit

-- Test Case: ATPApplication_preInit.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::preInit
TEST.NEW
TEST.NAME:ATPApplication_preInit.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.EXPECTED_GLOBALS_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication.compList
{{ <<ATP::Kernel::ATPApplication instance>>->compList == ( <<ATP::Kernel::ATPApplication instance>>->compList ) }}
TEST.END_EXPECTED_GLOBALS_USER_CODE:
TEST.END

-- Test Case: ATPApplication_preInit.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::preInit
TEST.NEW
TEST.NAME:ATPApplication_preInit.002
TEST.COMPOUND_ONLY
TEST.VALUE:atp_application.<<GLOBAL>>.(cl).ATP::Kernel::ATPApplication.ATP::Kernel::ATPApplication.compList:<<malloc 1>>
TEST.VALUE:atp_application.<<GLOBAL>>.(cl).ATP::Kernel::ATPApplication.ATP::Kernel::ATPApplication.compList[0]:<<function 1>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.AOS_HALT_CALLED:true
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.EXPECTED_GLOBALS_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication.compList
{{ <<ATP::Kernel::ATPApplication instance>>->compList == ( <<ATP::Kernel::ATPApplication instance>>->compList ) }}
TEST.END_EXPECTED_GLOBALS_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::run

-- Test Case: ATPApplication_run.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::run
TEST.NEW
TEST.NAME:ATPApplication_run.001
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::TargetCalculation::instance.return.ATP::Supv::TargetCalculation.<<constructor>>.TargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::AbstractCrossCompare.<<constructor>>.AbstractCrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.vfwGetHighResolutionClock.return:1500000,3000000
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConfigBase::basePtr.return.ATC::AbstractConfigBase.<<constructor>>.AbstractConfigBase().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::corePtr.return.ATP::Pos::AbstractOdometry.<<constructor>>.AbstractOdometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::corePtr.return.ATP::Pos::AbstractDecode.<<constructor>>.AbstractDecode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::corePtr.return.ATP::Pos::vcast_concrete_AbstractPosition.<<constructor>>.vcast_concrete_AbstractPosition().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::corePtr.return.ATP::Kernel::AbstractModeControl.<<constructor>>.AbstractModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::corePtr.return.ATP::DS::AbstractTracks.<<constructor>>.AbstractTracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::corePtr.return.ATP::DS::AbstractTargets.<<constructor>>.AbstractTargets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::corePtr.return.ATP::DS::vcast_concrete_AbstractTSetup.<<constructor>>.vcast_concrete_AbstractTSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::corePtr.return.ATP::IO::AbstractBTMHandler.<<constructor>>.AbstractBTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::corePtr.return.ATP::IO::vcast_concrete_AbstractLocoIO.<<constructor>>.vcast_concrete_AbstractLocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::corePtr.return.ATP::Supv::vcast_concrete_AbstractBrake.<<constructor>>.vcast_concrete_AbstractBrake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::corePtr.return.ATP::Supv::AbstractTargetCalculation.<<constructor>>.AbstractTargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::corePtr.return.ATP::Supv::AbstractSupervise.<<constructor>>.AbstractSupervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::AbstractRadioHandler::corePtr.return.ATP::RadioCom::vcast_concrete_AbstractRadioHandler.<<constructor>>.vcast_concrete_AbstractRadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::corePtr.return.ATP::DMICom::vcast_concrete_AbstractDMIHandler.<<constructor>>.vcast_concrete_AbstractDMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::corePtr.return.ATP::TG::vcast_concrete_AbstractVehicleCom.<<constructor>>.vcast_concrete_AbstractVehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::corePtr.return.ATP::TG::AbstractTIMS.<<constructor>>.AbstractTIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::corePtr.return.ATP::TG::AbstractTIC.<<constructor>>.AbstractTIC().<<call>>:0
TEST.VALUE:abstract_atp_application.<<GLOBAL>>.ATP::Supv::coreTargetCalculationInstancePtr.ATP::Supv::AbstractTargetCalculation.<<constructor>>.AbstractTargetCalculation().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::AbstractATPApplication instance>>
<<ATP::Kernel::AbstractATPApplication instance>> = ( ATP::Kernel::AbstractATPApplication::corePtr() );
TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_run.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::run
TEST.NEW
TEST.NAME:ATPApplication_run.002
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.vfwGetHighResolutionClock.return:1500000,3000000
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<testcase>>

TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.EXPECTED_USER_CODE:<<testcase>>

TEST.END_EXPECTED_USER_CODE:
TEST.END

-- Test Case: ATPApplication_run.003
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::run
TEST.NEW
TEST.NAME:ATPApplication_run.003
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConsole::corePtr.return.ATC::vcast_concrete_AbstractConsole.<<constructor>>.vcast_concrete_AbstractConsole().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.vfwGetHighResolutionClock.return:-9223372036854775808,9223372036854775807
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConfigBase::basePtr.return.ATC::AbstractConfigBase.<<constructor>>.AbstractConfigBase().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::corePtr.return.ATP::Pos::AbstractOdometry.<<constructor>>.AbstractOdometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::corePtr.return.ATP::Pos::AbstractDecode.<<constructor>>.AbstractDecode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::corePtr.return.ATP::Pos::vcast_concrete_AbstractPosition.<<constructor>>.vcast_concrete_AbstractPosition().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::corePtr.return.ATP::Kernel::AbstractModeControl.<<constructor>>.AbstractModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::corePtr.return.ATP::DS::AbstractTracks.<<constructor>>.AbstractTracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::corePtr.return.ATP::DS::AbstractTargets.<<constructor>>.AbstractTargets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::corePtr.return.ATP::DS::vcast_concrete_AbstractTSetup.<<constructor>>.vcast_concrete_AbstractTSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::corePtr.return.ATP::IO::AbstractBTMHandler.<<constructor>>.AbstractBTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::corePtr.return.ATP::IO::vcast_concrete_AbstractLocoIO.<<constructor>>.vcast_concrete_AbstractLocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::corePtr.return.ATP::Supv::vcast_concrete_AbstractBrake.<<constructor>>.vcast_concrete_AbstractBrake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::corePtr.return.ATP::Supv::AbstractTargetCalculation.<<constructor>>.AbstractTargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::corePtr.return.ATP::Supv::AbstractSupervise.<<constructor>>.AbstractSupervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::AbstractRadioHandler::corePtr.return.ATP::RadioCom::vcast_concrete_AbstractRadioHandler.<<constructor>>.vcast_concrete_AbstractRadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::corePtr.return.ATP::DMICom::vcast_concrete_AbstractDMIHandler.<<constructor>>.vcast_concrete_AbstractDMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::corePtr.return.ATP::TG::vcast_concrete_AbstractVehicleCom.<<constructor>>.vcast_concrete_AbstractVehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::corePtr.return.ATP::TG::AbstractTIMS.<<constructor>>.AbstractTIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::corePtr.return.ATP::TG::AbstractTIC.<<constructor>>.AbstractTIC().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: ATPApplication_run.004
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::run
TEST.NEW
TEST.NAME:ATPApplication_run.004
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:false
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE:uut_prototype_stubs.ATC::AbstractEventHandler::corePtr.return.ATC::EventHandler.<<constructor>>.EventHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractLogHandler::corePtr.return.ATC::vcast_concrete_AbstractLogHandler.<<constructor>>.vcast_concrete_AbstractLogHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::corePtr.return.ATP::AnalyzerIF.<<constructor>>.AnalyzerIF().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Support::AbstractCrossCompare::corePtr.return.ATP::Support::CrossCompare.<<constructor>>.CrossCompare().<<call>>:0
TEST.VALUE:uut_prototype_stubs.vfwGetHighResolutionClock.return:0,2147483647000000
TEST.VALUE:uut_prototype_stubs.ATC::AbstractConfigBase::basePtr.return.ATC::AbstractConfigBase.<<constructor>>.AbstractConfigBase().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractOdometry::corePtr.return.ATP::Pos::AbstractOdometry.<<constructor>>.AbstractOdometry().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractDecode::corePtr.return.ATP::Pos::AbstractDecode.<<constructor>>.AbstractDecode().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Pos::AbstractPosition::corePtr.return.ATP::Pos::vcast_concrete_AbstractPosition.<<constructor>>.vcast_concrete_AbstractPosition().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractModeControl::corePtr.return.ATP::Kernel::AbstractModeControl.<<constructor>>.AbstractModeControl().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTracks::corePtr.return.ATP::DS::AbstractTracks.<<constructor>>.AbstractTracks().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTargets::corePtr.return.ATP::DS::AbstractTargets.<<constructor>>.AbstractTargets().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DS::AbstractTSetup::corePtr.return.ATP::DS::vcast_concrete_AbstractTSetup.<<constructor>>.vcast_concrete_AbstractTSetup().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractBTMHandler::corePtr.return.ATP::IO::AbstractBTMHandler.<<constructor>>.AbstractBTMHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::IO::AbstractLocoIO::corePtr.return.ATP::IO::vcast_concrete_AbstractLocoIO.<<constructor>>.vcast_concrete_AbstractLocoIO().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractBrake::corePtr.return.ATP::Supv::vcast_concrete_AbstractBrake.<<constructor>>.vcast_concrete_AbstractBrake().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractTargetCalculation::corePtr.return.ATP::Supv::AbstractTargetCalculation.<<constructor>>.AbstractTargetCalculation().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Supv::AbstractSupervise::corePtr.return.ATP::Supv::AbstractSupervise.<<constructor>>.AbstractSupervise().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::AbstractRadioHandler::corePtr.return.ATP::RadioCom::vcast_concrete_AbstractRadioHandler.<<constructor>>.vcast_concrete_AbstractRadioHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::DMICom::AbstractDMIHandler::corePtr.return.ATP::DMICom::vcast_concrete_AbstractDMIHandler.<<constructor>>.vcast_concrete_AbstractDMIHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractVehicleCom::corePtr.return.ATP::TG::vcast_concrete_AbstractVehicleCom.<<constructor>>.vcast_concrete_AbstractVehicleCom().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIMS::corePtr.return.ATP::TG::AbstractTIMS.<<constructor>>.AbstractTIMS().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::TG::AbstractTIC::corePtr.return.ATP::TG::AbstractTIC.<<constructor>>.AbstractTIC().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.RADIOHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNIN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ODOMETRY_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DECODE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.POSITION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MODECONTROL_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETCALCULATION_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.SUPERVISE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BRAKE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.MESSAGEHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOCOIO_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BTMHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.DMIHANDLER_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TRACKS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TARGETS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TSETUP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.EVENTHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.LOGHANDLER_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.BASICIP_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONSOLE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.ANALYZERIF_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CONFIGBASE_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.CROSSCOMPARE_RUNOUT:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIMS_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.TIC_RUN:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.CONSTRUCTOR_USER_CODE:<<ATP::Kernel::ATPApplication instance>>.ATP::Kernel::ATPApplication
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::setDispatcherVersionString

-- Test Case: ATPApplication_setDispatcherVersionString.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::setDispatcherVersionString
TEST.NEW
TEST.NAME:ATPApplication_setDispatcherVersionString.001
TEST.COMPOUND_ONLY
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setDispatcherVersionString.versionString:<<malloc 7>>
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setDispatcherVersionString.versionString:"1.41.6"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::setPerformTachoPSSupervision

-- Test Case: ATPApplication_setPerformTachoPSSupervision.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::setPerformTachoPSSupervision
TEST.NEW
TEST.NAME:ATPApplication_setPerformTachoPSSupervision.001
TEST.COMPOUND_ONLY
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setPerformTachoPSSupervision.performSupervision:true
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::setPerformTachoPSSupervision.performSupervision:true
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = (  &ATP::Kernel::ATPApplication::instance()  );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::setVIOHClientHandle

-- Test Case: ATPApplication_setVIOHClientHandle.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle
TEST.NEW
TEST.NAME:ATPApplication_setVIOHClientHandle.001
TEST.COMPOUND_ONLY
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle.handle.VIOHnames::VIOHClient.<<constructor>>.VIOHClient(const VIOHnames::VIOHClient&).<<call>>:0
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle.handle.VIOHnames::VIOHClient.myclientId:0
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle.handle.VIOHnames::VIOHClient.mytaskType:enTTVITALSWIT
TEST.VALUE:atp_application.(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle.handle.VIOHnames::VIOHClient.mytaskCycle:100
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = &( ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::Kernel::ATPApplication::validateDispatcherVersion

-- Test Case: ATPApplication_validateDispatcherVersion.001
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion
TEST.NEW
TEST.NAME:ATPApplication_validateDispatcherVersion.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion.return:true
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: ATPApplication_validateDispatcherVersion.002
TEST.UNIT:atp_application
TEST.SUBPROGRAM:(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion
TEST.NEW
TEST.NAME:ATPApplication_validateDispatcherVersion.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.level:BriefLog
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.text:"Dispatcher version mismatch: 1.41.6, expecting: 1.47.0"
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractLogHandler::writeToLog.compName:"AP"
TEST.EXPECTED:atp_application.(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion.return:false
TEST.VALUE_USER_CODE:<<ATP::Kernel::ATPApplication instance>>
<<ATP::Kernel::ATPApplication instance>> = ( &ATP::Kernel::ATPApplication::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0001_Initialization_of_all_components_pass_execution_pass
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::addAllComponents", "1", "ATPApplication_addAllComponents.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::getBlockNr", "1", "ATPApplication_getBlockNr.001"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::preInit", "1", "ATPApplication_preInit.001"
TEST.SLOT: "5", "atp_application", "(cl)ATP::Kernel::ATPApplication::setPerformTachoPSSupervision", "1", "ATPApplication_setPerformTachoPSSupervision.001"
TEST.SLOT: "6", "<<COMPOUND>>", "<<COMPOUND>>", "1", "AP0002_ATPApplication_init_pass"
TEST.SLOT: "7", "atp_application", "(cl)ATP::Kernel::ATPApplication::run", "1", "ATPApplication_run.001"
TEST.SLOT: "8", "atp_application", "(cl)ATP::Kernel::ATPApplication::getLastExecTime", "1", "ATPApplication_getLastExecTime.001"
TEST.SLOT: "9", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMinExecTime", "1", "ATPApplication_getMinExecTime.001"
TEST.SLOT: "10", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMaxExecTime", "1", "ATPApplication_getMaxExecTime.001"
TEST.SLOT: "11", "atp_application", "(cl)ATP::Kernel::ATPApplication::clearExecTimes", "1", "ATPApplication_clearExecTimes.001"
TEST.SLOT: "12", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMinExecTime", "1", "ATPApplication_getMinExecTime.002"
TEST.SLOT: "13", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMaxExecTime", "1", "ATPApplication_getMaxExecTime.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0002_ATPApplication_init_pass
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.002"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.003"
TEST.SLOT: "5", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0003_Initialization_of_all_components_not_pass_execution_fail
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::addAllComponents", "1", "ATPApplication_addAllComponents.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::preInit", "1", "ATPApplication_preInit.001"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "5", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.002"
TEST.SLOT: "6", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.005"
TEST.SLOT: "7", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.013"
TEST.SLOT: "8", "atp_application", "(cl)ATP::Kernel::ATPApplication::run", "1", "ATPApplication_run.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0004_All_but_one_component_not_added_VFW_halt
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::addAllComponents", "1", "ATPApplication_addAllComponents.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0005_ATPApplication_verify_VIOH
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::setVIOHClientHandle", "1", "ATPApplication_setVIOHClientHandle.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::getVIOHClientHandle", "1", "ATPApplication_getVIOHClientHandle.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0006_ATPApplication_not_even_one_component_added
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::preInit", "1", "ATPApplication_preInit.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0007_Last_exec_time_less_than_min_exec_time
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::addAllComponents", "1", "ATPApplication_addAllComponents.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::preInit", "1", "ATPApplication_preInit.001"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.007"
TEST.SLOT: "5", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "6", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.008"
TEST.SLOT: "7", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.002"
TEST.SLOT: "8", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.009"
TEST.SLOT: "9", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.003"
TEST.SLOT: "10", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.004"
TEST.SLOT: "11", "atp_application", "(cl)ATP::Kernel::ATPApplication::run", "1", "ATPApplication_run.003"
TEST.SLOT: "12", "atp_application", "(cl)ATP::Kernel::ATPApplication::getBlockNr", "1", "ATPApplication_getBlockNr.001"
TEST.SLOT: "13", "atp_application", "(cl)ATP::Kernel::ATPApplication::getLastExecTime", "1", "ATPApplication_getLastExecTime.002"
TEST.SLOT: "14", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMaxExecTime", "1", "ATPApplication_getMaxExecTime.003"
TEST.SLOT: "15", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMinExecTime", "1", "ATPApplication_getMinExecTime.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0008_Last_exec_time_notless_than_min_exec_time
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::addAllComponents", "1", "ATPApplication_addAllComponents.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::preInit", "1", "ATPApplication_preInit.001"
TEST.SLOT: "4", "<<COMPOUND>>", "<<COMPOUND>>", "1", "AP0002_ATPApplication_init_pass"
TEST.SLOT: "5", "atp_application", "(cl)ATP::Kernel::ATPApplication::run", "1", "ATPApplication_run.004"
TEST.SLOT: "6", "atp_application", "(cl)ATP::Kernel::ATPApplication::getLastExecTime", "1", "ATPApplication_getLastExecTime.003"
TEST.SLOT: "7", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMinExecTime", "1", "ATPApplication_getMinExecTime.004"
TEST.SLOT: "8", "atp_application", "(cl)ATP::Kernel::ATPApplication::getMaxExecTime", "1", "ATPApplication_getMaxExecTime.004"
TEST.SLOT: "9", "atp_application", "(cl)ATP::Kernel::ATPApplication::getApplicationName", "1", "ATPApplication_getApplicationName.001"
TEST.SLOT: "10", "atp_application", "(cl)ATP::Kernel::ATPApplication::getApplicationVersionString", "1", "ATPApplication_getApplicationVersionString.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0012_ATPApplication_init_fail_initStage2
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.002"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.014"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0013_ATPApplication_init_fail_initStage1
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.015"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0014_ATPApplication_supervise_initialization_fail
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "1", "ATPApplication_init.002"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::init", "2", "ATPApplication_init.016"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0015_ATPApplication_validate_Dispatcher_Version_Match
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion", "1", "ATPApplication_validateDispatcherVersion.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString", "1", "ATPApplication_getDispatcherVersionString.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AP0016_ATPApplication_validate_Dispatcher_Version_Mismatch
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "ATPApplication_instance.001"
TEST.SLOT: "2", "atp_application", "(cl)ATP::Kernel::ATPApplication::setDispatcherVersionString", "1", "ATPApplication_setDispatcherVersionString.001"
TEST.SLOT: "3", "atp_application", "(cl)ATP::Kernel::ATPApplication::validateDispatcherVersion", "1", "ATPApplication_validateDispatcherVersion.002"
TEST.SLOT: "4", "atp_application", "(cl)ATP::Kernel::ATPApplication::getDispatcherVersionString", "1", "ATPApplication_getDispatcherVersionString.002"
TEST.END
--
